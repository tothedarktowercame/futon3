(ns futon3.drawbridge.claude
  "Drawbridge for Claude Code - multi-headed REPL routing to Claude subprocess.

   Like cemerick/drawbridge routes HTTP to nREPL, this routes HTTP/WebSocket
   to a Claude Code subprocess. Supports:
   - Multiple input heads (HTTP, WebSocket, Agency direct)
   - Streaming output via WebSocket (Java-WebSocket, not http-kit)
   - Session resume via Claude's --resume flag
   - Hot-reloadable (lives in the JVM with MUSN)

   Usage:
     (start! {:http-port 6768 :ws-port 6770 :resume-id \"abc123\"})
     ;; POST /claude with body to send input
     ;; WS ws://localhost:6770 for streaming output + push"
  (:require [clojure.core.async :as async :refer [go-loop <! chan close!]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [org.httpkit.server :as http]
            [ring.middleware.params :as ring-params]
            [ring.util.codec :as codec])
  (:import (org.java_websocket WebSocket)
           (org.java_websocket.handshake ClientHandshake)
           (org.java_websocket.server WebSocketServer)
           (java.net InetSocketAddress)))

;; Forward declarations
(declare broadcast-to-ws-clients! ws-clients register-with-agency! unregister-from-agency!
         irc-connected? send-to-irc!)

;; =============================================================================
;; Agent Process Management
;; =============================================================================

(defonce ^:private agent-state
  (atom {:process nil
         :stdin nil
         :stdout nil
         :stderr nil
         :session-id nil      ; Claude session ID for --resume
         :last-active nil
         :agent-id nil        ; Agency agent ID if registered
         :output-subscribers #{}}))

(defn spawn-agent!
  "Initialize the Claude agent state.
   No persistent process is spawned - each message spawns a one-shot --print process.
   Uses --resume with session-id for thread-safe persistence.

   Options:
     :resume-id - Optional session ID to resume an existing conversation"
  [{:keys [resume-id] :as _opts}]
  (swap! agent-state assoc
         :session-id resume-id  ; nil = new session, or resume existing
         :last-active nil
         :process nil
         :stdin nil
         :stdout nil
         :stderr nil)
  (println (format "[claude-bridge] Initialized Claude session%s"
                   (if resume-id (str " (resuming: " resume-id ")") " (new)")))
  @agent-state)

(defn kill-agent!
  "Clear the Claude agent state."
  []
  (swap! agent-state assoc :process nil :stdin nil :stdout nil :stderr nil :session-id nil)
  (println "[claude-bridge] Cleared Claude agent state"))

(defn agent-alive?
  "Check if Claude agent has an active session."
  []
  (some? (:session-id @agent-state)))

(defn send-input!
  "Send a message to Claude. Spawns a one-shot --print process per message.
   Output streams to WebSocket clients.
   Uses --resume with session-id for thread-safe conversation persistence."
  [text]
  (let [session-id (:session-id @agent-state)
        cmd (cond-> ["claude" "--print" "--output-format" "json"
                     "--permission-mode" "bypassPermissions"]
              session-id (into ["--resume" session-id]))
        _ (println (format "[claude-bridge] Executing: claude --print%s (stdin: %s)"
                           (if session-id (str " --resume " session-id) "")
                           (subs text 0 (min 50 (count text)))))
        pb (ProcessBuilder. ^java.util.List cmd)
        _ (.redirectErrorStream pb true)
        proc (.start pb)
        stdin (io/writer (.getOutputStream proc))
        stdout (io/reader (.getInputStream proc))]

    ;; Update last-active timestamp
    (swap! agent-state assoc :last-active (java.time.Instant/now))

    ;; Send prompt via stdin
    (doto stdin
      (.write text)
      (.flush)
      (.close))

    ;; Read JSON output and broadcast result
    (future
      (try
        (println "[claude-bridge] Reading JSON response...")
        (let [output (StringBuilder.)]
          ;; Read all output
          (loop []
            (when-let [line (.readLine ^java.io.BufferedReader stdout)]
              (.append output line)
              (recur)))
          (.waitFor proc)
          (let [exit-code (.exitValue proc)
                out-str (str output)]
            (if (zero? exit-code)
              (try
                (let [parsed (json/parse-string out-str true)
                      result (:result parsed)
                      new-session-id (:session_id parsed)]
                  ;; Store session-id for future calls
                  (when new-session-id
                    (swap! agent-state assoc :session-id new-session-id))
                  ;; Broadcast result line-by-line for nice display
                  (doseq [line (str/split-lines (or result ""))]
                    (broadcast-to-ws-clients! {:type "stdout" :line line}))
                  ;; Also send to IRC if connected
                  (when (irc-connected?)
                    (send-to-irc! (or result "")))
                  (println (format "[claude-bridge] Session: %s" new-session-id)))
                (catch Exception e
                  (println "[claude-bridge] JSON parse error:" (.getMessage e))
                  ;; Fall back to raw output
                  (broadcast-to-ws-clients! {:type "stdout" :line out-str})))
              (do
                (println "[claude-bridge] Process failed:" out-str)
                (broadcast-to-ws-clients! {:type "error" :message out-str})))
            (broadcast-to-ws-clients! {:type "done" :exit-code exit-code})))
        (catch Exception e
          (println "[claude-bridge] Reader error:" (.getMessage e)))))
    true))

(defn subscribe-output!
  "Subscribe a channel to receive output events."
  [ch]
  (swap! agent-state update :output-subscribers conj ch)
  ch)

(defn unsubscribe-output!
  "Unsubscribe a channel from output events."
  [ch]
  (swap! agent-state update :output-subscribers disj ch)
  (close! ch))

;; =============================================================================
;; Input Multiplexing
;; =============================================================================

(defonce ^:private input-chan (chan 100))

(defn enqueue-input!
  "Enqueue input from any source (HTTP, WebSocket, Agency, etc.)"
  [source payload]
  (async/put! input-chan {:source source :payload payload :timestamp (java.time.Instant/now)}))

(defn- start-input-processor!
  "Process the input queue, sending to Claude."
  []
  (go-loop []
    (when-let [{:keys [_source payload]} (<! input-chan)]
      ;; Just pass the payload directly for now
      (send-input! payload)
      (recur))))

;; =============================================================================
;; HTTP Handler
;; =============================================================================

(defn- wrap-token [handler token]
  (fn [request]
    (let [supplied (or (get-in request [:headers "x-admin-token"])
                       (some-> (:query-string request)
                               (codec/form-decode "UTF-8")
                               (get "token")))
          supplied (some-> supplied str/trim)
          expected (some-> token str/trim)]
      (if (= supplied expected)
        (handler request)
        {:status 403 :body "forbidden"}))))

(defn- handle-send [request]
  (let [body (some-> request :body slurp str/trim)]
    (if (str/blank? body)
      {:status 400 :body "empty input"}
      (do
        ;; Bypass queue, call send-input! directly for now
        (future (send-input! body))
        {:status 200
         :headers {"content-type" "application/json"}
         :body "{\"ok\":true}"}))))

(defn- handle-status [_request]
  {:status 200
   :headers {"content-type" "application/json"}
   :body (pr-str {:alive (agent-alive?)
                  :session-id (:session-id @agent-state)
                  :last-active (str (:last-active @agent-state))
                  :ws-clients (count @ws-clients)})})

;; =============================================================================
;; Java-WebSocket Server (separate port, reliable)
;; =============================================================================

(defonce ^:private ws-server-state (atom nil))
(defonce ^:private ws-clients (atom #{}))

(defn- send-to-ws-client! [^WebSocket conn data]
  (when (.isOpen conn)
    (try
      (.send conn (json/encode data))
      (catch Exception e
        (println "[claude-ws] Send failed:" (.getMessage e))))))

(defn- broadcast-to-ws-clients! [data]
  (doseq [client @ws-clients]
    (send-to-ws-client! client data)))

(defn- create-ws-server [port]
  (let [addr (InetSocketAddress. port)]
    (proxy [WebSocketServer] [addr]

      (onOpen [^WebSocket conn ^ClientHandshake handshake]
        (println "[claude-ws] Client connected:" (.getRemoteSocketAddress conn))
        (swap! ws-clients conj conn)
        (send-to-ws-client! conn {:type "connected"
                                   :session-id (:session-id @agent-state)
                                   :alive (agent-alive?)}))

      (onClose [^WebSocket conn code reason remote]
        (println "[claude-ws] Client disconnected:" code reason)
        (swap! ws-clients disj conn))

      (onMessage [^WebSocket conn ^String message]
        (println "[claude-ws] Received:" message)
        ;; Treat incoming WebSocket messages as input
        (enqueue-input! :websocket message))

      (onError [^WebSocket conn ^Exception ex]
        (println "[claude-ws] Error:" (.getMessage ex))
        (when conn
          (swap! ws-clients disj conn)))

      (onStart []
        (println "[claude-ws] WebSocket server started on port" port)))))

(defn start-ws-server! [port]
  (when-let [{:keys [server]} @ws-server-state]
    (try (.stop server 1000) (catch Exception _)))
  (let [server (create-ws-server port)]
    (.setReuseAddr server true)
    (.start server)
    (Thread/sleep 100)
    (reset! ws-server-state {:server server :port port})
    (println "[claude-ws] WebSocket server running on port" port)
    server))

(defn stop-ws-server! []
  (when-let [{:keys [server]} @ws-server-state]
    (try (.stop server 1000) (catch Exception _))
    (reset! ws-server-state nil)
    (reset! ws-clients #{})
    (println "[claude-ws] WebSocket server stopped")))

(defn- ring-handler [token]
  (fn [request]
    (let [uri (:uri request)
          method (:request-method request)]
      (cond
        ;; POST /claude - send input
        (and (= method :post) (= uri "/claude"))
        (handle-send request)

        ;; GET /claude/status - check status
        (and (= method :get) (= uri "/claude/status"))
        (handle-status request)

        :else
        {:status 404 :body "not found"}))))

;; =============================================================================
;; Server Lifecycle
;; =============================================================================

(defonce ^:private server (atom nil))

(defn start!
  "Start the Claude Drawbridge.

   Options:
     :http-port  - HTTP port for REST API (default 6768)
     :ws-port    - WebSocket port for streaming (default 6770)
     :bind       - Bind address (default 127.0.0.1)
     :token      - Auth token (default from .admintoken or 'change-me')
     :resume-id  - Claude session ID to resume (optional)
     :auto-spawn - Spawn Claude process on start (default true)
     :agent-id   - Agent ID to register with Agency (default nil, no registration)"
  [{:keys [http-port ws-port bind token resume-id auto-spawn agent-id]
    :or {http-port 6768
         ws-port 6770
         bind "127.0.0.1"
         auto-spawn true}}]
  ;; Stop existing servers
  (when-let [stop-fn @server]
    (stop-fn))
  (stop-ws-server!)

  ;; Load token from file if not provided
  (let [token (or token
                  (try (str/trim (slurp ".admintoken")) (catch Exception _ nil))
                  "change-me")
        handler (-> (ring-handler token)
                    ring-params/wrap-params
                    (wrap-token token))
        stop (http/run-server handler {:ip bind :port http-port})]

    (reset! server stop)

    ;; Start WebSocket server on separate port
    (start-ws-server! ws-port)

    ;; Start input processor
    (start-input-processor!)

    ;; Optionally spawn Claude
    (when auto-spawn
      (spawn-agent! {:resume-id resume-id}))

    ;; Register with Agency if agent-id provided
    (when agent-id
      (register-with-agency! agent-id)
      (swap! agent-state assoc :agent-id agent-id))

    (println (format "[claude-bridge] HTTP API on http://%s:%s/claude" bind http-port))
    (println (format "[claude-bridge] WebSocket on ws://%s:%s" bind ws-port))
    (when agent-id
      (println (format "[claude-bridge] Registered with Agency as '%s'" agent-id)))
    {:http-stop stop :ws-port ws-port :http-port http-port :agent-id agent-id}))

(defn stop!
  "Stop the Claude Drawbridge and kill the agent."
  []
  ;; Unregister from Agency if we were registered
  (when-let [agent-id (:agent-id @agent-state)]
    (unregister-from-agency! agent-id))
  (kill-agent!)
  (stop-ws-server!)
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil))
  (println "[claude-bridge] Stopped"))

;; =============================================================================
;; Agency Integration
;; =============================================================================

(defn- handle-agency-message
  "Handle a message from Agency (bell, etc.). Called by Agency's local handler mechanism."
  [msg]
  (let [msg-type (:type msg)]
    (println (format "[claude-bridge] Received Agency message: %s" msg-type))
    (case msg-type
      "bell"
      (let [bell-type (:bell-type msg)
            payload (:payload msg)
            prompt (format "[Agency Bell: %s] %s"
                           bell-type
                           (or (:message payload) (pr-str payload)))]
        (future (send-input! prompt)))

      ;; Default: send as-is
      (future (send-input! (format "[Agency: %s] %s" msg-type (pr-str msg)))))))

(defn register-with-agency!
  "Register this Drawbridge instance with Agency to receive bells and messages.
   Requires Agency to be running in the same JVM."
  [agent-id]
  (try
    (require 'futon3.agency.http)
    (let [register-fn (ns-resolve 'futon3.agency.http 'register-local-handler!)]
      (when register-fn
        (register-fn agent-id handle-agency-message)
        (println (format "[claude-bridge] Registered with Agency as '%s'" agent-id))
        true))
    (catch Exception e
      (println (format "[claude-bridge] Failed to register with Agency: %s" (.getMessage e)))
      false)))

(defn unregister-from-agency!
  "Unregister from Agency."
  [agent-id]
  (try
    (let [unregister-fn (ns-resolve 'futon3.agency.http 'unregister-local-handler!)]
      (when unregister-fn
        (unregister-fn agent-id)
        (println "[claude-bridge] Unregistered from Agency")))
    (catch Exception _)))

;; =============================================================================
;; IRC Chat Integration (for peripheral hops)
;; =============================================================================

(defonce ^:private irc-state
  (atom {:socket nil
         :reader nil
         :writer nil
         :room nil
         :nick nil
         :transcript []
         :active false}))

(defn- irc-send! [msg]
  (when-let [writer (:writer @irc-state)]
    (doto writer
      (.write (str msg "\r\n"))
      (.flush))))

(defn- parse-irc-message [line]
  (when line
    (let [[_ prefix command params] (re-matches #"(?::(\S+)\s+)?(\S+)\s*(.*)" line)]
      {:prefix prefix
       :command command
       :params params})))

(defn- handle-irc-privmsg [prefix params]
  (let [[target & text-parts] (str/split params #"\s+" 2)
        text (str/replace (first text-parts) #"^:" "")
        nick (first (str/split (or prefix "") #"!"))]
    (when (and nick text (not= nick (:nick @irc-state)))
      ;; Add to transcript
      (swap! irc-state update :transcript conj
             {:from nick :text text :at (str (java.time.Instant/now))})
      ;; Send to Claude
      (let [prompt (format "[IRC %s] <%s> %s" (:room @irc-state) nick text)]
        (println (format "[claude-irc] %s: %s" nick text))
        (future (send-input! prompt))))))

(defn- irc-reader-loop []
  (future
    (try
      (let [reader (:reader @irc-state)]
        (loop []
          (when-let [line (try (.readLine reader) (catch Exception _ nil))]
            (let [{:keys [command params prefix]} (parse-irc-message line)]
              (case command
                "PING" (irc-send! (str "PONG " params))
                "PRIVMSG" (handle-irc-privmsg prefix params)
                nil))
            (when (:active @irc-state)
              (recur)))))
      (catch Exception e
        (println "[claude-irc] Reader error:" (.getMessage e))))))

(defn connect-irc!
  "Connect to IRC and join a room.
   Options:
     :host - IRC server (default localhost)
     :port - IRC port (default 6667)
     :nick - Nickname (default claude-bridge)
     :room - Room to join
     :password - IRC password (optional)"
  [{:keys [host port nick room password]
    :or {host "localhost" port 6667 nick "claude-bridge"}}]
  (try
    (let [socket (java.net.Socket. host port)
          reader (io/reader socket)
          writer (io/writer socket)]
      (reset! irc-state {:socket socket
                         :reader reader
                         :writer writer
                         :room room
                         :nick nick
                         :transcript []
                         :active true})
      ;; IRC handshake
      (when password
        (irc-send! (str "PASS " password)))
      (irc-send! (str "NICK " nick))
      (irc-send! (str "USER " nick " 0 * :" nick))
      (Thread/sleep 1000)
      (when room
        (irc-send! (str "JOIN #" (str/replace room #"^#" ""))))
      ;; Start reader loop
      (irc-reader-loop)
      (println (format "[claude-irc] Connected to %s:%d as %s, joined #%s" host port nick room))
      true)
    (catch Exception e
      (println "[claude-irc] Connect failed:" (.getMessage e))
      false)))

(defn disconnect-irc!
  "Disconnect from IRC and return transcript."
  []
  (when (:active @irc-state)
    (swap! irc-state assoc :active false)
    (try
      (irc-send! "QUIT :Hop complete")
      (Thread/sleep 200)
      (.close (:socket @irc-state))
      (catch Exception _))
    (let [transcript (:transcript @irc-state)]
      (reset! irc-state {:socket nil :reader nil :writer nil
                         :room nil :nick nil :transcript [] :active false})
      (println "[claude-irc] Disconnected")
      transcript)))

(defn send-to-irc!
  "Send a message to the current IRC room."
  [text]
  (when-let [room (:room @irc-state)]
    (let [target (str "#" (str/replace room #"^#" ""))]
      (irc-send! (str "PRIVMSG " target " :" text))
      ;; Add to transcript
      (swap! irc-state update :transcript conj
             {:from (:nick @irc-state) :text text :at (str (java.time.Instant/now))}))))

(defn irc-connected? []
  (:active @irc-state))

(comment
  ;; Development / REPL usage
  (start! {:http-port 6768
           :ws-port 6770
           :resume-id "64570417-4354-40b8-b6a5-db804f69a1d0"})
  (stop!)

  (agent-alive?)
  (send-input! "Hello from the REPL!")
  (enqueue-input! :agency {:type :bell :bell-type :ping})

  ;; From another process:
  ;; HTTP: curl -X POST http://localhost:6768/claude -d "Hello" -H "X-Admin-Token: your-token" -H "Content-Type: text/plain"
  ;; WebSocket: bb ws-client connecting to ws://localhost:6770
  )
