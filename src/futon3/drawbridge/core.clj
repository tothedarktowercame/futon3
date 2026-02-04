(ns futon3.drawbridge.core
  "Shared Drawbridge infrastructure for agent wrappers.

   Provides:
   - HTTP API for input
   - Java-WebSocket for streaming output
   - Input multiplexing
   - Agency integration (local handler registration)
   - IRC chat integration

   Usage:
     (start! {:http-port 6768
              :ws-port 6770
              :invoke-fn (fn [text session-id] {:result \"...\" :session-id \"...\" :exit-code 0})
              :agent-id \"my-agent\"})

   The invoke-fn is agent-specific (Claude, Codex, etc.) and handles CLI invocation."
  (:require [clojure.core.async :as async :refer [go-loop <! chan close!]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clj-http.client :as httpc]
            [org.httpkit.server :as http]
            [ring.middleware.params :as ring-params]
            [ring.util.codec :as codec])
  (:import (org.java_websocket WebSocket)
           (org.java_websocket.handshake ClientHandshake)
           (org.java_websocket.server WebSocketServer)
           (java.net InetSocketAddress URI)
           (java.net.http HttpClient HttpClient$Version WebSocket$Listener)
           (java.util.concurrent CompletableFuture TimeUnit)))

;; Forward declarations
(declare broadcast-to-ws-clients! ws-clients irc-connected? send-to-irc!
         handle-agency-message stop-agency-ws-client! ack-test-bell!
         handle-page-sync handle-par-bell!)

;; =============================================================================
;; Agent State
;; =============================================================================

(defonce ^:private agent-state
  (atom {:session-id nil
         :last-active nil
         :agent-id nil
         :invoke-fn nil  ; Agent-specific invocation function
         :agency-http-url nil
         :output-subscribers #{}}))

(defn session-id
  "Get current session ID."
  []
  (:session-id @agent-state))

(defn set-session-id!
  "Set session ID (used by invoke functions)."
  [id]
  (swap! agent-state assoc :session-id id))

(defn agent-alive?
  "Check if agent has an active session."
  []
  (some? (:session-id @agent-state)))

(defn clear-state!
  "Clear agent state."
  []
  (swap! agent-state assoc :session-id nil :last-active nil :invoke-fn nil))

;; =============================================================================
;; Output Broadcasting
;; =============================================================================

(defonce ^:private ws-server-state (atom nil))
(defonce ^:private ws-clients (atom #{}))

(defonce ^:private agency-ws-state
  (atom {:running? false
         :stop nil
         :state (atom {:ws nil
                       :last-error nil
                       :last-attempt nil
                       :last-connected nil
                       :last-closed nil
                       :last-ping nil})
         :url nil
         :agent-id nil
         :reconnect-ms 5000
         :ping-ms 15000}))

(defn- send-to-ws-client! [^WebSocket conn data]
  (when (.isOpen conn)
    (try
      (.send conn (json/encode data))
      (catch Exception e
        (println "[drawbridge-ws] Send failed:" (.getMessage e))))))

(defn broadcast-to-ws-clients!
  "Broadcast a message to all connected WebSocket clients."
  [data]
  (doseq [client @ws-clients]
    (send-to-ws-client! client data)))

(defn broadcast-result!
  "Broadcast a result from an agent invocation. Handles line splitting for display."
  [result]
  (doseq [line (str/split-lines (or result ""))]
    (broadcast-to-ws-clients! {:type "stdout" :line line}))
  ;; Also send to IRC if connected
  (when (irc-connected?)
    (doseq [line (str/split-lines (or result ""))]
      (when-not (str/blank? line)
        (send-to-irc! line)))))

;; =============================================================================
;; Input Processing
;; =============================================================================

(defn send-input!
  "Send input to the agent. Uses the registered invoke-fn."
  [text]
  (if-let [invoke-fn (:invoke-fn @agent-state)]
    (do
      (swap! agent-state assoc :last-active (java.time.Instant/now))
      (future
        (try
          (let [session-id (:session-id @agent-state)
                {:keys [result session-id exit-code error]} (invoke-fn text session-id)]
            ;; Update session ID if returned
            (when session-id
              (swap! agent-state assoc :session-id session-id))
            ;; Broadcast result
            (if (and (zero? (or exit-code 0)) (not error))
              (broadcast-result! result)
              (broadcast-to-ws-clients! {:type "error" :message (or error result)}))
            (broadcast-to-ws-clients! {:type "done" :exit-code (or exit-code 0)}))
          (catch Exception e
            (println "[drawbridge] Invoke error:" (.getMessage e))
            (broadcast-to-ws-clients! {:type "error" :message (.getMessage e)}))))
      true)
    (do
      (println "[drawbridge] No invoke-fn registered!")
      false)))

(defonce ^:private input-chan (chan 100))

(defn enqueue-input!
  "Enqueue input from any source (HTTP, WebSocket, Agency, etc.)"
  [source payload]
  (async/put! input-chan {:source source :payload payload :timestamp (java.time.Instant/now)}))

(defn- start-input-processor!
  "Process the input queue."
  []
  (go-loop []
    (when-let [{:keys [_source payload]} (<! input-chan)]
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
        (future (send-input! body))
        {:status 200
         :headers {"content-type" "application/json"}
         :body "{\"ok\":true}"}))))

(defn- handle-bell [request]
  (let [body (some-> request :body slurp str/trim)]
    (if (str/blank? body)
      {:status 400 :body "empty input"}
      (try
        (let [msg (json/parse-string body true)]
          (future (handle-agency-message msg))
          {:status 200
           :headers {"content-type" "application/json"}
           :body "{\"ok\":true}"})
        (catch Exception e
          {:status 400
           :headers {"content-type" "application/json"}
           :body (json/encode {:ok false :error (.getMessage e)})})))))

(defn- handle-status [_request]
  {:status 200
   :headers {"content-type" "application/json"}
   :body (pr-str {:alive (agent-alive?)
                  :session-id (:session-id @agent-state)
                  :last-active (str (:last-active @agent-state))
                  :ws-clients (count @ws-clients)})})

;; =============================================================================
;; Java-WebSocket Server
;; =============================================================================

(defn- create-ws-server [port]
  (let [addr (InetSocketAddress. port)]
    (proxy [WebSocketServer] [addr]

      (onOpen [^WebSocket conn ^ClientHandshake _handshake]
        (println "[drawbridge-ws] Client connected:" (.getRemoteSocketAddress conn))
        (swap! ws-clients conj conn)
        (send-to-ws-client! conn {:type "connected"
                                   :session-id (:session-id @agent-state)
                                   :alive (agent-alive?)}))

      (onClose [^WebSocket conn code reason _remote]
        (println "[drawbridge-ws] Client disconnected:" code reason)
        (swap! ws-clients disj conn))

      (onMessage [^WebSocket _conn ^String message]
        (println "[drawbridge-ws] Received:" message)
        (enqueue-input! :websocket message))

      (onError [^WebSocket conn ^Exception ex]
        (println "[drawbridge-ws] Error:" (.getMessage ex))
        (when conn
          (swap! ws-clients disj conn)))

      (onStart []
        (println "[drawbridge-ws] WebSocket server started on port" port)))))

(defn start-ws-server! [port]
  (when-let [{:keys [server]} @ws-server-state]
    (try (.stop ^WebSocketServer server 1000) (catch Exception _)))
  (let [server (create-ws-server port)]
    (.setReuseAddr server true)
    (.start server)
    (Thread/sleep 100)
    (reset! ws-server-state {:server server :port port})
    (println "[drawbridge-ws] WebSocket server running on port" port)
    server))

(defn stop-ws-server! []
  (when-let [{:keys [server]} @ws-server-state]
    (try (.stop ^WebSocketServer server 1000) (catch Exception _))
    (reset! ws-server-state nil)
    (reset! ws-clients #{})
    (println "[drawbridge-ws] WebSocket server stopped")))

(defn- ring-handler [token endpoint-prefix]
  (fn [request]
    (let [uri (:uri request)
          method (:request-method request)
          send-path (str "/" endpoint-prefix)
          bell-path (str "/" endpoint-prefix "/bell")
          status-path (str "/" endpoint-prefix "/status")]
      (cond
        (and (= method :post) (= uri send-path))
        (handle-send request)

        (and (= method :post) (= uri bell-path))
        (handle-bell request)

        (and (= method :get) (= uri status-path))
        (handle-status request)

        :else
        {:status 404 :body "not found"}))))

;; =============================================================================
;; Agency Integration
;; =============================================================================

(defn- send-page-response!
  "Send a page response back to Agency.
   Uses local handler if available, otherwise sends via WebSocket."
  [request-id response]
  ;; Try local handler first (in-JVM Agency)
  (let [local-sent (try
                     (require 'futon3.agency.http)
                     (let [respond-fn (ns-resolve 'futon3.agency.http 'handle-page-response)]
                       (when respond-fn
                         (respond-fn request-id response)))
                     (catch Exception _
                       false))]
    ;; If not local, send via WebSocket
    (when-not local-sent
      (when-let [ws (:ws @(:state @agency-ws-state))]
        (try
          (.sendText ^java.net.http.WebSocket ws
                     (json/encode {:type "page-response"
                                   :request-id request-id
                                   :response response})
                     true)
          (println "[drawbridge] Page response sent via WS:" request-id)
          (catch Exception e
            (println "[drawbridge] Failed to send page response via WS:" (.getMessage e))))))))

(defn- handle-page-sync
  "Handle a page request synchronously - invoke agent and return response."
  [msg]
  (let [{:keys [request-id prompt]} msg]
    (println (format "[drawbridge] Handling page request %s" request-id))
    (if-let [invoke-fn (:invoke-fn @agent-state)]
      (try
        (let [session-id (:session-id @agent-state)
              {:keys [result session-id exit-code error]} (invoke-fn prompt session-id)]
          ;; Update session ID if returned
          (when session-id
            (swap! agent-state assoc :session-id session-id))
          ;; Send response back
          (send-page-response! request-id
                               {:result result
                                :exit-code (or exit-code 0)
                                :error error
                                :agent-id (:agent-id @agent-state)}))
        (catch Exception e
          (println "[drawbridge] Page invoke error:" (.getMessage e))
          (send-page-response! request-id
                               {:error (.getMessage e)
                                :agent-id (:agent-id @agent-state)})))
      (send-page-response! request-id
                           {:error "no invoke-fn registered"
                            :agent-id (:agent-id @agent-state)}))))

(defn- handle-agency-message
  "Handle a message from Agency (bell, page, etc.)."
  [msg]
  (let [msg-type (:type msg)]
    (println (format "[drawbridge] Received Agency message: %s" msg-type))
    (case msg-type
      "bell"
      (let [bell-type (:bell-type msg)
            payload (:payload msg)
            prompt (format "[Agency Bell: %s] %s"
                           bell-type
                           (or (:message payload) (pr-str payload)))]
        (println (format "[drawbridge] Bell type=%s" bell-type))
        (case bell-type
          "test-bell" (future (ack-test-bell! msg))
          "par" (future (handle-par-bell! msg))
          ;; Default: send prompt to agent
          (future (send-input! prompt))))

      "page"
      ;; Page is synchronous - invoke agent and return response
      (future (handle-page-sync msg))

      ;; Default: send as-is
      (future (send-input! (format "[Agency: %s] %s" msg-type (pr-str msg)))))))

(defn- agency-http-base-from-ws [ws-url]
  (when (seq ws-url)
    (let [u (URI/create ws-url)
          scheme (case (.getScheme u) "ws" "http" "wss" "https" (.getScheme u))
          host (.getHost u)
          port (.getPort u)
          port-str (if (neg? port) "" (str ":" port))]
      (when (and scheme host)
        (str scheme "://" host port-str)))))

(defn- ack-test-bell! [{:keys [secret-id]}]
  (let [base (or (:agency-http-url @agent-state)
                 (agency-http-base-from-ws (:url @agency-ws-state)))]
    (when (and base secret-id)
      (try
        (println (format "[drawbridge] test-bell ack: fetching secret %s" secret-id))
        (let [secret-resp (httpc/get (str base "/agency/secret/" secret-id)
                                     {:as :json :throw-exceptions false})
              secret-body (:body secret-resp)
              secret-value (or (get secret-body :value)
                               (get secret-body "value"))]
          (if secret-value
            (let [ack-resp (httpc/post (str base "/agency/ack")
                                       {:content-type :json
                                        :accept :json
                                        :throw-exceptions false
                                        :body (json/encode {:secret-id secret-id
                                                            :value secret-value
                                                            :agent-id (:agent-id @agent-state)})})]
              (println (format "[drawbridge] test-bell ack sent (status=%s)"
                               (:status ack-resp))))
            (println "[drawbridge] test-bell ack: secret missing in response")))
        (catch Exception e
          (println "[drawbridge] test-bell ack failed:" (.getMessage e)))))))

(defn- handle-par-bell!
  "Handle a PAR bell by spawning a local peripheral to contribute to the PAR.
  CRDT_HOST and AGENCY_URL are resolved locally from env (not from payload)."
  [{:keys [payload]}]
  (let [{:keys [par-title crdt-port]} payload
        crdt-host (or (System/getenv "CRDT_HOST") "127.0.0.1")
        agency-url (or (System/getenv "AGENCY_URL") "http://localhost:7070")
        agent-id (:agent-id @agent-state)
        peripheral-el (or (System/getenv "FUTON_PAR_PERIPHERAL_EL")
                          (str (System/getProperty "user.home")
                               "/code/futon0/contrib/futon-par-peripheral.el"))
        home (System/getProperty "user.home")
        crdt-el (or (System/getenv "CRDT_EL_PATH")
                    (first (filter (fn [p]
                                     (let [f (io/file p)]
                                       (and (.exists f) (.isFile f))))
                                   [(str home "/.emacs.d/elpa/crdt-0.3.5/crdt.el")
                                    (str home "/.emacs.d/lisp/crdt.el")
                                    (str home "/.emacs-graph/straight/build/crdt/crdt.el")
                                    (str home "/.emacs-graph/straight/repos/crdt/crdt.el")
                                    "/usr/share/emacs/site-lisp/crdt.el"])))]
    (if (and par-title crdt-host crdt-port agent-id crdt-el)
      (do
        (println (format "[drawbridge] PAR bell: spawning peripheral for %s, PAR='%s'"
                         agent-id par-title))
        (let [env {"CRDT_HOST" (str crdt-host)
                   "CRDT_PORT" (str crdt-port)
                   "AGENT_ID" (str agent-id)
                   "PAR_TITLE" (str par-title)
                   "AGENCY_URL" agency-url
                   "LANG" "en_US.UTF-8"
                   "LC_ALL" "en_US.UTF-8"}
              cmd ["emacs" "--batch" "-Q" "-l" crdt-el "-l" peripheral-el]
              pb (ProcessBuilder. ^java.util.List cmd)]
          ;; Set environment
          (let [pb-env (.environment pb)]
            (doseq [[k v] env]
              (.put pb-env k v)))
          ;; Redirect stderr to stdout
          (.redirectErrorStream pb true)
          ;; Start process asynchronously
          (future
            (try
              (let [proc (.start pb)
                    reader (io/reader (.getInputStream proc))]
                ;; Log output
                (doseq [line (line-seq reader)]
                  (println (format "[par-peripheral:%s] %s" agent-id line)))
                ;; Wait for completion
                (let [exit-code (.waitFor proc)]
                  (println (format "[drawbridge] PAR peripheral for %s exited with code %d"
                                   agent-id exit-code))))
              (catch Exception e
                (println (format "[drawbridge] PAR peripheral error: %s" (.getMessage e))))))))
      (println (format "[drawbridge] PAR bell missing params: par-title=%s crdt-host=%s crdt-port=%s agent-id=%s crdt-el=%s"
                       par-title crdt-host crdt-port agent-id crdt-el)))))

(defn- agency-ws-listener [state agent-id]
  (reify WebSocket$Listener
    (onOpen [_ ws]
      (swap! state assoc
             :ws ws
             :last-connected (str (java.time.Instant/now))
             :last-error nil)
      (try
        (.sendText ^java.net.http.WebSocket ws
                   (json/encode {:type "register" :agent-id agent-id})
                   true)
        (catch Exception e
          (swap! state assoc :last-error (.getMessage e))))
      (.request ^java.net.http.WebSocket ws 1))

    (onText [_ ws data last]
      (.request ^java.net.http.WebSocket ws 1)
      (when last
        (let [text (str data)]
          (try
            (let [msg (json/parse-string text true)
                  msg-type (:type msg)]
              (case msg-type
                "bell" (handle-agency-message msg)
                "page" (handle-agency-message msg)
                (println (format "[drawbridge] Agency WS message: %s" msg-type))))
            (catch Exception e
              (println "[drawbridge] Agency WS parse error:" (.getMessage e))))))
      nil)

    (onClose [_ _ws status reason]
      (swap! state assoc
             :ws nil
             :last-closed (str (java.time.Instant/now)))
      (println (format "[drawbridge] Agency WS closed: %s %s" status reason)))

    (onError [_ _ws err]
      (swap! state assoc :ws nil :last-error (.getMessage err))
      (println "[drawbridge] Agency WS error:" (.getMessage err)))))

(defn- open-agency-ws! [url agent-id state]
  (let [client (-> (HttpClient/newBuilder)
                   (.version HttpClient$Version/HTTP_1_1)
                   (.build))
        listener (agency-ws-listener state agent-id)
        fut (-> client
                (.newWebSocketBuilder)
                (.buildAsync (URI/create url) listener))]
    (.get ^CompletableFuture fut 10 TimeUnit/SECONDS)))

(defn start-agency-ws-client!
  "Start a WebSocket client to Agency (/agency/ws)."
  [{:keys [url agent-id reconnect-ms ping-ms]}]
  (let [url (some-> url str/trim not-empty)
        agent-id (or (some-> agent-id str/trim not-empty)
                     (:agent-id @agent-state)
                     "agent")
        reconnect-ms (or reconnect-ms 5000)
        ping-ms (or ping-ms 15000)]
    (when url
      (stop-agency-ws-client!)
      (let [stop-flag (atom false)
            state (atom {:ws nil
                         :last-error nil
                         :last-attempt nil
                         :last-connected nil
                         :last-closed nil
                         :last-ping nil})]
        (reset! agency-ws-state {:running? true
                                 :stop stop-flag
                                 :state state
                                 :url url
                                 :agent-id agent-id
                                 :reconnect-ms reconnect-ms
                                 :ping-ms ping-ms})
        (future
          (while (not @stop-flag)
            (if (nil? (:ws @state))
              (try
                (swap! state assoc :last-attempt (str (java.time.Instant/now)))
                (println (format "[drawbridge] Agency WS connecting to %s as %s" url agent-id))
                (open-agency-ws! url agent-id state)
                (catch Exception e
                  (swap! state assoc :last-error (.getMessage e))
                  (println "[drawbridge] Agency WS connect failed:" (.getMessage e))
                  (Thread/sleep reconnect-ms)))
              (Thread/sleep 1000))))
        (future
          (while (not @stop-flag)
            (when-let [ws (:ws @state)]
              (try
                (.sendText ^java.net.http.WebSocket ws
                           (json/encode {:type "ping"})
                           true)
                (swap! state assoc :last-ping (str (java.time.Instant/now)))
                (catch Exception e
                  (swap! state assoc :last-error (.getMessage e)))))
            (Thread/sleep ping-ms)))
        true))))

(defn stop-agency-ws-client!
  "Stop the Agency WebSocket client."
  []
  (when-let [{:keys [stop state]} @agency-ws-state]
    (when stop
      (reset! stop true))
    (when-let [ws (:ws @state)]
      (try
        (.sendClose ^java.net.http.WebSocket ws 1000 "drawbridge stop")
        (catch Exception _)))
    (reset! agency-ws-state {:running? false
                             :stop nil
                             :state (atom {:ws nil
                                           :last-error nil
                                           :last-attempt nil
                                           :last-connected nil
                                           :last-closed nil})
                             :url nil
                             :agent-id nil
                             :reconnect-ms 5000})
    true))

(defn register-with-agency!
  "Register this Drawbridge instance with Agency."
  [agent-id]
  (try
    (require 'futon3.agency.http)
    (let [register-fn (ns-resolve 'futon3.agency.http 'register-local-handler!)]
      (when register-fn
        (register-fn agent-id handle-agency-message)
        (println (format "[drawbridge] Registered with Agency as '%s'" agent-id))
        true))
    (catch Exception e
      (println (format "[drawbridge] Failed to register with Agency: %s" (.getMessage e)))
      false)))

(defn unregister-from-agency!
  "Unregister from Agency."
  [agent-id]
  (try
    (let [unregister-fn (ns-resolve 'futon3.agency.http 'unregister-local-handler!)]
      (when unregister-fn
        (unregister-fn agent-id)
        (println "[drawbridge] Unregistered from Agency")))
    (catch Exception _)))

;; =============================================================================
;; IRC Chat Integration
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
  (let [[_target & text-parts] (str/split params #"\s+" 2)
        text (str/replace (first text-parts) #"^:" "")
        nick (first (str/split (or prefix "") #"!"))]
    (when (and nick text (not= nick (:nick @irc-state)))
      (swap! irc-state update :transcript conj
             {:from nick :text text :at (str (java.time.Instant/now))})
      (let [prompt (format "[IRC %s] <%s> %s" (:room @irc-state) nick text)]
        (println (format "[drawbridge-irc] %s: %s" nick text))
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
        (println "[drawbridge-irc] Reader error:" (.getMessage e))))))

(defn connect-irc!
  "Connect to IRC and join a room."
  [{:keys [host port nick room password]
    :or {host "localhost" port 6667 nick "drawbridge"}}]
  (try
    (let [socket (java.net.Socket. ^String host ^int port)
          reader (io/reader socket)
          writer (io/writer socket)]
      (reset! irc-state {:socket socket
                         :reader reader
                         :writer writer
                         :room room
                         :nick nick
                         :transcript []
                         :active true})
      (when password
        (irc-send! (str "PASS " password)))
      (irc-send! (str "NICK " nick))
      (irc-send! (str "USER " nick " 0 * :" nick))
      (Thread/sleep 1000)
      (when room
        (irc-send! (str "JOIN #" (str/replace room #"^#" ""))))
      (irc-reader-loop)
      (println (format "[drawbridge-irc] Connected to %s:%d as %s, joined #%s" host port nick room))
      true)
    (catch Exception e
      (println "[drawbridge-irc] Connect failed:" (.getMessage e))
      false)))

(defn disconnect-irc!
  "Disconnect from IRC and return transcript."
  []
  (when (:active @irc-state)
    (swap! irc-state assoc :active false)
    (try
      (irc-send! "QUIT :Hop complete")
      (Thread/sleep 200)
      (.close ^java.net.Socket (:socket @irc-state))
      (catch Exception _))
    (let [transcript (:transcript @irc-state)]
      (reset! irc-state {:socket nil :reader nil :writer nil
                         :room nil :nick nil :transcript [] :active false})
      (println "[drawbridge-irc] Disconnected")
      transcript)))

(defn send-to-irc!
  "Send a message to the current IRC room."
  [text]
  (when-let [room (:room @irc-state)]
    (let [target (str "#" (str/replace room #"^#" ""))]
      (irc-send! (str "PRIVMSG " target " :" text))
      (swap! irc-state update :transcript conj
             {:from (:nick @irc-state) :text text :at (str (java.time.Instant/now))}))))

(defn irc-connected? []
  (:active @irc-state))

;; =============================================================================
;; Server Lifecycle
;; =============================================================================

(defonce ^:private server (atom nil))

(defn start!
  "Start the Drawbridge with a given invoke function.

   Options:
     :http-port       - HTTP port for REST API (default 6768)
     :ws-port         - WebSocket port for streaming (default 6770)
     :bind            - Bind address (default 127.0.0.1)
     :token           - Auth token (default from .admintoken or 'change-me')
     :invoke-fn       - REQUIRED. Function (fn [text session-id] -> {:result :session-id :exit-code})
     :endpoint-prefix - URL prefix (default 'agent', e.g. POST /agent)
     :agency-ws-url   - WebSocket URL for Agency (/agency/ws) (optional)
     :agency-http-url - HTTP base URL for Agency (optional)
     :agency-ws-agent-id - Agent id to use when connecting to Agency WS (optional)
     :agency-ws-reconnect-ms - Reconnect delay for Agency WS (default 5000)
     :agency-ws-ping-ms - Ping interval for Agency WS (default 15000)
     :register-local? - Register local handler with Agency (default true)
     :resume-id       - Initial session ID (optional)
     :agent-id        - Agent ID to register with Agency (optional)"
  [{:keys [http-port ws-port bind token invoke-fn endpoint-prefix resume-id agent-id
           agency-ws-url agency-ws-agent-id agency-ws-reconnect-ms agency-ws-ping-ms register-local?
           agency-http-url]
    :or {http-port 6768
         ws-port 6770
         bind "127.0.0.1"
         endpoint-prefix "agent"
         register-local? true}}]
  (when-not invoke-fn
    (throw (ex-info "invoke-fn is required" {})))

  ;; Stop existing servers
  (when-let [stop-fn @server]
    (stop-fn))
  (stop-ws-server!)

  ;; Set up state
  (swap! agent-state assoc
         :invoke-fn invoke-fn
         :session-id resume-id
         :agent-id agent-id
         :agency-http-url (or agency-http-url
                              (:agency-http-url @agent-state)
                              (agency-http-base-from-ws agency-ws-url)))

  ;; Load token from file if not provided
  (let [token (or token
                  (try (str/trim (slurp ".admintoken")) (catch Exception _ nil))
                  "change-me")
        handler (-> (ring-handler token endpoint-prefix)
                    ring-params/wrap-params
                    (wrap-token token))
        stop (http/run-server handler {:ip bind :port http-port})]

    (reset! server stop)

    ;; Start WebSocket server
    (start-ws-server! ws-port)

    ;; Start input processor
    (start-input-processor!)

    ;; Register with Agency (local handler) if agent-id provided
    (when (and agent-id register-local?)
      (register-with-agency! agent-id))

    ;; Start Agency WS client if URL provided
    (when agency-ws-url
      (start-agency-ws-client! {:url agency-ws-url
                                :agent-id (or agency-ws-agent-id agent-id)
                                :reconnect-ms agency-ws-reconnect-ms
                                :ping-ms agency-ws-ping-ms}))

    (println (format "[drawbridge] HTTP API on http://%s:%s/%s" bind http-port endpoint-prefix))
    (println (format "[drawbridge] WebSocket on ws://%s:%s" bind ws-port))
    (when (and agent-id register-local?)
      (println (format "[drawbridge] Registered with Agency as '%s'" agent-id)))

    {:http-stop stop :ws-port ws-port :http-port http-port :agent-id agent-id}))

(defn stop!
  "Stop the Drawbridge."
  []
  (when-let [agent-id (:agent-id @agent-state)]
    (unregister-from-agency! agent-id))
  (stop-agency-ws-client!)
  (clear-state!)
  (stop-ws-server!)
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil))
  (println "[drawbridge] Stopped"))
