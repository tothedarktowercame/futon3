(ns futon3.drawbridge.core
  "Multi-agent Drawbridge router for Agency.

   Routes messages between Agency (bells, whistles, pages) and registered agent
   sessions. Multiple agents can register simultaneously — each with its own
   invoke-fn, session-id, and IRC connection.

   Agents register with:
     (register-agent! \"claude-agency\" {:invoke-fn f :session-id s})

   The router does not spawn or own agent processes. They exist independently
   and register/deregister at will.

   For remote agents, start! provides backwards-compatible HTTP/WS + Agency WS
   client mode."
  (:require [clojure.java.io :as io]
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
(declare broadcast-to-ws-clients! ws-clients
         handle-agency-message-for stop-agency-ws-client!
         irc-connected-for? send-to-irc-for! connect-irc-for! disconnect-irc-for!)

;; =============================================================================
;; Multi-Agent Registry
;; =============================================================================

(defonce ^:private agents-registry
  (atom {}))
;; {agent-id-str -> {:invoke-fn    (fn [text session-id] -> {:result :session-id :exit-code})
;;                   :session-id   str
;;                   :irc-nick     str
;;                   :irc-state    atom  ;; {:socket :reader :writer :room :nick :transcript :active}
;;                   :last-active  Instant
;;                   :agency-http-url str}}

(defn- get-agent
  "Look up an agent's state in the registry."
  [agent-id]
  (get @agents-registry (name agent-id)))

(defn registered-agents
  "Return list of registered agent-ids."
  []
  (vec (keys @agents-registry)))

(defn session-id-for
  "Get session ID for a specific agent."
  [agent-id]
  (:session-id (get-agent agent-id)))

(defn agent-alive?
  "Check if an agent is registered (or any agent, for backwards compat)."
  ([] (boolean (seq @agents-registry)))
  ([agent-id] (some? (get-agent agent-id))))

(defn session-id
  "Get session ID. In multi-agent mode, returns first agent's session-id."
  []
  (when-let [[_aid agent] (first @agents-registry)]
    (:session-id agent)))

;; =============================================================================
;; Output Broadcasting (shared infrastructure)
;; =============================================================================

(defonce ^:private ws-server-state (atom nil))
(defonce ^:private ws-clients (atom #{}))

;; Agency WS state (for remote agents connecting to a remote Agency)
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

(defn- broadcast-result-for!
  "Broadcast a result from an agent invocation. Tags output with agent-id."
  [agent-id result]
  (let [aid (name agent-id)]
    (doseq [line (str/split-lines (or result ""))]
      (broadcast-to-ws-clients! {:type "stdout" :agent-id aid :line line}))
    (when (irc-connected-for? aid)
      (doseq [line (str/split-lines (or result ""))]
        (when-not (str/blank? line)
          (send-to-irc-for! aid line))))))

;; =============================================================================
;; Per-Agent Input Processing
;; =============================================================================

(defn send-input-to!
  "Send input to a specific agent. Uses the agent's registered invoke-fn."
  [agent-id text]
  (let [aid (name agent-id)]
    (if-let [{:keys [invoke-fn session-id]} (get-agent aid)]
      (do
        (swap! agents-registry update aid assoc :last-active (java.time.Instant/now))
        (future
          (try
            (let [{:keys [result session-id exit-code error]} (invoke-fn text session-id)]
              (when session-id
                (swap! agents-registry update aid assoc :session-id session-id))
              (if (and (zero? (or exit-code 0)) (not error))
                (broadcast-result-for! aid result)
                (broadcast-to-ws-clients! {:type "error" :agent-id aid :message (or error result)}))
              (broadcast-to-ws-clients! {:type "done" :agent-id aid :exit-code (or exit-code 0)}))
            (catch Exception e
              (println (format "[drawbridge:%s] Invoke error: %s" aid (.getMessage e)))
              (broadcast-to-ws-clients! {:type "error" :agent-id aid :message (.getMessage e)}))))
        true)
      (do
        (println (format "[drawbridge] No agent '%s' registered" aid))
        false))))

(defn send-input!
  "Send input to the first registered agent (backwards-compat)."
  [text]
  (if-let [[aid _] (first @agents-registry)]
    (send-input-to! aid text)
    (do (println "[drawbridge] No agents registered!") false)))

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

(defn- handle-send-for
  "Handle a send request for a specific agent."
  [agent-id request]
  (let [body (some-> request :body slurp str/trim)]
    (if (str/blank? body)
      {:status 400 :body "empty input"}
      (do
        (future (send-input-to! agent-id body))
        {:status 200
         :headers {"content-type" "application/json"}
         :body (json/encode {:ok true :agent-id agent-id})}))))

(defn- handle-bell-for
  "Handle a bell request for a specific agent."
  [agent-id request]
  (let [body (some-> request :body slurp str/trim)]
    (if (str/blank? body)
      {:status 400 :body "empty input"}
      (try
        (let [msg (json/parse-string body true)]
          (future (handle-agency-message-for agent-id msg))
          {:status 200
           :headers {"content-type" "application/json"}
           :body "{\"ok\":true}"})
        (catch Exception e
          {:status 400
           :headers {"content-type" "application/json"}
           :body (json/encode {:ok false :error (.getMessage e)})})))))

(defn- handle-status-for
  "Handle a status request for a specific agent."
  [agent-id]
  (if-let [agent (get-agent agent-id)]
    {:status 200
     :headers {"content-type" "application/json"}
     :body (json/encode {:ok true
                         :agent-id agent-id
                         :session-id (:session-id agent)
                         :last-active (str (:last-active agent))
                         :irc-connected (boolean (irc-connected-for? agent-id))})}
    {:status 404
     :headers {"content-type" "application/json"}
     :body (json/encode {:ok false :error "agent not found"})}))

(defn- handle-status-all
  "Handle a status request for all agents."
  []
  {:status 200
   :headers {"content-type" "application/json"}
   :body (json/encode {:ok true
                       :agents (into {}
                                 (map (fn [[aid agent]]
                                        [aid {:session-id (:session-id agent)
                                              :last-active (str (:last-active agent))
                                              :irc-connected (boolean (irc-connected-for? aid))}])
                                      @agents-registry))
                       :ws-clients (count @ws-clients)})})

;; =============================================================================
;; Java-WebSocket Server (shared)
;; =============================================================================

(defn- create-ws-server [port]
  (let [addr (InetSocketAddress. port)]
    (proxy [WebSocketServer] [addr]
      (onOpen [^WebSocket conn ^ClientHandshake _handshake]
        (println "[drawbridge-ws] Client connected:" (.getRemoteSocketAddress conn))
        (swap! ws-clients conj conn)
        (send-to-ws-client! conn {:type "connected"
                                   :agents (registered-agents)}))
      (onClose [^WebSocket conn code reason _remote]
        (println "[drawbridge-ws] Client disconnected:" code reason)
        (swap! ws-clients disj conn))
      (onMessage [^WebSocket _conn ^String message]
        (println "[drawbridge-ws] Received:" message)
        ;; Try to parse as JSON with agent-id, fall back to first agent
        (try
          (let [parsed (json/parse-string message true)
                agent-id (or (:agent-id parsed) (first (registered-agents)))]
            (when agent-id
              (send-input-to! agent-id (or (:text parsed) message))))
          (catch Exception _
            ;; Plain text: route to first agent
            (when-let [[aid _] (first @agents-registry)]
              (send-input-to! aid message)))))
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

(defn- ring-handler [_token]
  (fn [request]
    (let [uri (:uri request)
          method (:request-method request)
          ;; Parse: /drawbridge/:agent-id[/action]
          [_ db-prefix agent-id action] (re-matches #"/drawbridge(?:/([^/]+))?(?:/(bell|status))?" uri)]
      (cond
        ;; GET /drawbridge/status — all agents
        (and (= method :get) (= uri "/drawbridge/status"))
        (handle-status-all)

        ;; POST /drawbridge/:agent-id — send input
        (and (= method :post) db-prefix agent-id (nil? action))
        (if (get-agent agent-id)
          (handle-send-for agent-id request)
          {:status 404 :body (str "agent '" agent-id "' not registered")})

        ;; POST /drawbridge/:agent-id/bell — forward bell
        (and (= method :post) db-prefix agent-id (= action "bell"))
        (if (get-agent agent-id)
          (handle-bell-for agent-id request)
          {:status 404 :body (str "agent '" agent-id "' not registered")})

        ;; GET /drawbridge/:agent-id/status — agent status
        (and (= method :get) db-prefix agent-id (= action "status"))
        (handle-status-for agent-id)

        ;; Legacy: POST /:prefix — try as agent-id, then first registered
        (and (= method :post) (re-matches #"/[^/]+" uri))
        (let [prefix (subs uri 1)]
          (if (get-agent prefix)
            (handle-send-for prefix request)
            (if-let [[aid _] (first @agents-registry)]
              (handle-send-for aid request)
              {:status 503 :body "no agents registered"})))

        ;; Legacy: GET /:prefix/status
        (and (= method :get) (re-matches #"/[^/]+/status" uri))
        (let [prefix (second (re-matches #"/([^/]+)/status" uri))]
          (if (get-agent prefix)
            (handle-status-for prefix)
            (handle-status-all)))

        :else
        {:status 404 :body "not found"}))))

;; =============================================================================
;; Agency Integration — Per-Agent Message Handlers
;; =============================================================================

(defn- send-page-response!
  "Send a page response back to Agency."
  [request-id response]
  (let [local-sent (try
                     (require 'futon3.agency.http)
                     (let [respond-fn (ns-resolve 'futon3.agency.http 'handle-page-response)]
                       (when respond-fn
                         (respond-fn request-id response)))
                     (catch Exception _
                       false))]
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

(defn- send-whistle-response!
  "Send a whistle response back to Agency."
  [request-id response]
  (let [local-sent (try
                     (require 'futon3.agency.http)
                     (let [respond-fn (ns-resolve 'futon3.agency.http 'handle-whistle-response)]
                       (when respond-fn
                         (respond-fn request-id response)))
                     (catch Exception _
                       false))]
    (when-not local-sent
      (when-let [ws (:ws @(:state @agency-ws-state))]
        (try
          (.sendText ^java.net.http.WebSocket ws
                     (json/encode {:type "whistle-response"
                                   :request-id request-id
                                   :response response})
                     true)
          (println "[drawbridge] Whistle response sent via WS:" request-id)
          (catch Exception e
            (println "[drawbridge] Failed to send whistle response via WS:" (.getMessage e))))))))

(defn- handle-page-sync-for
  "Handle a page request for a specific agent."
  [agent-id msg]
  (let [{:keys [request-id prompt]} msg
        aid (name agent-id)]
    (println (format "[drawbridge:%s] Handling page request %s" aid request-id))
    (if-let [{:keys [invoke-fn session-id]} (get-agent aid)]
      (try
        (let [{:keys [result session-id exit-code error]} (invoke-fn prompt session-id)]
          (when session-id
            (swap! agents-registry update aid assoc :session-id session-id))
          (send-page-response! request-id
                               {:result result
                                :exit-code (or exit-code 0)
                                :error error
                                :agent-id aid}))
        (catch Exception e
          (println (format "[drawbridge:%s] Page invoke error: %s" aid (.getMessage e)))
          (send-page-response! request-id
                               {:error (.getMessage e) :agent-id aid})))
      (send-page-response! request-id
                           {:error (str "agent '" aid "' not registered")
                            :agent-id aid}))))

(defn- handle-whistle-sync-for
  "Handle a whistle request for a specific agent."
  [agent-id msg]
  (let [{:keys [request-id prompt]} msg
        aid (name agent-id)]
    (println (format "[drawbridge:%s] Handling whistle request %s" aid request-id))
    (if-let [{:keys [invoke-fn session-id]} (get-agent aid)]
      (try
        (let [{:keys [result session-id exit-code error]} (invoke-fn prompt session-id)]
          (when session-id
            (swap! agents-registry update aid assoc :session-id session-id))
          (send-whistle-response! request-id
                                  {:result result
                                   :exit-code (or exit-code 0)
                                   :error error
                                   :agent-id aid}))
        (catch Exception e
          (println (format "[drawbridge:%s] Whistle invoke error: %s" aid (.getMessage e)))
          (send-whistle-response! request-id
                                  {:error (.getMessage e) :agent-id aid})))
      (send-whistle-response! request-id
                              {:error (str "agent '" aid "' not registered")
                               :agent-id aid}))))

(defn- agency-http-base-from-ws [ws-url]
  (when (seq ws-url)
    (let [u (URI/create ws-url)
          scheme (case (.getScheme u) "ws" "http" "wss" "https" (.getScheme u))
          host (.getHost u)
          port (.getPort u)
          port-str (if (neg? port) "" (str ":" port))]
      (when (and scheme host)
        (str scheme "://" host port-str)))))

(defn- ack-bell-for!
  "Ack a bell for a specific agent (fetch secret + POST ack)."
  [agent-id {:keys [secret-id]}]
  (let [aid (name agent-id)
        agent (get-agent aid)
        base (or (:agency-http-url agent)
                 (agency-http-base-from-ws (:url @agency-ws-state)))]
    (when (and base secret-id)
      (try
        (println (format "[drawbridge:%s] Acking bell: fetching secret %s" aid secret-id))
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
                                                            :agent-id aid})})]
              (println (format "[drawbridge:%s] Bell ack sent (status=%s)"
                               aid (:status ack-resp))))
            (println (format "[drawbridge:%s] Bell ack: secret missing in response" aid))))
        (catch Exception e
          (println (format "[drawbridge:%s] Bell ack failed: %s" aid (.getMessage e))))))))

(defn- handle-standup-bell-for!
  "Handle a standup bell for a specific agent: ack, join IRC, participate.

   A standup is a co-present conversation, not a broadcast. The agent:
   1. Acks the bell (proves reception / rendezvous handshake)
   2. Joins the IRC room specified in the bell payload
   3. Sends an opening prompt to the agent (which responds via IRC)
   4. Stays in the room — the IRC reader loop handles ongoing conversation."
  [agent-id {:keys [secret-id payload]}]
  (let [aid (name agent-id)
        irc-host (or (:irc-host payload) "localhost")
        irc-port (or (:irc-port payload) 6667)
        room (or (:room payload) "standup")
        prompt (or (:prompt payload)
                   "Standup: Share what you're working on, any blockers, and what's next. Keep it concise.")]
    (ack-bell-for! aid {:secret-id secret-id})
    (when-not (irc-connected-for? aid)
      (try
        (let [nick (or (:irc-nick (get-agent aid)) aid)]
          (connect-irc-for! aid {:host irc-host :port irc-port
                                 :nick nick :room room}))
        (println (format "[drawbridge:%s] Standup: joined #%s on %s:%d" aid room irc-host irc-port))
        (catch Exception e
          (println (format "[drawbridge:%s] Standup: IRC connect failed: %s" aid (.getMessage e))))))
    (future (send-input-to! aid prompt))))

(defn- handle-par-bell-for!
  "Handle a PAR bell for a specific agent."
  [agent-id {:keys [payload]}]
  (let [aid (name agent-id)
        {:keys [par-title crdt-port]} payload
        crdt-host (or (System/getenv "CRDT_HOST") "127.0.0.1")
        agency-url (or (System/getenv "AGENCY_URL") "http://localhost:7070")
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
    (if (and par-title crdt-host crdt-port aid crdt-el)
      (do
        (println (format "[drawbridge:%s] PAR bell: spawning peripheral, PAR='%s'" aid par-title))
        (let [env {"CRDT_HOST" (str crdt-host)
                   "CRDT_PORT" (str crdt-port)
                   "AGENT_ID" (str aid)
                   "PAR_TITLE" (str par-title)
                   "AGENCY_URL" agency-url
                   "LANG" "en_US.UTF-8"
                   "LC_ALL" "en_US.UTF-8"}
              cmd ["emacs" "--batch" "-Q" "-l" crdt-el "-l" peripheral-el]
              pb (ProcessBuilder. ^java.util.List cmd)]
          (let [pb-env (.environment pb)]
            (doseq [[k v] env]
              (.put pb-env k v)))
          (.redirectErrorStream pb true)
          (future
            (try
              (let [proc (.start pb)
                    reader (io/reader (.getInputStream proc))]
                (doseq [line (line-seq reader)]
                  (println (format "[par-peripheral:%s] %s" aid line)))
                (let [exit-code (.waitFor proc)]
                  (println (format "[drawbridge:%s] PAR peripheral exited with code %d" aid exit-code))))
              (catch Exception e
                (println (format "[drawbridge:%s] PAR peripheral error: %s" aid (.getMessage e))))))))
      (println (format "[drawbridge:%s] PAR bell missing params: par-title=%s crdt-host=%s crdt-port=%s crdt-el=%s"
                       aid par-title crdt-host crdt-port crdt-el)))))

(defn- handle-agency-message-for
  "Handle a message from Agency for a specific agent."
  [agent-id msg]
  (let [aid (name agent-id)
        msg-type (:type msg)]
    (println (format "[drawbridge:%s] Agency message: %s" aid msg-type))
    (case msg-type
      "bell"
      (let [bell-type (:bell-type msg)
            payload (:payload msg)
            prompt (format "[Agency Bell: %s] %s"
                           bell-type
                           (or (:message payload) (pr-str payload)))]
        (println (format "[drawbridge:%s] Bell type=%s" aid bell-type))
        (case bell-type
          "test-bell" (future (ack-bell-for! aid msg))
          "par" (future (handle-par-bell-for! aid msg))
          "standup" (future (handle-standup-bell-for! aid msg))
          ;; Default: send prompt to agent
          (future (send-input-to! aid prompt))))

      "page"
      (future (handle-page-sync-for aid msg))

      "whistle"
      (future (handle-whistle-sync-for aid msg))

      ;; Default
      (future (send-input-to! aid (format "[Agency: %s] %s" msg-type (pr-str msg)))))))

;; =============================================================================
;; Agency WebSocket Client (for remote agents)
;; =============================================================================

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
                "bell" (handle-agency-message-for agent-id msg)
                ("page" "whistle") (handle-agency-message-for agent-id msg)
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

(defn- open-agency-ws! [url agent-id session-id state]
  (let [sep (if (str/includes? url "?") "&" "?")
        full-url (cond-> (str url sep "agent-id=" (java.net.URLEncoder/encode (str agent-id) "UTF-8"))
                   session-id (str "&session-id=" (java.net.URLEncoder/encode (str session-id) "UTF-8")))
        client (-> (HttpClient/newBuilder)
                   (.version HttpClient$Version/HTTP_1_1)
                   (.build))
        listener (agency-ws-listener state agent-id)
        fut (-> client
                (.newWebSocketBuilder)
                (.buildAsync (URI/create full-url) listener))]
    (.get ^CompletableFuture fut 10 TimeUnit/SECONDS)))

(defn start-agency-ws-client!
  "Start a WebSocket client to Agency (/agency/ws)."
  [{:keys [url agent-id session-id reconnect-ms ping-ms]}]
  (let [url (some-> url str/trim not-empty)
        agent-id (or (some-> agent-id str/trim not-empty) "agent")
        session-id (or (some-> session-id str/trim not-empty)
                       (session-id-for agent-id))
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
                                 :session-id session-id
                                 :reconnect-ms reconnect-ms
                                 :ping-ms ping-ms})
        (future
          (while (not @stop-flag)
            (if (nil? (:ws @state))
              (try
                (swap! state assoc :last-attempt (str (java.time.Instant/now)))
                (println (format "[drawbridge] Agency WS connecting to %s as %s (session: %s)" url agent-id session-id))
                (open-agency-ws! url agent-id session-id state)
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

;; =============================================================================
;; Registration API
;; =============================================================================

(defn register-agent!
  "Register an agent with the drawbridge router.

   The agent must provide an invoke-fn; everything else is optional.
   Registers a local handler with Agency so the agent receives bells,
   whistles, and pages routed by agent-id.

   Options:
     :invoke-fn       - REQUIRED. (fn [text session-id] -> {:result :session-id :exit-code})
     :session-id      - Initial session ID (optional)
     :irc-nick        - IRC nickname (defaults to agent-id)
     :agency-http-url - Agency HTTP base URL (optional, for acking bells)"
  [agent-id {:keys [invoke-fn session-id irc-nick agency-http-url]}]
  (when-not invoke-fn
    (throw (ex-info "invoke-fn is required" {:agent-id agent-id})))
  (let [aid (name agent-id)]
    (swap! agents-registry assoc aid
           {:invoke-fn invoke-fn
            :session-id session-id
            :irc-nick (or irc-nick aid)
            :irc-state (atom {:socket nil :reader nil :writer nil
                              :room nil :nick nil :transcript [] :active false})
            :last-active nil
            :agency-http-url agency-http-url})
    ;; Register local handler with Agency
    (try
      (require 'futon3.agency.http)
      (when-let [reg-fn (ns-resolve 'futon3.agency.http 'register-local-handler!)]
        (reg-fn aid (fn [msg] (handle-agency-message-for aid msg)))
        (println (format "[drawbridge] Registered agent '%s' with Agency" aid)))
      (catch Exception e
        (println (format "[drawbridge] Failed to register '%s' with Agency: %s" aid (.getMessage e)))))
    aid))

(defn deregister-agent!
  "Deregister an agent. Disconnects IRC, unregisters from Agency, removes from registry."
  [agent-id]
  (let [aid (name agent-id)]
    (when (get-agent aid)
      (disconnect-irc-for! aid)
      (try
        (require 'futon3.agency.http)
        (when-let [unreg-fn (ns-resolve 'futon3.agency.http 'unregister-local-handler!)]
          (unreg-fn aid))
        (catch Exception _))
      (swap! agents-registry dissoc aid)
      (println (format "[drawbridge] Deregistered agent '%s'" aid))
      true)))

;; =============================================================================
;; Per-Agent IRC Chat Integration
;; =============================================================================

(defn- irc-send-for!
  "Send a raw IRC message for a specific agent."
  [agent-id msg]
  (when-let [agent (get-agent agent-id)]
    (when-let [writer (:writer @(:irc-state agent))]
      (doto writer
        (.write (str msg "\r\n"))
        (.flush)))))

(defn- parse-irc-message [line]
  (when line
    (let [[_ prefix command params] (re-matches #"(?::(\S+)\s+)?(\S+)\s*(.*)" line)]
      {:prefix prefix
       :command command
       :params params})))

(defn- handle-irc-privmsg-for
  "Handle an IRC PRIVMSG for a specific agent."
  [agent-id prefix params]
  (when-let [agent (get-agent agent-id)]
    (let [irc-st @(:irc-state agent)
          [_target & text-parts] (str/split params #"\s+" 2)
          text (str/replace (first text-parts) #"^:" "")
          nick (first (str/split (or prefix "") #"!"))]
      (when (and nick text (not= nick (:nick irc-st)))
        (swap! (:irc-state agent) update :transcript conj
               {:from nick :text text :at (str (java.time.Instant/now))})
        (let [prompt (format "[IRC %s] <%s> %s" (:room irc-st) nick text)]
          (println (format "[drawbridge-irc:%s] %s: %s" agent-id nick text))
          (future (send-input-to! agent-id prompt)))))))

(defn- irc-reader-loop-for
  "Start an IRC reader loop for a specific agent."
  [agent-id]
  (future
    (try
      (when-let [agent (get-agent agent-id)]
        (let [reader (:reader @(:irc-state agent))]
          (loop []
            (when-let [line (try (.readLine reader) (catch Exception _ nil))]
              (let [{:keys [command params prefix]} (parse-irc-message line)]
                (case command
                  "PING" (irc-send-for! agent-id (str "PONG " params))
                  "PRIVMSG" (handle-irc-privmsg-for agent-id prefix params)
                  nil))
              (when-let [a (get-agent agent-id)]
                (when (:active @(:irc-state a))
                  (recur)))))))
      (catch Exception e
        (println (format "[drawbridge-irc:%s] Reader error: %s" agent-id (.getMessage e)))))))

(defn connect-irc-for!
  "Connect a specific agent to IRC and join a room."
  [agent-id {:keys [host port nick room password]
             :or {host "localhost" port 6667 nick "drawbridge"}}]
  (when-let [agent (get-agent agent-id)]
    (try
      (let [socket (java.net.Socket. ^String host ^int port)
            reader (io/reader socket)
            writer (io/writer socket)]
        (reset! (:irc-state agent)
                {:socket socket :reader reader :writer writer
                 :room room :nick nick :transcript [] :active true})
        (when password
          (irc-send-for! agent-id (str "PASS " password)))
        (irc-send-for! agent-id (str "NICK " nick))
        (irc-send-for! agent-id (str "USER " nick " 0 * :" nick))
        (Thread/sleep 1000)
        (when room
          (irc-send-for! agent-id (str "JOIN #" (str/replace room #"^#" ""))))
        (irc-reader-loop-for agent-id)
        (println (format "[drawbridge-irc:%s] Connected to %s:%d as %s, joined #%s"
                         agent-id host port nick room))
        true)
      (catch Exception e
        (println (format "[drawbridge-irc:%s] Connect failed: %s" agent-id (.getMessage e)))
        false))))

(defn disconnect-irc-for!
  "Disconnect a specific agent from IRC and return transcript."
  [agent-id]
  (when-let [agent (get-agent agent-id)]
    (let [irc-st @(:irc-state agent)]
      (when (:active irc-st)
        (swap! (:irc-state agent) assoc :active false)
        (try
          (irc-send-for! agent-id "QUIT :Hop complete")
          (Thread/sleep 200)
          (.close ^java.net.Socket (:socket irc-st))
          (catch Exception _))
        (let [transcript (:transcript irc-st)]
          (reset! (:irc-state agent)
                  {:socket nil :reader nil :writer nil
                   :room nil :nick nil :transcript [] :active false})
          (println (format "[drawbridge-irc:%s] Disconnected" agent-id))
          transcript)))))

(defn send-to-irc-for!
  "Send a message to a specific agent's IRC room."
  [agent-id text]
  (when-let [agent (get-agent agent-id)]
    (let [irc-st @(:irc-state agent)]
      (when-let [room (:room irc-st)]
        (let [target (str "#" (str/replace room #"^#" ""))]
          (irc-send-for! agent-id (str "PRIVMSG " target " :" text))
          (swap! (:irc-state agent) update :transcript conj
                 {:from (:nick irc-st) :text text :at (str (java.time.Instant/now))}))))))

(defn irc-connected-for?
  "Check if a specific agent is connected to IRC."
  [agent-id]
  (when-let [agent (get-agent agent-id)]
    (:active @(:irc-state agent))))

;; Backwards-compat wrappers (operate on first registered agent)
(defn connect-irc! [opts]
  (when-let [[aid _] (first @agents-registry)]
    (connect-irc-for! aid opts)))
(defn disconnect-irc! []
  (when-let [[aid _] (first @agents-registry)]
    (disconnect-irc-for! aid)))
(defn send-to-irc! [text]
  (when-let [[aid _] (first @agents-registry)]
    (send-to-irc-for! aid text)))
(defn irc-connected? []
  (when-let [[aid _] (first @agents-registry)]
    (irc-connected-for? aid)))

;; =============================================================================
;; Server Lifecycle
;; =============================================================================

(defonce ^:private server (atom nil))

(defn start!
  "Start the Drawbridge.

   In multi-agent mode: call register-agent! directly instead.
   start! is retained for backwards compatibility and for remote agents that
   need their own HTTP/WS server + Agency WS client.

   Options:
     :http-port       - HTTP port for REST API (default 6768)
     :ws-port         - WebSocket port for streaming (default 6770)
     :bind            - Bind address (default 127.0.0.1)
     :token           - Auth token (default from .admintoken or 'change-me')
     :invoke-fn       - REQUIRED. Function (fn [text session-id] -> {:result :session-id :exit-code})
     :endpoint-prefix - URL prefix (legacy, ignored in multi-agent routing)
     :agency-ws-url   - WebSocket URL for Agency (/agency/ws) (optional)
     :agency-http-url - HTTP base URL for Agency (optional)
     :agency-ws-agent-id - Agent id for Agency WS (optional)
     :agency-ws-reconnect-ms - Reconnect delay for Agency WS (default 5000)
     :agency-ws-ping-ms - Ping interval for Agency WS (default 15000)
     :register-local? - Register local handler with Agency (default true)
     :resume-id       - Initial session ID (optional)
     :agent-id        - Agent ID (optional)"
  [{:keys [http-port ws-port bind token invoke-fn _endpoint-prefix resume-id agent-id
           agency-ws-url agency-ws-agent-id agency-ws-reconnect-ms agency-ws-ping-ms _register-local?
           agency-http-url]
    :or {http-port 6768
         ws-port 6770
         bind "127.0.0.1"}}]
  (when-not invoke-fn
    (throw (ex-info "invoke-fn is required" {})))

  ;; Stop existing servers
  (when-let [stop-fn @server]
    (stop-fn))
  (stop-ws-server!)

  ;; Register the agent
  (when agent-id
    (register-agent! agent-id
                     {:invoke-fn invoke-fn
                      :session-id resume-id
                      :agency-http-url (or agency-http-url
                                           (agency-http-base-from-ws agency-ws-url))}))

  ;; Load token from file if not provided
  (let [token (or token
                  (try (str/trim (slurp ".admintoken")) (catch Exception _ nil))
                  "change-me")
        handler (-> (ring-handler token)
                    ring-params/wrap-params
                    (wrap-token token))
        stop (http/run-server handler {:ip bind :port http-port})]

    (reset! server stop)

    ;; Start WebSocket server
    (start-ws-server! ws-port)

    ;; Phase 3a: if Agency WS URL is configured, WS is the single routing
    ;; authority; deregister the local handler to avoid A1 eviction.
    (when agency-ws-url
      (when agent-id
        (try
          (require 'futon3.agency.http)
          (when-let [unreg-fn (ns-resolve 'futon3.agency.http 'unregister-local-handler!)]
            (unreg-fn (name agent-id)))
          (catch Exception _)))
      (start-agency-ws-client! {:url agency-ws-url
                                :agent-id (or agency-ws-agent-id agent-id)
                                :session-id resume-id
                                :reconnect-ms agency-ws-reconnect-ms
                                :ping-ms agency-ws-ping-ms}))

    (println (format "[drawbridge] HTTP API on http://%s:%s/drawbridge/:agent-id" bind http-port))
    (println (format "[drawbridge] WebSocket on ws://%s:%s" bind ws-port))
    (println (format "[drawbridge] Registered agents: %s" (registered-agents)))

    {:http-stop stop :ws-port ws-port :http-port http-port :agent-id agent-id}))

(defn stop!
  "Stop the Drawbridge. Deregisters all agents, stops servers."
  []
  (doseq [aid (registered-agents)]
    (deregister-agent! aid))
  (stop-agency-ws-client!)
  (stop-ws-server!)
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil))
  (println "[drawbridge] Stopped"))

(comment
  ;; === Multi-agent usage ===

  ;; Register agents directly (preferred for local/same-JVM agents):
  (register-agent! "claude-agency"
    {:invoke-fn (fn [text sid] {:result (str "echo: " text) :session-id sid :exit-code 0})
     :session-id "abc123"})

  (register-agent! "claude-forum"
    {:invoke-fn (fn [text sid] {:result (str "forum: " text) :session-id sid :exit-code 0})})

  (registered-agents)
  ;; => ["claude-agency" "claude-forum"]

  ;; Send input to specific agent:
  (send-input-to! "claude-agency" "Hello!")

  ;; IRC for specific agent:
  (connect-irc-for! "claude-agency" {:host "localhost" :port 6667 :nick "claude-agency" :room "standup"})
  (irc-connected-for? "claude-agency")
  (disconnect-irc-for! "claude-agency")

  ;; Deregister:
  (deregister-agent! "claude-forum")

  ;; === Backwards-compat (remote agent with HTTP/WS) ===
  (start! {:http-port 6768
           :ws-port 6770
           :invoke-fn (fn [text sid] {:result text :session-id sid :exit-code 0})
           :agent-id "codex"
           :agency-ws-url "ws://linode:7070/agency/ws"})
  (stop!))
