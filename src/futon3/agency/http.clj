(ns futon3.agency.http
  "HTTP adapter for Agency service."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [futon3.agency.codex-mirror :as codex-mirror]
            [futon3.agency.registry :as reg]
            [futon3.agency.service :as svc]))

(def ^:private log-path (or (System/getenv "AGENCY_LOG") "/tmp/agency_http.log"))

;; Forward declarations for functions used in handler
(declare handle-agency-ws connected-agent-ids local-agent-ids remote-agent-ids agent-sessions kick-agent!
         handle-get-secret handle-create-secret
         handle-ack handle-ack-status handle-ring-bell
         handle-page handle-page-response)

(defn- log! [msg & [ctx]]
  (try
    (spit log-path
          (str (java.time.Instant/now) " " msg
               (when ctx (str " " (pr-str ctx)))
               "\n")
          :append true)
    (catch Throwable _)))

(defn- parse-json-body [req]
  (let [body (slurp (:body req))]
    (when (seq body)
      (json/parse-string body true))))

(defn- json-response
  ([payload] (json-response 200 payload))
  ([status payload]
   {:status status
    :headers {"content-type" "application/json"}
    :body (json/generate-string payload)}))

(defn- parse-query [query-string]
  (when (seq query-string)
    (->> (str/split query-string #"&")
         (map #(str/split % #"=" 2))
         (map (fn [[k v]]
                [(java.net.URLDecoder/decode (or k "") "UTF-8")
                 (java.net.URLDecoder/decode (or v "") "UTF-8")]))
         (into {}))))

(defn- req-query-string
  "Extract a query string from an http-kit request, even for WS upgrades
   where :query-string may be missing. Falls back to parsing :request-uri."
  [req]
  (or (:query-string req)
      (when-let [request-uri (:request-uri req)]
        (second (str/split request-uri #"\?" 2)))))

(defn- handle-run [body]
  (let [{:keys [agent-id peripheral prompt inputs musn forum resume-id thread-id cwd approval-policy no-sandbox]} body
        resume-id (or resume-id
                      (when (and thread-id (nil? forum))
                        thread-id))
        prompt (or prompt "")]
    (if (or (str/blank? (str agent-id))
            (str/blank? (str peripheral))
            (and (str/blank? (str prompt)) (empty? inputs)))
      {:ok false :err "missing required fields: agent-id, peripheral, prompt or inputs"}
      (svc/run-peripheral! {:agent-id agent-id
                            :peripheral peripheral
                            :prompt prompt
                            :inputs inputs
                            :musn musn
                            :forum forum
                            :resume-id resume-id
                            :cwd cwd
                            :approval-policy approval-policy
                            :no-sandbox no-sandbox}))))

(defn- handle-rollover [body]
  (let [{:keys [agent-id reason musn forum]} body
        reason (or reason "manual")]
    (if (str/blank? (str agent-id))
      {:ok false :err "missing agent-id"}
      (svc/roll-over! agent-id reason {:musn musn :forum forum}))))

(defn- handle-state [params]
  (let [agent-id (get params "agent-id")]
    (if (str/blank? (str agent-id))
      {:ok false :err "missing agent-id"}
      {:ok true :state (svc/ensure-agent-state! agent-id)})))

(defn- handle-threads []
  {:ok true
   :agents (map (fn [entry]
                  {:agent-id (:agent/id entry)
                   :thread-id (:agent/current-thread-id entry)
                   :last-active (:agent/last-active entry)})
                (svc/list-agent-states))})

(defn handler [req]
  (try
    (let [uri (:uri req)
          method (:request-method req)]
      (cond
        (= uri "/health") (json-response {:ok true})

        (and (= uri "/agency/state") (= method :get))
        (json-response (handle-state (parse-query (:query-string req))))

        (and (= uri "/agency/threads") (= method :get))
        (json-response (handle-threads))

        (and (= uri "/agency/mirror/status") (= method :get))
        (json-response (codex-mirror/status))

        ;; Agent registry status (M-agency-unified-routing)
        (and (= uri "/agency/registry") (= method :get))
        (json-response {:ok true :registry (reg/registry-status)})

        ;; WebSocket endpoint for agent walkie-talkie
        (= uri "/agency/ws")
        (handle-agency-ws req)

        ;; Connected agents list
        (and (= uri "/agency/connected") (= method :get))
        (let [params (parse-query (:query-string req))
              type-filter (get params "type")]
          (json-response
           (case type-filter
             "local" {:ok true :agents (local-agent-ids)}
             "remote" {:ok true :agents (remote-agent-ids)}
             "registry" {:ok true :agents (reg/registered-agents)}
             ;; Default: return all with breakdown
             {:ok true
              :agents (connected-agent-ids)
              :registry (reg/registered-agents)
              :local (local-agent-ids)
              :remote (remote-agent-ids)
              :sessions (agent-sessions)})))

        ;; Kick (disconnect) an agent
        (and (str/starts-with? uri "/agency/kick/") (= method :post))
        (let [agent-id (subs uri (count "/agency/kick/"))]
          (json-response
           (if (kick-agent! agent-id)
             {:ok true :kicked agent-id}
             {:ok false :err "agent not found"})))

        ;; Secret retrieval (GET /agency/secret/:id)
        (and (= method :get) (str/starts-with? uri "/agency/secret/"))
        (let [secret-id (subs uri (count "/agency/secret/"))]
          (json-response (handle-get-secret secret-id)))

        ;; Secret acknowledgement status (GET /agency/ack/:id)
        (and (= method :get) (str/starts-with? uri "/agency/ack/"))
        (let [secret-id (subs uri (count "/agency/ack/"))]
          (json-response (handle-ack-status secret-id)))

        (not= :post method)
        (json-response 405 {:ok false :err "method not allowed"})

        ;; Secret creation (POST /agency/secret)
        (= uri "/agency/secret")
        (json-response (handle-create-secret (parse-json-body req)))

        ;; Secret acknowledgement (POST /agency/ack)
        (= uri "/agency/ack")
        (json-response (handle-ack (parse-json-body req)))

        ;; Ring bell to connected agents (async fire-and-forget)
        (= uri "/agency/bell")
        (json-response (handle-ring-bell (parse-json-body req)))

        ;; Page a connected agent (sync - wait for response)
        (= uri "/agency/page")
        (json-response (handle-page (parse-json-body req)))

        ;; Spawn CLI process (exec is alias for run)
        (= uri "/agency/exec")
        (json-response (handle-run (parse-json-body req)))

        (= uri "/agency/run")
        (json-response (handle-run (parse-json-body req)))

        (= uri "/agency/rollover")
        (json-response (handle-rollover (parse-json-body req)))

        :else
        (json-response 404 {:ok false :err "not found"})))
    (catch Throwable t
      (log! "handler error" {:err (.getMessage t)
                             :trace (mapv str (.getStackTrace t))})
      (json-response 400 {:ok false :err (.getMessage t)}))))

;; Secret storage for test-bell-ack peripheral
(defonce ^:private secrets (atom {}))
(defonce ^:private acks (atom {}))

(defn- generate-secret-id []
  (str "sec-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- handle-create-secret [body]
  (let [secret-id (generate-secret-id)
        value (or (:value body) (str (java.util.UUID/randomUUID)))
        ttl-ms (or (:ttl-ms body) 300000)] ; 5 min default
    (swap! secrets assoc secret-id {:value value
                                    :created (System/currentTimeMillis)
                                    :ttl-ms ttl-ms})
    ;; Schedule cleanup
    (future
      (Thread/sleep ttl-ms)
      (swap! secrets dissoc secret-id))
    {:ok true :secret-id secret-id :value value}))

(defn- handle-ack [body]
  (let [{:keys [secret-id value agent-id]} body
        secret-id (some-> secret-id str)
        value (some-> value str)
        secret (get @secrets secret-id)]
    (cond
      (str/blank? secret-id)
      {:ok false :err "missing secret-id"}

      (str/blank? value)
      {:ok false :err "missing value"}

      (nil? secret)
      {:ok false :err "unknown secret"}

      (not= value (:value secret))
      {:ok false :err "value mismatch"}

      :else
      (let [ack {:agent-id (some-> agent-id str)
                 :value value
                 :timestamp (str (java.time.Instant/now))}]
        (swap! acks assoc secret-id ack)
        {:ok true :secret-id secret-id :ack ack}))))

(defn- handle-ack-status [secret-id]
  (if-let [ack (get @acks (some-> secret-id str))]
    {:ok true :secret-id secret-id :ack ack}
    {:ok false :err "ack not found" :secret-id secret-id}))

(defn- handle-get-secret [secret-id]
  (if-let [entry (get @secrets secret-id)]
    {:ok true :value (:value entry)}
    {:ok false :err "secret not found or expired"}))

;; =============================================================================
;; WebSocket - Agent walkie-talkie
;; =============================================================================

(defonce ^:private connected-agents (atom {}))
;; {agent-id -> {:channel ch :registered-at inst :last-ping inst}}

;; Local (in-JVM) agent handlers - for Drawbridge etc
(defonce ^:private local-handlers (atom {}))
;; {agent-id -> handler-fn}

(defn register-local-handler!
  "Register a local (in-JVM) handler for an agent. Handler receives messages directly."
  [agent-id handler-fn]
  (swap! local-handlers assoc (name agent-id) handler-fn)
  (log! "local-handler-registered" {:agent-id agent-id}))

(defn unregister-local-handler!
  "Unregister a local handler."
  [agent-id]
  (swap! local-handlers dissoc (name agent-id))
  (log! "local-handler-unregistered" {:agent-id agent-id}))

(defn kick-agent!
  "Disconnect an agent by closing its WebSocket and removing from registry."
  [agent-id]
  (let [aid (name agent-id)]
    (when-let [entry (get @connected-agents aid)]
      (when-let [channel (:channel entry)]
        (http/close channel))
      (swap! connected-agents dissoc aid)
      (log! "agent-kicked" {:agent-id agent-id})
      true)))

(defn- ws-send! [channel msg]
  (when (http/open? channel)
    (http/send! channel (json/generate-string msg))))

(defn send-to-agent!
  "Send a message to a connected agent. Returns true if sent, false if not connected.
   Checks local handlers first, then WebSocket connections."
  [agent-id msg]
  (let [aid (name agent-id)]
    (if-let [handler (get @local-handlers aid)]
      ;; Local handler (in-JVM, like Drawbridge)
      (do
        (try
          (handler msg)
          (log! "local-send" {:agent-id agent-id :type (:type msg)})
          true
          (catch Exception e
            (log! "local-send-error" {:agent-id agent-id :error (.getMessage e)})
            false)))
      ;; WebSocket connection
      (if-let [entry (get @connected-agents aid)]
        (do
          (ws-send! (:channel entry) msg)
          (log! "ws-send" {:agent-id agent-id :type (:type msg)})
          true)
        (do
          (log! "ws-send-failed" {:agent-id agent-id :reason "not-connected"})
          false)))))

(defn connected-agent-ids
  "Return list of currently connected agent IDs (registry + local handlers + WebSocket)."
  []
  (vec (distinct (concat (reg/registered-agents)
                         (keys @local-handlers)
                         (keys @connected-agents)))))

(defn local-agent-ids
  "Return list of locally registered agent IDs (in-JVM handlers only)."
  []
  (vec (keys @local-handlers)))

(defn remote-agent-ids
  "Return list of remotely connected agent IDs (WebSocket only)."
  []
  (vec (keys @connected-agents)))

(defn agent-sessions
  "Return map of agent-id -> session-id for agents that reported session IDs."
  []
  (into {}
        (for [[aid entry] @connected-agents
              :when (:session-id entry)]
          [aid (:session-id entry)])))

(defn- handle-ws-message [agent-id channel raw]
  (try
    (let [msg (json/parse-string raw true)]
      (log! "ws-recv" {:agent-id agent-id :type (:type msg)})
      (case (:type msg)
        "register"
        (do
          (let [session-id (:session-id msg)]
            (swap! connected-agents assoc (name agent-id)
                   (cond-> {:channel channel
                            :registered-at (java.time.Instant/now)
                            :last-ping (java.time.Instant/now)}
                     session-id (assoc :session-id session-id))))
          (ws-send! channel {:type "registered" :agent-id agent-id}))

        "ping"
        (do
          (swap! connected-agents update (name agent-id)
                 assoc :last-ping (java.time.Instant/now))
          (ws-send! channel {:type "pong" :at (str (java.time.Instant/now))}))

        "ack"
        (log! "ws-ack" {:agent-id agent-id :payload (:payload msg)})

        "page-response"
        (let [{:keys [request-id response]} msg]
          (if request-id
            (handle-page-response request-id response)
            (log! "ws-page-response-missing-id" {:agent-id agent-id})))

        ;; Default: log unknown
        (log! "ws-unknown" {:agent-id agent-id :msg msg})))
    (catch Exception e
      (log! "ws-parse-error" {:agent-id agent-id :error (.getMessage e)}))))

(defn- handle-agency-ws [req]
  (let [params (parse-query (req-query-string req))
        agent-id (or (get params "agent-id") "unknown")
        session-id (get params "session-id")]
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (http/with-channel req channel
      (log! "ws-connect" {:agent-id agent-id :session-id session-id})

      ;; Auto-register on connect (with optional session-id)
      (swap! connected-agents assoc (name agent-id)
             (cond-> {:channel channel
                      :registered-at (java.time.Instant/now)
                      :last-ping (java.time.Instant/now)}
               session-id (assoc :session-id session-id)))
      (ws-send! channel {:type "connected" :agent-id agent-id :session-id session-id})

      (http/on-receive channel
        (fn [raw]
          (handle-ws-message agent-id channel raw)))

      (http/on-close channel
        (fn [_status]
          (log! "ws-disconnect" {:agent-id agent-id})
          (swap! connected-agents dissoc (name agent-id)))))))

;; =============================================================================
;; Bell endpoint - ring connected agents
;; =============================================================================

(defn- handle-ring-bell [body]
  (let [{:keys [agent-id type payload]} body
        agent-id (or agent-id "all")
        bell-type (or type "test-bell")
        secret-result (handle-create-secret {:ttl-ms 60000})
        bell-msg {:type "bell"
                  :bell-type bell-type
                  :secret-id (:secret-id secret-result)
                  :payload payload
                  :timestamp (str (java.time.Instant/now))}]
    (if (= agent-id "all")
      ;; Ring all connected agents
      (let [agents (connected-agent-ids)
            results (mapv (fn [aid] {:agent-id aid :sent (send-to-agent! aid bell-msg)}) agents)]
        {:ok true :bell bell-msg :agents results :secret-value (:value secret-result)})
      ;; Ring specific agent
      (let [sent (send-to-agent! agent-id bell-msg)]
        {:ok sent
         :bell bell-msg
         :agent-id agent-id
         :secret-value (:value secret-result)
         :error (when-not sent "agent not connected")}))))

;; =============================================================================
;; Page endpoint - synchronous request to connected agent
;; =============================================================================

(defonce ^:private pending-pages (atom {}))
;; {request-id -> {:promise p :agent-id aid :created-at inst}}

(defn- generate-request-id []
  (str "req-" (subs (str (java.util.UUID/randomUUID)) 0 12)))

(defn handle-page-response
  "Called by agents (local handlers or via WS) to deliver a page response.
   Returns true if the response was delivered, false if request not found/expired."
  [request-id response]
  (if-let [entry (get @pending-pages request-id)]
    (do
      (deliver (:promise entry) response)
      (swap! pending-pages dissoc request-id)
      (log! "page-response-delivered" {:request-id request-id})
      true)
    (do
      (log! "page-response-orphaned" {:request-id request-id})
      false)))

(def ^:private default-page-timeout-ms
  (or (some-> (System/getenv "AGENCY_PAGE_TIMEOUT_MS") Long/parseLong)
      60000)) ; 1 minute default

(defn- handle-page [body]
  (let [{:keys [agent-id prompt timeout-ms]} body
        timeout-ms (or timeout-ms default-page-timeout-ms)
        aid (some-> agent-id name)]
    (cond
      (str/blank? (str agent-id))
      {:ok false :err "missing agent-id"}

      (str/blank? (str prompt))
      {:ok false :err "missing prompt"}

      ;; Priority 1: Check agent-registry for direct invocation (M-agency-unified-routing)
      (reg/agent-registered? aid)
      (let [{:keys [ok result error session-id]} (reg/invoke-agent! aid prompt timeout-ms)]
        (if ok
          {:ok true :response result :session-id session-id :source :registry}
          {:ok false :err (or error "invoke failed") :source :registry}))

      ;; Priority 2: Check local-handlers (backwards compat with existing Drawbridge)
      (get @local-handlers aid)
      (let [request-id (generate-request-id)
            response-promise (promise)
            page-msg {:type "page"
                      :request-id request-id
                      :prompt prompt
                      :timestamp (str (java.time.Instant/now))}]
        (swap! pending-pages assoc request-id
               {:promise response-promise
                :agent-id agent-id
                :created-at (System/currentTimeMillis)})
        (future
          (Thread/sleep (+ timeout-ms 1000))
          (when (get @pending-pages request-id)
            (swap! pending-pages dissoc request-id)
            (log! "page-timeout-cleanup" {:request-id request-id})))
        (let [sent (send-to-agent! agent-id page-msg)]
          (if-not sent
            (do
              (swap! pending-pages dissoc request-id)
              {:ok false :err "failed to send page to agent"})
            (let [result (deref response-promise timeout-ms :timeout)]
              (swap! pending-pages dissoc request-id)
              (if (= result :timeout)
                {:ok false :err "timeout waiting for agent response" :request-id request-id}
                {:ok true :response result :request-id request-id :source :local-handler})))))

      ;; Priority 3: Check WebSocket connected agents
      (get @connected-agents aid)
      (let [request-id (generate-request-id)
            response-promise (promise)
            page-msg {:type "page"
                      :request-id request-id
                      :prompt prompt
                      :timestamp (str (java.time.Instant/now))}]
        (swap! pending-pages assoc request-id
               {:promise response-promise
                :agent-id agent-id
                :created-at (System/currentTimeMillis)})
        (future
          (Thread/sleep (+ timeout-ms 1000))
          (when (get @pending-pages request-id)
            (swap! pending-pages dissoc request-id)
            (log! "page-timeout-cleanup" {:request-id request-id})))
        (let [sent (send-to-agent! agent-id page-msg)]
          (if-not sent
            (do
              (swap! pending-pages dissoc request-id)
              {:ok false :err "failed to send page to agent"})
            (let [result (deref response-promise timeout-ms :timeout)]
              (swap! pending-pages dissoc request-id)
              (if (= result :timeout)
                {:ok false :err "timeout waiting for agent response" :request-id request-id}
                {:ok true :response result :request-id request-id :source :websocket})))))

      :else
      {:ok false :err "agent not connected"})))

(defonce ^:private server-state (atom nil))

(defn start!
  ([] (start! {}))
  ([{:keys [port]}]
   (let [port (or port
                  (some-> (System/getenv "AGENCY_PORT") Long/parseLong)
                  7070)
         stop-fn (http/run-server #'handler {:port port})
         mirror-stop (codex-mirror/start!)]
     (reset! server-state {:stop-fn stop-fn
                           :mirror-stop mirror-stop
                           :port port})
     (log! (format "agency http server on %d" port))
     (println (format "[agency] listening on %d" port))
     (fn []
       (println "[agency] stopping")
       (log! "agency stopping")
       (when-let [stop (:stop-fn @server-state)]
         (stop))
       (when-let [stop-mirror (:mirror-stop @server-state)]
         (stop-mirror))
       (reset! server-state nil)
       (println "[agency] stopped")
       (log! "agency stopped")))))

(defn -main [& _args]
  (let [stop-fn (start! {})]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (try
                                   (stop-fn)
                                   (catch Throwable _))))))
  @(promise))
