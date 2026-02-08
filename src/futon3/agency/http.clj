(ns futon3.agency.http
  "HTTP adapter for Agency service."
  (:require [cheshire.core :as json]
            [clj-http.client :as http-client]
            [clojure.set :as cset]
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
         handle-whistle handle-whistle-response
         handle-standup handle-rendezvous-status)

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

        ;; Rendezvous status (GET /agency/rendezvous/:id)
        (and (= method :get) (str/starts-with? uri "/agency/rendezvous/"))
        (let [rendezvous-id (subs uri (count "/agency/rendezvous/"))]
          (json-response (handle-rendezvous-status rendezvous-id)))

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

        ;; Whistle a connected agent (sync - wait for response)
        ;; "Bells and whistles" - bell is async, whistle is sync
        (or (= uri "/agency/whistle") (= uri "/agency/page"))
        (json-response (handle-whistle (parse-json-body req)))

        ;; Timed standup - whistle all agents, collect responses
        (= uri "/agency/standup")
        (json-response (handle-standup (parse-json-body req)))

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

;; Rendezvous state - tracks multi-agent handshake completion
;; {secret-id -> {:room :prompt :deadline-ms :expected-agents
;;                :acks {agent-id -> {:timestamp :value}}
;;                :created-at}}
(defonce ^:private rendezvous-state (atom {}))

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
        ;; If this secret belongs to a rendezvous, record agent-specific ack
        (when (get @rendezvous-state secret-id)
          (swap! rendezvous-state update-in [secret-id :acks]
                 assoc (some-> agent-id str) ack))
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

        ;; Accept both "whistle-response" and "page-response" (backwards compat)
        ("whistle-response" "page-response")
        (let [{:keys [request-id response]} msg]
          (if request-id
            (handle-whistle-response request-id response)
            (log! "ws-whistle-response-missing-id" {:agent-id agent-id})))

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
;; Whistle endpoint - synchronous request to connected agent
;; ("Bells and whistles" - bell is async fire-and-forget, whistle is sync)
;; =============================================================================

(defonce ^:private pending-whistles (atom {}))
;; {request-id -> {:promise p :agent-id aid :created-at inst}}

(defn- generate-request-id []
  (str "req-" (subs (str (java.util.UUID/randomUUID)) 0 12)))

(defn handle-whistle-response
  "Called by agents (local handlers or via WS) to deliver a whistle response.
   Returns true if the response was delivered, false if request not found/expired."
  [request-id response]
  (if-let [entry (get @pending-whistles request-id)]
    (do
      (deliver (:promise entry) response)
      (swap! pending-whistles dissoc request-id)
      (log! "whistle-response-delivered" {:request-id request-id})
      true)
    (do
      (log! "whistle-response-orphaned" {:request-id request-id})
      false)))

(def ^:private default-whistle-timeout-ms
  (or (some-> (System/getenv "AGENCY_WHISTLE_TIMEOUT_MS")  Long/parseLong)
      (some-> (System/getenv "AGENCY_PAGE_TIMEOUT_MS") Long/parseLong) ; backwards compat
      60000)) ; 1 minute default

(defn- handle-whistle [body]
  (let [{:keys [agent-id prompt timeout-ms]} body
        timeout-ms (or timeout-ms default-whistle-timeout-ms)
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
            whistle-msg {:type "page"
                      :request-id request-id
                      :prompt prompt
                      :timestamp (str (java.time.Instant/now))}]
        (swap! pending-whistles assoc request-id
               {:promise response-promise
                :agent-id agent-id
                :created-at (System/currentTimeMillis)})
        (future
          (Thread/sleep (+ timeout-ms 1000))
          (when (get @pending-whistles request-id)
            (swap! pending-whistles dissoc request-id)
            (log! "whistle-timeout-cleanup" {:request-id request-id})))
        (let [sent (send-to-agent! agent-id whistle-msg)]
          (if-not sent
            (do
              (swap! pending-whistles dissoc request-id)
              {:ok false :err "failed to send whistle to agent"})
            (let [result (deref response-promise timeout-ms :timeout)]
              (swap! pending-whistles dissoc request-id)
              (if (= result :timeout)
                {:ok false :err "timeout waiting for agent response" :request-id request-id}
                {:ok true :response result :request-id request-id :source :local-handler})))))

      ;; Priority 3: Check WebSocket connected agents
      (get @connected-agents aid)
      (let [request-id (generate-request-id)
            response-promise (promise)
            whistle-msg {:type "page"
                      :request-id request-id
                      :prompt prompt
                      :timestamp (str (java.time.Instant/now))}]
        (swap! pending-whistles assoc request-id
               {:promise response-promise
                :agent-id agent-id
                :created-at (System/currentTimeMillis)})
        (future
          (Thread/sleep (+ timeout-ms 1000))
          (when (get @pending-whistles request-id)
            (swap! pending-whistles dissoc request-id)
            (log! "whistle-timeout-cleanup" {:request-id request-id})))
        (let [sent (send-to-agent! agent-id whistle-msg)]
          (if-not sent
            (do
              (swap! pending-whistles dissoc request-id)
              {:ok false :err "failed to send whistle to agent"})
            (let [result (deref response-promise timeout-ms :timeout)]
              (swap! pending-whistles dissoc request-id)
              (if (= result :timeout)
                {:ok false :err "timeout waiting for agent response" :request-id request-id}
                {:ok true :response result :request-id request-id :source :websocket})))))

      :else
      {:ok false :err "agent not connected"})))

;; =============================================================================
;; MUSN chat posting (for IRC bridge integration)
;; =============================================================================

(def ^:private musn-url
  (or (System/getenv "FUTON3_MUSN_URL") "http://localhost:6065"))

(defn- post-to-musn-room! [room agent-id text]
  (try
    (http-client/post (str musn-url "/musn/chat/message")
      {:content-type :json
       :throw-exceptions false
       :body (json/generate-string
              {:room room
               :msg-id (str (java.util.UUID/randomUUID))
               :author {:id agent-id :name agent-id}
               :text text})})
    (log! "musn-post" {:room room :agent-id agent-id})
    (catch Exception e
      (log! "musn-post-error" {:room room :agent-id agent-id :error (.getMessage e)}))))

;; =============================================================================
;; Rendezvous - multi-agent handshake coordination
;; =============================================================================

(defn- handle-rendezvous-status [rendezvous-id]
  (if-let [rv (get @rendezvous-state rendezvous-id)]
    (let [acked (set (keys (:acks rv)))
          expected (set (:expected-agents rv))
          missing (vec (cset/difference expected acked))
          elapsed (- (System/currentTimeMillis) (:created-at rv))]
      {:ok true
       :rendezvous {:id rendezvous-id
                    :room (:room rv)
                    :expected (vec expected)
                    :acked (vec acked)
                    :missing missing
                    :complete (empty? missing)
                    :elapsed-ms elapsed
                    :deadline-ms (:deadline-ms rv)}})
    {:ok false :err "rendezvous not found"}))

;; =============================================================================
;; Standup endpoint - rendezvous-based multi-agent check-in
;; =============================================================================

(defn- handle-standup
  "Start a rendezvous-based standup. Bells all agents with standup info,
   returns immediately with a rendezvous-id for status polling.

   Each agent independently:
   1. Acks the bell (proves reception)
   2. Generates its own standup update
   3. Posts to MUSN room as itself (self-attribution)

   Request body:
     :prompt      - Standup prompt (default: status check-in)
     :deadline-ms - Ack deadline in ms (default: 120000 = 2 min)
     :agents      - Optional list of agent-ids (default: all connected)
     :room        - MUSN/IRC room name (default: \"standup\")"
  [body]
  (let [room (or (:room body) "standup")
        prompt (or (:prompt body) "Standup: What are you working on? Any blockers? 2-3 sentences.")
        deadline-ms (or (:deadline-ms body) 120000)
        agent-ids (or (:agents body) (connected-agent-ids))
        ;; Create the rendezvous
        secret-result (handle-create-secret {:ttl-ms (+ deadline-ms 60000)})
        secret-id (:secret-id secret-result)]
    (if (empty? agent-ids)
      {:ok false :err "no agents connected"}
      (do
        ;; Record rendezvous state
        (swap! rendezvous-state assoc secret-id
               {:room room :prompt prompt :deadline-ms deadline-ms
                :expected-agents (vec (map name agent-ids))
                :acks {} :created-at (System/currentTimeMillis)})
        ;; Schedule cleanup
        (future
          (Thread/sleep (+ deadline-ms 120000))
          (swap! rendezvous-state dissoc secret-id))
        ;; Bell all agents with rendezvous info
        (let [bell-msg {:type "bell"
                        :bell-type "standup"
                        :secret-id secret-id
                        :payload {:room room :prompt prompt
                                  :musn-url musn-url
                                  :deadline-ms deadline-ms}}
              results (mapv (fn [aid]
                              {:agent-id aid :sent (send-to-agent! aid bell-msg)})
                            agent-ids)]
          ;; Post announcement to MUSN as "agency"
          (future (post-to-musn-room! room "agency" "Standup started. Awaiting check-ins."))
          {:ok true
           :rendezvous-id secret-id
           :secret-value (:value secret-result)
           :room room
           :deadline-ms deadline-ms
           :agents results})))))

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
