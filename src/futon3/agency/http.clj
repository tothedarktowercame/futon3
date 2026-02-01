(ns futon3.agency.http
  "HTTP adapter for Agency service."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [futon3.agency.codex-mirror :as codex-mirror]
            [futon3.agency.service :as svc]))

(def ^:private log-path (or (System/getenv "AGENCY_LOG") "/tmp/agency_http.log"))

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

(defn- handle-run [body]
  (let [{:keys [agent-id peripheral prompt musn forum resume-id thread-id cwd approval-policy no-sandbox]} body
        resume-id (or resume-id
                      (when (and thread-id (nil? forum))
                        thread-id))]
    (if (or (str/blank? (str agent-id))
            (str/blank? (str peripheral))
            (str/blank? (str prompt)))
      {:ok false :err "missing required fields: agent-id, peripheral, prompt"}
      (svc/run-peripheral! {:agent-id agent-id
                            :peripheral peripheral
                            :prompt prompt
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

        ;; Secret retrieval (GET /agency/secret/:id)
        (and (= method :get) (str/starts-with? uri "/agency/secret/"))
        (let [secret-id (subs uri (count "/agency/secret/"))]
          (json-response (handle-get-secret secret-id)))

        (not= :post method)
        (json-response 405 {:ok false :err "method not allowed"})

        ;; Secret creation (POST /agency/secret)
        (= uri "/agency/secret")
        (json-response (handle-create-secret (parse-json-body req)))

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

(defn- handle-get-secret [secret-id]
  (if-let [entry (get @secrets secret-id)]
    (do
      ;; One-time retrieval - delete after fetch
      (swap! secrets dissoc secret-id)
      {:ok true :value (:value entry)})
    {:ok false :err "secret not found or expired"}))

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
     (fn []
       (when-let [stop (:stop-fn @server-state)]
         (stop))
       (when-let [stop-mirror (:mirror-stop @server-state)]
         (stop-mirror))
       (reset! server-state nil)))))

(defn -main [& _args]
  (start! {})
  @(promise))
