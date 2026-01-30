(ns futon3.agency.http
  "HTTP adapter for Agency service."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as http]
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
  (let [{:keys [agent-id peripheral prompt musn forum thread-id cwd approval-policy no-sandbox]} body]
    (if (or (str/blank? (str agent-id))
            (str/blank? (str peripheral))
            (str/blank? (str prompt)))
      {:ok false :err "missing required fields: agent-id, peripheral, prompt"}
      (svc/run-peripheral! {:agent-id agent-id
                            :peripheral peripheral
                            :prompt prompt
                            :musn musn
                            :forum forum
                            :thread-id thread-id
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

        (not= :post method)
        (json-response 405 {:ok false :err "method not allowed"})

        (= uri "/agency/run")
        (json-response (handle-run (parse-json-body req)))

        (= uri "/agency/rollover")
        (json-response (handle-rollover (parse-json-body req)))

        :else
        (json-response 404 {:ok false :err "not found"})))
    (catch Throwable t
      (log! "handler error" {:err (.getMessage t)})
      (json-response 400 {:ok false :err (.getMessage t)}))))

(defonce ^:private server-state (atom nil))

(defn start!
  ([] (start! {}))
  ([{:keys [port]}]
   (let [port (or port
                  (some-> (System/getenv "AGENCY_PORT") Long/parseLong)
                  7070)
         stop-fn (http/run-server #'handler {:port port})]
     (reset! server-state {:stop-fn stop-fn :port port})
     (log! (format "agency http server on %d" port))
     (fn []
       (when-let [stop (:stop-fn @server-state)]
         (stop)
         (reset! server-state nil))))))

(defn -main [& _args]
  (start! {})
  @(promise))
