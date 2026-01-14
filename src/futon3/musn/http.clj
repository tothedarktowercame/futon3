(ns futon3.musn.http
  "HTTP adapter for MUSN service. Provides JSON endpoints for session + turn lifecycle."
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]
            [org.httpkit.server :as http]
            [clojure.string :as str]
            [futon3.aif.viz :as aif-viz]
            [futon3.musn.service :as svc]))

(def log-path (or (System/getenv "MUSN_LOG") "/tmp/musn_http.log"))

(defn- log!
  "Append a timestamped line to the MUSN log file, ignore failures."
  [msg & more]
  (try
    (spit log-path
          (str (java.time.Instant/now) " " msg
               (when (seq more) (str " " (pr-str more)))
               "\n")
          :append true)
    (catch Throwable _)))

(defn- keywordize-enums
  "Convert certain string enum values to keywords so they pass schema validation."
  [data]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {}
             (map (fn [[k v]]
                    (cond
                      (and (#{:mode :source :cost} k) (string? v))
                      [k (keyword v)]

                      :else [k v])))
             x)
       x))
   data))

(defn- jsonable
  "Transform response payload into JSON-friendly values (e.g., stringifying Instants)."
  [data]
  (walk/postwalk
   (fn [x]
     (cond
       (instance? java.time.Instant x) (str x)
       :else x))
   data))

(defn- parse-json-body [req]
  (let [body (slurp (:body req))]
    (when (seq body)
      (-> (json/parse-string body true)
          (keywordize-enums)))))

(defn- json-response
  ([payload] (json-response 200 payload))
  ([status payload]
   {:status status
    :headers {"content-type" "application/json"}
    :body (json/generate-string (jsonable payload))}))

(defn- dispatch [uri body]
  (case uri
    "/musn/session/create" (svc/create-session! body)
    "/musn/turn/start"    (svc/turn-start! body)
    "/musn/turn/plan"     (svc/turn-plan! body)
    "/musn/turn/select"   (svc/turn-select! body)
    "/musn/turn/action"   (svc/turn-action! body)
    "/musn/turn/use"      (svc/turn-use! body)
    "/musn/evidence/add"  (svc/evidence-add! body)
    "/musn/turn/end"      (svc/turn-end! body)
    "/musn/turn/resume"   (svc/turn-resume! body)
    "/musn/session/state" (svc/session-state! body)
    nil))

(defn handler [req]
  (try
    (let [uri (:uri req)]
      (cond
        (= uri "/health") (json-response {:ok true})
        (not= :post (:request-method req)) (json-response 405 {:ok false :err "method not allowed"})
        :else
        (let [body (parse-json-body req)]
          (when (= uri "/musn/turn/end")
            (log! "[musn-http] turn/end"
                  {:session/id (:session/id body)
                   :turn (:turn body)
                   :remote-addr (:remote-addr req)
                   :user-agent (get-in req [:headers "user-agent"])
                   :client (get-in req [:headers "x-musn-client"])
                   :content-length (get-in req [:headers "content-length"])}))
          (if-let [resp (dispatch uri body)]
            (json-response resp)
            (json-response 404 {:ok false :err "not found"})))))
    (catch Throwable t
      (log! "[musn-http] handler error" {:uri (:uri req)
                                         :err (.getMessage t)})
      (json-response 400 {:ok false
                          :err (.getMessage t)
                          :type (keyword (.. t getClass getSimpleName))}))))

(defn -main [& _args]
  (let [port (Long/parseLong (or (System/getenv "MUSN_PORT") "6065"))]
    (when-let [viz-port (some-> (System/getenv "MUSN_AIF_VIZ_PORT") str/trim not-empty)]
      (aif-viz/start! {:port (Long/parseLong viz-port)}))
    (binding [*out* *err*]
      (println (format "MUSN HTTP server on %d" port))
      (println (format "Logging to %s" log-path)))
    (log! (format "MUSN HTTP server on %d" port))
    (http/run-server handler {:port port})
    (binding [*out* *err*]
      (println "Press Ctrl+C to exit."))
    @(promise)))
