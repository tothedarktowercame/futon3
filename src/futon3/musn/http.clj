(ns futon3.musn.http
  "HTTP adapter for MUSN service. Provides JSON endpoints for session + turn lifecycle."
  (:require [cheshire.core :as json]
            [org.httpkit.server :as http]
            [clojure.string :as str]
            [futon3.musn.service :as svc]))

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
    nil))

(defn handler [req]
  (try
    (let [uri (:uri req)]
      (cond
        (= uri "/health") (json-response {:ok true})
        (not= :post (:request-method req)) (json-response 405 {:ok false :err "method not allowed"})
        :else
        (if-let [resp (dispatch uri (parse-json-body req))]
          (json-response resp)
          (json-response 404 {:ok false :err "not found"}))))
    (catch Throwable t
      (json-response 400 {:ok false
                          :err (.getMessage t)
                          :type (keyword (.. t getClass getSimpleName))}))))

(defn -main [& _args]
  (let [port (Long/parseLong (or (System/getenv "MUSN_PORT") "6065"))]
    (println (format "MUSN HTTP server on %d" port))
    (http/run-server handler {:port port})
    (println "Press Ctrl+C to exit.")
    @(promise)))
