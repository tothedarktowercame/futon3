(ns scripts.musn_http_preflight
  "HTTP preflight against a running MUSN HTTP server. Fails fast on halts or bad responses."
  (:require [clj-http.client :as http]
            [cheshire.core :as json]))

(defn endpoint [path]
  (str (or (System/getenv "MUSN_URL") "http://localhost:6065") path))

(defn post! [path payload]
  (let [resp (http/post (endpoint path)
                        {:content-type :json
                         :accept :json
                         :throw-exceptions false
                         :body (json/generate-string payload)})
        body (some-> resp :body (json/parse-string true))]
    (if (= 200 (:status resp))
      body
      (throw (ex-info "HTTP error" {:status (:status resp) :body body})))))

(def ^:private pattern-id "musn/plan-before-tool")
(def ^:private pattern-file "library/musn/plan-before-tool.flexiarg")

(defn ensure-ok! [label resp]
  (when (or (:halt? resp) (false? (:ok resp)))
    (throw (ex-info (str "halt during " label) resp)))
  resp)

(defn -main [& _]
  (try
    (let [{sid :session/id} (post! "/musn/session/create"
                                   {:client {:id "musn-http-preflight"}})]
      (println "[http-preflight] session" sid)
      (post! "/musn/turn/start" {:session/id sid :turn 1 :hud {:candidates [pattern-id]}})
      (post! "/musn/turn/plan" {:session/id sid :turn 1 :plan "http preflight"})
      (post! "/musn/turn/select" {:session/id sid
                                  :turn 1
                                  :candidates [pattern-id]
                                  :chosen pattern-id
                                  :reason {:mode :use
                                           :reads []
                                           :note "preflight"
                                           :source :system}})
      (ensure-ok! "read"
                  (post! "/musn/turn/action" {:session/id sid
                                              :turn 1
                                              :pattern/id pattern-id
                                              :action "read"
                                              :note "preflight read"}))
      (ensure-ok! "implement"
                  (post! "/musn/turn/action" {:session/id sid
                                              :turn 1
                                              :pattern/id pattern-id
                                              :action "implement"
                                              :files [pattern-file]
                                              :note "preflight implement"}))
      (post! "/musn/evidence/add" {:session/id sid
                                   :turn 1
                                   :pattern/id pattern-id
                                   :files [pattern-file]
                                   :note "first test: http preflight"})
      (let [end-res (post! "/musn/turn/end" {:session/id sid :turn 1})]
        (ensure-ok! "turn/end" end-res)
        (println "[http-preflight] summary" (:summary end-res))))
    (println "[http-preflight] ok")
    (catch Throwable t
      (binding [*out* *err*]
        (println "[http-preflight] FAILED" (.getMessage t))
        (when-let [data (ex-data t)]
          (prn data)))
      (System/exit 1))))
