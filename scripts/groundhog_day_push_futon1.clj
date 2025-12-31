#!/usr/bin/env clojure
(require '[cheshire.core :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[org.httpkit.client :as http])

(def base-url (or (System/getenv "FUTON1_API_BASE") "http://localhost:8080/api/alpha"))
(def profile (or (System/getenv "FUTON1_PROFILE") "testing"))
(def ndjson-path (or (System/getenv "GROUNDHOG_NDJSON") "dev/groundhog_day.ndjson"))

(def headers {"Content-Type" "application/json"
              "X-Profile" profile})

(defn futon1-post!
  [path payload]
  (let [url (str base-url path)
        resp @(http/post url {:headers headers
                              :body (json/encode payload
                                                {:escape-non-ascii true})})]
    (when (>= (:status resp) 300)
      (throw (ex-info (str "Futon1 POST failed " (:status resp))
                      {:url url :body (:body resp)})))
    resp))

(def run-name (format "groundhog-day-%s" (subs (str (java.time.LocalDate/now)) 0 10)))

(defn ensure-run! []
  (futon1-post! "/entity" {:name run-name
                             :type ":demo/groundhog-run"}))

(defn event-name [msg-id]
  (str run-name "/" msg-id))

(defn normalize-type [frame]
  (case (:type frame)
    "workday" ":demo/workday"
    "check" ":demo/check"
    "hello" ":demo/hello"
    "event" ":demo/event"
    "bye" ":demo/bye"
    ":demo/event"))

(defn ensure-event!
  [frame]
  (let [name (event-name (get-in frame [:payload :msg-id] "anon"))
        payload {:name name
                 :type (normalize-type frame)
                 :external-id (get-in frame [:payload :msg-id])
                 :notes (get-in frame [:payload :notes])}]
    (futon1-post! "/entity" payload)
    (futon1-post! "/relation" {:type ":demo/run-event"
                                 :src run-name
                                 :dst name})
    (when (= (normalize-type frame) ":demo/check")
      (when-let [pattern (get-in frame [:payload :pattern/id])]
        (futon1-post! "/relation" {:type ":demo/check-pattern"
                                     :src name
                                     :dst pattern})))
    name))

(defn parse-ndjson []
  (with-open [r (io/reader ndjson-path)]
    (->> (line-seq r)
         (remove str/blank?)
         (map #(json/parse-string % true))
         vec)))

(defn -main [& _]
  (when-not (.exists (io/file ndjson-path))
    (throw (ex-info "NDJSON not found" {:path ndjson-path})))
  (println "Posting Groundhog Day run to Futon1 profile" profile)
  (ensure-run!)
  (doseq [frame (parse-ndjson)]
    (ensure-event! frame)
    (println "Synced" (:type frame) (get-in frame [:payload :msg-id])))
  (println "Done."))
