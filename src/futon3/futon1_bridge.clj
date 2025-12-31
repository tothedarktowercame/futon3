(ns futon3.futon1-bridge
  "Send FUTON3 workday + check artifacts to Futon1 via the HTTP API."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http]))

(def ^:private ensured-runs (atom #{}))

(defn- normalize-base [api-base]
  (when (seq api-base)
    (str/replace api-base #"/+$" "")))

(defn- enabled? [config]
  (let [base (normalize-base (:api-base config))]
    (and (:enabled? config) (seq base))))

(defn- headers [config]
  (cond-> {"content-type" "application/json"}
    (seq (:profile config)) (assoc "x-profile" (:profile config))))

(defn- post-json!
  [config path payload]
  (let [url (str (normalize-base (:api-base config)) path)
        timeout (or (:timeout-ms config) 2000)
        body (json/encode payload {:escape-non-ascii true})]
    (http/post url {:headers (headers config)
                    :timeout timeout
                    :body body}
               (fn [{:keys [error status]}]
                 (when error
                   (println "[futon1-bridge]" (.getMessage error)))
                 (when (and status (>= status 400))
                   (println "[futon1-bridge] HTTP" status "for" url))))))

(defn- ensure-run!
  [config run-id]
  (let [run-name (or run-id "futon3-demo/run")]
    (when (enabled? config)
      (when-not (contains? @ensured-runs run-name)
        (swap! ensured-runs conj run-name)
        (post-json! config "/entity"
                    {:name run-name
                     :type ":demo/run"
                     :external-id run-name}))
      run-name)))

(defn record-workday!
  "Send a workday entry (from futon3.workday/submit!) to Futon1."
  [config entry]
  (when (enabled? config)
    (try
      (let [run (ensure-run! config (:workday/run-id entry))
            name (:workday/id entry)
            entity {:name name
                    :type ":workday/entry"
                    :external-id (:workday/msg-id entry)
                    :notes (:workday/activity entry)
                    :payload entry}]
        (post-json! config "/entity" entity)
        (when run
          (post-json! config "/relation" {:type ":demo/run-event"
                                            :src run
                                            :dst name}))
        (doseq [e (:workday/evidence entry)]
          (let [eid (str name "#" (hash e))]
            (post-json! config "/entity"
                        {:name eid :type ":workday/evidence" :notes e})
            (post-json! config "/relation"
                        {:type ":workday/supports"
                         :src name
                         :dst eid}))))
      (catch Exception ex
        (println "[futon1-bridge] workday error" (.getMessage ex))))))

(defn record-check!
  "Send a check result (from futon3.checks/check!)."
  [config result]
  (when (and (enabled? config) (:ok result))
    (try
      (let [proof (:proof result)
            run (ensure-run! config (:proof/run-id proof))
            name (:proof/id proof)
            entity {:name name
                    :type ":check/proof"
                    :external-id (:pattern/id proof)
                    :notes (:pattern/title proof)
                    :payload proof}]
        (post-json! config "/entity" entity)
        (when run
          (post-json! config "/relation" {:type ":demo/run-event"
                                            :src run :dst name}))
        (when-let [pattern (:pattern/id proof)]
          (post-json! config "/relation" {:type ":check/pattern"
                                            :src name :dst pattern}))
        (post-json! config "/relation" {:type ":check/status"
                                          :src name
                                          :dst (name (:status result))})
        (doseq [missing (:missing result)]
          (post-json! config "/relation"
                      {:type ":check/missing"
                       :src name
                       :dst (name missing)})))
      (catch Exception ex
        (println "[futon1-bridge] check error" (.getMessage ex))))))
