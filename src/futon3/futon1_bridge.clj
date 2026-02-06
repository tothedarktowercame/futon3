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

;; =============================================================================
;; Lab Session Persistence
;; =============================================================================

(def ^:private lab-save-timestamps (atom {}))
(def ^:private lab-save-interval-ms (* 20 60 1000)) ;; 20 minutes

(defn- should-save? [session-id trigger]
  "Determine if we should save based on trigger type and last save time."
  (case trigger
    :par true  ;; Always save on PAR
    :context-compacted true  ;; Always save on context compaction
    :periodic
    (let [last-save (get @lab-save-timestamps session-id 0)
          now (System/currentTimeMillis)]
      (> (- now last-save) lab-save-interval-ms))
    ;; default
    false))

(defn- mark-saved! [session-id]
  (swap! lab-save-timestamps assoc session-id (System/currentTimeMillis)))

(defn- extract-session-summary [session]
  "Extract key metrics from session for the lab doc."
  (let [events (:events session)
        turns (filter #(#{:turn/user :turn/agent} (:event/type %)) events)
        pars (filter #(= :session/par (:event/type %)) events)
        psrs (filter #(= :pattern/selection-claimed (:event/type %)) events)
        purs (filter #(= :pattern/use-claimed (:event/type %)) events)
        affects (filter #(= :affect/transition (:event/type %)) events)
        timestamps (->> events (keep :at) sort)]
    {:turn-count (count turns)
     :par-count (count pars)
     :psr-count (count psrs)
     :pur-count (count purs)
     :affect-count (count affects)
     :event-count (count events)
     :timestamp-start (first timestamps)
     :timestamp-end (last timestamps)}))

(defn record-lab-session!
  "Persist a lab session checkpoint to Futon1 XTDB.

   Trigger can be:
   - :par - PAR submission checkpoint
   - :context-compacted - Claude Code context compaction
   - :periodic - 20-minute interval save

   Returns {:ok true} on success or {:ok false :error ...} on failure."
  [config session-id session trigger]
  (when (and (enabled? config) (should-save? session-id trigger))
    (try
      (let [summary (extract-session-summary session)
            doc (merge
                 {:lab/session-id session-id
                  :session/id session-id
                  :session/agent (:session/agent session)
                  :lab/trigger (name trigger)
                  :lab/saved-at (java.util.Date.)
                  :lab/timestamp-start (:timestamp-start summary)
                  :lab/timestamp-end (:timestamp-end summary)}
                 (dissoc summary :timestamp-start :timestamp-end)
                 ;; Include events for full replay capability
                 {:events (:events session)})]
        (post-json! config "/api/alpha/lab/session" doc)
        (mark-saved! session-id)
        (println "[futon1-bridge] lab session saved:" session-id "trigger:" trigger)
        {:ok true :session-id session-id :trigger trigger})
      (catch Exception ex
        (println "[futon1-bridge] lab session error:" (.getMessage ex))
        {:ok false :error (.getMessage ex)}))))

(defn lab-save-status
  "Get the last save timestamp for a session."
  [session-id]
  (get @lab-save-timestamps session-id))
