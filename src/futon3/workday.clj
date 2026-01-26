(ns futon3.workday
  "Workday submission intake + local logging so FUTON3 can hand evidence to FUTON1 later."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [f0.clock :as clock])
  (:import (java.util UUID)))

(def default-log-path "futon3/logs/workday.edn")

(defonce ^:private log-path (atom default-log-path))
(def ^:private lock (Object.))

(defn set-log-path!
  "Override the workday log path (mainly for tests)."
  [path]
  (reset! log-path path))

(defn current-log-path []
  @log-path)

(defn- ensure-log-file! []
  (let [file (io/file @log-path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    file))

(defn- append-log! [entry]
  (locking lock
    (let [file (ensure-log-file!)]
      (spit file (str (pr-str entry) "\n") :append true)
      entry)))

(defn- random-id []
  (str "WD-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn- blankish? [s]
  (or (nil? s)
      (and (string? s)
           (str/blank? s))))

(defn- normalize-evidence [items]
  (->> items
       (keep (fn [item]
               (cond
                 (string? item) (str/trim item)
                 (map? item) (str (get item :label) ": " (get item :url))
                 :else nil)))
       (remove str/blank?)
       vec))

(defn- base-entry [{:keys [payload client msg-id run-id source]}]
  (let [activity (:activity payload)
        evidence (normalize-evidence (:evidence payload))
        timestamp (or (:t payload) (clock/->iso-string))
        actor (or (:actor payload)
                  (:name client)
                  (:id client)
                  "anonymous")
        workday-id (or (:id payload) (random-id))
        now (clock/->iso-string)]
    {:workday/id workday-id
     :workday/msg-id msg-id
     :workday/run-id run-id
     :workday/actor actor
     :workday/activity activity
     :workday/evidence evidence
     :workday/aif-trace (:aif-trace payload)
     :workday/t timestamp
     :workday/received now
     :workday/source (or source :ws)
     :workday/client-id (:id client)
     :workday/prototypes (vec (:prototypes payload))
     :workday/sigils (vec (:sigils payload))
     :workday/tags (vec (:tags payload))
     :workday/notes (:notes payload)
     :workday/raw payload}))

(defn submit!
  "Validate PAYLOAD and append it to the local log when LOG? is true.
   Returns {:ok bool ...} so the caller can surface errors."
  [{:keys [payload client msg-id run-id source log?]
    :or {log? true}}]
  (if (blankish? (:activity payload))
    {:ok false
     :err "invalid-payload"
     :details {:missing [:activity]}}
    (let [entry (base-entry {:payload payload
                             :client client
                             :msg-id msg-id
                             :run-id run-id
                             :source source})
          persisted (if log? (append-log! entry) entry)]
      {:ok true
       :status :accepted
       :entry persisted
       :log-path (when log? (current-log-path))
       :blocked-by (when log? [:f1.persistence])
       :note (if log?
               "Queued locally; forward to FUTON1 graph when adapter is ready."
               "Forwarded to FUTON1 graph.")})))

(defn check-request
  "If the payload asked for an immediate pattern check, build the request map.
   The result can be passed directly to `futon3.checks/check!`."
  [payload entry]
  (let [check (:check payload)
        pattern-id (or (:pattern/id check)
                       (:pattern payload)
                       (:pattern/id payload))]
    (when (and pattern-id (not (blankish? pattern-id)))
      {:pattern/id pattern-id
       :context (or (:context check)
                    (:activity payload)
                    (:workday/activity entry))
       :evidence (or (:evidence check)
                     (:workday/evidence entry))
       :aif-trace (or (:aif-trace check)
                      (:aif-trace payload)
                      (:workday/aif-trace entry))
       :sigils (or (:sigils check) (:workday/sigils entry))
       :prototypes (or (:prototypes check) (:workday/prototypes entry))
       :origin {:source :workday
                :workday/id (:workday/id entry)
                :msg-id (:workday/msg-id entry)}
       :run-id (:workday/run-id entry)})))
