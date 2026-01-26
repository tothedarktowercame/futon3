(ns futon3.futon2-bridge
  "Emit lightweight viriya deltas for futon2 dashboards."
  (:require [clojure.java.io :as io]
            [f0.clock :as clock])
  (:import (java.util UUID)))

(def default-log-path "futon3/logs/viriya.edn")

(defonce ^:private log-path (atom default-log-path))
(def ^:private lock (Object.))

(def ^:private default-status->delta
  {:applies 1.0
   :needs-evidence 0.0
   :needs-alignment 0.0
   :needs-context 0.0})

(defn set-log-path!
  "Override the viriya log destination (tests)."
  [path]
  (reset! log-path path))

(defn current-log-path []
  @log-path)

(defn- ensure-log-file! [path]
  (let [file (io/file path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    file))

(defn- append-log! [path entry]
  (locking lock
    (let [file (ensure-log-file! path)]
      (spit file (str (pr-str entry) "\n") :append true)
      entry)))

(defn- random-viriya-id []
  (str "VIR-" (.substring (str (UUID/randomUUID)) 0 10)))

(defn record-check!
  "Record a viriya delta for a check result. Config supports:
   {:enabled? bool :log-path path :status->delta {status delta}}"
  [config result]
  (let [cfg (or config {})
        enabled? (if (contains? cfg :enabled?) (:enabled? cfg) true)
        status->delta (or (:status->delta cfg) default-status->delta)
        path (or (:log-path cfg) (current-log-path))]
    (when (and enabled? (:ok result))
      (let [proof (:proof result)
            status (:status result)
            delta (double (get status->delta status 0.0))
            entry {:viriya/id (random-viriya-id)
                   :viriya/recorded (clock/->iso-string)
                   :viriya/delta delta
                   :check/status status
                   :pattern/id (:pattern/id proof)
                   :proof/id (:proof/id proof)
                   :proof/run-id (:proof/run-id proof)}]
        {:ok true
         :entry (append-log! path entry)
         :log-path path}))))
