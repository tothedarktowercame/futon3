(ns futon3.tatami-store
  "Local EDN store for tatami session events."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)
           (java.time Instant)
           (java.time.temporal ChronoUnit)
           (java.util Date)))

(defonce ^:private tatami-log-path (atom "resources/tatami-events.edn"))
(def ^:private lock (Object.))

(defn set-log-path!
  "Override the log path (useful for tests)."
  [path]
  (reset! tatami-log-path path))

(defn- ensure-path! []
  (let [file (io/file @tatami-log-path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    (when-not (.exists file)
      (spit file "[]\n"))
    file))

(defn load-events
  "Return all logged events (vector)."
  ([]
   (let [file (ensure-path!)]
     (if (.exists file)
       (with-open [reader (PushbackReader. (io/reader file))]
         (or (edn/read {:eof []} reader) []))
       []))))

(defn load-events-since
  "Return events logged after `inst` (inclusive)."
  [inst]
  (let [since (or inst (Date/from (.minus (Instant/now) 1 ChronoUnit/DAYS)))]
    (->> (load-events)
         (filter (fn [event]
                   (let [stamp (:timestamp event)]
                     (and stamp
                          (not (neg? (.compareTo ^Date stamp since)))))))
         vec)))

(defn append-event!
  "Append EVENT to the log, returning the persisted vector."
  [event]
  (locking lock
    (let [file (ensure-path!)
          events (conj (vec (load-events)) event)]
      (spit file (with-out-str (pr events)))
      events)))

(defn reset-events!
  "Reset tatami events log."
  []
  (locking lock
    (let [file (io/file @tatami-log-path)]
      (when (.exists file)
        (spit file "[]\n")))))

;; TODO: FUTON1 integration — ingest tatami events into graph-memory.
;; TODO: Proof chain — aggregate session-proof events into proof-chain.edn.
