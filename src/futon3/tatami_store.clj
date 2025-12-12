(ns futon3.tatami-store
  "Local EDN store for tatami session events."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)
           (java.time Instant)
           (java.time.temporal ChronoUnit)
           (java.util Date UUID)))

(defonce ^:private tatami-log-path (atom "resources/tatami-events.edn"))
(defonce ^:private tatami-json-log-path (atom "resources/tatami/logs/sessions.jsonl"))
(def ^:private lock (Object.))

(defn set-log-path!
  "Override the log path (useful for tests)."
  [path]
  (reset! tatami-log-path path))

(defn set-json-log-path!
  "Override the supplemental JSON log path (useful for tests)."
  [path]
  (reset! tatami-json-log-path path))

(defn- ensure-json-log! []
  (when-let [path @tatami-json-log-path]
    (let [file (io/file path)
          parent (.getParentFile file)
          existed (.exists file)]
      (when parent (.mkdirs parent))
      (when-not existed
        (spit file ""))
      {:file file
       :created? (not existed)})))

(defn- keyword->string [kw]
  (if-let [ns (namespace kw)]
    (str ns "/" (name kw))
    (name kw)))

(defn- json-scalar [value]
  (cond
    (keyword? value) (keyword->string value)
    (symbol? value) (name value)
    (instance? UUID value) (str value)
    (instance? Date value) (.toString (.toInstant ^Date value))
    (instance? Instant value) (.toString ^Instant value)
    :else value))

(defn- json-friendly [value]
  (cond
    (map? value)
    (into {}
          (map (fn [[k v]]
                 [(json-scalar k) (json-friendly v)])
               value))
    (set? value) (mapv json-friendly value)
    (sequential? value) (mapv json-friendly value)
    :else (json-scalar value)))

(defn- append-json-event! [json-info event]
  (when-let [file (:file json-info)]
    (spit file (str (json/generate-string (json-friendly event)) "\n") :append true)))

(defn- rewrite-json-log! [json-info events]
  (when-let [file (:file json-info)]
    (with-open [writer (io/writer file)]
      (doseq [event events]
        (.write writer (json/generate-string (json-friendly event)))
        (.write writer "\n")))))

(defn- ensure-path! []
  (let [file (io/file @tatami-log-path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    (when-not (.exists file)
      (spit file "[]\n"))
    {:edn file
     :json (ensure-json-log!)}))

(defn- read-edn-events [file]
  (if (.exists file)
    (with-open [reader (PushbackReader. (io/reader file))]
      (or (edn/read {:eof []} reader) []))
    []))

(defn load-events
  "Return all logged events (vector)."
  ([]
   (let [{:keys [edn json]} (ensure-path!)]
     (let [events (read-edn-events edn)]
       (when (and json (:created? json) (seq events))
         (rewrite-json-log! json events))
       events))))

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
    (let [{:keys [edn json]} (ensure-path!)
          events (conj (vec (read-edn-events edn)) event)]
      (spit edn (with-out-str (pr events)))
      (append-json-event! json event)
      events)))

(defn reset-events!
  "Reset tatami events log."
  []
  (locking lock
    (let [{:keys [edn json]} (ensure-path!)]
      (when (.exists edn)
        (spit edn "[]\n"))
      (when-let [file (:file json)]
        (spit file "")))))

;; TODO: FUTON1 integration — ingest tatami events into graph-memory.
;; TODO: Proof chain — aggregate session-proof events into proof-chain.edn.
