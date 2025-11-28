(ns futon3.tatami-schema
  "Tatami event schema helpers shared by futon3 + tatami tooling."
  (:require [clojure.string :as str])
  (:import (java.time Instant)
           (java.util Date UUID)))

(def ^:private default-activities
  #{:tai-chi :coding :band :job-search :pattern-work :agent-work :other})

(def ^:private default-felt-states
  #{:tired :bored :ok :stressed :calm})

(defn- now-inst []
  (Date/from (Instant/now)))

(defn- ->keyword [value]
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)]
                     (when (seq trimmed)
                       (keyword trimmed)))
    :else value))

(defn- normalize-collection [values]
  (->> values
       (map ->keyword)
       (remove nil?)
       vec))

(defn- ensure-activity [activity]
  (let [kw (->keyword activity)]
    (when (and kw (contains? default-activities kw))
      kw)))

(defn- ensure-felt-state [felt]
  (let [kw (->keyword felt)]
    (when (and kw (contains? default-felt-states kw))
      kw)))

(defn- derive-tags [prototypes]
  (->> prototypes
       (map (fn [proto]
              (let [kw (->keyword proto)]
                (cond
                  (and (keyword? kw) (namespace kw)) (keyword (namespace kw))
                  (keyword? kw) kw
                  :else nil))))
       (remove nil?)
       distinct
       vec))

(defn new-event
  "Create a normalized event map for tatami logging.
   KIND is a keyword (e.g. :session-start, :activity, :skip, :session-proof).
   BASE-FIELDS is a partial map; this fn fills :id, :timestamp, and optionally :session-id."
  [kind base-fields]
  (let [session-id (or (:session-id base-fields)
                       (when (= kind :session-start)
                         (UUID/randomUUID)))
        prototypes (normalize-collection (:prototypes base-fields))
        futon-tags (normalize-collection (or (:futon-tags base-fields)
                                             (derive-tags prototypes)))]
    (cond-> (-> base-fields
                (assoc :id (UUID/randomUUID)
                       :kind kind
                       :timestamp (now-inst))
                (update :activity ensure-activity)
                (update :felt-state ensure-felt-state)
                (assoc :prototypes prototypes
                       :futon-tags futon-tags))
      session-id (assoc :session-id session-id))))

;; TODO: FUTON1 integration — emit schema metadata for graph-memory ingest.
