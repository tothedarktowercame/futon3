(ns futon3.tatami-schema
  "Tatami event schema helpers shared by futon3 + tatami tooling."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me])
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
    (cond
      (and kw (contains? default-activities kw)) kw
      kw :other
      :else nil)))

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

;; --- AIF schemas ---

(def aif-belief-state-schema
  [:map {:closed true}
   [:goal {:optional true} [:or :string :keyword]]
   [:subgoals {:optional true} [:vector [:or :string :keyword]]]
   [:current-hypothesis {:optional true} [:or :string :keyword]]
   [:blockers {:optional true} [:vector [:or :string :keyword]]]
   [:pattern-fit {:optional true} :double]
   [:expected-next-observation {:optional true} [:or :string :keyword]]
   [:uncertainty {:optional true} :double]])

(def aif-observation-vector-schema
  [:map {:closed true}
   [:test-status {:optional true} [:or :string :keyword]]
   [:compile-status {:optional true} [:or :string :keyword]]
   [:diff-size {:optional true} :int]
   [:failing-spec-count {:optional true} :int]
   [:user-constraints {:optional true} [:vector :string]]
   [:time-since-anchor {:optional true} :double]
   [:contradiction-flags {:optional true} [:vector [:or :string :keyword]]]])

(def aif-precision-registry-schema
  [:map {:closed true}
   [:tests {:optional true} :double]
   [:typecheck {:optional true} :double]
   [:static-analysis {:optional true} :double]
   [:tool-output {:optional true} :double]
   [:user-constraints {:optional true} :double]
   [:model-inference {:optional true} :double]])

(def aif-g-breakdown-schema
  [:map {:closed true}
   [:risk {:optional true} :double]
   [:ambiguity {:optional true} :double]
   [:info-gain {:optional true} :double]
   [:constraint-violation {:optional true} :double]
   [:cost {:optional true} :double]
   [:coordination-pressure {:optional true} :double]])

(def aif-term-provenance-entry-schema
  [:map {:closed true}
   [:term-id [:or :string :keyword]]
   [:observation-keys [:vector [:or :string :keyword]]]
   [:precision-channels [:vector [:or :string :keyword]]]
   [:intermediate-values [:map {:closed false}]]
   [:final-contribution :double]])

(def aif-term-provenance-schema
  [:vector aif-term-provenance-entry-schema])

(defn validate-aif-belief-state
  "Validate an AIF belief state map, returning {:ok? bool :errors map?}."
  [mu]
  (if (m/validate aif-belief-state-schema mu)
    {:ok? true}
    {:ok? false
     :errors (me/humanize (m/explain aif-belief-state-schema mu))}))

(defn validate-aif-observation-vector
  "Validate an AIF observation vector map, returning {:ok? bool :errors map?}."
  [observation]
  (if (m/validate aif-observation-vector-schema observation)
    {:ok? true}
    {:ok? false
     :errors (me/humanize (m/explain aif-observation-vector-schema observation))}))

(defn validate-aif-precision-registry
  "Validate an AIF precision registry map, returning {:ok? bool :errors map?}."
  [registry]
  (if (m/validate aif-precision-registry-schema registry)
    {:ok? true}
    {:ok? false
     :errors (me/humanize (m/explain aif-precision-registry-schema registry))}))

(defn validate-aif-g-breakdown
  "Validate an AIF G breakdown map, returning {:ok? bool :errors map?}."
  [breakdown]
  (if (m/validate aif-g-breakdown-schema breakdown)
    {:ok? true}
    {:ok? false
     :errors (me/humanize (m/explain aif-g-breakdown-schema breakdown))}))

(defn validate-aif-term-provenance
  "Validate an AIF term provenance collection, returning {:ok? bool :errors map?}."
  [provenance]
  (if (m/validate aif-term-provenance-schema provenance)
    {:ok? true}
    {:ok? false
     :errors (me/humanize (m/explain aif-term-provenance-schema provenance))}))

;; TODO: FUTON1 integration — emit schema metadata for graph-memory ingest.
