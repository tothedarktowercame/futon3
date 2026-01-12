(ns futon3.aif-bridge
  "Bridge between Futon2 AIF traces and FuLab proof system."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3.checks :as checks]))

(defn- normalize-text [value]
  (cond
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        trimmed))
    (keyword? value) (name value)
    :else nil))

(defn- normalize-texts [values]
  (let [items (cond
                (nil? values) []
                (sequential? values) values
                :else [values])]
    (->> items
         (map normalize-text)
         (remove nil?)
         vec)))

(defn- ->double [value]
  (cond
    (number? value) (double value)
    (string? value) (try
                      (Double/parseDouble (str/trim value))
                      (catch Exception _ nil))
    :else nil))

(defn- ->int [value]
  (cond
    (integer? value) (int value)
    (number? value) (int (Math/round (double value)))
    (string? value) (try
                      (Integer/parseInt (str/trim value))
                      (catch Exception _ nil))
    :else nil))

(defn- normalize-status [value]
  (normalize-keyword value))

(def ^:private g-term-keys
  [:risk :ambiguity :info-gain :constraint-violation :cost :coordination-pressure])

(def ^:private g-term-keyset
  (set g-term-keys))

(defn- normalize-keyword [value]
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (keyword (str/lower-case trimmed))))
    :else nil))

(defn- normalize-keywords [values]
  (let [items (cond
                (nil? values) []
                (sequential? values) values
                :else [values])]
    (->> items
         (map normalize-keyword)
         (remove nil?)
         vec)))

(def ^:private observation-vector-keys
  [:test-status
   :compile-status
   :diff-size
   :failing-spec-count
   :user-constraints
   :time-since-anchor
   :contradiction-flags])

(def ^:private precision-registry-normalizers
  {:tests ->double
   :typecheck ->double
   :static-analysis ->double
   :tool-output ->double
   :user-constraints ->double
   :model-inference ->double})

(defn- normalize-contradiction-flags [values]
  (->> values
       normalize-texts
       (map #(keyword (str/lower-case %)))
       vec))

(def ^:private observation-normalizers
  {:test-status normalize-status
   :compile-status normalize-status
   :diff-size ->int
   :failing-spec-count ->int
   :user-constraints normalize-texts
   :time-since-anchor ->double
   :contradiction-flags normalize-contradiction-flags})

(defn- select-observation-keys [value]
  (if (map? value)
    (select-keys value observation-vector-keys)
    {}))

(defn- normalize-observation-vector [observation]
  (letfn [(assoc-if-present [acc key value]
            (cond
              (nil? value) acc
              (and (sequential? value) (empty? value)) acc
              :else (assoc acc key value)))]
    (reduce-kv (fn [acc key normalizer]
                 (assoc-if-present acc key (normalizer (get observation key))))
               {}
               observation-normalizers)))

(defn- normalize-g-breakdown [breakdown]
  (when (map? breakdown)
    (let [entries (->> breakdown
                       (keep (fn [[k v]]
                               (let [term (normalize-keyword k)
                                     score (->double v)]
                                 (when (and term (g-term-keyset term) (some? score))
                                   [term score]))))
                       (into {}))]
      (when (seq entries)
        entries))))

(defn- normalize-term-provenance-entry [term-id entry]
  (when (map? entry)
    (let [term (or term-id (normalize-keyword (:term-id entry)))
          final-contribution (or (->double (:final-contribution entry))
                                 (->double (:value entry))
                                 (->double (:contribution entry)))
          observation-keys (normalize-keywords (:observation-keys entry))
          precision-channels (normalize-keywords (:precision-channels entry))
          intermediate-values (when (map? (:intermediate-values entry))
                                (:intermediate-values entry))]
      (when (and term (some? final-contribution))
        {:term-id term
         :observation-keys observation-keys
         :precision-channels precision-channels
         :intermediate-values (or intermediate-values {})
         :final-contribution final-contribution}))))

(defn- normalize-term-provenance [provenance]
  (let [entries (cond
                  (map? provenance)
                  (if (or (:term-id provenance)
                          (:final-contribution provenance)
                          (:value provenance)
                          (:contribution provenance))
                    [(normalize-term-provenance-entry nil provenance)]
                    (keep (fn [[term entry]]
                            (normalize-term-provenance-entry (normalize-keyword term) entry))
                          provenance))
                  (sequential? provenance)
                  (keep #(normalize-term-provenance-entry nil %) provenance)
                  :else nil)]
    (->> entries
         (remove nil?)
         vec)))

(defn- normalize-precision-registry [registry]
  (when (map? registry)
    (let [normalized (reduce-kv (fn [acc key normalizer]
                                  (let [value (normalizer (get registry key))]
                                    (if (some? value)
                                      (assoc acc key value)
                                      acc)))
                                {}
                                precision-registry-normalizers)]
      (when (seq normalized)
        normalized))))

(defn- extract-observation-vector [step-result]
  (let [observation (or (:observation step-result)
                        (:observation-vector step-result)
                        (get-in step-result [:perception :observation])
                        (get-in step-result [:perception :observation-vector])
                        {})
        merged (merge (select-observation-keys observation)
                      (select-observation-keys step-result))]
    (normalize-observation-vector merged)))

(defn- extract-g-breakdown [step-result]
  (let [candidate (or (:g-breakdown step-result)
                      (:g-terms step-result)
                      (:G-terms step-result)
                      (get-in step-result [:G :terms])
                      (get-in step-result [:aif :g-terms])
                      (get-in step-result [:aif :G-terms]))]
    (normalize-g-breakdown candidate)))

(defn- extract-term-provenance [step-result]
  (let [candidate (or (:g-term-provenance step-result)
                      (:term-provenance step-result)
                      (:G-term-provenance step-result)
                      (get-in step-result [:G :term-provenance])
                      (get-in step-result [:aif :g-term-provenance])
                      (get-in step-result [:aif :term-provenance]))]
    (normalize-term-provenance candidate)))

(defn- extract-precision-registry [step-result]
  (normalize-precision-registry
   (or (:precision-registry step-result)
       (get-in step-result [:perception :precision-registry])
       (get-in step-result [:perception :prec :registry])
       (get-in step-result [:prec :precision-registry])
       (get-in step-result [:prec :registry]))))

(defn- sorted-terms [terms]
  (->> terms
       (remove nil?)
       (sort-by name)
       vec))

(declare extract-evidence)

(defn validate-aif-proof
  "Check if an AIF episode satisfies pattern constraints."
  [episode-trace pattern-id]
  (let [summary (extract-evidence episode-trace)
        check-req {:pattern/id (name pattern-id)
                   :context (str "AIF episode with " (count episode-trace) " ticks")
                   :evidence [summary]
                   :aif-trace summary}]
    (checks/check! check-req)))

(defn- extract-evidence
  "Extract checkable evidence from an AIF trace."
  [episode-trace]
  (let [g-values (keep :G episode-trace)
        tau-values (keep (fn [step]
                           (or (get-in step [:perception :prec :tau])
                               (get-in step [:prec :tau])))
                         episode-trace)
        actions (keep :action episode-trace)
        action-counts (->> actions
                           frequencies
                           (into {} (map (fn [[k v]] [k (int v)]))))
        g-breakdowns (keep extract-g-breakdown episode-trace)
        term-provenance (vec (mapcat extract-term-provenance episode-trace))
        g-mean (if (seq g-values)
                 (/ (reduce + g-values) (double (count g-values)))
                 0.0)
        tau-range (if (seq tau-values)
                    [(apply min tau-values) (apply max tau-values)]
                    [0.0 0.0])
        g-term-means (when (seq g-breakdowns)
                       (let [{:keys [sum count]}
                             (reduce (fn [{:keys [sum count]} breakdown]
                                       {:sum (reduce (fn [acc [term value]]
                                                       (update acc term (fnil + 0.0) value))
                                                     sum
                                                     breakdown)
                                        :count (reduce (fn [acc [term _]]
                                                         (update acc term (fnil inc 0)))
                                                       count
                                                       breakdown)})
                                     {:sum {} :count {}}
                                     g-breakdowns)]
                         (->> sum
                              (map (fn [[term total]]
                                     (let [cnt (get count term 1)]
                                       [term (/ total (double cnt))])))
                              (into {}))))
        g-term-channels (when (seq term-provenance)
                          (->> term-provenance
                               (group-by :term-id)
                               (reduce (fn [acc [term entries]]
                                         (let [observation-keys (->> entries
                                                                     (mapcat :observation-keys)
                                                                     (remove nil?)
                                                                     distinct
                                                                     vec)
                                               precision-channels (->> entries
                                                                       (mapcat :precision-channels)
                                                                       (remove nil?)
                                                                       distinct
                                                                       vec)]
                                           (assoc acc term {:observation-keys observation-keys
                                                            :precision-channels precision-channels})))
                                       {})))
        observation-snapshots (->> episode-trace
                                   (keep (fn [step]
                                           (let [observation (extract-observation-vector step)]
                                             (when (seq observation)
                                               observation))))
                                   vec)
        observation-vector (peek observation-snapshots)
        observation-coverage (when (seq episode-trace)
                               (let [observed (count observation-snapshots)
                                     total (count episode-trace)]
                                 {:observed observed
                                  :total total
                                  :coverage (if (pos? total)
                                              (/ (double observed) total)
                                              0.0)}))
        precision-registry (some extract-precision-registry (reverse episode-trace))
        observed-g-terms (->> g-breakdowns (mapcat keys) set)
        provenance-keys (->> term-provenance (map :term-id) set)
        missing-keys (set/difference observed-g-terms provenance-keys)
        g-term-traceability (when (seq observed-g-terms)
                              {:terms (sorted-terms observed-g-terms)
                               :with-provenance (sorted-terms provenance-keys)
                               :missing-provenance (sorted-terms missing-keys)})
        constraint-violations (->> episode-trace
                                   (keep (fn [step]
                                           (let [ok? (get-in step [:pattern-trace :constraint-ok?] ::missing)]
                                             (when (false? ok?)
                                               (str "pattern-constraint-failed@" (:action step)))))))
        evidence (cond-> {:g-mean g-mean
                          :tau-range tau-range
                          :action-counts action-counts}
                   observation-vector (assoc :observation-vector observation-vector)
                   observation-coverage (assoc :observation-coverage observation-coverage)
                   precision-registry (assoc :precision-registry precision-registry)
                   (seq term-provenance) (assoc :term-provenance term-provenance)
                   (seq g-term-means) (assoc :g-terms g-term-means)
                   (seq g-term-channels) (assoc :g-term-channels g-term-channels)
                   g-term-traceability (assoc :g-term-traceability g-term-traceability))]
    (if (seq constraint-violations)
      (assoc evidence :constraint-violations (vec constraint-violations))
      evidence)))
