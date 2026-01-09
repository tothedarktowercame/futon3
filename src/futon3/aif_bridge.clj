(ns futon3.aif-bridge
  "Bridge between Futon2 AIF traces and FuLab proof system."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3.checks :as checks]
            [futon3.tatami-schema :as schema]))

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
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (keyword (str/lower-case trimmed))))
    :else nil))

(def ^:private g-term-keys
  [:risk :ambiguity :info-gain :constraint-violation :cost :coordination-pressure])

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

(defn- normalize-belief-state [mu]
  (when (map? mu)
    (let [goal (normalize-text (:goal mu))
          subgoals (normalize-texts (:subgoals mu))
          current-hypothesis (normalize-text (:current-hypothesis mu))
          blockers (normalize-texts (:blockers mu))
          pattern-fit (->double (:pattern-fit mu))
          expected-next-observation (normalize-text (:expected-next-observation mu))
          uncertainty (->double (:uncertainty mu))]
      (cond-> {}
        goal (assoc :goal goal)
        (seq subgoals) (assoc :subgoals subgoals)
        current-hypothesis (assoc :current-hypothesis current-hypothesis)
        (seq blockers) (assoc :blockers blockers)
        (some? pattern-fit) (assoc :pattern-fit pattern-fit)
        expected-next-observation (assoc :expected-next-observation expected-next-observation)
        (some? uncertainty) (assoc :uncertainty uncertainty)))))

(defn- normalize-observation-vector [observation]
  (let [test-status (normalize-status (:test-status observation))
        compile-status (normalize-status (:compile-status observation))
        diff-size (->int (:diff-size observation))
        failing-spec-count (->int (:failing-spec-count observation))
        user-constraints (normalize-texts (:user-constraints observation))
        time-since-anchor (->double (:time-since-anchor observation))
        contradiction-flags (->> (:contradiction-flags observation)
                                 normalize-texts
                                 (map #(keyword (str/lower-case %)))
                                 vec)]
    (cond-> {}
      test-status (assoc :test-status test-status)
      compile-status (assoc :compile-status compile-status)
      (some? diff-size) (assoc :diff-size diff-size)
      (some? failing-spec-count) (assoc :failing-spec-count failing-spec-count)
      (seq user-constraints) (assoc :user-constraints user-constraints)
      (some? time-since-anchor) (assoc :time-since-anchor time-since-anchor)
      (seq contradiction-flags) (assoc :contradiction-flags contradiction-flags))))

(defn- normalize-g-breakdown [breakdown]
  (when (map? breakdown)
    (let [entries (->> breakdown
                       (keep (fn [[k v]]
                               (let [term (normalize-keyword k)
                                     score (->double v)]
                                 (when (and term (some #{term} g-term-keys) (some? score))
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
  (let [tests (->double (:tests registry))
        typecheck (->double (:typecheck registry))
        static-analysis (->double (:static-analysis registry))
        tool-output (->double (:tool-output registry))
        user-constraints (->double (:user-constraints registry))
        model-inference (->double (:model-inference registry))
        normalized (cond-> {}
                     (some? tests) (assoc :tests tests)
                     (some? typecheck) (assoc :typecheck typecheck)
                     (some? static-analysis) (assoc :static-analysis static-analysis)
                     (some? tool-output) (assoc :tool-output tool-output)
                     (some? user-constraints) (assoc :user-constraints user-constraints)
                     (some? model-inference) (assoc :model-inference model-inference))]
    (when (seq normalized)
      normalized)))

(defn- extract-observation-vector [step-result]
  (let [observation (or (:observation step-result)
                        (:observation-vector step-result)
                        (get-in step-result [:perception :observation])
                        {})
        merged (merge observation
                      (select-keys step-result
                                   [:test-status
                                    :compile-status
                                    :diff-size
                                    :failing-spec-count
                                    :user-constraints
                                    :time-since-anchor
                                    :contradiction-flags]))]
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

(defn- diff-map [previous next]
  (let [prev (or previous {})
        nxt (or next {})
        keys (set/union (set (keys prev)) (set (keys nxt)))]
    (->> keys
         (keep (fn [k]
                 (let [before (get prev k ::missing)
                       after (get nxt k ::missing)]
                   (when (not= before after)
                     [k {:from (when-not (= before ::missing) before)
                         :to (when-not (= after ::missing) after)}]))))
         (into {}))))

(defn- mu-diff [previous next]
  (diff-map previous next))

(defn- observation-delta [previous next]
  (diff-map previous next))

(defn- sorted-terms [terms]
  (->> terms
       (remove nil?)
       (sort-by name)
       vec))

(defn aif-step->event
  "Convert an AIF step result to a FuLab event."
  [step-result session-id]
  (let [raw-mu (get-in step-result [:perception :mu])
        mu (normalize-belief-state raw-mu)
        prev-mu (normalize-belief-state
                 (or (:mu-prev step-result)
                     (:previous-mu step-result)
                     (get-in step-result [:perception :mu-prev])
                     (get-in step-result [:perception :previous-mu])))
        belief-delta (when (and prev-mu mu)
                       (mu-diff prev-mu mu))
        observation (extract-observation-vector step-result)
        prev-observation (normalize-observation-vector
                          (or (:observation-prev step-result)
                              (:previous-observation step-result)
                              (get-in step-result [:perception :observation-prev])
                              (get-in step-result [:perception :previous-observation])))
        observation-change (when (and (seq prev-observation) (seq observation))
                             (observation-delta prev-observation observation))
        precision-registry (extract-precision-registry step-result)
        mu-validation (schema/validate-aif-belief-state mu)
        observation-validation (schema/validate-aif-observation-vector observation)
        precision-validation (when precision-registry
                               (schema/validate-aif-precision-registry precision-registry))
        g-breakdown (extract-g-breakdown step-result)
        g-breakdown-validation (when g-breakdown
                                 (schema/validate-aif-g-breakdown g-breakdown))
        term-provenance (extract-term-provenance step-result)
        term-provenance-validation (when (seq term-provenance)
                                     (schema/validate-aif-term-provenance term-provenance))
        base {:session-id session-id
              :mu mu
              :mu/validation mu-validation
              :tau (or (get-in step-result [:perception :prec :tau])
                       (get-in step-result [:prec :tau]))
              :action (:action step-result)
              :G (:G step-result)
              :pattern-id (get-in step-result [:pattern-trace :id])
              :observation observation
              :observation/validation observation-validation}]
    (schema/new-event :aif/tick
                      (cond-> base
                        (seq belief-delta) (assoc :belief-delta belief-delta)
                        (seq observation-change) (assoc :observation-delta observation-change)
                        precision-registry (assoc :precision-registry precision-registry
                                                  :precision/validation precision-validation)
                        g-breakdown (assoc :g-breakdown g-breakdown
                                           :g-breakdown/validation g-breakdown-validation)
                        (seq term-provenance) (assoc :g-term-provenance term-provenance
                                                     :g-term-provenance/validation
                                                     term-provenance-validation)))))

(defn clock-in-from-aif
  "Generate a clock-in event from AIF ant spawn."
  [ant pattern-id session-id]
  (schema/new-event :clock-in/start
                    {:session-id session-id
                     :clock-in/pattern-id pattern-id
                     :clock-in/mu (:mu ant)
                     :clock-in/tau (get-in ant [:prec :tau])
                     :clock-in/intent (str "AIF agent with " (name pattern-id))}))

(defn clock-out-from-aif
  "Generate a clock-out event from AIF episode end."
  [episode-summary session-id]
  (schema/new-event :clock-out/complete
                    {:session-id session-id
                     :session/status (if (:success? episode-summary) :done :blocked)
                     :pattern/trail (:patterns-used episode-summary)
                     :artifacts [{:type :aif-trace
                                  :g-mean (:g-mean episode-summary)
                                  :tau-range (:tau-range episode-summary)
                                  :actions-taken (:action-counts episode-summary)}]}))

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
        term-provenance (mapcat extract-term-provenance episode-trace)
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
                                               observation)))))
        observation-vector (first (reverse observation-snapshots))
        observation-coverage (when (seq episode-trace)
                               (let [observed (count observation-snapshots)
                                     total (count episode-trace)]
                                 {:observed observed
                                  :total total
                                  :coverage (if (pos? total)
                                              (/ (double observed) total)
                                              0.0)}))
        precision-registry (some extract-precision-registry (reverse episode-trace))
        g-term-keys (->> g-breakdowns (mapcat keys) set)
        provenance-keys (->> term-provenance (map :term-id) set)
        missing-keys (set/difference g-term-keys provenance-keys)
        g-term-traceability (when (seq g-term-keys)
                              {:terms (sorted-terms g-term-keys)
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
                   (seq g-term-means) (assoc :g-terms g-term-means)
                   (seq g-term-channels) (assoc :g-term-channels g-term-channels)
                   g-term-traceability (assoc :g-term-traceability g-term-traceability))]
    (if (seq constraint-violations)
      (assoc evidence :constraint-violations (vec constraint-violations))
      evidence)))
