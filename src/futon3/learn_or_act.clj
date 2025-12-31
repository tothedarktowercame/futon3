(ns futon3.learn-or-act
  "Learn-or-act orchestration when no obvious pattern can be applied."
  (:require [clojure.string :as str]
            [futon3.cue-embedding :as cue]
            [futon3.gap-store :as gap-store]
            [futon3.pattern-hints :as hints])
  (:import (java.time Instant)
           (java.util Date UUID)))

(def ^:dynamic *pattern-search*
  "Allow tests to stub the underlying search implementation."
  hints/hints)

(def ^:private default-thresholds
  {:score/good 0.78
   :score/maybe 0.55
   :score/low 0.25})

(def ^:private candidate-limit 6)
(def ^:private max-triage-passes 3)

(def ^:private representation-question
  {:prompt "Which audience or role best describes the request?"
   :options [{:id :audience/leads :label "Strategic leads / sponsors"}
             {:id :audience/teams :label "Delivery teams / maintainers"}
             {:id :audience/researchers :label "Researchers / practitioners"}]
   :instruction "Pick one so I can frame the pattern search."})

(defn- ->instant []
  (Date/from (Instant/now)))

(defn- distance->similarity [distance]
  (if (number? distance)
    (-> 1.0 (- (min 1.0 (double distance))) (max 0.0))
    0.0))

(defn- format-candidate [entry]
  (let [similarity (distance->similarity (:score entry))]
    {:pattern/id (:id entry)
     :pattern/title (:title entry)
     :pattern/summary (:summary entry)
     :pattern/sigils (:sigils entry)
     :score/distance (:score entry)
     :score/similarity similarity}))

(defn- expand-hints
  [{:keys [pattern-limit fruit-limit paramita-limit intent] :as request}]
  (let [search-opts (-> request
                        (dissoc :thresholds :triage/history)
                        (assoc :pattern-limit (or pattern-limit candidate-limit)))
        raw (*pattern-search* search-opts)
        candidates (mapv format-candidate (:patterns raw))
        base {:patterns (:patterns raw)
              :fruits (:fruits raw)
              :paramitas (:paramitas raw)
              :search/candidates candidates
              :search/best-score (or (:score/similarity (first candidates)) 0.0)}
        cue-opts (cond-> {:intent intent
                          :patterns (:patterns raw)}
                    fruit-limit (assoc :fruit-limit fruit-limit)
                    paramita-limit (assoc :paramita-limit paramita-limit))
        cues (cue/intent-pattern-cues cue-opts)]
    (if cues
      (assoc base
             :fruits (:fruits cues)
             :paramitas (:paramitas cues)
             :cue/intent cues)
      base)))

(defn- score-class [score thresholds]
  (let [good (:score/good thresholds)
        maybe (:score/maybe thresholds)
        low (:score/low thresholds)]
    (cond
      (>= score good) :good
      (>= score maybe) :maybe
      (>= score low) :low
      :else :very-low)))

(defn- blankish? [value]
  (or (nil? value)
      (and (string? value) (str/blank? value))))

(defn- derive-intent-sigils [{:keys [intent]}]
  (let [text (some-> intent str/trim)]
    (when (seq text)
      (when-let [cues (cue/intent-pattern-cues {:intent text
                                                :patterns (hints/all-patterns)
                                                :max-patterns 5})]
        (->> (:sigils cues)
             (remove #(or (nil? (:emoji %)) (nil? (:hanzi %))))
             (map (fn [pair]
                    {:emoji (:emoji pair)
                     :hanzi (:hanzi pair)}))
             distinct
             vec)))))

(defn- merge-intent-sigils [request]
  (if-let [sigils (derive-intent-sigils request)]
    (update request :sigils
            (fn [existing]
              (let [current (vec (or existing []))
                    merged (concat sigils current)]
                (vec (distinct merged)))))
    request))

(defn- representation-ready? [request]
  (or (seq (:sigils request))
      (seq (:prototypes request))))

(defn- patch-representation [request]
  (cond-> request
    (blankish? (:audience request)) (assoc :audience :unknown)
    (blankish? (:role request)) (assoc :role :unknown)
    (and (not (seq (:prototypes request))) (seq (:recent-prototypes request)))
    (assoc :prototypes (vec (:recent-prototypes request)))))

(def ^:private sigil-lexicon
  [{:pattern #"license" :sigil {:emoji "📥" :hanzi "文"}}
   {:pattern #"platform|portal" :sigil {:emoji "🎶" :hanzi "工"}}
   {:pattern #"care|wellbeing" :sigil {:emoji "🍵" :hanzi "心"}}
   {:pattern #"rhythm|cadence" :sigil {:emoji "🔃" :hanzi "心"}}
   {:pattern #"community|citizen" :sigil {:emoji "👫" :hanzi "人"}}
   {:pattern #"bridge|interoperability" :sigil {:emoji "🚢" :hanzi "门"}}])

(defn- derive-context-sigil [context]
  (let [lc (str/lower-case (or context ""))]
    (some (fn [{:keys [pattern sigil]}]
            (when (re-find pattern lc) sigil))
          sigil-lexicon)))

(defn- query-perturbation [request]
  (let [sigil (derive-context-sigil (:context request))]
    (cond-> (assoc request :pattern-limit (max candidate-limit 8))
      sigil (update :sigils #(vec (distinct (conj (or % []) sigil)))))))

(defn- analogical-hop [request candidates]
  (if-let [sigils (seq (:pattern/sigils (first candidates)))]
    (assoc request :sigils sigils)
    request))

(defn- representation-patch-pass [request]
  (patch-representation request))

(def ^:private triage-passes
  [{:name :query-perturbation :exec (fn [ctx _] (query-perturbation ctx))}
   {:name :analogical-hop :exec (fn [ctx state] (analogical-hop ctx (:search/candidates state)))}
   {:name :representation-patch :exec (fn [ctx _] (representation-patch-pass ctx))}])

(defn- ambiguous-candidates? [candidates thresholds]
  (when (>= (count candidates) 2)
    (let [{a :score/similarity} (first candidates)
          {b :score/similarity} (second candidates)]
      (and (>= a (:score/maybe thresholds))
           (< (Math/abs ^double (- a b)) 0.08)))))

(defn- build-question [candidates]
  (let [labels ["A" "B" "C"]]
    {:prompt "No single pattern fits. Which direction feels closer right now?"
     :options (mapv (fn [label cand]
                      {:choice label
                       :pattern/id (:pattern/id cand)
                       :label (:pattern/title cand)})
                    labels
                    (take 3 candidates))}))

(defn- truncate [s limit]
  (let [text (or s "unspecified situation")]
    (if (> (count text) limit)
      (str (subs text 0 limit) "…")
      text)))

(defn- determine-gap-title [ctx summary]
  (or (:desired-title ctx)
      (when-let [intent (:intent ctx)]
        (str "Stub: " intent))
      (str "Stub: " (truncate summary 48))))

(defn- build-gap [ctx candidates reason]
  (let [summary (truncate (:context ctx) 160)
        id (UUID/randomUUID)
        title (determine-gap-title ctx summary)
        why (-> [:low-score]
                (cond-> (blankish? (:domain ctx)) (conj :missing-domain))
                vec)
        gap {:gap/id id
             :gap/timestamp (->instant)
             :gap/context (if (str/blank? summary) "unspecified" summary)
             :gap/why (if (seq (:gap/extra reason))
                        (vec (distinct (concat why (:gap/extra reason))))
                        why)
             :gap/nearest-patterns (mapv :pattern/id (take 3 candidates))
             :gap/proposed-title title
             :gap/tags [:proto :needs-pattern]}]
    (gap-store/append-gap! gap)
    gap))

(defn- build-stub [gap candidates]
  (let [context (:gap/context gap)
        title (:gap/proposed-title gap)
        neighbour-names (map :pattern/title (take 2 candidates))
        moves [(str "Write a sharper problem statement for " context)
               (if (seq neighbour-names)
                 (str "Borrow tactics from " (str/join " / " neighbour-names) " and note what fails.")
                 "List two analogous wins from your past work and extract the steps.")
               "Capture evidence/metrics that will prove the stub solved the gap."]
        success [(str "Team agrees the stub captures the missing capability for " context)
                 "A follow-up doc or pattern draft is scheduled."]]
    {:stub/id (:gap/id gap)
     :stub/title title
     :stub/context context
     :stub/problem (str "Need an actionable guide for " context)
     :stub/first-moves moves
     :stub/success success
     :stub/status :stub}))

(defn- representation-response []
  {:patterns []
   :fruits []
   :paramitas []
   :search/best-score 0.0
   :search/candidates []
   :search/attempts 0
   :failure/mode :failure/represent
   :triage/question representation-question
   :stub/current nil
   :gap/current nil
   :message "Need a bit more structure (audience/role) before I can search."})

(defn- retrieval-question-response [state attempts]
  (assoc state
         :search/attempts attempts
         :failure/mode :failure/retrieve
         :triage/question (build-question (:search/candidates state))
         :message "Search returned several nearby patterns. Which direction should I chase?"))

(defn- success-response [state attempts]
  (assoc state
         :search/attempts attempts
         :failure/mode nil
         :stub/current nil
         :gap/current nil))

(defn- gap-response [state attempts gap stub]
  (-> state
      (assoc :search/attempts attempts
             :failure/mode :failure/gap
             :gap/current gap
             :stub/current stub
             :message "No existing pattern matched. Entered Gap Mode and generated a provisional stub.")))

(defn decide
  "Evaluate REQUEST (sigils/prototypes/context keys) and either return a pattern
   recommendation, a clarifying action, or a gap/stub declaration."
  [request]
  (let [thresholds (merge default-thresholds (:thresholds request))
        prepared (-> request
                     patch-representation
                     merge-intent-sigils)]
    (if-not (representation-ready? prepared)
      (representation-response)
      (let [initial (expand-hints prepared)
            attempts 1
            class (score-class (:search/best-score initial) thresholds)]
        (cond
          (= class :good)
          (success-response initial attempts)

          (= class :maybe)
          (loop [passes (take max-triage-passes triage-passes)
                 ctx prepared
                 state initial
                 attempt attempts]
            (let [best (:search/best-score state)]
              (cond
                (>= best (:score/good thresholds))
                (success-response state attempt)

                (empty? passes)
                (if (ambiguous-candidates? (:search/candidates state) thresholds)
                  (retrieval-question-response state attempt)
                  (let [gap (build-gap prepared (:search/candidates state) {:gap/extra [:triage-exhausted]})
                        stub (build-stub gap (:search/candidates state))]
                    (gap-response state attempt gap stub)))

                :else
                (let [{:keys [exec]} (first passes)
                      next-ctx (exec ctx state)
                      next-state (expand-hints next-ctx)]
                  (recur (rest passes) next-ctx next-state (inc attempt))))))

          :else
          (let [gap (build-gap prepared (:search/candidates initial)
                               {:gap/extra (when (= class :very-low)
                                             [:no-candidates])})
                stub (build-stub gap (:search/candidates initial))]
            (gap-response initial attempts gap stub)))))))
