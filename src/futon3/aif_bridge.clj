(ns futon3.aif-bridge
  "Bridge between Futon2 AIF traces and FuLab proof system."
  (:require [futon3.checks :as checks]
            [futon3.tatami-schema :as schema]))

(defn aif-step->event
  "Convert an AIF step result to a FuLab event."
  [step-result session-id]
  (schema/new-event :aif/tick
                    {:session-id session-id
                     :mu (get-in step-result [:perception :mu])
                     :tau (or (get-in step-result [:perception :prec :tau])
                              (get-in step-result [:prec :tau]))
                     :action (:action step-result)
                     :G (:G step-result)
                     :pattern-id (get-in step-result [:pattern-trace :id])}))

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
        g-mean (if (seq g-values)
                 (/ (reduce + g-values) (double (count g-values)))
                 0.0)
        tau-range (if (seq tau-values)
                    [(apply min tau-values) (apply max tau-values)]
                    [0.0 0.0])
        constraint-violations (->> episode-trace
                                   (keep (fn [step]
                                           (let [ok? (get-in step [:pattern-trace :constraint-ok?] ::missing)]
                                             (when (false? ok?)
                                               (str "pattern-constraint-failed@" (:action step)))))))
        evidence {:g-mean g-mean
                  :tau-range tau-range
                  :action-counts action-counts}]
    (if (seq constraint-violations)
      (assoc evidence :constraint-violations (vec constraint-violations))
      evidence)))
