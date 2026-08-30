(ns ablate-g-snatch
  "Packet B-prime: score five-round Snatch policies on total-score
   distributions, sanity-check risk against expected score, then ablate EIG.
   Packet B's terminal-leaf risk remains in :terminal-leaf-baseline."
  (:require [clojure.pprint :as pprint]
            [derive-q-snatch :as derive]
            [how-kernel-snatch :as kernel]
            [playout-snatch :as snatch]))

(defn pi-always-abstain [_s _patterns] {:act :abstain})
(def policies (assoc derive/all-policies :always-abstain pi-always-abstain))

(defn feasible? [policy-id treatment]
  (not (and (= policy-id :always-abstain) (= treatment :g2))))

;; S-G3, declared once. The carrier is enumerated from finite five-round runs;
;; C(score) is proportional to exp(score), without fitted parameters.
(def preference-spec
  {:form :C-score-proportional-to-exp-score
   :carrier :total-score-after-five-rounds
   :rounds 5
   :line-stopped
   {:rule :use-recorded-total-score :status :declared-stipulation
    :why :stopped-trajectories-retain-the-score-earned-before-stop}})

(def dispositions [:sharer :snatcher :cautious])
(defn trajectory [policy treatment disposition]
  (snatch/play policy treatment disposition 5))
(defn total-score [trace] (:score (last trace)))

(defn score-carrier [treatment]
  (->> (for [[policy-id policy] policies
             :when (feasible? policy-id treatment)
             disposition dispositions]
         (total-score (trajectory policy treatment disposition)))
       set sort vec))

(def score-carriers
  (into (sorted-map)
        (map (fn [treatment] [treatment (score-carrier treatment)]))
        (keys snatch/treatments)))

(defn score-preferences [carrier]
  (let [weights (into {} (map (fn [score] [score (Math/exp score)]) carrier))
        z (reduce + (vals weights))]
    (into (sorted-map) (map (fn [[score weight]] [score (/ weight z)]) weights))))

(def C
  (into (sorted-map)
        (map (fn [[treatment carrier]]
               [treatment (score-preferences carrier)]))
        score-carriers))

(defn score-q [prior policy treatment]
  (reduce (fn [q [disposition mass]]
            (if (pos? mass)
              (update q (total-score (trajectory policy treatment disposition))
                      (fnil + 0.0) mass)
              q))
          (into (sorted-map) (map (fn [score] [score 0.0])
                                  (get score-carriers treatment)))
          prior))

(defn entropy-of [distribution] (kernel/entropy distribution))
(defn normalize [distribution]
  (let [z (reduce + (vals distribution))]
    (if (pos? z)
      (into {} (map (fn [[k v]] [k (/ v z)])) distribution)
      distribution)))

(defn posterior-update
  "Bayes over p2-response from the observed action/outcome, never the true
   disposition used to generate the trajectory."
  [posterior {:keys [action outcome]}]
  (if (= action :offer)
    (normalize
     (into {} (map (fn [[disposition mass]]
                     [disposition
                      (if (= outcome (snatch/p2-response disposition)) mass 0.0)]))
           posterior))
    posterior))

(defn expected-eig [prior policy treatment]
  (let [h0 (entropy-of prior)
        rows (for [[disposition mass] prior :when (pos? mass)
                   :let [path (reductions posterior-update prior
                                          (trajectory policy treatment disposition))]]
               {:mass mass
                :round-1 (- h0 (entropy-of (or (second path) (first path))))
                :terminal (- h0 (entropy-of (last path)))})]
    {:round-1 (reduce + (map #(* (:mass %) (:round-1 %)) rows))
     :terminal (reduce + (map #(* (:mass %) (:terminal %)) rows))}))

(defn kl-divergence [q c]
  (reduce + (for [[value mass] q :when (pos? mass)]
              (* mass (Math/log (/ mass (get c value)))))))
(defn expected-score [q]
  (reduce + (map (fn [[score mass]] (* score mass)) q)))

;; Packet B baseline, retained so the carrier correction is inspectable.
(def legacy-payoff
  {:O1 0.0 :O2 1.0 :O3 0.0 :O4 -1.0
   :O4a -1.0 :O4b -1.0 :O5 3.0 :line-stopped 0.0})
(defn legacy-c [treatment]
  (let [carrier (conj (get derive/outcome-space treatment) :line-stopped)
        weights (into {} (map (fn [leaf] [leaf (Math/exp (legacy-payoff leaf))])
                              carrier))
        z (reduce + (vals weights))]
    (into (sorted-map) (map (fn [[leaf weight]] [leaf (/ weight z)]) weights))))
(defn legacy-risk [prior-id prior policy-id policy treatment]
  (let [cell (derive/derive-cell prior-id prior policy-id policy treatment :terminal)
        q (merge-with + (:Q cell) (:outside-mass cell))]
    (kl-divergence q (legacy-c treatment))))

(defn term-cell [prior-id prior policy-id policy treatment]
  (if-not (feasible? policy-id treatment)
    {:policy policy-id :treatment treatment :prior prior-id
     :status :infeasible :why :abstention-not-in-g2-action-space}
    (let [q (score-q prior policy treatment)
          risk (kl-divergence q (get C treatment))
          eig (expected-eig prior policy treatment)]
      {:policy policy-id :treatment treatment :prior prior-id :status :scored
       :Q-score q :expected-score (expected-score q) :risk risk
       :eig (:terminal eig) :eig-round-1 (:round-1 eig)
       :G (- risk (:terminal eig))
       :terminal-leaf-risk (legacy-risk prior-id prior policy-id policy treatment)})))

(def epsilon 1.0e-12)
(defn approximately= [a b]
  (< (Math/abs (- (double a) (double b))) epsilon))
(defn spread [xs]
  (let [lo (apply min xs) hi (apply max xs) width (- hi lo)]
    {:min lo :max hi :range (if (approximately= width 0.0) 0.0 width)}))
(defn scored-cells [terms prior-id treatment]
  (filter #(and (= :scored (:status %)) (= prior-id (:prior %))
                (= treatment (:treatment %))) terms))

(defn ranking [cells value-key direction]
  (->> cells
       (sort-by (juxt (fn [cell] (* direction (double (value-key cell)))) :policy))
       (mapv (fn [cell] {:policy (:policy cell) :value (value-key cell)}))))
(defn ranking-agrees? [cells]
  (every? true?
          (for [a cells b cells
                :when (> (:expected-score a) (+ (:expected-score b) epsilon))]
            (< (:risk a) (+ (:risk b) epsilon)))))
(defn sanity-cell [terms prior-id treatment]
  (let [cells (scored-cells terms prior-id treatment)]
    {:prior prior-id :treatment treatment
     :expected-score-ranking (ranking cells :expected-score -1.0)
     :risk-ranking (ranking cells :risk 1.0)
     :agrees? (ranking-agrees? cells)}))
(defn range-cell [terms prior-id treatment]
  (let [cells (scored-cells terms prior-id treatment)]
    {:prior prior-id :treatment treatment
     :pragmatic (spread (map :risk cells))
     :epistemic (spread (map :eig cells))}))
(defn minimizers [cells value-key]
  (let [best (apply min (map value-key cells))]
    (->> cells (filter #(approximately= best (value-key %)))
         (map :policy) sort vec)))

(defn ablation-cell [terms ranges prior-id treatment]
  (let [cells (scored-cells terms prior-id treatment)
        range-row (some #(when (= [prior-id treatment]
                                  ((juxt :prior :treatment) %)) %) ranges)
        epistemic-range (get-in range-row [:epistemic :range])
        pragmatic-range (get-in range-row [:pragmatic :range])
        can-ablate? (pos? epistemic-range)
        argmin-g (when can-ablate? (minimizers cells :G))
        argmin-risk (when can-ablate? (minimizers cells :risk))
        moved? (when can-ablate? (not= argmin-g argmin-risk))]
    {:treatment treatment :prior prior-id :argmin-G argmin-g
     :argmin-risk argmin-risk :moved? moved?
     :nonDegenerate? (boolean (and can-ablate? (pos? pragmatic-range) moved?))
     :status (if can-ablate? :ablated :not-ablated)
     :why (when-not can-ablate? :epistemic-term-has-zero-range)}))

(defn predicted-move? [prior-id treatment]
  (and (= prior-id :snatcher-dominant) (not= treatment :g2)))
(defn prediction-report [ablation]
  (let [cells (mapv (fn [cell]
                      (let [predicted (predicted-move? (:prior cell) (:treatment cell))]
                        {:prior (:prior cell) :treatment (:treatment cell)
                         :predicted-moved? predicted
                         :observed-moved? (boolean (:moved? cell))
                         :matches? (= predicted (boolean (:moved? cell)))}))
                    ablation)
        matches (count (filter :matches? cells))]
    {:registered {:at "2026-08-30" :by :claude-15
                  :claim :moves-only-for-snatcher-dominant-outside-g2
                  :probe-expected-score "6p - 1; crosses abstention at p = 1/6"}
     :cells cells
     :observed (cond (= matches (count cells)) :confirmed
                     (zero? matches) :refuted
                     :else :partly-confirmed)}))

(defn base-report []
  (let [terms (vec (for [[prior-id prior] derive/priors
                         [policy-id policy] policies
                         treatment (sort (keys snatch/treatments))]
                     (term-cell prior-id prior policy-id policy treatment)))
        sanity (vec (for [prior-id (keys derive/priors)
                          treatment (sort (keys snatch/treatments))]
                      (sanity-cell terms prior-id treatment)))
        ranges (vec (for [prior-id (keys derive/priors)
                          treatment (sort (keys snatch/treatments))]
                      (range-cell terms prior-id treatment)))]
    {:carrier :total-score
     :C (assoc preference-spec :carrier-by-treatment score-carriers :per-treatment C)
     :priors derive/priors
     :epistemic-choice {:G :risk-minus-EIG
                        :why :deterministic-likelihood-makes-expected-ambiguity-zero}
     :sanity sanity :ranges ranges :terms terms
     :terminal-leaf-baseline
     {:carrier :terminal-outcome-leaf :payoff legacy-payoff
      :risk-by-term (mapv #(select-keys % [:prior :treatment :policy
                                           :status :terminal-leaf-risk]) terms)}}))

(defn finish-report [report]
  (let [ablation (vec (for [prior-id (keys derive/priors)
                            treatment (sort (keys snatch/treatments))]
                        (ablation-cell (:terms report) (:ranges report)
                                       prior-id treatment)))]
    (assoc report :ablation ablation :prediction (prediction-report ablation)
           :verdict (if (some :nonDegenerate? ablation)
                      :nonDegenerate-holds :nonDegenerate-does-not-hold))))

(defn print-report [report]
  (println "DECLARED PRIORS (S-G3):" (pr-str (:priors report)))
  (println "DECLARED TOTAL-SCORE C (S-G3):" (pr-str (:C report)))
  (println "G = risk - EIG; deterministic P(o|s) makes ambiguity zero.")
  (println "SANITY: prior treatment agrees? expected-score-ranking risk-ranking")
  (doseq [{:keys [prior treatment agrees? expected-score-ranking risk-ranking]}
          (:sanity report)]
    (println (name prior) (name treatment) agrees?
             (pr-str expected-score-ranking) (pr-str risk-ranking)))
  (if (some (complement :agrees?) (:sanity report))
    (println "SANITY FAILED"
             (pr-str (mapv #(select-keys % [:prior :treatment])
                           (remove :agrees? (:sanity report)))))
    (do
      (println "RANGES: prior treatment pragmatic[min,max,range] epistemic[min,max,range]")
      (doseq [{:keys [prior treatment pragmatic epistemic]} (:ranges report)]
        (println (name prior) (name treatment) (pr-str pragmatic) (pr-str epistemic)))
      (println "ABLATION: prior treatment argmin-G argmin-risk moved? nonDegenerate? status")
      (doseq [{:keys [prior treatment argmin-G argmin-risk moved?
                      nonDegenerate? status]} (:ablation report)]
        (println (name prior) (name treatment) (pr-str argmin-G)
                 (pr-str argmin-risk) moved? nonDegenerate? (name status)))
      (println "PREDICTION:" (name (get-in report [:prediction :observed])))
      (println "VERDICT:" (name (:verdict report))))))

(defn -main [& _]
  (let [base (base-report) sane? (every? :agrees? (:sanity base))
        report (if sane? (finish-report base)
                   (assoc base :ablation [] :prediction {:observed :not-run}
                          :verdict :sanity-failed))]
    (spit "checks/ablation-snatch.edn" (with-out-str (pprint/pprint report)))
    (print-report report)
    (when-not sane?
      (throw (ex-info "SANITY FAILED: risk ranking disagrees with expected score"
                      {:cells (remove :agrees? (:sanity report))})))))
