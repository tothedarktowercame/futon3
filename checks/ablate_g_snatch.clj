(ns ablate-g-snatch
  "Packet B: derive pragmatic risk and epistemic value for Snatch, then remove
   the epistemic term and test whether the preferred policy changes.

   The deterministic likelihood has zero ambiguity H[P(o|s)]. We therefore
   use G = risk - EIG: expected information gain is the only epistemic quantity
   with range in this model."
  (:require [clojure.pprint :as pprint]
            [derive-q-snatch :as derive]
            [how-kernel-snatch :as kernel]
            [playout-snatch :as snatch]))

;; S-G3 preference stipulation. These are the size-1 event payoffs implied by
;; the runner: exchange +1, snatch -1, and G4 repair +3. Shame has no current
;; payoff. :line-stopped is not a game leaf; the record lacks a preference for
;; it, so we declare the neutral payoff 0 once instead of dropping its mass.
(def preference-spec
  {:form :C-proportional-to-exp-payoff
   :offer-size 1
   :payoff {:O1 0.0 :O2 1.0 :O3 0.0 :O4 -1.0
            :O4a -1.0 :O4b -1.0 :O5 3.0
            :line-stopped 0.0}
   :line-stopped-choice
   {:payoff 0.0 :status :declared-stipulation
    :why :runner-can-stop-outside-game-leaf-carrier}})

(defn pi-always-abstain [_s _patterns] {:act :abstain})

(def policies
  (assoc derive/all-policies :always-abstain pi-always-abstain))

(defn softmax-preferences [treatment]
  (let [carrier (conj (get derive/outcome-space treatment) :line-stopped)
        weights (into {} (map (fn [leaf]
                                [leaf (Math/exp
                                       (get-in preference-spec [:payoff leaf]))])
                              carrier))
        z (reduce + (vals weights))]
    (into (sorted-map) (map (fn [[leaf weight]] [leaf (/ weight z)]) weights))))

(def C
  (into (sorted-map)
        (map (fn [treatment] [treatment (softmax-preferences treatment)]))
        (keys snatch/treatments)))

(defn entropy-of [distribution]
  (kernel/entropy distribution))

(defn normalize [distribution]
  (let [z (reduce + (vals distribution))]
    (if (pos? z)
      (into {} (map (fn [[k v]] [k (/ v z)])) distribution)
      distribution)))

(defn posterior-update
  "Bayes-update a disposition distribution from the action and observed base
   event. The likelihood is read from p2-response, not from the true hidden
   disposition used to generate the trajectory. Non-offer events carry no
   disposition information in this model."
  [posterior {:keys [action outcome]}]
  (if (= action :offer)
    (normalize
     (into {} (map (fn [[disposition mass]]
                     [disposition
                      (if (= outcome (snatch/p2-response disposition)) mass 0.0)]))
           posterior))
    posterior))

(defn posterior-path [prior trajectory]
  (reductions posterior-update prior trajectory))

(defn expected-eig
  "Expected H(prior)-H(posterior) after round 1 and after the full trajectory."
  [prior policy treatment]
  (let [h0 (entropy-of prior)
        reductions
        (for [[disposition mass] prior
              :when (pos? mass)
              :let [trajectory (snatch/play policy treatment disposition 5)
                    path (posterior-path prior trajectory)]]
          {:mass mass
           :round-1 (- h0 (entropy-of (or (second path) (first path))))
           :terminal (- h0 (entropy-of (last path)))})]
    {:round-1 (reduce + (map #(* (:mass %) (:round-1 %)) reductions))
     :terminal (reduce + (map #(* (:mass %) (:terminal %)) reductions))}))

(defn complete-q [{:keys [Q outside-mass]}]
  (merge-with + Q outside-mass))

(defn kl-divergence [q c]
  (reduce +
          (for [[leaf mass] q :when (pos? mass)]
            (* mass (Math/log (/ mass (get c leaf)))))))

(defn feasible? [policy-id treatment]
  ;; G2 removes abstention from the action space. Keeping the policy as
  ;; :infeasible is the institution acting on policy membership, not a score.
  (not (and (= policy-id :always-abstain) (= treatment :g2))))

(defn term-cell [prior-id prior policy-id policy treatment]
  (if-not (feasible? policy-id treatment)
    {:policy policy-id :treatment treatment :prior prior-id
     :status :infeasible :why :abstention-not-in-g2-action-space}
    (let [round-1 (derive/derive-cell prior-id prior policy-id policy
                                      treatment :round-1)
          terminal (derive/derive-cell prior-id prior policy-id policy
                                       treatment :terminal)
          c (get C treatment)
          risk-round-1 (kl-divergence (complete-q round-1) c)
          risk (kl-divergence (complete-q terminal) c)
          eig (expected-eig prior policy treatment)]
      {:policy policy-id
       :treatment treatment
       :prior prior-id
       :status :scored
       :risk risk
       :risk-round-1 risk-round-1
       :eig (:terminal eig)
       :eig-round-1 (:round-1 eig)
       :G (- risk (:terminal eig))})))

(defn spread [xs]
  (let [lo (apply min xs)
        hi (apply max xs)
        width (- hi lo)]
    {:min lo :max hi
     :range (if (< (Math/abs (double width)) 1.0e-12) 0.0 width)}))

(defn range-cell [terms prior-id treatment]
  (let [cells (filter #(and (= :scored (:status %))
                            (= prior-id (:prior %))
                            (= treatment (:treatment %)))
                      terms)]
    {:prior prior-id
     :treatment treatment
     :pragmatic (spread (map :risk cells))
     :epistemic (spread (map :eig cells))}))

(defn minimizers [cells key-fn]
  (let [best (apply min (map key-fn cells))]
    (->> cells
         (filter #(< (Math/abs (- (double best) (double (key-fn %)))) 1.0e-12))
         (map :policy)
         sort
         vec)))

(defn ablation-cell [terms ranges prior-id treatment]
  (let [cells (filter #(and (= :scored (:status %))
                            (= prior-id (:prior %))
                            (= treatment (:treatment %)))
                      terms)
        range-row (some #(when (= [prior-id treatment]
                                  ((juxt :prior :treatment) %)) %)
                        ranges)
        epistemic-range (get-in range-row [:epistemic :range])
        pragmatic-range (get-in range-row [:pragmatic :range])
        can-ablate? (pos? epistemic-range)
        argmin-g (when can-ablate? (minimizers cells :G))
        argmin-risk (when can-ablate? (minimizers cells :risk))
        moved? (when can-ablate? (not= argmin-g argmin-risk))]
    {:treatment treatment
     :prior prior-id
     :argmin-G argmin-g
     :argmin-risk argmin-risk
     :moved? moved?
     :nonDegenerate? (boolean (and can-ablate?
                                   (pos? pragmatic-range)
                                   moved?))
     :status (if can-ablate? :ablated :not-ablated)
     :why (when-not can-ablate? :epistemic-term-has-zero-range)}))

(defn build-report []
  (let [terms (vec
               (for [[prior-id prior] derive/priors
                     [policy-id policy] policies
                     treatment (sort (keys snatch/treatments))]
                 (term-cell prior-id prior policy-id policy treatment)))
        ranges (vec
                (for [prior-id (keys derive/priors)
                      treatment (sort (keys snatch/treatments))]
                  (range-cell terms prior-id treatment)))
        ablation (vec
                  (for [prior-id (keys derive/priors)
                        treatment (sort (keys snatch/treatments))]
                    (ablation-cell terms ranges prior-id treatment)))]
    {:C (assoc preference-spec :per-treatment C)
     :priors derive/priors
     :epistemic-choice
     {:G :risk-minus-EIG
      :why :deterministic-likelihood-makes-expected-ambiguity-zero}
     :risk-grain
     {:selected :terminal
      :why :policy-grade-is-assessed-after-the-five-round-run
      :round-1-also-reported true}
     :ranges ranges
     :terms terms
     :ablation ablation
     :verdict (if (some :nonDegenerate? ablation)
                :nonDegenerate-holds
                :nonDegenerate-does-not-hold)}))

(defn print-report [report]
  (println "DECLARED PRIORS (S-G3):" (pr-str (:priors report)))
  (println "DECLARED C (S-G3):" (pr-str (:C report)))
  (println "G = risk - EIG; deterministic P(o|s) makes ambiguity zero.")
  (println)
  (println "RANGES: prior treatment pragmatic[min,max,range] epistemic[min,max,range]")
  (doseq [{:keys [prior treatment pragmatic epistemic]} (:ranges report)]
    (println (name prior) (name treatment) (pr-str pragmatic) (pr-str epistemic)))
  (println)
  (println "ABLATION: prior treatment argmin-G argmin-risk moved? nonDegenerate? status")
  (doseq [{:keys [prior treatment argmin-G argmin-risk moved?
                  nonDegenerate? status]} (:ablation report)]
    (println (name prior) (name treatment) (pr-str argmin-G)
             (pr-str argmin-risk) moved? nonDegenerate? (name status)))
  (println)
  (println "VERDICT:" (name (:verdict report))))

(defn -main [& _]
  (let [report (build-report)]
    (spit "checks/ablation-snatch.edn"
          (with-out-str (pprint/pprint report)))
    (print-report report)))
