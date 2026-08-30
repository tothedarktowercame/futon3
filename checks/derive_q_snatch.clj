(ns derive-q-snatch
  "Derive Q(o|pi) by pushing declared disposition priors through the Snatch
   runner. Priors are stipulations (S-G3); outcome mass is computed, not copied
   from item-s001.edn."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [how-kernel-snatch :as kernel]
            [playout-snatch :as snatch]))

;; S-G3: these priors are declared inputs, printed in the report, and never
;; inferred from a run.
(def priors
  {:default  {:sharer 0.5 :snatcher 0.5 :cautious 0.0}
   :cautious {:sharer 0.45 :snatcher 0.45 :cautious 0.10}})

(def policies
  {:grim snatch/pi-grim
   :patterns snatch/pi-patterns
   :exchange-first snatch/pi-exchange-first})

(defn pi-probe-one-token
  "Offer one token (and, in the item contract, ask one corn); after a snatch,
   never offer again unless G2 has removed abstention from the action space."
  [s _patterns]
  (if (and (:snatched? s) (not (:forced-offer? s)))
    {:act :abstain}
    {:act :offer :size 1}))

(def all-policies (assoc policies :probe-one-token pi-probe-one-token))

(def outcome-space
  (:per-treatment (edn/read-string (slurp "checks/snatch-outcomes.edn"))))

(def stated-s001
  (edn/read-string (slurp "checks/item-s001.edn")))

(defn trajectory-leaf
  "Project one runner event onto the treatment-indexed outcome carrier.

   The runner records the base event plus state effects. G3's assigned shame
   refines O4 to O4a. G4's later :repaired event is the declared O5 leaf.
   A stopped line deliberately remains outside the carrier."
  [treatment event]
  (case (:outcome event)
    :O4 (if (= treatment :g3) :O4a :O4)
    :repaired (if (= treatment :g4) :O5 :repaired)
    (:outcome event)))

(defn event-at-grain [trajectory grain]
  (case grain
    :round-1 (first trajectory)
    :terminal (last trajectory)))

(defn empty-q [leaves]
  (into (sorted-map) (map (fn [leaf] [leaf 0.0])) leaves))

(defn derive-cell [prior-id prior policy-id policy treatment grain]
  (let [leaves (get outcome-space treatment)
        weighted (for [[disposition mass] prior
                       :when (pos? mass)
                       :let [trajectory (snatch/play policy treatment disposition 5)
                             event (event-at-grain trajectory grain)
                             leaf (trajectory-leaf treatment event)]]
                   {:disposition disposition :mass mass :leaf leaf})
        inside (filter #(contains? leaves (:leaf %)) weighted)
        outside (remove #(contains? leaves (:leaf %)) weighted)
        q (reduce (fn [acc {:keys [leaf mass]}] (update acc leaf + mass))
                  (empty-q leaves)
                  inside)
        outside-mass (reduce (fn [acc {:keys [leaf mass]}]
                               (update acc leaf (fnil + 0.0) mass))
                             (sorted-map)
                             outside)
        zero-mass (->> q (keep (fn [[leaf mass]] (when (zero? mass) leaf))) vec)]
    {:prior prior-id
     :policy policy-id
     :treatment treatment
     :grain grain
     :Q q
     ;; Entropy describes the complete derived distribution. Q remains typed
     ;; to the declared carrier; stopped-line mass is kept in the calculation
     ;; without being silently admitted to that carrier.
     :entropy (kernel/entropy (merge q outside-mass))
     :zero-mass zero-mass
     :outside-leaf-set (vec (keys outside-mass))
     :outside-mass outside-mass
     :mirror (if (seq zero-mass)
               {:has-zero-mass? true}
               {:has-zero-mass? false
                :why :declared-prior-populates-every-declared-leaf})}))

(defn derive-report []
  {:prior priors
   :results
   (vec
    (for [[prior-id prior] priors
          [policy-id policy] all-policies
          treatment (sort (keys snatch/treatments))
          grain [:round-1 :terminal]]
      (derive-cell prior-id prior policy-id policy treatment grain)))})

;; Review fix (claude-15, 2026-08-30): the acceptance target is READ from the item,
;; not restated here — otherwise the check compares the derivation to a copy of
;; itself rather than to S-001 as stated.
(def expected-s001-q
  (into (sorted-map) (:Q stated-s001)))

(defn result-for [report prior policy treatment grain]
  (some #(when (= [prior policy treatment grain]
                  ((juxt :prior :policy :treatment :grain) %))
           %)
        (:results report)))

(defn validate! [report]
  (let [probe-g1 (result-for report :default :probe-one-token :g1 :round-1)
        second-round-1 (filter #(and (= :cautious (:prior %))
                                     (= :round-1 (:grain %)))
                               (:results report))]
    (when-not (= expected-s001-q (:Q probe-g1))
      (throw (ex-info "Derived probe-one-token G1 Q disagrees with S-001"
                      {:derived (:Q probe-g1) :stated (:Q stated-s001)})))
    (doseq [{:keys [treatment Q]} (:results report)]
      (when-not (every? (get outcome-space treatment) (keys Q))
        (throw (ex-info "Q contains a leaf outside its treatment carrier"
                        {:treatment treatment :Q Q}))))
    (doseq [cell (filter #(= :g2 (:treatment %)) (:results report))]
      (when (or (contains? (:Q cell) :O1)
                (some #{:O1} (:outside-leaf-set cell)))
        (throw (ex-info "G2 emitted forbidden O1" cell))))
    (doseq [cell second-round-1]
      (when-not (pos? (get-in cell [:Q :O3] 0.0))
        (throw (ex-info "Cautious prior did not put round-1 mass on O3" cell))))
    true))

(defn fmt-mass [q]
  (pr-str (into (sorted-map) (map (fn [[k v]] [k (double v)])) q)))

(defn print-report [report]
  (println "DECLARED PRIORS (S-G3):" (pr-str (:prior report)))
  (println "prior policy treatment grain Q entropy zero-mass outside")
  (doseq [{:keys [prior policy treatment grain Q entropy zero-mass
                  outside-leaf-set]} (:results report)]
    (println (name prior) (name policy) (name treatment) (name grain)
             (fmt-mass Q) (format "%.4f" (double entropy))
             (pr-str zero-mass) (pr-str outside-leaf-set)))
  (let [actual (:Q (result-for report :default :probe-one-token :g1 :round-1))]
    (println)
    (println "ACCEPTANCE probe-one-token/default/g1/round-1 == S-001:"
             (= expected-s001-q actual) (pr-str actual))))

(defn -main [& _]
  (let [report (derive-report)]
    (validate! report)
    (spit "checks/derived-q-snatch.edn"
          (with-out-str (pprint/pprint report)))
    (print-report report)))
