(ns find-snatch-choices
  "Packet-3 comparison driver over the pinned evidence runner; no finder of its own."
  (:require [clojure.pprint :as pprint]
            [clojure.set :as set]
            [find-snatch-evidence :as e]))

(def driver-path "checks/find_snatch_choices.clj")
(defn file-digest [path]
  (e/digest "SHA-256" (java.nio.file.Files/readAllBytes (.toPath (java.io.File. path)))))
(defn selected [q]
  (set (map #(keyword "snatch" (name %)) (get-in q [:find :selected]))))
(defn population [m] (set (map :pattern (:interpretation-table m))))
(defn query-key [q] (conj (get-in q [:query :scenario]) (get-in q [:query :round])))

(defn f4 [m q]
  (let [pop (population m) s (selected q)
        scenario (get-in q [:query :scenario])
        b (:scenario-designated-exclusion (first (filter #(= scenario (:scenario %)) (:scenarios m))))
        designation (:F4-C-external-designation m)
        members (if (some #{(query-key q)} (:query-scope designation)) (:members designation) [])
        evaluated-omissions (vec (sort (for [[id v] (:evaluations q)
                                            :when (and (= :evaluated-excluded (:status v))
                                                       (not (s id)))] id)))]
    {:A (boolean (seq (set/difference pop s)))
     :A-evaluated-witnesses evaluated-omissions
     :B (and (contains? pop b) (not (contains? s b))) :B-member b
     :C (if (empty? members) :vacuous
            (every? #(not (contains? s %)) (filter pop members)))
     :C-members members :query (query-key q)}))

(defn b-at-scenario [m scenario queries]
  (let [b (:scenario-designated-exclusion (first (filter #(= scenario (:scenario %)) (:scenarios m))))]
    (and (contains? (population m) b) (not (contains? (apply set/union #{} (map selected queries)) b)))))

(defn primary-checks! [m sources evidence]
  (when-not (= (mapv :scenario (:scenarios m)) (mapv :scenario (:scenarios evidence)))
    (e/refuse! :scenario-accounting {}))
  (mapv
   (fn [{:keys [scenario queries]}]
     (let [rows
           (mapv
            (fn [q]
              (e/require-accounting! m q)
              (e/require-expectations! m scenario (:state q) (:find q))
              (let [as-of (e/query-as-of m scenario (:state q))
                    checks (into (sorted-map)
                                 (for [id (sort (selected q))]
                                   [id (e/receipt-checks m sources as-of id
                                                        (get-in q [:find :receipts (keyword (name id))]))]))
                    laws (f4 m q)]
                (when-not (and (= as-of (:query q))
                               (set/subset? (selected q) (population m))
                               (:A laws) (:B laws)
                               (not= false (:C laws))
                               (every? true? (mapcat vals (vals checks))))
                  (e/refuse! :primary-run-failed {:query (query-key q) :checks checks :f4 laws}))
                {:query (query-key q) :candidate-count (count (:evaluations q))
                 :statuses (into (sorted-map) (frequencies (map :status (vals (:evaluations q)))))
                 :receipt-checks checks :F1 true :F4 laws})) queries)]
       (when-not (b-at-scenario m scenario queries)
         (e/refuse! :primary-scenario-exclusion {:scenario scenario}))
       {:scenario scenario :B-at-scenario true :queries rows})) (:scenarios evidence)))

(defn checked-receipt [m sources q id receipt]
  (let [checks (e/receipt-checks m sources (:query q) id receipt)]
    (assoc checks :full (every? true? (vals checks)))))
(defn verdict [holds?] (if holds? :distinguished :not-distinguished-on-this-record))
(defn projection [q]
  (into (sorted-map) (for [id (get-in q [:find :selected])]
                      [id (contains? (get-in q [:find :receipts]) id)])))

(defn comparisons [m sources evidence]
  (let [g4 (first (filter #(= [:g4 :snatcher] (:scenario %)) (:scenarios evidence)))
        by-round (into {} (map (juxt #(get-in % [:query :round]) identity)) (:queries g4))
        q1 (get by-round 1) q2 (get by-round 2)
        probe :snatch/probe-before-committing
        receipt (get-in q1 [:find :receipts :probe-before-committing])
        _ (when-not (and receipt q2) (e/refuse! :comparison-input-missing {}))
        changed (assoc-in receipt [:evidence :clauses 0 :clause] :terminal-round)
        base (checked-receipt m sources q1 probe receipt)
        clause (checked-receipt m sources q1 probe changed)
        replacement (e/citation m sources (e/interpretation m :snatch/price-the-final-round-as-final) :if)
        cited (assoc-in receipt [:evidence :clauses 0 :citation] replacement)
        citation (checked-receipt m sources q1 probe cited)
        exchange :snatch/exchange-when-both-sides-gain
        ex1 (get-in q1 [:find :receipts :exchange-when-both-sides-gain])
        ex2 (get-in q2 [:find :receipts :exchange-when-both-sides-gain])
        as-of-swap (assoc-in ex2 [:evidence :as-of] (get-in ex1 [:evidence :as-of]))
        as-of-checks (checked-receipt m sources q2 exchange as-of-swap)
        matrix (mapv #(mapv % [:clause-attribution :citation]) [base clause citation])
        original-f4 (f4 m q2)
        b (:B-member original-f4)
        c (first (:C-members original-f4))
        admit (fn [id] (update-in q2 [:find :selected] conj (keyword (name id))))
        ab (admit b)
        bc (when c (admit c))
        ab-laws (f4 m ab)
        bc-laws (when bc (f4 m bc))
        scenario-b (fn [q] (b-at-scenario m [:g4 :snatcher]
                                         (mapv #(if (= 2 (get-in % [:query :round])) q %) (:queries g4))))]
    ;; Predictions were pinned before this run; an exclusion mismatch stops it.
    (doseq [expected (:independent-exclusion-expectations
                     (first (filter #(= 2 (:round %)) (:g4-independent-expectations m))))]
      (let [actual (get-in q2 [:evaluations (:pattern expected)])]
        (when-not (and (= :evaluated-excluded (:status actual))
                       (= (:if-boolean expected) (get-in actual [:if :boolean]))
                       (= (:however-boolean expected) (get-in actual [:however :boolean])))
          (e/refuse! :independent-exclusion-mismatch {:expected expected :actual actual}))))
    [{:comparison :F2-full-versus-presence
      :verdict (verdict (and (:full base) (:presence clause) (not (:full clause))
                             (not (:clause-attribution clause))
                             (= (projection q1)
                                (projection (assoc-in q1 [:find :receipts :probe-before-committing] changed)))))
      :genuine base :control {:control true :mutation :clause-attribution-only
                              :receipt changed :checks clause}
      :presence-projection (projection q1)}
     {:comparison :F2-as-of-cross-round
      :verdict (verdict (and ex1 ex2
                             (:full (checked-receipt m sources q1 exchange ex1))
                             (:full (checked-receipt m sources q2 exchange ex2))
                             (:presence as-of-checks) (:clause-attribution as-of-checks)
                             (:citation as-of-checks) (false? (:query-binding as-of-checks))))
      :control {:control true :mutation :round-1-as-of-in-round-2
                :receipt as-of-swap :checks as-of-checks}}
     {:comparison :F3-two-check-matrix
      :verdict (verdict (= [[true true] [false true] [true false]] matrix))
      :columns [:clause-attribution :citation]
      :rows [{:control false :case :genuine :checks base}
             {:control true :case :clause-only-swap :checks clause :receipt changed}
             {:control true :case :cited-pattern-only-swap :checks citation :receipt cited}]
      :observed matrix}
     {:comparison :F4-A-versus-B
      :verdict (verdict (and (:A original-f4) (:B original-f4)
                             (not ((selected q2) b)) (:A ab-laws)
                             (seq (:A-evaluated-witnesses ab-laws))
                             (false? (scenario-b ab))))
      :genuine original-f4
      :control {:control true :valid-primary false :scope :selection-projection-only
                :admitted b :selection (vec (sort (selected ab)))
                :F4 ab-laws :B-at-scenario (scenario-b ab)}}
     {:comparison :F4-B-versus-C
      :verdict (verdict (and c (not= b c) (true? (:C original-f4))
                             (= :evaluated-excluded (get-in q2 [:evaluations c :status]))
                             (:B original-f4) (:B bc-laws) (scenario-b bc)
                             (false? (:C bc-laws))))
      :genuine original-f4
      :control {:control true :valid-primary false :scope :selection-projection-only
                :admitted c :selection (when bc (vec (sort (selected bc))))
                :F4 bc-laws :B-at-scenario (when bc (scenario-b bc))}
      :empty-designation :vacuous}]))

(defn serialize [x] (with-out-str (pprint/pprint x)))
(defn -main [& _]
  (let [m (e/read-manifest)
        driver-hash (file-digest driver-path)
        sources (e/validate-basis! m ".")
        evidence (e/build-evidence m ".")
        primary (primary-checks! m sources evidence)
        cases (comparisons m sources evidence)
        basis {:manifest-file e/manifest-path :manifest-sha256 (file-digest e/manifest-path)
               :manifest-digest (e/state-digest m) :source-pins (get-in m [:source-basis :pins])
               :comparison-driver {:file driver-path :sha256 driver-hash
                                   :review-status :awaiting-independent-review}}
        report (assoc evidence :status :primary-validated :basis basis)
        report-text (serialize report)
        sheet {:schema :experimental/f11-find-choice-evidence-v1 :basis basis
               :status :next-steps-evidence :choice-status (:choice-status m)
               :use "NEXT-STEPS input only to find-f2-receipt-carrier, find-f4-reading and find-f3-citation-field; all remain observed-not-decided. No arm adopted and no registry updated."
               :limits "Finite records and explicitly labelled diagnostic faults distinguish checks on this record, not universal Lean propositions. Unknown interpretations remain outside semantic zero-mass claims."
               :summary "Query-clause content and per-query provenance detect faults that receipt presence alone preserves. The citation/attribution controls separate their checks. At the pinned g4 round-2 designation, controlled admissions separate A/B and B/C; the genuine selections are unchanged."
               :primary-artifact-sha256 (e/digest "SHA-256" (.getBytes report-text "UTF-8"))
               :primary-checks primary :comparisons cases}]
    (when-not (some #(= :distinguished (:verdict %)) cases)
      (e/refuse! :warrant-counterfactual {:comparisons cases}))
    (e/validate-basis! m ".")
    (when-not (and (= m (e/read-manifest)) (= driver-hash (file-digest driver-path)))
      (e/refuse! :comparison-basis-drift {}))
    (spit e/evidence-path report-text)
    (spit e/choices-path (serialize sheet))
    (doseq [c cases] (prn (select-keys c [:comparison :verdict])))
    (println "Primary:" (count primary) "scenarios," (count (mapcat :queries primary)) "queries; pins rechecked")
    (shutdown-agents)))
