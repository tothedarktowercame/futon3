(ns futon3.find-snatch-evidence-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [find-snatch :as finder]
            [find-snatch-evidence :as evidence]
            [playout-snatch :as snatch]))

(def manifest (edn/read-string (slurp evidence/manifest-path)))
;; Unit inputs from the independently authored manifest, not a fresh playout.
(def round-one (merge (:g4 snatch/treatments)
                      {:grain :play :treatment :g4 :round 1 :tokens 10 :seized 0
                       :snatched? false :repair-observed? false :last-round? false}))
(def source-texts
  (into {} (map (fn [{:keys [file]}] [file (slurp file)]))
        (get-in manifest [:source-basis :pins])))
(defn failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest draft-and-drift-refuse-before-play
  (testing "draft refusal precedes filesystem access"
    (is (= :manifest-not-reviewed
           (:finding (failure #(evidence/validate-basis!
                                (assoc manifest :status :draft-awaiting-independent-pin)
                                "/nonexistent"))))))
  (testing "source hash drift requires reviewer successor, never a silent refresh"
    (let [bad (update-in manifest [:source-basis :pins]
                         #(assoc-in % [0 :sha256] "deliberate-drift"))
          error (failure #(evidence/validate-basis! bad "."))]
      (is (= :reviewed-successor-pin-required (:finding error)))
      (is (= (:file (first (get-in manifest [:source-basis :pins]))) (:file error))))))

(deftest canonical-state-preserves-meaning-and-ignores-map-order
  (is (= (evidence/state-digest (array-map :a 1 :b #{2 3}))
         (evidence/state-digest (array-map :b #{3 2} :a 1))))
  (is (not= (evidence/state-digest {}) (evidence/state-digest {:a nil})))
  (is (not= (evidence/state-digest [1]) (evidence/state-digest '(1)))))

(deftest adapter-extension-collision-is-not-swallowed
  (doseq [key [:if :route :warrant]]
    (let [error (failure #(finder/find-with-evidence round-one
                                                    (fn [_ _] {key nil})))]
      (is (= :receipt-core-key-collision (:finding error)))
      (is (= [key] (:keys error)))
      (is (keyword? (:pattern error))))))

(deftest unit-query-preserves-selection-and-accounts-for-every-candidate
  (let [result (evidence/observe-query manifest source-texts [:g4 :snatcher] round-one)
        found (:find result)
        probe (get-in result [:evaluations :snatch/probe-before-committing])]
    (is (= [:ask-for-surplus-not-surrender :exchange-when-both-sides-gain
            :probe-before-committing] (:selected found)))
    (is (= (:selected (finder/find round-one)) (:selected found)))
    (is (= {:selected 3 :evaluated-excluded 8 :other-grain 8
            :p2-out-of-domain 1 :no-executable-interpretation 4}
           (frequencies (map :status (vals (:evaluations result))))))
    (is (= {:raw true :boolean true :evaluation :antecedent} (:if probe)))
    (is (= :diagnostic-after-if-false
           (get-in result [:evaluations :snatch/re-enter-after-observed-repair :however :evaluation])))
    (is (= :candidate-accounting
           (:finding (failure #(evidence/require-accounting!
                                manifest (update result :evaluations dissoc
                                                   :snatch/have-a-temperament))))))
    (is (= :candidate-accounting
           (:finding (failure #(evidence/require-accounting!
                                manifest (assoc-in result [:evaluations :snatch/have-a-temperament :status]
                                                     :selected))))))
    (is (= :selection-mismatch
           (:finding (failure #(evidence/require-selection-equality!
                                found (assoc found :selected []))))))))

(deftest pinned-citation-bytes-are-checked
  (let [id :snatch/probe-before-committing
        row (evidence/interpretation manifest id)
        file (get-in row [:authored-spans :if :file])]
    (is (= :citation-source-mismatch
           (:finding (failure #(evidence/citation manifest (assoc source-texts file "drift") row :if)))))))

(deftest observing-callback-returns-existing-policy-value
  (let [observations (atom [])
        observed {:query :unit-stub}
        action {:act :offer :size 1 :by :unit-stub}]
    (with-redefs [evidence/observe-query (fn [_ _ scenario state]
                                         (is (= [:g4 :snatcher] scenario))
                                         (is (= round-one state)) observed)
                  snatch/pi-patterns (fn [state patterns]
                                       (is (= round-one state))
                                       (is (= [:unit-pattern] patterns)) action)]
      (is (= action ((evidence/observing-policy manifest {} [:g4 :snatcher] observations)
                     round-one [:unit-pattern])))
      (is (= [observed] @observations)))))

(deftest build-preflight-refusal-does-not-start-a-playout-or-write
  (let [called (atom false)
        writes (atom [])]
    (with-redefs [snatch/play (fn [& _] (reset! called true))
                  clojure.core/spit (fn [& args] (swap! writes conj args))]
      (is (= :manifest-not-reviewed
             (:finding (failure #(evidence/build-evidence (assoc manifest :status :draft) ".")))))
      (is (false? @called))
      (is (empty? @writes)))))

(deftest predicate-observation-is-once-and-preserves-nil
  (let [calls (atom [])
        entry {:id :probe-before-committing :grain :play
               :if (fn [_] (swap! calls conj :if) nil)
               :however (fn [_] (swap! calls conj :however) true)}]
    (with-redefs-fn {(ns-resolve 'find-snatch 'runner-entry) {:probe-before-committing entry}}
      (fn []
        (let [result (finder/find-with-evidence round-one nil)
              evaluation (get-in result [:evaluations :snatch/probe-before-committing])]
          (is (= [:if :however] @calls))
          (is (= {:raw nil :boolean false :evaluation :antecedent} (:if evaluation)))
          (is (= {:raw true :boolean true :evaluation :diagnostic-after-if-false}
                 (:however evaluation)))
          (is (= :evaluated-excluded (:status evaluation))))))))
