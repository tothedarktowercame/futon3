(ns f12-preparation-test
  (:require [f12-preparation :as a]
            [find-organise :as fo]
            [clojure.test :refer [deftest is run-tests]]))
(def manifest "checks/fixtures/f12-preparation/manifest.json")
(deftest organiser-and-real-diagnostic-actions
  (let [b (a/diagnostic-arm manifest :baseline a/command!)
        i (a/diagnostic-arm manifest :intervention a/command!)
        row {:precedence-before [1 2 3 4 5 6] :precedence-after [2 1 3 4 5 6]
             :acting-order-before (:acting-order b) :acting-order-after (:acting-order i)
             ;; Explicit synthetic law-control numbers, not primary observations.
             :score-before 1 :score-after 1}]
    (is (= (mapv a/ids [0 1 2]) (:acting-order b)))
    (is (= (mapv a/ids [1 0 2]) (:acting-order i)))
    (is (= (:parent-order b) (:parent-order i)))
    (is (nil? (:primary-score b)))
    (is (= :both-match (get-in b [:state :disposition])))
    (is (fo/o4-precedence-governance row))
    (is (false? (fo/o4-precedence-governance
                 (assoc row :acting-order-after (:acting-order b)))))
    (is (false? (fo/o4-precedence-governance
                 (assoc row :acting-order-before (:parent-order b)
                            :acting-order-after (:parent-order i)))))))
(deftest total-consumers-and-failures
  (is (thrown? AssertionError (a/exhaustive! (conj a/enum :new) [a/summaries a/scores])))
  (is (= :find-mismatch (a/disposition {:result "mismatch"} {:result "match"})))
  (is (= :check-failed (a/disposition {:result "check-failed"} {:result "match"})))
  (is (thrown? clojure.lang.ExceptionInfo
               (fo/organise {:closure :selected-only} #{[:alien :context]} a/repository))))
(let [r (run-tests)] (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))
