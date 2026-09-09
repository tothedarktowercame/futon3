(ns futon3.find-snatch-choices-test
  (:require [clojure.test :refer [deftest is]]
            [find-snatch-choices :as choices]))

(def manifest
  {:interpretation-table [{:pattern :snatch/a} {:pattern :snatch/b} {:pattern :snatch/c}]
   :scenarios [{:scenario [:g4 :snatcher] :scenario-designated-exclusion :snatch/b}]
   :F4-C-external-designation {:members [:snatch/c] :query-scope [[:g4 :snatcher 2]]}})
(def query
  {:query {:scenario [:g4 :snatcher] :round 2}
   :find {:selected [:a]}
   :evaluations {:snatch/a {:status :selected}
                 :snatch/b {:status :evaluated-excluded}
                 :snatch/c {:status :no-executable-interpretation}}})

(deftest omissions-are-not-automatically-semantic-witnesses
  (let [result (choices/f4 manifest query)]
    (is (= {:A true :B true :C true} (select-keys result [:A :B :C])))
    (is (= [:snatch/b] (:A-evaluated-witnesses result)))))

(deftest designation-scope-and-emptiness-are-vacuous
  (is (= :vacuous (:C (choices/f4 manifest (assoc-in query [:query :round] 1)))))
  (is (= :vacuous (:C (choices/f4 (assoc-in manifest [:F4-C-external-designation :members] []) query)))))

(deftest diagnostic-admissions-separate-exclusion-readings
  (is (= {:A true :B false :C true}
         (select-keys (choices/f4 manifest (assoc-in query [:find :selected] [:a :b])) [:A :B :C])))
  (is (= {:A true :B true :C false}
         (select-keys (choices/f4 manifest (assoc-in query [:find :selected] [:a :c])) [:A :B :C])))
  (is (false? (choices/b-at-scenario manifest [:g4 :snatcher]
                                    [query (assoc-in query [:find :selected] [:b])]))))
