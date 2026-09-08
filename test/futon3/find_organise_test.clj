(load-file "checks/find_organise.clj")

(ns futon3.find-organise-test
  (:require [clojure.edn]
            [clojure.test :refer [deftest is testing]]
            [find-organise :as fo]))

(deftest ruled-o3-does-not-bootstrap-from-added-nodes
  (let [repo {:patterns #{0 1 2 3}
              :stands-on {0 #{1} 1 #{2} 2 #{3}}
              :acyclic? true}
        cascade (fo/organise fo/up-closure-temperament #{0 3} repo)]
    (testing "introduced bridge nodes are removed before fast-forwarding"
      (is (= #{1 2} (:added-by-organise cascade)))
      (is (= #{[0 3]} (:edges cascade)))
      (is (fo/o3-fast-forward (assoc cascade :stands-on (:stands-on repo)))))))

(deftest ruled-o3-rejects-the-old-bootstrap-edge-set
  (is (false? (fo/o3-fast-forward
               {:nodes #{0 1 2 3}
                :added-by-organise #{1 2}
                :stands-on {0 #{1} 1 #{2} 2 #{3}}
                :edges #{[0 1] [1 2] [2 3]}}))))

(deftest recorded-snatch-row-now-uses-the-ruled-edge-set
  (let [repo (fo/read-repository "library" [:snatch])
        fixture (clojure.edn/read-string (slurp "checks/snatch-cascade.edn"))
        row (first (filter #(= [:g4 :snatcher] (:scenario %))
                           (fo/cascade-diff-table fixture repo)))]
    (is (= #{[:snatch/re-enter-after-observed-repair
              :snatch/consult-the-remedy-before-exiting]}
           (:edges row)))
    (is (fo/o3-fast-forward row))))
