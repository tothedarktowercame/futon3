(ns graph-test
  (:require [clojure.test :refer :all]
            [f1.graph :as graph]))

(deftest fixture-loading
  (let [store (graph/init {:fixtures "dev/fixtures.edn"})]
    (is (= 3 (count (graph/tx-log store))))
    (is (seq (graph/q store '[:find ?e :where [?e :musn/type _]])))))
