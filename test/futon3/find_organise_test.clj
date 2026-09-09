(load-file "checks/find_organise.clj")

(ns futon3.find-organise-test
  (:require [clojure.edn]
            [clojure.test :refer [deftest is testing]]
            [find-organise :as fo]))

(def receipt-consumers
  (:cases (clojure.edn/read-string
           (slurp "test/fixtures/find-receipt-consumers.edn"))))

(deftest current-consumer-extensions-preserve-core-and-extension-evidence
  (doseq [{:keys [consumer route pattern entry extension note]} receipt-consumers]
    (testing (str consumer ": " note)
      (let [calls (atom [])
            result (fo/find {:context {} :route route
                             :fires? (fn [_ _] true)
                             :receipt (fn [id] (swap! calls conj id) extension)}
                            {:patterns #{pattern} :entries {pattern entry}})]
        (is (= [pattern] @calls))
        (is (= {:selected [pattern] :absence nil
                :receipts {pattern (assoc extension :if true :route route
                                          :warrant (fo/warrant
                                                    {:entries {pattern entry}} pattern))}}
               result))))))

(deftest receipt-extensions-cannot-supply-core-keys
  (let [{:keys [pattern entry]} (first receipt-consumers)
        repo {:patterns #{pattern} :entries {pattern entry}}]
    (doseq [extension [{:if false} {:route :forged} {:warrant nil}
                      {:if true} {:if nil :route nil :warrant nil :match 1}]]
      (testing (str "reject key presence, even unchanged values: " extension)
        (let [error (try
                      (fo/find {:fires? (fn [_ _] true)
                                :receipt (fn [_] extension)} repo)
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          (is (= {:finding :receipt-core-key-collision :pattern pattern
                  :keys (vec (sort (filter #{:if :route :warrant} (keys extension))))}
                 error)))))))

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
