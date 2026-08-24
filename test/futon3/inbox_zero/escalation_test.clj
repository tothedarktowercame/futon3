(ns futon3.inbox-zero.escalation-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3.inbox-zero.escalation :as escalation]))

(defn plan [paths]
  {:record/type :inbox-zero/promotion-plan
   :seat/id "seat:claude-3:s1" :repo/id "futon3"
   :worktree/id "worktree:futon3" :include paths :exclude []
   :verdict :proposed :held/reason nil})

(deftest key-material-holds-plan-and-innocent-plan-is-identical
  (let [sensitive (plan [{:path "config/deploy.pem" :git/status :untracked}])
        screened (escalation/screen-sensitivity sensitive escalation/default-rules)
        innocent (plan [{:path "src/futon3/core.clj" :git/status :modified}])]
    (is (= [:held :sensitive-content]
           [(:verdict screened) (:held/reason screened)]))
    (is (= [{:path "config/deploy.pem" :rule/kind :key-material}]
           (:sensitive/hits screened)))
    (is (= innocent
           (escalation/screen-sensitivity innocent escalation/default-rules)))))

(deftest size-rule-requires-present-size-over-ceiling
  (let [rules [{:rule/kind :large-binary
                :match {:path-patterns ["(?i)\\.bin$"] :max-bytes 100}}]
        missing (plan [{:path "asset.bin"}])
        equal (plan [{:path "asset.bin" :size 100}])
        over (plan [{:path "asset.bin" :size 101}])]
    (is (= missing (escalation/screen-sensitivity missing rules)))
    (is (= equal (escalation/screen-sensitivity equal rules)))
    (is (= :sensitive-content
           (:held/reason (escalation/screen-sensitivity over rules))))))

(deftest default-rules-are-kind-patterns-not-project-file-enumerations
  (is (= #{:key-material :credential-file :personal-data :large-binary}
         (set (map :rule/kind escalation/default-rules))))
  (doseq [rule escalation/default-rules
          pattern (get-in rule [:match :path-patterns])]
    (is (string? pattern))
    (is (re-find #"[()|*+?\\]" pattern)
        "each entry is recognizably a regex shape, not a literal path")
    (is (not (re-find #"(^|/)futon[0-9]|(^|/)src/|(^|/)scripts/" pattern)))))

(deftest sensitive-item-always-routes-to-operator
  (let [item (-> (plan [{:path "secret.pem"}])
                 (escalation/screen-sensitivity escalation/default-rules))
        decision (first (escalation/route
                         [item] {:live-seats #{"seat:claude-3:s1"}
                                 :operator-recipient "joe"}))]
    (is (= [3 "joe"] [(:route/tier decision) (:route/recipient decision)]))
    (is (re-find #"Holding on your word" (:route/message decision)))))

(deftest ordinary-held-plan-routes-to-live-seat-then-sweeper
  (let [item (assoc (plan []) :verdict :held :held/reason :gate-failed)]
    (testing "live responsible seat"
      (let [decision (first (escalation/route
                             [item] {:live-seats #{"seat:claude-3:s1"}}))]
        (is (= [1 "seat:claude-3:s1"]
               [(:route/tier decision) (:route/recipient decision)]))))
    (testing "dead responsible seat"
      (let [decision (first (escalation/route
                             [item] {:live-seats #{}
                                     :sweeper-recipient "sweeper"}))]
        (is (= [2 "sweeper"]
               [(:route/tier decision) (:route/recipient decision)]))))))

(deftest push-escalation-uses-live-seat-or-sweeper
  (let [base {:record/type :inbox-zero/push-result
              :repo-root "/code/futon3" :verdict :escalate
              :escalate/reason :ahead-outlier :ahead-count 21
              :ahead-threshold 10}]
    (testing "caller-supplied live responsible seat"
      (let [decision (first (escalation/route
                             [(assoc base :seat/id "seat:claude-3:s1")]
                             {:live-seats #{"seat:claude-3:s1"}}))]
        (is (= 1 (:route/tier decision)))
        (is (re-find #"21 ahead \(threshold 10\)" (:route/message decision)))))
    (testing "no responsible seat"
      (let [decision (first (escalation/route [base] {:live-seats #{}}))]
        (is (= [2 "street-sweeper"]
               [(:route/tier decision) (:route/recipient decision)]))))))

(deftest explicit-operator-need-cannot-be-downgraded
  (let [item (assoc (plan []) :verdict :held :held/reason :gate-failed
                    :needs-operator true)
        decision (first (escalation/route
                         [item] {:live-seats #{"seat:claude-3:s1"}}))]
    (is (= 3 (:route/tier decision)))
    (is (re-find #"Holding on your word" (:route/message decision)))))

(deftest routing-preserves-order-and-is-deterministic
  (let [items [(assoc (plan []) :verdict :held :held/reason :stale-plan)
               {:record/type :inbox-zero/push-result :seat/id nil
                :repo-root "/code/futon4" :verdict :escalate
                :escalate/reason :no-upstream}]
        context {:live-seats #{"seat:claude-3:s1"}}
        first-pass (escalation/route items context)
        second-pass (escalation/route items context)]
    (is (= first-pass second-pass))
    (is (= items (mapv :route/item first-pass)))
    (is (= [1 2] (mapv :route/tier first-pass)))))
