(ns futon3.musn-plan-wiring-test
  (:require [clojure.test :refer :all]
            [futon3.musn.plan-wiring :as plan]))

(def sample-diagram
  {:nodes [{:id :cmd :component :musn.plan/action-command
            :params {:cmd "rg -n \"wiring\" src"}}
           {:id :seq :component :musn.plan/sequence}]
   :edges [{:from :cmd :to :seq :to-port :actions}]
   :output :seq})

(def choose-diagram
  {:nodes [{:id :cmd :component :musn.plan/action-command
            :params {:cmd "rg -n \"plan\" src"}}
           {:id :reason :component :musn.plan/action-reasoning
            :params {:note "fallback"}}
           {:id :choose :component :musn.plan/if-then-else}
           {:id :out :component :musn.plan/sequence}]
   :edges [{:value true :value-type :bool :to :choose :to-port :cond}
           {:from :cmd :to :choose :to-port :then}
           {:from :reason :to :choose :to-port :else}
           {:from :choose :to :out :to-port :actions}]
   :output :out})

(deftest validate-basic-diagram
  (let [lib (plan/load-components)
        validation (plan/validate-diagram lib sample-diagram)]
    (is (:ok? validation))
    (is (empty? (:errors validation)))))

(deftest validate-disconnected-diagram
  (let [lib (plan/load-components)
        diagram (update sample-diagram :nodes conj
                        {:id :ghost :component :musn.plan/action-reasoning
                         :params {:note "unused"}})
        validation (plan/validate-diagram lib diagram)]
    (is (false? (:ok? validation)))
    (is (some #(re-find #"disconnected node" %) (:errors validation)))))

(deftest evaluate-plan-actions
  (let [actions (plan/evaluate-plan sample-diagram)
        action (first actions)]
    (is (= 1 (count actions)))
    (is (= :command_execution (:musn.action/type action)))
    (is (= {:cmd "rg -n \"wiring\" src"}
           (:musn.action/spec action)))))

(deftest evaluate-if-then-else
  (let [actions (plan/evaluate-plan choose-diagram)
        action (first actions)]
    (is (= 1 (count actions)))
    (is (= :command_execution (:musn.action/type action)))))

(deftest evaluate-plan-with-eval
  (let [{:keys [actions eval]} (plan/evaluate-plan+eval sample-diagram)]
    (is (= 1 (count actions)))
    (is (number? (:musn.plan/confidence eval)))
    (is (<= 0.0 (:musn.plan/confidence eval) 1.0))
    (is (<= 0.0 (:musn.plan/risk eval) 1.0))
    (is (map? (:musn.plan/mana eval)))))
