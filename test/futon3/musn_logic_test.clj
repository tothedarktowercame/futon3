(ns futon3.musn-logic-test
  (:require [clojure.test :refer :all]
            [futon3.musn.logic :as logic]))

(defn- obligations [result]
  (get-in result [:logic :obligations]))

(deftest action-missing-plan
  (let [state {:plan? false
               :selection {}
               :write? false}
        action {:action "implement"}
        result (logic/check-action state action)]
    (is (some #(= :missing-plan (:obligation %)) (obligations result)))))

(deftest action-missing-selection
  (let [state {:plan? true
               :selection nil
               :write? false}
        action {:action "update"}
        result (logic/check-action state action)]
    (is (some #(= :missing-selection (:obligation %)) (obligations result)))))

(deftest action-missing-consent
  (let [state {:plan? false
               :selection {}
               :write? false}
        action {:action "read"
                :cost :expensive}
        result (logic/check-action state action)]
    (is (some #(= :missing-consent (:obligation %)) (obligations result)))))

(deftest action-off-trail
  (let [state {:plan? true
               :selection {}
               :write? false
               :trail {:on 0 :off 5 :limit 1}}
        action {:action "read"}
        result (logic/check-action state action {:off-trail? true
                                                 :trail (:trail state)})]
    (is (some #(= :off-trail (:obligation %)) (obligations result)))))

(deftest turn-end-no-write
  (let [state {:selection {:selection/reason {:mode :use}}
               :write? false}
        result (logic/check-turn-end state)]
    (is (some #(= :no-write (:obligation %)) (obligations result)))))

(deftest turn-end-write-present
  (let [state {:selection {:selection/reason {:mode :use}}
               :write? true}
        result (logic/check-turn-end state)]
    (is (empty? (obligations result)))))
