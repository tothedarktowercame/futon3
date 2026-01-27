(ns futon3.fulab-wiring-test
  (:require [clojure.test :refer :all]
            [futon3.fulab.wiring :as wiring]))

(deftest interpret-step-branches
  (let [diagram {:start :sense
                 :nodes [{:id :sense :type :sense}
                         {:id :act :type :act}
                         {:id :stop :type :stop}]
                 :edges [{:from :sense :to :act :guard {:type :uncertainty-gt :value 0.5}}
                         {:from :sense :to :stop :guard {:type :uncertainty-lt :value 0.5}}]}
        result (wiring/interpret-step diagram {} {} {:uncertainty 0.7})
        result-low (wiring/interpret-step diagram {} {} {:uncertainty 0.2})]
    (is (= :sense (get-in result [:telemetry :node/id])))
    (is (= :act (:node-id (:new-state result))))
    (is (= :stop (:node-id (:new-state result-low))))))

(deftest interpret-step-done-guard
  (let [diagram {:start :act
                 :nodes [{:id :act :type :act}
                         {:id :stop :type :stop}]
                 :edges [{:from :act :to :stop :guard {:type :done? :value true}}]}
        result (wiring/interpret-step diagram {} {} {:done? true})
        result-false (wiring/interpret-step diagram {} {} {:done? false})]
    (is (= :stop (:node-id (:new-state result))))
    (is (nil? (:node-id (:new-state result-false))))))
