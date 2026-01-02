(ns futon3.trail-bridge-test
  (:require [clojure.test :refer :all]
            [futon3.trail-bridge :as bridge])
  (:import (java.util UUID)))

(deftest entry->payload-normalizes-fields
  (let [entry {:session-id "S-1"
               :clock "2024-05-01T12:00:00Z"
               :intent "Plan"
               :events [{:kind :note}]}
        cues {:fruits [{:fruit/id :doable}]
              :paramitas [{:paramita/id :truth}]}
        payload (#'bridge/trail-payload entry cues nil "default")]
    (is (= "S-1" (:session-id payload)))
    (is (= "default" (:profile payload)))
    (is (= [{:kind :note}] (:events payload)))
    (is (number? (:timestamp payload)))))

(deftest publish-skips-when-disabled
  (let [called (atom false)
        entry {:session-id "S" :intent "x"}
        cues {}]
    (with-redefs [bridge/trail-payload (fn [_ _ _ _] {:session-id "S"})
                  org.httpkit.client/post (fn [_ _ cb]
                                           (reset! called true)
                                           (when cb (cb {:status 200})))]
      (bridge/publish! {:enabled? false} entry cues nil)
      (is (false? @called))
      (bridge/publish! {:enabled? true :api-base "http://localhost" :profile "default"}
                       entry cues nil)
      (is @called))))
