(ns futon3.agency.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3.agency.registry :as reg]))

(deftest invoke-timeout-is-enforced
  (testing "registry-level timeout returns promptly and reports timeout"
    (reg/shutdown-all!)
    (reg/register-agent!
     {:agent-id "t-timeout"
      :type :codex
      :invoke-fn (fn [_prompt session-id]
                   (Thread/sleep 200)
                   {:result "late"
                    :session-id session-id
                    :exit-code 0})})
    (let [resp (reg/invoke-agent! "t-timeout" "hi" 50)]
      (is (false? (:ok resp)))
      (is (= "timeout" (:error resp))))))

(deftest invoke-does-not-resurrect-after-unregister
  (testing "invoke does not re-add agent record if it is unregistered mid-call"
    (reg/shutdown-all!)
    (reg/register-agent!
     {:agent-id "t-race"
      :type :codex
      :invoke-fn (fn [_prompt session-id]
                   (Thread/sleep 200)
                   {:result "done"
                    :session-id session-id
                    :exit-code 0})})
    (let [f (future (reg/invoke-agent! "t-race" "hi" 1000))]
      (Thread/sleep 50)
      (is (true? (reg/unregister-agent! "t-race")))
      (deref f 2000 :timeout)
      (is (false? (reg/agent-registered? "t-race"))))))

