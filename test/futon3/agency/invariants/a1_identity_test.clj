(ns futon3.agency.invariants.a1-identity-test
  "Invariant A1 (identity): single routing authority + self-attribution.

  Spec:
  - library/agency/single-routing-authority.flexiarg
  - library/agency/self-attribution.flexiarg
  - library/agency/identifier-separation.flexiarg (supports A1/A2)
  - library/agency/invariants.flexiarg (A1-identity)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3.agency.invariants.util :as u]
            [futon3.agency.registry :as reg]
            [futon3.agency.service :as svc]))

(use-fixtures
  :each
  (fn [f]
    (u/reset-registry!)
    (u/reset-agency-state!)
    (u/configure-state-dir! (u/temp-dir!))
    (f)))

(deftest connected-agent-ids-includes-registry
  (testing "connected-agent-ids should include registry agents (single list of known agents)"
    (reg/register-agent!
     {:agent-id "a1-reg-agent"
      :type :codex
      :invoke-fn (fn [_ s] {:result "ok" :session-id s :exit-code 0})})
    (let [connected-agent-ids @(ns-resolve 'futon3.agency.http 'connected-agent-ids)]
      (is (some #{"a1-reg-agent"} (connected-agent-ids))))))

(deftest no-dual-authority-for-same-agent-id
  (testing "same agent-id must not exist in multiple routing stores at once"
    ;; EXPECTED FAIL (A1): registry + local-handlers can coexist today.
    (reg/register-agent!
     {:agent-id "dual-agent"
      :type :codex
      :invoke-fn (fn [_ s] {:result "ok" :session-id s :exit-code 0})})
    ((ns-resolve 'futon3.agency.http 'register-local-handler!)
     "dual-agent" (fn [_msg] nil))
    (let [local-handlers @(ns-resolve 'futon3.agency.http 'local-handlers)
          connected-agents @(ns-resolve 'futon3.agency.http 'connected-agents)
          in-registry? (some? (reg/get-agent "dual-agent"))
          in-local? (contains? @local-handlers "dual-agent")
          in-ws? (contains? @connected-agents "dual-agent")
          n (count (filter true? [in-registry? in-local? in-ws?]))]
      (is (<= n 1) (str "agent-id present in " n " stores")))))

(deftest connected-agent-ids-one-transport-per-agent
  (testing "each connected agent-id must map to exactly one transport"
    ;; EXPECTED FAIL (A1): can be present in registry + local-handlers + ws simultaneously.
    (reg/register-agent!
     {:agent-id "tri-agent"
      :type :codex
      :invoke-fn (fn [_ s] {:result "ok" :session-id s :exit-code 0})})
    ((ns-resolve 'futon3.agency.http 'register-local-handler!)
     "tri-agent" (fn [_msg] nil))
    (swap! @(ns-resolve 'futon3.agency.http 'connected-agents)
           assoc "tri-agent" {:channel nil :registered-at (java.time.Instant/now)})
    (let [connected-agent-ids @(ns-resolve 'futon3.agency.http 'connected-agent-ids)
          local-handlers @(ns-resolve 'futon3.agency.http 'local-handlers)
          connected-agents @(ns-resolve 'futon3.agency.http 'connected-agents)]
      (doseq [aid (connected-agent-ids)]
        (let [in-registry? (some? (reg/get-agent aid))
              in-local? (contains? @local-handlers aid)
              in-ws? (contains? @connected-agents aid)
              n (count (filter true? [in-registry? in-local? in-ws?]))]
          (is (= 1 n)
              (str "agent-id " aid " present in " n " stores")))))))

(deftest identifier-separation-forum-thread-id-not-continuity
  (testing "forum thread-id must not clobber LLM resume-id in continuity state"
    (let [run-peripheral! (ns-resolve 'futon3.agency.service 'run-peripheral!)
          ;; Stub out codex runner so we only exercise the state update logic.
          stub-run (fn [_opts] {:ok true :response "ok" :thread-id "llm-thread-2" :usage {:total_tokens 1}})
          stub-get-peripheral (fn [_id] {:runner :codex})
          stub-build-run-prompt (fn [_peripheral _state _ctx] "prompt")]
      (with-redefs [futon3.agency.service/run-codex! stub-run
                    futon3.agency.service/get-peripheral stub-get-peripheral
                    futon3.agency.service/build-run-prompt stub-build-run-prompt]
        ;; Arrange: continuity resume-id plus a forum transport thread id.
        (svc/ensure-agent-state! "idsep-agent")
        (run-peripheral! {:agent-id "idsep-agent"
                          :peripheral "chat"
                          :prompt "hi"
                          :resume-id "llm-thread-1"
                          :forum {:thread-id "forum-thread-999"}})
        (let [st (svc/ensure-agent-state! "idsep-agent")]
          (is (= "llm-thread-2" (:agent/current-thread-id st)))
          (is (not= "forum-thread-999" (:agent/current-thread-id st))))))))

