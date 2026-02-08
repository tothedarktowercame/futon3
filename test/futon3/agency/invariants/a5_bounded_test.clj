(ns futon3.agency.invariants.a5-bounded-test
  "Invariant A5 (bounded lifecycle): every in-memory state entry has a bounded lifetime with deterministic cleanup.

  Spec:
  - library/agency/bounded-lifecycle.flexiarg
  - library/agency/invariants.flexiarg (A5-bounded)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3.agency.invariants.util :as u]))

(use-fixtures
  :each
  (fn [f]
    (u/reset-registry!)
    (u/reset-agency-state!)
    (u/configure-state-dir! (u/temp-dir!))
    (f)))

(deftest acks-must-be-bounded
  (testing "acks atom should be bounded (evicted/cleaned)"
    ;; EXPECTED FAIL (A5): acks atom grows without bound.
    (let [handle-create-secret @(ns-resolve 'futon3.agency.http 'handle-create-secret)
          handle-ack @(ns-resolve 'futon3.agency.http 'handle-ack)
          acks @(ns-resolve 'futon3.agency.http 'acks)]
      (doseq [i (range 100)]
        (let [{:keys [secret-id value]} (handle-create-secret {:ttl-ms 1000})]
          (handle-ack {:secret-id secret-id :value value :agent-id (str "agent-" i)})))
      (is (<= (count @acks) 10)
          (str "acks size is unbounded: " (count @acks))))))

(deftest secrets-must-expire-deterministically
  (testing "after TTL, secret retrieval should fail even if cleanup future has not run"
    ;; EXPECTED FAIL (A5): handle-get-secret does not enforce TTL; relies on async cleanup.
    (let [handle-create-secret @(ns-resolve 'futon3.agency.http 'handle-create-secret)
          handle-get-secret @(ns-resolve 'futon3.agency.http 'handle-get-secret)
          {:keys [secret-id]} (handle-create-secret {:ttl-ms 100})]
      (Thread/sleep 300)
      (let [resp (handle-get-secret secret-id)]
        (is (= false (:ok resp))
            (str "expected expired secret to be rejected, got: " resp))))))

(deftest pending-whistles-empties-after-timeout
  (testing "pending-whistles should be empty after whistle timeout"
    (let [register-local-handler! @(ns-resolve 'futon3.agency.http 'register-local-handler!)
          unregister-local-handler! @(ns-resolve 'futon3.agency.http 'unregister-local-handler!)
          handle-whistle @(ns-resolve 'futon3.agency.http 'handle-whistle)
          pending-whistles @(ns-resolve 'futon3.agency.http 'pending-whistles)]
      (try
        ;; Handler receives message but never responds (does not call handle-whistle-response).
        (register-local-handler! "silent-agent" (fn [_msg] nil))
        (let [resp (handle-whistle {:agent-id "silent-agent" :prompt "hi" :timeout-ms 50})]
          (is (= false (:ok resp)))
          (is (empty? @pending-whistles)
              (str "pending-whistles leaked entries: " (keys @pending-whistles))))
        (finally
          (unregister-local-handler! "silent-agent"))))))

(deftest disconnect-must-cascade-to-pending-whistles
  (testing "disconnecting an agent must cascade cleanup of that agent's pending whistles"
    ;; EXPECTED FAIL (A5): disconnect/kick does not cascade to pending-whistles.
    (let [kick-agent! @(ns-resolve 'futon3.agency.http 'kick-agent!)
          connected-agents @(ns-resolve 'futon3.agency.http 'connected-agents)
          pending-whistles @(ns-resolve 'futon3.agency.http 'pending-whistles)
          req-id "req-cascade"
          p (promise)]
      (swap! connected-agents assoc "cascade-agent" {:channel nil})
      (swap! pending-whistles assoc req-id {:promise p :agent-id "cascade-agent" :created-at (System/currentTimeMillis)})
      (kick-agent! "cascade-agent")
      (is (empty? @pending-whistles)
          (str "pending-whistles still contains: " (keys @pending-whistles))))))

