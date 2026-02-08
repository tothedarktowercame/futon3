(ns futon3.agency.invariants.soak-test
  "Soak test: sustained load against Agency invariants (bounded state + deterministic cleanup).

  Spec:
  - library/agency/bounded-lifecycle.flexiarg (A5-bounded)
  - library/agency/delivery-receipt.flexiarg (A0-delivery)
  - library/agency/invariants.flexiarg (A0-A5 set)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3.agency.invariants.util :as u]
            [futon3.agency.http]))

(use-fixtures
  :each
  (fn [f]
    (u/reset-registry!)
    (u/reset-agency-state!)
    (u/configure-state-dir! (u/temp-dir!))
    (f)))

(defn- var' [ns-sym var-sym]
  (deref (ns-resolve ns-sym var-sym)))

(deftest agency-soak-bounded-and-clean
  (testing "200+ bells, 50+ whistles stay bounded; disconnect cascades pending-whistles cleanup"
    (let [register-local-handler! (var' 'futon3.agency.http 'register-local-handler!)
          unregister-local-handler! (var' 'futon3.agency.http 'unregister-local-handler!)
          send-to-agent! (var' 'futon3.agency.http 'send-to-agent!)
          handle-create-secret (var' 'futon3.agency.http 'handle-create-secret)
          handle-get-secret (var' 'futon3.agency.http 'handle-get-secret)
          handle-ack (var' 'futon3.agency.http 'handle-ack)
          handle-whistle (var' 'futon3.agency.http 'handle-whistle)
          handle-whistle-response (var' 'futon3.agency.http 'handle-whistle-response)
          kick-agent! (var' 'futon3.agency.http 'kick-agent!)
          secrets (var' 'futon3.agency.http 'secrets)
          acks (var' 'futon3.agency.http 'acks)
          rendezvous-state (var' 'futon3.agency.http 'rendezvous-state)
          pending-whistles (var' 'futon3.agency.http 'pending-whistles)
          connected-agents (var' 'futon3.agency.http 'connected-agents)
          agent-id "soak-agent"
          bell-ttl-ms 150
          bell-sends 220
          whistle-sends 60]
      (try
        ;; Local handler: replies to whistles immediately; accepts bells (no-op).
        (register-local-handler!
          agent-id
          (fn [msg]
            (case (:type msg)
              "whistle"
              (handle-whistle-response
                (:request-id msg)
                {:ok true :echo (:prompt msg) :at (System/currentTimeMillis)})
              nil)))

        ;; Bells: create short TTL secrets to avoid unbounded growth; ack all to stress bounded acks.
        (dotimes [i bell-sends]
          (let [{:keys [ok secret-id value]} (handle-create-secret {:ttl-ms bell-ttl-ms})]
            (is ok)
            (is (:ok (send-to-agent! agent-id
                                     {:type "bell"
                                      :bell-type "soak"
                                      :secret-id secret-id
                                      :payload {:i i}})))
            (is (:ok (handle-ack {:secret-id secret-id
                                  :value value
                                  :agent-id agent-id}))))
          (Thread/sleep 15)
          (when (and (pos? i) (zero? (mod i 50)))
            (Thread/sleep 250)
            (let [s (count @secrets)
                  a (count @acks)
                  rv (count @rendezvous-state)
                  pw (count @pending-whistles)]
              (is (<= a 10) (str "acks exceeded bound: " a))
              (is (<= s 50) (str "secrets grew too large for TTL churn: " s))
              (is (<= rv 5) (str "unexpected rendezvous growth: " rv))
              (is (= 0 pw) (str "pending-whistles leaked during bell soak: " pw)))))

        ;; Whistles: run sequentially (local handler replies immediately).
        (dotimes [i whistle-sends]
          (let [resp (handle-whistle {:agent-id agent-id :prompt (str "hi-" i) :timeout-ms 500})]
            (is (= true (:ok resp)))
            (is (= :local-handler (:source resp)))))
        (is (empty? @pending-whistles)
            (str "pending-whistles should be empty after whistle roundtrip: "
                 (keys @pending-whistles)))

        ;; Ensure TTL secrets are actually gone by both async cleanup and lazy expiry-on-read.
        (Thread/sleep (+ bell-ttl-ms 300))
        (doseq [sid (keys @secrets)]
          (handle-get-secret sid))
        (is (<= (count @secrets) 5)
            (str "secrets should be near-empty after TTL: " (count @secrets)))
        (is (<= (count @acks) 10)
            (str "acks should remain bounded: " (count @acks)))

        ;; Disconnect cascade: use WS path (connected-agents) but stub ws-send! so we don't need a real channel.
        (with-redefs [futon3.agency.http/ws-send! (fn [_ch _msg] true)]
          (swap! connected-agents assoc "soak-ws-agent" {:channel nil})
          (let [futs (doall
                      (for [i (range 12)]
                        (future
                          (handle-whistle {:agent-id "soak-ws-agent"
                                           :prompt (str "ws-" i)
                                           :timeout-ms 5000}))))]
            (Thread/sleep 100)
            (is (true? (kick-agent! "soak-ws-agent")))
            (let [results (mapv deref futs)]
              (is (every? (fn [r] (= false (:ok r))) results))
              (is (empty? @pending-whistles)
                  (str "pending-whistles must be empty after kick cascade: "
                       (keys @pending-whistles))))))
        (finally
          (unregister-local-handler! agent-id))))))
