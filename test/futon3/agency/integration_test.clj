(ns futon3.agency.integration-test
  "Integration tests for Agency runtime behavior (HTTP handler + registry + bells/whistles).

  These tests are Phase 1 deliverables for holes/missions/M-agency-rebuild.md."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3.agency.invariants.util :as u]
            [futon3.agency.registry :as reg]
            [cheshire.core :as json]
            [futon3.agency.http :as agency-http]))

(use-fixtures
  :each
  (fn [f]
    (u/reset-registry!)
    (u/reset-agency-state!)
    (u/configure-state-dir! (u/temp-dir!))
    (f)))

(defn- call-handler
  [req]
  (let [handler @(ns-resolve 'futon3.agency.http 'handler)]
    (handler req)))

(deftest registry-whistle-roundtrip
  (testing "whistle via HTTP handler uses registry path and returns response"
    (reg/register-agent!
     {:agent-id "itest-reg-agent"
      :type :codex
      :invoke-fn (fn [_prompt session-id _timeout-ms]
                   {:result "hello" :session-id (or session-id "s1") :exit-code 0})})
    (let [resp (call-handler (u/ring-json-request :post "/agency/whistle"
                                                  {:agent-id "itest-reg-agent"
                                                   :prompt "hi"
                                                   :timeout-ms 1000}))
          body (u/parse-json-response resp)]
      (is (= 200 (:status resp)))
      (is (= true (:ok body)))
      (is (= "registry" (:source body)))
      (is (contains? body :response)))))

(deftest bell-and-ack-roundtrip
  (testing "bell + secret + ack records receipt"
    (let [bell-resp (call-handler (u/ring-json-request :post "/agency/bell"
                                                       {:agent-id "all"
                                                        :type "test-bell"
                                                        :payload {:message "ack me"}}))
          bell-body (u/parse-json-response bell-resp)
          bell (get bell-body :bell)
          secret-id (:secret-id bell)
          secret-value (:secret-value bell-body)
          ack-resp (call-handler (u/ring-json-request :post "/agency/ack"
                                                      {:secret-id secret-id
                                                       :value secret-value
                                                       :agent-id "itest"}))
          ack-body (u/parse-json-response ack-resp)]
      (is (= 200 (:status bell-resp)))
      (is (= true (:ok bell-body)))
      (is (string? secret-id))
      (is (string? secret-value))
      (is (= 200 (:status ack-resp)))
      (is (= true (:ok ack-body))))))

(deftest rendezvous-ack-tracking
  (testing "standup rendezvous status reflects acks"
    (reg/register-agent!
     {:agent-id "rv-agent"
      :type :codex
      :invoke-fn (fn [_prompt session-id _timeout-ms] {:result "ok" :session-id session-id :exit-code 0})})
    (let [standup-resp (call-handler (u/ring-json-request :post "/agency/standup"
                                                         {:room "standup"
                                                          :deadline-ms 2000}))
          standup-body (u/parse-json-response standup-resp)
          rv-id (:rendezvous-id standup-body)
          secret-value (:secret-value standup-body)]
      (is (= 200 (:status standup-resp)))
      (is (= true (:ok standup-body)))
      (is (string? rv-id))
      (let [status1 (u/parse-json-response
                     (call-handler {:request-method :get
                                    :uri (str "/agency/rendezvous/" rv-id)}))
            missing1 (get-in status1 [:rendezvous :missing])]
        (is (some #{"rv-agent"} missing1)))
      ;; Ack rendezvous secret as rv-agent
      (u/parse-json-response
       (call-handler (u/ring-json-request :post "/agency/ack"
                                          {:secret-id rv-id
                                           :value secret-value
                                           :agent-id "rv-agent"})))
      (let [status2 (u/parse-json-response
                     (call-handler {:request-method :get
                                    :uri (str "/agency/rendezvous/" rv-id)}))
            acked2 (set (get-in status2 [:rendezvous :acked]))]
        (is (contains? acked2 "rv-agent"))))))

(deftest agent-local-handler-send-to-agent-delivers
  (testing "local handler path: send-to-agent! delivers message to handler"
    (let [register-local-handler! @(ns-resolve 'futon3.agency.http 'register-local-handler!)
          unregister-local-handler! @(ns-resolve 'futon3.agency.http 'unregister-local-handler!)
          send-to-agent! @(ns-resolve 'futon3.agency.http 'send-to-agent!)
          recv (atom [])]
      (try
        (register-local-handler! "itest-local" (fn [msg] (swap! recv conj msg)))
        (let [receipt (send-to-agent! "itest-local" {:type "bell" :payload {:msg "hi"}})]
          (is (= true (:ok receipt)))
          (is (= :local-handler (:transport receipt)))
          (is (= "bell" (get-in (first @recv) [:type]))))
        (finally
          (unregister-local-handler! "itest-local"))))))

(deftest agent-ws-send-to-agent-invokes-ws-send
  (testing "ws path: send-to-agent! calls ws-send! and returns explicit receipt"
    (let [send-to-agent! @(ns-resolve 'futon3.agency.http 'send-to-agent!)
          connected-agents @(ns-resolve 'futon3.agency.http 'connected-agents)
          called (atom [])]
      (with-redefs [agency-http/ws-send! (fn [ch msg] (swap! called conj {:ch ch :msg msg}) true)]
        (swap! connected-agents assoc "itest-ws" {:channel ::dummy})
        (let [receipt (send-to-agent! "itest-ws" {:type "bell" :payload {:msg "hi"}})]
          (is (= true (:ok receipt)))
          (is (= :websocket (:transport receipt)))
          (is (= 1 (count @called))))))))

(deftest a1-local-then-ws-evicts-local
  (testing "A1 end-to-end: local handler registration is evicted when WS registers same agent-id"
    (let [register-local-handler! @(ns-resolve 'futon3.agency.http 'register-local-handler!)
          unregister-local-handler! @(ns-resolve 'futon3.agency.http 'unregister-local-handler!)
          local-handlers @(ns-resolve 'futon3.agency.http 'local-handlers)
          connected-agents @(ns-resolve 'futon3.agency.http 'connected-agents)]
      (try
        (register-local-handler! "itest-a1" (fn [_] nil))
        (is (contains? @local-handlers "itest-a1"))
        (swap! connected-agents assoc "itest-a1" {:channel nil})
        (is (contains? @connected-agents "itest-a1"))
        ;; A1 watch should evict local immediately.
        (is (not (contains? @local-handlers "itest-a1")))
        (finally
          (unregister-local-handler! "itest-a1"))))))
