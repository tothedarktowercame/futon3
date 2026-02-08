(ns futon3.agency.integration-test
  "Integration tests for Agency runtime behavior (HTTP handler + registry + bells/whistles).

  These tests are Phase 1 deliverables for holes/missions/M-agency-rebuild.md."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3.agency.invariants.util :as u]
            [futon3.agency.registry :as reg]
            [cheshire.core :as json]))

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
      :invoke-fn (fn [_ s] {:result "ok" :session-id s :exit-code 0})})
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
