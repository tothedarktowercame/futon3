(ns futon3.agency.invariants.a0-delivery-test
  "Invariant A0 (delivery receipt): every message send returns a receipt or explicit failure.

  Spec:
  - library/agency/delivery-receipt.flexiarg
  - library/agency/invariants.flexiarg (A0-delivery)"
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

(deftest send-to-agent-explicit-failure
  (testing "send-to-agent! should return explicit failure result (not boolean)"
    ;; EXPECTED FAIL (A0): send-to-agent! currently returns boolean true/false.
    (let [send-to-agent! @(ns-resolve 'futon3.agency.http 'send-to-agent!)
          r (send-to-agent! "nonexistent-agent" {:type "bell" :payload {:message "hi"}})]
      (is (map? r))
      (is (= false (:ok r)))
      (is (string? (:err r))))))

(deftest secret-ack-roundtrip-exists
  (testing "secret creation + retrieval + ack exists (receipt mechanism primitive)"
    (let [handle-create-secret @(ns-resolve 'futon3.agency.http 'handle-create-secret)
          handle-get-secret @(ns-resolve 'futon3.agency.http 'handle-get-secret)
          handle-ack @(ns-resolve 'futon3.agency.http 'handle-ack)
          {:keys [ok secret-id value]} (handle-create-secret {:ttl-ms 5000})]
      (is ok)
      (is (string? secret-id))
      (is (string? value))
      (is (= {:ok true :value value}
             (handle-get-secret secret-id)))
      (let [ack-resp (handle-ack {:secret-id secret-id :value value :agent-id "a0-test-agent"})]
        (is (:ok ack-resp))
        (is (= secret-id (:secret-id ack-resp)))))))

(deftest whistle-to-registered-agent-returns-response
  (testing "whistle to registry agent returns :ok true and :response"
    (reg/register-agent!
     {:agent-id "a0-reg-agent"
      :type :codex
      :invoke-fn (fn [_prompt session-id _timeout-ms]
                   {:result "hello" :session-id (or session-id "s1") :exit-code 0})})
    (let [handle-whistle @(ns-resolve 'futon3.agency.http 'handle-whistle)
          resp (handle-whistle {:agent-id "a0-reg-agent" :prompt "hi" :timeout-ms 2000})]
      (is (= true (:ok resp)))
      (is (= :registry (:source resp)))
      (is (contains? resp :response)))))

(deftest whistle-to-unregistered-agent-is-explicit
  (testing "whistle to unregistered agent should include :err and :agent-id"
    ;; EXPECTED FAIL (A0): current failure omits :agent-id (it returns {:ok false :err ...}).
    (let [handle-whistle @(ns-resolve 'futon3.agency.http 'handle-whistle)
          resp (handle-whistle {:agent-id "missing-agent" :prompt "hi" :timeout-ms 50})]
      (is (= false (:ok resp)))
      (is (string? (:err resp)))
      (is (= "missing-agent" (:agent-id resp))))))

(deftest throw-exceptions-false-must-check-status
  (testing ":throw-exceptions false must be followed by an HTTP status check"
    ;; EXPECTED FAIL (A0): current code uses :throw-exceptions false without checking :status.
    (let [paths ["src/futon3/agency/http.clj"
                 "src/futon3/agency/service.clj"]
          occurrences
          (for [p paths
                :let [lines (vec (clojure.string/split-lines (u/read-file p)))]
                i (range (count lines))
                :when (re-find #":throw-exceptions\\s+false" (nth lines i))]
            {:file p
             :line (inc i)
             :context (subvec lines i (min (+ i 25) (count lines)))})
          missing-check
          (filter (fn [{:keys [context]}]
                    (not (some #(re-find #"\(:status|:status" %) context)))
                  occurrences)]
      (is (empty? missing-check)
          (str "missing status checks near :throw-exceptions false:\n"
               (clojure.string/join
                "\n"
                (map (fn [{:keys [file line]}]
                       (format "%s:%d" file line))
                     missing-check)))))))
