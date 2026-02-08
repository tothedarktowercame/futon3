(ns futon3.agency.registry-integration-test
  "Integration test for unified agent registry.

   Starts Agency on a test port, registers agents, pages them, validates responses.
   Run with: clj -M -m futon3.agency.registry-integration-test"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [cheshire.core :as json]
            [futon3.agency.http :as http]
            [futon3.agency.agents :as agents]
            [futon3.agency.registry :as reg])
  (:import [java.net HttpURLConnection URL]))

(defn http-post [url body]
  (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
               (.setRequestMethod "POST")
               (.setRequestProperty "Content-Type" "application/json")
               (.setDoOutput true))]
    (with-open [os (.getOutputStream conn)]
      (.write os (.getBytes (json/generate-string body) "UTF-8")))
    (let [status (.getResponseCode conn)
          stream (if (< status 400)
                   (.getInputStream conn)
                   (.getErrorStream conn))
          body (slurp stream)]
      {:status status
       :body (try (json/parse-string body true) (catch Exception _ body))})))

(defn http-get [url]
  (let [conn ^HttpURLConnection (.openConnection (URL. url))]
    {:status (.getResponseCode conn)
     :body (json/parse-string (slurp (.getInputStream conn)) true)}))

(def test-port 17070)
(def base-url (str "http://localhost:" test-port))

(deftest test-registry-routing
  (println "\n=== Starting Agency on port" test-port "===")
  (let [stop-fn (http/start! {:port test-port})]
    (try
      (Thread/sleep 1000) ; Let server start

      (testing "Health check"
        (let [{:keys [status body]} (http-get (str base-url "/health"))]
          (is (= 200 status))
          (is (:ok body))))

      (testing "Register mock agent"
        (agents/register-mock! "test-registry-agent")
        (is (reg/agent-registered? "test-registry-agent")))

      (testing "Whistle mock agent via HTTP - should use registry path"
        (let [{:keys [status body]} (http-post (str base-url "/agency/whistle")
                                               {:agent-id "test-registry-agent"
                                                :prompt "Hello from integration test"
                                                :timeout-ms 5000})]
          (is (= 200 status))
          (is (:ok body))
          (is (= :registry (keyword (:source body))) "Should route via registry")
          (is (string? (:response body)))
          (println "  Response:" (:response body))
          (println "  Source:" (:source body))))

      (testing "Registry status endpoint"
        (let [{:keys [status body]} (http-get (str base-url "/agency/registry"))]
          (is (= 200 status))
          (is (:ok body))
          (is (= 1 (get-in body [:registry :count])))))

      (testing "Connected agents includes registry"
        (let [{:keys [body]} (http-get (str base-url "/agency/connected"))]
          (is (some #(= "test-registry-agent" %) (:agents body)))))

      (testing "Multiple agents"
        (agents/register-mock! "test-agent-2")
        (agents/register-mock! "test-agent-3")
        (is (= 3 (:count (reg/registry-status))))

        ;; Page both
        (let [r1 (http-post (str base-url "/agency/whistle")
                           {:agent-id "test-agent-2" :prompt "test2" :timeout-ms 5000})
              r2 (http-post (str base-url "/agency/whistle")
                           {:agent-id "test-agent-3" :prompt "test3" :timeout-ms 5000})]
          (is (:ok (:body r1)))
          (is (:ok (:body r2)))
          (is (= :registry (keyword (get-in r1 [:body :source]))))
          (is (= :registry (keyword (get-in r2 [:body :source]))))))

      (testing "Cleanup"
        (agents/shutdown-all!)
        (is (= 0 (:count (reg/registry-status)))))

      (println "\n=== All tests passed ===\n")

      (finally
        (println "Stopping test server...")
        (stop-fn)))))

(defn -main [& _args]
  (let [results (run-tests 'futon3.agency.registry-integration-test)]
    (System/exit (if (and (zero? (:fail results))
                          (zero? (:error results)))
                   0 1))))
