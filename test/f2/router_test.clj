(ns f2.router-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [f2.adapters.mock :as mock]
            [f2.router :as router]))

(defn test-router []
  (router/create {:adapters (mock/adapters)
                  :log! (constantly nil)
                  :run-id-fn (fn [] "RUN-test")}))

(def client {:id "C-1"})

(deftest hello-negotiation
  (let [r (test-router)
        resp (router/dispatch r :hello client {:client "alice"
                                               :caps ["eval"]
                                               :rev router/protocol-revision})]
    (is (= true (get-in resp [:reply :ok])))
    (is (= router/protocol-revision (get-in resp [:reply :rev]))))
  (let [r (test-router)
        resp (router/dispatch r :hello client {:client "alice"
                                               :caps []
                                               :rev 99})]
    (is (= false (get-in resp [:reply :ok])))
    (is (= "unsupported-rev" (get-in resp [:reply :err])))))

(deftest event-validation-and-idempotency
  (let [r (test-router)
        payload {:msg-id "m-1"
                  :t "2024-01-01T00:00:00Z"
                  :actor "alice"
                  :verb "observes"
                  :object "practice"
                  :prov {:file "demo" :line 1}}
        envelope {:payload payload}
        resp (router/dispatch r :event client envelope)
        resp2 (router/dispatch r :event client envelope)]
    (is (= "ack" (get-in resp [:reply :type])))
    (is (= resp resp2))
    (is (= "RUN-test" (get-in resp [:reply :run-id]))))
  (let [r (test-router)
        bad {:payload {:msg-id "oops" :object "practice"}}
        ex-type (try
                  (router/dispatch r :event client bad)
                  :no-throw
                  (catch clojure.lang.ExceptionInfo ex
                    (:type (ex-data ex))))]
    (is (= :event ex-type))))

(deftest hundred-identical-events-return-same-eid
  (let [r (test-router)
        payload {:msg-id "bulk-1"
                  :t "2024-01-01T00:00:00Z"
                  :actor "bulk"
                  :verb "observes"
                  :object "practice"
                  :prov {:file "demo" :line 1}}
        envelope {:payload payload}
        responses (repeatedly 100 #(router/dispatch r :event client envelope))]
    (is (= 1 (count (distinct responses))))
    (is (= (get-in (first responses) [:reply :eid])
           (get-in (last responses) [:reply :eid])))))

(deftest run-and-status-flow
  (let [r (test-router)
        run-env {:payload {:msg-id "run-1"
                           :sid "SID-1"
                           :policy {:mode :demo :options {}}}}
        run-resp (router/dispatch r :run client run-env)
        job-id (get-in run-resp [:reply :job-id])
        status-env {:payload {:msg-id "status-1" :job-id job-id}}
        status-resp (router/dispatch r :status client status-env)]
    (is (= "ack" (get-in run-resp [:reply :type])))
    (is (= "status" (get-in status-resp [:reply :type])))
    (is (= :done (get-in status-resp [:reply :state])))
    (is (= job-id (get-in status-resp [:reply :metrics :job-id])))
    (is (get-in status-resp [:reply :metrics :completed]))))

(deftest session-close-uses-adapter
  (let [r (test-router)
        resp (router/dispatch r :session-close client {:payload {:msg-id "bye"
                                                                 :sid "SID-9"}})]
    (is (= "SID-9" (get-in resp [:reply :sid])))))

(deftest export-ack-returns-scenario-path-without-touching-files
  (let [r (test-router)
        resp (router/dispatch r :export client {:payload {:msg-id "exp-1"
                                                          :sid "SID-10"}})
        path (get-in resp [:reply :scenario-path])]
    (is (= "ack" (get-in resp [:reply :type])))
    (is (string? path))
    (is (not (.exists (io/file path))))))
