(ns transport-test
  (:require [clojure.test :refer :all]
            [f2.adapters.mock :as mock]
            [f2.transport :as transport]
            [futon3.checks :as checks]
            [futon3.workday :as workday]))

(defn make-state
  ([] (make-state {}))
  ([config]
   {:config (atom (merge {:repl {:mode :off}}
                         config))
    :clients (atom {"C-1" {:id "C-1"
                             :name "anon"
                             :caps #{"eval"}
                             :connected? true
                             :remote-addr "127.0.0.1"}})
    :history (atom [])
    :adapters (atom (mock/adapters))
    :router (atom nil)}))

(deftest hello-updates-client
  (let [state (make-state)
        resp (transport/apply-envelope! state "C-1" {:type "hello" :rev 1 :client "alice" :caps ["eval"]})]
    (is (= true (get-in resp [:reply :ok])))
    (is (= "alice" (get-in @(:clients state) ["C-1" :name])))))

(deftest eval-disabled-by-default
  (let [state (make-state)
        resp (transport/apply-envelope! state "C-1" {:type "eval" :payload {:code "(+ 1 2)"}})]
    (is (= "repl-disabled" (get-in resp [:reply :err])))))

(deftest safe-eval-executes
  (let [state (make-state {:repl {:mode :safe}})
        resp (transport/apply-envelope! state "C-1" {:type "eval" :payload {:code "(+ 1 2 3)"}})]
    (is (= 6 (get-in resp [:reply :result])))
    (is (= "eval" (get-in resp [:reply :type])))))

(deftest admin-mode-requires-token-and-local-remote
  (let [state (make-state {:repl {:mode :admin :admin-token "secret"}})
        missing (transport/apply-envelope! state "C-1" {:type "eval" :payload {:code "(+ 1 1)" :mode "admin"}})]
    (is (= "admin-auth-failed" (get-in missing [:reply :err]))))
  (let [state (make-state {:repl {:mode :admin :admin-token "secret"}})
        resp (transport/apply-envelope! state "C-1" {:type "eval" :payload {:code "(+ 1 1)" :mode "admin" :token "secret"}})]
    (is (= 2 (get-in resp [:reply :result])))
    (is (= "admin" (get-in resp [:reply :mode]))))
  (let [state (make-state {:repl {:mode :admin :admin-token "secret" :admin-allow #{"10.0.0.1"}}})]
    (swap! (:clients state) assoc-in ["C-1" :remote-addr] "127.0.0.1")
    (let [resp (transport/apply-envelope! state "C-1" {:type "eval" :payload {:code "(+ 1 1)" :mode "admin" :token "secret"}})]
      (is (= "admin-auth-failed" (get-in resp [:reply :err]))))))

(deftest workday-submission-logs-entry
  (let [state (make-state)
        tmp (doto (java.io.File/createTempFile "workday" ".edn")
              (.delete)
              (.deleteOnExit))
        original (workday/current-log-path)]
    (try
      (workday/set-log-path! (.getAbsolutePath tmp))
      (let [resp (transport/apply-envelope! state "C-1"
                                            {:type "workday"
                                             :payload {:activity "triage pattern obligations"
                                                       :evidence ["link screenshot"]}})]
        (is (= true (get-in resp [:reply :ok])))
        (is (= "workday" (get-in resp [:reply :type])))
        (is (.exists tmp))
        (is (= :workday (-> @(:history state) last :type))))
      (finally
        (workday/set-log-path! original)))))

(deftest check-route-evaluates-pattern
  (let [state (make-state)
        tmp (doto (java.io.File/createTempFile "checks" ".edn")
              (.delete)
              (.deleteOnExit))
        original (checks/current-log-path)]
    (try
      (checks/set-log-path! (.getAbsolutePath tmp))
      (let [resp (transport/apply-envelope! state "C-1"
                                            {:type "check"
                                             :payload {:pattern/id "library/devmap-coherence/ifr-f3-piti"
                                                       :context "workday submit proves proof trail"
                                                       :evidence ["workday submit"]}})]
        (is (= true (get-in resp [:reply :ok])))
        (is (= "check" (get-in resp [:reply :type])))
        (is (= :applies (get-in resp [:reply :status])))
        (is (= "library/devmap-coherence/ifr-f3-piti"
               (get-in resp [:reply :proof :pattern/id]))))
      (finally
        (checks/set-log-path! original)))))

(deftest routes-event-through-router
  (let [state (make-state)
        payload {:type "event"
                  :payload {:msg-id "msg-1"
                            :t "2024-01-01T00:00:00Z"
                            :actor "alice"
                            :verb "observes"
                            :object "practice"
                            :prov {:file "demo" :line 1}}}
        resp (transport/apply-envelope! state "C-1" payload)]
    (is (= "ack" (get-in resp [:reply :type])))
    (is (= "msg-1" (-> @(:router state) :msg-cache deref keys first)))))
