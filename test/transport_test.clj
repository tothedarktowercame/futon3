(ns transport-test
  (:require [clojure.test :refer :all]
            [f2.transport :as transport]))

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
    :history (atom [])}))

(deftest hello-updates-client
  (let [state (make-state)
        resp (transport/apply-envelope! state "C-1" {:type "hello" :client "alice" :caps ["eval"]})]
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
