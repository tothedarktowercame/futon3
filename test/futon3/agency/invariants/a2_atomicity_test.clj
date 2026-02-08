(ns futon3.agency.invariants.a2-atomicity-test
  "Invariant A2 (atomicity): state transitions are all-or-nothing; corruption is rejected.

  Spec:
  - library/agency/state-atomicity.flexiarg
  - library/agency/invariants.flexiarg (A2-atomicity)"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3.agency.invariants.util :as u]
            [futon3.agency.service :as svc]))

(use-fixtures
  :each
  (fn [f]
    (u/reset-registry!)
    (u/reset-agency-state!)
    (u/configure-state-dir! (u/temp-dir!))
    (f)))

(deftest corrupted-state-must-not-silently-load
  (testing "corrupted .edn state file should throw or return explicit error (not nil)"
    ;; EXPECTED FAIL (A2): load-agent-state catches parse errors and returns nil.
    (let [agent-id "corrupt-agent"
          state-dir (:state-dir (svc/config))
          path (io/file state-dir (str agent-id ".edn"))
          load-agent-state @(ns-resolve 'futon3.agency.service 'load-agent-state)]
      (io/make-parents path)
      (spit path "{this is not edn")
      (let [res (try
                  {:threw false :val (load-agent-state agent-id)}
                  (catch Exception e
                    {:threw true :ex e}))]
        (is (or (:threw res)
                (and (map? (:val res)) (contains? (:val res) :error)))
            (str "silent load produced: " (:val res)))))))

(deftest save-agent-state-roundtrips
  (testing "save-agent-state! should write state that round-trips via edn/read-string"
    (let [agent-id "roundtrip-agent"
          state-dir (:state-dir (svc/config))
          path (io/file state-dir (str agent-id ".edn"))
          save-agent-state! @(ns-resolve 'futon3.agency.service 'save-agent-state!)]
      (let [state {:agent/id agent-id :agent/current-thread-id "t-1" :agent/summary "s"}]
        (save-agent-state! agent-id state)
        (is (.exists path))
        (is (= state (edn/read-string (slurp path))))))))

(deftest continuity-id-must-not-be-nil-after-update
  (testing "continuity id should not be set to nil by any state transition"
    ;; EXPECTED FAIL (A2): roll-over-state! and other transitions can set :agent/current-thread-id nil.
    (let [update-agent-state! @(ns-resolve 'futon3.agency.service 'update-agent-state!)]
      (svc/ensure-agent-state! "no-nil-agent")
      (update-agent-state! "no-nil-agent" (fn [st] (assoc st :agent/current-thread-id "t1")))
      (update-agent-state! "no-nil-agent" (fn [st] (assoc st :agent/current-thread-id nil)))
      (is (some? (:agent/current-thread-id (svc/ensure-agent-state! "no-nil-agent")))))))

(deftest save-agent-state-must-use-atomic-write
  (testing "save-agent-state! should use temp-file + fsync + rename (atomic write)"
    ;; EXPECTED FAIL (A2): current implementation uses spit directly.
    (let [src (slurp (io/file "src/futon3/agency/service.clj"))]
      (is (re-find #"fsync|FileChannel|rename" src))
      (is (not (re-find #"\(spit\s+path" src))))))
