(ns futon3.inbox-zero.confirm-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3.inbox-zero.confirm :as confirm]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state]))

(def seat-id "seat:claude-3:7cdc25b0-2189-4f90-801e-c517e7f37d4d")
(def worktree-id "worktree:83d88d109136f3c8")
(def confirmed-at #inst "2026-08-24T12:15:00Z")

(def tracer-proposal
  {:path/key {:repo/id "futon3c-d"
              :worktree/id worktree-id
              :path "scripts/session-cost.py"}
   :candidate {:seat/id seat-id
               :confidence :corroborated
               :evidence [{:source/id "emacs-6427"}
                          {:source/id "claim:781b"}
                          {:source/id "emacs-6427"}]}})

(def tracer-confirmation
  {:confirmed-by seat-id
   :at confirmed-at
   :response/id "response:claude-3-confirms-tracer"})

(def seat-record
  {:record/type :inbox-zero/session-seat
   :seat/id seat-id
   :agent/id "claude-3"
   :session/id "7cdc25b0-2189-4f90-801e-c517e7f37d4d"
   :surface :agent-tool-stream
   :host/id "zone"
   :workspace/root "/home/joe/code/futon3c"
   :observed-at #inst "2026-08-24T11:12:09Z"
   :registry-witness
   {:endpoint :invoke-tool-result
    :session/id "7cdc25b0-2189-4f90-801e-c517e7f37d4d"
    :observed-at #inst "2026-08-24T11:12:09Z"}})

(def tracer-observation
  {:record/type :inbox-zero/file-observation
   :observation/id "observation:tracer"
   :repo/id "futon3c-d"
   :repo/root "/home/joe/code/futon3c"
   :worktree/id worktree-id
   :path "scripts/session-cost.py"
   :git/status :modified
   :content/hash "sha256:tracer"
   :head/sha "5ac4531b"
   :observed-at #inst "2026-08-24T11:14:44Z"
   :source :multi-watcher})

(defn error-type [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (:error/type (ex-data error)))))

(deftest confirmed-tracer-becomes-attributed
  (let [claim (confirm/confirmation-claim tracer-proposal tracer-confirmation)
        before (state/replay [seat-record tracer-observation])
        after (state/apply-record before claim)
        before-view (projection/project-dirty-sets before confirmed-at)
        after-view (projection/project-dirty-sets after confirmed-at)
        dirty-set (first (:dirty-sets after-view))]
    (is (= claim (state/validate-record claim)))
    (is (= ["scripts/session-cost.py"]
           (mapv :path (:unattributed before-view))))
    (is (empty? (:unattributed after-view)))
    (is (= seat-id (:seat/id dirty-set)))
    (is (= ["scripts/session-cost.py"] (mapv :path (:members dirty-set))))
    (is (= (:claim/id claim) (-> dirty-set :members first :claim/id)))))

(deftest confirmer-must-be-the-candidate-seat
  (is (= :inbox-zero/confirmer-mismatch
         (error-type #(confirm/confirmation-claim
                       tracer-proposal
                       (assoc tracer-confirmation :confirmed-by
                              "seat:claude-4:another-session"))))))

(deftest claim-id-is-canonical-over-distinct-sorted-evidence
  (let [claim (confirm/confirmation-claim tracer-proposal tracer-confirmation)
        reordered (update-in tracer-proposal [:candidate :evidence]
                             #(vec (reverse %)))
        repeated (confirm/confirmation-claim tracer-proposal tracer-confirmation)]
    (is (= (:claim/id claim) (:claim/id repeated)))
    (is (= (:claim/id claim)
           (:claim/id (confirm/confirmation-claim reordered tracer-confirmation))))))

(deftest missing-confirmation-fields-fail-closed
  (testing "response id"
    (let [error (try
                  (confirm/confirmation-claim tracer-proposal
                                              (dissoc tracer-confirmation :response/id))
                  nil
                  (catch clojure.lang.ExceptionInfo value value))]
      (is (= :inbox-zero/invalid-confirmation (:error/type (ex-data error))))
      (is (= :response/id (:field (ex-data error))))))
  (testing "instant"
    (is (= :inbox-zero/invalid-confirmation
           (error-type #(confirm/confirmation-claim
                         tracer-proposal
                         (assoc tracer-confirmation :at ""))))))
  (testing "proposal"
    (is (= :inbox-zero/invalid-confirmation-proposal
           (error-type #(confirm/confirmation-claim
                         (dissoc tracer-proposal :path/key)
                         tracer-confirmation))))))

(deftest claim-preserves-confirmation-and-evidence-semantics
  (let [claim (confirm/confirmation-claim tracer-proposal tracer-confirmation)]
    (is (= :confirmed-attribution (:witness/type claim)))
    (is (not= :tool-edit (:witness/type claim)))
    (is (= :edited-by-session (:relation claim)))
    (is (= "response:claude-3-confirms-tracer" (:witness/id claim)))
    (is (= ["claim:781b" "emacs-6427"] (:attribution/evidence claim)))))
