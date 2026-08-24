(ns futon3.inbox-zero.promotion-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3.inbox-zero.promotion :as promotion]
            [futon3.inbox-zero.state :as state]))

(def now (java.util.Date. 9000))

(defn seat [id]
  {:record/type :inbox-zero/session-seat
   :seat/id id :agent/id id :session/id (str id "-session")
   :surface :test :host/id "test-host" :workspace/root "/code"
   :observed-at (java.util.Date. 0)
   :registry-witness {:session/id (str id "-session")}})

(def seat-a (seat "seat-a"))
(def seat-b (seat "seat-b"))

(defn observation
  ([n path status] (observation n "repo-a" "worktree-a" path status))
  ([n repo worktree path status]
   {:record/type :inbox-zero/file-observation
    :observation/id (str "observation:" n)
    :repo/id repo :repo/root (str "/code/" repo)
    :worktree/id worktree :path path :git/status status
    :content/hash (when-not (= :deleted status) (str "sha256:" n))
    :head/sha "head" :observed-at (java.util.Date. n)
    :source :multi-watcher}))

(defn claim
  ([n seat-id path] (claim n seat-id "repo-a" "worktree-a" path))
  ([n seat-id repo worktree path]
   {:record/type :inbox-zero/session-file-claim
    :claim/id (str "claim:" n) :seat/id seat-id :repo/id repo
    :worktree/id worktree :path path :relation :edited-by
    :witness/type :tool-edit :witness/id (str "tool:" n)
    :first-observed-at (java.util.Date. n)
    :last-observed-at (java.util.Date. n) :state :active}))

(defn store [& records]
  (state/replay (concat [seat-a seat-b] records)))

(deftest sole-claimed-dirty-path-is-included
  (let [plans (promotion/plan-promotion
               (store (observation 1 "a.clj" :modified)
                      (claim 1 (:seat/id seat-a) "a.clj"))
               (:seat/id seat-a) now)]
    (is (= [{:path "a.clj" :git/status :modified :claim/id "claim:1"}]
           (:include (first plans))))
    (is (= :proposed (:verdict (first plans))))
    (is (nil? (:held/reason (first plans))))))

(deftest fail-closed-exclusions-never-enter-include
  (let [stored (store
                (observation 1 "ambiguous.clj" :modified)
                (observation 2 "unattributed.clj" :untracked)
                (observation 3 "other.clj" :modified)
                (claim 1 (:seat/id seat-a) "ambiguous.clj")
                (claim 2 (:seat/id seat-b) "ambiguous.clj")
                (claim 3 (:seat/id seat-b) "other.clj"))
        plan (first (promotion/plan-promotion stored (:seat/id seat-a) now))]
    (is (empty? (:include plan)))
    (is (= [{:path "ambiguous.clj" :reason :ambiguous}
            {:path "other.clj" :reason :other-seat}
            {:path "unattributed.clj" :reason :unattributed}]
           (:exclude plan)))))

(deftest deleted-sole-claimed-path-is-included
  (let [plan (first (promotion/plan-promotion
                     (store (observation 1 "gone.clj" :deleted)
                            (claim 1 (:seat/id seat-a) "gone.clj"))
                     (:seat/id seat-a) now))]
    (is (= :deleted (-> plan :include first :git/status)))))

(deftest path-cleaned-before-planning-is-absent
  (let [plan (first (promotion/plan-promotion
                     (store (observation 1 "a.clj" :modified)
                            (observation 2 "a.clj" :clean)
                            (claim 1 (:seat/id seat-a) "a.clj")
                            (observation 3 "b.clj" :modified)
                            (claim 2 (:seat/id seat-a) "b.clj"))
                     (:seat/id seat-a) now))]
    (is (= ["b.clj"] (mapv :path (:include plan))))
    (is (not-any? #(= "a.clj" (:path %)) (concat (:include plan) (:exclude plan))))))

(deftest no-promotable-paths-produce-loud-held-plan
  (let [plans (promotion/plan-promotion
               (store (observation 1 "orphan.clj" :modified))
               (:seat/id seat-a) now)
        plan (first plans)]
    (is (= 1 (count plans)))
    (is (= :held (:verdict plan)))
    (is (= :nothing-promotable (:held/reason plan)))
    (is (= [{:path "orphan.clj" :reason :unattributed}] (:exclude plan)))))

(deftest ambiguous-and-unattributed-only-worktree-remains-visible
  (let [plan (first
              (promotion/plan-promotion
               (store (observation 1 "ambiguous.clj" :modified)
                      (observation 2 "orphan.clj" :modified)
                      (claim 1 (:seat/id seat-a) "ambiguous.clj")
                      (claim 2 (:seat/id seat-b) "ambiguous.clj"))
               (:seat/id seat-a) now))]
    (is (= :held (:verdict plan)))
    (is (= #{:ambiguous :unattributed} (set (map :reason (:exclude plan)))))))

(deftest two-worktrees-produce-deterministically-ordered-plans
  (let [stored (store
                (observation 1 "repo-z" "worktree-z" "z.clj" :modified)
                (claim 1 (:seat/id seat-a) "repo-z" "worktree-z" "z.clj")
                (observation 2 "repo-a" "worktree-a" "a.clj" :modified)
                (claim 2 (:seat/id seat-a) "repo-a" "worktree-a" "a.clj"))
        plans (promotion/plan-promotion stored (:seat/id seat-a) now)]
    (is (= [["repo-a" "worktree-a"] ["repo-z" "worktree-z"]]
           (mapv (juxt :repo/id :worktree/id) plans)))))

(deftest globally-clean-state-has-no-plan
  (testing "no worktree tuple exists, so no unscoped held edge is manufactured"
    (is (= [] (promotion/plan-promotion
               (store (observation 1 "a.clj" :clean))
               (:seat/id seat-a) now)))))

(deftest planning-is-pure-and-has-no-io-dependencies
  (let [stored (store (observation 1 "a.clj" :modified)
                      (claim 1 (:seat/id seat-a) "a.clj"))
        first-plan (promotion/plan-promotion stored (:seat/id seat-a) now)
        second-plan (promotion/plan-promotion stored (:seat/id seat-a) now)]
    (is (= first-plan second-plan))
    (is (= #{'projection} (set (keys (ns-aliases 'futon3.inbox-zero.promotion)))))))
