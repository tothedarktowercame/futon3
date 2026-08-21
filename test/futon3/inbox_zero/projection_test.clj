(ns futon3.inbox-zero.projection-test
  (:require [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state]))

(def base-ms 1787306400000)

(defn instant [offset-ms]
  (java.util.Date. (+ base-ms offset-ms)))

(defn seat [agent session]
  {:record/type :inbox-zero/session-seat
   :seat/id (str "seat:" agent ":" session)
   :agent/id agent
   :session/id session
   :surface :emacs-repl
   :host/id "dionysus"
   :workspace/root "/home/joe/code"
   :observed-at (instant 0)
   :registry-witness {:endpoint (str "/api/alpha/agents/" agent)
                      :session/id session
                      :observed-at (instant 0)}})

(defn observation [n path status offset-ms]
  {:record/type :inbox-zero/file-observation
   :observation/id (str "observation:" n)
   :repo/id "futon0"
   :repo/root "/home/joe/code/futon0"
   :worktree/id "worktree:futon0-main"
   :path path
   :git/status status
   :content/hash (when-not (= :deleted status) (str "sha256:" n))
   :head/sha "head-1"
   :observed-at (instant offset-ms)
   :source :multi-watcher})

(defn claim [n seat-id path claim-state offset-ms]
  {:record/type :inbox-zero/session-file-claim
   :claim/id (str "claim:" n)
   :seat/id seat-id
   :repo/id "futon0"
   :worktree/id "worktree:futon0-main"
   :path path
   :relation :edited-by
   :witness/type :tool-edit
   :witness/id (str "tool-call:" n)
   :first-observed-at (instant offset-ms)
   :last-observed-at (instant offset-ms)
   :state claim-state})

(def seat-a (seat "codex-11" "session-a"))
(def seat-b (seat "codex-11" "session-b"))

(defn store [& records]
  (state/replay (concat [seat-a seat-b] records)))

(deftest projects-distinct-current-dirty-paths
  (let [stored (store (observation 1 "a.clj" :modified 1000)
                      (observation 2 "a.clj" :modified 2000)
                      (observation 3 "b.clj" :untracked 3000)
                      (claim 1 (:seat/id seat-a) "a.clj" :active 1000)
                      (claim 2 (:seat/id seat-a) "b.clj" :active 3000))
        result (projection/project-dirty-sets stored (instant 4000))
        dirty-set (first (:dirty-sets result))]
    (is (= 2 (:count dirty-set)))
    (is (= ["a.clj" "b.clj"] (mapv :path (:members dirty-set))))
    (is (= "observation:2" (-> dirty-set :members first :observation/id)))
    (is (= (instant 1000) (:oldest-dirty-at dirty-set)))))

(deftest latest-clean-observation-removes-membership
  (let [stored (store (observation 1 "a.clj" :modified 1000)
                      (observation 2 "a.clj" :clean 2000)
                      (claim 1 (:seat/id seat-a) "a.clj" :active 1000))
        result (projection/project-dirty-sets stored (instant 3000))]
    (is (empty? (:dirty-sets result)))
    (is (empty? (:ambiguous result)))
    (is (empty? (:unattributed result)))))

(deftest competing-current-claims-are-ambiguous
  (let [stored (store (observation 1 "shared.clj" :modified 1000)
                      (claim 1 (:seat/id seat-a) "shared.clj" :active 1000)
                      (claim 2 (:seat/id seat-b) "shared.clj" :active 1000))
        result (projection/project-dirty-sets stored (instant 2000))]
    (is (empty? (:dirty-sets result)))
    (is (= [{:repo/id "futon0"
             :worktree/id "worktree:futon0-main"
             :path "shared.clj"
             :observation/id "observation:1"
             :seat/ids [(:seat/id seat-a) (:seat/id seat-b)]}]
           (:ambiguous result)))))

(deftest later-release-removes-one-competing-claim
  (let [stored (store (observation 1 "shared.clj" :modified 1000)
                      (claim 1 (:seat/id seat-a) "shared.clj" :active 1000)
                      (claim 2 (:seat/id seat-b) "shared.clj" :active 1000)
                      (claim 3 (:seat/id seat-b) "shared.clj" :released 2000))
        result (projection/project-dirty-sets stored (instant 3000))]
    (is (= (:seat/id seat-a) (-> result :dirty-sets first :seat/id)))
    (is (empty? (:ambiguous result)))))

(deftest new-session-does-not-inherit-old-session-claim
  (let [stored (store (observation 1 "a.clj" :modified 1000)
                      (claim 1 (:seat/id seat-a) "a.clj" :active 1000))
        dirty-set (-> (projection/project-dirty-sets stored (instant 2000))
                      :dirty-sets first)]
    (is (= (:seat/id seat-a) (:seat/id dirty-set)))
    (is (not= (:seat/id seat-b) (:seat/id dirty-set)))))

(deftest default-count-threshold-is-five-distinct-paths
  (let [records (mapcat (fn [n]
                          [(observation n (str n ".clj") :modified n)
                           (claim n (:seat/id seat-a) (str n ".clj") :active n)])
                        (range 1 6))
        dirty-set (-> (apply store records)
                      (projection/project-dirty-sets (instant 10000))
                      :dirty-sets first)
        four-files (update dirty-set :members pop)]
    (is (= {:type :count :threshold 5}
           (:trigger (projection/eligibility dirty-set (instant 10000)))))
    (is (nil? (projection/eligibility
               (assoc four-files :count 4
                                 :oldest-dirty-at (instant 1))
               (instant 10000))))))

(deftest age-threshold-catches-one-old-file
  (let [day-ms (:dirty-age-threshold-ms projection/default-policy)
        stored (store (observation 1 "old.clj" :modified 0)
                      (claim 1 (:seat/id seat-a) "old.clj" :active 0))
        dirty-set (-> (projection/project-dirty-sets stored (instant day-ms))
                      :dirty-sets first)]
    (is (= {:type :age :threshold-ms day-ms}
           (:trigger (projection/eligibility dirty-set (instant day-ms)))))))

(deftest dedupe-key-is-stable-across-scan-time-and-member-order
  (let [stored (store (observation 1 "a.clj" :modified 1000)
                      (observation 2 "b.clj" :modified 2000)
                      (claim 1 (:seat/id seat-a) "a.clj" :active 1000)
                      (claim 2 (:seat/id seat-a) "b.clj" :active 2000))
        dirty-set (-> (projection/project-dirty-sets stored (instant 3000))
                      :dirty-sets first)
        policy {:dirty-count-threshold 2}
        first-result (projection/eligibility dirty-set (instant 3000) policy)
        rescanned (-> dirty-set
                      (assoc :computed-at (instant 9000))
                      (update :members #(vec (reverse %))))
        second-result (projection/eligibility rescanned (instant 9000) policy)]
    (is (= (:dedupe/key first-result) (:dedupe/key second-result)))
    (is (= 64 (count (last (:dedupe/key first-result)))))))

(deftest unclaimed-dirty-path-is-reported
  (let [result (projection/project-dirty-sets
                (store (observation 1 "orphan.clj" :modified 1000))
                (instant 2000))]
    (is (= ["orphan.clj"] (mapv :path (:unattributed result))))
    (is (empty? (:dirty-sets result)))))
