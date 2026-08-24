(ns futon3.inbox-zero.amnesty-test
  (:require [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.amnesty :as amnesty]
            [futon3.inbox-zero.state :as state]))

(def now (java.util.Date. 1787572000000))

(defn observation
  [n repo worktree path status]
  {:record/type :inbox-zero/file-observation
   :observation/id (str "observation:" n)
   :repo/id repo
   :repo/root (str "/home/joe/code/" repo)
   :worktree/id worktree
   :path path
   :git/status status
   :content/hash (when-not (= :deleted status) (str "sha256:" n))
   :head/sha "head-1"
   :observed-at (java.util.Date. (+ 1787571000000 n))
   :source :multi-watcher})

(defn store
  [& observations]
  (state/replay observations))

(deftest deliberate-tracer-remains-exempt
  (let [tracer {:repo/id "futon3c-d" :path "scripts/session-cost.py"}
        plans (amnesty/plan-amnesty
               (store (observation 1 "futon3c-d" "worktree:futon3c-d-main"
                                   "scripts/session-cost.py" :untracked))
               {:exempt #{tracer} :now now})
        plan (first plans)]
    (is (= ["scripts/session-cost.py"] (:exempt plan)))
    (is (empty? (:baseline plan)))
    (is (empty? (:sensitive plan)))
    (is (= {:exempt 1 :sensitive 0 :baseline 0} (:counts plan)))))

(deftest partitions-sensitive-and-ordinary-paths
  (let [plan (first
              (amnesty/plan-amnesty
               (store (observation 1 "futon3" "worktree:futon3-main"
                                   ".env" :untracked)
                      (observation 2 "futon3" "worktree:futon3-main"
                                   "src/futon3/core.clj" :modified))
               {:exempt #{} :now now}))]
    (is (= [{:path ".env" :rule/kind :credential-file}]
           (:sensitive plan)))
    (is (= ["src/futon3/core.clj"] (:baseline plan)))
    (is (empty? (:exempt plan)))
    (is (= {:exempt 0 :sensitive 1 :baseline 1} (:counts plan)))))

(deftest empty-and-fully-attributed-states-have-no-plan
  (is (= [] (amnesty/plan-amnesty (store) {:exempt #{} :now now})))
  (is (= []
         (amnesty/plan-amnesty
          (store (observation 1 "futon3" "worktree:futon3-main"
                              "clean.clj" :clean))
          {:exempt #{} :now now}))))

(deftest plans-and-paths-are-deterministic-and-sorted
  (let [stored (store
                (observation 1 "z-repo" "worktree:z" "z.clj" :modified)
                (observation 2 "a-repo" "worktree:b" "b.clj" :modified)
                (observation 3 "a-repo" "worktree:b" "a.clj" :untracked)
                (observation 4 "a-repo" "worktree:a" "c.clj" :deleted))
        options {:exempt #{} :now now}
        first-plan (amnesty/plan-amnesty stored options)
        second-plan (amnesty/plan-amnesty stored options)]
    (is (= first-plan second-plan))
    (is (= [["a-repo" "worktree:a"]
            ["a-repo" "worktree:b"]
            ["z-repo" "worktree:z"]]
           (mapv (juxt :repo/id :worktree/id) first-plan)))
    (is (= ["a.clj" "b.clj"] (:baseline (second first-plan))))))
