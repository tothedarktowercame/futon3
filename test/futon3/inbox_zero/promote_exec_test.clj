(ns futon3.inbox-zero.promote-exec-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3.inbox-zero.promote-exec :as promote-exec])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- git! [repo & args]
  (let [{:keys [exit err] :as result}
        (apply shell/sh (concat ["git"] args [:dir (.getPath repo)]))]
    (when-not (zero? exit)
      (throw (ex-info "test git failed" {:args args :stderr err})))
    result))

(defn- init-repo []
  (let [repo (.toFile (Files/createTempDirectory
                       "promotion-exec-test-" (make-array FileAttribute 0)))]
    (git! repo "init" "-q")
    (git! repo "config" "user.name" "Promotion Test")
    (git! repo "config" "user.email" "promotion@example.invalid")
    (spit (io/file repo "modified.txt") "base modified\n")
    (spit (io/file repo "deleted.txt") "base deleted\n")
    (spit (io/file repo "unplanned.txt") "base unplanned\n")
    (git! repo "add" ".")
    (git! repo "commit" "-q" "-m" "base")
    repo))

(defn- plan
  ([include] (plan :proposed include))
  ([verdict include]
   {:record/type :inbox-zero/promotion-plan
    :seat/id "seat:test" :repo/id "fixture" :worktree/id "worktree:test"
    :computed-at (java.util.Date. 1000) :include include :exclude []
    :verdict verdict
    :held/reason (when (= :held verdict) :nothing-promotable)}))

(defn- execute [repo plan & [options]]
  (promote-exec/execute-plan!
   plan (merge {:repo-root (.getPath repo) :gates [] :message "promote planned paths"}
               options)))

(deftest modified-and-deleted-paths-commit-exactly
  (let [repo (init-repo)]
    (spit (io/file repo "modified.txt") "changed\n")
    (.delete (io/file repo "deleted.txt"))
    (let [result (execute repo (plan [{:path "modified.txt" :git/status :modified}
                                      {:path "deleted.txt" :git/status :deleted}])
                          {:gates [{:gate/name :passing
                                    :cmd ["sh" "-c" "printf gate-ok"]}]})]
      (is (= :committed (:verdict result)))
      (is (re-matches #"[0-9a-f]{40}" (:commit/sha result)))
      (is (= [{:gate/name :passing :exit 0 :output "gate-ok"}]
             (:gate-results result)))
      (is (= #{"deleted.txt" "modified.txt"}
             (set (str/split-lines (:out (git! repo "show" "--format="
                                               "--name-only" "HEAD"))))))
      (is (str/blank? (:out (git! repo "status" "--porcelain" "--"
                                  "modified.txt" "deleted.txt")))))))

(deftest unplanned-dirty-path-remains-untouched
  (let [repo (init-repo)]
    (spit (io/file repo "modified.txt") "planned\n")
    (spit (io/file repo "unplanned.txt") "unplanned dirt\n")
    (execute repo (plan [{:path "modified.txt" :git/status :modified}]))
    (is (= " M unplanned.txt\n"
           (:out (git! repo "status" "--porcelain" "--" "unplanned.txt"))))
    (is (not (str/includes? (:out (git! repo "show" "--format=" "--name-only" "HEAD"))
                            "unplanned.txt")))))

(deftest failing-gate-holds-with-bounded-output-and-clean-index
  (let [repo (init-repo)
        marker (io/file repo "later-gate-ran")]
    (spit (io/file repo "modified.txt") "changed\n")
    (let [result (execute repo (plan [{:path "modified.txt" :git/status :modified}])
                          {:gates [{:gate/name :failure
                                    :cmd ["sh" "-c" "printf failed-output; exit 7"]}
                                   {:gate/name :later
                                    :cmd ["sh" "-c" (str "touch " (.getPath marker))]}]})]
      (is (= [:held :gate-failed] [(:verdict result) (:held/reason result)]))
      (is (= 7 (-> result :gate-results first :exit)))
      (is (= "failed-output" (-> result :gate-results first :output)))
      (is (= 1 (count (:gate-results result))))
      (is (not (.exists marker)))
      (is (str/blank? (:out (git! repo "diff" "--cached" "--name-only"))))
      (is (= "base" (str/trim (:out (git! repo "log" "-1" "--format=%s"))))))))

(deftest stale-cleaned-plan-holds-and-second-execution-is-stale
  (testing "clean before first execution"
    (let [repo (init-repo)
          promotion (plan [{:path "modified.txt" :git/status :modified}])]
      (is (= :stale-plan (:held/reason (execute repo promotion))))))
  (testing "successful plan becomes stale after its paths are committed"
    (let [repo (init-repo)
          promotion (plan [{:path "modified.txt" :git/status :modified}])]
      (spit (io/file repo "modified.txt") "changed\n")
      (is (= :committed (:verdict (execute repo promotion))))
      (is (= :stale-plan (:held/reason (execute repo promotion)))))))

(deftest held-plan-performs-no-git-activity
  (let [held (plan :held [])
        result (promote-exec/execute-plan!
                held {:repo-root "/definitely/not/a/repository"
                      :message "unused" :gates []})]
    (is (= [:held :plan-already-held]
           [(:verdict result) (:held/reason result)]))))

(deftest blank-message-fails-before-git-activity
  (let [repo (init-repo)
        before (:out (git! repo "status" "--porcelain=v1"))]
    (spit (io/file repo "modified.txt") "changed\n")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"message must be non-blank"
                          (execute repo
                                   (plan [{:path "modified.txt" :git/status :modified}])
                                   {:message "  "})))
    (is (= before ""))
    (is (str/blank? (:out (git! repo "diff" "--cached" "--name-only"))))))

(deftest occupied-index-holds-before-gates-without-changing-index
  (let [repo (init-repo)
        marker (io/file repo "gate-ran")]
    (spit (io/file repo "unplanned.txt") "pre-staged\n")
    (git! repo "add" "unplanned.txt")
    (spit (io/file repo "modified.txt") "planned\n")
    (let [before (:out (git! repo "diff" "--cached" "--binary"))
          result (execute repo (plan [{:path "modified.txt" :git/status :modified}])
                          {:gates [{:gate/name :must-not-run
                                    :cmd ["sh" "-c" (str "touch " (.getPath marker))]}]})
          after (:out (git! repo "diff" "--cached" "--binary"))]
      (is (= [:held :index-not-empty] [(:verdict result) (:held/reason result)]))
      (is (= ["unplanned.txt"] (:index/paths result)))
      (is (empty? (:gate-results result)))
      (is (not (.exists marker)))
      (is (= before after)))))
