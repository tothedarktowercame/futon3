(ns futon3.inbox-zero.promote-push-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3.inbox-zero.promote-push :as promote-push])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- git! [repo & args]
  (let [{:keys [exit err] :as result}
        (apply shell/sh (concat ["git"] args [:dir (.getPath repo)]))]
    (when-not (zero? exit)
      (throw (ex-info "test git failed" {:args args :stderr err})))
    result))

(defn- configure! [repo]
  (git! repo "config" "user.name" "Push Test")
  (git! repo "config" "user.email" "push@example.invalid")
  repo)

(defn- commit! [repo message]
  (git! repo "commit" "-q" "--allow-empty" "-m" message)
  (str/trim (:out (git! repo "rev-parse" "HEAD"))))

(defn- remote-head [bare branch]
  (str/trim (:out (git! bare "rev-parse" (str "refs/heads/" branch)))))

(defn- fixture []
  (let [bare (temp-dir "promotion-bare-")
        local (temp-dir "promotion-local-")]
    (git! bare "init" "--bare" "-q")
    (git! local "init" "-q" "-b" "main")
    (configure! local)
    (spit (io/file local "seed.txt") "seed\n")
    (git! local "add" "seed.txt")
    (commit! local "seed")
    (git! local "remote" "add" "origin" (.getPath bare))
    (git! local "push" "-q" "-u" "origin" "main")
    {:bare bare :local local}))

(defn- push! [repo & [options]]
  (promote-push/push-promoted!
   (merge {:repo-root (.getPath repo) :remote "origin" :branch "main"}
          options)))

(deftest ordinary-fast-forward-pushes-and-records-ahead-count
  (let [{:keys [bare local]} (fixture)
        local-head (commit! local "ordinary")
        result (push! local {:ahead-threshold 1})]
    (is (= :pushed (:verdict result)))
    (is (nil? (:escalate/reason result)))
    (is (= 1 (:ahead-count result)))
    (is (= 1 (:ahead-threshold result)))
    (is (= local-head (remote-head bare "main")))))

(deftest ahead-outlier-does-not-move-remote
  (let [{:keys [bare local]} (fixture)
        before (remote-head bare "main")]
    (commit! local "one")
    (commit! local "two")
    (let [result (push! local {:ahead-threshold 1})]
      (is (= [:escalate :ahead-outlier]
             [(:verdict result) (:escalate/reason result)]))
      (is (= 2 (:ahead-count result)))
      (is (= before (remote-head bare "main"))))))

(deftest divergent-remote-escalates-push-failure-without-moving-either-head
  (let [{:keys [bare local]} (fixture)
        other-parent (temp-dir "promotion-other-parent-")
        other (io/file other-parent "clone")]
    (git! other-parent "clone" "-q" (.getPath bare) (.getPath other))
    (configure! other)
    (git! other "checkout" "-q" "main")
    (commit! other "remote divergence")
    (git! other "push" "-q" "origin" "main")
    (let [remote-before (remote-head bare "main")
          local-before (commit! local "local divergence")
          result (push! local {:ahead-threshold 10})]
      (is (= [:escalate :push-failed]
             [(:verdict result) (:escalate/reason result)]))
      (is (= 1 (:ahead-count result))
          "the deliberately stale local upstream undercounts remote divergence")
      (is (pos? (count (:output result))))
      (is (<= (count (:output result)) 4096))
      (is (= local-before (str/trim (:out (git! local "rev-parse" "HEAD")))))
      (is (= remote-before (remote-head bare "main"))))))

(deftest missing-upstream-and-remote-branch-escalates-distinctly
  (let [repo (temp-dir "promotion-no-upstream-")]
    (git! repo "init" "-q" "-b" "main")
    (configure! repo)
    (commit! repo "local only")
    (let [result (push! repo)]
      (is (= [:escalate :no-upstream]
             [(:verdict result) (:escalate/reason result)]))
      (is (nil? (:ahead-count result)))
      (is (= 10 (:ahead-threshold result))))))

(deftest threshold-defaults-to-ten-and-is-configurable
  (let [{:keys [bare local]} (fixture)
        before (remote-head bare "main")]
    (dotimes [n 11] (commit! local (str "offline-" n)))
    (testing "default ten holds eleven commits"
      (let [result (push! local)]
        (is (= :ahead-outlier (:escalate/reason result)))
        (is (= 11 (:ahead-count result)))
        (is (= 10 (:ahead-threshold result)))
        (is (= before (remote-head bare "main")))))
    (testing "a configured threshold of eleven permits the same accumulation"
      (let [result (push! local {:ahead-threshold 11})]
        (is (= :pushed (:verdict result)))
        (is (= 11 (:ahead-count result)))
        (is (= 11 (:ahead-threshold result)))))))
