(ns futon3.inbox-zero.amnesty-exec-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.amnesty-exec :as amnesty-exec])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def computed-at (java.util.Date/from (java.time.Instant/parse
                                       "2026-08-24T11:03:00Z")))
(def executed-at (java.util.Date. 1787573000000))

(defn- git! [repo & args]
  (let [{:keys [exit err] :as result}
        (apply shell/sh (concat ["git"] args [:dir (.getPath repo)]))]
    (when-not (zero? exit)
      (throw (ex-info "test git failed" {:args args :stderr err})))
    result))

(defn- git-result [repo-root & args]
  (apply shell/sh (concat ["git"] args [:dir repo-root])))

(defn- init-repo []
  (let [repo (.toFile (Files/createTempDirectory
                       "amnesty-exec-test-" (make-array FileAttribute 0)))]
    (git! repo "init" "-q")
    (git! repo "config" "user.name" "Amnesty Test")
    (git! repo "config" "user.email" "amnesty@example.invalid")
    (doseq [path ["one.txt" "two.txt" "exempt.txt" "secret.env"]]
      (spit (io/file repo path) (str "base " path "\n")))
    (git! repo "add" ".")
    (git! repo "commit" "-q" "-m" "base")
    repo))

(defn- plan [baseline]
  {:record/type :inbox-zero/amnesty-plan
   :repo/id "fixture"
   :worktree/id "worktree:fixture"
   :computed-at computed-at
   :exempt ["exempt.txt"]
   :sensitive [{:path "secret.env" :rule/kind :credential-file}]
   :baseline baseline
   :counts {:exempt 1 :sensitive 1 :baseline (count baseline)}})

(defn- execute [repo amnesty-plan & [options]]
  (amnesty-exec/execute-amnesty-plan!
   amnesty-plan
   (merge {:repo-root (.getPath repo)
           :git-fn git-result
           :gates []
           :now executed-at}
          options)))

(deftest commits-exact-baseline-and-leaves-other-partitions-dirty
  (let [repo (init-repo)]
    (doseq [path ["one.txt" "two.txt" "exempt.txt" "secret.env"]]
      (spit (io/file repo path) (str "changed " path "\n")))
    (let [result (execute repo (plan ["one.txt" "two.txt"]))
          expected-message (str "inbox-zero: pre-witnessing baseline, 2 path(s)\n\n"
                                "one.txt\ntwo.txt\n\n"
                                "Amnesty-plan: 2026-08-24T11:03:00Z")]
      (is (= :committed (:verdict result)))
      (is (re-matches #"[0-9a-f]{40}" (:commit/sha result)))
      (is (= #{"one.txt" "two.txt"}
             (set (str/split-lines
                   (:out (git! repo "show" "--format=" "--name-only" "HEAD"))))))
      (is (= expected-message
             (str/trim (:out (git! repo "log" "-1" "--format=%B")))))
      (is (= #{"exempt.txt" "secret.env"}
             (set (str/split-lines
                   (:out (git! repo "diff" "--name-only"))))))
      (is (str/blank? (:out (git! repo "diff" "--cached" "--name-only")))))))

(deftest occupied-index-holds-without-staging-baseline
  (let [repo (init-repo)]
    (spit (io/file repo "exempt.txt") "already staged\n")
    (git! repo "add" "exempt.txt")
    (spit (io/file repo "one.txt") "planned\n")
    (let [before (:out (git! repo "diff" "--cached" "--binary"))
          result (execute repo (plan ["one.txt"]))]
      (is (= [:held :index-not-empty]
             [(:verdict result) (:held/reason result)]))
      (is (= ["exempt.txt"] (:index/paths result)))
      (is (= before (:out (git! repo "diff" "--cached" "--binary"))))
      (is (= "base" (str/trim (:out (git! repo "log" "-1" "--format=%s"))))))))

(deftest staged-set-mismatch-is-held-and-index-is-cleaned
  (let [repo (init-repo)
        vanished? (atom false)
        runner (fn [repo-root & args]
                 (if (= ["add" "--" "one.txt" "two.txt"] args)
                   (do
                     (.delete (io/file repo "two.txt"))
                     (reset! vanished? true)
                     (git-result repo-root "add" "--" "one.txt"))
                   (apply git-result repo-root args)))]
    (spit (io/file repo "one.txt") "planned one\n")
    (spit (io/file repo "two.txt") "planned two\n")
    (let [result (execute repo (plan ["one.txt" "two.txt"])
                          {:git-fn runner})]
      (is @vanished?)
      (is (= [:held :staged-set-mismatch]
             [(:verdict result) (:held/reason result)]))
      (is (= ["one.txt"] (:staged/paths result)))
      (is (str/blank? (:out (git! repo "diff" "--cached" "--name-only"))))
      (is (= "base" (str/trim (:out (git! repo "log" "-1" "--format=%s"))))))))

(deftest gate-failure-holds-with-output-and-does-not-commit
  (let [repo (init-repo)]
    (spit (io/file repo "one.txt") "planned\n")
    (let [result (execute repo (plan ["one.txt"])
                          {:gates [{:gate/name :failure
                                    :cmd ["sh" "-c" "printf gate-failed; exit 9"]}]})]
      (is (= [:held :gate-failed]
             [(:verdict result) (:held/reason result)]))
      (is (= [{:gate/name :failure :exit 9 :output "gate-failed"}]
             (:gate-results result)))
      (is (str/blank? (:out (git! repo "diff" "--cached" "--name-only"))))
      (is (= "base" (str/trim (:out (git! repo "log" "-1" "--format=%s"))))))))

(deftest empty-baseline-makes-no-git-calls
  (let [calls (atom [])
        result (amnesty-exec/execute-amnesty-plan!
                (plan [])
                {:repo-root "/not/a/repository"
                 :git-fn (fn [& args]
                           (swap! calls conj args)
                           {:exit 99 :out "" :err "must not run"})
                 :gates []
                 :now executed-at})]
    (is (= :nothing-to-commit (:verdict result)))
    (is (empty? @calls))))

(deftest path-aware-gate-filters-amnesty-baseline
  (let [repo (init-repo)]
    (spit (io/file repo "one.txt") "changed one\n")
    (spit (io/file repo "rule.CLJ") "new rule\n")
    (let [result
          (execute repo (plan ["one.txt" "rule.CLJ"])
                   {:gates [{:gate/name :clojure-paths
                             :cmd ["sh" "-c" "printf '%s|' \"$@\"" "argv0"]
                             :gate/paths? true
                             :gate/extensions #{"clj"}}]})]
      (is (= :committed (:verdict result)))
      (is (= "rule.CLJ|" (-> result :gate-results first :output))))))
