(ns futon3.inbox-zero.promote-exec
  "Execute one already-computed inbox-zero promotion plan.

  This boundary owns the Git index only when it starts empty. It revalidates
  status classes, runs caller-supplied gates, stages exactly the planned paths,
  verifies the complete cached path set, and commits without pushing."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.util Date]))

(def output-limit 4096)
(def index-path-limit 100)

(defn- bounded [text]
  (subs (str text) 0 (min output-limit (count (str text)))))

(defn- command-result [repo-root command]
  (apply shell/sh (concat command [:dir repo-root])))

(defn- git-result [repo-root & args]
  (command-result repo-root (into ["git"] args)))

(defn- git! [repo-root & args]
  (let [{:keys [exit err] :as result} (apply git-result repo-root args)]
    (when-not (zero? exit)
      (throw (ex-info "Promotion Git command failed"
                      {:error/type :inbox-zero/git-failed
                       :command (into ["git"] args)
                       :stderr err
                       :exit exit})))
    result))

(defn- nul-paths [output]
  (->> (str/split output #"\u0000" -1)
       (remove str/blank?)
       vec))

(defn- cached-paths [repo-root]
  (nul-paths (:out (git! repo-root "diff" "--cached" "--name-only" "-z"))))

(defn- status-class [repo-root path]
  (let [output (:out (git! repo-root "status" "--porcelain=v1" "-z"
                            "--untracked-files=all" "--" path))]
    (when-not (str/blank? output)
      (let [xy (subs output 0 2)]
        (cond
          (str/includes? xy "R") :renamed
          (str/includes? xy "D") :deleted
          (= xy "??") :untracked
          (= xy "!!") :ignored
          :else :modified)))))

(defn- result [plan verdict reason gate-results & [extra]]
  (merge {:record/type :inbox-zero/promotion-result
          :plan plan
          :verdict verdict
          :commit/sha nil
          :held/reason reason
          :gate-results gate-results
          :executed-at (Date.)}
         extra))

(defn- run-gates [repo-root gates]
  (loop [remaining gates results []]
    (if-let [{:gate/keys [name] :keys [cmd]} (first remaining)]
      (let [{:keys [exit out err]}
            (try
              (command-result repo-root cmd)
              (catch Exception error
                {:exit -1 :out "" :err (.getMessage error)}))
            gate-result {:gate/name name
                         :exit exit
                         :output (bounded (str out err))}
            results* (conj results gate-result)]
        (if (zero? exit)
          (recur (next remaining) results*)
          {:passed? false :results results*}))
      {:passed? true :results results})))

(defn- validate-input! [plan gates message]
  (when-not (and (string? message) (not (str/blank? message)))
    (throw (ex-info "Promotion commit message must be non-blank"
                    {:error/type :inbox-zero/invalid-message})))
  (when-not (vector? gates)
    (throw (ex-info "Promotion gates must be a vector"
                    {:error/type :inbox-zero/invalid-gates})))
  (doseq [gate gates]
    (when-not (and (keyword? (:gate/name gate))
                   (vector? (:cmd gate))
                   (seq (:cmd gate))
                   (every? string? (:cmd gate)))
      (throw (ex-info "Promotion gate is malformed"
                      {:error/type :inbox-zero/invalid-gate :gate gate}))))
  (let [paths (mapv :path (:include plan))]
    (when-not (and (seq paths)
                   (every? #(and (string? %) (not (str/blank? %))) paths)
                   (= (count paths) (count (set paths))))
      (throw (ex-info "Proposed promotion plan must contain distinct paths"
                      {:error/type :inbox-zero/invalid-plan :paths paths})))))

(defn execute-plan!
  "Execute PLAN in REPO-ROOT after ordered GATES, committing with MESSAGE.

  An input held plan is passed through without Git activity. Proposed plans
  require an initially empty index; an occupied index is surfaced unchanged as
  :index-not-empty. Once staging begins, every exceptional path resets the
  index before propagating the failure. This function never pushes."
  [plan {:keys [repo-root gates message] :or {gates []}}]
  (if (= :held (:verdict plan))
    (result plan :held :plan-already-held [])
    (do
      (validate-input! plan gates message)
      (let [occupied (cached-paths repo-root)]
        (cond
          (seq occupied)
          (result plan :held :index-not-empty []
                  {:index/paths (vec (take index-path-limit occupied))})

          (some (fn [{:keys [path git/status]}]
                  (not= status (status-class repo-root path)))
                (:include plan))
          (result plan :held :stale-plan [])

          :else
          (let [{:keys [passed? results]} (run-gates repo-root gates)]
            (if-not passed?
              (result plan :held :gate-failed results)
              (let [paths (mapv :path (:include plan))
                    planned-paths (set paths)
                    staging-started? (atom false)]
                (try
                  ;; Set before invocation because git add can partially stage
                  ;; before returning a failure.
                  (reset! staging-started? true)
                  (apply git! repo-root (concat ["add" "--"] paths))
                  (if-not (= planned-paths (set (cached-paths repo-root)))
                    (do
                      (git! repo-root "reset" "--")
                      (reset! staging-started? false)
                      (result plan :held :stale-plan results))
                    (do
                      (git! repo-root "commit" "-m" message)
                      (reset! staging-started? false)
                      (assoc (result plan :committed nil results)
                             :commit/sha (str/trim (:out (git! repo-root "rev-parse" "HEAD"))))))
                  (catch Exception error
                    (when @staging-started?
                      (try
                        (git! repo-root "reset" "--")
                        (catch Exception cleanup-error
                          (throw (ex-info "Promotion failed and index cleanup failed"
                                          {:error/type :inbox-zero/cleanup-failed
                                           :original error
                                           :cleanup cleanup-error}
                                          cleanup-error)))))
                    (throw error)))))))))))
