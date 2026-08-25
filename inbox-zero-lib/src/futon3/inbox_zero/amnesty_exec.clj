(ns futon3.inbox-zero.amnesty-exec
  "Commit the baseline partition of one pre-witnessing amnesty plan.

  The executor refuses an occupied index, runs caller-supplied gates, stages
  only the plan's baseline paths, verifies the complete staged path set, and
  never pushes."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3.inbox-zero.gates :as gates])
  (:import [java.time Instant]
           [java.util Date]))

(def output-limit 4096)
(def index-path-limit 100)

(defn- bounded [value]
  (let [text (str value)]
    (subs text 0 (min output-limit (count text)))))

(defn- default-git-fn [repo-root & args]
  (apply shell/sh (concat ["git"] args [:dir repo-root])))

(defn- git-call [git-fn repo-root & args]
  (try
    (let [result (apply git-fn repo-root args)]
      (merge {:exit -1 :out "" :err "Git runner returned no result"}
             result))
    (catch Exception error
      {:exit -1 :out "" :err (.getMessage error) :exception error})))

(defn- nul-paths [output]
  (->> (str/split (str output) #"\u0000" -1)
       (remove str/blank?)
       vec))

(defn- result [plan now verdict reason gate-results & [extra]]
  (merge {:record/type :inbox-zero/amnesty-result
          :plan plan
          :verdict verdict
          :commit/sha nil
          :held/reason reason
          :gate-results gate-results
          :executed-at now}
         extra))

(defn- validate-input! [plan gates]
  (gates/validate-gates! gates)
  (let [paths (:baseline plan)]
    (when-not (and (vector? paths)
                   (every? #(and (string? %) (not (str/blank? %))) paths)
                   (= (count paths) (count (set paths))))
      (throw (ex-info "Amnesty baseline must contain distinct paths"
                      {:error/type :inbox-zero/invalid-plan :paths paths})))))

(defn- instant-text [value]
  (cond
    (instance? Date value) (str (.toInstant ^Date value))
    (instance? Instant value) (str value)
    :else (str value)))

(defn- commit-message [plan]
  (let [paths (:baseline plan)
        subject (format "inbox-zero: pre-witnessing baseline, %d path(s)"
                        (count paths))
        body (str (str/join "\n" paths)
                  "\n\nAmnesty-plan: " (instant-text (:computed-at plan)))]
    {:subject subject :body body}))

(defn- git-failure [plan now gate-results args git-result]
  (result plan now :held :git-failed gate-results
          {:git/command (into ["git"] args)
           :git/exit (:exit git-result)
           :git/output (bounded (str (:out git-result) (:err git-result)))}))

(defn execute-amnesty-plan!
  "Execute the baseline paths in one amnesty PLAN, without pushing.

  GIT-FN receives REPO-ROOT followed by ordinary Git arguments and returns a
  shell-style map containing :exit, :out, and :err. The initially empty index
  is restored on every failure after staging begins."
  [plan {:keys [repo-root git-fn gates now]
         :or {git-fn default-git-fn gates []}}]
  (validate-input! plan gates)
  (if (empty? (:baseline plan))
    (result plan now :nothing-to-commit nil [])
    (let [cached-result (git-call git-fn repo-root
                                  "diff" "--cached" "--name-only" "-z")]
      (if-not (zero? (:exit cached-result))
        (git-failure plan now []
                     ["diff" "--cached" "--name-only" "-z"] cached-result)
        (let [occupied (nul-paths (:out cached-result))]
          (if (seq occupied)
            (result plan now :held :index-not-empty []
                    {:index/paths (vec (take index-path-limit occupied))})
            (let [{:keys [passed? results]}
                  (gates/run-gates repo-root gates (:baseline plan))]
              (if-not passed?
                (result plan now :held :gate-failed results)
                (let [paths (:baseline plan)
                      planned (set paths)
                      add-args (into ["add" "--"] paths)
                      add-result (apply git-call git-fn repo-root add-args)]
                  (if-not (zero? (:exit add-result))
                    (do
                      (git-call git-fn repo-root "reset" "--")
                      (git-failure plan now results add-args add-result))
                    (let [verify-result (git-call git-fn repo-root
                                                  "diff" "--cached"
                                                  "--name-only" "-z")]
                      (cond
                        (not (zero? (:exit verify-result)))
                        (do
                          (git-call git-fn repo-root "reset" "--")
                          (git-failure plan now results
                                       ["diff" "--cached" "--name-only" "-z"]
                                       verify-result))

                        (not= planned (set (nul-paths (:out verify-result))))
                        (let [observed (nul-paths (:out verify-result))]
                          (git-call git-fn repo-root "reset" "--")
                          (result plan now :held :staged-set-mismatch results
                                  {:staged/paths observed}))

                        :else
                        (let [{:keys [subject body]} (commit-message plan)
                              commit-args ["commit" "-m" subject "-m" body]
                              commit-result (apply git-call git-fn repo-root
                                                   commit-args)]
                          (if-not (zero? (:exit commit-result))
                            (do
                              (git-call git-fn repo-root "reset" "--")
                              (git-failure plan now results commit-args
                                           commit-result))
                            (let [sha-result (git-call git-fn repo-root
                                                       "rev-parse" "HEAD")]
                              (if (zero? (:exit sha-result))
                                (assoc (result plan now :committed nil results)
                                       :commit/sha (str/trim (:out sha-result)))
                                (git-failure plan now results
                                             ["rev-parse" "HEAD"]
                                             sha-result)))))))))))))))))
