(ns futon3.inbox-zero.promote-push
  "Push-decision boundary for a successful inbox-zero promotion commit.

  Ordinary accumulations are pushed with plain `git push remote branch`;
  unusual ahead counts and every failure become escalation data. This function
  never fetches, pulls, rebases, forces, or otherwise rewrites history. Ahead
  count is deliberately measured against the local upstream ref: if that ref
  is stale it can undercount divergence, and the subsequent push honestly
  surfaces the non-fast-forward as :push-failed. Fetch cadence belongs to
  futon-sync, not a per-turn boundary."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.util Date]))

(def default-ahead-threshold 10)
(def output-limit 4096)

(defn- bounded [output]
  (let [text (str output)]
    (subs text 0 (min output-limit (count text)))))

(defn- git-result [repo-root & args]
  (apply shell/sh (concat ["git"] args [:dir repo-root])))

(defn- successful-output [repo-root & args]
  (let [{:keys [exit out]} (apply git-result repo-root args)]
    (when (zero? exit) (str/trim out))))

(defn- current-branch [repo-root]
  (successful-output repo-root "symbolic-ref" "--quiet" "--short" "HEAD"))

(defn- configured-upstream [repo-root branch]
  (successful-output repo-root "rev-parse" "--abbrev-ref" "--symbolic-full-name"
                     (str branch "@{upstream}")))

(defn- remote-branch-ref [repo-root remote branch]
  (let [candidate (str remote "/" branch)]
    (when (successful-output repo-root "rev-parse" "--verify" "--quiet"
                             (str "refs/remotes/" candidate))
      candidate)))

(defn- parse-count [text]
  (when (and text (re-matches #"[0-9]+" text))
    (Long/parseLong text)))

(defn- result [repo-root threshold verdict reason ahead-count output]
  {:record/type :inbox-zero/push-result
   :repo-root repo-root
   :verdict verdict
   :escalate/reason reason
   :ahead-count ahead-count
   :ahead-threshold threshold
   :output (bounded output)
   :pushed-at (Date.)})

(defn push-promoted!
  "Push an ordinary promoted HEAD, or return explicit escalation data.

  The branch is explicit or the current symbolic branch. Its configured local
  upstream is the comparison basis when present; otherwise an existing local
  REMOTE/branch tracking ref is used. Missing local basis is :no-upstream.
  Counts strictly greater than AHEAD-THRESHOLD are outliers and are never
  pushed: an unusual accumulation can signal an offline stretch or divergence
  and must be shown to a human rather than quietly tidied."
  [{:keys [repo-root remote branch ahead-threshold]
    :or {remote "origin" ahead-threshold default-ahead-threshold}}]
  (when-not (and (integer? ahead-threshold) (not (neg? ahead-threshold)))
    (throw (ex-info "Ahead threshold must be a non-negative integer"
                    {:error/type :inbox-zero/invalid-ahead-threshold
                     :ahead-threshold ahead-threshold})))
  (let [branch (or branch (current-branch repo-root))
        upstream (when branch
                   (or (configured-upstream repo-root branch)
                       (remote-branch-ref repo-root remote branch)))]
    (if-not (and branch upstream)
      (result repo-root ahead-threshold :escalate :no-upstream nil "")
      (let [ahead-count (parse-count
                         (successful-output repo-root "rev-list" "--count"
                                            (str upstream "..HEAD")))]
        (if-not (integer? ahead-count)
          (result repo-root ahead-threshold :escalate :no-upstream nil "")
          (if (> ahead-count ahead-threshold)
            (result repo-root ahead-threshold :escalate :ahead-outlier
                    ahead-count "")
            (let [{:keys [exit out err]} (git-result repo-root "push" remote branch)]
              (if (zero? exit)
                (result repo-root ahead-threshold :pushed nil ahead-count out)
                (result repo-root ahead-threshold :escalate :push-failed
                        ahead-count err)))))))))
