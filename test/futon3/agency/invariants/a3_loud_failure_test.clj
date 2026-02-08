(ns futon3.agency.invariants.a3-loud-failure-test
  "Invariant A3 (loud failure): errors surface at the layer that caused them; no silent catch-and-swallow.

  Spec:
  - library/agency/loud-failure.flexiarg
  - library/agency/invariants.flexiarg (A3-hierarchy, A4-debugging derived)

  Note on A4 (debugging):
  A4 is treated as derived from A3 in the spec; these enforcement tests are the
  mechanical precondition for diagnosable failure traces."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3.agency.invariants.util :as u]))

(def ^:private agency-src-dir "src/futon3/agency")

(deftest no-silent-catch-nil
  (testing "no (catch Exception _ nil) or (catch Throwable _ nil) in Agency code"
    ;; EXPECTED FAIL (A3): current code contains silent catch-and-return-nil sites.
    (let [files (u/clj-files-under agency-src-dir)
          violations (concat
                      (u/find-re-matches files #"\(catch\s+Exception\s+_\s+nil\)")
                      (u/find-re-matches files #"\(catch\s+Throwable\s+_\s+nil\)"))]
      (is (empty? violations)
          (str "silent catch sites:\n"
               (str/join
                "\n"
                (map (fn [{:keys [file line text]}]
                       (format "%s:%d: %s" file line (str/trim text)))
                     violations)))))))

(deftest throw-exceptions-false-must-check-status
  (testing "every :throw-exceptions false must have an explicit status check nearby"
    ;; EXPECTED FAIL (A3): current code uses :throw-exceptions false without checking :status.
    (let [paths [(io/file "src/futon3/agency/http.clj")
                 (io/file "src/futon3/agency/service.clj")]
          occurrences
          (for [p paths
                :let [lines (vec (str/split-lines (slurp p)))]
                i (range (count lines))
                :when (re-find #":throw-exceptions\s+false" (nth lines i))]
            {:file (.getPath p)
             :line (inc i)
             :context (subvec lines i (min (+ i 25) (count lines)))})
          missing-check
          (filter (fn [{:keys [context]}]
                    (not (some #(re-find #"\(:status|:status" %) context)))
                  occurrences)]
      (is (empty? missing-check)
          (str "missing status checks near :throw-exceptions false:\n"
               (str/join
                "\n"
                (map (fn [{:keys [file line]}]
                       (format "%s:%d" file line))
                     missing-check)))))))

