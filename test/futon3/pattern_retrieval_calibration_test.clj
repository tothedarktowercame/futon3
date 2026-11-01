(ns futon3.pattern-retrieval-calibration-test
  "Enactment tests for the reviewed compile-phase completion checks of
  M-pattern-retrieval-calibration. Positive path runs against the REAL
  mission file on disk (no stub). Negative path constructs the exact bad
  case each check class guards against — a mission record missing the
  named observable — and shows the checker catches it."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [futon3.pattern-retrieval-calibration :as prc]))

(deftest real-mission-satisfies-every-reviewed-check
  (let [mission (prc/read-mission)
        results (prc/verify mission)
        report (prc/summary results)]
    (is (= 21 (:total report)) "all 21 reviewed holes are covered")
    (is (= [] (:unsatisfied report))
        (str "no check may fail against the real mission record: "
             (pr-str (:unsatisfied report))))
    (is (true? (:all-satisfied? report)))))

(deftest title-check-catches-wrong-title
  (let [bad (prc/read-mission (java.io.StringReader.
                               "# Mission: Something Else Entirely\n\n**Status:** OPEN — DERIVE\n**Owner:** Joe\n**Cross-refs:**\n"))
        result (get (prc/verify bad) ["problems/r12-two-layer-calibration" 0])]
    (is (false? (:satisfied? result)) "the title check must catch a renamed mission title")))

(deftest p1-block-check-catches-removed-dependency
  (let [good "Blocks on P-1 (`futon3a/src/futon/flexiarg/projection.clj`) shipping."
        bad "No dependency at all."
        run (fn [t] (:satisfied?
                     (get (prc/verify (prc/read-mission (java.io.StringReader. t)))
                          ["cascade-construction/read-what-exists-first" 13])))]
    (is (true? (run good)))
    (is (false? (run bad)) "the P-1 block check must catch its named bad case")))

(deftest cross-refs-ordering-catches-section-after-motivation
  (let [bad (prc/read-mission (java.io.StringReader.
                               "# Mission: X\n\n## Motivation\n\nbody\n\n**Cross-refs:**\n"))
        result (get (prc/verify bad) ["cascade-construction/read-what-exists-first" 3])]
    (is (false? (:satisfied? result))
        "a Cross-refs listing that appears after Motivation must not satisfy the ordering check")))

(deftest normalized-text-bridges-line-wrapping
  ;; The MiniLM-cosine sentence is wrapped across two lines in the real file;
  ;; normalization must let the predicate see it as one sentence.
  (let [wrapped "  futon3a MiniLM-cosine substrate is artifact-prone and unsafe to treat\n  as a clean sensor."
        m (prc/read-mission (java.io.StringReader. wrapped))]
    (is (str/includes? (:normalized m) "unsafe to treat as a clean sensor"))))
