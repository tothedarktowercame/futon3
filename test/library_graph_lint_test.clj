#!/usr/bin/env bb
(ns library-graph-lint-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [checks.library-graph-lint :as lint]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is run-tests testing]]))

(defn write-pattern! [root id directives body]
  (let [file (fs/path root (str id ".flexiarg"))]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (str "@flexiarg " id "\n" directives "\n! conclusion: " body "\n"))))

(defn baseline! [root]
  (lint/snapshot (lint/scan-library root)))

(defn run-fixture [setup]
  (let [root (fs/create-temp-dir {:prefix "library-graph-lint-"})
        baseline (fs/path root "baseline.edn")
        attestations (fs/path root "attestations.edn")
        evidence-records (fs/path root "evidence-records.edn")
        report-path (fs/path root "report.edn")]
    (try
      (let [{:keys [base]} (setup root)]
        (spit (str baseline) (pr-str base))
        (when-not (fs/exists? attestations) (spit (str attestations) "[]"))
        (when-not (fs/exists? evidence-records) (spit (str evidence-records) "{}"))
        (let [proc (process/shell
                    {:continue true :out :string :err :string}
                    "bb" "checks/library_graph_lint.clj"
                    "--library" (str root) "--section" "s"
                    "--baseline" (str baseline)
                    "--attestations" (str attestations)
                    "--evidence-records" (str evidence-records)
                    "--report" (str report-path))]
          (assoc (edn/read-string (slurp (str report-path)))
                 :test/exit (:exit proc))))
      (finally (fs/delete-tree root)))))

(defn reasons [report] (set (map :reason (:checks report))))
(defn failed-for? [report reason]
  (and (pos? (:test/exit report)) (contains? (reasons report) reason)))

(deftest five-refusal-fixtures
  (testing "cycle"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "@why s/b" "a")
                        (write-pattern! root "s/b" "@why s/a" "b")
                        {:base (baseline! root)}))
                     :why-cycle)))
  (testing "dangling target"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "@why absent/b" "a")
                        {:base (baseline! root)}))
                     :dangling-target)))
  (testing "new edge without attestation"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "" "a")
                        (write-pattern! root "s/b" "" "b")
                        (let [base (baseline! root)]
                          (write-pattern! root "s/a" "@why s/b" "a")
                          {:base base})))
                     :new-edge-without-attestation)))
  (testing "argument body edit"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "" "before")
                        (let [base (baseline! root)]
                          (write-pattern! root "s/a" "" "after")
                          {:base base})))
                     :argument-body-changed)))
  (testing "malformed attestation"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "" "a")
                        (spit (str (fs/path root "attestations.edn"))
                              "[{:edge {:from \"s/a\"}}]")
                        {:base (baseline! root)}))
                     :malformed-attestation)))
  (testing "rung and evidence semantics"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "" "source warrant sentence")
                        (write-pattern! root "s/b" "" "target")
                        (let [base (baseline! root)]
                          (write-pattern! root "s/a" "@how s/b" "source warrant sentence")
                          (spit (str (fs/path root "evidence-records.edn"))
                                (pr-str {"e-test" {:evidence/id "e-test"
                                                   :evidence/body "an excerpt that really occurs"}}))
                          (spit (str (fs/path root "attestations.edn"))
                                (pr-str [{:edge {:from "s/a" :to "s/b" :kind :how}
                                          :by "zai-test" :at "2026-08-30"
                                          :read ["s/a" "s/b"] :cited "source warrant sentence"
                                          :evidence [{:id "e-test" :via :text :query "q"
                                                      :excerpt "an excerpt that really occurs"}]
                                          :rung 1 :state :proposed}]))
                          {:base base})))
                     :rung-via-mismatch))))

(deftest live-library-passes
  (let [report (lint/lint
                {:library "library" :section "aif"
                 :baseline "library/.spider/baseline-edges.edn"
                 :attestations "library/aif/attestations.edn"})]
    (is (true? (get-in report [:summary :pass?])))
    (is (= 1243 (get-in report [:summary :files])))
    (is (= {:why 86 :how 23 :see-also 125}
           (get-in report [:summary :edges-by-kind])))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
