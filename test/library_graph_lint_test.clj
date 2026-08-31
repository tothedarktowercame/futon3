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

(def snapshot-path "test/fixtures/library-graph/snapshot.edn")
(def evidence-records-path "test/fixtures/library-graph/evidence-records.edn")
(def evidence-pin-path "test/fixtures/library-graph/evidence-records.pin.edn")

(defn valid-snapshot? [snapshot]
  (let [census (:census snapshot)
        kinds (get census :edges-by-kind)]
    (and (= :library-graph/snapshot-2026-08-31 (:snapshot/id snapshot))
         (= "2026-08-31" (:recorded-at snapshot))
         (pos-int? (:files census))
         (= lint/edge-kinds (set (keys kinds)))
         (every? nat-int? (vals kinds))
         (= (:edges census) (reduce + (vals kinds)))
         (= 64 (count (get-in snapshot [:basis :content-sha256] "")))
         (string? (get-in snapshot [:basis :git-sha])))))

(deftest committed-library-graph-snapshot-owns-exact-census-and-pin
  (let [snapshot (edn/read-string (slurp snapshot-path))]
    (is (valid-snapshot? snapshot))
    (is (= {:files 1244 :edges 247
            :edges-by-kind {:why 86 :how 27 :see-also 134}}
           (:census snapshot)))
    ;; Snapshot falsifier: an exact total inconsistent with its typed partition
    ;; must be rejected without consulting the live library.
    (is (false? (valid-snapshot? (update-in snapshot [:census :edges] inc))))))

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
                 :attestations "library/aif/attestations.edn"
                 :evidence-records evidence-records-path})]
    (println "pinned library graph evidence"
             (pr-str (select-keys (:summary report) [:files :edges-by-kind])))
    (is (true? (get-in report [:summary :pass?])))
    (is (= lint/edge-kinds (set (keys (get-in report [:summary :edges-by-kind])))))
    (is (zero? (get-in report [:summary :unresolved-targets])))
    (is (zero? (get-in report [:summary :why-cycles])))))

(deftest pinned-evidence-records-have-content-pin-and-falsifier
  (let [serialized (slurp evidence-records-path)
        records (edn/read-string serialized)
        pin (edn/read-string (slurp evidence-pin-path))]
    (is (= 56 (:record-count pin) (count records)))
    (is (= (:content-sha256 pin) (lint/sha256 serialized)))
    (is (not= (:content-sha256 pin) (lint/sha256 (str serialized "\nmutation")))
        "an incoherent/mutated fixture must not satisfy its content pin")))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
