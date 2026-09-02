#!/usr/bin/env bb
(ns library-graph-lint-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [checks.library-graph-lint :as lint]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests testing]]))

(defn write-pattern! [root id directives body]
  (let [file (fs/path root (str id ".flexiarg"))]
    (fs/create-dirs (fs/parent file))
    (spit (str file) (str "@flexiarg " id "\n" directives "\n! conclusion: " body "\n"))))

(defn baseline! [root]
  (lint/snapshot (lint/scan-library root)))

(defn run-fixture
  ([setup] (run-fixture setup []))
  ([setup extra-args]
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
        (let [proc (apply process/shell
                          {:continue true :out :string :err :string}
                          "bb" "checks/library_graph_lint.clj"
                          "--library" (str root) "--section" "s"
                          "--baseline" (str baseline)
                          "--attestations" (str attestations)
                          "--evidence-records" (str evidence-records)
                          "--report" (str report-path)
                          extra-args)]
          (assoc (edn/read-string (slurp (str report-path)))
                 :test/exit (:exit proc))))
      (finally (fs/delete-tree root))))))

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

(def warrant-evidence
  {"e-warrant" {:evidence/id "e-warrant"
                :evidence/body "the runner calls s/b to carry out s/a"}})

(defn attestation
  "A rung-2 record for s/a -> s/b whose excerpt occurs in warrant-evidence."
  [extra]
  (merge {:edge {:from "s/a" :to "s/b" :kind :how}
          :by "zai-test" :at "2026-09-01"
          :read ["s/a" "s/b"] :cited "the runner calls s/b to carry out s/a"
          :evidence [{:id "e-warrant" :via :text :query "q"
                      :excerpt "the runner calls s/b to carry out s/a"}]
          :rung 2 :state :proposed}
         extra))

(defn plant-refusal
  "Plant one attestation in a two-pattern section. When keep-directive? the
  @how line stays in s/a, which is what a warrant-refusal leaves behind and an
  edge-refusal must not."
  [keep-directive? extra]
  (run-fixture
   (fn [root]
     (write-pattern! root "s/a" "" "a")
     (write-pattern! root "s/b" "" "b")
     (let [base (baseline! root)]
       (when keep-directive? (write-pattern! root "s/a" "@how s/b" "a"))
       (spit (str (fs/path root "evidence-records.edn")) (pr-str warrant-evidence))
       (spit (str (fs/path root "attestations.edn")) (pr-str [(attestation extra)]))
       {:base base}))))

(deftest edge-refusal-and-warrant-refusal-are-different-acts
  (testing "warrant-refusal: the edge stays :proposed and the directive line stays"
    (let [report (plant-refusal true {:warrant-refused
                                      {:by "claude-13" :at "2026-09-01"
                                       :note "a tag-list co-occurrence states no relation"}})]
      (is (zero? (:test/exit report)))
      (is (= 0 (get-in report [:summary :edge-refusals])))
      (is (= 1 (get-in report [:summary :warrant-refusals])))))
  (testing "edge-refusal: the directive line must leave the file"
    (let [refused {:state :refused :reason "the cited text does not mention the target"}]
      (is (failed-for? (plant-refusal true refused) :refused-edge-still-present))
      (let [report (plant-refusal false refused)]
        (is (zero? (:test/exit report)))
        (is (= 1 (get-in report [:summary :edge-refusals])))
        (is (= 0 (get-in report [:summary :warrant-refusals]))))))
  (testing "one record cannot perform both acts"
    (is (failed-for? (plant-refusal false
                                    {:state :refused
                                     :reason "the cited text does not mention the target"
                                     :warrant-refused {:by "claude-13" :at "2026-09-01"
                                                       :note "and the warrant is thin"}})
                     :warrant-refusal-requires-proposed-state)))
  (testing "a warrant-refusal without a note is not a refusal anyone can audit"
    (is (failed-for? (plant-refusal true {:warrant-refused {:by "claude-13" :at "2026-09-01"}})
                     :malformed-attestation))))

(deftest non-keyword-attestation-key-is-malformed
  ;; An unescaped quote inside :reason splits the string and leaves the tail as
  ;; a symbol key; library/writing-coherence/attestations.edn carried one and
  ;; every schema check passed over it.
  (is (failed-for? (run-fixture
                    (fn [root]
                      (write-pattern! root "s/a" "" "a")
                      (spit (str (fs/path root "attestations.edn"))
                            (str "[{:edge {:from \"s/a\" :to \"s/b\" :kind :how}"
                                 " :by \"zai-test\" :at \"2026-09-01\" :read [\"s/a\"]"
                                 " :cited \"c\" :evidence [{:id \"e\" :via :text :query \"q\""
                                 " :excerpt \"x\"}] :rung 2 :state :proposed"
                                 " :reason \"the turn uses \" meta-lede \" as a name\"}]"))
                      {:base (baseline! root)}))
                   :malformed-attestation)))

(deftest live-library-passes
  (let [report (lint/lint
                {:library "library" :section "aif"
                 :baseline "library/.spider/baseline-edges.edn"
                 :attestations "library/aif/attestations.edn"
                 :evidence-records evidence-records-path})]
    (println "pinned library graph evidence"
             (pr-str (select-keys (:summary report) [:files :edges-by-kind])))
    (is (true? (get-in report [:summary :pass?])))
    (is (= lint/counted-edge-kinds (set (keys (get-in report [:summary :edges-by-kind])))))
    ;; L9 slice 4 wrote the first post-hoc edges into the library: five in
    ;; war-room and two in aif, each carrying a :via :pattern-text warrant that
    ;; resolves against the pattern file it names. The count was zero from LD1
    ;; until 2026-09-02 and is pinned here rather than merely reported, so a
    ;; directive added or dropped without a ledger entry fails this test.
    (is (= 7 (get-in report [:summary :edges-by-kind lint/posthoc-why-kind])))
    ;; The authored measures did not move with them: a verified trace is still
    ;; a trace, which is the property the arm was adopted for. They DID move on
    ;; 2026-09-02 for a different reason -- worklist LA1 slice (b) authored six
    ;; snatch/ temperament patterns (have-a-temperament and the five it names by
    ;; @how), carrying 6 @why, 5 @how and 6 @see-also into the library: :why
    ;; 86 -> 92 and :patterns-in-why-graph 97 -> 104. Their edges take the
    ;; author-written exempt path in the baseline, not an attestation (decision
    ;; :authored-edge-attestation-path), which is why the section still passes.
    (is (= 92 (get-in report [:summary :edges-by-kind :why])))
    (is (= 104 (get-in report [:summary :patterns-in-why-graph])))
    (is (zero? (get-in report [:summary :unresolved-targets])))
    (is (zero? (get-in report [:summary :why-cycles])))))

(defn posthoc-fixture
  "s/a -> s/b spelled with the interim post-hoc directive, attested so the
  new-edge rule is satisfied and only the counting is under test."
  [root]
  (write-pattern! root "s/a" "" "source warrant sentence")
  (write-pattern! root "s/b" "" "target")
  (let [base (baseline! root)]
    (write-pattern! root "s/a" "@why-posthoc s/b" "source warrant sentence")
    (spit (str (fs/path root "evidence-records.edn")) (pr-str warrant-evidence))
    (spit (str (fs/path root "attestations.edn"))
          (pr-str [{:edge {:from "s/a" :to "s/b" :kind :why-posthoc}
                    :by "zai-test" :at "2026-09-01"
                    :read ["s/a" "s/b"] :cited "source warrant sentence"
                    :evidence [{:id "e-warrant" :via :text :query "q"
                                :excerpt "the runner calls s/b to carry out s/a"}]
                    :rung 2 :state :proposed}]))
    {:base base}))

(deftest posthoc-why-is-parsed-and-counted-apart-from-authored-why
  (let [report (run-fixture posthoc-fixture)
        summary (:summary report)
        section (:section summary)]
    (testing "the interim form is recognised, not dropped"
      (is (zero? (:test/exit report)) (pr-str (:checks report)))
      (is (= 1 (get-in summary [:edges-by-kind :why-posthoc]))))
    (testing "it is never summed into the authored @why"
      (is (zero? (get-in summary [:edges-by-kind :why])))
      (is (zero? (:patterns-in-why-graph summary)))
      (is (zero? (get-in section [:edges-by-kind :why])))
      (is (zero? (:patterns-with-outgoing-why section)))
      (is (= 1 (:patterns-with-outgoing-why-posthoc section)))
      ;; The measure the wave plan reads: a trace cannot raise it.
      (is (zero? (:fraction-organised section))))
    (testing "an authored @why on the same edge does raise it"
      (let [authored (:summary (run-fixture
                                (fn [root]
                                  (write-pattern! root "s/b" "" "target")
                                  (write-pattern! root "s/a" "@why s/b" "a")
                                  {:base (baseline! root)})))]
        (is (= 1 (get-in authored [:edges-by-kind :why])))
        (is (= 1 (:patterns-with-outgoing-why (:section authored))))
        (is (= 0.5 (:fraction-organised (:section authored))))))))

(deftest posthoc-why-earns-the-evidence-checks-but-not-the-authority-law
  (testing "a post-hoc edge must resolve to a real pattern"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "@why-posthoc absent/b" "a")
                        {:base (baseline! root)}))
                     :dangling-target)))
  (testing "a new post-hoc edge must earn an attestation like any other"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "" "a")
                        (write-pattern! root "s/b" "" "b")
                        (let [base (baseline! root)]
                          (write-pattern! root "s/a" "@why-posthoc s/b" "a")
                          {:base base})))
                     :new-edge-without-attestation)))
  (testing "acyclicity is about authority, so a post-hoc back-edge is not a cycle"
    ;; s/a --@why-posthoc--> s/b and s/b --@why--> s/a close a loop only if the
    ;; two kinds are merged. cycle-failures admits :why alone, so this passes;
    ;; the same shape with both edges spelled @why is the failing case below.
    (let [report (run-fixture
                  (fn [root]
                    (write-pattern! root "s/a" "@why-posthoc s/b" "a")
                    (write-pattern! root "s/b" "@why s/a" "b")
                    {:base (baseline! root)}))]
      (is (zero? (get-in report [:summary :why-cycles])))
      (is (= 1 (get-in report [:summary :edges-by-kind :why-posthoc])))
      (is (= 1 (get-in report [:summary :edges-by-kind :why])))
      (is (zero? (:test/exit report)) (pr-str (:checks report)))))
  (testing "an authored cycle still fails"
    (is (failed-for? (run-fixture
                      (fn [root]
                        (write-pattern! root "s/a" "@why s/b" "a")
                        (write-pattern! root "s/b" "@why s/a" "b")
                        {:base (baseline! root)}))
                     :why-cycle))))

(def authored-warrant-sentence
  "The split was forced by discovering the calibration was self-certifying.")

(defn write-warranted-pair!
  "s/a and s/b with the warrant sentence at a line this test knows by
  construction. Line 1 is the header, line 2 the directive slot (blank in the
  baseline, the directive afterwards, so the body digest is the same either
  way), line 3 the conclusion, line 4 the warrant, line 5 a decoy that must not
  satisfy a pointer at line 4."
  [root directive]
  (doseq [id ["s/a" "s/b"]]
    (let [file (fs/path root (str id ".flexiarg"))]
      (fs/create-dirs (fs/parent file))
      (spit (str file)
            (str "@flexiarg " id "\n"
                 (if (= id "s/a") directive "") "\n"
                 "! conclusion: " id "\n"
                 (if (= id "s/a") authored-warrant-sentence "target prose") "\n"
                 "A decoy line that repeats no warrant.\n")))))

(defn authored-warrant-fixture
  "The shape L9's readings are actually in: the warrant is a sentence in the
  source pattern, pointed at by file and line, with no store record anywhere."
  ([] (authored-warrant-fixture {}))
  ([{:keys [edge-kind evidence]
     :or {edge-kind :why-posthoc}}]
   (fn [root]
     (write-warranted-pair! root "")
     (let [base (baseline! root)]
       (write-warranted-pair! root (str "@" (name edge-kind) " s/b"))
       (spit (str (fs/path root "attestations.edn"))
             (pr-str [{:edge {:from "s/a" :to "s/b" :kind edge-kind}
                       :by "claude-owner" :at "2026-09-02"
                       :read ["s/a" "s/b"] :cited authored-warrant-sentence
                       :evidence [(or evidence
                                      {:via :pattern-text :file "s/a.flexiarg" :line 4
                                       :query "warrant sentence in the source pattern"
                                       :excerpt authored-warrant-sentence})]
                       :rung 2 :state :proposed}]))
       {:base base}))))

(def pattern-text-arm ["--posthoc-warrant" "pattern-text"])
(def store-arm ["--posthoc-warrant" "store"])

(deftest an-authored-warrant-is-refused-by-the-store-bar-and-verified-under-its-own-arm
  ;; decisions.edn :posthoc-warrant-evidence, :interim-adopted 2026-09-02 on the
  ;; numbers slice 3's run produced. :pattern-text is now the default; :store is
  ;; still reachable by flag, which is how a section number recorded before that
  ;; date is re-checked against the bar it was taken under.
  (testing "arm :store refuses it twice over -- the probe recorded in library/.spider/posthoc-warrant-blocker-2026-09-02.edn"
    (let [report (run-fixture (authored-warrant-fixture) store-arm)]
      (is (pos? (:test/exit report)))
      (is (contains? (reasons report) :malformed-attestation))
      (is (contains? (reasons report) :new-edge-without-attestation))))
  (testing "the default arm is :pattern-text -- the same fixture with NO flag"
    ;; This is what the section gate checks/spider_runner.clj:93 runs, which
    ;; passes no flag: before the flip a committed @why-posthoc with an authored
    ;; warrant failed there, and that is the whole content of adoption.
    (let [report (run-fixture (authored-warrant-fixture))]
      (is (zero? (:test/exit report)) (pr-str (:checks report)))
      (is (= [] (:checks report)))))
  (testing "arm :pattern-text accepts the same record, verified against the library"
    (let [report (run-fixture (authored-warrant-fixture) pattern-text-arm)]
      (is (zero? (:test/exit report)) (pr-str (:checks report)))
      (is (= [] (:checks report)))
      (is (= 1 (get-in report [:summary :edges-by-kind :why-posthoc])))
      ;; A verified trace is still a trace: it does not raise the organised
      ;; fraction, so the arm cannot move the wave measure.
      (is (zero? (get-in report [:summary :section :fraction-organised])))))
  (testing "the pointer is checked, not just the quotation"
    (doseq [[label evidence]
            [["the excerpt is real but sits on another line"
              {:via :pattern-text :file "s/a.flexiarg" :line 5 :query "q"
               :excerpt authored-warrant-sentence}]
             ["the excerpt is nowhere in the file"
              {:via :pattern-text :file "s/a.flexiarg" :line 4 :query "q"
               :excerpt "a sentence no pattern file contains"}]
             ["the file does not exist"
              {:via :pattern-text :file "s/absent.flexiarg" :line 4 :query "q"
               :excerpt authored-warrant-sentence}]
             ["the window runs past the end of the file"
              {:via :pattern-text :file "s/a.flexiarg" :line 99 :query "q"
               :excerpt authored-warrant-sentence}]]]
      (is (failed-for? (run-fixture (authored-warrant-fixture {:evidence evidence})
                                    pattern-text-arm)
                       :pattern-text-excerpt-mismatch)
          label)))
  (testing "the arm widens nothing outside the post-hoc lane (P-validated-R5 law O2)"
    (is (failed-for? (run-fixture (authored-warrant-fixture {:edge-kind :why})
                                  pattern-text-arm)
                     :malformed-attestation)
        "an authored @why may not carry a pattern-text warrant even on this arm")
    (is (failed-for? (run-fixture
                      (authored-warrant-fixture
                       {:evidence {:id "e-dressed-up" :via :pattern-text
                                   :file "s/a.flexiarg" :line 4 :query "q"
                                   :excerpt authored-warrant-sentence}})
                      pattern-text-arm)
                     :malformed-attestation)
        "a store id on a pattern-text warrant is refused: the two corpora do not mix"))
  (testing "the store bar is unchanged on the other arm"
    ;; Same fixture, store evidence, run WITH the flag: a resolving excerpt
    ;; passes and a non-resolving one fails exactly as before, so no recorded
    ;; number from a :store run is comparable-only-by-luck to one taken here.
    (let [store-ev {:id "e-warrant" :via :text :query "q"
                    :excerpt "the runner calls s/b to carry out s/a"}
          with-records (fn [ev]
                         (fn [root]
                           (let [f (authored-warrant-fixture {:evidence ev})
                                 out (f root)]
                             (spit (str (fs/path root "evidence-records.edn"))
                                   (pr-str warrant-evidence))
                             out)))]
      (is (zero? (:test/exit (run-fixture (with-records store-ev) pattern-text-arm))))
      (is (failed-for? (run-fixture
                        (with-records (assoc store-ev :excerpt "text no record holds"))
                        pattern-text-arm)
                       :evidence-excerpt-mismatch))))
  (testing "an unrecognised arm fails loudly instead of running the default"
    (is (failed-for? (run-fixture (authored-warrant-fixture)
                                  ["--posthoc-warrant" "nonsense"])
                     :linter-error))
    (is (= :pattern-text (lint/warrant-arm {})))
    (is (= :store (lint/warrant-arm {:posthoc-warrant "store"})))
    (is (= :pattern-text (lint/warrant-arm {:posthoc-warrant "pattern-text"})))))

(deftest posthoc-why-is-invisible-to-consumers-that-have-not-opted-in
  (testing "the cascade @why graph reader (checks/playout_snatch.clj:237) reads zero"
    (let [parse-why-line #(second (re-matches #"@why (.*)" %))]
      (is (nil? (parse-why-line "@why-posthoc war-room/wr-3")))
      (is (= "war-room/wr-3" (parse-why-line "@why war-room/wr-3")))))
  (testing "the interim form is on the ingest whitelist, so it is not an anomaly"
    (let [directives (:directives (edn/read-string (slurp "flexiarg-directives.edn")))]
      (is (= :standard (get-in directives [:why-posthoc :status])))
      (is (= :standard (get-in directives [:why :status]))))))

(deftest pinned-evidence-records-have-content-pin-and-falsifier
  (let [serialized (slurp evidence-records-path)
        records (edn/read-string serialized)
        pin (edn/read-string (slurp evidence-pin-path))]
    (is (= 56 (:record-count pin) (count records)))
    (is (= (:content-sha256 pin) (lint/sha256 serialized)))
    (is (not= (:content-sha256 pin) (lint/sha256 (str serialized "\nmutation")))
        "an incoherent/mutated fixture must not satisfy its content pin")))

(def v3-audit-path "library/aif/attestations-v3-reflection-audit.edn")
(def v3-fixtures-path "test/fixtures/library-graph/reflection-v3-fixtures.edn")

(defn reflection-v2?
  "Rule v2 verbatim: worker-seat author, Agency job envelope naming a worker or
  the spider agent, or spider self-text. v3 keeps all three and adds the
  authoring turn, so v2 is what remains when that clause is removed."
  [record worker-seats spider-agent]
  (let [author (str (:evidence/author record))]
    (boolean (or (contains? worker-seats author)
                 (and (lint/agency-job-envelope? record)
                      (or (lint/worker-seat-mentioned? record worker-seats)
                          (lint/worker-seat-mentioned? record #{spider-agent})))
                 (lint/spider-self-text? record)))))

(defn pattern-text-only?
  "A row whose every evidence entry is an authored warrant, so it names no store
  record for a reflection rule to have an opinion about."
  [att]
  (and (seq (:evidence att))
       (every? #(= :pattern-text (:via %)) (:evidence att))))

(deftest wave-1-attestations-re-audited-under-reflection-rule-v3
  (let [audit (edn/read-string (slurp v3-audit-path))
        workers (set (get-in audit [:reflection-rule :worker-seats]))
        spider (get-in audit [:reflection-rule :spider-agent])
        records (edn/read-string (slurp evidence-records-path))
        attestations (edn/read-string (slurp "library/aif/attestations.edn"))
        ;; The audit (2026-09-01) measured which CITED RECORDS the reflection
        ;; rule catches, so its scope is the rows that cite the store. L9 slice 4
        ;; added two :via :pattern-text rows, which name no record and were never
        ;; in that scope; counting them here would fail the pin for something the
        ;; audit has nothing to say about. The remainder is asserted below to be
        ;; pattern-text only, so a store-citing row cannot slip out of the count.
        store-cited (remove pattern-text-only? attestations)
        authored-warrant (filter pattern-text-only? attestations)
        v2 (set (keep (fn [[id record]]
                        (when (reflection-v2? record workers spider) id)) records))
        v3 (set (keep (fn [[id record]]
                        (when (lint/reflection-record? record workers spider) id)) records))
        authoring (set (keep (fn [[id record]]
                               (when (lint/authoring-turn? record) id)) records))
        cited (fn [att] (map :id (:evidence att)))
        changed (filter (fn [att]
                          (and (every? v3 (cited att))
                               (not (every? v2 (cited att)))))
                        attestations)]
    (is (= (:records-audited audit) (count records)))
    (is (= (:attestations-audited audit) (count store-cited)))
    (is (= (count attestations) (+ (count store-cited) (count authored-warrant))))
    (is (= 2 (count authored-warrant)))
    (is (every? #(= :why-posthoc (get-in % [:edge :kind])) authored-warrant)
        "an authored warrant is admissible on no other edge kind")
    (is (= (:reflection-under-v2 audit) (count v2)))
    (is (= (:reflection-under-v3 audit) (count v3)))
    (is (= (:authoring-turns-under-v3 audit) (count authoring)))
    (is (= (set (:newly-reflection-under-v3 audit))
           (set (remove v2 authoring))))
    (is (= (:changed-verdicts audit) (count changed)))
    ;; Falsifier: the audit is only worth reading if the same computation over a
    ;; record that IS an authoring turn and is not a fleet record would move it.
    (let [fixtures (edn/read-string (slurp v3-fixtures-path))
          rejected (:authoring-turn fixtures)]
      (is (true? (lint/authoring-turn? rejected)))
      (is (false? (reflection-v2? rejected workers spider)))
      ;; And the audit counts the author's own authoring only: a record about
      ;; somebody else's is not an authoring turn, so it cannot inflate the
      ;; count or move a verdict.
      (is (false? (lint/authoring-turn? (:third-party-authoring fixtures)))))))

(defn cache-header
  "The header shape ensure-live-evidence-index! writes, with an empty index --
  only the three keys read-index-cache decides on are under test here."
  [rule]
  {:schema 3 :store "http://127.0.0.1:7073"
   :basis {:count 191738 :max-at "2026-08-30T19:35:18.668154294Z"}
   :reflection-rule rule :index {}})

(deftest a-cache-name-that-contradicts-its-recorded-rule-is-refused
  ;; L8. Every cache built between 299f47b and 2026-09-02 is named -v3- and
  ;; records :reflection-rule {:version 2}, because the tag was the literal 3
  ;; and rule-version reached the name only inside the sha. A reader choosing a
  ;; pin reads the name.
  (let [dir (fs/create-temp-dir {:prefix "evidence-cache-tag-"})
        basis {:count 191738 :max-at "2026-08-30T19:35:18.668154294Z"}
        seats #{"zai-1" "zai-2"}
        v2-name (str (fs/file-name (lint/evidence-cache-path basis seats "codex-20" 2)))
        v3-name (str (fs/file-name (lint/evidence-cache-path basis seats "codex-20" 3)))
        rule-2 {:version 2 :worker-seats ["zai-1" "zai-2"]}
        write! (fn [name value]
                 (let [path (str (fs/path dir name))]
                   (spit path (pr-str value))
                   path))]
    (try
      (testing "the tag is computed from the rule version, not a literal"
        (is (str/includes? v2-name "-v2-"))
        (is (str/includes? v3-name "-v3-"))
        (is (= 2 (lint/cache-path-rule-version v2-name)))
        (is (= 3 (lint/cache-path-rule-version v3-name))))
      (testing "the defect shape -- v3 in the name, version 2 inside -- is refused"
        (let [path (write! v3-name (cache-header rule-2))]
          (is (= {:reason :cache-version-tag-mismatch :path path
                  :filename-tag 3 :content-version 2}
                 (lint/cache-version-failure path (edn/read-string (slurp path)))))
          (is (thrown-with-msg? Exception #"evidence cache refused"
                                (lint/read-index-cache path)))))
      (testing "control: the same content under the name the fix computes loads"
        (let [path (write! v2-name (cache-header rule-2))]
          (is (nil? (lint/cache-version-failure path (edn/read-string (slurp path)))))
          (is (= {} (:index (lint/read-index-cache path))))))
      (testing "a cache that records no rule version is refused, not guessed at"
        (let [path (write! v3-name (cache-header {:worker-seats []}))]
          (is (= :cache-declares-no-rule-version
                 (:reason (lint/cache-version-failure path (edn/read-string (slurp path))))))
          (is (thrown-with-msg? Exception #"evidence cache refused"
                                (lint/read-index-cache path)))))
      (testing "an older cache format is refused before any version is read"
        ;; A pre-cad5034 cache is a bare index map: no :schema, no header at all.
        (let [path (write! v3-name {"s/a" []})]
          (is (= :cache-schema-mismatch
                 (:reason (lint/cache-version-failure path (edn/read-string (slurp path))))))))
      (testing "a path this namespace did not name states no version to contradict"
        (let [path (write! "operator-chosen-pin.edn" (cache-header rule-2))]
          (is (nil? (lint/cache-path-rule-version path)))
          (is (nil? (lint/cache-version-failure path (edn/read-string (slurp path)))))))
      (finally (fs/delete-tree dir)))))

(deftest the-corpus-pins-in-the-library-resolve-to-a-cache-name-that-matches
  ;; The rename record is only worth keeping if the names in it are the names
  ;; the fixed function computes from the pins that are actually in the library.
  (let [record (edn/read-string (slurp "library/.spider/evidence-cache-rename-2026-09-02.edn"))
        pins (for [file (fs/glob "library" "*/attestations.edn")
                   att (edn/read-string (slurp (str file)))
                   :let [corpus (:corpus att)]
                   :when corpus]
               corpus)]
    (is (seq pins))
    (doseq [{:keys [basis reflection-rule]} pins]
      (let [version (:version reflection-rule)
            path (lint/evidence-cache-path basis (set (:worker-seats reflection-rule))
                                           (:spider-agent reflection-rule) version)]
        (is (some? version)
            (str "a corpus pin must name the rule it was built under: " (pr-str basis)))
        (is (= version (lint/cache-path-rule-version path))
            (str "pin " (pr-str basis) " resolves to " path))))
    (doseq [{:keys [to content-rule-version]} (:renamed record)]
      (is (= content-rule-version (lint/cache-path-rule-version to))))
    (doseq [{:keys [from]} (:renamed record)]
      (is (= 3 (lint/cache-path-rule-version from))
          "every renamed file was named v3 before the fix"))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
