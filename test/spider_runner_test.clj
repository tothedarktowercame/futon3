#!/usr/bin/env bb
(ns spider-runner-test
  (:require [checks.library-graph-lint :as lint]
            [checks.spider-runner :as runner]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(deftest cursor-paging-follows-an-empty-terminal-page
  (let [requests (atom [])
        pages [{:entries [{:evidence/id "e-1"}]
                :next-cursor {:at "2026-01-03T00:00:00Z" :id "e-1"}}
               {:entries [{:evidence/id "e-2"}]
                :next-cursor {:at "2026-01-02T00:00:00Z" :id "e-2"}}
               {:entries []}]
        fetch (fn [request]
                (let [n (count @requests)]
                  (swap! requests conj request)
                  (nth pages n)))
        result (lint/page-evidence fetch)]
    (is (= ["e-1" "e-2"] (mapv :evidence/id (:entries result))))
    (is (= 3 (count (:pages result))))
    (is (= [nil {:at "2026-01-03T00:00:00Z" :id "e-1"}
            {:at "2026-01-02T00:00:00Z" :id "e-2"}]
           (mapv :cursor @requests)))))

(deftest listing-only-rung-one-is-not-a-warrant
  (let [evidence [{:id "e-listing" :via :tag :query "exact" :excerpt "source and target"}]
        listing [{:id "e-listing" :via :tag :listing true
                  :self-text false :co-mention false :reflection false
                  :query "exact" :excerpt "source and target in ranking"}]
        stated [{:id "e-listing" :via :tag :listing false
                 :self-text false :co-mention false :reflection false
                 :query "exact" :excerpt "source and target in statement"}]
        reflected [{:id "e-listing" :via :tag :listing false :self-text true
                    :co-mention false :reflection true
                    :query "exact" :excerpt "source and target in spider output"}]]
    (is (false? (runner/rung-one-warrant? evidence listing)))
    (is (false? (runner/rung-one-warrant? evidence reflected)))
    (is (true? (runner/rung-one-warrant? evidence stated)))
    (is (true? (runner/context-retrieval-listing?
                {:evidence/body {:event "context-retrieval"}})))
    (is (true? (runner/context-retrieval-listing?
                {:evidence/body "{\"event\" \"context-retrieval\", \"results\" []}"})))))

(deftest reflection-is-provenance-based
  (let [workers #{"zai-1" "zai-2"}
        worker-turn {:evidence/author "zai-1"
                     :evidence/type :coordination
                     :evidence/claim-type :step
                     :evidence/body {:turn-id "zai-turn-test"
                                     :cost/source :zai
                                     :calls [{:tool "run_shell"
                                              :args "curl text-search?q=war-room/wr-5-war-machine-is-not-a-mission"}]}}
        paper-turn {:evidence/author "claude-13"
                    :evidence/type :coordination
                    :evidence/claim-type :step
                    :evidence/body {:event :turn-round
                                    :text "war-room/wr-9-bites-empirical-or-logical"}}
        paper-index (lint/add-record-occurrences
                     {} #{"war-room/wr-9-bites-empirical-or-logical"}
                     {"WR-9" "war-room/wr-9-bites-empirical-or-logical"}
                     workers "codex-20" (assoc paper-turn :evidence/id "e-paper"))
        paper-hit (first (get paper-index "war-room/wr-9-bites-empirical-or-logical"))]
    (is (true? (lint/reflection-record? worker-turn workers "codex-20")))
    (is (false? (lint/reflection-record? paper-turn workers "codex-20")))
    (is (true? (lint/clean-non-reflection-hit? paper-hit)))))

(def v3-fixtures
  (edn/read-string (slurp "test/fixtures/library-graph/reflection-v3-fixtures.edn")))

(def authoring-turn
  "The record wave 2 rejected a warrant on: the pattern's own author announcing
  the file and listing its references. See the fixture file's header."
  (:authoring-turn v3-fixtures))

(def external-description
  "The counter-fixture: a third party reporting a use, saying \"flexiarg\" and
  \"authored\" without announcing a pattern file."
  (:external-description v3-fixtures))

(def third-party-authoring
  "The attribution counter-fixture: an operator turn relaying what codex-15 and
  an invoke job wrote, with real .flexiarg paths beside the authoring verbs."
  (:third-party-authoring v3-fixtures))

(defn authoring-turn-by-proximity?
  "The predicate as first written for L2: an authoring verb within the window of
  a *.flexiarg path, with nothing said about who did the authoring. Kept here so
  the counter-fixture is testable as a case the rule used to get wrong."
  [record]
  (let [text (str/join " " (lint/string-leaves (:evidence/body record)))
        paths (lint/regex-spans lint/pattern-file-path text)
        verbs (lint/regex-spans lint/authoring-verb text)]
    (boolean
     (some (fn [[path-start path-end]]
             (some (fn [[verb-start verb-end]]
                     (or (and (<= verb-end path-start)
                              (<= (- path-start verb-end) lint/authoring-verb-window))
                         (and (<= path-end verb-start)
                              (<= (- verb-start path-end) lint/authoring-verb-window))))
                   verbs))
           paths))))

(deftest authoring-turn-is-reflection-under-v3-and-not-under-v2
  (let [workers #{"zai-1" "zai-2"}]
    ;; v2 was exactly these three clauses; all three are false on the record,
    ;; which is why the fleet handed it up as external corroboration.
    (is (false? (contains? workers (:evidence/author authoring-turn))))
    (is (not (lint/agency-job-envelope? authoring-turn)))
    (is (false? (lint/spider-self-text? authoring-turn)))
    (is (true? (lint/authoring-turn? authoring-turn)))
    (is (true? (lint/reflection-record? authoring-turn workers "codex-20")))
    ;; The counter-fixture must survive v3: a genuine external description.
    (is (false? (lint/authoring-turn? external-description)))
    (is (false? (lint/reflection-record? external-description workers "codex-20")))
    (is (= 3 lint/reflection-rule-version))
    (is (= :author-attributed-authoring-verb-within-window-of-a-flexiarg-path
           (:authoring-turn (lint/reflection-rule workers "codex-20"))))))

(deftest an-authoring-turn-is-the-record-authors-own-authoring
  (let [workers #{"zai-1" "zai-2"}]
    ;; Proximity alone calls this operator turn an authoring turn, which would
    ;; discard joe's report of what codex-15 built as the fleet's own record.
    (is (true? (authoring-turn-by-proximity? third-party-authoring)))
    (is (false? (lint/authoring-turn? third-party-authoring)))
    (is (false? (lint/reflection-record? third-party-authoring workers "codex-20")))
    ;; The wave-2 case is impersonal ("Pattern authored at <path>"), so the
    ;; attribution test must not cost it: both predicates still take it.
    (is (true? (authoring-turn-by-proximity? authoring-turn)))
    (is (true? (lint/authoring-turn? authoring-turn)))
    ;; A named third party in an active clause is a report, not an announcement.
    (is (false? (lint/authoring-turn?
                 {:evidence/author "claude-1"
                  :evidence/body {:text (str "Alice wrote library/aif/example.flexiarg; "
                                             "I independently reviewed how it uses "
                                             "the cited pattern.")}})))
    ;; First person and self-naming stay authoring turns.
    (is (true? (lint/authoring-turn?
                {:evidence/author "claude-1"
                 :evidence/body {:text (str "I wrote library/aif/example.flexiarg "
                                            "this afternoon and listed its references.")}})))
    (is (true? (lint/authoring-turn?
                {:evidence/author "codex-15"
                 :evidence/body {:text (str "codex-15 wrote library/aif/example.flexiarg "
                                            "with three references.")}})))
    (is (= :impersonal-first-person-or-subject-is-the-record-author
           (:authoring-attribution (lint/reflection-rule workers "codex-20"))))))

(deftest rung-two-warrant-refuses-an-authoring-turn
  (let [records {"e-authoring" authoring-turn "e-external" external-description}
        records (assoc records "e-third-party" third-party-authoring)
        fetch records]
    (reset! runner/worker-seats #{"zai-1" "zai-2"})
    (is (false? (runner/rung-two-warrant? [{:id "e-authoring" :via :text}] fetch)))
    ;; A record about somebody else's authoring warrants rung 2 on its own.
    (is (true? (runner/rung-two-warrant? [{:id "e-third-party" :via :text}] fetch)))
    (is (true? (runner/rung-two-warrant? [{:id "e-authoring" :via :text}
                                          {:id "e-external" :via :text}] fetch)))
    ;; An id the store does not return warrants nothing.
    (is (false? (runner/rung-two-warrant? [{:id "e-missing" :via :text}] fetch)))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
