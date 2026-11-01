(ns futon3.flexiarg-projection-d11-test
  "D-11 acceptance checks (mission M-pattern-retrieval-calibration,
  acceptance line 1026): futon3a's `futon.flexiarg.projection` is the
  single shared parser and must pass four named checks against the REAL
  parser and the REAL corpus -- heterogeneous-root parsing, the anchored
  metadata regression, visible failure on malformed patterns, and a
  deterministic projection artifact.  These tests probe the claimed
  properties themselves (parsed packet content and serialized bytes),
  not proxies."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [flexiarg-projection]
            [scripts.build-pattern-index]
            [scripts.pattern-sync]
            [futon.flexiarg.projection :as projection]))

(def futon3-root
  (.getCanonicalPath (io/file ".")))

(def heterogeneous-roots
  "One real .flexiarg from each D-11.1 heterogeneous source: four library
  sections plus at least one .flexiarg under holes/."
  ["library/futon-theory/agent-contract.flexiarg"
   "library/agent/budget-bounds-exploration.flexiarg"
   "library/realtime/authoritative-transcript.flexiarg"
   "library/aif/admissibility.flexiarg"
   "holes/futon-stack.flexiarg"])

(def canonical-packet-keys
  [:pattern/id :pattern/title :pattern/source-path :pattern/sigils
   :pattern/keywords :pattern/references :pattern/conclusion
   :pattern/clauses :pattern/projection-version])

(defn- ok-packets [path]
  (->> (projection/parse-file (io/file futon3-root path)
                              {:futon3-root futon3-root :report? false})
       (filter #(= :ok (:pattern/status %)))
       vec))

(deftest heterogeneous-root-check
  (testing "every heterogeneous source parses to a full canonical packet"
    (doseq [path heterogeneous-roots]
      (testing path
        (let [packets (ok-packets path)]
          (is (seq packets) "no ok packets parsed from a real source file")
          (doseq [packet packets]
            (is (= "flexiarg-v0" (:pattern/projection-version packet)))
            (doseq [key canonical-packet-keys]
              (is (contains? packet key)
                  (str "canonical packet missing " key)))
            (is (string? (:pattern/id packet)))
            (is (not (str/blank? (:pattern/id packet))))
            (is (string? (:pattern/title packet)))
            (is (seq (:pattern/clauses packet))
                "no clauses parsed")
            (doseq [clause (:pattern/clauses packet)
                    :when (contains? clause :name-key)]
              (is (contains? clause :name))
              (is (contains? clause :slug))
              (is (contains? clause :text)))))))))

(def anchored-regression-block
  "A block whose body prose contains `@title` and `@keywords` inside a
  clause.  The looser un-anchored regex the futon3 parsers used before
  D-9.4 would capture these as metadata; the anchored regex must not."
  "@flexiarg memory/probe-anchored-metadata
@title Real Title

! conclusion: the parser wins.

  + context: the body mentions @title Inline Spoiler and @keywords
    [fake directive] inside prose, which must not become metadata.")

(deftest anchored-metadata-check
  (testing "directives inside body prose are not captured as metadata"
    (is (nil? (projection/extract-meta "  prose line @title Inline Spoiler"
                                       "title"))
        "unanchored occurrence matched"))
  (let [packet (projection/parse-block
                (io/file futon3-root "library/agent/probe-anchored-metadata.flexiarg")
                anchored-regression-block
                {:futon3-root futon3-root :report? false})]
    (is (= :ok (:pattern/status packet)))
    (is (= "Real Title" (:pattern/title packet))
        "title was overridden by prose occurrence")
    (is (not (str/includes? (str (:pattern/title packet)) "Inline Spoiler")))))

(def malformed-block
  "A block with no @arg/@flexiarg/@multiarg header at all."
  "! conclusion: no header here.

  + context: nothing identifies this as a pattern block.")

(deftest malformed-pattern-check
  (testing "a headerless block fails visibly instead of disappearing"
    (let [packet (projection/parse-block
                  (io/file futon3-root "library/agent/probe-malformed.flexiarg")
                  malformed-block
                  {:futon3-root futon3-root :report? false})]
      (is (= :error (:pattern/status packet)))
      (is (= "missing" (:pattern/projection-version packet)))
      (is (:pattern/error packet)
          "error packet carries no :pattern/error detail")))
  (testing "an unreadable path yields a visible error packet, not silence"
    (let [packets (projection/parse-file
                   (io/file futon3-root "library/agent/definitely-absent.flexiarg")
                   {:futon3-root futon3-root :report? false})]
      (is (= 1 (count packets)))
      (is (= "missing" (:pattern/projection-version (first packets))))
      (is (= :error (:pattern/status (first packets)))))))

(def deterministic-roots
  "Real corpus subset spanning all four heterogeneous sections (full-tree
  determinism follows from the same code path; this subset keeps the
  check fast enough for a unit-test budget)."
  ["library/futon-theory" "library/agent" "library/realtime" "library/aif"])

(deftest deterministic-artifact-check
  (testing "two projections of the unchanged tree serialize to identical bytes"
    (let [run (fn [] (pr-str (projection/parse-roots
                              {:futon3-root futon3-root
                               :source-roots deterministic-roots})))
          first-bytes  (run)
          second-bytes (run)]
      (is (= first-bytes second-bytes)
          "projection of the unchanged tree is not byte-identical")
      (is (pos? (count first-bytes))))))

(def delegating-scripts
  "D-11.3: the three futon3 scripts must delegate to the shared parser
  and retain no parallel block/metadata parsers of their own."
  {"scripts/build_pattern_index.clj" 'scripts.build-pattern-index
   "scripts/pattern_sync.clj" 'scripts.pattern-sync
   "scripts/flexiarg_projection.clj" 'flexiarg-projection})

(def parallel-parser-defs
  #"defn-?\s+(split-arg-blocks|parse-components|extract-meta)\b")

(deftest scripts-delegate-check
  (testing "each script requires the shared parser and defines no parallel parsers"
    (doseq [[path ns-sym] delegating-scripts]
      (testing path
        (let [source (slurp (io/file futon3-root path))]
          (is (re-find #"\[\s*futon\.flexiarg\.projection\s*:\w+" source)
              "script does not require futon.flexiarg.projection")
          (is (nil? (re-find parallel-parser-defs source))
              "script retains a parallel parser definition")
          (is (some? (find-ns ns-sym))
              "script namespace not loaded on the classpath"))))))
