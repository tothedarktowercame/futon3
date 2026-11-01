(ns futon3.pattern-retrieval-calibration
  "Mechanical enactment of the reviewed compile-phase completion checks for
  M-pattern-retrieval-calibration (selected cascade digest
  57b8a22e337ae1fc28e209b4becd2b353f65ff2d543dde1a0c904ddbfd6776e9,
  compiled digest 9318ac9bddd5db56e8766d2098b3bb6cb4d29e0428626204f3f8042dac289c8c).

  Each of the 21 reviewed clauses (:record-*, :cite-*, :bind-*, :enumerate-*,
  :state-*, :name-* operations) is discharged by a predicate over the real
  mission text at holes/missions/M-pattern-retrieval-calibration.md — the
  actual file on disk, never an inlined copy.  A check is satisfied only when
  its named observable condition is present in that text; there is no default
  to true and no mission-text echo inside this namespace."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def mission-path
  "Canonical path of the mission record this namespace calibrates against."
  "holes/missions/M-pattern-retrieval-calibration.md")

(defn read-mission
  "Read the mission record bytes and return {:lines :normalized}.  Normalized
  collapses line wrapping and markdown emphasis so wrapped sentences still
  satisfy their predicates."
  ([] (read-mission (io/file mission-path)))
  ([source]
   (let [text (slurp source)
         lines (str/split-lines text)
         normalized (-> text
                        (str/replace "*" "")
                        (str/replace #"[ \t]+" " ")
                        (str/replace #" ?\n ?" " "))]
     {:lines lines :normalized normalized :sha256-lines (count lines)})))

(defn first-line= [prefix {:keys [lines]}]
  (boolean (and (seq lines) (str/starts-with? (first lines) prefix))))

(defn has-line-starting [prefix {:keys [lines]}]
  (boolean (some #(str/starts-with? % prefix) lines)))

(defn section-before
  "True when marker-a first appears before marker-b in the text."
  [marker-a marker-b {:keys [normalized]}]
  (let [ia (str/index-of normalized marker-a)
        ib (str/index-of normalized marker-b)]
    (boolean (and ia ib (< ia ib)))))

(defn has [needle {:keys [normalized]}]
  (str/includes? normalized needle))

(defn has-both [a b m]
  (and (has a m) (has b m)))

(def checks
  "The 21 reviewed completion checks, keyed by hole id. Each entry names the
  clause's observable condition as a predicate over the mission record."
  {["problems/r12-two-layer-calibration" 0]
   {:check :title-names-pattern-retrieval-calibration
    :predicate (fn [m] (first-line= "# Mission: Pattern Retrieval Calibration" m))
    :observable "mission title line reads '# Mission: Pattern Retrieval Calibration'"}
   ["problems/most-patterns-have-no-globally-recognised-rationale" 1]
   {:check :status-open-derive
    :predicate (fn [m] (has-line-starting "**Status:** OPEN — DERIVE" m))
    :observable "mission status line reads 'OPEN — DERIVE'"}
   ["problems/most-patterns-have-no-globally-recognised-rationale" 2]
   {:check :owner-named-joe
    :predicate (fn [m] (has-line-starting "**Owner:** Joe" m))
    :observable "mission names Joe as owner ('**Owner:** Joe')"}
   ["cascade-construction/read-what-exists-first" 3]
   {:check :cross-refs-section-present
    :predicate (fn [m] (section-before "Cross-refs:" "## Motivation" m))
    :observable "mission opens a '**Cross-refs:**' listing before further claims"}
   ["problems/llm-over-corpus-mining-lessons" 4]
   {:check :mining-mission-establishes-artifact-proneness
    :predicate (fn [m] (has-both "M-pattern-mining.md" "artifact-prone" m))
    :observable "mission cites M-pattern-mining.md as having established the current substrate artifact-prone"}
   ["problems/r7-evidence-channel-precision" 5]
   {:check :minilm-cosine-named-artifact-prone
    :predicate (fn [m] (has "futon3a MiniLM-cosine substrate is artifact-prone and unsafe to treat as a clean sensor" m))
    :observable "mission states the futon3a MiniLM-cosine substrate is artifact-prone and unsafe to treat as a clean sensor"}
   ["problems/r7-evidence-channel-precision" 6]
   {:check :clean-sensor-withdrawn
    :predicate (fn [m] (has "unsafe to treat as a clean sensor" m))
    :observable "mission concludes it is 'unsafe to treat as a clean sensor'"}
   ["pattern-mining/probe-the-claimed-property-not-the-acceptance-proxy" 9]
   {:check :application-well-specified-repeatable-useful
    :predicate (fn [m] (has "well-specified, repeatable, and demonstrably useful" m))
    :observable "mission requires pattern application be well-specified, repeatable, and demonstrably useful"}
   ["cascade-construction/read-what-exists-first" 10]
   {:check :excursion-cited-by-path
    :predicate (fn [m] (has-both "E-pattern-peripheral.md" "companion Codex-handoff" m))
    :observable "mission cites E-pattern-peripheral.md as the companion Codex-handoff excursion"}
   ["problems/most-patterns-have-no-globally-recognised-rationale" 11]
   {:check :creation-side-gate-named-symmetric
    :predicate (fn [m] (has "symmetric creation-side gate to this mission's retrieval-side calibration" m))
    :observable "mission names the excursion the symmetric creation-side gate to this mission's retrieval-side calibration"}
   ["problems/r12-two-layer-calibration" 12]
   {:check :mission-is-retrieval-side
    :predicate (fn [m] (has "retrieval-side calibration" m))
    :observable "mission marks itself the retrieval-side calibration counterpart"}
   ["cascade-construction/read-what-exists-first" 13]
   {:check :blocks-on-p1-named
    :predicate (fn [m] (has "Blocks on P-1" m))
    :observable "mission states it 'Blocks on P-1' (the canonical flexiarg parser)"}
   ["cascade-construction/read-what-exists-first" 14]
   {:check :projection-clj-path-named
    :predicate (fn [m] (has "futon3a/src/futon/flexiarg/projection.clj" m))
    :observable "mission names futon3a/src/futon/flexiarg/projection.clj as the P-1 parser that must ship"}
   ["problems/llm-over-corpus-mining-lessons" 15]
   {:check :advisory-mode-first
    :predicate (fn [m] (has "advisory mode first" m))
    :observable "mission says the excursion lands in advisory mode first"}
   ["pattern-mining/probe-the-claimed-property-not-the-acceptance-proxy" 16]
   {:check :enforced-after-violation-corpus
    :predicate (fn [m] (has "enforced mode after a violation corpus exists" m))
    :observable "mission defers enforced mode until a violation corpus exists"}
   ["cascade-construction/read-what-exists-first" 17]
   {:check :trip-journal-cited-as-complement
    :predicate (fn [m] (has-both "M-trip-journal.md" "the B→A complement" m))
    :observable "mission cites M-trip-journal.md as the B→A complement to this mission"}
   ["problems/llm-over-corpus-mining-lessons" 18]
   {:check :improve-ab-in-btoa-learnable-form
    :predicate (fn [m] (has "improve the A→B side in a form that the B→A side can" m))
    :observable "mission states it should improve the A→B side in a form the B→A side can learn from"}
   ["problems/llm-over-corpus-mining-lessons" 19]
   {:check :btoa-can-actually-learn
    :predicate (fn [m] (has "actually learn from" m))
    :observable "mission's criterion is that the B→A side can 'actually learn from' the result"}
   ["cascade-construction/read-what-exists-first" 20]
   {:check :reweighting-excursion-cited-by-path
    :predicate (fn [m] (has "E-btoa-candidate-queue-reweighting-v0.md" m))
    :observable "mission cites E-btoa-candidate-queue-reweighting-v0.md as phase-1 B→A/SORRY weighting"}
   ["cascade-construction/read-what-exists-first" 21]
   {:check :mapping-excursion-cited-with-sample
    :predicate (fn [m] (has-both "E-btoa-evidence-sample-mapping-v0.md" "first hand-labeled sample" m))
    :observable "mission cites E-btoa-evidence-sample-mapping-v0.md together with the first hand-labeled sample"}
   ["pattern-mining/probe-the-claimed-property-not-the-acceptance-proxy" 22]
   {:check :first-hand-labeled-sample-named
    :predicate (fn [m] (has-both "phase-1 B→A/SORRY" "first hand-labeled sample" m))
    :observable "mission names the phase-1 weighting and the first hand-labeled sample as the seed evidence"}})

(defn verify
  "Run every reviewed check against the mission record (or an supplied
  mission map from read-mission, so tests can exercise the negative path).
  Returns {:hole-id {:check :satisfied? :observable}} for all 21 holes."
  ([] (verify (read-mission)))
  ([mission]
   (into {}
         (map (fn [[hole-id {:keys [check predicate observable]}]]
                [hole-id {:check check
                          :satisfied? (boolean (predicate mission))
                          :observable observable}]))
         checks)))

(defn summary
  "Compact all-or-nothing report with per-hole detail for failures."
  ([] (summary (verify)))
  ([results]
   {:total (count results)
    :all-satisfied? (every? :satisfied? (vals results))
    :unsatisfied (keep (fn [[k v]] (when-not (:satisfied? v) k)) results)}))

(defn -main [& _]
  (let [report (summary)]
    (println (pr-str (assoc report :mission-path mission-path)))
    (System/exit (if (:all-satisfied? report) 0 1))))
