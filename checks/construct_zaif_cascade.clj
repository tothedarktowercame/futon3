(ns construct-zaif-cascade
  "The CONSTRUCTOR of LA1c-restatement.md §4 run over the WHOLE `library/` for one
   real zaif seat task -- worklist row `:LA5`
   (futon3/holes/labs/library-contract/worklist.edn).

   Zaif is the third of the four domains LA1c §2 names, and it is there for one
   reason the other three cannot supply: **its repository is the whole library**,
   so it is the only domain that exercises `find` at the library's real size
   (1,255 flexiargs, 107 sections).  `:LA3` also ran over the whole library, but
   on a tension ABOUT the library; this one is about a seat doing an unrelated
   job, so the search space and the tension are independent for the first time.

   WHAT IS HERE, and what is deliberately NOT.

   Not here: a second copy of `acknowledgements`, `match`, `antecedent-holds?`,
   `score`, `ranked`, the three policy-grain rules, the two temperaments,
   `initial-state`, `run` or `cascade-of`.  All of them come from
   `construct_cascade.clj`; `tension` is the one thing this file rebinds, and it
   is `^:dynamic` there for exactly that.  `construct_ants_cascade.clj` made the
   same split for `:LA4` and this file makes it again rather than growing a third
   spelling of the constructor.

   Here: the zaif Tension and its authored cues, the falsifiers named BEFORE the
   run, and the artefact `checks/zaif-cascade.edn` -- the constructed cascade as
   DATA (members, precedence, provenance, stop), which is what
   `futon3c/scripts/zaif_cascade_gate.clj` reads.

   WHY THE SPLIT IS THE POINT OF THIS ROW, and not just of this file.
   `:LA5`'s acceptance asks for a cascade REVIEWED BEFORE THE RUN by someone
   other than its constructor, because that is what LA1c §7 says a carried
   cascade can offer and per-decision arm arithmetic cannot: `zaif_controller.clj`
   chooses an arm per round from fixed constants its own docstring calls
   \"fixed, uncalibrated\", and there is no object to read until the decisions
   have already happened.  This file writes the object.  It runs no seat, reads
   no transcript, and computes no comparison -- the gate does that, afterwards,
   from this artefact.

   THE TENSION IS A REAL TASK, NOT A DESCRIPTION OF ONE.
   `checks/zaif-task-a97J05.md` is a verbatim copy of one persisted turn-start
   prompt (evidence `e-cb139dba-b1db-4e85-9353-c9b8b1c8c62d`, agent `zai-4`,
   2026-08-06), and every clause below cites a line span of that file.  The
   prompt was written by `apm-driver` in August for a Lean proving job and not
   by anyone working on this row.

   THE CONFOUND, recorded here rather than discovered by a reader.  The author of
   these cues had already read the TRANSCRIPT of the turn this prompt started --
   102 rounds, `final false`, i.e. the seat ran out of tool rounds without ever
   reporting.  So the cue set is not blind to the outcome.  Three guards, none of
   them a claim of blindness: (i) every clause is a line span of the PROMPT, which
   was fixed before the turn ran; (ii) the falsifiers below are named before the
   run and checked on every `-main`; (iii) the arm comparison the gate computes is
   against decisions ALREADY RECORDED in the store, which no part of this file can
   move.  This is the same confound `decisions.edn
   :constructor-degree-term-measured :confound-recorded` states for `:LA3`."
  (:require [clojure.pprint :as pprint]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [construct-cascade :as cc]
            [find-organise :as fo]))

(def task-file "checks/zaif-task-a97J05.md")

;; ---------------------------------------------------------------------------
;; the tension
;; ---------------------------------------------------------------------------
;; Same discipline as `construct_cascade.clj:88-100` and
;; `construct_ants_cascade.clj:46-64`: a clause carries the text it is a clause
;; of, the line span of the record that states it, and the literal CUES an author
;; wrote for it.  Crude on purpose, because a literal occurrence is the weakest
;; thing that can still be CITED, and authored on purpose, because the
;; alternative is the embedding relevance F3 refuses.
;;
;; THE CUE LICENCE, added after the PRE-RUN REVIEW of futon3 37f7506 (codex-17,
;; 2026-09-02, verdict REVISE).  A cue must OCCUR IN ITS OWN CLAUSE'S CITED SPAN
;; of the task file.  The first cue set here did not meet that and nothing
;; checked it: `"scope"`, `"cap"` and `"reuse"` appear in no span of the
;; prompt, and the review found them by hand -- 18 of the 27 cues turned out to
;; be the author's paraphrase of a clause rather than the clause's own words.
;; `"scope"` alone put `plos-npt-with-small-n/scope-shield-via-companion-paper`
;; -- a pattern about narrowing an academic paper -- into a seed about a frozen
;; Lean theorem, which is the review's finding and is why that pattern is now a
;; named falsifier below.
;;
;; The licence is a REMOVAL rule and never an addition rule: `cue-licence`
;; strikes an unlicensed cue and no cue was added after the first run, because a
;; cue added once the seed is known is tuning whatever it is called.

(def tension
  {:id :the-seat-does-not-stop
   :statement "A zaif seat was given a bounded proving job with three stated bounds -- consult the store first, keep to a per-problem time budget, and stop honestly rather than force a proof -- and the arm controller that shadowed it has no term for any of them. zaif v0 (futon3c/src/futon3c/agents/zaif_controller.clj:105 `decide`, over the fixed uncalibrated constants at :11-20) scores four arms per round from beliefs about the current round alone: nothing it reads carries how many rounds have already gone, what the job's stated budget was, or whether stopping is permitted. The Tension is the seat's own task, and the repository it has to be answered from is the whole library."
   :source "checks/zaif-task-a97J05.md:30-77 (verbatim copy of evidence e-cb139dba-b1db-4e85-9353-c9b8b1c8c62d, turn zai-turn-f1513520-b760-4b67-bbd1-b2941d3e0325, agent zai-4, 2026-08-06T15:26:42Z)"
   :clauses
   [{:id :consult-the-store-before-attempting
     :text "Consult `memory_search` and `psr_search` for routes (tags in your own vocabulary; the store holds routes from prior closed problems)."
     :source "checks/zaif-task-a97J05.md:48-49"
     :cues ["search" "prior"]}
    {:id :cite-what-you-actually-used
     :text "Cite every memory whose content you actually use with `-- (Memory: e-<id>)` adjacent to the informed code, and every PATTERN whose content you use. The forms are machine-checked and distinct."
     :source "checks/zaif-task-a97J05.md:50-56"
     :cues ["cite" "machine-check"]}
    {:id :stopping-honestly-beats-a-forced-fake
     :text "SKIPPED: statement attempted < 10 minutes, blocker identified and stated. Skipping honestly is BETTER than a forced fake."
     :source "checks/zaif-task-a97J05.md:63-65"
     :cues ["blocker"]}
    {:id :the-statement-is-frozen-and-out-of-bounds
     :text "THE STATEMENTS ARE FROZEN. You may NOT change any statement, hypothesis, definition, or theorem name. If a statement seems wrong or unprovable as stated, SAY SO in your report and move on; do not repair it yourself."
     :source "checks/zaif-task-a97J05.md:38-43"
     :cues ["frozen"]}
    {:id :the-budget-is-stated-and-must-bind
     :text "Budget ~15-25 minutes per problem; move on when the budget is spent. Total set budget: do not exceed the job window."
     :source "checks/zaif-task-a97J05.md:68-69"
     :cues ["budget" "spent"]}]})

(def zero-mass-pattern
  "F4's falsifier, named BEFORE the run and checked on every one.  Same pattern
   `:LA3` named for a different tension, and named again here for the reason that
   makes it worth naming at all: this run searches all 107 sections, so a
   falsifier tests whether `find` WANDERS at library scale.  A seat deciding
   whether to keep reading Mathlib in a Lean proving turn has no clause about
   pheromone decay in an ant board simulation."
  :ants/pheromone-trail-tuner)

(def review-named-falsifier
  "A SECOND falsifier, named by the PRE-RUN REVIEWER rather than by the
   constructor, and recorded with its provenance because that is the whole point
   of the row.  Asked in the review packet to name a harder falsifier than
   `ants/pheromone-trail-tuner`, codex-17 named this one and observed that it was
   SELECTED under the first cue set -- `library/plos-npt-with-small-n/scope-shield-via-companion-paper.flexiarg:14-20`
   is about narrowing an academic paper and naming its companion, not about
   respecting a frozen theorem statement, and it entered the seed through the
   unlicensed cue `\"scope\"`.

   So this is not a falsifier chosen because it is safe.  It is one that FAILED
   before the cue licence and is checked here to establish that the licence is
   what removed it.  `:LA3`'s and `:LA4`'s single falsifiers tested gross
   wandering; this one tests false positives, which the review is right that they
   do not."
  :plos-npt-with-small-n/scope-shield-via-companion-paper)

(def watched-pattern
  "NOT a falsifier -- a WATCH, named before the run and reported either way.
   `budgeted-action-selection/mana-gated-work` is the surface-similar pattern of
   this library: its title, keywords and THEN are all about budget-gated action
   selection, which is what clause :the-budget-is-stated-and-must-bind is about.
   Its budget is nonetheless a DIFFERENT quantity -- the operator's session-level
   mana balance across a working session (its `+ ON-UNIT-CORRECTION` is explicit:
   \"budgets attach to sessions, not to agents ... an agent has no metabolism
   beyond its turn\") -- and the seat's budget here is tool rounds inside one
   turn.  It is not made a falsifier precisely because a reader could argue
   either way, and a falsifier one can argue about is not one."
  :budgeted-action-selection/mana-gated-work)

(def report-path "checks/zaif-cascade.edn")

(def gate-artifact
  "Written by `futon3c/scripts/zaif_cascade_gate.clj`.  Read if present, so the
   O4 row below is filled from a RUN rather than left as a claim; absent on the
   first pass, which is the honest ordering (the gate reads this file's cascade,
   so the cascade has to exist first)."
  "../futon3c/holes/zaif-cascade-gate.edn")

;; ---------------------------------------------------------------------------
;; the cascade, as data
;; ---------------------------------------------------------------------------

(defn cascade-data
  "The constructed cascade as the GATE needs it.  Everything here is a field of
   the CascadeState the constructor finished with; nothing is recomputed.  The
   gate reads this and authors nothing about which pattern means which arm --
   that mapping is its own, committed before it runs, and the two sides never
   share a table."
  [temperament-id final]
  (sorted-map
   :id temperament-id
   :members (vec (sort (:members final)))
   :precedence (into (sorted-map) (select-keys (:precedence final) (:members final)))
   :authored-order (vec (:authored-order final))
   :ordered (fo/ordered final)
   :provenance (into (sorted-map) (select-keys (:provenance final) (:members final)))
   :stop (:stop final)
   :steps (:steps final)))

;; ---------------------------------------------------------------------------
;; the report
;; ---------------------------------------------------------------------------

(defn- read-gate []
  (let [f (io/file gate-artifact)]
    (when (.exists f) (edn/read-string (slurp f)))))

(defn o4-row
  "O4 (`find_organise.clj:523`) over the GATE's two arms: the constructed cascade,
   and the same cascade with two members' precedence exchanged.  The law is
   evaluated by `fo/o4-precedence-governance` and not by a second spelling of it
   in the gate: the gate reports the six numbers, this file applies the predicate."
  [gate]
  (when-let [o4 (:o4 gate)]
    (let [row {:precedence-before (:precedence-before o4)
               :precedence-after (:precedence-after o4)
               :acting-order-before (:acting-order-before o4)
               :acting-order-after (:acting-order-after o4)
               :score-before (:score-before o4)
               :score-after (:score-after o4)}]
      (sorted-map :row (into (sorted-map) row)
                  :holds? (fo/o4-precedence-governance row)
                  :precedence-changed? (not= (:precedence-before row) (:precedence-after row))
                  :acting-order-changed? (not= (:acting-order-before row) (:acting-order-after row))
                  :score-changed? (not= (:score-before row) (:score-after row))))))

(defn cue-licence
  "THE CUE LICENCE, checked rather than promised.  Every cue of every clause must
   occur, case-insensitively, in that clause's own cited span of the task file,
   re-read from disk on every run.  An unlicensed cue is a hard failure: it is a
   word the constructor's author chose, presented as a word the task uses, and it
   is exactly how `\"scope\"` put a paper-scoping pattern into a seed about a
   frozen theorem before the pre-run review caught it by hand."
  []
  (let [lines (vec (str/split-lines (slurp task-file)))]
    (vec (for [{:keys [id source cues]} (:clauses tension)
               :let [[_ span] (str/split source #":")
                     [from to] (mapv parse-long (str/split span #"-"))
                     text (when (and from to (<= to (count lines)))
                            (str/lower-case (str/join " " (subvec lines (dec from) to))))]
               cue cues
               :when (not (and text (str/includes? text (str/lower-case cue))))]
           (sorted-map :clause id :cue cue :source source
                       :finding :cue-does-not-occur-in-its-own-cited-span)))))

(defn task-file-correspondence
  "Every clause's `:source` is a line span of `task-file`, re-read from disk on
   every run: the span must exist and must contain the clause's own text after
   whitespace normalisation.  Without this the Tension is five Clojure strings
   that only LOOK like a copy of a persisted prompt."
  []
  (let [lines (vec (str/split-lines (slurp task-file)))
        norm #(some-> % str/trim (str/replace #"\s+" " ") str/lower-case)]
    (mapv (fn [{:keys [id text source]}]
            (let [[path span] (str/split source #":")
                  [from to] (mapv parse-long (str/split span #"-"))
                  got (when (and from to (<= 1 from) (<= to (count lines)))
                        (norm (str/join " " (subvec lines (dec from) to))))
                  ;; the prompt wraps and uses an en dash where the clause text
                  ;; uses a hyphen, so compare on the clause's longest unbroken
                  ;; ASCII run rather than on the whole sentence.
                  probe (->> (str/split (norm text) #"[^a-z0-9_/]+")
                             (filter #(>= (count %) 6))
                             (take 40))]
              (sorted-map :clause id :source source
                          :file-is-the-task-file? (= path task-file)
                          :span-exists? (some? got)
                          :probe-words (count probe)
                          :probe-words-not-in-span
                          (vec (remove #(str/includes? (or got "") %) probe)))))
          (:clauses tension))))

(defn report []
  (binding [cc/tension tension]
    (let [sections (cc/library-sections cc/library-root)
          why-repo (fo/read-repository cc/library-root sections {:kinds #{:why}})
          wh-repo (fo/read-repository cc/library-root sections {:kinds #{:why :how}})
          related (cc/related-adjacency wh-repo)
          {:keys [matches seed]} (cc/seed-and-candidates why-repo)
          ctx {:seed seed :matches matches :related related}
          found (fo/find {:context ctx
                          :route :cue-citation
                          :fires? (fn [id _] (cc/antecedent-holds? (get-in why-repo [:entries id])))
                          :receipt (fn [id] (cc/receipt-for (get-in why-repo [:entries id])))}
                         why-repo)
          selected (set (:selected found))
          find-row {:scenario [(:id tension)]
                    :repository (:patterns why-repo)
                    :selected selected
                    :receipted (set (keys (:receipts found)))
                    :non-self-certifying (into #{} (comp (filter (comp cc/cites-text? val))
                                                         (map key))
                                               (:receipts found))
                    :zero-mass #{zero-mass-pattern}
                    :absence (:absence found)}
          runs (into (sorted-map)
                     (for [t cc/temperaments]
                       (let [final (cc/run t ctx)]
                         [(:id t) (assoc final :cascade (cc/cascade-of final seed why-repo))])))
          acks (vec (mapcat #(cc/acknowledgements (get-in why-repo [:entries %]))
                            (sort (into (set seed)
                                        (mapcat #(cc/admitted-of (val %)) runs)))))
          state0 (cc/initial-state ctx)
          scored (remove (:members state0) (keys (:candidates state0)))
          degrees (mapv #(cc/degree related (:members state0) %) scored)
          budget-cascade (get-in runs [(:id cc/budgeted-temperament) :cascade])
          gate (read-gate)]
      (sorted-map
       :as-of (sorted-map
               :sections (count sections)
               :patterns (count (:patterns why-repo))
               :read-digest (cc/read-digest wh-repo)
               :authored-why-edges (count (:edges why-repo))
               :authored-why-how-edges (count (:edges wh-repo))
               :dangling-edges (count (:dangling why-repo))
               :why-acyclic? (:acyclic? why-repo)
               ;; LB1's finding, `decisions.edn :math-formalization-untracked-flexiargs`:
               ;; library/math-formalization* takes files from a live scribe with no
               ;; baseline step, so a count from this file is a timestamp, not a
               ;; standing property.  The digest is what makes that visible.
               :untracked-flexiargs-are-possible? true)
       :tension (sorted-map
                 :id (:id tension)
                 :source (:source tension)
                 :clause-hits (into (sorted-map)
                                    (for [c (:clauses tension)]
                                      [(:id c)
                                       (count (filter (fn [e]
                                                        (some #(= (:id c) (:clause %))
                                                              (cc/acknowledgements e)))
                                                      (vals (:entries why-repo))))]))
                 :clauses-that-hit-nothing
                 (vec (for [c (:clauses tension)
                            :when (not-any? (fn [e] (some #(= (:id c) (:clause %))
                                                          (cc/acknowledgements e)))
                                            (vals (:entries why-repo)))]
                        (:id c)))
                 :match-distribution (into (sorted-map) (frequencies (vals matches))))
       :find (sorted-map
              :selected (vec (sort selected))
              :seed-size (count seed)
              :candidates (- (count matches) (count seed))
              :zero-mass-pattern zero-mass-pattern
              :zero-mass-in-repository? (contains? (:patterns why-repo) zero-mass-pattern)
              :zero-mass-selected? (contains? selected zero-mass-pattern)
              :review-named-falsifier review-named-falsifier
              :review-named-falsifier-in-repository?
              (contains? (:patterns why-repo) review-named-falsifier)
              :review-named-falsifier-selected? (contains? selected review-named-falsifier)
              :review-named-falsifier-match (get matches review-named-falsifier 0)
              :review-named-falsifier-was-selected-before-the-cue-licence? true
              :watched-pattern watched-pattern
              :watched-in-repository? (contains? (:patterns why-repo) watched-pattern)
              :watched-selected? (contains? selected watched-pattern)
              :watched-match (get matches watched-pattern 0)
              :sections-supplying-the-seed (vec (sort (distinct (map #(keyword (namespace %)) selected))))
              :laws (into (sorted-map)
                          (for [[law holds?] fo/find-laws] [law (boolean (holds? find-row))])))
       :temperaments (cc/differ-only-in-the-stop cc/budgeted-temperament
                                                 cc/floored-temperament)
       :runs (into (sorted-map)
                   (for [[id final] runs]
                     [id (sorted-map
                          :stop (:stop final)
                          :steps (:steps final)
                          :members (count (:members final))
                          :admitted (vec (cc/admitted-of final))
                          :record (mapv (fn [e]
                                          (sorted-map :step (:step e) :edit (:edit e)
                                                      :by (:by e) :pattern (:pattern e)
                                                      :reason (:reason e)))
                                        (:record final))
                          :cascade-nodes (count (get-in final [:cascade :nodes]))
                          :cascade-edges (count (get-in final [:cascade :edges]))
                          :laws (into (sorted-map)
                                      (for [[law holds?] (dissoc fo/organise-laws :O4)]
                                        [law (boolean (holds? (:cascade final)))]))
                          :cascade (cascade-data id final))]))
       :cascades-differ?
       (not= (get-in runs [(:id cc/budgeted-temperament) :members])
             (get-in runs [(:id cc/floored-temperament) :members]))
       :scores-at-step-0 (vec (for [[p s] (take 40 (cc/ranked state0))]
                                [p (double s) (get matches p) (cc/degree related seed p)]))
       ;; the THIRD measurement of the term `decisions.edn
       ;; :constructor-degree-term-measured` records.  :LA3 measured it on a
       ;; tension about the library (4 of 50 above zero, sections :cascades and
       ;; :snatch); :LA4 on a five-pattern repository with no authored edge at all
       ;; (0 of 1).  This one is over the same whole library as :LA3 but on a
       ;; tension that is about a seat's job and not about the library, which is
       ;; the case neither of those covers.
       :degree-term (sorted-map
                     :candidates-scored (count scored)
                     :candidates-with-degree-above-zero (count (filter pos? degrees))
                     :degree-distribution (into (sorted-map) (frequencies degrees))
                     :authored-edges-in-this-repository (count (:edges wh-repo))
                     :sections-supplying-the-degree-term
                     (vec (sort (distinct (map #(keyword (namespace %))
                                               (map first (filter (fn [[_ d]] (pos? d))
                                                                  (map vector scored degrees)))))))
                     :distinguishable-from-uniform?
                     (boolean (some (fn [t]
                                      (let [real (cc/run t ctx)
                                            unif (cc/run t (assoc ctx :uniform? true))]
                                        (or (not= (cc/admitted-of real) (cc/admitted-of unif))
                                            (not= (:stop real) (:stop unif)))))
                                    cc/temperaments)))
       :o4 (or (o4-row gate) :not-exercised-the-gate-has-not-run-yet)
       :controls (sorted-map
                  :determinism-failures (cc/determinism ctx)
                  :library-correspondence (cc/library-correspondence why-repo)
                  :task-file-correspondence (task-file-correspondence)
                  :unlicensed-cues (cue-licence)
                  :cues-declared (reduce + (map (comp count :cues) (:clauses tension)))
                  :grain-separation (cc/grain-separation ctx)
                  :grain-conjunct-mutations (cc/grain-conjunct-mutations ctx)
                  :degree-relation-authors-no-edge
                  (cc/degree-relation-authors-no-edge budget-cascade related)
                  :citations (count acks)
                  :citations-that-do-not-read-back (cc/citations-verified acks)
                  :negative-controls-declared [:O2-invented-edge :O3-dropped-edge
                                               :O1-unrecorded-node
                                               :citation-cue-not-in-the-cited-span
                                               :citation-span-off-the-end-of-the-file]
                  ;; A mutation that cannot be BUILT on this run reads like a
                  ;; passing one in the report, so it is declared absent BY NAME
                  ;; with the property of the run that makes it unbuildable --
                  ;; and that property is asserted from the data beside it, not
                  ;; from a docstring.  `construct_ants_cascade.clj` does the
                  ;; same for the same two controls; there the reason was that
                  ;; `library/ants` holds no authored edge at all.  Here the
                  ;; library holds plenty and the CASCADE holds none, which is a
                  ;; different fact and the more interesting one.
                  :negative-controls-not-available-here
                  (let [n (count (:edges budget-cascade))]
                    (if (pos? n)
                      (sorted-map)
                      (sorted-map
                       :O3-dropped-edge :the-constructed-cascade-has-no-authored-edge-to-drop
                       :cascade-edges n
                       :authored-why-edges-in-the-library (count (:edges why-repo)))))
                  :negative-controls
                  (vec (remove #(and (= :O3-dropped-edge (:control %))
                                     (not (:exercised? %))
                                     (zero? (count (:edges budget-cascade))))
                               (cc/negative-controls budget-cascade acks related))))))))

(defn require-pass!
  "The failures that stop this run.  What is NOT here, and why:
   `:constructor-admitted-nothing` -- if every candidate scores below epsilon the
   floored temperament admits nothing, and that is a RESULT about the library's
   thinness, not a defect (`construct_ants_cascade.clj` made the same call for
   the same reason)."
  [result]
  (let [failures
        (concat
         (for [[law holds?] (get-in result [:find :laws]) :when (not holds?)]
           {:where :find :finding :law-fails :law law})
         (when-not (get-in result [:find :zero-mass-in-repository?])
           [{:where :find :finding :f4-falsifier-is-not-in-the-repository
             :pattern (get-in result [:find :zero-mass-pattern])}])
         (when (get-in result [:find :zero-mass-selected?])
           [{:where :find :finding :f4-falsifier-was-selected
             :pattern (get-in result [:find :zero-mass-pattern])}])
         (when-not (get-in result [:find :review-named-falsifier-in-repository?])
           [{:where :find :finding :review-named-falsifier-is-not-in-the-repository
             :pattern (get-in result [:find :review-named-falsifier])}])
         (when (get-in result [:find :review-named-falsifier-selected?])
           [{:where :find :finding :review-named-falsifier-was-selected
             :pattern (get-in result [:find :review-named-falsifier])}])
         (for [c (get-in result [:controls :unlicensed-cues])]
           {:where :controls :finding :unlicensed-cue :clause (:clause c) :cue (:cue c)})
         (when-not (get-in result [:temperaments :holds?])
           [{:where :temperaments :finding :temperaments-differ-in-more-than-the-stop}])
         (for [[id row] (:runs result)
               [law holds?] (:laws row) :when (not holds?)]
           {:where :runs :finding :law-fails :law law :temperament id})
         (for [[id row] (:runs result) :when (empty? (get-in row [:cascade :members]))]
           {:where :runs :finding :empty-cascade :temperament id})
         (for [r (get-in result [:controls :grain-separation])]
           {:where :controls :finding :grain-leak :rule r})
         ;; the grain conjunct must be doing work, not decorating: at least one
         ;; rule has to fire under the ENRICHED forge, or the separation above
         ;; would also hold on key-disjointness alone.
         (when-not (some #(= :fires (:enriched %))
                         (get-in result [:controls :grain-conjunct-mutations]))
           [{:where :controls :finding :grain-conjunct-is-decoration}])
         (for [d (get-in result [:controls :determinism-failures])]
           (assoc d :where :controls))
         (for [c (get-in result [:controls :library-correspondence])
               :when (not (and (:in-repository? c) (:source-line-is-a-then? c)))]
           {:where :controls :finding :rule-does-not-encode-an-authored-then :rule (:rule c)})
         (for [c (get-in result [:controls :task-file-correspondence])
               :when (not (and (:file-is-the-task-file? c) (:span-exists? c)
                               (empty? (:probe-words-not-in-span c))))]
           {:where :controls :finding :clause-is-not-a-span-of-the-task-file
            :clause (:clause c) :missing (:probe-words-not-in-span c)})
         (for [c (get-in result [:controls :citations-that-do-not-read-back])]
           {:where :controls :finding :citation-does-not-read-back :citation c})
         (for [c (get-in result [:controls :negative-controls])
               :when (not (and (:exercised? c) (:caught? c)))]
           {:where :controls
            :finding (if (:exercised? c) :mutation-slipped :mutation-not-exercised)
            :control (:control c)}))]
    (when (seq failures)
      (throw (ex-info "construct-zaif-cascade: law or control failed"
                      {:finding (or (:finding (first failures)) :unknown)
                       :failures (vec failures)})))
    result))

(defn -main [& _]
  (try
    (let [result (require-pass! (report))]
      (spit report-path (with-out-str (pprint/pprint result)))
      (println (format "library/ whole: %d sections, %d patterns, %d authored @why edges, %d @why+@how; read-digest %s"
                       (get-in result [:as-of :sections])
                       (get-in result [:as-of :patterns])
                       (get-in result [:as-of :authored-why-edges])
                       (get-in result [:as-of :authored-why-how-edges])
                       (subs (get-in result [:as-of :read-digest]) 0 8)))
      (println (format "tension %s: seed %d (F1-F4 %s), candidates %d"
                       (name (:id tension))
                       (get-in result [:find :seed-size])
                       (pr-str (get-in result [:find :laws]))
                       (get-in result [:find :candidates])))
      (println (format "  clause hits %s; clauses that hit nothing %s"
                       (pr-str (get-in result [:tension :clause-hits]))
                       (pr-str (get-in result [:tension :clauses-that-hit-nothing]))))
      (println (format "  seed sections %s" (pr-str (get-in result [:find :sections-supplying-the-seed]))))
      (println (format "F4 falsifier %s selected? %s"
                       (get-in result [:find :zero-mass-pattern])
                       (get-in result [:find :zero-mass-selected?])))
      (println (format "review-named falsifier %s selected? %s (match %d) -- selected under the pre-licence cue set: %s"
                       (get-in result [:find :review-named-falsifier])
                       (get-in result [:find :review-named-falsifier-selected?])
                       (get-in result [:find :review-named-falsifier-match])
                       (get-in result [:find :review-named-falsifier-was-selected-before-the-cue-licence?])))
      (println (format "watched %s selected? %s (match %d)"
                       (get-in result [:find :watched-pattern])
                       (get-in result [:find :watched-selected?])
                       (get-in result [:find :watched-match])))
      (println (format "cue licence: %d cues declared, %d unlicensed"
                       (get-in result [:controls :cues-declared])
                       (count (get-in result [:controls :unlicensed-cues]))))
      (doseq [[id row] (:runs result)]
        (println (format "  %-34s stop %-40s members %d, O1-O3 %s"
                         (name id) (pr-str (:stop row)) (:members row) (pr-str (:laws row))))
        (println (format "      ordered (most precedent first): %s"
                         (pr-str (get-in row [:cascade :ordered])))))
      (let [d (:degree-term result)]
        (println (format "degree term over the whole library: %d of %d scored candidates above zero (%s), from %s; distinguishable from uniform = %s"
                         (:candidates-with-degree-above-zero d) (:candidates-scored d)
                         (pr-str (:degree-distribution d))
                         (pr-str (:sections-supplying-the-degree-term d))
                         (:distinguishable-from-uniform? d))))
      (println (format "O4 over the gate: %s"
                       (if (map? (:o4 result)) (pr-str (select-keys (:o4 result) [:holds? :acting-order-changed? :score-changed?]))
                           (pr-str (:o4 result)))))
      (println (format "controls: %d citations re-read, %d unreadable; grain leaks %d; %d mutations declared, %d slipped; task-file spans %d/%d verified"
                       (get-in result [:controls :citations])
                       (count (get-in result [:controls :citations-that-do-not-read-back]))
                       (count (get-in result [:controls :grain-separation]))
                       (count (get-in result [:controls :negative-controls-declared]))
                       (count (remove #(and (:exercised? %) (:caught? %))
                                      (get-in result [:controls :negative-controls])))
                       (count (filter #(and (:file-is-the-task-file? %) (:span-exists? %)
                                            (empty? (:probe-words-not-in-span %)))
                                      (get-in result [:controls :task-file-correspondence])))
                       (count (get-in result [:controls :task-file-correspondence]))))
      (println (format "wrote %s" report-path))
      (println "construct-zaif-cascade: PASS exit-convention=0-pass/1-fail")
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println "construct-zaif-cascade: FAIL" (ex-message e))
      (pprint/pprint (ex-data e))
      (shutdown-agents)
      (System/exit 1))))
