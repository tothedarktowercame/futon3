(ns construct-ants-cascade
  "The CONSTRUCTOR of LA1c-restatement.md §4 run over `library/ants` -- worklist
   row `:LA4` (futon3/holes/labs/library-contract/worklist.edn).

   `:LA3` built the constructor and ran it once, on one tension, over the whole
   library.  This file is the SECOND domain LA1c §6 names, and Ants is there for
   one reason: it is the CONTROL.  Its authority gate refused every real pattern
   on held-out yield in 2026-07-16 with sound controls
   (`futon2/holes/cascade-ants.edn:21-23`), so a constructor that only ever
   confirms is caught here.  LA1c §6 states the expected result in advance and
   this file does not soften it: **the honest expectation is that the cascade
   also fails the gate, and a refusal is a delivery.**

   WHAT IS HERE, and what is deliberately NOT.

   Not here: a second copy of `acknowledgements`, `match`, `antecedent-holds?`,
   `score`, `ranked`, the three policy-grain rules, the two temperaments,
   `initial-state`, `run` or `cascade-of`.  All of them come from
   `construct_cascade.clj` unchanged; `tension` is the one thing this file
   rebinds, and it is `^:dynamic` there for exactly that.  A second copy would be
   the facade LA1c §11 names, one level below the firing loop `:LA3` moved into
   `find_organise.clj` for the same reason.

   Here: the ants Tension and its authored cues, the F4 falsifier named before
   the run, and the artefact `checks/ants-cascade.edn` -- the constructed cascade
   as DATA (members, precedence, provenance, stop), which is what
   `futon2/scripts/cascade_authority_gate.clj` reads.  That split is the point of
   LA1c §7: a carried cascade can be reviewed BEFORE the run, and the reviewer
   reads this artefact rather than a Clojure literal inside the runner.

   WHAT THIS RUN MEASURES THAT `:LA3` COULD NOT.
   `decisions.edn :constructor-degree-term-measured :what-would-change-this` names
   one thing: \"A second tension whose seed does not touch library/snatch or
   library/cascades.  If the degree term is 0 for every candidate there, the
   verdict is tension-specific and this entry overstates it.\"  `library/ants`
   holds FIVE patterns and ZERO authored `@why`/`@how` directives, so the degree
   term is identically zero over this repository before anything is scored.  That
   condition is met by construction, not by luck, and the run records it."
  (:require [clojure.pprint :as pprint]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [construct-cascade :as cc]
            [find-organise :as fo]))

;; ---------------------------------------------------------------------------
;; the tension
;; ---------------------------------------------------------------------------
;; Same discipline as `construct_cascade.clj:88-100`: a clause carries the text
;; it is a clause of, the record that states it, and the literal CUES an author
;; wrote for it.  Crude on purpose, because a literal occurrence is the weakest
;; thing that can still be CITED, and authored on purpose, because the
;; alternative is the embedding relevance F3 refuses.
;;
;; A CONFOUND, recorded here rather than discovered by a reader.  These cues were
;; written by an author who had already read all five ants antecedents -- there
;; are only five, and reading them is how one learns the domain at all.  So the
;; cue set is not blind, and the guard against tuning is F4 below, named before
;; the run and checked on every -main, plus the fourth clause, which is left in
;; with cues that hit NOTHING: the ants library says nothing about composing
;; itself, and a cue set tuned to the patterns would not carry a clause that
;; scores zero.  This is the same confound `decisions.edn
;; :constructor-degree-term-measured :confound-recorded` states for :LA3.

(def tension
  {:id :the-colony-does-not-eat
   :statement "Ants is the control domain: its authority gate refused every real pattern on held-out yield (P-validated-R5.md:692-694), and it refused CHANNEL 2 alone -- the patterns' own declared @aif-delta parameters, channel 1, have never been measured (cascade-ants.edn:29-38). The rubric is external and unambiguous: they eat (cascade-ants.edn:5). Yield is lost three ways the five patterns name between them -- the ant thrashes between nest and field without depositing, it burns ticks where food and pheromone traces are low, and hunger does not sharpen precision -- and no two of those remedies can be attached at once, because two patterns write one config key and the winner is attachment order rather than a property of either (cascade-ants.edn:110,148)."
   :source "futon2/holes/problems/P-validated-R5.md:685-694; futon2/holes/cascade-ants.edn:5,21-38,107-110,120-126,144-148"
   :clauses
   [{:id :the-ant-thrashes-instead-of-depositing
     :text "the ant walks home, refuses to deposit, and never re-enters outbound mode, starving the colony -- the failure `cr` exists to clamp"
     :source "futon2/holes/cascade-ants.edn:107"
     :cues ["thrash" "deposit" "return" "cargo" "nest"]}
    {:id :ticks-burn-where-the-traces-are-low
     :text "food and pheromone traces are low and novelty is high, and the two patterns that answer it BOTH write :efe :lambda :info"
     :source "futon2/holes/cascade-ants.edn:110"
     :cues ["novelty" "pheromone" "food" "outbound" "signal"]}
    {:id :hunger-does-not-sharpen-precision
     :text "Three patterns shape tau through DIFFERENT keys, so merge-deep does not detect the contention -- but they are arguing about one quantity. cr caps it, ws floors it, hp drives it. Whether cap > floor is never checked."
     :source "futon2/holes/cascade-ants.edn:120-126"
     :cues ["tau" "hunger" "precision" "reserve" "clamp"]}
    {:id :only-one-pattern-can-be-attached
     :text "`:pattern/active` is singular -- one pattern at a time. A cascade is by definition composed. Composition needs a resolution rule, and 'last write wins' is not one."
     :source "futon2/holes/cascade-ants.edn:144-148"
     :cues ["cascade" "compose" "one at a time" "singular" "resolution"]}]})

(def zero-mass-pattern
  "F4's falsifier, named BEFORE the run and checked on every one:
   `ants/baseline-cyber-ant` is the pattern of this repository the tension must
   NOT select.  Its `@aif-delta` is literally empty -- `cascade-ants.edn:48`
   calls it \"the identity element of the pattern algebra\" -- and its antecedent
   is about porting the Futon2 loop into a Futon5 operator, not about a forager
   losing yield.  It is also the SHAM arm of the authority gate, so a run that
   selected it would put the control inside the treatment."
  :ants/baseline-cyber-ant)

(def sections [:ants])
(def report-path "checks/ants-cascade.edn")
(def gate-artifact
  "Written by `futon2/scripts/cascade_authority_gate.clj`.  Read if present, so
   the O4 row below is filled from a RUN rather than left as a claim; absent on
   the first pass, which is the honest ordering (the gate reads this file's
   cascade, so the cascade has to exist first)."
  "../futon2/holes/cascade-authority-gate.edn")

;; ---------------------------------------------------------------------------
;; the cascade, as data
;; ---------------------------------------------------------------------------

(defn ordered-members
  "`fo/ordered` (find_organise.clj:382) over a finished CascadeState: the members in precedence order,
   least precedence number FIRST.  This vector is what the actuator folds, and
   `futon2/src/ants/cyber.clj`'s `ordered-members` recomputes it from the same
   two fields rather than trusting this one -- the artefact carries both so a
   disagreement is visible."
  [final]
  (fo/ordered final))

(defn cascade-data
  "The constructed cascade as the ACTUATOR needs it.  Everything here is a field
   of the CascadeState the constructor finished with; nothing is recomputed and
   nothing about `@aif-delta` appears -- the delta belongs to `cyber.clj`, which
   reads it from the same flexiarg files, and keeping it out of this artefact is
   what makes the two folds independent."
  [temperament-id final]
  (sorted-map
   :id temperament-id
   :members (vec (sort (:members final)))
   :precedence (into (sorted-map) (select-keys (:precedence final) (:members final)))
   :authored-order (vec (:authored-order final))
   :ordered (ordered-members final)
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
  "O4 (`find_organise.clj:523`, organiseO4PrecedenceGovernance) over the GATE's
   two arms: the constructed cascade, and the same cascade with the precedence of
   the two members that write `[:efe :lambda :info]` exchanged.  `:LA3` recorded
   O4 as `:not-exercised-nothing-is-played` because its constructions were never
   run; here they are, and the score is mean held-out yield.

   The law is evaluated by `fo/o4-precedence-governance` and not by a second
   spelling of it in the gate: the gate reports the six numbers, this file
   applies the predicate."
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
                  :score-changed? (not= (:score-before row) (:score-after row))))))

(defn report []
  (binding [cc/tension tension]
    (let [why-repo (fo/read-repository cc/library-root sections {:kinds #{:why}})
          wh-repo (fo/read-repository cc/library-root sections {:kinds #{:why :how}})
          related (cc/related-adjacency wh-repo)
          {:keys [matches seed]} (cc/seed-and-candidates why-repo)
          ctx {:seed seed :matches matches :related related}
          found (fo/find {:context ctx
                          :route :cue-citation
                          :fires? (fn [id _] (cc/antecedent-holds? (get-in why-repo [:entries id])))
                          :receipt (fn [id] (cc/receipt-for (get-in why-repo [:entries id])))}
                         why-repo)
          find-row {:scenario [(:id tension)]
                    :repository (:patterns why-repo)
                    :selected (set (:selected found))
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
          gate (read-gate)]
      (sorted-map
       :as-of (sorted-map
               :sections (vec sections)
               :patterns (count (:patterns why-repo))
               :read-digest (cc/read-digest wh-repo)
               :authored-why-edges (count (:edges why-repo))
               :authored-why-how-edges (count (:edges wh-repo))
               :dangling-edges (count (:dangling why-repo))
               :why-acyclic? (:acyclic? why-repo))
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
                 :match-distribution (into (sorted-map) (frequencies (vals matches))))
       :find (sorted-map
              :selected (vec (:selected found))
              :seed-size (count seed)
              :candidates (- (count matches) (count seed))
              :zero-mass-pattern zero-mass-pattern
              :zero-mass-selected? (contains? (set (:selected found)) zero-mass-pattern)
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
       :scores-at-step-0 (vec (for [[p s] (cc/ranked state0)]
                                [p (double s) (get matches p) (cc/degree related seed p)]))
       ;; the measurement `:constructor-degree-term-measured :what-would-change-this`
       ;; asked for, on the second tension it asked for.
       :degree-term (sorted-map
                     :candidates-scored (count scored)
                     :candidates-with-degree-above-zero (count (filter pos? degrees))
                     :degree-distribution (into (sorted-map) (frequencies degrees))
                     :authored-edges-in-this-repository (count (:edges wh-repo))
                     :distinguishable-from-uniform?
                     (boolean (some (fn [t]
                                      (let [real (cc/run t ctx)
                                            unif (cc/run t (assoc ctx :uniform? true))]
                                        (or (not= (cc/admitted-of real) (cc/admitted-of unif))
                                            (not= (:stop real) (:stop unif)))))
                                    cc/temperaments))
                     :verdict-is-tension-specific? true)
       :o4 (or (o4-row gate) :not-exercised-the-gate-has-not-run-yet)
       :controls (sorted-map
                  :determinism-failures (cc/determinism ctx)
                  :library-correspondence (cc/library-correspondence
                                           (fo/read-repository cc/library-root
                                                               (cc/library-sections cc/library-root)
                                                               {:kinds #{:why}}))
                  :grain-separation (cc/grain-separation ctx)
                  :citations (count acks)
                  :citations-that-do-not-read-back (cc/citations-verified acks)
                  ;; the two mutations this repository can carry.  It has no
                  ;; authored edge, so the O2/O3 edge mutants of
                  ;; `construct_cascade.clj:530-560` cannot be BUILT here -- and an
                  ;; unexercised control reads like a passing one, so they are
                  ;; declared absent by name rather than quietly omitted.
                  :negative-controls-declared [:O1-unrecorded-node
                                               :citation-cue-not-in-the-cited-span
                                               :citation-span-off-the-end-of-the-file]
                  :negative-controls-not-available-here
                  (sorted-map :O2-invented-edge :no-authored-edge-in-library-ants
                              :O3-dropped-edge :no-authored-edge-in-library-ants)
                  :negative-controls
                  (let [cascade (get-in runs [(:id cc/budgeted-temperament) :cascade])
                        ack (first acks)
                        row (fn [control exercised? caught? detail]
                              (sorted-map :control control :exercised? (boolean exercised?)
                                          :caught? (boolean caught?) :detail detail))]
                    [(row :O1-unrecorded-node true
                          (not (fo/o1-nodes-recorded
                                (update cascade :nodes conj :not-a-pattern/forged)))
                          "a node in neither selected, addedByOrganise nor admittedBy")
                     (row :citation-cue-not-in-the-cited-span (some? ack)
                          (and (some? ack)
                               (seq (cc/citations-verified
                                     [(assoc ack :cue "no pattern in this library says this")])))
                          (pr-str (:file ack)))
                     (row :citation-span-off-the-end-of-the-file (some? ack)
                          (and (some? ack)
                               (seq (cc/citations-verified [(assoc ack :lines [999999 1000000])])))
                          (pr-str (:file ack)))]))))))

(defn require-pass!
  "The failures that stop this run.  NOTE what is NOT here: `:LA3`'s
   `:constructor-admitted-nothing`.  Under `:widen-to-the-marginal-gain-floor`
   the ants constructor admits nothing at all -- every candidate scores
   match*alpha <= 0.9, below epsilon 1.0, because the degree term is zero over a
   repository with no authored edges.  That is the RESULT here, not a defect, and
   turning it into an exception would be the constructor refusing to report its
   own thinness."
  [result]
  (let [failures
        (concat
         (for [[law holds?] (get-in result [:find :laws]) :when (not holds?)]
           {:where :find :finding :law-fails :law law})
         (when (get-in result [:find :zero-mass-selected?])
           [{:where :find :finding :f4-falsifier-was-selected
             :pattern (get-in result [:find :zero-mass-pattern])}])
         (when-not (get-in result [:temperaments :holds?])
           [{:where :temperaments :finding :temperaments-differ-in-more-than-the-stop}])
         (when-not (:cascades-differ? result)
           [{:where :runs :finding :two-stopping-rules-one-cascade}])
         (for [[id row] (:runs result)
               [law holds?] (:laws row) :when (not holds?)]
           {:where :runs :finding :law-fails :law law :temperament id})
         (for [[id row] (:runs result) :when (empty? (get-in row [:cascade :members]))]
           {:where :runs :finding :empty-cascade :temperament id})
         (for [r (get-in result [:controls :grain-separation])]
           {:where :controls :finding :grain-leak :rule r})
         (for [d (get-in result [:controls :determinism-failures])]
           (assoc d :where :controls))
         (for [c (get-in result [:controls :citations-that-do-not-read-back])]
           {:where :controls :finding :citation-does-not-read-back :citation c})
         (for [c (get-in result [:controls :negative-controls])
               :when (not (and (:exercised? c) (:caught? c)))]
           {:where :controls
            :finding (if (:exercised? c) :mutation-slipped :mutation-not-exercised)
            :control (:control c)}))]
    (when (seq failures)
      (throw (ex-info "construct-ants-cascade: law or control failed"
                      {:finding (or (:finding (first failures)) :unknown)
                       :failures (vec failures)})))
    result))

(defn -main [& _]
  (try
    (let [result (require-pass! (report))]
      (spit report-path (with-out-str (pprint/pprint result)))
      (println (format "library/ants: %d patterns, %d authored @why edges, %d @why+@how; read-digest %s"
                       (get-in result [:as-of :patterns])
                       (get-in result [:as-of :authored-why-edges])
                       (get-in result [:as-of :authored-why-how-edges])
                       (subs (get-in result [:as-of :read-digest]) 0 8)))
      (println (format "tension %s: seed %d (F1-F4 %s), candidates %d, clause hits %s"
                       (name (:id tension))
                       (get-in result [:find :seed-size])
                       (pr-str (get-in result [:find :laws]))
                       (get-in result [:find :candidates])
                       (pr-str (get-in result [:tension :clause-hits]))))
      (println (format "F4 falsifier %s selected? %s"
                       (get-in result [:find :zero-mass-pattern])
                       (get-in result [:find :zero-mass-selected?])))
      (doseq [[id row] (:runs result)]
        (println (format "  %-34s stop %-38s members %d, O1-O3 %s"
                         (name id) (pr-str (:stop row)) (:members row) (pr-str (:laws row))))
        (println (format "      ordered (most precedent first): %s"
                         (pr-str (get-in row [:cascade :ordered])))))
      (let [d (:degree-term result)]
        (println (format "degree term over library/ants: %d of %d scored candidates above zero (%s); distinguishable from uniform = %s"
                         (:candidates-with-degree-above-zero d) (:candidates-scored d)
                         (pr-str (:degree-distribution d))
                         (:distinguishable-from-uniform? d))))
      (println (format "O4 over the gate: %s" (pr-str (:o4 result))))
      (println (format "controls: %d citations re-read, %d unreadable; grain leaks %d; %d mutations declared, %d slipped; %s unavailable here"
                       (get-in result [:controls :citations])
                       (count (get-in result [:controls :citations-that-do-not-read-back]))
                       (count (get-in result [:controls :grain-separation]))
                       (count (get-in result [:controls :negative-controls-declared]))
                       (count (remove #(and (:exercised? %) (:caught? %))
                                      (get-in result [:controls :negative-controls])))
                       (pr-str (vec (keys (get-in result [:controls :negative-controls-not-available-here]))))))
      (println (format "wrote %s" report-path))
      (println "construct-ants-cascade: PASS exit-convention=0-pass/1-fail")
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println "construct-ants-cascade: FAIL" (ex-message e))
      (pprint/pprint (ex-data e))
      (shutdown-agents)
      (System/exit 1))))
