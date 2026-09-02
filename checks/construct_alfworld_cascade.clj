(ns construct-alfworld-cascade
  "The CONSTRUCTOR of LA1c-restatement.md §4 run over `futon3c/library/alfworld`
   -- worklist row `:LA6` (futon3/holes/labs/library-contract/worklist.edn), the
   EXTERNAL-BENCHMARK domain of P-validated-R5.md:685-694.

   WHY THIS DOMAIN IS DIFFERENT FROM THE THREE BEFORE IT.  Snatch (`:LA3`), Ants
   (`:LA4`) and Zaif (`:LA5`) are all libraries written inside this line of work.
   `library/alfworld` is not: its ten patterns were written on 2026-02-20
   (futon3c 27136615, Joseph Corneli, mission
   `futon3c/holes/missions/M-alfworld-pattern-discovery.md`) by an agent playing
   ALFWorld games, months before the constructor existed and with no knowledge of
   it.  Nothing in them was shaped to be selectable.  That is the whole value of
   the domain and it is why the row calls it a stretch.

   WHAT THIS RUN CAN MEASURE, AND WHAT IT CANNOT.

   It CANNOT test the architecture on a repository that has a standsOn relation.
   All ten patterns carry ZERO `@why`, ZERO `@how` and ZERO `@see-also`, so the
   degree term is identically zero here exactly as it is over `library/ants`, and
   `decisions.edn :alfworld-standson-has-no-licensed-author` records why this
   lane may not repair that: a `@why` is the author's causal claim, README-flexiarg
   §5a and `decisions.edn :spider-editorial-standing` confine this lane to `@how`
   and `@see-also` and to PROPOSING `@why`, and P-validated-R5 law O2 forbids
   deriving the edges from similarity.  So the repository half of the external
   benchmark is NOT delivered by this file and is not claimed to be.

   It CAN measure the one thing `:LA5` left open and named as this row's own
   work.  LA5's result (worklist.edn `:LA5 :evidence :the-result`) is that the
   constructor's reach over a library is bounded by how antecedents are WRITTEN:
   `antecedent-holds?` (construct_cascade.clj:165-171) needs the IF and the
   HOWEVER each to acknowledge a clause, by literal cue substring, and two zaif
   patterns that answered LA5's tension almost word for word were passed over
   because their IFs were written in general terms.  Whether that bound is a
   property of the CONSTRUCTOR or of the libraries it had been run on could not
   be told apart there, because all three libraries were written by the same
   hand inside the same project.  Here they were not.  So the reach measurement
   below is pre-registered (`expected-to-fire`) and reported as a confusion
   matrix against what actually fired.

   WHAT IS HERE, and what is deliberately NOT.

   Not here: a second copy of `acknowledgements`, `match`, `antecedent-holds?`,
   `score`, `ranked`, the policy-grain rules, the temperaments, `initial-state`,
   `run` or `cascade-of`.  All come from `construct_cascade.clj` unchanged;
   `tension` is the one thing rebound, and it is `^:dynamic` there for that.

   Here: the ALFWorld Tension and its authored cues, the F4 falsifier named
   before the run, the pre-registered reach expectation, and the artefact
   `checks/alfworld-cascade.edn`.

   THE REPOSITORY IS READ IN PLACE, ACROSS A REPO BOUNDARY.  `library-root` below
   is `../futon3c/library`, not a copy imported into futon3.  Reading in place is
   what keeps the ten patterns owned by the checkout and the author that hold
   them; importing them would make this lane the apparent source of files it did
   not write.  `find_organise.clj:115-137` derives an entry's `:file` from
   `library-root` so that `citations-verified` (construct_cascade.clj:465) slurps
   where the file actually is; before `:LA6` that path was the literal string
   \"library/...\" and a citation from another checkout could not have read back.
   For `library-root` \"library\" the derived string is unchanged, which is what
   the byte-identical regeneration of `construct-cascade.edn`, `ants-cascade.edn`
   and `zaif-cascade.edn` checks."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [construct-cascade :as cc]
            [find-organise :as fo]))

(def library-root
  "futon3c's library, read in place.  Relative to the futon3 checkout, which is
   where every recipe in checks/README.md runs the constructors from."
  "../futon3c/library")

(def sections [:alfworld])
(def report-path "checks/alfworld-cascade.edn")

;; ---------------------------------------------------------------------------
;; the tension
;; ---------------------------------------------------------------------------
;; Same discipline as `construct_cascade.clj:88-100` and
;; `construct_ants_cascade.clj:64-84`: a clause carries the text it is a clause
;; of, the record that states it, and the literal CUES an author wrote for it.
;;
;; THE CONFOUND, recorded here rather than discovered by a reader, and it is
;; WEAKER here than in the two rows before it.  In `:LA3` and `:LA4` the cues
;; were written by an author who had read the antecedents they would be matched
;; against.  That is true again -- there are ten patterns and reading them is how
;; one learns the domain.  What is NOT true here is the other half of the
;; confound: the PATTERNS were written by someone else, months earlier, for a
;; different purpose, so they cannot have been shaped to the cues.  Only one side
;; of the fit is free.  The guards are F4 below, named before the run, and the
;; fourth clause, which is left in with cues that hit NOTHING.

(def tension
  {:id :the-step-budget-is-spent-finding-not-doing
   :statement "ALFWorld scores a task by steps taken, and the ten patterns of this library agree between them that the steps are not spent on the task: execution is fixed at six steps for a pick-clean-then-place and cannot be reduced, so the variable is the number of failed searches before the successful one. Every location check costs exactly one step whether it succeeds or fails, there is no scan-the-room command, and a room offers twenty or more places to look. A step is also lost when the command typed is not one the engine admits. And when search does stall -- the failure the library itself calls the only one that matters -- the agent has nobody to ask: the mission that commissioned these patterns asked for coordination triggers and the library has none."
   :source "futon3c/holes/missions/M-alfworld-pattern-discovery.md:1-47; futon3c/library/alfworld/search-dominates-execution.flexiarg:14; single-carry-economy.flexiarg:14; object-location-priors.flexiarg:12; plan-then-execute.flexiarg:46"
   :clauses
   [{:id :every-location-check-costs-a-step
     :text "Navigation is the ONLY way to discover what's at a location. There is no scan-room command. Every location check costs exactly one step, whether it succeeds or fails, and the variable in the score is the number of failed searches before the successful one."
     :source "futon3c/library/alfworld/single-carry-economy.flexiarg:14; search-dominates-execution.flexiarg:14"
     :cues ["wastes steps" "waste steps" "costs exactly one step" "zero progress"
            "failed searches" "number of failed" "navigate"]}
    {:id :the-room-offers-too-many-places-to-look
     :text "the room has 20+ possible locations and exhaustive receptacle-by-receptacle search is what the step budget cannot afford"
     :source "futon3c/library/alfworld/object-location-priors.flexiarg:12; systematic-search-fallback.flexiarg:12"
     :cues ["possible locations" "exhaustive" "receptacles remaining" "every receptacle"
            "20+" "15+" "many receptacles" "many possible"]}
    {:id :a-step-is-lost-when-the-command-is-not-admitted
     :text "the action typed is built from the task description's wording and the engine's grammar is narrower than that, so the step is spent on a command that is not admitted"
     :source "futon3c/library/alfworld/admissible-commands-are-ground-truth.flexiarg:11-14"
     :cues ["admissible" "action grammar" "paraphrase" "not a literal command"]}
    ;; THE NULL CLAUSE, kept in on purpose.  The mission that commissioned this
    ;; library asked for "Coordination triggers (when to bell for help)" as one
    ;; of its five pattern classes (M-alfworld-pattern-discovery.md:47) and the
    ;; library delivered none -- so this clause of the tension is real, is
    ;; sourced, and no antecedent in the repository acknowledges it.  A cue set
    ;; tuned to the patterns would not carry a clause that scores zero, and
    ;; `:clause-hits` in the report is where that shows.
    {:id :when-search-stalls-there-is-nobody-to-ask
     :text "search stalls -- the plan is clear but the object cannot be found, the failure the library calls the only one that matters -- and the mission's fifth pattern class, coordination triggers for when to bell for help, was never written"
     :source "futon3c/holes/missions/M-alfworld-pattern-discovery.md:47; futon3c/library/alfworld/plan-then-execute.flexiarg:46"
     :cues ["bell for help" "ask for help" "coordination" "another agent" "escalate"]}]})

(def zero-mass-pattern
  "F4's falsifier, named BEFORE the run: the pattern of this repository the
   tension must NOT select.  `alfworld/closed-containers-need-opening` is about
   what to do once you have ARRIVED somewhere -- whether a container's contents
   are visible without an `open` first -- which is interaction mechanics at a
   location, not the economics of choosing which locations to spend steps on.
   Its `+ THEN:` prescribes an open-before-take reflex and would change no search
   order.  A run that selected it would mean the cues are reading the domain's
   vocabulary rather than this tension's clauses."
  :alfworld/closed-containers-need-opening)

(def expected-to-fire
  "PRE-REGISTERED, before the run, and this is the measurement `:LA5` asked for.
   A reader who has read the tension and the ten antecedents, and who is NOT
   applying `antecedent-holds?`, would say these six answer it: four are about
   where to spend a search step, one is about not paying for the same discovery
   twice, and one is about the step lost to an inadmissible command.  The other
   four are about parsing the task, identifying the room, choosing the appliance
   for a transformation verb, and opening containers.

   This set is a JUDGEMENT and it is written down so it can be wrong.  What the
   report measures is the disagreement between it and what actually fired, in
   both directions: a pattern a reader expects and the constructor misses is
   LA5's failure mode recurring on a library written by someone else, and a
   pattern the constructor fires that no reader expected is the opposite
   finding."
  #{:alfworld/object-location-priors
    :alfworld/systematic-search-fallback
    :alfworld/search-dominates-execution
    :alfworld/single-carry-economy
    :alfworld/remember-what-you-see
    :alfworld/admissible-commands-are-ground-truth})

;; ---------------------------------------------------------------------------
;; the cascade, as data
;; ---------------------------------------------------------------------------

(defn cascade-data
  "The constructed cascade as an actuator would need it -- identical in shape to
   `construct_ants_cascade.clj:126-142`, and NOTE that no actuator reads this one:
   ALFWorld is not installed in this checkout (no `futon3c/.venv-alfworld`, no
   ALFWORLD_DATA), so nothing here is played.  The artefact is the constructed
   policy as data, which is what LA1c §7 asks for and is reviewable without a
   run."
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
;; antecedent reach -- the LA5 follow-up
;; ---------------------------------------------------------------------------

(defn reach-rows
  "Per pattern: which clauses its IF acknowledges, which its HOWEVER
   acknowledges, and therefore whether `antecedent-holds?`.  This is the table a
   reviewer needs to tell LA5's failure mode from a genuine non-answer -- a
   pattern with clause hits in the HOWEVER and none in the IF was passed over for
   how its antecedent is WRITTEN, not for what it says."
  [repo]
  (into (sorted-map)
        (for [[id entry] (:entries repo)]
          (let [acks (cc/acknowledgements entry)
                by-block (fn [b] (into (sorted-set) (map :clause) (filter #(= b (:block %)) acks)))]
            [id (sorted-map
                 :if-clauses (by-block :if)
                 :however-clauses (by-block :however)
                 :match (cc/match entry)
                 :fires? (boolean (cc/antecedent-holds? entry))
                 :expected-to-fire? (contains? expected-to-fire id))]))))

(defn reach-summary [rows]
  (let [fired (into (sorted-set) (map key) (filter #(:fires? (val %)) rows))
        expected expected-to-fire
        missed (into (sorted-set) (remove fired expected))
        unexpected (into (sorted-set) (remove expected fired))]
    (sorted-map
     :expected-to-fire (into (sorted-set) expected)
     :fired fired
     :agreed (into (sorted-set) (filter fired expected))
     :expected-but-did-not-fire missed
     :fired-but-not-expected unexpected
     ;; LA5's failure mode, stated as a checkable property rather than a story:
     ;; an expected pattern whose HOWEVER acknowledges a clause and whose IF
     ;; acknowledges none was passed over for how its antecedent is written.
     :missed-on-the-if-alone
     (into (sorted-set)
           (filter (fn [id]
                     (let [r (get rows id)]
                       (and (empty? (:if-clauses r)) (seq (:however-clauses r)))))
                   missed))
     :missed-on-the-however-alone
     (into (sorted-set)
           (filter (fn [id]
                     (let [r (get rows id)]
                       (and (seq (:if-clauses r)) (empty? (:however-clauses r)))))
                   missed))
     :missed-acknowledging-nothing
     (into (sorted-set)
           (filter (fn [id]
                     (let [r (get rows id)]
                       (and (empty? (:if-clauses r)) (empty? (:however-clauses r)))))
                   missed)))))

;; ---------------------------------------------------------------------------
;; the report
;; ---------------------------------------------------------------------------

(defn report []
  (binding [cc/tension tension]
    (let [why-repo (fo/read-repository library-root sections {:kinds #{:why}})
          wh-repo (fo/read-repository library-root sections {:kinds #{:why :how}})
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
          rows (reach-rows why-repo)]
      (sorted-map
       :as-of (sorted-map
               :library-root library-root
               :read-in-place-not-imported? true
               :sections (vec sections)
               :patterns (count (:patterns why-repo))
               :read-digest (cc/read-digest wh-repo)
               :authored-why-edges (count (:edges why-repo))
               :authored-why-how-edges (count (:edges wh-repo))
               :authored-see-also-edges (count (:edges (fo/read-repository
                                                        library-root sections
                                                        {:kinds #{:see-also}})))
               :dangling-edges (count (:dangling why-repo))
               :why-acyclic? (:acyclic? why-repo)
               :provenance "futon3c 27136615, 2026-02-20, Joseph Corneli -- written by an agent playing ALFWorld games, months before this constructor existed and with no knowledge of it.")
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
                 :null-clause-scored-zero?
                 (zero? (count (filter (fn [e]
                                         (some #(= :when-search-stalls-there-is-nobody-to-ask
                                                   (:clause %))
                                               (cc/acknowledgements e)))
                                       (vals (:entries why-repo)))))
                 :match-distribution (into (sorted-map) (frequencies (vals matches))))
       :find (sorted-map
              :selected (vec (:selected found))
              :seed-size (count seed)
              :candidates (- (count matches) (count seed))
              :zero-mass-pattern zero-mass-pattern
              :zero-mass-selected? (contains? (set (:selected found)) zero-mass-pattern)
              :laws (into (sorted-map)
                          (for [[law holds?] fo/find-laws] [law (boolean (holds? find-row))])))
       :antecedent-reach (sorted-map :rows rows :summary (reach-summary rows))
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
       :degree-term (sorted-map
                     :candidates-scored (count scored)
                     :candidates-with-degree-above-zero (count (filter pos? degrees))
                     :degree-distribution (into (sorted-map) (frequencies degrees))
                     :authored-edges-in-this-repository (count (:edges wh-repo))
                     :zero-by-construction-not-by-luck? (zero? (count (:edges wh-repo)))
                     :distinguishable-from-uniform?
                     (boolean (some (fn [t]
                                      (let [real (cc/run t ctx)
                                            unif (cc/run t (assoc ctx :uniform? true))]
                                        (or (not= (cc/admitted-of real) (cc/admitted-of unif))
                                            (not= (:stop real) (:stop unif)))))
                                    cc/temperaments)))
       ;; No authority gate exists for ALFWorld and the benchmark is not
       ;; installed here, so O4 is UNEXERCISED and says so by name rather than
       ;; being quietly omitted -- the same rule `:LA5`'s review applied to the
       ;; zaif O4 row.
       :o4 :not-exercised-no-authority-gate-and-alfworld-is-not-installed
       :controls (sorted-map
                  :determinism-failures (cc/determinism ctx)
                  ;; deliberately over futon3's OWN library: this control checks
                  ;; the constructor's READER against the patterns the three
                  ;; policy-grain rules cite, and those rules cite futon3
                  ;; patterns.  It is not a claim about library/alfworld.
                  :library-correspondence (cc/library-correspondence
                                           (fo/read-repository cc/library-root
                                                               (cc/library-sections cc/library-root)
                                                               {:kinds #{:why}}))
                  :grain-separation (cc/grain-separation ctx)
                  :citations (count acks)
                  :citations-that-do-not-read-back (cc/citations-verified acks)
                  :citations-resolve-across-the-repo-boundary?
                  (boolean (and (seq acks)
                                (every? #(str/starts-with? (:file %) "../futon3c/library/") acks)))
                  :negative-controls-declared [:O1-unrecorded-node
                                               :citation-cue-not-in-the-cited-span
                                               :citation-span-off-the-end-of-the-file]
                  :negative-controls-not-available-here
                  (sorted-map :O2-invented-edge :no-authored-edge-in-library-alfworld
                              :O3-dropped-edge :no-authored-edge-in-library-alfworld)
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
  "The failures that stop this run.  NOTE what is NOT here, and why.

   Not here: any assertion about the SIZE of the reach agreement.  Whether the
   pre-registered six fired is the measurement, not the acceptance bar; asserting
   it would turn a pre-registration into a target and this file would then be
   tuned until it passed.  `:antecedent-reach` is reported and judged by a
   reader.

   Not here: `:empty-cascade` as an error when the degree term is zero.  Over a
   library with no authored edges the constructor can admit nothing at all --
   that is `:LA4`'s recorded result, not a defect -- so an empty cascade is
   reported and does not throw.  What DOES throw is a law violation, a control
   failure, a citation that does not read back, and F4."
  [result]
  (let [failures
        (concat
         (for [[law holds?] (get-in result [:find :laws]) :when (not holds?)]
           {:where :find :finding :law-fails :law law})
         (when (get-in result [:find :zero-mass-selected?])
           [{:where :find :finding :f4-falsifier-was-selected
             :pattern (get-in result [:find :zero-mass-pattern])}])
         (when-not (get-in result [:tension :null-clause-scored-zero?])
           [{:where :tension :finding :null-clause-was-acknowledged
             :detail "the anti-tuning guard is only a guard while it hits nothing"}])
         (when-not (get-in result [:temperaments :holds?])
           [{:where :temperaments :finding :temperaments-differ-in-more-than-the-stop}])
         (for [[id row] (:runs result)
               [law holds?] (:laws row) :when (not holds?)]
           {:where :runs :finding :law-fails :law law :temperament id})
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
      (throw (ex-info "construct-alfworld-cascade: law or control failed"
                      {:finding (or (:finding (first failures)) :unknown)
                       :failures (vec failures)})))
    result))

(defn -main [& _]
  (try
    (let [result (require-pass! (report))]
      (spit report-path (with-out-str (pprint/pprint result)))
      (println (format "%s: %d patterns, %d @why, %d @why+@how, %d @see-also; read-digest %s"
                       library-root
                       (get-in result [:as-of :patterns])
                       (get-in result [:as-of :authored-why-edges])
                       (get-in result [:as-of :authored-why-how-edges])
                       (get-in result [:as-of :authored-see-also-edges])
                       (subs (get-in result [:as-of :read-digest]) 0 8)))
      (println (format "tension %s: seed %d (F1-F4 %s), candidates %d"
                       (name (:id tension))
                       (get-in result [:find :seed-size])
                       (pr-str (get-in result [:find :laws]))
                       (get-in result [:find :candidates])))
      (println (format "clause hits %s; null clause scored zero? %s"
                       (pr-str (get-in result [:tension :clause-hits]))
                       (get-in result [:tension :null-clause-scored-zero?])))
      (println (format "F4 falsifier %s selected? %s"
                       (get-in result [:find :zero-mass-pattern])
                       (get-in result [:find :zero-mass-selected?])))
      (let [s (get-in result [:antecedent-reach :summary])]
        (println "antecedent reach (pre-registered vs actual):")
        (println (format "  expected %d, fired %d, agreed %d"
                         (count (:expected-to-fire s)) (count (:fired s)) (count (:agreed s))))
        (println (format "  expected but did not fire: %s" (pr-str (:expected-but-did-not-fire s))))
        (println (format "    of those, missed on the IF alone: %s" (pr-str (:missed-on-the-if-alone s))))
        (println (format "    of those, missed on the HOWEVER alone: %s" (pr-str (:missed-on-the-however-alone s))))
        (println (format "    of those, acknowledging nothing: %s" (pr-str (:missed-acknowledging-nothing s))))
        (println (format "  fired but not expected: %s" (pr-str (:fired-but-not-expected s)))))
      (doseq [[id row] (:runs result)]
        (println (format "  %-34s stop %-38s members %d, O1-O3 %s"
                         (name id) (pr-str (:stop row)) (:members row) (pr-str (:laws row))))
        (println (format "      ordered (most precedent first): %s"
                         (pr-str (get-in row [:cascade :ordered])))))
      (let [d (:degree-term result)]
        (println (format "degree term: %d of %d scored candidates above zero (%s); zero by construction = %s; distinguishable from uniform = %s"
                         (:candidates-with-degree-above-zero d) (:candidates-scored d)
                         (pr-str (:degree-distribution d))
                         (:zero-by-construction-not-by-luck? d)
                         (:distinguishable-from-uniform? d))))
      (println (format "O4: %s" (pr-str (:o4 result))))
      (println (format "controls: %d citations re-read, %d unreadable, resolve across repo boundary = %s; grain leaks %d; %d mutations declared, %d slipped; %s unavailable here"
                       (get-in result [:controls :citations])
                       (count (get-in result [:controls :citations-that-do-not-read-back]))
                       (get-in result [:controls :citations-resolve-across-the-repo-boundary?])
                       (count (get-in result [:controls :grain-separation]))
                       (count (get-in result [:controls :negative-controls-declared]))
                       (count (remove #(and (:exercised? %) (:caught? %))
                                      (get-in result [:controls :negative-controls])))
                       (pr-str (vec (keys (get-in result [:controls :negative-controls-not-available-here]))))))
      (println (format "wrote %s" report-path))
      (println "construct-alfworld-cascade: PASS exit-convention=0-pass/1-fail")
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println "construct-alfworld-cascade: FAIL" (ex-message e))
      (pprint/pprint (ex-data e))
      (shutdown-agents)
      (System/exit 1))))
