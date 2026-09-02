(ns how-witness-two-layer-calibration
  "A DERIVED behavioural check for ONE attested `@how` edge — the sixth priced,
   after `how-witness-heartbeat` (:L4), `how-witness-split-transport` (slice 1),
   `how-witness-declare-conditioning` (slice 2), `how-witness-no-self-certification`
   (slice 3), `how-witness-scheduled-observer` (slice 4) and
   `how-witness-status-gated-belief` (slice 5). Worklist item :L10 slice 6; the
   step named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112.

   The edge under test:

     aif/two-layer-calibration
       @how aif/off-continuity-null-discriminates

   Attested at library/aif/attestations.edn record 26, rung 2,
   `:state [:attested-by \"claude-15\"]`, reviewed by claude-15 2026-08-30 with
   the note \"@how warranted: the null-discriminates check is how L1 earns its
   keep; evidence records the 2.55x live result\". `:cited` is the SOURCE
   pattern's own THEN clause (flexiarg:29-30, \"**L1 = G-vs-G dynamics**: cheap,
   every cycle, labelled never-value-evidence; it earns its keep as a
   consistency/sanity layer\"), and the single `:evidence` record is a `:via
   :text` hit whose `:excerpt` is the source's canonical-instance line. The
   attestation therefore quotes the source pattern to itself twice over: nothing
   in it reads the method pattern, and nothing reads a site. Slice 3's edge had
   the same shape; this is the second of six.

   Same method as the five before it: a relation run FORWARD over stated facts,
   with controls and a mirror, so an empty witness cannot be confused with a
   relation that cannot see.

   THE SITE. Both patterns name the FutonZero G-SIM WM-pilot arc of 2026-06-11
   and both are found where they say. It spans three repositories: the two
   forward models and the dual-prediction recompute in futon2 (d8e1fee), the
   pilot, the flight record and the canonical calibration reader in futon3c
   (de776e18), and the L2 reward gate plus the charter ledger in futon0
   (0de06f6). The recorded results live in futon3c/holes/PILOTS-LOG.md and
   holes/PILOT-STOCK-TAKE-001.md. NOT :no-site-to-check-against.

   Facts below are read at futon2 d8e1fee, futon3c de776e18, futon0 0de06f6.

   INVENTIONS. I1 (obligations are the THEN's clauses, one row each), I2
   (grounding), I3 (a meaning for \"carries out\"), I4 (site adjudication is a
   read, not a computation) recur from :L4 unchanged. I6 from slice 2 (one row
   per THEN clause when the THEN is prose, each citing the line it sits on)
   applies to both flexiargs. I7 from slice 3 (contradiction is not silence)
   carries the two `:s5` rows. One is specific to this edge:

     I11. PLUMBING IS NOT DISCRIMINATION. The method's THEN has two halves —
          SELECT an off-continuity begin (:32-33), and READ the gap between the
          two models as a discriminant (:34-36). The site implements neither in
          code. What it implements, thoroughly, is the PLUMBING between them: a
          second `rank-actions` under `*effects-mode* :constant`, attached to
          every ranked entry, carried into the pilot's begin state, copied to
          close, folded into the flight record's `:counterfactual` organ, and
          normalized onto every gamma-frame by the calibration reader — five
          repositories' worth of a number that no verdict ever compares. A
          mechanism that CARRIES the constant prediction without ever reading
          the difference is `:dual-prediction-plumbing`, not `:discrimination`.
          This is the invention the verdict turns on: drop I11 and the edge
          attests at 2 of 6 rather than 1, and `plumbing-control` reports that
          number."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/aif/two-layer-calibration.flexiarg THEN (:29-33), one row per clause
;; (I1/I6). The L1 sentence asserts three independent properties and is split at
;; them, because the finding is that they do not all hold.
(def source-obligations
  [[:s1 29 "name and keep both layers explicitly"]
   [:s2 29 "L1 = G-vs-G dynamics: cheap, every cycle"]
   [:s3 29 "L1 is labelled never-value-evidence"]
   [:s4 30 "L1 earns its keep as a consistency/sanity layer"]
   [:s5 30 "L2 = outcome-vs-prediction against WITNESSED outcomes the model did not produce (closure-folds, three-witness certificates, attested real functioning), scarce, gated behind the reward discipline; only L2 clears value"]
   [:s6 33 "forbid L1's cheap pass from being reported as L2 clearance"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/aif/off-continuity-null-discriminates.flexiarg THEN (:32-36). On the
;; record so the method's own content is visible; the relation uses
;; `method-instanceo` below, which is the property the site can be read for (I3).
(def method-obligations
  [[:m1 32 "pick an OFF-continuity begin — a below-cap target where the two models predict DIFFERENT values at the current state"]
   [:m2 33 "fly a tight null there — a content-advance moving no counted hole; settled window, two scans agreeing within epsilon"]
   [:m3 34 "the realised lands on the state-sensitive prediction and misses the constant by exactly the off-continuity gap"]
   [:m4 35 "the null IS the discriminating measurement; the state-blind model's error is the state it cannot see"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — kind is :discrimination when the mechanism
;; puts the two models' predictions for the SAME pair beside each other and
;; reads the difference, and names what it is otherwise (I11).
(def site-mechanisms
  [;; ---- the two models, and the continuity point between them -------------
   ;; The dial that selects state-sensitive vs state-blind. The docstring names
   ;; `prior == value` as the reason the constant model was unfalsifiable — the
   ;; source pattern's context clause, written into the code it is about.
   [:effects-mode-dial      :continuity-point  "futon2 src/futon2/aif/forward_model.clj:69-82"]
   ;; 1.0 under :constant, else x/continuity-point clamped — so the two models
   ;; coincide exactly at the continuity point. The method's central concept,
   ;; implemented.
   [:sensitivity-continuity :continuity-point  "futon2 src/futon2/aif/forward_model.clj:84-92"]
   ;; "normalized so N=3 remains the constant-model continuity point" — the
   ;; open-hole-count 3 the method's context clause names, in the source.
   [:advance-mission-continuity :continuity-point "futon2 src/futon2/aif/forward_model.clj:94-110"]
   ;; The other continuity point: intrinsic-value 0.5 for :address-sorry.
   [:address-sorry-continuity :continuity-point "futon2 src/futon2/aif/forward_model.clj:128-135"]

   ;; ---- the plumbing: the constant prediction, carried everywhere ---------
   ;; A whole second `rank-actions` under :constant, keyed by [type target] and
   ;; attached as :G-constant to every ranked entry — every candidate, every
   ;; tick. "Pure recompute, no second scan." The dual prediction the method
   ;; needs, produced unconditionally and cheaply.
   [:constant-mode-recompute :dual-prediction-plumbing "futon2 scripts/futon2/report/war_machine.clj:5095-5111"]
   ;; Into the pilot's begin state as :predicted-constant.
   [:pilot-begin-capture    :dual-prediction-plumbing "futon3c src/futon3c/peripheral/war_machine_pilot.clj:265-266, :385-386"]
   ;; And copied to close, twice.
   [:pilot-close-copy       :dual-prediction-plumbing "futon3c src/futon3c/peripheral/war_machine_pilot.clj:513-514, :573-574"]
   ;; The flight record's :counterfactual organ — |realised - predicted-constant|,
   ;; or `(ghost :not-yet)` when the constant prediction is absent.
   [:flight-counterfactual  :dual-prediction-plumbing "futon3c src/futon3c/aif/flight_record.clj:179-184"]
   ;; The same organ on the backfill path, whose ground carries the site's own
   ;; statement about this edge: :limitation :clean-vs-null-lived-in-prose.
   [:flight-backfill-organ  :dual-prediction-plumbing "futon3c src/futon3c/aif/flight_record.clj:330-342"]
   ;; The canonical reader normalizes :predicted-constant onto every gamma-frame.
   [:calibration-normalizer :dual-prediction-plumbing "futon3c src/futon3c/aif/calibration.clj:93"]

   ;; ---- the one discrimination in code, over an empty corpus (the mirror) --
   ;; `flight-stratification` computes :mean-error beside :mean-constant-error
   ;; per interpretation class. This IS the comparison the method asks for. It
   ;; globs `*.flight.edn` under data/repl-traces, which holds one file and it
   ;; is not one — see `not-found`. No caller outside this namespace.
   [:stratified-constant-mean :discrimination  "futon3c src/futon3c/aif/calibration.clj:315-352 (:344 :constant-error, :352 :mean-constant-error)"]

   ;; ---- the discriminations that were actually carried out: three prose rows
   ;; Turn 10 (2026-06-11), the stock-take: over 3 post-switch pairs the two
   ;; models are TIED, and the log records the correction of an earlier 2x
   ;; overclaim under the heading HONESTY.
   [:stock-take-tied        :discrimination    "futon3c holes/PILOTS-LOG.md:169-171; holes/PILOT-STOCK-TAKE-001.md"]
   ;; Turn 20 (2026-06-11): 2.55x over n=4. This is the number the source
   ;; pattern's canonical-instance cites and the attestation's :evidence excerpts.
   [:arc2-2p55x             :discrimination    "futon3c holes/PILOTS-LOG.md:66"]
   ;; Turn 23 (2026-06-12): the method itself, flown once, by hand. An
   ;; off-continuity ohc-4 null: scaled error 7.65e-5 vs constant error 0.0396.
   [:ohc4-null              :discrimination    "futon3c holes/PILOTS-LOG.md:36-38"]

   ;; ---- naming the layers -------------------------------------------------
   ;; Both layers named, in prose, in a stock-take document.
   [:stock-take-naming      :layer-naming      "futon3c holes/PILOT-STOCK-TAKE-001.md:25-26"]
   ;; The one layer label that reaches an artifact: `:gate :G-SIM` in the
   ;; emitted charter report. It names the gate, not the verdict.
   [:gate-g-sim-label       :layer-naming      "futon0 scripts/futon0/futonzero/rollout_ledger.clj:131; futon0/data/futonzero-calibration-report.edn"]
   ;; "honestly *never value evidence*" — the source's third clause, in prose.
   [:stock-take-never-value :prose             "futon3c holes/PILOT-STOCK-TAKE-001.md:25"]

   ;; ---- the L1 verdict ----------------------------------------------------
   ;; The one verdict the apparatus computes: :insufficient-evidence /
   ;; :degenerate / :calibratable, over predicted-vs-realised pairs. Nothing in
   ;; it reads :predicted-constant.
   [:calibration-report     :l1-verdict        "futon3c src/futon3c/aif/calibration.clj:260-312"]
   ;; The charter artifact copies the verdict verbatim and prints the
   ;; witness-class census in the same map.
   [:ledger-witness-census  :l1-verdict        "futon0 scripts/futon0/futonzero/rollout_ledger.clj:104-136"]
   ;; The ledger's own restraint clause, in its docstring.
   [:ledger-no-fruit-clause :prose             "futon0 scripts/futon0/futonzero/rollout_ledger.clj:5-7"]

   ;; ---- the L2 lane -------------------------------------------------------
   ;; The reward gate: witnessed class AND independent AND measured-realised AND
   ;; a numeric return. A complete, correct implementation of "gated behind the
   ;; reward discipline" — in a namespace that declares itself synthetic.
   [:reward-admissible-gate :l2-gate           "futon0 scripts/futon0/futonzero/toy_field.clj:21-44"]
   ;; The L2 referent, read: closure-folds from futon6, ch2-discharges from
   ;; futon3a, tagged :build-discharge — with :predicted, :realised and :error
   ;; hard-coded nil.
   [:closure-fold-records   :l2-referent       "futon3c src/futon3c/aif/calibration.clj:169-179 (ch2 at :198-210)"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:stock-take-naming       :s1]
   [:gate-g-sim-label        :s1]
   [:constant-mode-recompute :s2]  ; cheap and every cycle — plumbing (I11)
   [:calibration-report      :s2]
   [:stock-take-never-value  :s3]  ; prose only; see `not-found`
   [:ohc4-null               :s4]  ; the method, carried out
   [:arc2-2p55x              :s4]
   [:stock-take-tied         :s4]
   [:reward-admissible-gate  :s5]  ; discharged, and contradicted — see below
   [:ledger-witness-census   :s6]
   [:ledger-no-fruit-clause  :s6]])

;; [mechanism obligation contradiction] — I7: a clause the site actively
;; falsifies is not the same as a clause the site is silent about.
(def contradictions
  [[:reward-admissible-gate :s5
    (str "the gate is complete and correct and cannot ever see a real record. "
         "Its namespace declares itself \"deliberately pure and synthetic\" and "
         "says it \"does not model, call, or mutate the real field\" "
         "(toy_field.clj:4-6); `reward-admissible?` has no production caller — "
         "grep over futon0 scripts/ and test/ finds it called only from "
         "reward_red_team.clj:38, itself a fixture, and from two test "
         "namespaces. And the records the canonical reader DOES produce cannot "
         "satisfy it: `closure-records` (calibration.clj:169-179) emits no "
         ":independent?, no :realised-source and no :return, which is three of "
         "the gate's four conjuncts. The L2 gate and the L2 evidence are built "
         "to different shapes.")]
   [:closure-fold-records :s5
    (str "the L2 referent is loaded and then structurally excluded from the "
         "only verdict there is. :predicted, :realised and :error are hard-coded "
         "nil at calibration.clj:172-174, so a closure-fold can never enter "
         "`paired` (:273-276) and therefore never `independent` (:288-297), and "
         "`verdict` is a function of `independent` alone (:299-303). The "
         "2026-08-15 charter snapshot shows the arithmetic: 13 records at "
         ":witness-class :build-discharge, :independent-paired-count 0 "
         "(futon0/data/futonzero-calibration-report.edn).")]])

;; [what-was-looked-for where-it-was-named how-the-absence-was-established]
(def not-found
  [["the label \"never-value-evidence\" on any executable or artifact"
    "source flexiarg:29-30 — L1 is to be LABELLED never-value-evidence"
    "`grep -rn never-value-evidence` over futon0 futon1 futon1b futon2 futon3a futon3b futon3c futon4 futon5 returns exactly ONE hit: futon3c/holes/PILOTS-LOG.md:106, a prose log line from the turn that minted the pattern. No .clj, .cljs, .py, .el or .edn hit anywhere; the emitted charter report carries `:gate :G-SIM` and no layer label on `:verdict`"]
   ["any guard forbidding the L1 verdict from being reported as clearance"
    "source flexiarg:33 — \"Forbid L1's cheap pass from being reported as L2 clearance\""
    "`:calibratable` is produced at calibration.clj:303 and appears nowhere else outside calibration_test.clj:80,:107; the rollout ledger copies `:verdict` verbatim into `:cross-check` and into the report root (rollout_ledger.clj:114,:131). The only thing standing between the two layers in the artifact is adjacency: `:count-by-witness` printed beside `:verdict`. No rule, no refusal, no assertion"]
   ["a flight-witness record for the off-continuity null live-df706c45"
    "method flexiarg:44-46 — the case the method exists to produce, ~517x discrimination"
    "`ls futon3c/holes/specs/flight.witness.*` returns exactly one file, flight.witness.live-957a4836.edn — the CONTINUITY-POINT case, the method's own example of 0x discrimination from a clean move. That spec is itself hand-recorded (\"the pilot is the recorder this once; close-live-cycle! does not yet persist this shape\") and cites a gamma frame `data/repl-traces/live-957a4836-*.edn` that is not in the directory"]
   ["a below-cap target"
    "method flexiarg:32 and :48 — both the THEN and the how-to-apply require the begin be below-cap"
    "the cap was removed at futon2 cf7e538 (2026-07-16). At cf7e538^ `:advance-mission` scaled through `(sensitivity-factor open-hole-count 3 2.0)` — continuity at 3, capped at 2x = 6+ holes, exactly what the method's context clause describes. At d8e1fee it routes through `mission-value-factor`, which prefers a judge-supplied scalar clamped to [0,1] and falls back to `advance-mission-ordinal-factor`, whose docstring reads \"Unlike the former hard cap at N=6, it remains strictly monotone\". `git log --all -S` on the cap text returns that one commit"]
   ["any *.flight.edn record for the one discrimination in code to run on"
    "`flight-stratification` globs them at calibration.clj:326-331"
    "`ls futon3c/data/repl-traces/` holds exactly one file, live-zai4-wm-pilot-cycle-001.edn, and it is not a .flight.edn; `git log --all -- data/repl-traces` is one commit. The three gamma-frames that file yields carry no :predicted-constant and none is :independent? — counted by walking the file"]])

;; ---- when the three recorded discriminations happened ---------------------
;; [mechanism turn date result] — the attestation's :evidence excerpt is the
;; middle row; the method's only instance is the last.
(def discrimination-dates
  [[:stock-take-tied "Turn 10" "2026-06-11" "tied, n=3 post-switch (scaled 0.026 vs constant 0.027, winning on opposite cases)"]
   [:arc2-2p55x      "Turn 20" "2026-06-11" "2.55x, n=4 — the number the source's canonical-instance cites and the attestation excerpts"]
   [:ohc4-null       "Turn 23" "2026-06-12" "~517x, n=1, the first and only OFF-continuity null: scaled 7.65e-5 vs constant 0.0396"]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))
(defn contradictedo [s] (l/fresh [m note] (l/membero [m s note] contradictions)))

(defn uncontradictedo
  "Obligations with no contradiction row. Same idiom slice 3 used: grounded in
   the two reads recorded in `contradictions`, not asserted."
  [s]
  (l/conda [(contradictedo s) l/fail]
           [l/succeed]))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is a DISCRIMINATION: it puts
   the state-sensitive and state-blind predictions of the same pair beside each
   other and reads the difference. Carrying the constant prediction without ever
   comparing it is plumbing, not the method (I11, I3)."
  [m]
  (mechanism-kindo m :discrimination))

(defn carried-out-by-methodo
  "Source obligation `s` is carried out BY THE METHOD at the site, via `m`, and
   is not contradicted there (I7)."
  [s m]
  (l/all (source-obligationo s)
         (dischargeso m s)
         (method-instanceo m)
         (uncontradictedo s)))

(defn witness
  "The edge holds behaviourally iff EVERY source obligation appears here."
  []
  (l/run* [s m] (carried-out-by-methodo s m)))

;; ---- controls ------------------------------------------------------------
(defn positive-control
  "Same relation with the method-instance conjunct dropped. Must be NON-empty,
   or an empty `witness` would mean only that the relation cannot see."
  []
  (l/run* [s m] (l/all (source-obligationo s) (dischargeso m s))))

(defn plumbing-control
  "The witness with I11 dropped — :dual-prediction-plumbing admitted as an
   instance of the method. Reports what the verdict would be if carrying the
   constant prediction counted as reading it."
  []
  (l/run* [s m]
    (l/all (source-obligationo s)
           (dischargeso m s)
           (l/conde
            [(mechanism-kindo m :discrimination)]
            [(mechanism-kindo m :dual-prediction-plumbing)])
           (uncontradictedo s))))

(defn mirror
  "The one discrimination the site implements in CODE, asked what it carries
   out. Must be EMPTY: `flight-stratification` computes :mean-error beside
   :mean-constant-error per class — structurally the method — over a corpus of
   zero *.flight.edn records, and has no caller. A witness that counted it would
   attest the edge on a comparison that has never had an input."
  []
  (l/run* [s] (carried-out-by-methodo s :stratified-constant-mean)))

(defn -main [& _]
  (let [w (witness) p (positive-control) c (plumbing-control) m (mirror)
        n-src (count source-obligations)
        distinct-obs (fn [rows] (count (distinct (map first rows))))]
    (println "edge: aif/two-layer-calibration")
    (println "  @how aif/off-continuity-null-discriminates")
    (println "site: futon2 d8e1fee + futon3c de776e18 + futon0 0de06f6 (FutonZero G-SIM, WM-pilot 2026-06-11)")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println (format "positive control (any mechanism, must be non-empty): %d of %d"
                     (distinct-obs p) n-src))
    (doseq [b p] (println "  " b))
    (println (format "plumbing control (I11 dropped): %d of %d" (distinct-obs c) n-src))
    (println "mirror (must be empty):" m)
    (println)
    (println "contradictions — obligations the site falsifies rather than omits:")
    (doseq [[mech s _] contradictions] (println "  " mech s))
    (println "recorded discriminations, in order:")
    (doseq [[mech turn at result] discrimination-dates]
      (println "  " mech turn at "--" result))
    (println "not found:")
    (doseq [[what where _] not-found] (println "  " what "--" where))
    (println)
    (println (cond
               (not (seq p))
               "INCONCLUSIVE — the relation finds nothing at all; the fact rows are broken."
               (seq m)
               "INCONCLUSIVE — the mirror is not silent; the relation is too weak."
               (= (distinct-obs w) n-src)
               "ATTESTED — every source obligation is carried out by the method."
               (seq w)
               (format (str "NOT ATTESTED — the method carries out %d of %d obligations. "
                            "%d of %d are discharged at this site, so the overstatement is "
                            "ownership, not absence: the layers are named in a stock-take, "
                            "L1's cheapness is discharged by plumbing that never compares "
                            "(I11), L1's never-value-evidence LABEL exists in one prose log "
                            "line and nowhere executable, the L2 lane is discharged by a gate "
                            "that declares itself synthetic and cannot read the records the "
                            "canonical reader produces, and the forbidding clause is "
                            "discharged by adjacency in a report rather than by any rule. "
                            "The one obligation the method does carry out is s4.")
                       (distinct-obs w) n-src (distinct-obs p) n-src)
               :else
               (format "NOT ATTESTED — the method carries out 0 of %d obligations." n-src)))))
