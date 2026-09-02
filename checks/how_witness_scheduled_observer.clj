(ns how-witness-scheduled-observer
  "A DERIVED behavioural check for ONE attested `@how` edge — the fourth priced
   under worklist item :L10 (slice 4), after `how-witness-split-transport`
   (slice 1), `how-witness-declare-conditioning` (slice 2) and
   `how-witness-no-self-certification` (slice 3), all generalising :L4's
   `how-witness-heartbeat`. The step is named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112.

   The edge under test:

     aif/scheduled-observer-entrypoint  @how  aif/grounded-actuation-not-reobservation

   Attested at library/aif/attestations.edn record 19,
   `:state [:attested-by \"claude-15\"]`, rung 2.

   THE ATTESTATION'S CITATION. Back to the NEXT-STEPS shape after slice 3 broke
   the streak: `:cited` is scheduled-observer-entrypoint.flexiarg:30, \"Couple R10
   to R16: a scheduled tick counts as 'live' only if it can (and does) move the
   substrate dial via grounded actuation\" — an item on a to-do list, quoted
   verbatim as the ground for an edge that says the coupling is already carried
   out. The single `:evidence` record is a `:via :text` hit whose `:excerpt` is
   that same line. So four of five priced edges cite an aspiration and the fifth
   quotes its own source pattern to itself; no attestation in this set has yet
   cited a site.

   THE SITE. Both flexiargs name it and both pointers hold, so there is no
   relocation cost: the source names `futon2 -M:wm-scheduled` (hourly) and the
   `holes/aif-wiring-actuator.html` status panel; the method names
   `futon2/src/futon2/aif/actuator_a3.clj`, `fold_escrow.clj` and build-match.
   All were found where the paragraphs imply. What the paragraphs do NOT say is
   that the site has FIVE separate surfaces that could carry the coupling —
   the scheduler lane, the grounded-actuation lane, the R10 criterion verdict,
   the recorded cron state, and the APM tripwire lane — and that the gate the
   source pattern asks for is built in the second and installed on none of the
   third.

   Facts below are read at futon2 2b4f996.

   THE INVENTIONS. I1-I4 of `how-witness-heartbeat` (obligations, grounding, a
   meaning for \"carries out\", site adjudication), I6 of slice 2 (one row per
   THEN clause when the THEN is prose) and I7 of slice 3 (contradiction is not
   silence) recur and are not restated at length. One is specific to this edge:

     I9. WHICH VERDICT. This source pattern is unusual in NAMING the verdict it
         wants gated: not \"gate on a dial move\" but `gate \"R10 = ✓\"` on one.
         The site builds exactly that gate — `grounded-close-errors`
         (full_loop_cohort.clj:313-325) refuses to label an outcome
         `:grounded-change` unless a before/after substrate witness carries
         `:dial-moved?` — and installs it on the per-attempt OUTCOME LABEL. The
         verdict the obligation names, R10 itself, is decided elsewhere and by
         a different test: the completeness contract's operational check for
         R10 reads, in full, \"Find the schedule + the trace store\"
         (docs/futon-aif-completeness.md:264). Each gate mechanism therefore
         carries a `verdict-addressee`, and a discharge counts only where the
         addressee matches the one the obligation names. Drop I9 and this edge
         attests at 1 of 5 rather than 0 — so the invention's effect on the
         verdict is a number, reported by `addressee-control` below.

   The distinction I9 draws is not available on the three edges priced before
   it, because their source patterns name no addressee. It is what makes this
   slice's failure mode a fourth kind: the method is present, executable, and
   correct, and it is wired to the wrong verdict."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/aif/scheduled-observer-entrypoint.flexiarg THEN (:21-22). The THEN is
;; one prose paragraph, so rows are cut at its clauses and each cites :22, the
;; line every clause sits on (I6, from slice 2).
(def source-obligations
  [[:s1 22 "install the scheduled entrypoint"]
   [:s2 22 "gate \"R10 = ✓\" on evidence that a tick changed observable substrate state (a dial moved, a hole closed) — not merely that the entrypoint fired"]
   [:s3 22 "when the loop can only no-op, DISABLE the cron rather than let it paint green"]
   [:s4 22 "re-enable it precisely when a tick can do real work — when R16's grounded actuation can move a dial"]
   [:s5 22 "add a liveness assertion: N consecutive scheduled runs with zero state-delta FLAGS, it does not PASS"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/aif/grounded-actuation-not-reobservation.flexiarg THEN (:21-22). On
;; the record so the method's own content is visible; the relation uses
;; `method-instanceo` below, which is the property the site can be read for (I3).
(def method-obligations
  [[:m1 22 "the act writes a TYPED, UNGAMEABLE witness to a substrate OUTSIDE the model, derived from the authored bindings and not from builder claims"]
   [:m2 22 "build-match — every box's :produces inhabited, not just the terminal boundary"]
   [:m3 22 "that witness feeds the next decision — the deliberator re-ranks on the substrate dial"]
   [:m4 22 "gate \"closed = ✓\" on a real dial move (1/2 → 2/2), NEVER on the enactor merely firing"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — `kind` is `:grounded-actuation` when the
;; mechanism is an instance of the method (I3); the other kinds name what the
;; mechanism is instead, so a wide positive control cannot be mistaken for a
;; wide witness.
(def site-mechanisms
  [;; --- the method's own lane --------------------------------------------
   ;; The substrate read: endpoint inhabitation resolved by an authed
   ;; Drawbridge query over the REVIEWED endpoint bindings, not over anything
   ;; the builder returned. m1.
   [:substrate-inhabitation   :grounded-actuation "futon2 src/futon2/aif/actuator_a3.clj:296-300"]
   ;; build-match: the per-box snapshot, every bound box's endpoint checked,
   ;; not only the terminal boundary. m2.
   [:box-match-snapshot       :grounded-actuation "futon2 src/futon2/aif/actuator_a3.clj:339"]
   ;; The dial itself: `:dial-moved?` is a set-difference of inhabited endpoint
   ;; NAMES between a before-read and an after-read — a world delta, computable
   ;; only from two substrate reads. m1 + m4.
   [:endpoint-dial-review     :grounded-actuation "futon2 src/futon2/aif/actuator_a3.clj:315-324"]
   ;; The gate. An outcome may not be LABELLED `:grounded-change` unless it is
   ;; `:grounded?`, is not `:artifact-only?`, and carries a before/after witness
   ;; with `:resolved?` and `:dial-moved?` both true. This is the source
   ;; pattern's clause s2, built and executable — see `verdict-addressee`.
   [:grounded-change-invariant :grounded-actuation "futon2 src/futon2/aif/full_loop_cohort.clj:313-325"]
   ;; The A5 substrate dial as a scoring feed: realized-score = bound −
   ;; inhabited, `:realized-source :substrate-dial`. Armed by default; see
   ;; `mirror`.
   [:realized-outcome-grounded :grounded-actuation "futon2 src/futon2/aif/fold_realized.clj:149-181"]

   ;; --- the scheduler lane (not the method) -------------------------------
   ;; The entrypoint the source pattern asks to be installed, and the alias
   ;; that drives it. The alias was REPOINTED at the actuating full loop, so
   ;; the entrypoint that a scheduler would fire today does grounded work —
   ;; a real move toward s4, made in the entrypoint and not in the schedule.
   [:wm-scheduled-alias       :scheduler          "futon2 deps.edn:15"]
   [:scheduled-entrypoint     :scheduler          "futon2 scripts/wm_scheduled_run.clj:66-113"]
   ;; and the old judgement-only runner, renamed to say what it is: retained
   ;; under `:wm-judgement-only`, \"not a scheduler entrypoint\".
   [:judgement-only-alias     :scheduler          "futon2 deps.edn:21, README-clicks-and-ticks.md:163-169"]

   ;; --- the R10 criterion verdict (the addressee s2 names) ----------------
   ;; The operational check for R10, in full: \"Find the schedule + the trace
   ;; store.\" The entrypoint firing and the tape it writes — which is the test
   ;; the source pattern was written against.
   [:r10-operational-check    :criterion-verdict  "futon2 docs/futon-aif-completeness.md:264-266"]
   ;; and the summary row that records the ✓ on that ground.
   [:r10-summary-row          :criterion-verdict  "futon2 docs/futon-aif-completeness.md:349"]
   ;; and the status panel node: R10 `st:\"done\"`, subtitle \"scheduled
   ;; heartbeat\", `how:` \"WIRED (scheduled-ready) … the one remaining step is
   ;; YOU installing the cron entry\". Green, on a schedule that is not running.
   [:explainer-live-node      :criterion-verdict  "futon2 holes/aif-wiring-explainer.html:133"]

   ;; --- the recorded cron state -------------------------------------------
   ;; The disable, with its reason and its re-enable condition.
   [:cron-disable-record      :cron-state         "futon2 holes/aif-wiring-actuator.html:185"]
   ;; and the current state, in the repo's own source table.
   [:clicks-ticks-source-row  :cron-state         "futon2 README-clicks-and-ticks.md:158"]

   ;; --- the APM tripwire lane ---------------------------------------------
   ;; The zero-delta-over-N SHAPE, built: three consecutive opportunities on the
   ;; same unresolved stop-line with no fresh commit raise a violation. Over APM
   ;; opportunities and repository artifacts — not over scheduled runs, and not
   ;; over the substrate dial.
   [:t7-wedge                 :apm-tripwire       "futon2 src/futon2/aif/tripwire.clj:286-307"]])

;; ---- stated facts: what is NOT at the site -------------------------------
;; Searched and not found, each recorded rather than left silent.
(def not-found
  [[:zero-delta-over-N
    "no liveness assertion over scheduled runs. `zero-delta` has one hit in all
     of futon2 — holes/aif-r1-r16-pattern-map.md:47, prose restating this very
     pattern. No code counts consecutive scheduled runs with no state change."]
   [:tick-moved-substrate
    "`:tick/moved-substrate?`, the trace-schema split the source pattern's
     NEXT-STEPS:32 asks for, exists nowhere in futon2 code. Same one prose hit,
     aif-r1-r16-pattern-map.md:47-48."]
   [:grounded-sample-in-trace
    "no trace record carries `:realized-source`. Over the whole
     data/wm-trace/ corpus: 88 `:realized-outcome` records, all dated
     2026-07-02 to 2026-07-06 and all from the coverage mirror; 0 records with
     `:realized-source :substrate-dial`. See `mirror`."]
   [:running-schedule
    "no schedule fires this loop. `crontab -l` carries no futon2 entry and
     ~/.config/systemd/user/wm-scheduled.timer does not exist (both read
     2026-09-02); the repo says the same at README-clicks-and-ticks.md:158,
     \"no authoritative full-loop cron installed\". The last `:wallclock-cron`
     trace record is 2026-07-14."]])

;; ---- stated facts: which verdict each gate is installed on ---------------
;; I9. Read off the mechanism, not assigned by preference: the invariant fires
;; inside `close-attempt!` and decides an attempt's outcome label; the
;; completeness doc's check and the panel node decide R10.
(def verdict-addressee
  [[:grounded-change-invariant :attempt-outcome-label]
   [:r10-operational-check     :r10-criterion]
   [:r10-summary-row           :r10-criterion]
   [:explainer-live-node       :r10-criterion]])

;; The addressee an obligation names, where it names one.
(def obligation-addressee
  [[:s2 :r10-criterion]])

;; ---- adjudication: which mechanism discharges which obligation -----------
;; One row per [mechanism obligation] the check RULES to be a discharge. A
;; judgement over the facts named above, not a computation.
(def discharges
  [[:scheduled-entrypoint      :s1]  ; the entrypoint exists
   [:wm-scheduled-alias        :s1]  ; and is invocable, now via the full loop
   [:grounded-change-invariant :s2]  ; the dial-move gate, built and executable
   [:cron-disable-record       :s3]  ; the cron was disabled rather than left green
   [:clicks-ticks-source-row   :s3]  ; and is still off
   [:t7-wedge                  :s5]]); zero-delta-over-N, other population
;; :s4 gets no row from any mechanism, and that absence is this slice's finding
;; rather than an omission — see `s4-trigger` below.
;; :m3 of `method-obligations` reaches the site only as a declared gap: the
;; method's own NEXT-STEPS:32 records \"the deliberator does not yet re-rank on
;; the substrate dial\", and no trace record has ever carried a grounded sample
;; for it to re-rank on (`not-found` :grounded-sample-in-trace).

;; The condition s4 names, and the dates that decide whether it was met.
;; Recorded as facts because the finding is a comparison of them.
(def s4-trigger
  {:disabled-at   "2026-07-06 — holes/aif-wiring-actuator.html:185, reason \"cached no-op runs suffice; re-enable when the loop does real work\""
   :condition     "re-enable when R16's grounded actuation can move a dial (flexiarg:22)"
   :dial-landed   "2026-07-08 — futon2 723cacf \"Ground realized outcome in substrate dial\" + d36086f (flag-gated, dark)"
   :dial-armed    "2026-07-14 — futon2 9d8f2de, *selection-gain-grounded-feed?* default ON (fold_realized.clj:39-49)"
   :re-enabled-at nil
   :read-at       "2026-09-02 — no cron entry, no systemd timer, last :wallclock-cron trace 2026-07-14"})

;; [obligation mechanism-that-contradicts] — I7, from slice 3. A surface at this
;; site that decides the same question the opposite way.
(def contradictions
  [;; The verdict s2 names is decided by finding a schedule and a tape. The
   ;; operational check is one sentence and it is the sentence the source
   ;; pattern says is wrong.
   [:s2 :r10-operational-check]
   ;; and the panel paints R10 green on \"scheduled heartbeat\" while its own
   ;; `how:` text says the cron is not installed and actuator.html:185 says it
   ;; was disabled.
   [:s2 :explainer-live-node]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is grounded actuation: a
   read of a substrate outside the model, a per-box match against it, a dial
   computed from two such reads, or a gate on that dial. Installing a
   scheduler, recording a cron state, or writing a criterion's ✓ are things the
   source pattern also asks for, and are not this method (I3)."
  [m]
  (mechanism-kindo m :grounded-actuation))

(defn addressee-matcheso
  "I9. Where the obligation names the verdict it wants gated, the mechanism
   must be installed on that verdict. Obligations that name no addressee pass
   this conjunct vacuously."
  [s m]
  (l/conda [(l/fresh [a] (l/membero [s a] obligation-addressee)
                     (l/membero [m a] verdict-addressee))
            l/succeed]
           [(l/fresh [a] (l/membero [s a] obligation-addressee)) l/fail]
           [l/succeed]))

(defn contradictedo [s]
  (l/fresh [m] (l/membero [s m] contradictions)))

(defn uncontradictedo
  "Obligations with no contradiction row (I7). Grounded, not asserted: both
   rows are reads of surfaces that decide the named verdict."
  [s]
  (l/conda [(contradictedo s) l/fail]
           [l/succeed]))

(defn carried-out-by-methodo
  "Source obligation `s` is carried out BY THE METHOD at the site, via `m`, on
   the verdict `s` names, and is not contradicted elsewhere at the same site."
  [s m]
  (l/all (source-obligationo s)
         (dischargeso m s)
         (method-instanceo m)
         (addressee-matcheso s m)
         (uncontradictedo s)))

(defn witness
  "The edge holds behaviourally iff EVERY source obligation appears here."
  []
  (l/run* [s m] (carried-out-by-methodo s m)))

;; ---- controls ------------------------------------------------------------
(defn positive-control
  "Same relation with the method-instance, addressee and contradiction
   conjuncts dropped. Must be NON-empty, or an empty `witness` would mean only
   that the relation cannot see."
  []
  (l/run* [s m] (l/all (source-obligationo s) (dischargeso m s))))

(defn addressee-control
  "The witness with I9 dropped — what the edge scores if a gate installed on
   another verdict still counts. Reported so the invention's effect on the
   verdict is a number rather than a claim."
  []
  (l/run* [s m] (l/all (source-obligationo s) (dischargeso m s) (method-instanceo m))))

(defn mirror
  "The A5 substrate dial as a scoring feed: `realized-outcome-grounded`, armed
   by default since 2026-07-14 and never once exercised — 0 of the wm-trace
   corpus carries `:realized-source`. It is grounded actuation by every
   structural test and it discharges nothing here. Must be EMPTY: a witness
   that counted it would attest the edge on machinery that has never run."
  []
  (l/run* [s] (carried-out-by-methodo s :realized-outcome-grounded)))

(defn -main [& _]
  (let [w (witness) p (positive-control) ac (addressee-control) m (mirror)
        n-src (count source-obligations)
        distinct-w (count (distinct (map first w)))
        distinct-p (count (distinct (map first p)))
        distinct-ac (count (distinct (map first ac)))]
    (println "edge: aif/scheduled-observer-entrypoint")
    (println "  @how aif/grounded-actuation-not-reobservation")
    (println "site: futon2 2b4f996 — five surfaces: scheduler lane, grounded-actuation")
    (println "      lane, R10 criterion verdict, recorded cron state, APM tripwire")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out")
    (println "                                 on the verdict they name:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println "positive control (any mechanism, any lane, must be non-empty):")
    (doseq [b p] (println "  " b))
    (println "addressee control (I9 dropped):" distinct-ac "of" n-src "obligations"
             (vec (distinct (map first ac))))
    (println "mirror (must be empty):" m)
    (println)
    (println "not found at the site:")
    (doseq [[k _] not-found] (println "   " k))
    (println "s4 trigger:" (pr-str (select-keys s4-trigger [:disabled-at :dial-armed :re-enabled-at])))
    (println)
    (println (cond
               (not (seq p))
               "INCONCLUSIVE — the relation finds nothing at all; the fact rows are broken."
               (seq m)
               "INCONCLUSIVE — the mirror is not silent; the relation is too weak."
               (= distinct-w n-src)
               "ATTESTED — every source obligation is carried out by the method."
               (seq w)
               (format "NOT ATTESTED — the method carries out %d of %d obligations."
                       distinct-w n-src)
               :else
               (format (str "NOT ATTESTED — the method carries out 0 of %d obligations, "
                            "with a positive control of %d. The one obligation the method "
                            "does discharge (:s2, the dial-move gate) is built and "
                            "executable and is installed on the attempt outcome label, "
                            "not on the R10 verdict the clause names (I9); the R10 verdict "
                            "is decided by \"find the schedule + the trace store\". The "
                            "remaining three discharged obligations belong to the "
                            "scheduler, cron-state and APM-tripwire lanes. :s4 is "
                            "discharged by nothing: its re-enable condition was met on "
                            "%s and the schedule has been off since %s.")
                       n-src distinct-p
                       (:dial-armed s4-trigger) (:disabled-at s4-trigger))))))
