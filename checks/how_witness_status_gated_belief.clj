(ns how-witness-status-gated-belief
  "A DERIVED behavioural check for ONE attested `@how` edge — the fifth priced,
   after `how-witness-heartbeat` (:L4), `how-witness-split-transport` (slice 1),
   `how-witness-declare-conditioning` (slice 2), `how-witness-no-self-certification`
   (slice 3) and `how-witness-scheduled-observer` (slice 4). Worklist item :L10
   slice 5; the step named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112.

   The edge under test:

     aif/status-gated-belief-update
       @how aif/predictive-coding-belief-update

   Attested at library/aif/attestations.edn record 21, rung 2,
   `:state [:attested-by \"claude-15\"]`, reviewed by claude-15 2026-08-30 with
   the note \"@how warranted: the gate is carried out by calling update-mu\".
   `:cited` is the source pattern's NEXT-STEPS line 35 — \"Implement the gate as
   a case-dispatch in the per-tick post-processing step, *not* inside update-mu
   itself. update-mu stays a pure function over (μ, observation, opts); the gate
   decides whether to call it.\" Five of six priced edges now cite a NEXT-STEPS
   line rather than a site.

   Same method as the four before it: a relation run FORWARD over stated facts,
   with controls and a mirror, so an empty witness cannot be confused with a
   relation that cannot see.

   THE SITE. Neither pattern names a repository. Both name paths inside one:
   the source's REFINED line (flexiarg:38) names `notebooks/ukrn_v3_kernel.clj
   postprocess-institution`; the method's DONE line (flexiarg:35) names
   `notebooks/ukrn_v3_belief.clj:1-51`. Both resolve, exactly and including the
   line range, in `/home/joe/code/ukrn-services-simulation` at ba27028 — a
   repository OUTSIDE the futon0..futon7 stack, the first such site in the six
   edges priced. It is a public-release repository: its history is 17 commits
   beginning with \"Initial public release\", so the pre-release archaeology the
   DONE lines invite is impossible by construction (see `not-found` below).

   Facts below are read at ukrn-services-simulation ba27028.

   INVENTIONS. I1 (obligations are the THEN's bullets, one row each), I2
   (grounding), I3 (a meaning for \"carries out\"), I4 (site adjudication is a
   read, not a computation) recur from :L4 unchanged. I7 from slice 3
   (contradiction is not silence) applies. One is specific to this edge:

     I10. NON-INVOCATION IS NOT CARRYING-OUT. The source's third clause —
          :truncated retains pre-step :mu unchanged — is discharged at the site
          precisely by NOT calling update-mu (kernel.clj:222 carries `(:mu pre)`
          forward). An `@how` edge asserts the source is carried out BY the
          method. Counting a clause that the method's ABSENCE satisfies would
          attest the coupling on the one case that excludes the method, so it is
          not counted here. `non-invocation-control` reports the number the
          verdict would take if I10 were dropped: 3 rather than 2."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/aif/status-gated-belief-update.flexiarg THEN (:23-28), one row per
;; status bullet plus the framing rule the THEN opens with and the two
;; properties stated as prose after the bullets (I1).
(def source-obligations
  [[:s1 24 "distinguish engagement status (does the agent act on the instance this tick) from observation availability (does the agent see its state this tick), and gate on the second"]
   [:s2 25 "status :active — engaging AND observing. Apply update-mu using the post-step state"]
   [:s3 26 "status :paused — not engaging but still observing. Apply update-mu using the post-step (D, A) as observation"]
   [:s4 27 "status :truncated — no further observations are meaningful. Retain pre-step :mu unchanged"]
   [:s5 28 "the trace record emits :mu for all instances every tick"]
   [:s6 28 "a :paused instance's belief evolves under observation noise — variance grows by sensor-variance per tick — and tracks the post-step (D, A); only :truncated shows a frozen belief"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/aif/predictive-coding-belief-update.flexiarg THEN (:22-26). On the
;; record so the method's own content is visible; the relation uses
;; `method-instanceo` below, which is the property the site can be read for (I3).
(def method-obligations
  [[:m1 24 "mean: μ ← μ + α · Π · ε, where ε = observation − μ"]
   [:m2 25 "variance: var ← (1−α)·var + α·(observation − new-μ)² + σ²_sensor, σ²_sensor a configurable floor"]
   [:m3 26 "the residual is taken against the NEW mean, not the old mean"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — kind is :update-mu-call when the mechanism
;; is an invocation of the method's implementation, and names what it is
;; otherwise. Paths are relative to ukrn-services-simulation at ba27028.
(def site-mechanisms
  [;; The gate itself: a `case` on the PRE-step engagement status, sited in the
   ;; per-tick post-processing step and not inside update-mu. Exactly the shape
   ;; the :cited NEXT-STEPS line asks for.
   [:case-dispatch        :gate-structure       "notebooks/ukrn_v3_kernel.clj:200"]
   ;; The two branches that call the method. These are the ONLY two production
   ;; call sites of update-mu in the repository (grep over notebooks/ and
   ;; scripts/), so "the gate decides whether to call it" is literally true.
   [:active-update        :update-mu-call       "notebooks/ukrn_v3_kernel.clj:205-206"]
   [:paused-update        :update-mu-call       "notebooks/ukrn_v3_kernel.clj:209-213"]
   ;; The third branch carries the prior forward. The method is not invoked.
   [:truncated-carry      :method-not-called    "notebooks/ukrn_v3_kernel.clj:222"]
   ;; The observation fed to both calls is read off `post` before the paused
   ;; branch merges pre's rolled-back factors back in, so it is post-step.
   [:observed-from-post   :observation-source   "notebooks/ukrn_v3_kernel.clj:198-199"]
   ;; And the rollback cannot reach it: NPT-KEYS is 16 factor keys and contains
   ;; neither :current-D nor :current-A.
   [:npt-keys-exclude-da  :rollback-scope       "notebooks/ukrn_v2_dynamics.clj:630-636"]
   ;; The method's own three rules, implemented as one pure function of
   ;; (μ, observed-D, observed-A, opts) — m1, m2, m3 of `method-obligations`.
   [:mean-rule            :method-implementation "notebooks/ukrn_v3_belief.clj:29-30"]
   [:post-update-residual :method-implementation "notebooks/ukrn_v3_belief.clj:31-32"]
   [:variance-rule        :method-implementation "notebooks/ukrn_v3_belief.clj:35-40"]
   ;; The trace: mu_mean_D/A and mu_var_D/A are four of the sixteen CSV columns,
   ;; written for every institution of every snapshot, and a snapshot is taken
   ;; every tick including tick 0. A reader, not a caller.
   [:trajectory-csv-row   :trace-writer         "notebooks/ukrn_v2_runner.clj:120-137"]
   [:trajectory-csv-loop  :trace-writer         "notebooks/ukrn_v2_runner.clj:148-153"]
   [:snapshot-every-tick  :trace-writer         "notebooks/ukrn_v2_runner.clj:61-74"]
   ;; The prose statement of the engagement/observation distinction, at the gate.
   ;; It is a comment: it states the rule the case dispatch is written against.
   [:gate-comment         :prose                "notebooks/ukrn_v3_kernel.clj:191-197"]
   ;; Where the status the gate reads comes from. Three-valued and sticky; no
   ;; separate observation-availability value is ever computed.
   [:status-determination :gate-structure       "notebooks/ukrn_v3_kernel.clj:125-153"]
   ;; The regression test the source's REFINED line names. Exercises the gate
   ;; through eng/engagement-step, which is a thin wrapper on kernel/step-kernel
   ;; (notebooks/ukrn_v2_engagement.clj:207-240), so it does reach this site.
   [:regression-test      :test                 "test/notebooks/ukrn_v3_belief_update_test.clj:71-104"]
   ;; THE MIRROR. The EFE planner's forward model calls step-kernel on a
   ;; candidate band-action, so the same gate fires and update-mu runs on an
   ;; IMAGINED next state, once per candidate per band per tick. The resulting
   ;; :mu is then discarded: predict-engagement-step returns :mean-D/:mean-A off
   ;; the institution's (D, A) and :var off prediction noise, never off :mu.
   [:planner-update       :update-mu-call       "notebooks/ukrn_v3_forward.clj:155 → kernel.clj:205-213; discarded at forward.clj:156-169"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:case-dispatch       :s1]  ; the distinction is made — as a case on status,
                               ; with observation availability folded into which
                               ; branch a status lands in
   [:active-update       :s2]
   [:paused-update       :s3]
   [:truncated-carry     :s4]  ; discharged, and by NOT calling the method (I10)
   [:trajectory-csv-row  :s5]
   [:trajectory-csv-loop :s5]
   [:snapshot-every-tick :s5]])
;; :s6 has NO discharge row, and that is this slice's finding. See
;; `contradictions` below: the method's variance rule does not produce the
;; monotone growth the source's clause asserts.

;; [mechanism obligation contradiction] — I7: a clause the site actively
;; falsifies is not the same as a clause the site is silent about.
(def contradictions
  [[:variance-rule :s6
    (str "the rule is an EWMA with a σ² addend, not an increment of σ² per tick: "
         "var' = (1−α)·var + α·resid² + σ². At the shipped defaults α=0.4, "
         "σ²=0.02 (notebooks/ukrn_v2_substrate.clj:307-315) its zero-residual "
         "fixed point is σ²/α = 0.05, so variance CONVERGES to 0.05 from either "
         "side. From an initial 0.04 the sequence is 0.040 0.044 0.0464 0.0478 "
         "0.0487 …; from 0.08 it is 0.080 0.068 0.0608 0.0565 …, i.e. it SHRINKS "
         "under a strictly positive sensor variance. \"Variance grows by "
         "sensor-variance per tick\" is not a property of the named method.")]
   [:regression-test :s6
    (str "the test the source's REFINED line names asserts only "
         "(> (:var-D post-mu) (:var-D pre-mu)) at :103, one step from the "
         "initial wide prior DEFAULT-BELIEF-VARIANCE = 0.04 "
         "(notebooks/ukrn_v2_substrate.clj:293-295). It passes because 0.04 lies "
         "below the fixed point 0.05, not because variance grows. The clause and "
         "its test agree on a run the fixed point happens to make true.")]])

;; [what-was-looked-for where-it-was-named how-the-absence-was-established]
(def not-found
  [["handoffs/v3_runner/validation_HO-02.md"
    "source flexiarg:37 and method flexiarg:35, as the evidence for both DONE lines"
    "`find /home/joe/code -maxdepth 8 -name 'validation_HO-0*'` — no hits; no directory named v3_runner anywhere under /home/joe/code; `git log --all -- 'handoffs/*'` in the site repository is empty"]
   ["handoffs/v3_runner/validation_HO-08b.md"
    "source flexiarg:38, as the evidence for the REFINED semantic"
    "same three searches"]
   ["the v2 implementation named in the source's first DONE line"
    "source flexiarg:37 — \"status-gated update first implemented in notebooks/ukrn_v2_engagement.clj\""
    "the file exists and contains no update-mu call and no belief gate at HEAD; `git log --all -S 'update-mu' -- notebooks/ukrn_v2_engagement.clj` is empty, as is `git log --all -S 'update-mu'` over the whole repository apart from the squashed initial commit"]
   ["the test name the source's first DONE line gives, paused-and-truncated-institutions-keep-their-beliefs"
    "source flexiarg:37"
    "`git log --all -S` on the name returns nothing; the repository's history is 17 commits beginning with cb72c71 \"Initial public release\", so no state before the release exists to search"]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it INVOKES the method's
   implementation. update-mu has exactly two production callers and both are
   branches of the gate, so this is a narrow test that the site nonetheless
   passes twice (I3)."
  [m]
  (mechanism-kindo m :update-mu-call))

(defn carried-out-by-methodo
  "Source obligation `s` is carried out BY THE METHOD at the site, via `m`."
  [s m]
  (l/all (source-obligationo s)
         (dischargeso m s)
         (method-instanceo m)))

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

(defn non-invocation-control
  "The witness with I10 dropped — :method-not-called admitted as an instance of
   the method. Reports what the verdict would be if a clause satisfied by the
   method's ABSENCE were allowed to attest the coupling."
  []
  (l/run* [s m]
    (l/all (source-obligationo s)
           (dischargeso m s)
           (l/conde
            [(mechanism-kindo m :update-mu-call)]
            [(mechanism-kindo m :method-not-called)]))))

(defn mirror
  "The planner's update-mu call, asked what it carries out. Must be EMPTY: a
   witness that counted any invocation of the method would attest a pattern
   about what the agent OBSERVED on a call made about a state the agent has not
   observed and will discard — which is the fiction the source pattern's
   BECAUSE (flexiarg:30-31) exists to rule out."
  []
  (l/run* [s] (carried-out-by-methodo s :planner-update)))

(defn -main [& _]
  (let [w (witness) p (positive-control) n (non-invocation-control) m (mirror)
        n-src (count source-obligations)
        distinct-obs (fn [rows] (count (distinct (map first rows))))]
    (println "edge: aif/status-gated-belief-update")
    (println "  @how aif/predictive-coding-belief-update")
    (println "site: ukrn-services-simulation ba27028 (outside the futon stack)")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println (format "positive control (any mechanism, must be non-empty): %d of %d"
                     (distinct-obs p) n-src))
    (doseq [b p] (println "  " b))
    (println (format "non-invocation control (I10 dropped): %d of %d"
                     (distinct-obs n) n-src))
    (println "mirror (must be empty):" m)
    (println)
    (println "contradictions — obligations the site falsifies rather than omits:")
    (doseq [[mech s _] contradictions] (println "  " mech s))
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
                            "%d of %d are discharged at this site; of the rest, one is "
                            "discharged by NOT calling the method (I10), one by a CSV "
                            "writer that reads :mu, one by the shape of the case "
                            "dispatch, and one is contradicted by the method's own "
                            "variance rule.")
                       (distinct-obs w) n-src (distinct-obs p) n-src)
               :else
               (format "NOT ATTESTED — the method carries out 0 of %d obligations." n-src)))))
