(ns how-witness-no-self-certification
  "A DERIVED behavioural check for ONE attested `@how` edge — the third priced
   under worklist item :L10 (slice 3), after `how-witness-split-transport`
   (slice 1) and `how-witness-declare-conditioning` (slice 2), all generalising
   :L4's `how-witness-heartbeat`. The step is named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112.

   The edge under test:

     aif/no-self-certification  @how  aif/measurement-window-hygiene

   Attested at library/aif/attestations.edn record 10,
   `:state [:attested-by \"claude-15\"]`, rung 2.

   THE ATTESTATION'S CITATION differs from the three edges priced before it,
   and the difference is the first thing to record. Slices 1 and 2 and :L4 all
   found `:cited` pointing at a NEXT-STEPS / cross-link line — an aspiration.
   Here `:cited` is the SOURCE pattern's own operative THEN clause
   (no-self-certification.flexiarg:33-36), and the single `:evidence` record is
   a `:via :text` hit whose `:excerpt` is a fragment of that same clause. So the
   evidence quotes the source pattern to itself: nothing in the attestation
   reads the METHOD pattern, and nothing reads a site. The `:review` note is
   narrower than the edge and says so plainly — \"@how warranted: settled/
   transient tagging is carried out by window hygiene\" names ONE of the source
   pattern's six THEN clauses. The check below is what that note would have to
   be widened to, to warrant an unqualified `@how`.

   THE SITE. Both flexiargs name the same one, and unlike slice 1 the pointer
   holds: the FutonZero G-SIM calibration arc (WM-pilot, 2026-06-11), cited by
   no-self-certification at its `+ canonical-instance:` (:45-52, \"four distinct
   self-certification attempts surfaced live\") and by measurement-window-hygiene
   at its `+ BECAUSE:` (:37-39, the two fake 0.277 moves). Both incidents were
   found where the paragraphs imply. The transient spike is written up at
   futon3c holes/PILOTS-LOG.md:206 (\"Cycle 6 error 0.277 was a TRANSIENT
   artifact\") and the stale-begin drift at :146-148 (\"any close-error (~0.277)
   would be entirely field-drift ... confounded, excluded\"). The apparatus that
   grew out of them is in three places: the tagger (futon3c war_machine_pilot),
   the two verdict surfaces (futon3c calibration + flight-record), and the
   offline fixture (futon0 reward-red-team).

   Facts below are read at futon3c 9128f5a8, futon0 0de06f6.

   THE INVENTIONS. I1-I4 of `how-witness-heartbeat` (obligations, grounding, a
   meaning for \"carries out\", site adjudication) and I6 of slice 2 (one row per
   THEN clause when the THEN is prose) recur unchanged and are not restated.
   Two are specific to this edge:

     I7. CONTRADICTION IS NOT SILENCE. This site has TWO verdict surfaces
         reading the SAME tag with OPPOSITE polarity. `validity-mask`
         (flight_record.clj:263-264) masks a record OUT when no settled window
         is present — untagged never counts. `calibration-report`
         (calibration.clj:294) admits a record whose `:realised-read` is absent,
         because it tests `(not= :transient ...)` and `nil` is not `:transient`
         — untagged counts. The check therefore carries a `contradictions`
         table beside `discharges`, and an obligation with a contradiction row
         gets no witness row even where a mechanism discharges it. This is not
         an outside judgement imposed on the site: the fixture says it about
         itself, in `known-leniencies` (reward_red_team.clj:95-108), which names
         the shape \"launder by omission\", records that it \"Mirrors the LIVE
         verdict's missing-tag grandfathering\", and states that both should
         stricten. `strictness-control` below reports what the edge would score
         if I7 were dropped, so the invention's effect on the verdict is a
         number and not a claim.

     I8. WHOSE OBLIGATION. Three of the source pattern's six clauses are about
         tags — independence, source, settledness — and window hygiene owns
         exactly one of them. A mechanism may discharge a source obligation and
         still not be an instance of the METHOD; `:independent?` and
         `:measured`/`:target-absent-fallback` are the anti-laundering lane, not
         the window lane. Marking those mechanisms `:independence-tag` and
         `:source-tag` rather than `:window-hygiene` is the adjudication the
         verdict turns on, and it is the reason the positive control is wide
         while the witness is narrow."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/aif/no-self-certification.flexiarg THEN (:31-36). The THEN is one
;; prose paragraph, so rows are cut at its clauses and each cites the line the
;; clause sits on (I6, from slice 2).
(def source-obligations
  [[:s1 32 "a verdict may be moved ONLY by evidence the verdict-maker did not manufacture"]
   [:s2 34 "tag every evidence record at birth with :independent? — was there an out-of-band witness?"]
   [:s3 35 "tag source — :measured vs :target-absent-fallback"]
   [:s4 35 "tag settledness — :settled vs :transient"]
   [:s5 36 "have the verdict count ONLY the records whose tags clear"]
   [:s6 36 "strictness default: untagged never counts"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/aif/measurement-window-hygiene.flexiarg THEN (:26-31). On the record
;; so the method's own content is visible; the relation uses `method-instanceo`
;; below, which is the property the site can be read for (I3).
(def method-obligations
  [[:m1 27 "BEFORE — capture the baseline immediately prior to the field-visible action; prepare, THEN baseline, THEN commit"]
   [:m2 29 "AFTER — read at the settled state; require >=2 consecutive reads agreeing within epsilon"]
   [:m3 29 "tag :settled vs :transient and exclude transient"]
   [:m4 30 "if the system drifted independently inside the window, DROP the pair rather than attribute the drift to the action"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — `kind` is `:window-hygiene` when the
;; mechanism is an instance of the method (I3/I8); the other kinds name what the
;; mechanism is instead, so a wide positive control cannot be mistaken for a
;; wide witness.
(def site-mechanisms
  [;; --- the method's own lane -------------------------------------------
   ;; The settled/transient tag, validated at birth: anything that is not one
   ;; of the two keywords throws at close time. m3.
   [:realised-read-tag        :window-hygiene   "futon3c src/futon3c/peripheral/war_machine_pilot.clj:475-476"]
   ;; The ">=2 consecutive reads agreeing within epsilon" rule, executable:
   ;; two scans, distinct :as-of, both past :threshold, |g1-g2| <= epsilon. m2.
   [:settled-window-pred      :window-hygiene   "futon3c src/futon3c/aif/flight_record.clj:213-232"]
   ;; The strict verdict surface: no settled window => mask :out. `window-of`
   ;; returns nil when the organ is absent and `settled-window?` fails a nil,
   ;; so an untagged record is excluded rather than admitted.
   [:validity-mask-window     :window-hygiene   "futon3c src/futon3c/aif/flight_record.clj:263-264"]
   ;; The lenient verdict surface: transient pairs are excluded from the
   ;; calibration verdict. Same tag, and see `contradictions` for its polarity.
   [:calibration-transient    :window-hygiene   "futon3c src/futon3c/aif/calibration.clj:294"]
   ;; The offline fixture's transient case: a spike read before the field
   ;; settled must reject with :not-settled. m3, in a pure test double.
   [:red-team-transient-case  :window-hygiene   "futon0 scripts/futon0/futonzero/reward_red_team.clj:62-67"]
   ;; The window's ordering constraint, begin <= commit < threshold — the only
   ;; executable trace of m1 anywhere at the site.
   [:window-begin-le-commit   :window-hygiene   "futon3c src/futon3c/aif/flight_record.clj:223"]

   ;; --- the anti-laundering lane, which is NOT the method (I8) -----------
   ;; :independent? is only set on an executed close, and independence is a
   ;; claim that needs a witness: :executed? without :evidence-ref throws.
   [:independence-throw       :independence-tag "futon3c src/futon3c/peripheral/war_machine_pilot.clj:467-469"]
   [:independent-tag          :independence-tag "futon3c src/futon3c/peripheral/war_machine_pilot.clj:518-519"]
   [:red-team-independence    :independence-tag "futon0 scripts/futon0/futonzero/reward_red_team.clj:78-89"]
   ;; A vanished target copies predicted into realised — a censored 0.0. The
   ;; source tag exists so the verdict can drop those.
   [:realised-source-tag      :source-tag       "futon3c src/futon3c/peripheral/war_machine_pilot.clj:495"]
   [:red-team-censored-case   :source-tag       "futon0 scripts/futon0/futonzero/reward_red_team.clj:54-61"]
   ;; The verdict counts only records that are independent AND explicitly
   ;; :measured. Both conjuncts are POSITIVE tests, so untagged never counts —
   ;; for these two tags.
   [:calibration-independent  :verdict-gate     "futon3c src/futon3c/aif/calibration.clj:289-290"]

   ;; --- present, and discharging nothing ---------------------------------
   ;; The baseline is captured at the TOP of begin-live-cycle! (:309), before
   ;; the consent-gate artefact is minted (:364) and before the tick is
   ;; requested (:376) — prepare-after-baseline, which is the stale-baseline
   ;; failure m1 is written against, in the apparatus the pattern came from.
   [:baseline-before-mint     :inverted-ordering "futon3c src/futon3c/peripheral/war_machine_pilot.clj:309,364,376"]
   ;; The begin scan's :as-of is recorded "so the stale-begin confound is
   ;; checkable only if these are data" (:381-383) and is written into the
   ;; flight record twice (flight_record.clj:113,145). Nothing reads it back:
   ;; no comparison, no threshold, no exclusion, in src, scripts or test.
   [:begin-scan-as-of         :recorded-unread   "futon3c src/futon3c/peripheral/war_machine_pilot.clj:383"]
   ;; m4 discharged once, by a person, in prose: the pilot detected the drift
   ;; and excluded the pair by hand and by narrative. No code path drops a
   ;; confounded pair on this ground.
   [:drift-exclusion-by-hand  :prose-only        "futon3c holes/PILOTS-LOG.md:146-148"]
   ;; The fixture's ninth shape: omitting :realised-read passes the settled
   ;; check, because the guard is `contains?`. Documented, not fixed.
   [:launder-by-omission      :known-leniency    "futon0 scripts/futon0/futonzero/reward_red_team.clj:95-108"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:realised-read-tag       :s4]  ; the settledness tag, minted at birth
   [:red-team-transient-case :s4]  ; the same tag, in the offline fixture
   [:validity-mask-window    :s5]  ; a verdict that counts only cleared tags
   [:calibration-transient   :s5]  ; the same, on the calibration surface
   [:validity-mask-window    :s6]  ; absent window => masked out
   [:independent-tag         :s2]  ; the independence tag, minted at birth
   [:independence-throw      :s2]  ; and unforgeable without a witness ref
   [:red-team-independence   :s2]
   [:realised-source-tag     :s3]  ; :measured vs :target-absent-fallback
   [:red-team-censored-case  :s3]
   [:calibration-independent :s1]  ; the verdict moves only on independent,
                                   ; explicitly measured pairs
   [:calibration-independent :s6]]); both conjuncts are positive tests
;; :s1 gets no row from any :window-hygiene mechanism, and the reason is the
;; distinction I8 draws: a transient spike is an artifact of the apparatus's
;; scan TIMING, not evidence the verdict-maker manufactured. Excluding it makes
;; the number honest without making the evidence un-manufactured.
;; :m1 and :m4 of `method-obligations` reach the site only as
;; :inverted-ordering, :recorded-unread and :prose-only rows. The method's
;; baseline half is not carried out here at all; only its readback half is.

;; [obligation mechanism-that-contradicts] — I7. A verdict surface at this site
;; that reads the SAME tag with the opposite polarity, or a leniency the site
;; documents about itself.
(def contradictions
  [;; nil is not :transient, so a record with no :realised-read passes the
   ;; calibration filter and counts toward the verdict.
   [:s6 :calibration-transient]
   ;; and the fixture grants the same leniency by the same mechanism, and says
   ;; so: "Per the arc-1 principle (untagged never counts), BOTH this fixture
   ;; and the live verdict should stricten".
   [:s6 :launder-by-omission]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is window hygiene: a
   baseline-tightening, a settled readback, or an exclusion made on one of
   those grounds. Tagging independence or source is anti-laundering the source
   pattern also asks for, and is not this method (I8)."
  [m]
  (mechanism-kindo m :window-hygiene))

(defn contradictedo [s]
  (l/fresh [m] (l/membero [s m] contradictions)))

(defn uncontradictedo
  "Obligations with no contradiction row. Grounded, not asserted: the
   contradiction rows are reads of two verdict surfaces and the site's own
   `known-leniencies` (I7)."
  [s]
  (l/conda [(contradictedo s) l/fail]
           [l/succeed]))

(defn carried-out-by-methodo
  "Source obligation `s` is carried out BY THE METHOD at the site, via `m`, and
   is not contradicted elsewhere at the same site."
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
  "Same relation with the method-instance and contradiction conjuncts dropped.
   Must be NON-empty, or an empty `witness` would mean only that the relation
   cannot see."
  []
  (l/run* [s m] (l/all (source-obligationo s) (dischargeso m s))))

(defn strictness-control
  "The witness with I7 dropped — what the edge scores if a contradicted
   obligation still counts. Reported so the invention's effect on the verdict
   is a number rather than a claim."
  []
  (l/run* [s m] (l/all (source-obligationo s) (dischargeso m s) (method-instanceo m))))

(defn mirror
  "The mechanism that records the window's own ground and consumes nothing:
   `:scan-as-of`, written into the begin state and copied into the flight
   record twice, read back by no comparison, threshold or exclusion anywhere in
   futon3c src, scripts or test. Must be EMPTY — a witness that counted it
   would attest the edge on the strength of a field that is stored and never
   used."
  []
  (l/run* [s] (carried-out-by-methodo s :begin-scan-as-of)))

(defn -main [& _]
  (let [w (witness) p (positive-control) sc (strictness-control) m (mirror)
        n-src (count source-obligations)
        distinct-w (count (distinct (map first w)))
        distinct-sc (count (distinct (map first sc)))]
    (println "edge: aif/no-self-certification")
    (println "  @how aif/measurement-window-hygiene")
    (println "site: futon3c 9128f5a8 WM-pilot tagger + calibration + flight-record,")
    (println "      futon0 0de06f6 reward-red-team fixture")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println "positive control (any mechanism, any lane, must be non-empty):")
    (doseq [b p] (println "  " b))
    (println "strictness control (I7 dropped):" distinct-sc "of" n-src "obligations" (vec (distinct (map first sc))))
    (println "mirror (must be empty):" m)
    (println)
    (println (cond
               (not (seq p))
               "INCONCLUSIVE — the relation finds nothing at all; the fact rows are broken."
               (seq m)
               "INCONCLUSIVE — the mirror is not silent; the relation is too weak."
               (= distinct-w n-src)
               "ATTESTED — every source obligation is carried out by the method."
               (seq w)
               (format (str "NOT ATTESTED — the method carries out %d of %d obligations. "
                            "All %d ARE discharged at this site, by two separate routes: "
                            "%s are carried out by the anti-laundering lane (independence "
                            "and source tags), which the source pattern also asks for and "
                            "this method is not; %s is discharged by one verdict surface "
                            "and contradicted by another reading the same tag (I7).")
                       distinct-w n-src (count (distinct (map first p)))
                       (vec (remove (set (map first sc)) (distinct (map first p))))
                       (vec (distinct (map first contradictions))))
               :else
               (format "NOT ATTESTED — the method carries out 0 of %d obligations." n-src)))))
