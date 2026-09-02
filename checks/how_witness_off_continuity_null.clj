(ns how-witness-off-continuity-null
  "A DERIVED behavioural check for ONE attested `@how` edge — the seventh
   priced under worklist item :L10 (slice 7), after `how-witness-split-transport`
   (slice 1), `how-witness-declare-conditioning` (2),
   `how-witness-no-self-certification` (3), `how-witness-scheduled-observer` (4),
   `how-witness-status-gated-belief` (5) and
   `how-witness-two-layer-calibration` (6), all generalising :L4's
   `how-witness-heartbeat`. The step is named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112.

   The edge under test:

     aif/off-continuity-null-discriminates  @how  aif/measurement-window-hygiene

   Attested at library/aif/attestations.edn record 47, by zai-2 2026-08-30,
   `:state [:attested-by \"claude-15\"]`, rung 2.

   THIS SLICE CLOSES A TWO-HOP CHAIN, which is why it was kept for last. Slice 6
   priced two-layer-calibration @how off-continuity-null-discriminates — this
   slice's SOURCE was that slice's METHOD. Slice 3 priced no-self-certification
   @how measurement-window-hygiene — this slice's METHOD was that slice's METHOD
   too, from a different source. So both endpoints arrive with their site
   already read, and the new question is the composed one: the source pattern
   says a null is only informative if it is CLEAN, and names window hygiene as
   what makes it clean. Was the one off-continuity null that was actually flown
   clean by the method's own rule? That question has an answer in data, and it
   is the finding below.

   THE ATTESTATION'S CITATION is a third shape, new in the seven. Slices 1, 2,
   4, 5 and :L4 found `:cited` pointing at a NEXT-STEPS or cross-link line;
   slices 3 and 6 found it quoting the source pattern's own THEN. Here it is a
   sentence from the source's `+ how-to-apply:` (:50-52), and the single
   `:evidence` record's `:excerpt` is the same sentence verbatim. That sentence
   is not a restatement — it states a GATE CONDITION: \"Mask-in still requires
   the null be CLEAN: settled, two distinct-as-of scans agreeing within ε
   ([[measurement-window-hygiene]]); a confounded null discriminates nothing.\"
   The mask it names is implemented, at flight_record.clj:241-266. So for the
   first time in the seven, the attestation's citation points at something the
   site can be held to.

   THE SITE. futon3c's WM-pilot flight-record apparatus and the two flights of
   2026-06-12 that the source pattern names as its canonical contrast cases:
   live-957a4836 (the continuity-point clean MOVE, 0x discrimination) and
   live-df706c45 (the off-continuity NULL, ~517x). Both flexiargs name the same
   arc and the pointers hold. NOT :no-site-to-check-against.

   Facts below are read at futon3c de776e18, futon2 86b72e4, and — for the
   null's own record, which is not in any git tree (see `not-found`) — from the
   futon1b substrate at Zone :7073, where the 2026-06-12 ingest of that record
   survives as 13 organ sections under
   arxana/essay/flight/live-df706c45-d0b4-402a-9f0f-5222d42ab470.

   THE INVENTIONS. I1-I4 of `how-witness-heartbeat` (obligations, grounding, a
   meaning for \"carries out\", site adjudication), I6 of slice 2 (one row per
   THEN clause when the THEN is prose) and I7 of slice 3 (contradiction is not
   silence) recur and are not restated. Two are specific to this edge:

     I12. THE CITED CLAUSE IS AN OBLIGATION. `source-obligations` normally comes
          off the THEN. Here the THEN gives s1-s5 and the attestation cites
          none of them — it cites a how-to-apply sentence. Since that sentence
          is the whole of what the attestation asserts, it is carried as s6, at
          its own line. Omitting it would leave the check testing the pattern
          and not the attestation. It is also the only clause of the six that
          names a mechanism (mask-in) rather than a practice, so it is the one
          clause whose discharge is decidable from code rather than adjudicated
          from prose.

     I13. ORDERING IS NOT TIGHTNESS. The method's BEFORE half asks for a
          baseline captured \"immediately prior to the field-visible action\".
          The only thing at the site that touches the baseline is
          `settled-window?`'s `(<= (compare begin commit) 0)`
          (flight_record.clj:223) — a lexicographic test on two ISO strings,
          which constrains the SEQUENCE and not the DURATION. It admits an
          18-second window and a 201-second window on identical terms, and both
          were in fact flown (`window-comparison` below). A predicate that
          cannot distinguish the tight case from the loose one does not carry
          out a clause about tightness, so `:window-begin-le-commit` is marked
          `:ordering-only` rather than `:window-hygiene`. `tightness-control`
          reports what the edge scores if I13 is dropped, and
          `neither-invention-control` what it scores with I7 dropped as well, so
          the inventions' effect on the verdict is three numbers and not a
          claim."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/aif/off-continuity-null-discriminates.flexiarg. s1-s5 are the THEN
;; (:31-36) cut at its clauses (I6); s6 is the how-to-apply sentence the
;; attestation's `:cited` and its one `:evidence` `:excerpt` both quote
;; verbatim (:50-52) — see I12.
(def source-obligations
  [[:s1 32 "pick an OFF-continuity begin — a below-cap target where the two models predict DIFFERENT values at the current state"]
   [:s2 33 "fly a TIGHT null there — a content-advance moving no counted hole"]
   [:s3 33 "settled window, two scans agreeing within epsilon"]
   [:s4 34 "the realised lands on the state-sensitive prediction and misses the constant by exactly the off-continuity gap"]
   [:s5 35 "the null IS the discriminating measurement; the state-blind model's error is the state it cannot see"]
   [:s6 50 "MASK-IN still requires the null be CLEAN: settled, two distinct-as-of scans agreeing within epsilon; a confounded null discriminates nothing"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/aif/measurement-window-hygiene.flexiarg THEN (:26-31). The same four
;; rows slice 3 read, unchanged — this is the same method pattern, and reading
;; it differently for a different source would be the error the row is testing
;; for. On the record so the method's own content is visible; the relation uses
;; `method-instanceo` below (I3).
(def method-obligations
  [[:m1 27 "BEFORE — capture the baseline immediately prior to the field-visible action; prepare, THEN baseline, THEN commit"]
   [:m2 29 "AFTER — read at the settled state; require >=2 consecutive reads agreeing within epsilon"]
   [:m3 29 "tag :settled vs :transient and exclude transient"]
   [:m4 30 "if the system drifted independently inside the window, DROP the pair rather than attribute the drift to the action"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — `kind` is `:window-hygiene` when the
;; mechanism is an instance of the METHOD (I3); the other kinds name what the
;; mechanism is instead, so a wide positive control cannot be read as a wide
;; witness.
(def site-mechanisms
  [;; --- the method's own lane, the readback half -------------------------
   ;; The ">=2 scans, distinct :as-of, both past :threshold, |g1-g2| <= epsilon"
   ;; rule, executable and re-derived from the record's own numbers. m2. This
   ;; is the clause s3 asks for, implemented exactly.
   [:settled-window-pred    :window-hygiene   "futon3c src/futon3c/aif/flight_record.clj:213-232"]
   ;; The mask that refuses a record with no settled window. m3. The mechanism
   ;; s6 names by name.
   [:validity-mask-window   :window-hygiene   "futon3c src/futon3c/aif/flight_record.clj:263-264"]
   ;; The :settled/:transient tag, validated at birth — anything that is not one
   ;; of the two keywords throws at close time. m3.
   [:realised-read-tag      :window-hygiene   "futon3c src/futon3c/peripheral/war_machine_pilot.clj:475-476"]
   ;; The settle rule restated per-record and checkable from the record alone:
   ;; "two scans, distinct :as-of, both > :threshold, |ΔG| 7.55e-5 ≤ ε 0.005
   ;; ⇒ settled". Carried out on the continuity-point flight.
   [:witness-rule-check     :window-hygiene   "futon3c holes/specs/flight.witness.live-957a4836.edn, :window :judgment :rule-check"]
   ;; And on the null: two scans past threshold, agreeing — the readback half
   ;; WAS carried out on the off-continuity flight.
   [:null-scans-settled     :window-hygiene   "futon1b arxana/essay/flight/live-df706c45.../organ/window — scans[0] :as-of 2026-06-12T10:27:02.369198479Z, past :threshold 10:26:46Z"]

   ;; --- the baseline half, which reaches the site as ordering only (I13) --
   ;; `(<= (compare begin commit) 0)`: begin must not come AFTER commit. No
   ;; bound, no epsilon, no threshold on the gap between them. It is the only
   ;; expression anywhere in futon3c src, scripts or test that compares :begin
   ;; to anything.
   [:window-begin-le-commit :ordering-only    "futon3c src/futon3c/aif/flight_record.clj:223"]

   ;; --- the source's own lane, which is NOT the method --------------------
   ;; The dial that selects state-sensitive vs state-blind, and the factor that
   ;; is 1.0 under :constant and x/continuity-point clamped otherwise — so the
   ;; two models coincide exactly at the continuity point and differ off it.
   ;; What makes an OFF-continuity begin a thing that can be picked. (Slice 6.)
   [:sensitivity-continuity :continuity-point "futon2 src/futon2/aif/forward_model.clj:84-92"]
   ;; The begin actually picked: M-first-flights at ohc 4, one hole off the
   ;; continuity point of 3, with the two models predicting different values.
   [:null-off-continuity-begin :off-continuity-begin "futon1b .../organ/field-read (:chosen :ohc 4) and .../organ/measurement (:predicted -4.120613753468093 vs :predicted-constant -4.080947086801427)"]
   ;; The null half of "a tight null": the act was a doc-edit, a VERIFY
   ;; content-advance moving no counted hole, so the field held.
   [:null-content-advance   :null-act         "futon3c holes/missions/M-first-flights.md:729-731 (checkpoint 12)"]
   ;; The realised landing on the scaled prediction and missing the constant.
   [:null-realised-on-scaled :discrimination  "futon1b .../organ/measurement (:realised -4.1205372129849565, :error 7.65404831364e-5) and .../organ/counterfactual (:constant-error 0.03959012618352986)"]
   ;; The one time the discrimination reached a learner-lane output rather than
   ;; pilot prose: the :null stratum, n=1, with mean-constant-error beside it.
   [:stratified-null-stratum :discrimination  "futon3c holes/missions/M-first-flights.md:790-794 (checkpoint 15)"]

   ;; --- what the mask did with the null -----------------------------------
   ;; The verdict the site reached on this record: mask IN, and the null
   ;; entered the stratification as the whole of its stratum.
   [:null-masked-in         :mask-verdict     "futon3c holes/missions/M-first-flights.md:756-757 (checkpoint 13, \"re-verifies CONFORMS with mask IN\") and :791-793 (checkpoint 15, :null n=1)"]
   ;; The begin that mask admitted: 2026-06-12T10:21:59.505556758Z to commit
   ;; 10:25:21Z — 201.5 seconds, 11.2x the continuity-point flight's 18. The
   ;; site flags it in the mission record and does not gate on it.
   [:null-window-loose      :loose-baseline   "futon1b .../organ/window (:begin 10:21:59.505556758Z, :commit 10:25:21Z); flagged at futon3c holes/missions/M-first-flights.md:752-755"]

   ;; --- present, and discharging nothing ----------------------------------
   ;; :begin-at is written by the pilot (:382) and copied into the record twice
   ;; (:143 and, with :scan-as-of, :113/:145). Checkpoint 13 says the loose
   ;; begin "is checkable from :begin-at". Nothing checks it — see `mirror`.
   [:begin-at-recorded-unread :recorded-unread "futon3c src/futon3c/peripheral/war_machine_pilot.clj:382; src/futon3c/aif/flight_record.clj:143"]
   ;; And the companion field, on the one flight it was introduced for, is nil:
   ;; `(some-> (:as-of j) str)` yields nil when the begin judgement carries no
   ;; :as-of, which is what happened on the first live use of the new path.
   [:null-scan-as-of-nil    :recorded-unread   "futon1b .../organ/begin-state (:scan-as-of nil); written at futon3c src/futon3c/peripheral/war_machine_pilot.clj:383"]
   ;; The earlier flight of the same experiment, with a validity caveat the log
   ;; raised and no one answered — see `not-found` row 4.
   [:turn8-null-caveat      :unanswered-question "futon3c holes/PILOTS-LOG.md:194 (Turn 8, 2026-06-11)"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:sensitivity-continuity   :s1]  ; the continuity point, computable
   [:null-off-continuity-begin :s1] ; and a begin picked off it
   [:null-content-advance     :s2]  ; the null half of "a tight null"
   [:window-begin-le-commit   :s2]  ; the tight half — ordering only (I13)
   [:settled-window-pred      :s3]  ; the settle rule, executable
   [:null-scans-settled       :s3]  ; carried out on this null
   [:witness-rule-check       :s3]  ; and on the contrast flight
   [:realised-read-tag        :s3]  ; the tag the rule mints
   [:null-realised-on-scaled  :s4]
   [:stratified-null-stratum  :s5]
   [:validity-mask-window     :s6]]); the mask s6 names, which does refuse an
                                    ; unsettled window — and see below
;; s4 and s5 get no row from any :window-hygiene mechanism, and should not:
;; where the realised lands, and whether the gap counts as evidence, are
;; questions about the two forward models, not about the window that brackets
;; them. The method's own m1 reaches the site only as :ordering-only and
;; :recorded-unread rows; m4 reaches it not at all. That is the same split
;; slice 3 found from the other source — the method's readback half is carried
;; out at this site and its baseline half is not — and this slice adds what it
;; cost on the one measurement that mattered.

;; [obligation mechanism-that-contradicts note] — I7. A clause the site
;; actively falsifies is not a clause the site is silent about.
(def contradictions
  [[:s2 :null-window-loose
    (str "the THEN's word is TIGHT and the null flown was not. Its begin is "
         "2026-06-12T10:21:59.505556758Z and its commit 10:25:21Z — 201.5 s, "
         "against 18 s (09:57:20 to 09:57:38) on the continuity-point flight "
         "the same morning, an 11.2x looser bracket on the pair the pattern "
         "exists to produce. The site is not hiding this: checkpoint 13 records "
         "\"the loose begin window (3.4 min, a file-race re-prep) was flagged "
         "in the record's own window-ground\". What it does not do is refuse "
         "the pair on that ground.")]
   [:s6 :null-masked-in
    (str "s6 is the attestation's own citation, and it is the clause the site "
         "falsifies most directly. It says mask-in REQUIRES the null be clean "
         "and that a confounded null discriminates nothing. The mask it names "
         "exists (flight_record.clj:241-266) and it admitted this null: "
         "checkpoint 13, \"the stepped record re-verifies CONFORMS with mask "
         "IN\"; checkpoint 15, the :null stratum at n=1. Every conjunct of "
         "`settled-window?` that the loose begin could have tripped is a "
         "conjunct about ordering or about the two scans, so the 201.5-second "
         "baseline is invisible to all of them (I13). The sentence attaches a "
         "cleanliness requirement to a gate, the gate is real, and the half of "
         "cleanliness that failed here is the half the gate does not read.")]])

;; [what-was-looked-for where-it-was-named how-the-absence-was-established]
(def not-found
  [["any bound on the baseline half of the window — a maximum begin-to-commit
    duration, an epsilon, a threshold, anything that could separate an 18-second
    bracket from a 201-second one"
    "method flexiarg:27-28 — the baseline is to be captured \"immediately prior\" to the action; source flexiarg:33 — \"a TIGHT null\""
    "`settled-window?` (flight_record.clj:213-232) is the whole of the window rule and its only mention of :begin is `(<= (compare begin commit) 0)` at :223 — a lexicographic comparison of two ISO strings. `grep -rn begin src/futon3c/aif/flight_record.clj` filtered for commit/duration/elapsed/tight/max/limit returns that line and its docstring and nothing else. No arithmetic on the pair anywhere in futon3c src, scripts or test"]
   ["a reader of :begin-at — the field checkpoint 13 says the confound is
    \"checkable from\""
    "futon3c holes/missions/M-first-flights.md:754; and checkpoint 11 (:712-714), \"the stale-begin confound is checkable only if these are data\""
    "`grep -rn begin-at src scripts test` over futon3c returns five hits: two writes (war_machine_pilot.clj:382, flight_record.clj:143), one pretty-printer that prints the string (scripts/flight_scope_view.bb:109), and two test/verifier fixtures. No comparison, no threshold, no exclusion. The same grep for `scan-as-of` returns the same shape: three writes, three fixtures, no reader"]
   ["the flight record itself — the persisted <run-id>.flight.edn the mission
    calls the evidence"
    "M-first-flights.md:732-734 (checkpoint 12), \"the persisted <run-id>.flight.edn beside the γ frame is the evidence\""
    "`ls futon3c/data/repl-traces/` holds exactly one file, live-zai4-wm-pilot-cycle-001.edn. `git log --all --oneline --diff-filter=A -- '*df706c45*'` in futon3c returns nothing and `git log --all --name-only -- '*.flight.edn'` returns zero paths — the 33 records checkpoint 15 stratified were never committed and are not on disk. The record survives only as the 2026-06-12 substrate ingest (14 entities, 13 annotations), readable at futon1b :7073 as arxana/essay-section entities; every organ fact quoted above was read from there. The essay's own `:source-file` prop still points at the vanished path"]
   ["an answer to whether the realised value has an observational component —
    the question that decides whether an off-continuity null is a model test at
    all"
    "futon3c holes/PILOTS-LOG.md:194, Turn 8 (2026-06-11), raised by the pilot as a CAVEAT \"flagged to ground control as a QUESTION\""
    "Turn 8 flew the same experiment a day before Turn 23 and got the same shape — off-continuity ohc 4, below-cap, flat/null, predicted -4.1239 scaled vs realised -4.124, error 6e-5, predicted-constant -4.0843 off by 0.04, \"scaled matches, constant misses\" — and flagged that it \"may not be a genuine model-test\" because \"realised IS the field G which composes the same scaled predict-effects the model predicts with\". `grep -rn` for the caveat's text over futon3c holes/, src/ and scripts/ returns exactly the one line that raised it. The pattern was minted on the repeat of an experiment whose own pilot had asked, and not been told, whether it measures anything external"]])

;; ---- the two windows, side by side ---------------------------------------
;; [run-id class begin commit baseline-seconds discrimination] — the fact the
;; verdict turns on, as a table. Both flights are 2026-06-12; both pass
;; `validity-mask` with :mask :in; the mask reads no column but the last two.
(def window-comparison
  [["live-957a4836" :clean "09:57:20Z"           "09:57:38Z" 18.0
    "0x — the begin sat AT the continuity point, so scaled == constant and a clean MOVE distinguished nothing"]
   ["live-df706c45" :null  "10:21:59.505556758Z" "10:25:21Z" 201.5
    "~517x — scaled error 7.654e-5 against constant error 0.03959, the number the source pattern's BECAUSE cites"]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))
(defn contradictedo [s] (l/fresh [m note] (l/membero [s m note] contradictions)))

(defn uncontradictedo
  "Obligations with no contradiction row. Same idiom slices 3 and 6 used:
   grounded in the reads recorded in `contradictions`, not asserted."
  [s]
  (l/conda [(contradictedo s) l/fail]
           [l/succeed]))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is window hygiene: a
   tightened baseline, a settled readback, or an exclusion made on one of those
   grounds. Picking an off-continuity begin and reading the gap between two
   models are the SOURCE pattern's own moves, and an ordering test on the begin
   is not the tightness clause (I13)."
  [m]
  (mechanism-kindo m :window-hygiene))

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
   obligation still counts."
  []
  (l/run* [s m] (l/all (source-obligationo s) (dischargeso m s) (method-instanceo m))))

(defn tightness-control
  "The witness with I13 dropped — what the edge scores if the ordering test on
   :begin is allowed to count as the method's baseline half."
  []
  (l/run* [s m] (l/all (source-obligationo s)
                       (dischargeso m s)
                       (l/conde [(mechanism-kindo m :window-hygiene)]
                                [(mechanism-kindo m :ordering-only)])
                       (uncontradictedo s))))

(defn neither-invention-control
  "Both I7 and I13 dropped. Reported because the verdict should not rest on an
   invention: if this is still short of the full set, the edge is overstated on
   the site's own terms."
  []
  (l/run* [s m] (l/all (source-obligationo s)
                       (dischargeso m s)
                       (l/conde [(mechanism-kindo m :window-hygiene)]
                                [(mechanism-kindo m :ordering-only)]))))

(defn mirror
  "The field the site says the confound is checkable from, and which nothing
   reads: :begin-at, written by the pilot and copied into the record, consumed
   by one pretty-printer and two test fixtures. Must be EMPTY — a witness that
   counted it would attest the edge on the strength of a timestamp that is
   stored and never compared."
  []
  (l/run* [s] (carried-out-by-methodo s :begin-at-recorded-unread)))

(defn -main [& _]
  (let [w (witness) p (positive-control)
        sc (strictness-control) tc (tightness-control) nc (neither-invention-control)
        m (mirror)
        n-src (count source-obligations)
        d #(count (distinct (map first %)))]
    (println "edge: aif/off-continuity-null-discriminates")
    (println "  @how aif/measurement-window-hygiene")
    (println "site: futon3c de776e18 flight-record + WM-pilot; futon2 86b72e4 forward-model;")
    (println "      the two flights of 2026-06-12, read from the futon1b substrate ingest")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println "positive control (any mechanism, any lane, must be non-empty):"
             (d p) "of" n-src (vec (distinct (map first p))))
    (println "strictness control (I7 dropped): " (d sc) "of" n-src (vec (distinct (map first sc))))
    (println "tightness control  (I13 dropped):" (d tc) "of" n-src (vec (distinct (map first tc))))
    (println "neither invention  (both dropped):" (d nc) "of" n-src (vec (distinct (map first nc))))
    (println "mirror (must be empty):" m)
    (println)
    (println "baseline windows, both masked IN:")
    (doseq [[id klass begin commit secs disc] window-comparison]
      (println (format "  %-14s %-7s %s -> %s  %6.1fs  %s" id (str klass) begin commit secs disc)))
    (println)
    (println (cond
               (not (seq p))
               "INCONCLUSIVE — the relation finds nothing at all; the fact rows are broken."
               (seq m)
               "INCONCLUSIVE — the mirror is not silent; the relation is too weak."
               (= (d w) n-src)
               "ATTESTED — every source obligation is carried out by the method."
               (seq w)
               (format (str "NOT ATTESTED — the method carries out %d of %d obligations, and the one "
                            "it does carry out is the readback half (s3: settled window, two scans "
                            "within epsilon), implemented exactly. All %d ARE discharged at this site. The "
                            "two the method owns and does not deliver are %s: the source asks for a "
                            "TIGHT null and the null flown had a 201.5-second baseline against the "
                            "contrast flight's 18, and the attestation's own cited clause says mask-in "
                            "requires the null be clean — the mask exists, admitted this null, and "
                            "reads no part of the baseline. Dropping both inventions still leaves the "
                            "edge at %d of %d, so the overstatement does not rest on the inventions.")
                       (d w) n-src (d p) (vec (distinct (map first contradictions))) (d nc) n-src)
               :else
               (format "NOT ATTESTED — the method carries out 0 of %d obligations." n-src)))))
