(ns how-witness-delivery-vs-practice
  "A DERIVED behavioural check for ONE attested `@how` edge — the eighth and
   last priced under worklist item :L10, after `how-witness-heartbeat` (:L4),
   `how-witness-split-transport` (slice 1), `how-witness-declare-conditioning`
   (slice 2), `how-witness-no-self-certification` (slice 3),
   `how-witness-scheduled-observer` (slice 4),
   `how-witness-status-gated-belief` (slice 5),
   `how-witness-two-layer-calibration` (slice 6) and
   `how-witness-off-continuity-null` (slice 7). The step named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112.

   The edge under test:

     ukrns/distinguish-delivery-from-practice-change
       @how ukrns/calibrate-impact-promises-to-current-indicator-capacity

   Attested at library/ukrns/attestations.edn record 0 — the file's ONLY
   record — rung 2, `:by \"zai-2\"` 2026-08-30, `:state [:attested-by
   \"claude-15\"]`, reviewed 2026-08-31. `:cited` is the source pattern's own
   THEN (flexiarg:23), the third of the eight to quote itself rather than a
   NEXT-STEPS line. Its one `:evidence` row is NOT from either pattern and not
   from any site: `e-3e8f1734-53b8-4fbb-a18d-70fbf0b5a7c2` is a chat turn of
   2026-05-18 proposing three candidate wordings for one table cell of a paper
   under co-author review, and the `:excerpt` is the middle candidate, offered
   with \"OK to apply the middle version, or want a different cut?\" (read at
   futon1b textprobe/history-versions-full.edn, band :evidence-full, vt
   2026-05-18T17:41:13Z). See `not-found` rows 1 and 2.

   Same method as the seven before it: a relation run FORWARD over stated
   facts, with controls and a mirror, so an empty witness cannot be confused
   with a relation that cannot see.

   THE SITE. Neither pattern names a repository or a path — the first of the
   eight with no pointer of any kind. Both name the same document by
   description: the source's context says \"the current paper's TRAINING
   track\", the method's says \"the current paper's INDICATORS track\". Exactly
   one document in /home/joe/code defines tracks under those two names, and
   defines them in one sentence:
   futon4/holes/missions/M-or-training-as-learning-system.md:130. It also
   carries the source's canonical number (2,816 trainees, :146) and the
   method's canonical partition (WP09's operational-at-pilot-scale /
   achievable-but-limited split, :167-171). The simulation that consumes the
   delivery counts is a second site file, ukrn-services-simulation — the same
   repository slice 5 priced, reached here from the other end.

   Facts below are read at futon4 26d1b27 and ukrn-services-simulation ba27028.
   NOT :no-site-to-check-against: eight of eight edges priced under :L10 named
   a site that exists.

   INVENTIONS. I1 (obligations are the THEN's clauses, one row each), I2
   (grounding), I3 (a meaning for \"carries out\"), I4 (site adjudication is a
   read, not a computation) recur from :L4 unchanged. I6 from slice 2 (one row
   per clause when the THEN is prose) and I7 from slice 3 (contradiction is not
   silence) recur; both contradiction rows below are I7 rows. Two are specific
   to this edge:

     I14. THE THEN'S QUALIFIER IS ITS OWN OBLIGATION. The THEN's second half —
          \"reserve impact claims for downstream evidence that actually tracks
          changed practice\" — is carried as TWO rows: s2, that impact claims
          are reserved, and s3, that the evidence reserved for actually tracks
          changed practice. They come apart at this site: the reservation is
          made and the instrument named for it has not been run, and the
          measurements that HAVE been made track practice levels rather than
          practice change. `qualifier-control` reports the verdict with s2 and
          s3 merged back into one row.

     I15. THE CONCLUSION'S PROHIBITION IS AN OBLIGATION. s4 is taken off the
          conclusion (flexiarg:12), not the THEN — \"a training system that
          reports delivery as if it were impact will systematically overstate
          what it knows\". It earns a row because it is the only clause the
          site can FAIL rather than omit, and it does fail it, in code.
          `then-only-control` reports the verdict with s4 dropped."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/ukrns/distinguish-delivery-from-practice-change.flexiarg. s1-s3 are
;; the THEN (:23), one row per clause (I1, I6) with its qualifier split out
;; (I14); s4 is the conclusion's prohibition (:12, I15).
(def source-obligations
  [[:s1 23 "use delivery counts to claim delivery"]
   [:s2 23 "reserve impact claims for downstream evidence"]
   [:s3 23 "the downstream evidence reserved for actually tracks changed practice"]
   [:s4 12 "do not report delivery as if it were impact; a system that does will systematically overstate what it knows"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/ukrns/calibrate-impact-promises-to-current-indicator-capacity.flexiarg
;; THEN (:22). On the record so the method's own content is visible; the
;; relation uses `method-instanceo` below, which is the property the site can be
;; read for (I3).
(def method-obligations
  [[:m1 22 "state which practices are currently demonstrable"]
   [:m2 22 "state which are still development work"]
   [:m3 22 "align service claims with that distinction"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer]. Two prefixes: `mission/` is
;; futon4/holes/missions/M-or-training-as-learning-system.md at 26d1b27;
;; `sim/` is a path in ukrn-services-simulation at ba27028.
;;
;; :capacity-partition and :claim-alignment are the two kinds that make a
;; mechanism an instance of the METHOD (I3): the first states the
;; demonstrable / development-work split (m1, m2), the second conditions a
;; claim on it (m3). Every other kind is something the site does by other
;; means.
(def site-mechanisms
  [;; ---- delivery claims made from delivery counts ----
   [:t1-output-counts       :delivery-claim      "mission/:146 — 2,816 trainees, 104% of target, per-institution distribution"]
   [:delivery-viability     :delivery-claim      "mission/:150 — \"the T1 counts confirm delivery happened, at network scale\""]
   [:overshoot-surprise     :delivery-claim      "mission/:316 — 2,816 vs target ~2,700 read as network-scale confirmation of the delivery-viability dimension"]
   [:count-provenance       :delivery-claim      "sim/docs/tech_note_v11_four_tier_model_fidelity.md:31 — \"reliable count data; it is what the design's quantitative claims about reach and band-distribution rest on\""]

   ;; ---- the distinction itself, stated ----
   [:counts-not-outcomes    :distinction-stated  "mission/:152 — \"The T1 counts measure delivery (trainees attended) but not outcomes (trainees adopted practices)\""]
   [:sorry-1                :distinction-stated  "mission/:15 — researcher-level behaviour change named as an unmet proof obligation"]
   [:ready-vs-missing-row-1 :gap-register        "mission/:285 — delivery counts in the have column, \"whether trainees adopted practices\" in the missing column, cross-referenced to Sorry 1"]

   ;; ---- impact claims reserved ----
   [:sorry-1-closure        :impact-reservation  "mission/:508 — Sorry 1 closes by running the T1 wave, not by argument from counts"]
   [:pi-not-available       :impact-reservation  "sim/docs/tech_note_v11_four_tier_model_fidelity.md:60 — phenotype-expression pi refused: \"the downstream evaluation wave has not been run ... pi would be a free parameter with no anchor\""]
   [:exclusion-list         :impact-reservation  "sim/docs/tech_note_v11_four_tier_model_fidelity.md:144 — no state variable without a current-data anchor or a named collectable source"]

   ;; ---- the method: the capacity partition ----
   [:wp09-partition         :capacity-partition  "mission/:167-171 — operational at pilot scale / achievable but limited / manual checking still necessary"]
   [:indicators-inventory   :capacity-partition  "mission/:243-246 — the same split re-stated as an inventory, plus the unfunded-work caveat"]
   [:pilot-vs-sector-row    :capacity-partition  "mission/:287 — pre-registration/FAIRness/CRediT at pilot scale in the have column, the same indicators at sector scale in the missing column, cross-referenced partial Sorry 1"]
   [:twelve-aspects-row     :capacity-partition  "mission/:294 — WP02's 12 priority aspects against \"only ~3 are currently buildable per WP09\""]

   ;; ---- the method: claims aligned to that partition ----
   [:contract-calibration   :claim-alignment     "mission/:177 — \"A UKRN-S contract that promises demonstrable change has to be calibrated against this current state\""]
   [:visible-impact-gate    :claim-alignment     "mission/:175 — the Visible Impact gate factors reported as partially constructible, not yet at scale"]
   [:contractors-partial    :claim-alignment     "mission/:319 — contractors looking for demonstrable change \"get a partial answer at present, with named gaps\""]

   ;; ---- the downstream evidence, named ----
   [:t1-instrument          :downstream-instrument "mission/:221 — T1 evaluation instrument, 5 behaviour-change indicators, ethics-approved KCL MRA-23/24-42968"]
   [:t1-tripwire            :downstream-instrument "mission/:439 — the preserve-list tripwire: ethics renewed, first wave runs by Q3 2026"]

   ;; ---- the model wiring that couples practice to counts ----
   [:t1-to-d                :model-wiring        "sim/notebooks/ukrn_v2_substrate.clj:52-63 — D = T1/(T1+scale), the delivery count normalised onto the D axis"]
   [:d-from-t1              :model-wiring        "sim/notebooks/ukrn_v2_substrate.clj:951 and :1198 — every institution's starting D is t1-to-d of its cumulative T1"]
   [:mode-of                :model-wiring        "sim/notebooks/ukrn_v2_substrate.clj:866-874 — (D,A) thresholded at 0.5 into multiplied/absorbed/latent/mismatch"]
   [:sigmoid-assumption     :prose               "sim/docs/tech_note_v11_four_tier_model_fidelity.md:18 and :50 — \"the two are tied together by a sigmoid — practice is assumed proportional to throughput\""]

   ;; THE MIRROR. A claim deferred, in the same grammatical shape as
   ;; :contract-calibration, on grounds that have nothing to do with indicator
   ;; capacity: the MVSG architectural label is held conditional pending a
   ;; VERIFY verdict. Kind :claim-alignment, so it IS a method instance by I3
   ;; and the relation must be stopped by the discharge table alone.
   [:mvsg-deferral          :claim-alignment     "mission/:472 and :483-488 — \"Design coherence does not depend on the MVSG label surviving\""]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:t1-output-counts       :s1]
   [:delivery-viability     :s1]
   [:overshoot-surprise     :s1]
   [:count-provenance       :s1]

   [:sorry-1-closure        :s2]
   [:pi-not-available       :s2]
   [:exclusion-list         :s2]
   [:pilot-vs-sector-row    :s2]   ; and by the METHOD: the missing impact
   [:twelve-aspects-row     :s2]   ; evidence is stated in capacity terms
   [:contract-calibration   :s2]   ; and the promise is held to that state

   [:t1-instrument          :s3]   ; an instrument that tracks changed practice
   [:t1-tripwire            :s3]   ; is named, and dated

   [:counts-not-outcomes    :s4]
   [:sorry-1                :s4]
   [:ready-vs-missing-row-1 :s4]])

;; [mechanism obligation contradiction] — I7: a clause the site actively
;; falsifies is not the same as a clause the site is silent about.
(def contradictions
  [[:mode-of :s4
    (str "the model reports delivery as practice, by construction. D is "
         "t1-to-d of the cumulative delivery count (substrate.clj:951, :1198) "
         "and nothing else; mode-of thresholds (D,A) at 0.5 into four cells "
         "whose names are practice-outcome claims — Multiplied, Absorbed, "
         "Latent, Mismatch (substrate.clj:866-874). An institution crosses "
         "from Mismatch into Absorbed at D >= 0.5, i.e. at T1 >= the scale "
         "constant 100 trainees, with no observation of practice anywhere in "
         "the path. The tech note states the coupling in its own words at "
         ":18 — \"D combines genotype stock (T1 throughput) with phenotype "
         "expression (whether the cadre is actually doing the practice). "
         "Currently the two are tied together by a sigmoid — practice is "
         "assumed proportional to throughput\" — and at :50, \"the simulation "
         "assumes pi ~ a fixed sigmoid function of T1 throughput\". The note "
         "that says so is headed \"Status: Open. Conceptual proposal; no code "
         "changes attached\" (:3).")]
   [:wp09-partition :s3
    (str "the capacity partition's operational indicators do not track "
         "CHANGED practice. Pre-registration detection, persistent-identifier "
         "detection, metadata-completeness scoring and CRediT detection "
         "(mission/:243) are cross-sectional reads of the scholarly record at "
         "institution level; none is linked to a trainee, none has a "
         "before-and-after, and the site says the sector-scale version is "
         "missing (mission/:287). So the evidence the METHOD can currently "
         "offer satisfies s2 (something is reserved for) without satisfying "
         "s3 (it tracks changed practice), which is the split I14 exists to "
         "make visible. The one instrument that does track changed practice, "
         "the T1 wave, is outside the method's capacity partition entirely.")]])

;; [what-was-looked-for where-it-was-named how-the-absence-was-established]
(def not-found
  [["the paper carrying the section-2.3 summary table the attestation's evidence edits"
    "attestations.edn record 0 :evidence :excerpt, which is a proposed cell of a table whose header is \"What exists | What is missing | Relates to\" and whose first row is \"Quarterly delivery counts (2,816 trainees, 24 institutions)\""
    "`grep -rln '2,816' /home/joe/code` over .md/.org/.txt/.tex, excluding the sweeper snapshots, returns exactly two files — the futon4 mission and the simulation tech note — and neither contains that table or a section 2.3; `grep -rln 'What exists' --include=*.md` returns no UKRN document. The paper is not in the tree"]
   ["confirmation that the cited wording was ever applied"
    "the same :evidence row, which quotes the middle of three candidate wordings"
    "the source record (futon1b textprobe/history-versions-full.edn, id e-3e8f1734-53b8-4fbb-a18d-70fbf0b5a7c2) ends \"OK to apply the middle version, or want a different cut?\" and there is no later record in the corpus dump answering it. The attestation cites a proposal, not text"]
   ["the ORP 2027 survey round, one of the two downstream instruments the evidence names"
    "the same :evidence :excerpt — \"the ORP 2027 survey round will address institution-level change\""
    "`grep -rn 'ORP 2027\\|ORP2027'` over the futon4 mission and the whole of ukrn-services-simulation returns 0 hits. Only the other instrument it names, the planned T1 wave, is at the site (mission/:221, :439, :508)"]
   ["the v11 working paper the tech note argues from"
    "sim/docs/tech_note_v11_four_tier_model_fidelity.md:6, :12, :54, :156 — \"v11 section 1.1\", \"v11 section 1.2\", \"the v11 working paper's verification work\""
    "`find /home/joe/code -maxdepth 4 -name '*v11*'` returns figure scripts, this note, a customer-journeys note and a CSV fixture — no paper; `draft_paper.md`, cited from substrate.clj:857 for the Absorbed definition, does not exist in the repository"]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it either STATES the
   demonstrable / development-work partition or ALIGNS a claim to it — the
   method's THEN, m1/m2 and m3 respectively (I3). Seven of the twenty-three
   mechanisms qualify, so this is not a narrow test that the site struggles to
   meet; it is met, and the question is what it discharges."
  [m]
  (l/conde
   [(mechanism-kindo m :capacity-partition)]
   [(mechanism-kindo m :claim-alignment)]))

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

(defn qualifier-control
  "The witness with I14 dropped: s2 and s3 merged back into the single THEN
   clause they are split from. A merged row counts as carried out by the method
   if EITHER half is, so this reports the most favourable reading of the
   qualifier."
  []
  (let [merged (fn [s] (if (#{:s2 :s3} s) :s2+s3 s))]
    (distinct (map (fn [[s m]] [(merged s) m])
                   (l/run* [s m] (carried-out-by-methodo s m))))))

(defn then-only-control
  "The witness with I15 dropped: s4, taken off the conclusion rather than the
   THEN, is not counted. Reports what the verdict would be on the THEN alone."
  []
  (l/run* [s m] (l/all (carried-out-by-methodo s m) (l/!= s :s4))))

(defn mirror
  "The MVSG deferral, asked what it carries out. Must be EMPTY: it has the same
   shape as :contract-calibration and is a method instance under I3, so a
   witness that counted any conditional claim would attest this edge from a
   passage about an architectural label, where no delivery count, no impact
   promise and no indicator appears."
  []
  (l/run* [s] (carried-out-by-methodo s :mvsg-deferral)))

(defn -main [& _]
  (let [w (witness) p (positive-control)
        q (qualifier-control) t (then-only-control) mi (mirror)
        n-src (count source-obligations)
        distinct-obs (fn [rows] (count (distinct (map first rows))))]
    (println "edge: ukrns/distinguish-delivery-from-practice-change")
    (println "  @how ukrns/calibrate-impact-promises-to-current-indicator-capacity")
    (println "site: futon4 26d1b27 M-or-training-as-learning-system.md")
    (println "      + ukrn-services-simulation ba27028")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println (format "positive control (any mechanism, must be non-empty): %d of %d"
                     (distinct-obs p) n-src))
    (doseq [b p] (println "  " b))
    (println (format "qualifier control (I14 dropped, s2+s3 merged): %d of %d"
                     (distinct-obs q) (dec n-src)))
    (println (format "then-only control (I15 dropped, s4 not counted): %d of %d"
                     (distinct-obs t) (dec n-src)))
    (println "mirror (must be empty):" mi)
    (println)
    (println "method obligations on the record:")
    (doseq [[k line text] method-obligations] (println "  " k (str "flexiarg:" line) text))
    (println "contradictions — obligations the site falsifies rather than omits:")
    (doseq [[mech s _] contradictions] (println "  " mech s))
    (println "not found:")
    (doseq [[what where _] not-found] (println "  " what "--" where))
    (println)
    (println (cond
               (not (seq p))
               "INCONCLUSIVE — the relation finds nothing at all; the fact rows are broken."
               (seq mi)
               "INCONCLUSIVE — the mirror is not silent; the relation is too weak."
               (= (distinct-obs w) n-src)
               "ATTESTED — every source obligation is carried out by the method."
               (seq w)
               (format (str "NOT ATTESTED — the method carries out %d of %d obligations. "
                            "%d of %d are discharged at this site, so the shortfall is "
                            "ownership, not absence: the delivery claims are made from "
                            "the counts directly, the distinction is stated in the "
                            "training track's own prose, and the one instrument that "
                            "would track changed practice sits outside the indicator "
                            "capacity the method partitions. Two obligations are "
                            "contradicted rather than omitted.")
                       (distinct-obs w) n-src (distinct-obs p) n-src)
               :else
               (format "NOT ATTESTED — the method carries out 0 of %d obligations." n-src)))))
