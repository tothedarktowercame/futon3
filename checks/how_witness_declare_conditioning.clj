(ns how-witness-declare-conditioning
  "A DERIVED behavioural check for ONE attested `@how` edge — the third one
   priced, after `how-witness-heartbeat` (:L4) and `how-witness-split-transport`
   (:L10 slice 1), and the FIRST outside `library/peripherals/`.

   The edge under test:

     aif/declare-the-conditioning
       @how aif/structured-observation-vector

   Attested at library/aif/attestations.edn:2, `:state [:attested-by
   \"claude-15\"]`, rung 1. `:cited` is once again the source pattern's own
   NEXT-STEPS line (its flexiarg:31, \"Route all lanes toward ONE situation
   source (the structured observation vector) where feasible\") — an aspiration,
   not an application. That is now THREE of three attested `@how` edges priced,
   across TWO sections, whose citation is a NEXT-STEPS line; :L4 recorded it
   first as a finding about one section.

   Same method as the two earlier witnesses: a relation run FORWARD over stated
   facts, with a positive control and a mirror, so an empty witness cannot be
   confused with a relation that cannot see.

   THE SITE, and it is a real one. The source pattern's `+ evidence:` paragraph
   (flexiarg:27) names futon2's M-evaluate-policies §9.6–9.7, its ARGUE exhibit's
   \"conditions on\" column, and the E5 conditioned-cascade probe. All three exist
   and were read: holes/M-evaluate-policies.md:1037-1123,
   scripts/exhibit_cascade_argue.py, scripts/e5_conditioned_probe.py and
   holes/labs/M-evaluate-policies/e5-conditioned-probe.json. Unlike :L10 slice 1
   the paragraph IS a pointer, and unlike :L4 there is no
   :no-site-to-check-against finding to record.

   Facts below are read at futon2 76bdcd0 and futon3c 9128f5a8.

   THE FOUR INVENTIONS of `how-witness-heartbeat` (I1 obligations, I2 grounding,
   I3 a meaning for \"carries out\", I4 site adjudication) recur unchanged. Two
   are specific to this edge:

     I6. Both patterns state their THEN as a single sentence, so every obligation
         row below cites the same line (:22 in each flexiarg) and is labelled by
         its clause. :L4 and slice 1 could cite one line per numbered step.

     I7. A typed feature map SUPPLIES a situation; the source pattern asks a
         surface to DECLARE which situation-slice it consumed. Whether supplying
         can discharge declaring is the adjudication this edge turns on, and it
         is made here: it can, but only where a surface actually consumes the
         typed object. A surface that consumes a rendering of it has consumed a
         different slice, and the declaration is exactly what would have to say
         so."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/aif/declare-the-conditioning.flexiarg THEN (:22), one row per clause
;; of the single sentence (I1, I6).
(def source-obligations
  [[:s1 22 "SENSE — declare which formal object the surface claims"]
   [:s2 22 "SELECTION-SEMANTICS — extremum / amortized sample / bound, over what menu"]
   [:s3 22 "CONDITIONING — declare what situation-slice it consumed"]
   [:s4 22 "render the declaration where the number is rendered"]
   [:s5 22 "score the fit, never the form alone — the world supplies the antecedent"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/aif/structured-observation-vector.flexiarg THEN (:22). On the record
;; so the method's own content is visible; the relation uses `method-instanceo`
;; below, which is the property a site can be read for (I3).
(def method-obligations
  [[:m1 22 "maintain an explicit (o) — per step, not per surface"]
   [:m2 22 "typed"]
   [:m3 22 "normalized"]
   [:m4 22 "a feature map derived from session state"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — kind is :typed-observation-vector when the
;; mechanism IS an instance of the method (a typed, normalized per-channel map
;; that a later reader can open), and names what it is otherwise.
(def site-mechanisms
  [;; The exhibit's method note enumerates the four senses of "EFE", and the
   ;; per-cascade rows carry "(sense iii)" / "(sense i)" labels beside the value.
   [:four-senses-note    :declaration        "futon2 scripts/exhibit_cascade_argue.py:192-195"]
   [:sense-labels-in-rows :declaration       "futon2 scripts/exhibit_cascade_argue.py:229,231"]
   ;; The column the evidence paragraph names, in the same table as the number.
   [:conditions-on-column :declaration-column "futon2 scripts/exhibit_cascade_argue.py:202-205,221-231"]
   ;; Selection semantics appears at this site ONLY as repair item (v) of a list
   ;; of things that "would earn back the sentence EFE-scored".
   [:selection-semantics-repair :unbuilt-repair "futon2 scripts/exhibit_cascade_argue.py:360-361"]
   ;; The two rows that name the METHOD name it in the subjunctive.
   [:blend-row-subjunctive :counterfactual   "futon2 scripts/exhibit_cascade_argue.py:229 \"(would: live observation vector)\""]
   [:core-g-row-subjunctive :counterfactual  "futon2 scripts/exhibit_cascade_argue.py:231 \"(would: live observation vector)\""]
   ;; The blend row is not merely undeclared, it is unscored: a self-describing
   ;; placeholder. Nothing to condition.
   [:blend-placeholder   :placeholder        "futon2 scripts/futon2/report/war_machine.clj:5090 (or dG 0.0), :score-provenance :placeholder at :5093"]
   ;; A genuine instance of the method in the production lane: 14 declared
   ;; channels, each normalized to [0,1], derived from scan state per tick.
   [:atomic-lane-observe :typed-observation-vector "futon2 src/futon2/aif/observation.clj:103 (channels :17-31), called at scripts/futon2/report/war_machine.clj:4684"]
   ;; A second instance: the per-channel map persisted in each wm-trace tick,
   ;; carrying :value :preferred :gap :in-range? per channel. This is the object
   ;; E5 reaches for.
   [:tick-per-channel-map :typed-observation-vector "futon2 data/wm-trace/*.edn [:free-energy :per-channel], built at src/futon2/aif/free_energy.clj:94-108, read at scripts/e5_conditioned_probe.py:72"]
   ;; ...and here the typing is discarded: the six largest out-of-range gaps are
   ;; `format`ted into one English sentence and truncated to 400 characters.
   [:obs-summary-prose   :flattening         "futon2 scripts/e5_conditioned_probe.py:76-83"]
   ;; What the valuation actually consumed: that sentence, string-concatenated
   ;; onto the 160-char psi scrap and handed to the embedding constructor.
   [:psi-concat          :prose-conditioning "futon2 scripts/e5_conditioned_probe.py:132 (cascade (f\"{psi} {obs}\"))"]
   ;; The experiment says so itself, before the result.
   [:e5-caveat           :recorded-nonuse    "futon2 holes/labs/M-evaluate-policies/e5-conditioned-probe.json :honesty-caveats :a-prose-conditioning"]
   ;; Not at this site, in another repository, against another surface — and a
   ;; fully typed observation vector. See the mirror.
   [:futon3c-mission-channels :typed-observation-vector "futon3c src/futon3c/aif/observe.clj:1-14 (10 normalized channels), named as grounding at src/futon3c/aif/mission_head.clj:10"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:four-senses-note      :s1]   ; four senses enumerated in the method note
   [:sense-labels-in-rows  :s1]   ; and carried on each row beside its value
   [:conditions-on-column  :s3]   ; a situation-slice stated per valuation
   [:conditions-on-column  :s4]   ; in the valuation table, beside the number
   [:psi-concat            :s5]]) ; supplying the antecedent moved the answer:
                                  ; 11/13 top-pattern flips, mean |dF| 1.95
                                  ; (e5-conditioned-probe.json :aggregate)
;; :s2 has no row. At this site selection semantics is named only as repair item
;; (v) of what "would earn back the sentence EFE-scored"
;; (exhibit_cascade_argue.py:360). Recorded as unbuilt, not as failing.

;; ---- what the declaration would have had to say --------------------------
;; The exhibit writes the conditioning of two rows as one phrase, "the live
;; observation vector". Read forward, that phrase names four different objects.
;; Counts measured at futon2 76bdcd0 against data/wm-trace/wm-trace-2026-09-01.edn,
;; not asserted.
(def conditioning-slices
  [[14 "channels DECLARED"           "src/futon2/aif/observation.clj:17-31"]
   [13 "reach the tick's :per-channel map — free_energy.clj:94 iterates (pref/current-C), whose keyset is the 13 of preferences.clj:11-24; :depositing-signal is observed (observation.clj:144) and has no preference entry, so it never enters the conditioning"
       "src/futon2/aif/preferences.clj:11-24"]
   [6  "carry a weight into g-pragmatic"  "src/futon2/aif/preferences.clj:50-58, summed at free_energy.clj:109-111"]
   [6  "reach E5's prose summary — the six largest out-of-range gaps, a DIFFERENT six from the weighted six"
       "scripts/e5_conditioned_probe.py:78"]])

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is a typed, normalized
   per-channel map a later reader can open. A rendering of one is not one — it
   is the thing the source pattern would have wanted declared as a different
   slice (I7)."
  [m]
  (mechanism-kindo m :typed-observation-vector))

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

(defn mirror
  "A real typed observation vector, in another repository, against another
   surface, asked what it carries out here. Must be EMPTY: a witness that
   counted it would attest the edge on the strength of the method existing
   SOMEWHERE in the stack rather than at the site the evidence names."
  []
  (l/run* [s] (carried-out-by-methodo s :futon3c-mission-channels)))

(defn -main [& _]
  (let [w (witness) p (positive-control) m (mirror)
        n-src (count source-obligations)]
    (println "edge: aif/declare-the-conditioning")
    (println "  @how aif/structured-observation-vector")
    (println "site: futon2 76bdcd0 M-evaluate-policies ARGUE exhibit + E5 probe;")
    (println "      mirror at futon3c 9128f5a8 Mission Peripheral AIF head")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println "positive control (any mechanism, must be non-empty):")
    (doseq [b p] (println "  " b))
    (println "mirror (must be empty):" m)
    (println)
    (println "what \"conditions on: the live observation vector\" names, read forward:")
    (doseq [[n what where] conditioning-slices]
      (println (format "   %2d %s" n what))
      (println (str "      " where)))
    (println)
    (println (cond
               (not (seq p))
               "INCONCLUSIVE — the relation finds nothing at all; the fact rows are broken."
               (seq m)
               "INCONCLUSIVE — the mirror is not silent; the relation is too weak."
               (= (count w) n-src)
               "ATTESTED — every source obligation is carried out by the method."
               (seq w)
               (format (str "NOT ATTESTED — the method carries out %d of %d obligations. "
                            "%d ARE discharged at this site, by prose declarations and by a "
                            "prose rendering of the method's output.")
                       (count w) n-src (count p))
               :else
               (format (str "NOT ATTESTED — the method carries out 0 of %d obligations. "
                            "The typed vector IS read at the site and is flattened to a "
                            "400-character sentence before any valuation consumes it "
                            "(e5_conditioned_probe.py:76-83, :132); the two exhibit rows that "
                            "name it name it in the subjunctive. Not absence — de-typing at "
                            "the boundary.")
                       n-src)))))
