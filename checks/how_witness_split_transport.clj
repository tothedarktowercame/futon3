(ns how-witness-split-transport
  "A DERIVED behavioural check for ONE attested `@how` edge — the second one
   priced, after `how-witness-heartbeat` (worklist item :L10 slice 1,
   generalising :L4; the step named at
   p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112).

   The edge under test:

     peripherals/split-transport-from-embodiment
       @how peripherals/read-existing-seam-before-implementing

   Attested at library/peripherals/attestations.edn:4,
   `:state [:attested-by \"claude-15\"]`, rung 1. `:cited` is again the source
   pattern's own NEXT-STEPS cross-link line (its flexiarg:124-125, \"For each
   layer, do an independent seam-read\") — the same aspiration-not-application
   shape :L4 recorded as its second finding, reproduced here independently on a
   different edge.

   Same method as `how-witness-heartbeat`: a relation run FORWARD over stated
   facts, with a positive control and a mirror, so an empty witness cannot be
   confused with a relation that cannot see.

   THE SITE. The source pattern's second `+ evidence:` paragraph (flexiarg:107-112)
   names one: \"futon3c War Machine: the AIF backend emits structured events; the
   markdown renderer (Emacs *War Machine* buffer) and web UI (hex layout) are two
   embodiments of the same transport. Adding the R-Criterion Status section to the
   markdown renderer did not require touching the web UI, and vice versa, precisely
   because the split was honoured.\"

   Locating it cost more than :L4's site did, and the reason is a fact about the
   edge, not about this check:

     (a) The paragraph says futon3c. The code is in futon2 and futon4. futon3c
         holds only the HTTP route that publishes it (http.clj:6918-6928).
     (b) `R-Criterion Status` does not occur anywhere in futon3c, at HEAD or in
         history (`git log --all -S`), so the named incident cannot be found by
         searching the repository the paragraph names.
     (c) The mission the change is attributed to, M-war-machine-frontend-upgrade1
         §6.20, is cited from code (war_machine.clj:3702, api.cljs:18) and exists
         as a file in NO repository of the stack (futon0..futon5 searched).

   Facts below are read at futon2 f5d4d0a, futon4 26d1b27, futon3c e78ac336.

   THE FOUR INVENTIONS of `how-witness-heartbeat` (I1 obligations, I2 grounding,
   I3 a meaning for \"carries out\", I4 site adjudication) all recur here
   unchanged, so they are not restated. A fifth is specific to this edge:

     I5. Which embodiment. The site has THREE renderings of the same R-criterion
         state — the markdown section, the hex web UI, and a VSATARCS elisp
         chrome that re-parses the source document. The evidence paragraph names
         two. Treating the third as part of the site is a decision made here;
         it is what turns S4 from satisfied into unread-as-satisfied, so it
         changes the verdict."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; library/peripherals/split-transport-from-embodiment.flexiarg THEN (:48),
;; one row per numbered step plus the two rules stated as prose after them (I1).
(def source-obligations
  [[:s1 52 "design the transport layer on its own terms — events, schema, channel, reliability, backpressure, consent — describable without reference to any UI surface"]
   [:s2 59 "design the embodiment layer as a function of transport events"]
   [:s3 65 "the transport layer never imports from the embodiment layer; the embodiment subscribes"]
   [:s4 67 "a new embodiment is added by writing a new subscriber; a new transport by porting the schema"]
   [:s5 70 "the mission name encodes both layers"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/peripherals/read-existing-seam-before-implementing.flexiarg THEN (:52).
;; On the record so the method's own content is visible; the relation uses
;; `method-instanceo` below, which is the property the site can be read for (I3).
(def method-obligations
  [[:m1 55 "transport seam — is there a canonical pipeline to extend? if not, write down why not"]
   [:m2 59 "event seam — extend the existing taxonomy as first-class events"]
   [:m3 64 "registration seam — register, do not invent a parallel identity"]
   [:m4 67 "consent seam — locate the observing/writing boundary"]
   [:m5 71 "prior-art seam — read the structurally similar prior build fully"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — kind is :recorded-seam-read when the mechanism
;; is an instance of the method (a seam-read WRITTEN DOWN at the site, which is
;; the only form of it a later reader can check), and names what it is otherwise.
(def site-mechanisms
  [;; The transport seam, read and recorded: the viewer is put on the existing
   ;; futon3c JVM rather than a second HTTP server, and the wire keeps namespaced
   ;; keys. That is m1 and m2 of `method-obligations`, in prose, at the seam.
   [:wm-json-route      :recorded-seam-read "futon3c src/futon3c/transport/http.clj:6918-6928"]
   ;; The prior-art seam, read and recorded: the Clojure parser names the elisp
   ;; parser it mirrors, "same source-of-truth, different consumer surface".
   ;; m5. Recorded, and structurally inert — see the mirror.
   [:mirror-parser-note :recorded-seam-read "futon2 scripts/futon2/report/war_machine.clj:5427-5431"]
   ;; The transport payload the route serves: :r-criteria is a key on scan-data.
   [:scan-data-map      :transport-payload  "futon2 scripts/futon2/report/war_machine.clj:5600"]
   ;; The markdown embodiment is NOT called with scan-data; it is called with a
   ;; separately hand-listed subset map, so a new field must be threaded twice.
   [:render-arg-map     :hand-threading     "futon2 scripts/futon2/report/war_machine.clj:5621"]
   [:markdown-section   :embodiment         "futon2 scripts/futon2/report/war_machine.clj:3702-3730"]
   ;; The web embodiment subscribes to the whole JSON blob and ignores keys it
   ;; does not render — which is why adding :r-criteria did not touch it.
   [:hex-web-ui         :subscriber         "futon2 web/war-machine/src/war_machine/client/api.cljs:22"]
   ;; The third rendering of the same state: it re-parses the source markdown
   ;; document with its own regex and its own status vocabulary, off-transport.
   [:vsatarcs-chrome    :duplicate-parser   "futon4 dev/arxana-vsatarcs-r-criteria-wm.el:42"]
   ;; The observation layer CALLS the markdown renderer and returns its output.
   ;; Present so the inversion is on the record; it discharges nothing.
   [:generate-calls-render :inverted-import "futon2 scripts/futon2/report/war_machine.clj:5612"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:wm-json-route :s1]   ; a JSON route with a stated key-encoding rule, named
                          ; without reference to any surface
   [:hex-web-ui    :s2]   ; fetch → ratom → render: presence as a function of
                          ; transport events
   [:hex-web-ui    :s3]]) ; the web embodiment subscribes; nothing on the
                          ; transport side imports it
;; :s3 gets no second row, and the reason is worth reading: in the MARKDOWN
;; direction the rule is inverted. `generate-war-machine` calls
;; `render-war-machine` and returns `:markdown` in its own result
;; (war_machine.clj:5612-5626), so the observation layer imports the embodiment.
;; The rule holds toward the web and fails toward the markdown, at one site.
;; :s4 has no row: adding :r-criteria required editing TWO maps in one function
;; (:5600 and :5621), and the third rendering was added as a new PARSER of the
;; source document (futon4), not as a new subscriber. A new embodiment here is
;; not a new subscriber.
;; :s5 has no row: the mission M-war-machine-frontend-upgrade1 exists in no
;; repository of the stack, so whether its name encodes both layers cannot be
;; read. Recorded as unread rather than as failing.

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is a seam-read recorded at
   the site. An unrecorded seam-read may well have happened; it is not a thing
   a later reader can check, so it is not a fact here (I3)."
  [m]
  (mechanism-kindo m :recorded-seam-read))

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
  "The recorded seam-read that changed no structure, asked what it carries out.
   Must be EMPTY: a witness that counted any recorded seam-read would attest the
   edge on the strength of a comment naming a mirror parser that the split was
   never actually applied to."
  []
  (l/run* [s] (carried-out-by-methodo s :mirror-parser-note)))

(defn -main [& _]
  (let [w (witness) p (positive-control) m (mirror)
        n-src (count source-obligations)]
    (println "edge: peripherals/split-transport-from-embodiment")
    (println "  @how peripherals/read-existing-seam-before-implementing")
    (println "site: futon2 f5d4d0a War Machine scans + web viewer,")
    (println "      futon4 26d1b27 VSATARCS chrome, futon3c e78ac336 JSON route")
    (println)
    (println "witness [obligation mechanism] — obligations the METHOD carries out:")
    (doseq [b w] (println "  " b))
    (when (empty? w) (println "   (none)"))
    (println "positive control (any mechanism, must be non-empty):")
    (doseq [b p] (println "  " b))
    (println "mirror (must be empty):" m)
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
                            "%d ARE discharged at this site; the other %d are discharged "
                            "by the transport's own shape, with no seam-read recorded "
                            "that produced them.")
                       (count w) n-src (count p) (- (count p) (count w)))
               :else
               (format "NOT ATTESTED — the method carries out 0 of %d obligations." n-src)))))
