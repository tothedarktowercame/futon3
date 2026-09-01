(ns how-witness-heartbeat
  "A DERIVED behavioural check for ONE attested `@how` edge, written to price
   the check rather than to settle the edge (worklist item :L4; the next step
   named at p4ng/empirics-futon/NOTE-the-chain-layer-exists-and-is-empty.md:105-112).

   The edge under test:

     peripherals/progress-heartbeat-distinct-from-cycle-completion
       @how peripherals/canonical-typed-event-vs-side-channel

   Attested at library/peripherals/attestations.edn:0,
   `:state [:attested-by \"claude-15\"]`, rung 1, `:cited` the source pattern's
   own NEXT-STEPS cross-link line (its flexiarg:151-153).

   Method, after `how-witness-snatch`: a witness is a relation run FORWARD
   against stated facts, so a goal either succeeds or it does not. The
   difference from the snatch witness is where the facts come from. Snatch read
   its facts off the game's design diagrams and the relation did arithmetic on
   them. Here the facts are readings of source files, and the relation joins
   them; see FOUR INVENTIONS below for what that costs.

   THE SITE. Both patterns are about peripherals in general; a behavioural
   check needs one place where the pattern is applied. The site chosen is the
   futon3c multi-watcher and its watchdog, because the source pattern's own
   `+ evidence:` names that incident (flexiarg:107-113, \"multi-watcher fix
   2026-05-24, codex-8\"). Facts below are read at futon3c ffb7e7bc.

   FOUR INVENTIONS — things this check needs that the edge does not supply.
   Each is a judgement made here, not a fact carried by the edge or by either
   flexiarg file:

     I1. Obligations. Neither pattern marks which of its THEN steps is a
         checkable obligation. The five numbered steps of each THEN are
         READ as obligations below; the prose around them is dropped.
     I2. Grounding. Nothing in the edge, in `attestations.edn:0`, or in either
         flexiarg names a file, a function, or a system where the edge could
         be tested. The site was picked from the source pattern's evidence
         paragraph, which is not a machine-readable field.
     I3. A meaning for \"carries out\". `@how` is defined at
         futon3/README-flexiarg.md:171 as \"the named methods by which this
         pattern is carried out\" and says nothing further. The reading used
         here: the method carries out the pattern at a site iff each of the
         source's obligations is discharged there by a mechanism that is an
         instance of the method. Other readings (any one obligation; the
         method merely available; the method recommended in prose) would give
         different verdicts on the same facts.
     I4. The site adjudication itself. Every `site-mechanism` row below is a
         human reading of a source file: that `mark-subtask!` discharges S1,
         that the status map is one channel and not two. core.logic joins
         these rows; it does not produce them.

   Consequence for the price, stated once here and not generalised: the
   relation is ~40 lines and ran in under a second. The rows it runs over are
   the whole cost, and they are reading, not computation."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts: the source pattern's obligations ----------------------
;; Read from library/peripherals/progress-heartbeat-distinct-from-cycle-completion.flexiarg
;; THEN (:54), one row per numbered step (I1).
(def source-obligations
  [[:s1 57 "emit heartbeats from inside the work, not from cycle boundaries"]
   [:s2 62 "separate the heartbeat channel from the cycle channel"]
   [:s3 68 "watchdog measures real silence, not cycle-open-duration"]
   [:s4 73 "heartbeats carry a small payload"]
   [:s5 78 "the regulator is a separate process from the watchdog"]])

;; ---- stated facts: the method pattern's obligations ----------------------
;; library/peripherals/canonical-typed-event-vs-side-channel.flexiarg THEN (:53).
;; Present so the method's own content is on the record; the relation uses
;; `method-instance?` below, which is m1+m2 collapsed to a testable property.
(def method-obligations
  [[:m1 56 "author the event schema as a first-class type in the taxonomy"]
   [:m2 60 "the peripheral emits the event on the canonical transport"]
   [:m3 62 "document the consumer contract"]
   [:m4 65 "update the taxonomy index/docs"]
   [:m5 68 "consume via the same subscription mechanism as other consumers"]])

;; ---- stated facts: the site ----------------------------------------------
;; [mechanism kind site-pointer] — kind is :typed-event when the mechanism is
;; an instance of the method (a named event type on futon3c's canonical
;; transport), :state-field when it is a key in a polled status map.
(def site-mechanisms
  [[:last-progress-at   :state-field "futon3c src/futon3c/watcher/multi.clj:889"]
   [:last-subtask       :state-field "futon3c src/futon3c/watcher/multi.clj:888"]
   [:cycle-timestamps   :state-field "futon3c src/futon3c/watcher/multi.clj:1537"]
   [:watchdog-silence   :state-field "futon3c src/futon3c/process_watchdog.clj:136"]
   [:cyder-last-active  :state-field "futon3c src/futon3c/cyder.clj:145"]
   ;; A real typed event on the same transport, carrying nothing about liveness.
   ;; Present so the mirror below has something to fail on.
   [:job-tool-use-event :typed-event "futon3c src/futon3c/transport/http.clj:546"]])

;; [mechanism obligation-it-discharges] — I4: each row is a read of the file
;; named above, not a computation.
(def discharges
  [[:last-progress-at  :s1]   ; stamped inside mark-subtask!, per unit of work
   [:watchdog-silence  :s3]   ; fires on last-progress-age-ms, not cycle age
   [:last-subtask      :s4]]) ; the payload set beside the stamp
;; :s2 has no row: :last-progress-at and :cycle-timestamps are distinct KEYS
;; in ONE `!state` map (multi.clj:1490-1494) projected through one `status`
;; call (multi.clj:1678). Distinct keys on one channel is not two channels.
;; :s5 has no row: not established either way at this site; recorded as
;; unread rather than as failing.

;; ---- the relation --------------------------------------------------------
(defn source-obligationo [s] (l/fresh [line text] (l/membero [s line text] source-obligations)))
(defn mechanism-kindo [m k] (l/fresh [ptr] (l/membero [m k ptr] site-mechanisms)))
(defn dischargeso [m s] (l/membero [m s] discharges))

(defn method-instanceo
  "A mechanism is an instance of the method iff it is a named typed event on
   the canonical transport — m1 and m2 of `method-obligations`, collapsed to
   the property the site can be read for (I3)."
  [m]
  (mechanism-kindo m :typed-event))

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
  "The one real typed event at the site, asked what it carries out. Must be
   EMPTY: a witness that counted any typed event anywhere in the system would
   attest the edge for a system that never heartbeats at all."
  []
  (l/run* [s] (carried-out-by-methodo s :job-tool-use-event)))

(defn -main [& _]
  (let [w (witness) p (positive-control) m (mirror)
        n-src (count source-obligations)]
    (println "edge: peripherals/progress-heartbeat-distinct-from-cycle-completion")
    (println "  @how peripherals/canonical-typed-event-vs-side-channel")
    (println "site: futon3c ffb7e7bc multi-watcher + process-watchdog")
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
               (format "NOT ATTESTED — the method carries out %d of %d obligations."
                       (count w) n-src)
               :else
               (format (str "NOT ATTESTED — the method carries out 0 of %d obligations. "
                            "%d ARE discharged at this site, by state fields polled "
                            "through one status map, which is the side channel the "
                            "method pattern names.")
                       n-src (count p))))))
