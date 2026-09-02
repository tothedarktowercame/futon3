(ns playout-snatch
  "A pattern-theoretic playout of Snatch or Share.

   At each round we ask three things and record the answers:
     1. COVERAGE  — which pattern's IF matches this situation? (none = a gap)
     2. PATH      — does the trajectory trace edges of the @how graph?
     3. Q         — how does item S-001's prediction fare?

   Deterministic and reproducible: P2's disposition is fixed per run, so the
   trace can be read rather than sampled."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint]
            [find-organise :as fo]))

;; ---- the collection, as (IF-guard, THEN) pairs -------------------------
;; Each guard is a predicate on the situation. This IS the pattern's IF clause,
;; encoded — the point of the exercise is that some situations match nothing.
;;
;; ONE flat collection, three grains.  Nothing separates the grains but the
;; conjunct at the head of every guard, which is the claim LA1c §3.9 makes and
;; `grain-separation` below is the control for.
(def collection
  [{:id :protect-the-unprotected-move :grain :design
    :if (fn [s] (= :design (:grain s)))}
   {:id :preserve-the-right-to-abstain :grain :design
    :if (fn [s] (and (= :design (:grain s)) (:rule-proposed? s)))}
   {:id :mark-without-force :grain :design
    :if (fn [s] (and (= :design (:grain s)) (:want-sanction? s)))}
   {:id :revert-then-invert :grain :design
    :if (fn [s] (and (= :design (:grain s)) (:judge-available? s)))}
   {:id :non-binding-talk-still-moves-play :grain :design
    :if (fn [s] (and (= :design (:grain s)) (not (:judge-available? s))))}
   {:id :institutions-vary-by-position-and-force :grain :design
    :if (fn [s] (= :design (:grain s)))}

   ;; --- PLAY grain: antecedents over game states -------------------------
   ;; A play-grain entry may carry :then — the action its THEN clause
   ;; recommends, as a function of the situation — and :precedence, the
   ;; conflict-resolution order when several fire.  Entries with no :then are
   ;; advisory here: their THEN speaks about something this model does not
   ;; carry (ask size, a talk channel, a mark that changes no payoff).
   ;;
   ;; No guard may mention the action P1 took this round.  A pattern advises a
   ;; decision; conditioning its IF on the decision inverts the exercise.
   ;;
   ;; The ANTECEDENT is :if AND :however.  IF names the situation; HOWEVER names
   ;; the counter-force the THEN exists to overcome.  Where the force is not
   ;; live the THEN would happen anyway and the pattern has nothing to add, so
   ;; both conjuncts must hold for a pattern to fire.
   {:id :an-unmodelled-response-stops-the-line :grain :play :precedence 1
    :if (fn [s] (and (= :play (:grain s)) (:unmodelled? s)))
    ;; "Play continues whether or not you notice" — in the final round it does
    ;; not, so there is nothing left for a stop to protect.
    :however (fn [s] (not (:last-round? s)))
    :then (fn [_] {:act :stop})}
   {:id :consult-the-remedy-before-exiting :grain :play :precedence 2
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s)
                     (or (:shame-available? s) (:judge-available? s))))
    ;; "a policy written for the state of nature exits on the first defection" —
    ;; the temptation requires that exiting be possible at all.
    :however (fn [s] (contains? (:p1-actions s) :abstain))
    ;; "check what the arrangement entitles you to and use it" — denounce while
    ;; the remedy is unspent, and say nothing once it is spent.  Denouncing is
    ;; optional in G4, so it is an action, not an automatic consequence.
    :then (fn [s] (when (and (:judge-available? s) (pos? (:seized s))
                             (not (:denounced? s)))
                    ;; the size of a denunciation is the size of the seizure
                    {:act :denounce :size (:seized s)}))}
   {:id :re-enter-after-observed-repair :grain :play :precedence 3
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s)
                     (:repair-observed? s) (pos? (:tokens s))))
    ;; "permanent exit discards any improvement" — live once the remedy is spent
    ;; and exit is the only remaining alternative to re-entry.
    :however (fn [s] (zero? (:seized s)))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}
   {:id :forced-play-needs-a-loss-floor :grain :play :precedence 4
    :if (fn [s] (and (= :play (:grain s)) (:forced-offer? s) (:snatched? s)))
    ;; "compulsion does not make a large exposed offer safer" — there is no
    ;; large offer to be tempted by when the floor is all you hold.
    :however (fn [s] (> (:tokens s) 1))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}
   {:id :escalate-only-as-far-as-you-can-lose :grain :play :precedence 5
    :if (fn [s] (and (= :play (:grain s)) (= :accepted (:last s))))
    ;; NON-DISCRIMINATING.  "One acceptance is weak evidence" and a snatch is
    ;; always possible, so this force is live in every state where the IF holds.
    ;; It names a permanent condition of the game rather than a tension in the
    ;; situation, and is recorded as such instead of being given a guard.
    :however (fn [_] true)
    :then (fn [s] (when (pos? (:tokens s))
                    {:act :offer :size (min (inc (:last-size s)) (:tokens s))}))}
   {:id :probe-before-committing :grain :play :precedence 6
    :if (fn [s] (and (= :play (:grain s)) (nil? (:disposition-known s))))
    ;; "a large first offer is unrecoverable ... a zero offer buys nothing" —
    ;; both errors need to be reachable for the sizing advice to bite.
    :however (fn [s] (> (:tokens s) 1))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}
   {:id :exchange-when-both-sides-gain :grain :play :precedence 7
    :if (fn [s] (and (= :play (:grain s)) (pos? (:tokens s))))
    ;; "attention to seizure risk can make abstention look like the objective" —
    ;; live only where seizure is salient: it has happened, or the counterpart
    ;; is still unread.  Against a known sharer the exchange needs no argument.
    :however (fn [s] (or (:snatched? s) (nil? (:disposition-known s))))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}

   ;; Advisory in this harness — real game features the model does not carry.
   {:id :ask-for-surplus-not-surrender :grain :play
    :if (fn [s] (and (= :play (:grain s)) (pos? (:tokens s))))}
   {:id :a-free-mark-is-always-worth-assigning :grain :play
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s) (:shame-available? s)))}
   {:id :use-talk-to-make-a-testable-offer :grain :play
    :if (fn [s] (and (= :play (:grain s)) (:chat-available? s) (nil? (:last s))))}
   {:id :price-the-final-round-as-final :grain :play
    :if (fn [s] (and (= :play (:grain s)) (:last-round? s)))}
   ;; A P2 pattern.  P2 here is a fixed disposition, not a chooser, so it is
   ;; excluded from P1's set; :offer-received? is P2's view and is never set.
   {:id :accept-an-offer-that-beats-holding :grain :play :actor :p2
    :if (fn [s] (and (= :play (:grain s)) (:offer-received? s)))}

   ;; --- POLICY grain: antecedents over the CASCADE, not over game state ----
   ;; LA1c-restatement.md §3.1: Sit policy = CascadeState, Act policy =
   ;; CascadeEdit.  The operand is the cascade — its membership and its
   ;; precedence field — so no guard here may read :tokens, :snatched? or any
   ;; other field of a play situation, and none does.
   ;;
   ;; Two of the six policy-grain patterns LA1 slice (b) authored are encoded.
   ;; `unexecuted-policy-patterns` below names the other four and the edit each
   ;; would need, so the gap is a list rather than an impression.
   ;;
   ;; `:then-source` points at the authored THEN this encoding claims to be an
   ;; encoding OF.  `library-correspondence` re-reads those lines on every run;
   ;; without it the six files and these rules are two collections that merely
   ;; look alike, which is the facade LA1c §11 names.
   {:id :play-the-authored-order-first :grain :policy :precedence 1
    :then-source "library/snatch/play-the-authored-order-first.flexiarg:25-28"
    ;; "You have a cascade with an authored precedence and are considering a
    ;; temperament that re-wires it."
    :if (fn [s] (and (= :policy (:grain s)) (seq (:precedence s))))
    ;; "An empty edit looks like no policy at all, so it goes unnamed" — the
    ;; force is live exactly while nothing has been recorded on this
    ;; construction.  Once the temperament is on the record it has been named,
    ;; and the rule would be repeating itself.
    :however (fn [s] (empty? (:record s)))
    ;; The identity temperament's edit is the empty one.  What it still has to
    ;; do is BE RECORDED, "exactly as a non-empty one would be" — which is what
    ;; a halt with a stated reason is.
    :then (fn [_] (fo/halt :authored-order-is-the-baseline))}
   {:id :lead-with-the-exchange-rule :grain :policy :precedence 1
    :then-source "library/snatch/lead-with-the-exchange-rule.flexiarg:28-31"
    ;; "You want a temperament that leads with gains from trade" — so the
    ;; exchange rule has to be in the cascade to be led with.
    :if (fn [s] (and (= :policy (:grain s))
                     (contains? (:members s) :exchange-when-both-sides-gain)))
    ;; The counter-force is the one-node cascade the promotion produces, and it
    ;; is live only while the exchange rule is not already first: once it is,
    ;; promoting it again would change nothing and the pattern has nothing to
    ;; add.  This is also what terminates the construction.
    :however (fn [s] (not= :exchange-when-both-sides-gain (first (fo/ordered s))))
    :then (fn [s] (fo/promote-above :exchange-when-both-sides-gain (first (fo/ordered s))))}])

;; P1's set only: a pattern whose actor is P2 is not decidable by this harness.
;; That exclusion is the GAME's, so it stays here; `fo/fires?` is the generic
;; antecedent test (IF and HOWEVER together) -- worklist :LA3 moved it there --
;; and it knows nothing about P2.
(defn fires? [pat s]
  (and (not= :p2 (:actor pat)) (fo/fires? pat s)))

(defn applicable [s] (mapv :id (filter #(fires? % s) collection)))

;; ---- treatments, from the flowcharts -----------------------------------
(def treatments
  {:g1 {:p1-actions #{:abstain :offer} :shame-available? false :judge-available? false :chat-available? false}
   :g2 {:p1-actions #{:offer} :forced-offer? true :shame-available? false :judge-available? false :chat-available? false}
   :g3 {:p1-actions #{:abstain :offer} :shame-available? true  :judge-available? false :chat-available? false}
   :g4 {:p1-actions #{:abstain :offer} :shame-available? true  :judge-available? true  :chat-available? false}
   :g5 {:p1-actions #{:abstain :offer} :shame-available? false :judge-available? false :chat-available? true}})

;; ---- P2, by disposition -------------------------------------------------
(defn p2-response [disposition] (case disposition :sharer :O2 :snatcher :O4 :cautious :O3))

;; ---- two policies -------------------------------------------------------
;; GRIM TRIGGER: offer one token until snatched, then abstain for the rest of
;; the game, with no way back.  Hardcoded — the collection cannot reach it.
(defn pi-grim [s _pats]
  (if (or (zero? (:tokens s))                 ; cannot offer what you do not hold
          (and (:snatched? s) (not (:forced-offer? s))))
    {:act :abstain}
    {:act :offer :size 1}))

;; PATTERN-DRIVEN: the action is whatever the highest-precedence firing
;; pattern's THEN says.  Editing a pattern changes what P1 does.
;; `overrides` re-wires the precedence order without touching a pattern.  Same
;; collection, different wiring, different policy — which is what
;; futon2.aif.cascade-prior claims of cascades, made testable here.
;;
;; `overrides` is no longer written here: `temperament-overrides` below derives
;; it by firing a policy-grain rule (worklist :LA2).  This function keeps the
;; argument because the argument is the interface between the two grains — what
;; changed is who writes it.
(defn pattern-policy [overrides]
  (fn [s pats]
    (let [fired (set pats)
          order #(get overrides (:id %) (:precedence %))]
      (if-let [[pat a] (fo/fire (filter #(fired (:id %)) collection) order s)]
        (assoc a :by (:id pat))
        {:act :abstain :by :no-pattern}))))

;; ---- the cascade under construction, and the loop that edits it ---------
;; What a temperament orders: the play-grain rules that carry a THEN.  A rule
;; with no THEN is advisory in this harness and there is nothing to consult it
;; for, so it is not in the seed.
(def play-rules (filterv #(and (= :play (:grain %)) (:then %)) collection))

(def authored-precedence (into {} (map (juxt :id :precedence)) play-rules))

(defn initial-cascade-state
  "C₀ of the construction loop (LA1c §4.1): the seed's patterns, provenance
   `:found`, precedence the authored one, no flags and an empty record.
   `:grain :policy` is the field every policy-grain guard checks — the same
   mechanism that keeps a design rule out of a play situation."
  []
  {:grain :policy
   :members (into #{} (map :id) play-rules)
   :authored-order (mapv :id play-rules)
   :precedence authored-precedence
   :provenance (into {} (map (juxt :id (constantly :found))) play-rules)
   :flags #{}
   :record []
   :halted nil})

(def ^:private rule-by-id (into {} (map (juxt :id identity)) collection))

(defn construct
  "`fo/construct` (LA1c §4.1) with this collection's nodes resolved.  The loop
   itself is not here: `:LA3` moved it to `find_organise.clj` so that the
   library constructor fires the same one."
  ([temperament] (construct temperament (initial-cascade-state)))
  ([temperament state]
   (fo/construct temperament (mapv rule-by-id (:nodes temperament)) state)))

(defn temperament-overrides
  "The `overrides` map `pattern-policy` takes, DERIVED: the entries of the
   constructed cascade's precedence field that differ from the authored one.

   This is the argument worklist :LA2 exists to stop hand-writing.  Before it,
   the two maps below were literals in this file and no authored pattern reached
   them; now each is what a policy-grain THEN emitted."
  [temperament]
  (let [final (construct temperament)]
    (into {} (remove (fn [[id n]] (= n (get authored-precedence id))))
          (:precedence final))))

;; The two temperaments, as CASCADES at policy grain — LA1c §3.5: a Policy is a
;; cascade of policy-grain patterns, not a fourth kind of thing.  Each has one
;; node here, so nothing about their own precedence is exercised; ordering
;; temperaments against each other is the hand-authorship §3.8 says this design
;; moves up a level rather than removes.
(def identity-temperament
  {:id :the-authored-order :grain :policy
   :nodes [:play-the-authored-order-first]
   :precedence {:play-the-authored-order-first 1}})

(def trading-temperament
  {:id :exchange-first :grain :policy
   :nodes [:lead-with-the-exchange-rule]
   :precedence {:lead-with-the-exchange-rule 1}})

(def recorded-overrides
  "What the two maps were as literals before :LA2 — verbatim from
   `playout_snatch.clj:161,166` at futon3 4e1c410.  The acceptance test of the
   row is that the derived maps equal these, so the record is kept here rather
   than in the commit message."
  {:the-authored-order {}
   :exchange-first {:exchange-when-both-sides-gain 0}})

(def patterns-overrides (temperament-overrides identity-temperament))
(def exchange-first-overrides (temperament-overrides trading-temperament))

;; Checked at load, not only in -main: `derive_q_snatch.clj`, `ablate_g_snatch.clj`
;; and `find_snatch.clj` all take these two maps through `pi-patterns` /
;; `pi-exchange-first`, and a derivation that drifted would move their artefacts
;; without anyone running this file's report.
(doseq [[temperament derived] [[:the-authored-order patterns-overrides]
                               [:exchange-first exchange-first-overrides]]]
  (when-not (= derived (get recorded-overrides temperament))
    (throw (ex-info "the derived overrides differ from the recorded literals"
                    {:finding :derived-overrides-drifted
                     :temperament temperament :derived derived
                     :recorded (get recorded-overrides temperament)}))))

(def pi-patterns (pattern-policy patterns-overrides))

;; One re-wiring: put the gain pattern above the remedy and the stop.  Nothing
;; about any pattern changes; only the order in which they are consulted.
(def pi-exchange-first (pattern-policy exchange-first-overrides))

(def ^:private modelled #{:O1 :O2 :O4})   ; S-001's support; O3 carries zero mass

;; P1's payoff.  Own tokens score 1, the counterpart's score 2 (README, "The
;; game"), so a completed exchange of n nets +n and a snatched offer nets -n.
;; A denunciation is the AutoJudge's two steps from `revert-then-invert`:
;; revert returns the n seized tokens (+n), invert transfers what P1 asked for
;; at P2's expense (+2n).
(defn- payoff [act size outcome]
  (case act
    :offer (case outcome :O2 size :O4 (- size) 0)
    :denounce (* 3 size)
    0))

(defn play
  "A PLAY-grain trajectory under `policy`. Every situation carries :grain :play."
  [policy treatment disposition rounds]
  (loop [r 1
         s (merge (treatments treatment)
                  {:grain :play :treatment treatment :round 1
                   :snatched? false :repair-observed? false :denounced? false
                   :seized 0 :last-size 1 :tokens 10 :shame 0 :score 0})
         trace []]
    (if (> r rounds)
      trace
      (let [situation (assoc s :last-round? (= r rounds))
            pats (applicable situation)
            {:keys [act size by]} (policy situation pats)
            size (or size 0)]
        (if (= act :stop)
          (conj trace {:round r :patterns pats :action :stop :outcome :line-stopped
                       :size 0 :score (:score s) :by by})
          (let [out (case act
                      :offer (p2-response disposition)
                      :denounce :repaired
                      :O1)
                s' (cond-> (assoc s
                                  :round (inc r)
                                  :score (+ (:score s) (payoff act size out))
                                  :last (case out :O2 :accepted :O3 :refused
                                              :O4 :snatched :repaired :repaired :none))
                     ;; only an offer tests the disposition model
                     (= act :offer) (-> (assoc :last-size size)
                                        (assoc :unmodelled?
                                               (not (contains? modelled out)))
                                        (assoc :disposition-known
                                               (or (:disposition-known s)
                                                   (when (#{:O2 :O4} out) true))))
                     (= out :O4) (-> (assoc :snatched? true :seized size
                                            :denounced? false)
                                     (update :tokens - size)
                                     (update :shame #(if (:shame-available? s) (inc %) %)))
                     (= out :O2) (update :tokens + size)
                     ;; repair is an OBSERVED EVENT, not the availability of a
                     ;; judge: denouncing is what produces it, and is optional.
                     (= act :denounce) (assoc :repair-observed? true
                                              :denounced? true :seized 0))]
            (recur (inc r) s'
                   (conj trace {:round r :patterns pats :action act :outcome out
                                :size size :score (:score s') :by by}))))))))

;; ---- the @why graph, read from the library ------------------------------
;; The cascade a run produces is not the chain of acting patterns.  It is the
;; sub-graph of @why that those patterns stand on — and @why is a semilattice
;; rather than a tree, which is Alexander's own Figure 1 shape.
;;
;; The reader, the up-closure and the induced edges used to live here, against a
;; hardcoded `library/snatch`.  They are now `find-organise`'s, which reads any
;; section and states the laws the closure has to satisfy (worklist :L6).  The
;; ids there are section-qualified, so a cross-section `@why` resolves rather
;; than colliding; this file's artefacts are keyed inside snatch, so `q` and
;; `l` convert at the boundary and nowhere else.
(def repository (fo/read-repository "library" [:snatch]))

(defn- q [id] (fo/qualified :snatch id))
(defn- l [id] (fo/local id))

(defn cascade
  "The cascade of a run, under the temperament this file has always run: take
   the up-closure (P-validated-R5 §2.1d).  Ids come back bare."
  [acting]
  (let [c (fo/organise fo/up-closure-temperament (into #{} (map q) acting) repository)]
    {:nodes (into #{} (map l) (:nodes c))
     :added-by-organise (into #{} (map l) (:added-by-organise c))
     :edges (mapv (fn [[u v]] [(l u) (l v)]) (:edges c))}))

(defn- up-closure
  "Every pattern the given ones stand on, transitively."
  [ids]
  (:nodes (cascade ids)))

;; Alexander's contrast is tree vs semilattice, but a rooted tree IS a meet
;; semilattice — every two nodes have a greatest common ancestor.  So the
;; contrast that carries information is tree vs NOT-a-tree, i.e. whether overlap
;; is present, and the semilattice property has to be checked separately rather
;; than inferred from the overlap.
(defn- authorities-of
  "The down-set: a pattern together with everything it transitively stands on."
  [id]
  (up-closure #{id}))

(defn- meet
  "The greatest common authority of two patterns, if there is exactly one."
  [nodes a b]
  (let [common (set/intersection (authorities-of a) (authorities-of b) nodes)
        maxima (filter (fn [g] (every? #(contains? (authorities-of g) %) common))
                       common)]
    (when (= 1 (count maxima)) (first maxima))))

(defn- shape [acting]
  (let [{:keys [nodes edges]} (cascade acting)
        parents-of (reduce (fn [m [c p]] (update m c (fnil conj #{}) p)) {} edges)
        shared (sort (map name (keys (filter #(> (count (val %)) 1) parents-of))))
        pairs (for [a nodes, b nodes :when (neg? (compare (str a) (str b)))] [a b])
        without (remove (fn [[a b]] (meet nodes a b)) pairs)]
    {:nodes (count nodes) :edges (count edges) :shared shared
     :tree? (empty? shared)
     :meet-semilattice? (empty? without)
     :pairs-without-meet (count without)}))

(defn- cascade-report [acting]
  (let [{:keys [nodes edges shared tree? meet-semilattice? pairs-without-meet]}
        (shape acting)]
    (println (format "  cascade: %d acting, %d nodes in the @why closure, %d edges"
                     (count acting) nodes edges))
    (if tree?
      (println "  TREE — every node in the closure has at most one authority")
      (println (format "  NOT A TREE — standing on two or more authorities: %s"
                       (str/join ", " shared))))
    (println (format "  meet-semilattice: %s%s"
                     (if meet-semilattice? "yes" "NO")
                     (if meet-semilattice? ""
                         (format " (%d pair(s) with no greatest common authority)"
                                 pairs-without-meet))))))

(def ^:private scenarios
  [[:g1 :snatcher 5] [:g1 :sharer 5] [:g1 :cautious 5]
   [:g4 :snatcher 5] [:g2 :snatcher 12] [:g5 :sharer 5]])

(defn- show [policy label treatment disposition rounds]
  (println (format "\n── %s, P2 is a %s, %d rounds — π = %s ──"
                   (name treatment) (name disposition) rounds label))
  (let [trace (play policy treatment disposition rounds)]
    (doseq [t trace]
      (println (format "  r%-2d %-56s %s%s -> %-13s %+3d %s"
                       (:round t) (pr-str (:patterns t)) (name (:action t))
                       (if (pos? (:size t)) (str " " (:size t)) "")
                       (name (:outcome t)) (:score t)
                       (if (:by t) (str "[" (name (:by t)) "]") ""))))
    (println (format "  coverage gaps: %d    final score: %+d"
                     (count (filter #(empty? (:patterns %)) trace))
                     (:score (last trace))))
    (println (format "  acting chain: %s"
                     (str/join " → " (map #(name (:by %))
                                          (filter :by trace)))))
    ;; `:no-pattern` is the runner's "nothing fired" sentinel, not a pattern in
    ;; the repository, so it is not a cascade node (F1/O1).  `emit-cascade-edn`
    ;; below has always removed it; these two reports did not, and counted it as
    ;; a node of the @why closure until :L6 put organise behind the laws.
    (cascade-report (into #{} (comp (keep :by) (remove #{:no-pattern})) trace))))

;; The figure is drawn from this, so it cannot drift from the run that
;; produced it: bb p4ng/empirics-futon/gen_snatch_cascade.bb
(defn- emit-cascade-edn []
  (let [out "checks/snatch-cascade.edn"
        policies [[:patterns pi-patterns patterns-overrides]
                  [:exchange-first pi-exchange-first exchange-first-overrides]]
        ;; `play-rules`, not `collection`: a policy-grain rule also carries a
        ;; THEN, and the precedence this artefact records is the play-grain one
        ;; a temperament ORDERS — not the temperament's own.
        precedence (fn [overrides]
                     (->> play-rules
                          (sort-by #(get overrides (:id %) (:precedence %)))
                          (mapv :id)))
        rows (for [[policy-name policy overrides] policies
                   [t d n] scenarios
                   :let [trace (play policy t d n)
                         acting-order (into [] (comp (keep :by) (remove #{:no-pattern}) (distinct)) trace)
                         acting (set acting-order)
                         organised (cascade acting)]]
               {:treatment t :disposition d :rounds n
                :policy policy-name
                :precedence (precedence overrides)
                :acting acting-order            ; play order (first firing), :no-pattern removed
                :fallback-rounds (count (filter #(= :no-pattern (:by %)) trace))
                :nodes (vec (sort acting))
                :added-by-organise (vec (sort (:added-by-organise organised)))
                :score (:score (last trace))
                :grim-score (:score (last (play pi-grim t d n)))})
        by-key (group-by (juxt :treatment :disposition) rows)
        s-g4 (mapv (fn [[t d _]]
                     (let [pair (get by-key [t d])
                           patterns (some #(when (= :patterns (:policy %)) %) pair)
                           exchange-first (some #(when (= :exchange-first (:policy %)) %) pair)]
                       {:treatment t
                        :disposition d
                        :nodes-equal? (= (:nodes patterns) (:nodes exchange-first))
                        :score-patterns (:score patterns)
                        :score-exchange-first (:score exchange-first)
                        :precedence-differs? (not= (:precedence patterns)
                                                   (:precedence exchange-first))}))
                   scenarios)
        holds? (boolean (some #(and (:precedence-differs? %)
                                    (not= (:score-patterns %) (:score-exchange-first %)))
                              s-g4))]
    (spit out (with-out-str
                (clojure.pprint/pprint {:scenarios (vec rows)
                                        :s-g4 {:scenarios s-g4
                                               :verdict (if holds? :holds :does-not-hold)}})))
    (println (format "\nwrote %s" out))))

(defn- compare-policies []
  (println "\n── G(π): the same six scenarios under three policies ──")
  (println "  scenario              grim   patterns   exchange-first")
  (doseq [[t d n] scenarios]
    (let [g (:score (last (play pi-grim t d n)))
          p (:score (last (play pi-patterns t d n)))
          x (:score (last (play pi-exchange-first t d n)))]
      (println (format "  %-6s %-10s %6d %10d %15d" (name t) (name d) g p x)))))

(defn- compare-shapes []
  (println "\n── the same collection, one policy, six situations ──")
  (println "  scenario           acting  nodes  edges  overlap  tree?  meet-slat?  G-grade?")
  (doseq [[t d n] scenarios]
    (let [trace (play pi-patterns t d n)
          acting (into #{} (comp (keep :by) (remove #{:no-pattern})) trace)
          {:keys [nodes edges shared tree? meet-semilattice?]} (shape acting)
          moved? (not= (:score (last trace))
                       (:score (last (play pi-exchange-first t d n))))]
      (println (format "  %-6s %-10s %5d %6d %6d %8d  %-5s  %-9s  %s"
                       (name t) (name d) (count acting) nodes edges
                       (count shared) (if tree? "yes" "no")
                       (if meet-semilattice? "yes" "NO")
                       (if moved? "earns" "refused"))))))

;; ---- the policy grain, checked ------------------------------------------

(def policy-rules (filterv #(= :policy (:grain %)) collection))

(def unexecuted-policy-patterns
  "The four of slice (b)'s six policy-grain patterns this row does NOT execute,
   and the CascadeEdit each of their THENs would need.  Every constructor of the
   type has a named claimant here or in `policy-rules`; none is speculative."
  {:have-a-temperament
   {:needs [] :because "the parent. Its THEN asks that temperaments be written as
            patterns at all, which the other five satisfy; it emits no edit of its own."}
   :promote-the-remedy-before-the-exit
   {:needs [:promote-above :drop]
    :because "its THEN promotes the applicable remedies WHILE A BREACH STANDS and
              drops the edit when it no longer does. The guard reads breach state
              off the run's record, which the CascadeState carries and no run has
              yet fed it: :record is the construction here, not the play trace."}
   :widen-the-cascade-only-on-evidence
   {:needs [:admit :halt]
    :because "one admission at a time, ordered by what the last one bought, with a
              stated stopping rule. Executed by :LA3, but NOT HERE: the ordering
              term needs a repository to read authored edges off, and this
              harness's operand is a seven-rule cascade over one section.
              `checks/construct_cascade.clj` fires it over the whole library, and
              its two temperaments differ only in which of this THEN's stopping
              rules they carry."}
   :grim-cuts-the-cascade-and-never-widens-it
   {:needs [:drop :set-flag]
    :because "drop what depends on the arrangement at the first breach and set the
              monotone no-readmission flag. `fo/apply-edit` implements both; nothing
              carries a cascade between ROUNDS, so the flag would have nowhere to
              be monotone over — `play` rebuilds the acting set every round."}})

(defn grain-separation
  "The `:policy` conjunct is CHECKED, not declared.  Both directions, because the
   conjunct is one clause in each guard and a missing one on either side is the
   same defect: a policy-grain rule offered a play situation must not fire, and a
   play-grain rule offered a CascadeState must not fire.

   The second direction is also why a play guard may read `:tokens` without a nil
   check — `(= :play (:grain s))` short-circuits first."
  []
  (let [play-situation (merge (treatments :g1)
                              {:grain :play :round 1 :snatched? false :seized 0
                               :tokens 10 :last-size 1 :score 0 :shame 0})
        policy-state (initial-cascade-state)]
    {:policy-rules-firing-in-a-play-situation
     (mapv :id (filter #(fires? % play-situation) policy-rules))
     :play-rules-firing-in-a-cascade-state
     (mapv :id (filter #(fires? % policy-state) play-rules))
     :design-rules-firing-in-a-cascade-state
     (mapv :id (filter #(fires? % policy-state)
                       (filter #(= :design (:grain %)) collection)))}))

(defn grain-conjunct-mutations
  "`grain-separation` shows no rule fires outside its grain.  It does not show
   the CONJUNCT is what stops them — a guard that read no field of the foreign
   situation would pass it too.  So forge the grain: hand each rule a situation
   of the wrong kind with `:grain` set to its own, which is the mutation that
   deletes the conjunct without rewriting the closure, and record what the rest
   of the guard then does.

   A `:throws` or a `:fires` is the conjunct doing work.  A `:silent` is the
   conjunct being redundant for that rule, which is worth knowing and is not a
   failure — the control fails only if EVERY rule is silent, because then the
   separation rests on nothing but which keys the two situation types happen to
   use."
  []
  (let [play-situation (merge (treatments :g1)
                              {:grain :play :round 1 :snatched? false :seized 0
                               :tokens 10 :last-size 1 :score 0 :shame 0})
        policy-state (initial-cascade-state)
        probe (fn [rule s]
                (try (if (fires? rule s) :fires :silent)
                     (catch Exception _ :throws)))]
    (vec (concat
          (for [r policy-rules]
            (sorted-map :rule (:id r) :grain :policy :forged-into :play
                        :result (probe r (assoc play-situation :grain :policy))))
          (for [r play-rules]
            (sorted-map :rule (:id r) :grain :play :forged-into :policy
                        :result (probe r (assoc policy-state :grain :play))))))))

(defn library-correspondence
  "Each policy-grain runner entry names a pattern the library actually holds, and
   its `:then-source` points at that file's THEN.  Re-read on every run: the
   facade LA1c §11 names is six library files and a runner that only looks like
   it reads them, and an id that agreed by coincidence would be the same thing."
  []
  (mapv (fn [{:keys [id then-source]}]
          (let [[path lines] (str/split then-source #":")
                [from _to] (mapv parse-long (str/split lines #"-"))
                text (vec (str/split-lines (slurp path)))
                head (get text (dec from) "")]
            (sorted-map :pattern id
                        :then-source then-source
                        :in-repository? (contains? (:patterns repository) (q id))
                        :source-line-is-a-then? (boolean (re-find #"^\s*\+ THEN:" head)))))
        policy-rules))

(defn- policy-grain-report []
  (println "\n── the policy grain: temperaments fired, not written down ──")
  (doseq [t [identity-temperament trading-temperament]]
    (let [final (construct t)
          derived (temperament-overrides t)
          recorded (get recorded-overrides (:id t))]
      (println (format "  %-20s %d edit(s), stop %s"
                       (name (:id t)) (count (:record final)) (pr-str (:stop final))))
      (doseq [e (:record final)]
        (println (format "    step %d  %-14s %s"
                         (:step e) (name (:edit e))
                         (pr-str (dissoc e :step :edit :by)))))
      (println (format "    order   %s" (pr-str (fo/ordered final))))
      (println (format "    overrides %s  %s recorded %s"
                       (pr-str derived) (if (= derived recorded) "==" "!=")
                       (pr-str recorded)))))
  (let [sep (grain-separation)
        corr (library-correspondence)
        bad-corr (remove #(and (:in-repository? %) (:source-line-is-a-then? %)) corr)
        leaks (into [] (mapcat val) sep)
        mutations (grain-conjunct-mutations)
        load-bearing (filterv #(not= :silent (:result %)) mutations)]
    (doseq [c corr]
      (println (format "  %-32s authored: %-5s THEN at %s: %s"
                       (name (:pattern c)) (:in-repository? c)
                       (:then-source c) (:source-line-is-a-then? c))))
    (println (format "  grain separation: %s"
                     (if (empty? leaks) "no rule fires outside its grain"
                         (str "LEAK " (pr-str sep)))))
    (println (format "  grain conjunct deleted: %d of %d rules then fire or throw%s"
                     (count load-bearing) (count mutations)
                     (if (seq load-bearing)
                       (str " — " (str/join ", " (map #(format "%s %s" (name (:rule %))
                                                               (name (:result %)))
                                                      load-bearing)))
                       "")))
    (println (format "  of slice (b)'s six policy-grain patterns: %d executed, %d not (%s)"
                     (count policy-rules) (count unexecuted-policy-patterns)
                     (str/join ", " (map name (sort (keys unexecuted-policy-patterns))))))
    (when (or (seq leaks) (seq bad-corr) (empty? load-bearing))
      (throw (ex-info "policy grain: control failed"
                      {:finding (cond (seq leaks) :grain-leak
                                      (seq bad-corr) :library-correspondence
                                      :else :grain-conjunct-is-decoration)
                       :leaks sep :correspondence (vec bad-corr)
                       :mutations mutations})))))

(defn -main [& _]
  (println "PATTERN-THEORETIC PLAYOUT — Snatch or Share")
  (println "\nDESIGN-grain situations (choosing an institution):")
  (doseq [[label s] [["designing G1 from scratch" {:grain :design :judge-available? false}]
                     ["a rule to force offers is proposed" {:grain :design :rule-proposed? true :judge-available? false}]
                     ["we want sanctions, no judge" {:grain :design :want-sanction? true :judge-available? false}]
                     ["a judge is available" {:grain :design :judge-available? true}]]]
    (println (format "  %-36s -> %s" label (pr-str (applicable s)))))
  (policy-grain-report)
  (doseq [[t d n] scenarios] (show pi-patterns "patterns" t d n))
  (compare-policies)
  (compare-shapes)
  (emit-cascade-edn)
  (println "\n── item S-001 (pi = probe-one-token, G1, round 1) ──")
  (println "  Q = {O1 0.0, O2 0.5, O3 0.0, O4 0.5}; falsifier O3")
  (doseq [d [:sharer :snatcher :cautious]]
    (let [o (:outcome (first (play pi-grim :g1 d 1)))]
      (println (format "  P2 %-9s -> %s  %s" (name d) (name o)
                       (case o :O2 "in support, posterior collapses to sharer"
                               :O4 "in support, posterior collapses to snatcher"
                               :O3 "*** FALSIFIER FIRES — the two-disposition model is refuted ***"
                               "outside support"))))))
