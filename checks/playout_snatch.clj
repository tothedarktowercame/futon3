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
            [clojure.pprint]))

;; ---- the collection, as (IF-guard, THEN) pairs -------------------------
;; Each guard is a predicate on the situation. This IS the pattern's IF clause,
;; encoded — the point of the exercise is that some situations match nothing.
(def collection
  [{:id :protect-the-unprotected-move :grain :design
    :if-text "An exchange requires one party to part with something before receiving anything."
    :if (fn [s] (= :design (:grain s)))}
   {:id :preserve-the-right-to-abstain :grain :design
    :if-text "A rule is proposed that increases participation in an exchange."
    :if (fn [s] (and (= :design (:grain s)) (:rule-proposed? s)))}
   {:id :mark-without-force :grain :design
    :if-text "You want a defection to have consequences and you have no enforcement apparatus."
    :if (fn [s] (and (= :design (:grain s)) (:want-sanction? s)))}
   {:id :revert-then-invert :grain :design
    :if-text "A defection can be detected, denounced, and adjudicated."
    :if (fn [s] (and (= :design (:grain s)) (:judge-available? s)))}
   {:id :non-binding-talk-still-moves-play :grain :design
    :if-text "You want cooperation and cannot or will not add enforcement."
    :if (fn [s] (and (= :design (:grain s)) (not (:judge-available? s))))}
   {:id :institutions-vary-by-position-and-force :grain :design
    :if-text "You must design, compare, or extend a family of institutional arrangements."
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
    :if-text "An outcome to which your model assigned no probability."
    :if (fn [s] (and (= :play (:grain s)) (:unmodelled? s)))
    ;; "Play continues whether or not you notice" — in the final round it does
    ;; not, so there is nothing left for a stop to protect.
    :however-text "Play continues, and repeating the policy is the cheapest move."
    :however (fn [s] (not (:last-round? s)))
    :then (fn [_] {:act :stop})}
   {:id :consult-the-remedy-before-exiting :grain :play :precedence 2
    :if-text "You were defected against and the arrangement provides a response."
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s)
                     (or (:shame-available? s) (:judge-available? s))))
    ;; "a policy written for the state of nature exits on the first defection" —
    ;; the temptation requires that exiting be possible at all.
    :however-text "Exiting looks the same whether or not a remedy existed."
    :however (fn [s] (contains? (:p1-actions s) :abstain))
    ;; "check what the arrangement entitles you to and use it" — denounce while
    ;; the remedy is unspent, and say nothing once it is spent.  Denouncing is
    ;; optional in G4, so it is an action, not an automatic consequence.
    :then (fn [s] (when (and (:judge-available? s) (pos? (:seized s))
                             (not (:denounced? s)))
                    ;; the size of a denunciation is the size of the seizure
                    {:act :denounce :size (:seized s)}))}
   {:id :re-enter-after-observed-repair :grain :play :precedence 3
    :if-text "A prior loss has been observably repaired and a bounded probe is available."
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s)
                     (:repair-observed? s) (pos? (:tokens s))))
    ;; "permanent exit discards any improvement" — live once the remedy is spent
    ;; and exit is the only remaining alternative to re-entry.
    :however-text "Permanent exit discards the improvement; re-entry on a promise repeats the exposure."
    :however (fn [s] (zero? (:seized s)))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}
   {:id :forced-play-needs-a-loss-floor :grain :play :precedence 4
    :if-text "Abstention is unavailable after seizure has become a live risk."
    :if (fn [s] (and (= :play (:grain s)) (:forced-offer? s) (:snatched? s)))
    ;; "compulsion does not make a large exposed offer safer" — there is no
    ;; large offer to be tempted by when the floor is all you hold.
    :however-text "Compulsion does not make a large exposed offer safer."
    :however (fn [s] (> (:tokens s) 1))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}
   {:id :escalate-only-as-far-as-you-can-lose :grain :play :precedence 5
    :if-text "A counterpart has accepted an offer."
    :if (fn [s] (and (= :play (:grain s)) (= :accepted (:last s))))
    ;; NON-DISCRIMINATING.  "One acceptance is weak evidence" and a snatch is
    ;; always possible, so this force is live in every state where the IF holds.
    ;; It names a permanent condition of the game rather than a tension in the
    ;; situation, and is recorded as such instead of being given a guard.
    :however-text "One acceptance is weak evidence, and belief-sized offers are unrecoverable."
    :however (fn [_] true)
    :then (fn [s] (when (pos? (:tokens s))
                    {:act :offer :size (min (inc (:last-size s)) (:tokens s))}))}
   {:id :probe-before-committing :grain :play :precedence 6
    :if-text "A counterpart whose disposition you do not know."
    :if (fn [s] (and (= :play (:grain s)) (nil? (:disposition-known s))))
    ;; "a large first offer is unrecoverable ... a zero offer buys nothing" —
    ;; both errors need to be reachable for the sizing advice to bite.
    :however-text "A large first offer is unrecoverable; a zero offer buys nothing."
    :however (fn [s] (> (:tokens s) 1))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}
   {:id :exchange-when-both-sides-gain :grain :play :precedence 7
    :if-text "Both players hold tokens and an exchange remains available."
    :if (fn [s] (and (= :play (:grain s)) (pos? (:tokens s))))
    ;; "attention to seizure risk can make abstention look like the objective" —
    ;; live only where seizure is salient: it has happened, or the counterpart
    ;; is still unread.  Against a known sharer the exchange needs no argument.
    :however-text "Attention to seizure risk makes abstention look like the objective."
    :however (fn [s] (or (:snatched? s) (nil? (:disposition-known s))))
    :then (fn [s] (when (pos? (:tokens s)) {:act :offer :size 1}))}

   ;; Advisory in this harness — real game features the model does not carry.
   {:id :ask-for-surplus-not-surrender :grain :play
    :if-text "An offer is available and its ask is still open."
    :if (fn [s] (and (= :play (:grain s)) (pos? (:tokens s))))}
   {:id :a-free-mark-is-always-worth-assigning :grain :play
    :if-text "A mark may be attached and changes no payoff this round."
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s) (:shame-available? s)))}
   {:id :use-talk-to-make-a-testable-offer :grain :play
    :if-text "Cheap talk is available before the first observed response."
    :if (fn [s] (and (= :play (:grain s)) (:chat-available? s) (nil? (:last s))))}
   {:id :price-the-final-round-as-final :grain :play
    :if-text "The known final round."
    :if (fn [s] (and (= :play (:grain s)) (:last-round? s)))}
   ;; A P2 pattern.  P2 here is a fixed disposition, not a chooser, so it is
   ;; excluded from P1's set; :offer-received? is P2's view and is never set.
   {:id :accept-an-offer-that-beats-holding :grain :play :actor :p2
    :if-text "P2 receives a positive give-and-ask offer that benefits both sides."
    :if (fn [s] (and (= :play (:grain s)) (:offer-received? s)))}])

;; P1's set only: a pattern whose actor is P2 is not decidable by this harness.
;; A pattern fires when its ANTECEDENT holds: IF and HOWEVER together.  A
;; pattern with no stated :however fires on its IF alone.
(defn fires? [pat s]
  (and (not= :p2 (:actor pat))
       ((:if pat) s)
       (if-let [however (:however pat)] (however s) true)))

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
(defn pattern-policy [overrides]
  (fn [s pats]
    (let [fired (set pats)
          order #(get overrides (:id %) (:precedence %))]
      (or (some (fn [pat] (when-let [a ((:then pat) s)] (assoc a :by (:id pat))))
                (->> collection
                     (filter #(and (:then %) (fired (:id %))))
                     (sort-by order)))
          {:act :abstain :by :no-pattern}))))

(def patterns-overrides {})
(def pi-patterns (pattern-policy patterns-overrides))

;; One re-wiring: put the gain pattern above the remedy and the stop.  Nothing
;; about any pattern changes; only the order in which they are consulted.
(def exchange-first-overrides {:exchange-when-both-sides-gain 0})
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
(def ^:private library-dir "library/snatch")

(defn- parse-why [file]
  (let [lines (str/split-lines (slurp file))
        id (some #(second (re-matches #"@flexiarg snatch/(\S+)" %)) lines)
        why (some #(second (re-matches #"@why (.*)" %)) lines)]
    (when id
      [(keyword id)
       (into #{} (map #(keyword (str/replace % "snatch/" "")))
             (remove str/blank? (str/split (or why "") #"\s+")))])))

(def why-graph
  (into {} (keep parse-why)
        (->> (file-seq (java.io.File. library-dir))
             (filter #(str/ends-with? (.getName ^java.io.File %) ".flexiarg")))))

(defn- up-closure
  "Every pattern the given ones stand on, transitively."
  [ids]
  (loop [seen #{} frontier (set ids)]
    (if (empty? frontier)
      seen
      (let [seen' (into seen frontier)]
        (recur seen'
               (set/difference (into #{} (mapcat #(get why-graph % #{})) frontier)
                               seen'))))))

(defn- induced-edges [nodes]
  (for [n nodes, parent (get why-graph n #{}) :when (nodes parent)] [n parent]))

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
  (let [nodes (up-closure acting)
        edges (induced-edges nodes)
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
    (cascade-report (into #{} (keep :by) trace))))

;; The figure is drawn from this, so it cannot drift from the run that
;; produced it: bb p4ng/empirics-futon/gen_snatch_cascade.bb
(defn- emit-cascade-edn []
  (let [out "checks/snatch-cascade.edn"
        policies [[:patterns pi-patterns patterns-overrides]
                  [:exchange-first pi-exchange-first exchange-first-overrides]]
        precedence (fn [overrides]
                     (->> collection
                          (filter :then)
                          (sort-by #(get overrides (:id %) (:precedence %)))
                          (mapv :id)))
        rows (for [[policy-name policy overrides] policies
                   [t d n] scenarios
                   :let [trace (play policy t d n)
                         acting-order (into [] (comp (keep :by) (remove #{:no-pattern}) (distinct)) trace)
                         acting (set acting-order)
                         closure (up-closure acting)]]
               {:treatment t :disposition d :rounds n
                :policy policy-name
                :precedence (precedence overrides)
                :acting acting-order            ; play order (first firing), :no-pattern removed
                :fallback-rounds (count (filter #(= :no-pattern (:by %)) trace))
                :nodes (vec (sort acting))
                :added-by-organise (vec (sort (set/difference closure acting)))
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
          acting (into #{} (keep :by) trace)
          {:keys [nodes edges shared tree? meet-semilattice?]} (shape acting)
          moved? (not= (:score (last trace))
                       (:score (last (play pi-exchange-first t d n))))]
      (println (format "  %-6s %-10s %5d %6d %6d %8d  %-5s  %-9s  %s"
                       (name t) (name d) (count acting) nodes edges
                       (count shared) (if tree? "yes" "no")
                       (if meet-semilattice? "yes" "NO")
                       (if moved? "earns" "refused"))))))

(defn -main [& _]
  (println "PATTERN-THEORETIC PLAYOUT — Snatch or Share")
  (println "\nDESIGN-grain situations (choosing an institution):")
  (doseq [[label s] [["designing G1 from scratch" {:grain :design :judge-available? false}]
                     ["a rule to force offers is proposed" {:grain :design :rule-proposed? true :judge-available? false}]
                     ["we want sanctions, no judge" {:grain :design :want-sanction? true :judge-available? false}]
                     ["a judge is available" {:grain :design :judge-available? true}]]]
    (println (format "  %-36s -> %s" label (pr-str (applicable s)))))
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
