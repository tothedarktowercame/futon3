(ns construct-cascade
  "The CONSTRUCTOR of LA1c-restatement.md §4, over the whole pattern library.

   Worklist row `:LA3` (futon3/holes/labs/library-contract/worklist.edn).  `:LA2`
   built the policy grain against `library/snatch`: a `CascadeEdit` type, one
   grain-polymorphic firing loop, and two temperaments that re-wire a seven-rule
   cascade's precedence.  What it did not build is the loop §4.1 describes --
   admit one pattern at a time, ordered by a score, and stop on a rule stated in
   advance -- because the score had no substrate the laws admit.
   `decisions.edn :constructor-relevance-substrate` records why:
   `futon3a/holes/labs/M-memes-arrows/cascade_construct.py` supplies the loop
   (`:222-232`) and both of its factors are refused -- relevance is MiniLM cosine
   (`:48-58`), which F3 refuses as a receipt citing the finder's score alone, and
   connectivity comes from a phylogeny whose descent relation is substring
   containment (`futon6/scripts/pattern_phylogeny.py:22`), which law O2 refuses.

   The interim arm keeps the FORM and swaps both substrates:

     score(p | chosen, tension) = match(p, tension) * (alpha + degree(p, chosen))

     match   the F2/F3 receipt: how many clauses of the tension this pattern's
             IF and HOWEVER acknowledge, each acknowledgement citing the pattern
             file and the line span the cue occurs in.  Never a score.
     degree  how many of the already-chosen patterns are joined to this one by an
             AUTHORED @why/@how edge -- `ReachOutside` (Holes.lean:297-300)
             restricted to `chosen`.  Never similarity, co-occurrence or prose.

   Both terms are authored, so O2 holds of the score by construction.  The score
   is nonetheless NOT an edge: the constructed cascade's edges come from
   `fast-forward` over the authored `@why` relation exactly as `organise`'s do,
   and `degree-relation-authors-no-edge` below is the control that they do.

   The row's third clause is a MEASUREMENT, and it is the part that can come out
   against the constructor: at 8.3% rationale-layer coverage, is the degree term
   distinguishable from uniform on a real tension?  If it is not, the finding is
   that L5/L9 coverage blocks the constructor and the effort belongs there.  The
   comparison is pre-registered as `:uniform` below -- the same construction with
   `degree` forced to zero -- and is run on every `-main`."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [find-organise :as fo]))

;; ---------------------------------------------------------------------------
;; the library, read whole
;; ---------------------------------------------------------------------------

(def library-root "library")

(defn library-sections
  "Every directory of `library-root` that holds at least one `.flexiarg`.  Read
   from the filesystem rather than listed here: a hand-kept list of sections is
   the same defect as a hand-kept list of edges, one level up."
  [root]
  (->> (.listFiles (io/file root))
       (filter #(.isDirectory ^java.io.File %))
       (filter (fn [d] (some #(str/ends-with? (.getName ^java.io.File %) ".flexiarg")
                             (.listFiles ^java.io.File d))))
       (map #(keyword (.getName ^java.io.File %)))
       sort
       vec))

(defn- normalise [s]
  (some-> s str/trim (str/replace #"\s+" " ")))

(defn read-digest
  "sha256 over exactly what the constructor READS: every pattern's file, its IF
   and HOWEVER text, and its edge directives.  library/math-formalization takes
   files from a live scribe with no baseline step of its own (LB1's finding,
   `decisions.edn :math-formalization-untracked-flexiargs`), so a report from
   this file is a timestamp and not a standing property.  The digest is what
   makes that visible instead of silent."
  [repository]
  (let [payload (->> (vals (:entries repository))
                     (map (fn [e] [(:file e) (:if-text e) (:however-text e)
                                   (mapv (juxt :kind :to) (:edges e))]))
                     (sort-by first)
                     vec
                     pr-str)
        digest (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest digest (.getBytes payload "UTF-8"))))))

;; ---------------------------------------------------------------------------
;; the tension
;; ---------------------------------------------------------------------------
;; A Tension here is a list of CLAUSES.  Each clause carries the text it is a
;; clause of, the record that states it, and the CUES an author wrote for it: the
;; literal strings whose occurrence in a pattern's IF or HOWEVER counts as that
;; pattern acknowledging the clause.
;;
;; The cues are authored and crude on purpose.  Crude, because a literal
;; occurrence is the weakest thing that can still be CITED -- the receipt names
;; the cue, the file and the line span, and `citations-verified` re-reads the
;; file and checks that the cue is there.  Authored, because the alternative is
;; the embedding relevance F3 refuses: a cue set written by a person in advance
;; is a claim about the tension, and a cosine is a claim about a model.
;;
;; What this does NOT give is a good matcher.  It gives a fixed one, which is
;; what the measurement needs: `match` is held constant across the two arms of
;; `:uniform`, so the comparison isolates `degree`.

;; `tension` is DYNAMIC so a second domain can rebind it without a second copy
;; of `acknowledgements` / `match` / `antecedent-holds?` / `seed-and-candidates`.
;; Its root value is unchanged and `-main` runs at the root, so :LA3's report
;; regenerates byte-identically; :LA4 (`construct_ants_cascade.clj`) binds it to
;; the ants tension and calls the same five functions.  A second copy of them is
;; the facade LA1c-restatement.md §11 names, one level down from the firing loop.
(def ^:dynamic tension
  {:id :organise-the-library
   :statement "The pattern library is the repository `find` searches and `organise` builds cascades from, and it is 93% unconnected: nothing states what most patterns stand on or what carries them out, so a cascade can only be hand-authored."
   :source "futon2/holes/problems/P-organise-the-library.md:33-36"
   :clauses
   [{:id :nothing-states-what-a-pattern-stands-on
     :text "nothing states what most patterns stand on or what carries them out"
     :source "futon2/holes/problems/P-organise-the-library.md:34-35"
     :cues ["stands on" "stand on" "authority" "rationale" "@why" "@how"]}
    {:id :a-cascade-can-only-be-hand-authored
     :text "a cascade can only be hand-authored"
     :source "futon2/holes/problems/P-organise-the-library.md:35-36"
     :cues ["cascade" "hand-authored" "by hand"]}
    {:id :find-has-no-search-space
     :text "this is the work that makes `find` have a search space and `organise` have edges to fast-forward"
     :source "futon2/holes/problems/P-organise-the-library.md:5-6"
     :cues ["search space" "retrieval" "retrieve" "which pattern" "select a pattern" "look up"]}
    {:id :an-inferred-edge-is-a-facade
     :text "inferred edges (similarity / co-occurrence / embedding nearness) presented as standsOn -- refused by O2 of P-validated-R5 §3e; a spider must cite text"
     :source "futon2/holes/problems/P-organise-the-library.md:57-58"
     :cues ["similarity" "embedding" "co-occurrence" "cite" "citation"]}]})

(def zero-mass-pattern
  "F4's falsifier, named BEFORE the run: a pattern of the repository this tension
   must not select.  `library/ants` is the unorganised control domain
   (P-validated-R5 §1b addendum 3) and this one is about pheromone decay in an
   ant simulation, which the library-organisation tension has no clause for."
  :ants/pheromone-trail-tuner)

;; ---------------------------------------------------------------------------
;; match -- the F2/F3 receipt
;; ---------------------------------------------------------------------------

(defn- cue-hits [text cues]
  (when text
    (let [t (str/lower-case text)]
      (filterv #(str/includes? t (str/lower-case %)) cues))))

(defn acknowledgements
  "Every [clause, block, cue] this pattern's antecedent acknowledges, each with
   the file and line span to cite for it.  This is the whole of `match`: a count
   of the CLAUSES covered, with a citation per clause."
  [entry]
  (vec (for [clause (:clauses tension)
             [block text lines] [[:if (:if-text entry) (:if-lines entry)]
                                 [:however (:however-text entry) (:however-lines entry)]]
             cue (cue-hits text (:cues clause))]
         (sorted-map :clause (:id clause) :block block :cue cue
                     :file (:file entry) :lines lines))))

(defn match
  "|{ clauses of the tension the pattern's IF or HOWEVER acknowledges }|."
  [entry]
  (count (into #{} (map :clause) (acknowledgements entry))))

(defn antecedent-holds?
  "IF and HOWEVER together, as everywhere else in this harness: the pattern's IF
   acknowledges some clause of the tension AND its HOWEVER acknowledges some
   clause.  A pattern that only names the situation, with nothing to say about
   the counter-force, does not fire -- it is a candidate, not a selection."
  [entry]
  (let [acks (acknowledgements entry)]
    (and (some #(= :if (:block %)) acks)
         (some #(= :however (:block %)) acks))))

(defn receipt-for [entry]
  (sorted-map :match (match entry)
              :acknowledges (acknowledgements entry)))

;; ---------------------------------------------------------------------------
;; degree -- ReachOutside restricted to chosen
;; ---------------------------------------------------------------------------
;; `@why` points at what a pattern stands ON and `@how` at what carries it OUT,
;; so their union as a DIRECTED relation runs both ways and is cyclic -- 15
;; patterns lie on such a cycle, which `:union-is-cyclic` in the report records.
;; A cyclic relation is not a `standsOn` and has no Repository (Holes.lean:121
;; requires acyclicDescent), so the union is never used as one.  It is used
;; UNDIRECTED, which is what "an authored edge BETWEEN p and q"
;; (`decisions.edn :constructor-relevance-substrate`, arm `:authored-edge-degree`)
;; means, and it is used for one thing only: a weight in the prior.

(defn related-adjacency
  "The authored `@why` u `@how` relation, symmetrised.  Every entry is an
   authored directive line; nothing here is inferred."
  [repository]
  (reduce (fn [m {:keys [from to]}]
            (-> m (update from (fnil conj #{}) to) (update to (fnil conj #{}) from)))
          {}
          (:edges repository)))

(defn degree
  "`ReachOutside` (Holes.lean:297-300) restricted to `chosen`: how many chosen
   patterns are joined to `p` by an authored path whose intermediates are all
   outside the cascade.  Direct edges are the depth-1 case."
  [related chosen p]
  (let [chosen (set chosen)]
    (count (set/intersection chosen
                             (set (fo/reach-outside (conj chosen p) related p))))))

;; ---------------------------------------------------------------------------
;; the score
;; ---------------------------------------------------------------------------

(def alpha
  "`cascade_construct.py:206`'s default, kept: alpha is what a candidate with no
   authored edge to the cascade is still worth, and dropping it would make the
   degree term a gate rather than a prior."
  0.3)

(def budget
  "`cascade_serve.py:21-27`'s ceiling, raised 6 -> 20 by operator ruling
   2026-07-05.  Cascade SIZE, not admissions."
  20)

(def epsilon
  "The marginal-gain floor, on THIS score's scale -- not `cascade_construct.py`'s
   0.15, which is on the scale of its own marginal coverage and is recorded in the
   report as `:transplanted-epsilon-never-binds` because every candidate here
   scores at least alpha = 0.3.

   1.0 is the line between the two kinds of support.  A candidate joined to the
   cascade by one authored edge scores at least 1*(0.3+1) = 1.3; a candidate
   joined by none scores match*0.3, which is below 1.0 for match <= 3.  The
   report records the observed maximum match, so the reader can see whether that
   equivalence held on this tension rather than taking it on trust."
  1.0)

(defn score
  "match(p, tension) * (alpha + degree(p, chosen)).  `:uniform?` is the
   pre-registered control arm: the same construction with the degree term forced
   to zero, so the factor is the constant alpha and the ordering is `match`
   alone."
  [state p]
  (* (double (get-in state [:candidates p] 0))
     (+ (:alpha state)
        (if (:uniform? state) 0 (degree (:related state) (:members state) p)))))

(defn ranked
  "The candidates not yet in the cascade, best first.  Ties break by pattern id,
   so a run is reproducible."
  [state]
  (->> (keys (:candidates state))
       (remove (:members state))
       (map (fn [p] [p (score state p)]))
       (sort-by (fn [[p s]] [(- s) p]))
       vec))

(defn- best [state] (first (ranked state)))

;; ---------------------------------------------------------------------------
;; the policy-grain rules
;; ---------------------------------------------------------------------------
;; Three rules, all three encoding ONE authored THEN --
;; `library/snatch/widen-the-cascade-only-on-evidence.flexiarg:29-33`:
;;
;;   "Admit patterns one at a time, ordered by what the previous admission
;;    bought, and stop on a rule stated in advance -- a marginal-gain floor, a
;;    budget, or saturation.  Record the rule and where it stopped on the cascade
;;    with the run."
;;
;; The admission is one rule; the two stops it names are one rule each; the third
;; (saturation) is `fo/construct`'s fall-through and needs no rule.  Splitting the
;; stops out is what lets two temperaments differ ONLY in where they stop, which
;; is the row's second acceptance clause and the pattern's own BECAUSE ("it is
;; what lets two temperaments differ on the same repository").
;;
;; `:then-source` is re-read from disk on every run by `library-correspondence`.

(def then-source
  "library/snatch/widen-the-cascade-only-on-evidence.flexiarg:29-33")

(def collection
  [{:id :halt-on-budget :grain :policy :precedence 1
    :then-source then-source
    :encodes "the `a budget` disjunct of the stopping rule"
    ;; "stop on a rule stated in advance" -- the budget has to BE stated, in
    ;; advance, on the state the temperament is handed.
    :if (fn [s] (and (= :policy (:grain s)) (some? (:budget s))))
    ;; The counter-force is that stopping gives something up: it is live only
    ;; while a candidate remains that could still be admitted.  With nothing left
    ;; to admit, saturation is the honest stop and a halt would be a claim the
    ;; run did not earn.
    :however (fn [s] (and (seq (ranked s)) (>= (count (:members s)) (:budget s))))
    :then (fn [s] (fo/halt [:budget (:budget s)]))}

   {:id :halt-at-the-marginal-gain-floor :grain :policy :precedence 1
    :then-source then-source
    :encodes "the `a marginal-gain floor` disjunct of the stopping rule"
    :if (fn [s] (and (= :policy (:grain s)) (some? (:epsilon s))))
    ;; `cascade_construct.py:230`'s test, on this score: the next admission is
    ;; worth less than the floor.  Same liveness condition as the budget rule.
    :however (fn [s] (and (seq (ranked s)) (< (second (best s)) (:epsilon s))))
    :then (fn [s] (fo/halt [:marginal-gain-floor (:epsilon s)]))}

   {:id :widen-the-cascade-only-on-evidence :grain :policy :precedence 2
    :then-source then-source
    :encodes "the `admit patterns one at a time, ordered by what the previous admission bought` clause"
    ;; "You are building a cascade rather than being handed one" -- there is a
    ;; candidate set, which is the thing a handed cascade does not have.
    :if (fn [s] (and (= :policy (:grain s)) (seq (:candidates s))))
    ;; "the temperament alone is too little to play from; the full closure is too
    ;; much" -- the force this THEN overcomes is the availability of admitting
    ;; everything at once, and it is live exactly while some candidate has
    ;; positive evidence for admission.
    :however (fn [s] (and (seq (ranked s)) (pos? (second (best s)))))
    :then (fn [s] (fo/admit (first (best s))))}])

(def ^:private rule-by-id (into {} (map (juxt :id identity)) collection))

;; ---------------------------------------------------------------------------
;; the two temperaments
;; ---------------------------------------------------------------------------
;; They differ ONLY in their stopping rule.  `differ-only-in-the-stop` checks
;; that, rather than leaving it to the reader of these two literals.

(def budgeted-temperament
  {:id :widen-to-a-budget :grain :policy
   :nodes [:halt-on-budget :widen-the-cascade-only-on-evidence]
   :precedence {:halt-on-budget 1 :widen-the-cascade-only-on-evidence 2}})

(def floored-temperament
  {:id :widen-to-the-marginal-gain-floor :grain :policy
   :nodes [:halt-at-the-marginal-gain-floor :widen-the-cascade-only-on-evidence]
   :precedence {:halt-at-the-marginal-gain-floor 1 :widen-the-cascade-only-on-evidence 2}})

(def temperaments [budgeted-temperament floored-temperament])

(def ^:private stop-rules #{:halt-on-budget :halt-at-the-marginal-gain-floor})

(defn differ-only-in-the-stop
  "The two temperaments' node sets, with their stop rules removed, must be EQUAL
   and must resolve to the same rule maps; their stop rules must differ.  A
   difference anywhere else would make the row's second acceptance clause -- two
   temperaments that differ only in their stopping rule produce different
   cascades -- a claim about two different constructors."
  [a b]
  (let [without #(vec (remove stop-rules (:nodes %)))
        stops #(vec (filter stop-rules (:nodes %)))]
    (sorted-map
     :shared-nodes (without a)
     :same-shared-nodes? (= (without a) (without b))
     :same-shared-rules? (= (mapv rule-by-id (without a)) (mapv rule-by-id (without b)))
     :same-shared-precedence? (= (select-keys (:precedence a) (without a))
                                 (select-keys (:precedence b) (without b)))
     :stops [(stops a) (stops b)]
     :stops-differ? (not= (stops a) (stops b))
     :holds? (and (= (without a) (without b))
                  (= (mapv rule-by-id (without a)) (mapv rule-by-id (without b)))
                  (= (select-keys (:precedence a) (without a))
                     (select-keys (:precedence b) (without b)))
                  (not= (stops a) (stops b))))))

;; ---------------------------------------------------------------------------
;; the construction
;; ---------------------------------------------------------------------------

(defn seed-and-candidates
  "C_0 of §4.1 is what `find` selected; the candidate pool is everything else the
   tension's cues reach at all.  A pattern with match 0 is never scored, which is
   what keeps the constructor from wandering: `score` would be 0 for it anyway."
  [repository]
  (let [entries (:entries repository)
        matches (into {} (keep (fn [[id e]] (let [m (match e)] (when (pos? m) [id m])))) entries)
        seed (into (sorted-set) (keep (fn [[id e]] (when (antecedent-holds? e) id))) entries)]
    {:matches matches :seed seed}))

(defn initial-state
  [{:keys [seed matches related uniform?]}]
  {:grain :policy
   :members (set seed)
   :authored-order (vec (sort seed))
   :precedence (into {} (map-indexed (fn [i id] [id (inc i)])) (sort seed))
   :provenance (into {} (map (juxt identity (constantly :found))) seed)
   :flags #{}
   :record []
   :halted nil
   :candidates matches
   :related related
   :alpha alpha
   :budget budget
   :epsilon epsilon
   :uniform? (boolean uniform?)})

(defn run
  "One construction: `fo/construct` (LA1c §4.1) with this file's rules resolved.
   `:budget` and `:epsilon` are both on the state; which one is READ is decided
   by which stop rule the temperament carries, so the two arms share a state."
  [temperament ctx]
  (fo/construct temperament (mapv rule-by-id (:nodes temperament)) (initial-state ctx)))

(defn admitted-of [final]
  (into (sorted-set) (comp (filter #(= :admit (:edit %))) (map :pattern)) (:record final)))

(defn cascade-of
  "The construction read as a Cascade, so O1-O3 can be run on it.  `:edges` come
   from `fast-forward` over the authored `@why` relation -- the SAME function
   `organise` uses, over the same relation.  The degree term's symmetrised
   `@why` u `@how` relation is not consulted here and
   `degree-relation-authors-no-edge` is the control that it is not."
  [final seed why-repository]
  (let [admitted (admitted-of final)
        nodes (set (:members final))]
    {:selected (set seed)
     :added-by-organise #{}
     :admitted-by admitted
     :nodes nodes
     :edges (fo/fast-forward nodes (:stands-on why-repository))
     :stands-on (:stands-on why-repository)
     ;; O4 is a law about a RUN: precedence changed => acting order or score
     ;; changed.  Nothing here is played, so there is no acting order to move and
     ;; the law is recorded as not-exercised rather than passed on empty vectors.
     :precedence-before []
     :precedence-after []}))

;; ---------------------------------------------------------------------------
;; the controls
;; ---------------------------------------------------------------------------

(defn determinism
  "Two constructions of the same temperament over the same context must agree
   edit for edit.  `ranked` breaks ties by pattern id for this reason; without
   the control the tie-break is a claim in a docstring, and a ranking that fell
   back on hash order would still pass every law above."
  [ctx]
  (vec (for [t temperaments
             :let [a (run t ctx) b (run t ctx)]
             :when (not (and (= (:record a) (:record b)) (= (:stop a) (:stop b))))]
         {:temperament (:id t) :finding :construction-is-not-reproducible})))

(defn library-correspondence
  "Each rule's `:then-source` points at a `+ THEN:` marker of a pattern the
   repository actually holds, re-read from disk on every run.  LA1c §11's facade
   at runner level is three Clojure rules that only look like an encoding of an
   authored THEN."
  [repository]
  (mapv (fn [{:keys [id then-source encodes]}]
          (let [[path lines] (str/split then-source #":")
                [from _to] (mapv parse-long (str/split lines #"-"))
                text (vec (str/split-lines (slurp path)))
                pattern (keyword (str/replace (str/replace path #"^library/" "")
                                              #"\.flexiarg$" ""))]
            (sorted-map :rule id :encodes encodes :then-source then-source
                        :pattern pattern
                        :in-repository? (contains? (:patterns repository) pattern)
                        :source-line-is-a-then?
                        (boolean (re-find #"^\s*\+ THEN:" (get text (dec from) ""))))))
        collection))

(defn citations-verified
  "Every acknowledgement's citation, checked against the file on disk: the cited
   line span must read back as the clause text the match was computed from, and
   the cited cue must occur in it.  This is what makes `match` a receipt in F3's
   sense rather than a number -- and it is checked rather than asserted, because
   an unchecked citation is exactly the facade the standard names."
  [acks]
  (vec (for [{:keys [file lines cue] :as ack} acks
             :let [text (vec (str/split-lines (slurp file)))
                   [from to] lines
                   span (when (and from to (<= 1 from) (<= to (count text)))
                          (normalise (str/join " " (subvec text (dec from) to))))]
             :when (not (and span (str/includes? (str/lower-case span) (str/lower-case cue))))]
         (assoc ack :finding :citation-does-not-read-back))))

(defn degree-relation-authors-no-edge
  "The prior's relation and the cascade's relation are different objects and must
   stay so.  Every edge of the constructed cascade is authored `@why`
   reachability (O2 over `stands-on`); the symmetrised `@why` u `@how` relation
   the score reads would produce a DIFFERENT edge set over the same nodes, and
   that difference is reported so the separation is visible rather than asserted."
  [cascade related]
  (let [why-edges (:edges cascade)
        related-edges (fo/fast-forward (:nodes cascade) related)]
    (sorted-map
     :cascade-edges (count why-edges)
     :edges-if-the-prior-relation-were-used (count related-edges)
     :cascade-edges-are-why-only? (= (set why-edges)
                                     (set (fo/fast-forward (:nodes cascade)
                                                           (:stands-on cascade))))
     :relations-differ? (not= (set why-edges) (set related-edges)))))

(defn grain-conjunct-mutations
  "`grain-separation` shows no rule fires outside its grain.  It does not show the
   `:policy` CONJUNCT is what stops them -- a guard reading a field a play
   situation does not carry would pass that test too.  So forge the grain, as
   `playout_snatch.clj` does: hand each rule a situation of the wrong kind with
   `:grain` set to its own, so the rest of the guard runs without its conjunct.

   Two forges, because they answer different questions.  `:bare` is a plain play
   situation: every rule here is silent under it, which says the separation would
   ALSO hold on key-disjointness alone.  `:enriched` is a play situation carrying
   the constructor's own fields as well -- a state of both kinds, where the grain
   conjunct is the only thing left to tell them apart.  A `:fires` or `:throws`
   under `:enriched` is the conjunct doing work; all-`:silent` there would mean the
   conjunct is decoration."
  [ctx]
  (let [play {:grain :play :round 1 :snatched? false :seized 0
              :tokens 10 :last-size 1 :score 0 :shame 0}
        policy (initial-state ctx)
        forge (fn [s] (assoc s :grain :policy))
        probe (fn [rule s] (try (if (fo/fires? rule s) :fires :silent)
                                (catch Exception _ :throws)))]
    (vec (for [r collection]
           (sorted-map :rule (:id r)
                       :bare (probe r (forge play))
                       :enriched (probe r (forge (merge play (dissoc policy :grain)))))))))

(defn grain-separation
  "No rule of this file fires in a play-grain situation."
  [_ctx]
  (let [play-situation {:grain :play :round 1 :snatched? false :seized 0
                        :tokens 10 :last-size 1 :score 0 :shame 0}]
    (mapv :id (filter #(try (fo/fires? % play-situation) (catch Exception _ false))
                      collection))))

(def ^:private declared-controls
  "Mutations that must be REJECTED.  Each names the law or check that has to catch
   it.  A mutation that slips is a failure of the check; a mutation that could not
   be BUILT on this run is also a failure, because an unexercised control reads
   like a passing one in the report and is the quieter defect."
  [:O2-invented-edge :O3-dropped-edge :O1-unrecorded-node
   :citation-cue-not-in-the-cited-span :citation-span-off-the-end-of-the-file])

(defn negative-controls
  "Build and run each declared mutation.  `:exercised?` says the mutant could be
   constructed at all; `:caught?` says the law or check rejected it."
  [cascade acks related]
  (let [an-edge (first (sort (:edges cascade)))
        ;; an edge the PRIOR's relation carries and authored `@why` reachability
        ;; does not -- the exact facade O2 exists to refuse.
        invented (first (sort (set/difference
                               (set (fo/fast-forward (:nodes cascade) related))
                               (set (:edges cascade)))))
        ack (first acks)
        row (fn [control exercised? caught? detail]
              (sorted-map :control control :exercised? (boolean exercised?)
                          :caught? (boolean caught?) :detail detail))]
    [(row :O2-invented-edge (some? invented)
          (and (some? invented)
               (not (fo/o2-authored-reachability (update cascade :edges conj invented))))
          (pr-str invented))
     (row :O3-dropped-edge (some? an-edge)
          (and (some? an-edge)
               (not (fo/o3-fast-forward (update cascade :edges #(disj (set %) an-edge)))))
          (pr-str an-edge))
     (row :O1-unrecorded-node true
          (not (fo/o1-nodes-recorded (update cascade :nodes conj :not-a-pattern/forged)))
          "a node in neither selected, addedByOrganise nor admittedBy")
     (row :citation-cue-not-in-the-cited-span (some? ack)
          (and (some? ack)
               (seq (citations-verified
                     [(assoc ack :cue "no pattern in this library says this")])))
          (pr-str (:file ack)))
     (row :citation-span-off-the-end-of-the-file (some? ack)
          (and (some? ack)
               (seq (citations-verified [(assoc ack :lines [999999 1000000])])))
          (pr-str (:file ack)))]))

;; ---------------------------------------------------------------------------
;; the measurement -- the row's third acceptance clause
;; ---------------------------------------------------------------------------
;; PRE-REGISTERED, and the arm is run on every -main so it cannot be dropped when
;; it is inconvenient.
;;
;;   claim      the degree term is DISTINGUISHABLE FROM UNIFORM on this tension
;;   test       the same two temperaments run with `degree` forced to zero.  The
;;              term is distinguishable iff at least one construction admits a
;;              DIFFERENT pattern, or stops at a different point, than its
;;              uniform twin.
;;   also       the share of scored candidates with degree > 0 at step 0, which
;;              is what says whether the term is a general prior or a tie-break
;;              over a handful.
;;   if it fails  the delivery is the finding that rationale-layer coverage
;;              blocks the constructor and the effort belongs on L5/L9
;;              (`decisions.edn :constructor-relevance-substrate :changes-it`).

(defn measurement [ctx]
  (let [uniform-ctx (assoc ctx :uniform? true)
        arms (for [t temperaments]
               (let [real (run t ctx)
                     unif (run t uniform-ctx)]
                 (sorted-map
                  :temperament (:id t)
                  :admitted (vec (admitted-of real))
                  :admitted-uniform (vec (admitted-of unif))
                  :stop (:stop real)
                  :stop-uniform (:stop unif)
                  :steps (:steps real)
                  :steps-uniform (:steps unif)
                  :differs? (or (not= (admitted-of real) (admitted-of unif))
                                (not= (:stop real) (:stop unif))))))
        state0 (initial-state ctx)
        scored (remove (:members state0) (keys (:candidates state0)))
        degrees (map #(degree (:related ctx) (:members state0) %) scored)]
    (sorted-map
     :arms (vec arms)
     :distinguishable-from-uniform? (boolean (some :differs? arms))
     ;; The budget arm settles the question WITHOUT the floor: it admits a fixed
     ;; number of patterns and two of them change when the degree term is
     ;; removed.  Recorded separately because the floor arm's difference does
     ;; depend on where `epsilon` was put, and a verdict resting on a threshold
     ;; this file chose would be a weaker claim than the row asks for.
     :distinguishable-without-the-floor?
     (boolean (some :differs? (filter #(= (:id budgeted-temperament) (:temperament %)) arms)))
     :candidates-scored (count scored)
     :candidates-with-degree-above-zero (count (filter pos? degrees))
     ;; where the discrimination comes from: the sections a person organised by
     ;; hand.  This is the cost `decisions.edn :constructor-relevance-substrate`
     ;; states, measured rather than predicted.
     :degree-positive-candidates
     (into (sorted-map)
           (for [p scored :let [d (degree (:related ctx) (:members state0) p)]
                 :when (pos? d)]
             [p d]))
     :sections-supplying-the-degree-term
     (into (sorted-set)
           (for [p scored :when (pos? (degree (:related ctx) (:members state0) p))]
             (keyword (namespace p))))
     :coverage-of-the-rationale-layer
     (sorted-map :patterns (count (:candidates state0))
                 :note "the library-wide figure is in :as-of -- 104 of 1239 patterns touch an authored @why edge, the 8.3% the row names")
     :degree-distribution (into (sorted-map) (frequencies degrees))
     :max-match (reduce max 0 (vals (:candidates state0)))
     :epsilon-equivalence-holds?
     ;; the claim `epsilon` documents: below 4 clauses, "scores >= epsilon" and
     ;; "is joined to the cascade by an authored edge" are the same test.
     (< (* (double (reduce max 0 (vals (:candidates state0)))) alpha) epsilon)
     :transplanted-epsilon-never-binds?
     (>= (* 1.0 alpha) 0.15))))

;; ---------------------------------------------------------------------------
;; the run
;; ---------------------------------------------------------------------------

(defn cites-text?
  "F3, as `find_organise.clj`'s own reading of it: a receipt cites the pattern's
   TEXT -- a file and the line span of the clause -- and never the finder's
   score.  Recomputed here rather than assumed of every receipt this file emits."
  [receipt]
  (and (not= :score-alone (:route receipt))
       (string? (get-in receipt [:warrant :file]))
       (vector? (get-in receipt [:warrant :if-lines]))
       (string? (get-in receipt [:warrant :if-text]))
       (every? #(and (string? (:file %)) (vector? (:lines %)) (string? (:cue %)))
               (:acknowledges receipt))))

(def report-path "checks/construct-cascade.edn")

(defn- r4 [x] (/ (Math/round (* 10000.0 (double x))) 10000.0))

(defn report []
  (let [sections (library-sections library-root)
        why-repo (fo/read-repository library-root sections {:kinds #{:why}})
        wh-repo (fo/read-repository library-root sections {:kinds #{:why :how}})
        related (related-adjacency wh-repo)
        {:keys [matches seed]} (seed-and-candidates why-repo)
        ctx {:seed seed :matches matches :related related}
        found (fo/find {:context ctx
                        :route :cue-citation
                        :fires? (fn [id _] (antecedent-holds? (get-in why-repo [:entries id])))
                        :receipt (fn [id] (receipt-for (get-in why-repo [:entries id])))}
                       why-repo)
        find-row {:scenario [:organise-the-library]
                  :repository (:patterns why-repo)
                  :selected (set (:selected found))
                  :receipted (set (keys (:receipts found)))
                  :non-self-certifying (into #{} (comp (filter (comp cites-text? val))
                                                        (map key))
                                             (:receipts found))
                  :zero-mass #{zero-mass-pattern}
                  :absence (:absence found)}
        runs (into (sorted-map)
                   (for [t temperaments]
                     (let [final (run t ctx)]
                       [(:id t) (assoc final :cascade (cascade-of final seed why-repo))])))
        acks (vec (mapcat #(acknowledgements (get-in why-repo [:entries %]))
                          (sort (into (set seed)
                                      (mapcat #(admitted-of (val %)) runs)))))]
    (sorted-map
     :as-of (sorted-map
             :sections (count sections)
             :patterns (count (:patterns why-repo))
             :read-digest (read-digest wh-repo)
             :authored-why-edges (count (:edges why-repo))
             :authored-why-how-edges (count (:edges wh-repo))
             :patterns-touching-an-edge
             (count (into (set (keys (:stands-on why-repo)))
                          (mapcat val) (:stands-on why-repo)))
             :why-acyclic? (:acyclic? why-repo)
             :union-is-cyclic (vec (:cycles wh-repo)))
     :tension (sorted-map
               :id (:id tension)
               :source (:source tension)
               :clause-hits (into (sorted-map)
                                  (for [c (:clauses tension)]
                                    [(:id c)
                                     (count (filter (fn [e]
                                                      (some #(= (:id c) (:clause %))
                                                            (acknowledgements e)))
                                                    (vals (:entries why-repo))))]))
               :match-distribution (into (sorted-map) (frequencies (vals matches))))
     :find (sorted-map
            :selected (vec (:selected found))
            :seed-size (count seed)
            :candidates (- (count matches) (count seed))
            :zero-mass-pattern zero-mass-pattern
            :laws (into (sorted-map)
                        (for [[law holds?] fo/find-laws] [law (boolean (holds? find-row))])))
     :temperaments (differ-only-in-the-stop budgeted-temperament floored-temperament)
     :runs (into (sorted-map)
                 (for [[id final] runs]
                   [id (sorted-map
                        :stop (:stop final)
                        :steps (:steps final)
                        :members (count (:members final))
                        :admitted (vec (admitted-of final))
                        :record (mapv (fn [e]
                                        (sorted-map :step (:step e) :edit (:edit e)
                                                    :by (:by e)
                                                    :pattern (:pattern e)
                                                    :reason (:reason e)))
                                      (:record final))
                        :cascade-nodes (count (get-in final [:cascade :nodes]))
                        :cascade-edges (count (get-in final [:cascade :edges]))
                        :laws (into (sorted-map)
                                    (for [[law holds?] (dissoc fo/organise-laws :O4)]
                                      [law (boolean (holds? (:cascade final)))]))
                        :o4 :not-exercised-nothing-is-played
                        :prior-separation (degree-relation-authors-no-edge
                                           (:cascade final) related))]))
     :cascades-differ?
     (not= (get-in runs [(:id budgeted-temperament) :members])
           (get-in runs [(:id floored-temperament) :members]))
     :scores-at-step-0 (vec (for [[p s] (take 12 (ranked (initial-state ctx)))]
                              [p (r4 s) (get matches p)
                               (degree related seed p)]))
     :measurement (measurement ctx)
     :controls (sorted-map
                :determinism-failures (determinism ctx)
                :library-correspondence (library-correspondence why-repo)
                :grain-separation (grain-separation ctx)
                :grain-conjunct-mutations (grain-conjunct-mutations ctx)
                :citations (count acks)
                :citations-that-do-not-read-back (citations-verified acks)
                :negative-controls-declared (vec declared-controls)
                :negative-controls
                (negative-controls (get-in runs [(:id budgeted-temperament) :cascade])
                                   acks related)))))

(defn require-pass! [result]
  (let [failures
        (concat
         (for [[law holds?] (get-in result [:find :laws]) :when (not holds?)]
           {:where :find :finding :law-fails :law law})
         (when-not (get-in result [:temperaments :holds?])
           [{:where :temperaments :finding :temperaments-differ-in-more-than-the-stop}])
         (when-not (:cascades-differ? result)
           [{:where :runs :finding :two-stopping-rules-one-cascade}])
         (for [[id row] (:runs result)
               [law holds?] (:laws row) :when (not holds?)]
           {:where :runs :finding :law-fails :law law :temperament id})
         (for [[id row] (:runs result)
               :when (not (get-in row [:prior-separation :cascade-edges-are-why-only?]))]
           {:where :runs :finding :cascade-edge-not-from-the-authored-why-relation :temperament id})
         (for [[id row] (:runs result) :when (empty? (:admitted row))]
           {:where :runs :finding :constructor-admitted-nothing :temperament id})
         (for [c (get-in result [:controls :library-correspondence])
               :when (not (and (:in-repository? c) (:source-line-is-a-then? c)))]
           {:where :controls :finding :library-correspondence :rule (:rule c)})
         (for [r (get-in result [:controls :grain-separation])]
           {:where :controls :finding :grain-leak :rule r})
         (when (every? #(= :silent (:enriched %))
                       (get-in result [:controls :grain-conjunct-mutations]))
           [{:where :controls :finding :grain-conjunct-is-decoration}])
         (for [d (get-in result [:controls :determinism-failures])]
           (assoc d :where :controls))
         (for [c (get-in result [:controls :citations-that-do-not-read-back])]
           {:where :controls :finding :citation-does-not-read-back :citation c})
         (for [c (get-in result [:controls :negative-controls])
               :when (not (and (:exercised? c) (:caught? c)))]
           {:where :controls
            :finding (if (:exercised? c) :mutation-slipped :mutation-not-exercised)
            :control (:control c)})
         (when-not (get-in result [:as-of :why-acyclic?])
           [{:where :as-of :finding :why-relation-is-cyclic}]))]
    (when (seq failures)
      (throw (ex-info "construct-cascade: law or control failed"
                      {:finding (or (:finding (first failures)) :unknown)
                       :failures (vec failures)})))
    result))

(defn -main [& _]
  (try
    (let [result (require-pass! (report))
          m (:measurement result)]
      (spit report-path (with-out-str (pprint/pprint result)))
      (println (format "library: %d sections, %d patterns, %d authored @why edges, %d @why+@how; read-digest %s"
                       (get-in result [:as-of :sections])
                       (get-in result [:as-of :patterns])
                       (get-in result [:as-of :authored-why-edges])
                       (get-in result [:as-of :authored-why-how-edges])
                       (subs (get-in result [:as-of :read-digest]) 0 8)))
      (println (format "tension %s: seed %d (find F1-F4 %s), candidates %d, clause hits %s"
                       (name (:id tension))
                       (get-in result [:find :seed-size])
                       (pr-str (get-in result [:find :laws]))
                       (get-in result [:find :candidates])
                       (pr-str (get-in result [:tension :clause-hits]))))
      (println (format "the two temperaments differ only in the stop: %s %s"
                       (get-in result [:temperaments :holds?])
                       (pr-str (get-in result [:temperaments :stops]))))
      (doseq [[id row] (:runs result)]
        (println (format "  %-32s %2d edit(s), stop %-34s members %2d, cascade %2d nodes/%2d edges, O1-O3 %s"
                         (name id) (:steps row) (pr-str (:stop row)) (:members row)
                         (:cascade-nodes row) (:cascade-edges row) (pr-str (:laws row))))
        (doseq [e (:record row)]
          (println (format "    step %2d  %-6s %s"
                           (:step e) (name (:edit e))
                           (pr-str (or (:pattern e) (:reason e)))))))
      (println "\n── the pre-registered measurement: is the degree term distinguishable from uniform? ──")
      (doseq [a (:arms m)]
        (println (format "  %-32s real %s stop %s / uniform %s stop %s -> differs=%s"
                         (name (:temperament a)) (pr-str (:admitted a)) (pr-str (:stop a))
                         (pr-str (:admitted-uniform a)) (pr-str (:stop-uniform a))
                         (:differs? a))))
      (println (format "  %d of %d scored candidates have degree > 0 at step 0; degrees %s; max match %d"
                       (:candidates-with-degree-above-zero m) (:candidates-scored m)
                       (pr-str (:degree-distribution m)) (:max-match m)))
      (println (format "  the %d are %s -- all in sections a person organised by hand: %s"
                       (:candidates-with-degree-above-zero m)
                       (pr-str (vec (keys (:degree-positive-candidates m))))
                       (pr-str (vec (:sections-supplying-the-degree-term m)))))
      (println (format "  VERDICT distinguishable-from-uniform = %s (without the floor arm: %s)"
                       (:distinguishable-from-uniform? m)
                       (:distinguishable-without-the-floor? m)))
      (println (format "controls: %d citations re-read, %d unreadable; correspondence %d/%d; grain leaks %d; mutations %d declared, %d slipped"
                       (get-in result [:controls :citations])
                       (count (get-in result [:controls :citations-that-do-not-read-back]))
                       (count (filter #(and (:in-repository? %) (:source-line-is-a-then? %))
                                      (get-in result [:controls :library-correspondence])))
                       (count (get-in result [:controls :library-correspondence]))
                       (count (get-in result [:controls :grain-separation]))
                       (count declared-controls)
                       (count (remove #(and (:exercised? %) (:caught? %))
                                      (get-in result [:controls :negative-controls])))))
      (println (format "wrote %s" report-path))
      (println "construct-cascade: PASS exit-convention=0-pass/1-fail")
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println (str "construct-cascade: FAIL finding="
                    (name (or (:finding (ex-data e)) :unknown))
                    " exit-convention=0-pass/1-fail"))
      (pprint/pprint (:failures (ex-data e)))
      (shutdown-agents)
      (System/exit 1))))
