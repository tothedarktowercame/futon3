(ns find-organise
  "The generic `find` / `organise` path of P-validated-R5 §3e, over ANY library
   section rather than over `library/snatch` alone.

   Worklist row `:L6` (futon3/holes/labs/library-contract/worklist.edn).  Before
   this file the path existed twice and both copies were Snatch-shaped:
   `checks/find_snatch.clj` parsed `library/snatch` inline, and
   `checks/playout_snatch.clj:228-260` carried its own `@why` reader,
   `up-closure` and `induced-edges` against the same hardcoded directory.  The
   Lean side declared the two functions and refused both bodies
   (`mathlib4/DarkTower/WarMachine/Holes.lean:264` `find`, `:308` `organise`).

   What is here:

     read-repository   a Repository (patterns, standsOn, acyclic) from any set
                       of library sections -- Holes.lean:121
     find              Tension -> Repository -> FindResult      -- Holes.lean:264
     organise          Cascade policy -> Set P -> Repository P -> Cascade P
                                                                -- Holes.lean:308
     F1..F4, O1..O4    the laws in the NARROWED, witnessed-instance form the
                       2026-08-31 scope amendment gave them: predicates on a
                       recorded row / a recorded CascadeDiff, not universals
                       over a refused implementation.

   `organise` takes three arguments, not the two `Holes.lean:308` declares.  The
   third is the temperament -- a cascade at policy grain -- and it is taken on
   the foresight LA1c-restatement.md §10 offers L6: a two-argument `organise`
   has to be redefined when LA2 lands, a three-argument one does not.  LA2 has
   landed and the firing loop is `checks/playout_snatch.clj`'s (`fire`,
   `construct`, `apply-edit`); it is not joined to this function yet, so the
   temperament here is still READ rather than fired.  It is read for exactly two
   fields, and both are law-bearing:

     :closure     which patterns enter the cascade -- and this is not decoration.
                  `playout_snatch.clj` takes the up-closure under standsOn
                  (P-validated-R5 §2.1d, \"the cascade of a run is the
                  up-closure\"); `wmCascadeDiffFixture` (Holes.lean:319) keeps
                  nodes = selected and fast-forwards through the unselected
                  bridge.  Those are two organise policies over one repository,
                  and O1's narrowed form (nodes = selected u addedByOrganise u
                  admittedBy) admits both.  Which one runs is a policy-grain
                  decision, so
                  the temperament is where it belongs.
     :precedence  O4 -- precedence is recorded on the cascade as a
                  collection-level field, not derived from the patterns.

   Both arms take their edges from ONE definition, `fast-forward` over the
   cascade's own nodes.  That is not two code paths reconciled by hand: an
   up-closure under standsOn is closed under ancestors, so every intermediate on
   an authored path between two of its members is itself a member, `ReachOutside`
   has nothing to route around, and fast-forward collapses to the induced
   authored edges.  O2 and O3 therefore hold of both arms by the same code.

   The directive reader matches `checks/library_graph_lint.clj:207-210` -- the
   same `;;`-comment strip and the same `target-pattern` -- and `-main` proves
   it does by re-reading every section through that linter's own `scan-library`
   and refusing any disagreement.  Two readers of one graph is the defect
   `find_snatch.clj`'s `representation-mismatches` refuses one level up."
  (:refer-clojure :exclude [find])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; the repository -- Holes.lean:121
;; ---------------------------------------------------------------------------

;; Copied from checks/library_graph_lint.clj:22 rather than re-derived: a second
;; hand-maintained spelling of what counts as an edge target is the same defect
;; find_snatch.clj:100-120 refuses for antecedent text.  -main proves the copy.
(def target-pattern #"[A-Za-z0-9_.-]+/[A-Za-z0-9_./'-]+")
(def edge-directive-re #"\s*@(why-posthoc|why|how|see-also)\s+(.+?)\s*")

(defn- normalise [s]
  (some-> s str/trim (str/replace #"\s+" " ")))

(defn- clause-block
  "The `+ LABEL:` block of a flexiarg body, with the 1-based line span of its
   content.  find_snatch.clj:31-44, moved here unchanged."
  [lines label]
  (when-let [marker (first (keep-indexed
                            (fn [i line]
                              (when (re-matches
                                     (re-pattern (str "^\\s+\\+ " label ":\\s*$"))
                                     line)
                                i))
                            lines))]
    (let [content (->> (subvec lines (inc marker))
                       (take-while #(and (not (str/blank? %))
                                         (not (re-matches #"^\s+\+ \S.*" %))))
                       vec)]
      (when (seq content)
        {:lines [(+ marker 2) (+ marker 1 (count content))]
         :text (normalise (str/join " " content))}))))

(defn qualified
  "The canonical pattern id: section-qualified, as the library graph lint keys
   it (`library_graph_lint.clj:174-177`), so that a cross-section `@why` target
   resolves instead of turning into a bare name that collides."
  [section local]
  (keyword (str (name section) "/" (name local))))

(defn local
  "The bare name, for callers whose artefacts are keyed inside one section."
  [id]
  (keyword (name id)))

(defn- parse-pattern-file [section file]
  (let [lines (vec (str/split-lines (slurp file)))
        path-id (qualified section (str/replace (.getName ^java.io.File file)
                                                #"\.flexiarg$" ""))
        directive-id (some #(some-> (re-matches #"@flexiarg (\S+)" %) second keyword)
                           lines)
        if-clause (clause-block lines "IF")
        however-clause (clause-block lines "HOWEVER")
        edges (into []
                    (comp (map-indexed vector)
                          (mapcat (fn [[i line]]
                                    (let [code (first (str/split line #";;" 2))]
                                      (when-let [[_ kind tail] (re-matches edge-directive-re code)]
                                        (for [token (str/split tail #"\s+")
                                              :when (re-matches target-pattern token)]
                                          {:from path-id :to (keyword token)
                                           :kind (keyword kind) :line (inc i)}))))))
                    lines)]
    [path-id (cond-> {:id path-id
                      :directive-id directive-id
                      :file (str "library/" (name section) "/" (.getName ^java.io.File file))
                      :edges edges}
               if-clause (assoc :if-lines (:lines if-clause) :if-text (:text if-clause))
               however-clause (assoc :however-lines (:lines however-clause)
                                     :however-text (:text however-clause)))]))

(defn- section-files [library-root section]
  (->> (file-seq (io/file (str library-root "/" (name section))))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".flexiarg"))
       (sort-by #(.getName ^java.io.File %))))

(defn- descend
  "Every id reachable from `start` under `rel`, `start` excluded unless it is
   reachable from itself."
  [rel start]
  (loop [seen #{} frontier (get rel start #{})]
    (if (empty? frontier)
      seen
      (let [seen' (into seen frontier)]
        (recur seen' (set/difference (into #{} (mapcat #(get rel % #{})) frontier)
                                     seen'))))))

(defn read-repository
  "A Repository over `sections` of `library-root`.

   `:stands-on` is the authored `@why` relation RESTRICTED to the repository's
   own patterns -- a `@why` whose target is outside the sections read is
   recorded in `:dangling` and is not an edge.  That restriction is what keeps
   `organise` inside F1/O1: a pattern nobody read cannot become a cascade node.
   (`playout_snatch.clj`'s reader admitted such a target as a leaf; no acting
   set has reached one, so no recorded cascade changes.)

   `:acyclic?` is `acyclicDescent standsOn` (Holes.lean:124) checked by descent
   rather than assumed: the Repository structure REQUIRES it, so a library that
   is cyclic has no Repository and the caller must be told, not silently given
   one."
  ([library-root sections] (read-repository library-root sections {:kinds #{:why}}))
  ([library-root sections {:keys [kinds] :or {kinds #{:why}}}]
   (let [entries (into (sorted-map)
                       (mapcat (fn [section]
                                 (map #(parse-pattern-file section %)
                                      (section-files library-root section))))
                       sections)
         patterns (set (keys entries))
         all-edges (->> (vals entries)
                        (mapcat :edges)
                        (filter #(contains? kinds (:kind %)))
                        (sort-by (juxt :from :to :kind)))
         [authored dangling] [(filter #(contains? patterns (:to %)) all-edges)
                              (remove #(contains? patterns (:to %)) all-edges)]
         stands-on (reduce (fn [m {:keys [from to]}] (update m from (fnil conj #{}) to))
                           {} authored)
         cyclic (into (sorted-set)
                      (filter #(contains? (descend stands-on %) %))
                      patterns)]
     {:library-root library-root
      :sections (vec sections)
      :patterns patterns
      :entries entries
      :stands-on stands-on
      :edges (vec authored)
      :dangling (vec dangling)
      :acyclic? (empty? cyclic)
      :cycles (vec cyclic)
      :id-directive-mismatches (into (sorted-set)
                                     (comp (filter #(not= (:id %) (:directive-id %)))
                                           (map :id))
                                     (vals entries))})))

(defn warrant
  "F3's citation: the pattern's own file and the line span of the clause the
   receipt names.  A receipt that cites this cites TEXT, never a score."
  [repository id]
  (let [{:keys [file if-lines if-text however-lines however-text]}
        (get-in repository [:entries id])]
    (cond-> (sorted-map :file file :if-lines if-lines :if-text if-text)
      however-text (assoc :however-lines however-lines
                          :however-text however-text))))

;; ---------------------------------------------------------------------------
;; find -- Holes.lean:264
;; ---------------------------------------------------------------------------

(defn find
  "Tension -> Repository -> FindResult.

   The tension supplies its own antecedent evaluator, `:fires?`, a predicate on
   `[id context]`.  That is the whole of what is domain-specific: Snatch reads a
   game state, another domain reads whatever its states are, and neither can
   reach a pattern the repository does not hold -- F1 is true by CONSTRUCTION
   here, because the candidate set IS `repository.patterns`.  (`find_snatch.clj`
   filtered a separately held runner collection and then checked containment
   afterwards.  The check it kept -- that no runner id is unauthored -- is the
   other direction and stays where it is.)

   `:receipt` may add fields; it may not remove `:route` or `:warrant`, so no
   caller can make a receipt self-certifying by omission (F3)."
  [{:keys [context fires? route receipt] :or {route :structured-antecedent}} repository]
  (let [firing (into [] (filter #(fires? % context)) (sort (:patterns repository)))]
    (sorted-map
     :absence (when (empty? firing) :no-pattern-addresses-this-tension)
     :receipts (into (sorted-map)
                     (map (fn [id]
                            [id (into (sorted-map
                                       :if true
                                       :route route
                                       :warrant (warrant repository id))
                                      (when receipt (receipt id)))]))
                     firing)
     :selected (vec firing))))

;; ---------------------------------------------------------------------------
;; organise -- Holes.lean:308, with the third argument LA1c §10 asks for
;; ---------------------------------------------------------------------------

(defn up-closure
  "Every pattern the given ones stand on, transitively (P-validated-R5 §2.1d)."
  [repository ids]
  (let [rel (:stands-on repository)]
    (loop [seen #{} frontier (set ids)]
      (if (empty? frontier)
        seen
        (let [seen' (into seen frontier)]
          (recur seen' (set/difference (into #{} (mapcat #(get rel % #{})) frontier)
                                       seen')))))))

(defn reach-outside
  "`ReachOutside` (Holes.lean:297-300): an authored path from u to v whose
   INTERMEDIATE vertices are all outside `inside`.  Endpoints are not
   constrained here; `fast-forward` constrains them."
  [inside stands-on u]
  (loop [seen #{} frontier (get stands-on u #{}) reached #{}]
    (if (empty? frontier)
      reached
      (let [seen' (into seen frontier)
            through (remove #(contains? inside %) frontier)]
        (recur seen'
               (set/difference (into #{} (mapcat #(get stands-on % #{})) through) seen')
               (into reached frontier))))))

(defn fast-forward
  "O3: `edges u v` iff u and v are nodes and an authored path joins them through
   patterns that are not nodes -- `fastForward` (Holes.lean:302-305).  Joe's
   \"fast-forward the edges that didn't fit\"."
  [nodes stands-on]
  (into (sorted-set)
        (for [u (sort nodes)
              v (sort (reach-outside nodes stands-on u))
              :when (contains? nodes v)]
          [u v])))

(def up-closure-temperament
  "The temperament `playout_snatch.clj` has always run: take the up-closure, so
   the cascade of a run is everything the acting patterns stand on."
  {:id :take-the-up-closure :grain :policy :closure :stands-on-up-closure})

(def selected-only-temperament
  "The temperament `wmCascadeDiffFixture` (Holes.lean:319) records: keep the
   selected patterns and fast-forward past the bridges."
  {:id :keep-only-what-was-found :grain :policy :closure :selected-only})

(defn organise
  "Cascade policy -> Set P -> Repository P -> Cascade P.

   `temperament` is a cascade at policy grain.  This function READS it (`:closure`,
   `:precedence`); it does not fire it.  Firing a policy-grain rule -- so that a
   temperament's THEN emits the edit rather than a keyword naming it -- landed in
   `:LA2` and lives in `checks/playout_snatch.clj` (`fire`, `construct`,
   `apply-edit`), against library/snatch's authored temperaments.  It is not
   wired to this function: the two temperaments HERE declare a closure policy as
   a keyword and emit no edits, so `:admitted-by` is empty on every cascade this
   builds, and no node arrives that is neither selected nor stood-on.  Joining
   the two is a further row, not a side effect of LA2."
  [temperament selected repository]
  (when-not (set/subset? (set selected) (:patterns repository))
    (throw (ex-info "organise: selected escapes the repository"
                    {:finding :o1-selected-outside-repository
                     :outside (sort (set/difference (set selected) (:patterns repository)))})))
  (let [selected (set selected)
        added (case (:closure temperament)
                :selected-only #{}
                :stands-on-up-closure (set/difference (up-closure repository selected)
                                                      selected)
                (throw (ex-info "organise: temperament declares no closure policy"
                                {:finding :no-closure-policy :temperament temperament})))
        nodes (set/union selected added)]
    {:temperament (:id temperament)
     :selected selected
     :added-by-organise added
     ;; O1's third origin (Holes.lean CascadeDiff.admittedBy). Empty here, and
     ;; carried rather than omitted so a cascade that DID admit could not be
     ;; recorded as authored closure.
     :admitted-by #{}
     :nodes nodes
     :edges (fast-forward nodes (:stands-on repository))
     :precedence (vec (:precedence temperament))
     :acyclic? (:acyclic? repository)}))

;; ---------------------------------------------------------------------------
;; the laws, in the narrowed form the 2026-08-31 scope amendment gave them
;; ---------------------------------------------------------------------------
;; Every predicate below is a function of a RECORDED row, exactly as the Lean
;; declarations are after the amendment.  None quantifies over `find` or
;; `organise`: that universal is what the amendment removed, on the ground that
;; no serialized evidence could establish the correspondence.

(defn f1-containment
  "findF1Containment (Holes.lean:266-268)."
  [{:keys [selected repository absence]}]
  (and (set/subset? selected repository)
       (or (seq selected) (= absence :no-pattern-addresses-this-tension))
       true))

(defn f2-receipted
  "findF2Receipted (Holes.lean:271)."
  [{:keys [selected receipted]}]
  (set/subset? selected receipted))

(defn f3-non-self-certifying
  "findF3NonSelfCertifying (Holes.lean:274)."
  [{:keys [selected non-self-certifying]}]
  (set/subset? selected non-self-certifying))

(defn f4-falsifiable
  "findF4Falsifiable (Holes.lean:277-279)."
  [{:keys [repository selected zero-mass]}]
  (and (seq repository)
       (boolean (some #(and (contains? repository %) (not (contains? selected %)))
                      zero-mass))))

(def find-laws
  {:F1 f1-containment :F2 f2-receipted :F3 f3-non-self-certifying :F4 f4-falsifiable})

(defn o1-nodes-recorded
  "organiseO1NodesRecorded (Holes.lean:353-360), the THREE-way union: a node is
   in the cascade because `find` selected it, because organise closed over an
   authored edge to it, or because a policy-grain THEN admitted it.  `:LA2` added
   the third term; `organise` never writes it (see below), so it is empty on
   every row this file builds and the law still has to hold with it there."
  [{:keys [nodes selected added-by-organise admitted-by]}]
  (= nodes (set/union selected added-by-organise (set admitted-by))))

(defn o2-authored-reachability
  "organiseO2AuthoredReachability (Holes.lean:347-351): every organised edge is
   authored reachability, never similarity or co-occurrence."
  [{:keys [edges stands-on]}]
  (every? (fn [[u v]] (contains? (descend stands-on u) v)) edges))

(defn o3-fast-forward
  "organiseO3FastForward (Holes.lean:354-358) -- a biconditional, so an edge the
   cascade omits fails it exactly as an edge it invents does."
  [{:keys [edges nodes stands-on]}]
  (= (set edges) (set (fast-forward nodes stands-on))))

(defn o4-precedence-governance
  "organiseO4PrecedenceGovernance (Holes.lean:361-366): where the precedence
   changed, the acting order or the score changed with it."
  [{:keys [precedence-before precedence-after acting-order-before acting-order-after
           score-before score-after]}]
  (or (= precedence-before precedence-after)
      (not= acting-order-before acting-order-after)
      (not= score-before score-after)))

(def organise-laws
  {:O1 o1-nodes-recorded :O2 o2-authored-reachability
   :O3 o3-fast-forward :O4 o4-precedence-governance})

(defn check-rows [laws rows]
  (vec (for [row rows
             [law holds?] (sort-by key laws)
             :when (not (holds? row))]
         (sorted-map :law law :row (:scenario row)))))

;; ---------------------------------------------------------------------------
;; the pinned evidence
;; ---------------------------------------------------------------------------

(def find-fixture-path "checks/find-snatch.edn")
(def cascade-fixture-path "checks/snatch-cascade.edn")

(def find-fixture-sha256
  "The sha256 the four `findF*` declarations pin (Holes.lean:266,270,273,276).
   The laws are checked against THIS file; a regenerated one is a different
   fixture and would need the Lean pin moved with it."
  "839897ef8fe44952403700bd237389449ae4735d3da7df8239b1b94dc7ef4dfa")

(defn sha256-file [path]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest digest (.readAllBytes (io/input-stream path)))))))

(defn- receipt-cites-text? [receipt]
  (and (not= :score-alone (:route receipt))
       (string? (get-in receipt [:warrant :file]))
       (vector? (get-in receipt [:warrant :if-lines]))
       (string? (get-in receipt [:warrant :if-text]))))

(defn find-receipt-table
  "The recorded fixture read as a `FindReceiptTable` (Holes.lean:261): one row
   per ROUND, plus one per scenario over the round union.  The per-round rows
   are the finer evidence -- a scenario whose union satisfies F4 can still have
   a round that selected the zero-mass pattern."
  [fixture]
  (let [repository (set (:repository fixture))]
    (vec
     (for [{:keys [treatment disposition round-results selected-union f4]} (:scenarios fixture)
           row (conj (vec (for [{:keys [round find]} round-results]
                            {:scenario [treatment disposition round]
                             :selected (set (:selected find))
                             :receipts (:receipts find)
                             :absence (:absence find)}))
                     {:scenario [treatment disposition :union]
                      :selected (set selected-union)
                      :receipts (into {} (mapcat #(get-in % [:find :receipts])) round-results)
                      :absence (when (empty? selected-union)
                                 :no-pattern-addresses-this-tension)})]
       (let [{:keys [selected receipts absence scenario]} row]
         {:scenario scenario
          :repository repository
          :selected selected
          :receipted (set (keys receipts))
          :non-self-certifying (into #{} (comp (filter (comp receipt-cites-text? val))
                                               (map key))
                                     receipts)
          :zero-mass #{(:zero-mass-pattern f4)}
          :absence absence})))))

(defn cascade-diff-table
  "The recorded cascade fixture read as `CascadeDiff`s (Holes.lean:294): one per
   scenario, `:patterns` before and `:exchange-first` after -- the two wirings of
   one collection that P-validated-R5 §3b calls the re-wired variant.

   `snatch-cascade.edn`'s `:nodes` key holds the SELECTED set (O1's prose
   reading, P-validated-R5:499); the Lean narrowed O1 wants the union with
   `:added-by-organise`.  Both are in the row, so the union is recomputed here
   rather than read."
  [fixture repository]
  (let [by-key (group-by (juxt :treatment :disposition) (:scenarios fixture))]
    (vec
     (for [[[treatment disposition] rows] (sort-by key by-key)
           :let [before (some #(when (= :patterns (:policy %)) %) rows)
                 after (some #(when (= :exchange-first (:policy %)) %) rows)
                 qualify (fn [ids] (into #{} (map #(qualified :snatch %)) ids))
                 cascade (organise (assoc up-closure-temperament
                                          :precedence (:precedence before))
                                   (qualify (:acting before))
                                   repository)]]
       (merge cascade
              {:scenario [treatment disposition]
               :stands-on (:stands-on repository)
               :recorded-selected (qualify (:nodes before))
               :recorded-added (qualify (:added-by-organise before))
               :precedence-before (:precedence before)
               :precedence-after (:precedence after)
               :acting-order-before (:acting before)
               :acting-order-after (:acting after)
               :score-before (:score before)
               :score-after (:score after)})))))

(defn organise-reproduces-record?
  "The generic path is the SAME path, or it is a second implementation nobody
   checks: every recorded cascade must come back out of `organise`."
  [diff]
  (and (= (:selected diff) (:recorded-selected diff))
       (= (:added-by-organise diff) (:recorded-added diff))))

;; ---------------------------------------------------------------------------
;; the C59 fixture, in Clojure -- the second organise temperament
;; ---------------------------------------------------------------------------

(def c59-repository
  "`wmCascadeDiffFixture` (Holes.lean:310-334): 0 = probe, 1 = the unselected
   bridge, 2 = remedy; authored 0->1->2; selected {0,2}.  Transcribed so the
   `:selected-only` arm is exercised against a case where fast-forward has
   something to route around -- library/snatch under the up-closure temperament
   never does."
  {:patterns #{:c59/probe :c59/bridge :c59/remedy}
   :entries {}
   :stands-on {:c59/probe #{:c59/bridge} :c59/bridge #{:c59/remedy}}
   :acyclic? true})

(def c59-selected #{:c59/probe :c59/remedy})

;; ---------------------------------------------------------------------------
;; negative controls
;; ---------------------------------------------------------------------------

(defn mutate-find-row [row law]
  (case law
    :F1 (update row :selected conj :outside/repository)
    :F2 (update row :receipted disj (first (sort (:selected row))))
    :F3 (update row :non-self-certifying disj (first (sort (:selected row))))
    :F4 (update row :repository set/difference (:zero-mass row))
    row))

(def extra-diff-controls
  "One control per law CLAUSE that the per-law mutation above does not reach.
   `:O1-orphan` is the one `:LA2` added: O1's content is that there is no fourth
   way into the cascade, so a node with none of the three recorded origins must be
   rejected.  The `:O1` mutation only removes a node, which tests the other
   direction of the same equality and would pass a carrier that admitted anything."
  {:O1-orphan [:O1 (fn [diff] (update diff :nodes conj :orphan/no-recorded-origin))]})

(defn mutate-diff [diff law]
  (case law
    :O1 (update diff :nodes disj (first (sort (:nodes diff))))
    :O2 (update diff :edges conj [(first (sort (:nodes diff))) :invented/target])
    :O3 (update diff :edges disj (first (sort (:edges diff))))
    :O4 (assoc diff :acting-order-after (:acting-order-before diff)
               :score-after (:score-before diff))
    diff))

(defn- first-row-with-selected [rows]
  (first (filter #(seq (:selected %)) rows)))

(defn- first-diff-with-edges [diffs]
  (first (filter #(seq (:edges %)) diffs)))

(defn negative-controls
  "Each law, mutated on a real recorded row, must be REJECTED.  A law that
   cannot fail is not a law -- the mirror discipline of checks/README.md."
  [rows diffs]
  (let [row (first-row-with-selected rows)
        diff (first-diff-with-edges diffs)]
    (vec
     (concat
      (for [[law holds?] (sort-by key find-laws)
            :when (holds? (mutate-find-row row law))]
        (sorted-map :control :find :law law :finding :mutation-slipped))
      (for [[law holds?] (sort-by key organise-laws)
            :when (holds? (mutate-diff diff law))]
        (sorted-map :control :organise :law law :finding :mutation-slipped))
      (for [[control [law mutate]] (sort-by key extra-diff-controls)
            :when ((get organise-laws law) (mutate diff))]
        (sorted-map :control :organise :law control :finding :mutation-slipped))))))

;; ---------------------------------------------------------------------------
;; the reader-agreement control
;; ---------------------------------------------------------------------------

(defn reader-disagreements
  "`checks/library_graph_lint.clj` is the library's canonical graph reader.  If
   this file's reader and that one disagree about a section's `@why` edges, one
   of them is wrong and the laws below are being checked over the wrong graph."
  [library-root sections]
  (let [scan (requiring-resolve 'checks.library-graph-lint/scan-library)
        scanned (scan library-root)
        section? (set (map name sections))
        ids (:ids scanned)
        theirs (into (sorted-set)
                     (comp (filter #(= :why (:kind %)))
                           ;; a target that names no file is dangling for both
                           ;; readers; this control is about the ones that resolve
                           (filter #(contains? ids (:to %)))
                           (filter #(section? (first (str/split (:from %) #"/"))))
                           (filter #(section? (first (str/split (:to %) #"/"))))
                           (map (juxt #(keyword (:from %)) #(keyword (:to %)))))
                     (:edges scanned))
        ours (into (sorted-set)
                   (map (juxt :from :to))
                   (:edges (read-repository library-root sections)))]
    {:theirs (count theirs) :ours (count ours)
     :only-theirs (vec (set/difference theirs ours))
     :only-ours (vec (set/difference ours theirs))}))

;; ---------------------------------------------------------------------------
;; the run
;; ---------------------------------------------------------------------------

(def report-path "checks/find-organise.edn")
(def library-root "library")

(def generality-sections
  "Three sections that are not Snatch, chosen so the path is exercised where the
   library is organised, where it is partly organised, and where it is not
   organised at all -- `library/ants` has five patterns and zero authored edges,
   and P-validated-R5 §1b addendum 3 names it as the control domain."
  [:cycle-machine :war-room :ants])

(defn endpoints
  "The patterns that are not STRICT INTERMEDIATES of the authored graph: the
   frontier (nothing stands on them -- the shape an acting set has) together
   with the roots (they stand on nothing -- the ultimate authorities).  This is
   the C59 shape generalised: everything dropped is a bridge, so it is the
   selection under which the two temperaments have to disagree if fast-forward
   does anything at all.  Selecting the frontier alone would not do: a frontier
   pattern is never a target, so no authored path ends at one, and the
   selected-only arm would be edgeless by construction rather than by
   measurement."
  [repository]
  (let [stands-on (:stands-on repository)
        targets (into #{} (mapcat val) stands-on)
        frontier (set/difference (:patterns repository) targets)
        roots (into #{} (remove #(seq (get stands-on % #{}))) (:patterns repository))]
    (set/union frontier roots)))

(defn- section-measures [section]
  (let [repo (read-repository library-root [section])
        selected (endpoints repo)
        up (organise up-closure-temperament selected repo)
        only (organise selected-only-temperament selected repo)
        with-graph #(assoc % :stands-on (:stands-on repo))]
    (sorted-map
     :section section
     :patterns (count (:patterns repo))
     :authored-why-edges (count (:edges repo))
     :dangling-targets (count (:dangling repo))
     :acyclic? (:acyclic? repo)
     :organised-fraction (if (zero? (count (:patterns repo)))
                           0
                           (/ (double (count (keys (:stands-on repo))))
                              (count (:patterns repo))))
     :selected-endpoints (count selected)
     :strict-intermediates-dropped (- (count (:patterns repo)) (count selected))
     :nodes-up-closure (count (:nodes up))
     :nodes-selected-only (count (:nodes only))
     :edges-up-closure (count (:edges up))
     :edges-selected-only (count (:edges only))
     :temperaments-differ? (not= (:edges up) (:edges only))
     :laws-hold? (every? true?
                  (for [c [up only], [_ holds?] organise-laws]
                    (boolean (holds? (with-graph (merge {:precedence-before []
                                                         :precedence-after []} c)))))))))

(defn report []
  (let [snatch (read-repository library-root [:snatch])
        fixture-sha (sha256-file find-fixture-path)
        fixture (edn/read-string (slurp find-fixture-path))
        rows (find-receipt-table fixture)
        cascade (edn/read-string (slurp cascade-fixture-path))
        diffs (cascade-diff-table cascade snatch)
        c59 (organise selected-only-temperament c59-selected c59-repository)]
    (sorted-map
     :as-of (sorted-map :find-fixture fixture-sha
                        :find-fixture-pinned find-fixture-sha256
                        :find-fixture-matches-lean-pin? (= fixture-sha find-fixture-sha256)
                        :cascade-fixture (sha256-file cascade-fixture-path))
     :repository (sorted-map :sections [:snatch]
                             :patterns (count (:patterns snatch))
                             :authored-why-edges (count (:edges snatch))
                             :dangling-targets (mapv #(vector (:from %) (:to %))
                                                     (:dangling snatch))
                             :acyclic? (:acyclic? snatch)
                             :id-directive-mismatches (vec (:id-directive-mismatches snatch)))
     :find (sorted-map :rows (count rows)
                       :laws (vec (sort (keys find-laws)))
                       :failures (check-rows find-laws rows))
     :organise (sorted-map
                :diffs (count diffs)
                :laws (vec (sort (keys organise-laws)))
                :failures (check-rows organise-laws diffs)
                :reproduces-record (vec (for [d diffs
                                              :when (not (organise-reproduces-record? d))]
                                          (:scenario d)))
                :o4-moved-the-score (vec (for [d diffs
                                               :when (not= (:score-before d) (:score-after d))]
                                           (:scenario d))))
     :c59 (sorted-map :edges (vec (:edges c59))
                      :nodes (vec (sort (:nodes c59)))
                      :added-by-organise (vec (sort (:added-by-organise c59)))
                      :matches-lean-fixture?
                      (= (:edges c59) #{[:c59/probe :c59/remedy]}))
     :generality (mapv section-measures generality-sections)
     :negative-controls (negative-controls rows diffs))))

(defn require-pass! [result]
  (let [failures (concat
                  (for [f (get-in result [:find :failures])] (assoc f :where :find))
                  (for [f (get-in result [:organise :failures])] (assoc f :where :organise))
                  (for [s (get-in result [:organise :reproduces-record])]
                    {:where :organise :finding :does-not-reproduce-record :row s})
                  (for [c (:negative-controls result)] (assoc c :where :negative-control))
                  (when-not (get-in result [:as-of :find-fixture-matches-lean-pin?])
                    [{:where :fixture :finding :fixture-differs-from-lean-pin}])
                  (when-not (get-in result [:c59 :matches-lean-fixture?])
                    [{:where :c59 :finding :does-not-match-lean-fixture}])
                  (when-not (get-in result [:repository :acyclic?])
                    [{:where :repository :finding :repository-is-cyclic}])
                  (when (seq (get-in result [:repository :id-directive-mismatches]))
                    [{:where :repository :finding :flexiarg-id-disagrees-with-path}])
                  (for [s (:generality result) :when (not (:laws-hold? s))]
                    {:where :generality :finding :law-fails-off-snatch :row (:section s)}))]
    (when (seq failures)
      (throw (ex-info "find-organise: law or control failed"
                      {:finding (:finding (first failures) (:law (first failures)))
                       :failures (vec failures)})))
    result))

(defn -main [& args]
  (let [flag (some #(when (str/starts-with? % "--negative-") %) args)
        negative (some-> flag (subs (count "--negative-")) str/upper-case keyword)
        result (report)
        disagreements (reader-disagreements library-root [:snatch :cycle-machine :war-room :ants])
        result (assoc result :reader-agreement
                      (sorted-map :ours (:ours disagreements) :theirs (:theirs disagreements)
                                  :only-ours (:only-ours disagreements)
                                  :only-theirs (:only-theirs disagreements)))]
    (try
      (when (or (seq (:only-ours disagreements)) (seq (:only-theirs disagreements)))
        (throw (ex-info "find-organise: reader disagrees with library_graph_lint"
                        {:finding :reader-disagreement :detail disagreements})))
      (require-pass!
       (if negative
         (cond
           (contains? find-laws negative)
           (update-in result [:find :failures]
                      into (check-rows (select-keys find-laws [negative])
                                       [(mutate-find-row (first-row-with-selected
                                                          (find-receipt-table
                                                           (edn/read-string (slurp find-fixture-path))))
                                                         negative)]))
           (contains? organise-laws negative)
           (update-in result [:organise :failures]
                      into (check-rows (select-keys organise-laws [negative])
                                       [(mutate-diff (first-diff-with-edges
                                                      (cascade-diff-table
                                                       (edn/read-string (slurp cascade-fixture-path))
                                                       (read-repository library-root [:snatch])))
                                                     negative)]))
           :else result)
         result))
      (when negative
        (println (str "find-organise: FAIL law mutation " (name negative)
                      " slipped exit-convention=0-pass/1-fail/2-mutation-slipped"))
        (shutdown-agents)
        (System/exit 2))
      (spit report-path (with-out-str (pprint/pprint result)))
      (println (format "repository library/snatch: %d patterns, %d authored @why edges, acyclic=%s"
                       (get-in result [:repository :patterns])
                       (get-in result [:repository :authored-why-edges])
                       (get-in result [:repository :acyclic?])))
      (println (format "find  F1-F4 over %d recorded rows of %s (sha256 %s, Lean pin %s): 0 failures"
                       (get-in result [:find :rows]) find-fixture-path
                       (subs (get-in result [:as-of :find-fixture]) 0 8)
                       (if (get-in result [:as-of :find-fixture-matches-lean-pin?]) "MATCHED" "MOVED")))
      (println (format "organise O1-O4 over %d recorded CascadeDiffs: 0 failures; %d reproduce the record; score moved in %s"
                       (get-in result [:organise :diffs])
                       (get-in result [:organise :diffs])
                       (pr-str (get-in result [:organise :o4-moved-the-score]))))
      (println (format "C59 fixture under the selected-only temperament: edges %s (Lean %s)"
                       (pr-str (get-in result [:c59 :edges]))
                       (if (get-in result [:c59 :matches-lean-fixture?]) "matched" "DIFFERS")))
      (doseq [s (:generality result)]
        (println (format "  %-14s %2d patterns %2d @why edges  endpoints %2d (-%d bridges)  up-closure %2d nodes/%2d edges  selected-only %2d/%2d  differ=%-5s laws=%s"
                         (name (:section s)) (:patterns s) (:authored-why-edges s)
                         (:selected-endpoints s) (:strict-intermediates-dropped s)
                         (:nodes-up-closure s) (:edges-up-closure s)
                         (:nodes-selected-only s) (:edges-selected-only s)
                         (:temperaments-differ? s) (:laws-hold? s))))
      (println (format "reader agreement with library_graph_lint: %d edges both, 0 either side alone"
                       (get-in result [:reader-agreement :ours])))
      (println (format "negative controls: %d mutations, %d slipped"
                       (+ (count find-laws) (count organise-laws)
                          (count extra-diff-controls))
                       (count (:negative-controls result))))
      (println (format "wrote %s" report-path))
      (println "find-organise: PASS exit-convention=0-pass/1-fail")
      (shutdown-agents)
      (System/exit 0)
      (catch clojure.lang.ExceptionInfo e
        (println (str "find-organise: "
                      (if negative "PASS negative-control rejected finding=" "FAIL finding=")
                      (name (or (:finding (ex-data e)) :unknown))
                      " exit-convention=0-pass/1-fail/2-mutation-slipped"))
        (when-not negative
          (pprint/pprint (:failures (ex-data e)))
          (pprint/pprint (:detail (ex-data e))))
        (shutdown-agents)
        (System/exit (if negative 0 1))))))
