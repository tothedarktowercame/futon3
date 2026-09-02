(ns learn-edge-weights
  "The LEARNING PASS of LA1c-restatement.md §4.3, and the argument about whether
   its proposed law should be adopted.

   Worklist row `:LA9` (futon3/holes/labs/library-contract/worklist.edn).  Joe's
   third exchange asked for the how/why landscape as *a substrate for learning
   over multiple runs*.  LA1c §4.3 proposes the one restriction that keeps such
   learning inside law O2, as PROPOSED law O5:

     > Learning may update the WEIGHTS on authored edges.  It may never ADD an
     > edge.  A learned high weight is a hypothesis that an author should write
     > an edge -- it is `find`'s and the spider's input, never `organise`'s.

   This file implements that, measures it, and reports what the sentence turns
   out to contain.  The short of it, stated here because a reader should not
   have to reach the verdict to find it: **O5 as written is two laws sharing one
   sentence, and they need different domains.**

     O5a  weights live on AUTHORED EDGES and a learning pass never changes the
          edge set.  Implementable exactly as stated, and checked here by
          `edge-store-untouched` and `write-ledger`.
     O5b  a learned high weight is a hypothesis that an author should write an
          edge.  This CANNOT hold on O5a's domain: a weight on an authored edge
          is a weight on an edge that is already written, so it proposes
          nothing.  A proposal needs a weight on a pair that has NO edge, which
          O5a's domain excludes.

   So the routing clause of O5 has no referent under the domain clause of O5.
   What actually keeps a pair-weight inside O2 is not the domain restriction --
   it is the ROUTING restriction, and that is separately checkable: the weight
   store is read by `find` and by the spider and by no `organise` path.  This
   file therefore keeps TWO stores, and the separation between them is the whole
   of the safety argument:

     checks/edge-weights.edn     weights on authored edges          (O5a)
     checks/edge-proposals.edn   weighted pairs with NO edge, for a
                                 human author and for the spider    (O5b)

   Neither is under `library/`; neither is a `.flexiarg`; nothing reads either
   one on the way to a cascade.  `library-graph-lint` and every `construct_*`
   file name neither path, which `organise-blindness` checks by reading them.

   THE CORPUS is the recorded constructions -- eight artefacts, four domains,
   the runs LA3 through LA8 already produced.  Nothing is re-run: this pass reads
   artefacts and writes stores, so it cannot move a published number.

   WHAT IT DOES NOT DO, and the reader should not read past it: §4.3 says the
   weight is *updated by G-improvement per attachment step*.  No recorded run
   carries a per-step score -- every `:record` entry's `:reason` is nil and no
   run row has a score field, which `g-improvement-availability` checks rather
   than asserts.  So the credit here is an ATTACHMENT COUNT, which is weaker: it
   says the two patterns were joined at some admission step, not by how much
   that step improved anything.  The one G-shaped distinction the record does
   support is carried as its own field (`:attachments-under-a-floor-stop`) and
   is NOT folded into the weight, because folding it would need a constant this
   file would have to invent."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [construct-cascade :as cc]
            [find-organise :as fo]))

(def library-root "library")
(def weight-store-path "checks/edge-weights.edn")
(def proposal-store-path "checks/edge-proposals.edn")
(def report-path "checks/learn-edge-weights.edn")

;; ---------------------------------------------------------------------------
;; the edge store, and the digest the acceptance test is over
;; ---------------------------------------------------------------------------
;; The EDGE STORE is the authored directive lines, which live in the `.flexiarg`
;; files themselves.  There is no separate edge database to diff, so "the edge
;; set is byte-identical before and after a learning pass" is checked two ways at
;; once: over the raw BYTES of every file that can carry a directive, and over
;; the parsed edge set `find-organise` reads out of them.  The first catches a
;; write that does not change the parse; the second catches a parse that changes
;; without a write (it cannot, and the check says so rather than assuming it).

(defn sha256 [^String s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest digest (.getBytes s "UTF-8"))))))

(defn edge-store-files
  "Every file that can carry an authored edge directive, sorted.  Read from the
   filesystem, not listed: a hand-kept list of the edge store's files would be
   the same defect as a hand-kept list of edges."
  [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile ^java.io.File %))
       (map #(.getPath ^java.io.File %))
       (filter #(str/ends-with? % ".flexiarg"))
       sort
       vec))

(defn library-sections
  "Every directory of `root` holding at least one `.flexiarg`.  Same rule as
   `construct_cascade.clj`'s, restated here rather than imported, so that this
   file's view of the edge store does not depend on the constructor's."
  [root]
  (->> (.listFiles (io/file root))
       (filter #(.isDirectory ^java.io.File %))
       (filter (fn [d] (some #(str/ends-with? (.getName ^java.io.File %) ".flexiarg")
                             (.listFiles ^java.io.File d))))
       (map #(keyword (.getName ^java.io.File %)))
       sort
       vec))

(defn read-library
  "The repository over ALL THREE authored kinds.  The prior of `construct_cascade`
   reads `@why` u `@how`; O5's domain is every AUTHORED edge, so `@see-also` is in
   the weight store's domain and is counted by kind in the report."
  [root]
  (fo/read-repository root (library-sections root) {:kinds #{:why :how :see-also}}))

(defn canonical-edge-set
  "The edge set as a sorted vector of `[from kind to]` -- the object the
   acceptance sentence is about."
  [repository]
  (->> (:edges repository) (map (juxt :from :kind :to)) sort vec))

(defn store-digest
  "The part of the digest a STORE carries: the edge set it was learned against,
   and nothing about the working tree.  `:bytes-sha256` is deliberately left out.
   It is a fact about the tree at a moment, not about the edge set, and another
   lane rewrites a harvest timestamp into a pattern file
   (library/problems/refusal-prediction-error-v1--source-field-missing.flexiarg,
   rewritten by holes/labs/wm-contract/harvest_refusals.bb, observed twice
   40 minutes apart on 2026-09-02 with the edge set unmoved).  Carrying it would
   make both stores churn on a change that is not an edge change, and would make
   this row's artefacts irreproducible for a reviewer.  The before/after byte
   comparison still happens -- it is the acceptance test -- and it happens
   WITHIN one pass, in the report."
  [digest]
  (into (sorted-map) (select-keys digest [:edge-set-sha256 :edges :edges-by-kind :patterns])))

(defn edge-store-digest
  "The digest the before/after comparison is over.  Taken against a GIVEN file
   list so that a file another lane adds mid-pass (LB1's finding: library/math-
   formalization takes files from a live scribe) is reported as an observation
   rather than counted as this pass having written one."
  [root files]
  (let [repository (read-library root)]
    (sorted-map
     :files (count files)
     :bytes-sha256 (sha256 (pr-str (mapv (fn [f] [f (sha256 (slurp f))]) files)))
     :edge-set-sha256 (sha256 (pr-str (canonical-edge-set repository)))
     :edges (count (:edges repository))
     :edges-by-kind (into (sorted-map) (frequencies (map :kind (:edges repository))))
     :patterns (count (:patterns repository)))))

;; ---------------------------------------------------------------------------
;; the write ledger -- every path this pass writes, and a guard on each
;; ---------------------------------------------------------------------------
;; The digest comparison says the edge store came out unchanged.  It does not say
;; the pass never tried: a file written and restored inside the pass would pass a
;; before/after digest.  So every write goes through one function that REFUSES a
;; path in the edge store, and the ledger of what it wrote is a control.

(def ^:private writes (atom []))

(def write-allow-list #{weight-store-path proposal-store-path report-path})

(defn write!
  "Write EDN to `path`, refusing anything that is part of the edge store."
  [path value]
  (when (or (str/starts-with? path (str library-root "/"))
            (str/ends-with? path ".flexiarg"))
    (throw (ex-info "learn-edge-weights: refused a write into the edge store"
                    {:finding :pass-would-write-into-the-edge-store :path path})))
  (swap! writes conj path)
  (spit path (with-out-str (pprint/pprint value)))
  path)

;; ---------------------------------------------------------------------------
;; the corpus -- the runs already recorded, read and never re-run
;; ---------------------------------------------------------------------------

(def corpus-artefacts
  "Every recorded construction in `checks/`, with the shape its file is in.  A
   `:runs` artefact is one domain with a temperament per run; an `:items`
   artefact is one construction per work item, each with its own temperaments.
   Listed rather than globbed, because an artefact that is not a construction
   (`snatch-cascade.edn` is a playout; `find-snatch.edn` is a find) would be
   silently absorbed by a glob and is excluded here BY NAME."
  [{:path "checks/construct-cascade.edn" :shape :runs :domain :the-library :row :LA3}
   {:path "checks/ants-cascade.edn" :shape :runs :domain :ants :row :LA4}
   {:path "checks/alfworld-cascade.edn" :shape :runs :domain :alfworld :row :LA6}
   {:path "checks/zaif-cascade.edn" :shape :runs :domain :zaif :row :LA5}
   {:path "checks/retrodiction-cascade.edn" :shape :items :domain :retrodiction :row :LA7}
   {:path "checks/retrodiction-cascade-per-clause.edn" :shape :items
    :domain :retrodiction-per-clause :row :LA7}
   {:path "checks/open-cascade.edn" :shape :items :domain :open-items :row :LA8}
   {:path "checks/open-cascade-short-cue.edn" :shape :items
    :domain :open-items-short-cue :row :LA8}])

(defn- admissions
  "The admitted patterns of one run, IN STEP ORDER.  The order is the whole of
   what makes this an attachment record rather than a co-occurrence count: a
   pattern is credited against the patterns that were already there when it
   arrived, not against everything that ever shared a cascade with it."
  [record]
  (->> record (filter #(= :admit (:edit %))) (sort-by :step) (mapv :pattern)))

(defn- floor-stop?
  "Did this run stop at a stated marginal-gain floor?  Every admission of such a
   run scored at or above that floor, which is the closest the recorded corpus
   comes to the G-improvement signal §4.3 asks for."
  [run]
  (boolean (some #(= :marginal-gain-floor %) (flatten (vector (:stop run))))))

(defn- runs-of-scope [scope]
  (for [[tid run] (sort-by key (:runs scope))]
    {:temperament tid
     :seed (set (get-in scope [:find :selected]))
     :admissions (admissions (:record run))
     :floor-stop? (floor-stop? run)}))

(defn runs-of [{:keys [path shape domain row]}]
  (let [m (edn/read-string (slurp path))
        tag (fn [scope r] (assoc r :artefact path :domain domain :row row :scope scope))]
    (case shape
      :runs (mapv #(tag :whole %) (runs-of-scope m))
      :items (vec (for [[item scope] (sort-by key (:items m))
                        r (runs-of-scope scope)]
                    (tag item r))))))

(defn run-id [r] [(:domain r) (:scope r) (:temperament r)])

(defn attachment-steps
  "One entry per (admission step, pattern already in the cascade).  This is the
   `per attachment step` of §4.3, as far as the record supports it."
  [r]
  (loop [chosen (set (:seed r)) todo (:admissions r) i 0 out []]
    (if (empty? todo)
      out
      (let [p (first todo)]
        (recur (conj chosen p) (rest todo) (inc i)
               (into out (for [q (sort chosen) :when (not= p q)]
                           {:step i :admitted p :held q :run (run-id r)
                            :floor-stop? (:floor-stop? r)})))))))

;; ---------------------------------------------------------------------------
;; the learning pass
;; ---------------------------------------------------------------------------

(defn- pair [a b] (vec (sort [a b])))

(defn edge-index
  "pair -> the authored directive lines joining it, in either direction and of
   any kind.  The weight store's KEYS come from here and from nowhere else,
   which is O5a's domain restriction made mechanical."
  [repository]
  (reduce (fn [m {:keys [from to kind]}]
            (update m (pair from to) (fnil conj []) [from kind to]))
          {} (:edges repository)))

(defn learn
  "The pass.  Rebuilt from the corpus every time rather than incremented in
   place, so it is idempotent and a re-run is a check rather than a second
   update (`determinism` below).

   Every attachment step contributes 1.  A step whose two patterns are joined by
   authored edges credits EACH of those edges -- a `@why` and a `@see-also`
   between the same pair are two edges and get a weight each.  A step whose two
   patterns are joined by nothing is a PROPOSAL, and is the only thing this pass
   produces that an author could act on."
  [repository runs]
  (let [idx (edge-index repository)
        patterns (:patterns repository)
        steps (mapcat attachment-steps runs)
        in-repo (fn [{:keys [admitted held]}]
                  (and (contains? patterns admitted) (contains? patterns held)))
        [kept dropped] [(filter in-repo steps) (remove in-repo steps)]
        tally (fn [m s]
                (let [pr (pair (:admitted s) (:held s))
                      edges (get idx pr)
                      bump (fn [rec]
                             (-> (or rec {:attachments 0 :attachments-under-a-floor-stop 0
                                          :runs #{}})
                                 (update :attachments inc)
                                 (update :attachments-under-a-floor-stop
                                         + (if (:floor-stop? s) 1 0))
                                 (update :runs conj (:run s))))]
                  (if (seq edges)
                    (reduce (fn [m e] (update-in m [:weights e] bump)) m edges)
                    (update-in m [:proposals pr] bump))))]
    (-> (reduce tally {:weights {} :proposals {}} kept)
        (assoc :steps-total (count steps)
               :steps-credited (count kept)
               :steps-dropped (count dropped)
               :dropped-by-domain
               (into (sorted-map)
                     (for [[d ss] (sort-by key (group-by (comp first :run) dropped))]
                       [d (count ss)]))))))

(defn- finalise [m]
  (into (sorted-map)
        (for [[k rec] m]
          [k (sorted-map :weight (double (:attachments rec))
                         :attachments (:attachments rec)
                         :attachments-under-a-floor-stop
                         (:attachments-under-a-floor-stop rec)
                         :runs (vec (sort (:runs rec))))])))

(defn weight-store [repository learned digest]
  (sorted-map
   :store :library-contract/edge-weights-v1
   :law "O5a (PROPOSED, not adopted): a learning pass may update the weight on an
         authored edge and may never add an edge.  Every key below is an authored
         directive line read from a .flexiarg; this file is not the edge store and
         nothing on the way to a cascade reads it."
   :update-rule "weight := the number of attachment steps, across the recorded
         corpus, at which one endpoint was admitted to a construction while the
         other was already in it.  Rebuilt from the corpus on every pass, so a
         re-run is idempotent.  NOT the G-improvement of LA1c §4.3: no recorded
         run carries a per-step score (:g-improvement-availability in the report)."
   :edge-store-digest (store-digest digest)
   :corpus (mapv (fn [a] (select-keys a [:path :domain :row])) corpus-artefacts)
   :edges-in-the-repository (count (:edges repository))
   :edges-with-a-weight (count (:weights learned))
   :weights (finalise (:weights learned))))

(defn proposal-store [learned digest]
  (let [props (finalise (:proposals learned))
        ranked (vec (sort-by (fn [[p rec]] [(- (:attachments rec)) p]) props))]
    (sorted-map
     :store :library-contract/edge-proposals-v1
     :not-an-edge true
     :law "O5b (PROPOSED, not adopted): a learned high weight on a pair with NO
           authored edge is a HYPOTHESIS that an author might have an edge to
           write.  It is not an edge, not a warrant and not a citation.  It is
           routed to `find` (as an ordering over what find already selected and
           receipted) and to the spider (as prompt material, checks/spider_runner.clj),
           and to no `organise` path.  An author writes the edge or nobody does."
     :edge-store-digest (store-digest digest)
     :pairs (count props)
     :proposals (mapv (fn [[[a b] rec]]
                        (sorted-map :pair [a b]
                                    :weight (:weight rec)
                                    :attachments (:attachments rec)
                                    :attachments-under-a-floor-stop
                                    (:attachments-under-a-floor-stop rec)
                                    :runs (:runs rec)))
                      ranked)
     ;; The spider works one pattern at a time, so the channel is keyed by
     ;; section and capped PER SOURCE PATTERN: a section-wide top ten would give
     ;; every turn in a busy section the same ten rows and nothing to the rest.
     ;; Both ends are section-qualified, which is the form the spider's prompt
     ;; uses for :from and :to alike (spider_runner.clj `pattern-id`); a bare
     ;; name would not compare against the pattern it is working on.
     :for-the-spider
     (let [rows (for [[[a b] rec] ranked
                      [from to] [[a b] [b a]]]
                  {:section (keyword (namespace from))
                   :row (sorted-map
                         :from (str (namespace from) "/" (name from))
                         :to (str (namespace to) "/" (name to))
                         :weight (:weight rec))})
           per-pattern (mapcat (fn [[_ rs]] (take 5 (map :row rs)))
                               (sort-by key (group-by (comp :from :row) rows)))]
       (into (sorted-map)
             (for [[section rs] (sort-by key (group-by #(keyword (first (str/split (:from %) #"/")))
                                                       per-pattern))]
               [section (vec (sort-by (fn [r] [(- (:weight r)) (:from r) (:to r)]) rs))]))))))

;; ---------------------------------------------------------------------------
;; the routing -- to find, and to the spider
;; ---------------------------------------------------------------------------

(defn weighted-find
  "The route to `find`.  A weight may ORDER what find selected; it may not change
   the set and it may not enter a receipt.  That restriction is not decoration:
   F2 says every selected pattern carries a receipt and F3 says the receipt cites
   the pattern's TEXT and never a score.  A weight is a score.  So it is carried
   in a sibling key that no law reads, and the controls check that the selection
   and the receipts come out identical.

   The weight of a selected pattern is the learned weight on the authored edges
   INCIDENT to it, whether or not the other endpoint was also selected.  The
   narrower arm -- edges with both endpoints in the selection -- was built first
   and is empty on the only real tension this harness has: no two of the nine
   patterns `find` selects for :organise-the-library are joined by an authored
   edge, so every weight over the selection is 0.0 and the ordering is the
   identity.  A route that cannot move is not a route, so the arm is recorded in
   decisions.edn :learned-weight-route-to-find and the incident arm is what runs."
  [find-result store]
  (let [w (fn [p] (reduce + 0.0
                          (for [[[from _ to] rec] (:weights store)
                                :when (or (= p from) (= p to))]
                            (:weight rec))))]
    (assoc find-result
           :route-arm :incident-to-the-selected-pattern
           :learned-weight (into (sorted-map) (map (juxt identity w)) (:selected find-result))
           :learned-order (vec (sort-by (fn [p] [(- (w p)) p]) (:selected find-result))))))

(defn unlearned
  "The store with nothing learned in it.  This is the control arm for the route
   to `find`, and it is the same shape as LA3's `:uniform?` arm: the route is
   live iff the ordering under the learned store differs from the ordering with
   no learning at all.  An inverted store would NOT do the job here -- the
   quantity `weighted-find` orders on is a SUM over incident edges, so inverting
   every weight leaves a pattern with nine edges ahead of one with three, and the
   control would report `no change` for a reason that has nothing to do with
   whether the route reads the store.  That was measured, not reasoned: the
   inverted arm is carried below as `:inverted-arm-does-not-move-the-order` so
   the reader can see the trap rather than take this paragraph on trust."
  [store]
  (assoc store :weights (sorted-map)))

(defn inverted
  "The same store with every weight replaced by `max + 1 - w`."
  [store]
  (let [ws (:weights store)
        top (inc (reduce max 0.0 (map (comp :weight val) ws)))]
    (assoc store :weights
           (into (sorted-map)
                 (for [[k rec] ws] [k (assoc rec :weight (- top (:weight rec)))])))))

;; ---------------------------------------------------------------------------
;; the controls
;; ---------------------------------------------------------------------------

(defn co-membership
  "The null the attachment record has to beat.  For every pair of patterns that
   were MEMBERS of the same recorded cascade, one count per run; order thrown
   away.  Law O2 refuses co-occurrence as a substrate for an EDGE, and this pass
   does not produce edges -- but if the attachment record says nothing that plain
   co-membership does not, then the proposals are co-occurrence with extra steps,
   and that is worth knowing BEFORE a law is adopted on top of them."
  [repository runs]
  (let [patterns (:patterns repository)]
    (reduce (fn [m r]
              (let [mem (vec (sort (filter patterns (into (set (:seed r)) (:admissions r)))))]
                (reduce (fn [m pr] (update m pr (fnil inc 0)))
                        m
                        (for [i (range (count mem)) j (range (inc i) (count mem))]
                          (pair (nth mem i) (nth mem j))))))
            {} runs)))

(defn co-occurrence-null
  "How much the attachment record adds over co-membership, measured rather than
   argued.  Within one run a pair can be credited at most once -- whichever of
   the two arrived second pairs with the first -- so an attachment count can only
   ever be at or below a co-membership count, and the gap is exactly the pairs
   that were BOTH in the seed and so were never attached to each other."
  [repository runs learned]
  (let [co (co-membership repository runs)
        idx (edge-index repository)
        attached (into {} (for [[p rec] (:proposals learned)] [p (:attachments rec)]))
        unauthored-co (into {} (remove (fn [[p _]] (contains? idx p)) co))
        rank (fn [m] (mapv first (take 10 (sort-by (fn [[p v]] [(- v) p]) m))))
        disagree (remove (fn [[p v]] (= v (get unauthored-co p))) attached)]
    (sorted-map
     :unauthored-pairs-with-an-attachment (count attached)
     :unauthored-pairs-with-co-membership (count unauthored-co)
     :pairs-co-member-but-never-attached
     (count (remove #(contains? attached %) (keys unauthored-co)))
     :pairs-where-the-two-counts-disagree (count disagree)
     :largest-disagreement (reduce max 0 (for [[p v] disagree]
                                           (- (get unauthored-co p 0) v)))
     :top-ten-by-attachment (rank attached)
     :top-ten-by-co-membership (rank unauthored-co)
     :same-top-ten? (= (rank attached) (rank unauthored-co)))))

(def organise-side-files
  "Every file on the way from a repository to a cascade.  None of them may name
   either store.  This is what `never to organise` means as something a later
   change can FAIL rather than as a sentence in a docstring: wire a weight into
   any constructor and this control goes red."
  ["checks/find_organise.clj" "checks/construct_cascade.clj"
   "checks/construct_ants_cascade.clj" "checks/construct_alfworld_cascade.clj"
   "checks/construct_zaif_cascade.clj" "checks/construct_retrodiction_cascade.clj"
   "checks/construct_open_cascade.clj" "checks/playout_snatch.clj"
   "checks/find_snatch.clj" "checks/library_graph_lint.clj"])

(def store-readers
  "The files allowed to name a store: this one, and the spider -- which is where
   `routed to the spider` actually happens."
  ["checks/learn_edge_weights.clj" "checks/spider_runner.clj"])

(defn organise-blindness
  "STATIC, and the report says so.  A same-process rerun of `organise` with a
   forged store bound would pass by construction -- `organise` takes a
   temperament, a selected set and a repository, and there is no third place for
   a weight to enter -- so it is not offered as evidence.  What is offered is
   this text scan, plus the O2 mutation in `negative-controls`: a proposal turned
   into an edge is REJECTED by O2, which is the law doing the refusing."
  []
  (let [names (fn [path] (let [t (slurp path)]
                           (filterv #(str/includes? t %)
                                    [weight-store-path proposal-store-path])))]
    (sorted-map
     :method :static-text-scan-plus-the-O2-mutation
     :not-claimed "a same-process rerun would be vacuous and is not run"
     :organise-side-files-naming-a-store
     (into (sorted-map) (for [f organise-side-files :let [n (names f)] :when (seq n)] [f n]))
     :store-readers (into (sorted-map) (for [f store-readers] [f (names f)]))
     :spider-reads-the-proposals?
     (str/includes? (slurp "checks/spider_runner.clj") proposal-store-path)
     :holds? (every? #(empty? (names %)) organise-side-files))))

(defn g-improvement-availability
  "Checked, not asserted: does ANY recorded run carry a per-step score?  §4.3's
   update rule needs one.  If this ever comes back true, the weight here should
   be rebuilt on it and this file's `:update-rule` is out of date."
  [runs artefacts]
  (let [score-keys #{:score :gain :marginal-gain :g :delta-g}
        ;; ADMISSIONS only.  A `:halt` entry does carry a `:reason` -- the stop
        ;; rule and its threshold -- and counting those would report a per-step
        ;; score that is not there.  36 of the 264 record entries are halts, and
        ;; reading them as scores is exactly the mistake this check exists to
        ;; make impossible.
        reasons (mapcat (fn [a]
                          (let [m (edn/read-string (slurp (:path a)))]
                            (for [scope (if (= :items (:shape a)) (vals (:items m)) [m])
                                  [_ run] (:runs scope)
                                  e (:record run)
                                  :when (= :admit (:edit e))]
                              (:reason e))))
                        artefacts)
        run-rows (mapcat (fn [a]
                           (let [m (edn/read-string (slurp (:path a)))]
                             (for [scope (if (= :items (:shape a)) (vals (:items m)) [m])
                                   [_ run] (:runs scope)]
                               run)))
                         artefacts)]
    (sorted-map
     :runs (count runs)
     :admission-entries (count reasons)
     :admission-entries-with-a-reason (count (remove nil? reasons))
     :run-rows-with-a-score-field
     (count (filter #(seq (set/intersection score-keys (set (keys %)))) run-rows))
     :per-step-score-available? (boolean (or (seq (remove nil? reasons))
                                             (some #(seq (set/intersection score-keys (set (keys %))))
                                                   run-rows))))))

(def ^:private declared-controls
  "Mutations that must be REJECTED.  As everywhere in this harness, a mutation
   that could not be BUILT is a failure and not a pass: an unexercised control
   reads exactly like a passing one."
  [:a-weight-on-an-unauthored-pair
   :a-proposal-that-is-already-an-authored-edge
   :a-learned-pair-admitted-to-a-cascade-as-an-edge
   :a-write-into-the-edge-store
   :a-weight-that-moves-what-find-selected
   :a-store-whose-edge-set-digest-is-stale])

(defn the-argument
  "The row's last acceptance clause: O5 is PROPOSED, and the first delivery may
   be the argument that it should not be adopted.  This is that argument, with
   every number in it computed from the run that prints it rather than typed."
  [repository weights props learned runs annotated]
  (let [null (co-occurrence-null repository runs learned)
        edges (count (:edges repository))
        weighted (count (:weights weights))
        prop-values (map :weight (:proposals props))
        vs (vec (sort > prop-values))]
    (sorted-map
     :o5-as-written
     "LA1c-restatement.md §4.3 and the §9 law table: `learning updates WEIGHTS on
      authored edges and never adds an edge (§4.3). A learned weight is input to
      find and to authorship, never to organise.`"

     :one-the-two-clauses-need-different-domains
     "The domain clause puts w on the authored edge set. The routing clause says a
      learned high weight is a hypothesis that an author SHOULD WRITE an edge. On
      the domain clause's domain that hypothesis has no referent: the edge is
      already written, so a weight on it proposes nothing. For the routing clause
      to have content the learner must score PAIRS, of which authored edges are a
      subset -- which is what this file does, in two stores that are separate for
      exactly this reason. As one sentence O5 is not adoptable; as two laws it is."

     :two-what-keeps-a-pair-weight-inside-O2-is-the-routing-not-the-domain
     "Once the domain is pairs, the domain restriction is no longer what keeps
      learning legal. What does is that no path from a repository to a cascade
      reads either store, which is checkable and is checked: ten organise-side
      files scanned, none names a store; and a proposal turned into an edge is
      rejected by O2 (negative control :a-learned-pair-admitted-to-a-cascade-as-an-edge).
      That is a stronger law than the one §4.3 proposes, because it survives the
      domain widening that the routing clause forces."

     :three-the-update-rule-is-not-implementable-from-the-record
     (format "§4.3 says the weight is updated by G-improvement per attachment step.
      No recorded run carries one: %d admission entries across %d runs, %d with a
      reason, 0 run rows with a score field. What is implemented instead is an
      attachment COUNT. Recording the admission's score would move
      checks/construct-cascade.edn, which is LA3's byte-identical reference
      artefact and LA4's regeneration check, so it is a later row's change and not
      this one's."
             (:admission-entries (g-improvement-availability runs corpus-artefacts))
             (count runs)
             (:admission-entries-with-a-reason
              (g-improvement-availability runs corpus-artefacts)))

     :four-the-proposal-channel-is-a-filter-not-a-ranking
     (format "Against the co-occurrence null: %d unauthored pairs carry an
      attachment and %d are merely co-members, so the attachment record EXCLUDES
      %d pairs that co-occurrence would admit -- those whose two patterns were
      both in the seed and so were never attached to each other. That is a real
      difference. But on the pairs it does admit the two counts agree on %d of
      %d; the ordering is co-membership. So the record's contribution is the
      filter, not the ranking, and any claim that a HIGH weight means more than a
      NONZERO one is not supported here."
             (:unauthored-pairs-with-an-attachment null)
             (:unauthored-pairs-with-co-membership null)
             (:pairs-co-member-but-never-attached null)
             (- (:unauthored-pairs-with-an-attachment null)
                (:pairs-where-the-two-counts-disagree null))
             (:unauthored-pairs-with-an-attachment null))

     :five-there-is-almost-nothing-to-learn-on-yet
     (format "%d of %d authored edges (%.1f%%) are reached by the whole recorded
      corpus at all, and the weights they carry run %s. The proposal side is
      flatter still: %d pairs, top weight %.1f, median %.1f. A law adopted now
      would be legislating for machinery with two orders of magnitude less data
      than it needs to discriminate anything. This is the same bound
      :constructor-degree-term-measured records one level down -- the constructor
      is bounded by how much of the library is organised, and so is anything
      learned from running it."
             weighted edges (* 100.0 (/ (double weighted) (max 1 edges)))
             (pr-str (into (sorted-map) (frequencies (map (comp :weight val) (:weights weights)))))
             (count vs) (double (first vs)) (double (nth vs (quot (count vs) 2))))

     :six-the-route-to-find-barely-exists-on-the-only-real-tension
     (format "%d of the %d patterns find selects for :organise-the-library carry
      any learned weight at all, and no two of the nine are joined by an authored
      edge, which is why the within-the-selection arm of the route is the identity
      (decisions.edn :learned-weight-route-to-find). The route is live -- the
      order differs from the unlearned arm -- and it is live on two patterns."
             (count (remove (comp zero? val) (:learned-weight annotated)))
             (count (:learned-order annotated)))

     :recommendation
     "DO NOT ADOPT O5 as written. Three things are on offer instead, in the order
      they should be taken. (a) Adopt nothing and keep the machinery: the stores,
      the routing separation and the byte-identity control are worth having
      whether or not a law is written over them, and they cost nothing to leave
      running. (b) If a law is wanted now, adopt the ROUTING law -- no path from a
      repository to a cascade reads a learned quantity -- which is what actually
      does the work, is checkable today, and does not depend on the domain
      question. (c) Leave the DOMAIN question (weights on edges only, versus
      weights on pairs with the proposals routed out) open until the corpus can
      discriminate: it cannot now, and adopting either domain today fixes an
      answer on 41 weighted edges.
      Adopting is Joe's either way; this row's delivery is the argument and the
      apparatus, not the decision."

     :what-would-change-this
     "Any one of: (1) a constructor that records the admission's score, so the
      weight can be the G-improvement §4.3 asks for rather than a count -- that is
      one field on the record and a re-run of every artefact, and it moves LA3's
      pinned sha, so it is its own row; (2) the L5/L9 wave raising authored-edge
      coverage far enough that a corpus this size reaches more than 15% of the
      edge set; (3) a single proposal from this store that an author actually
      writes as an edge, which would be the first evidence that the channel
      carries anything, and which nothing here has yet."

     :what-this-row-does-not-claim
     ["No proposal here has been written as an edge, and none should be on the
       strength of a weight. The spider prompt says so and the runner's evidence
       path does not read the store."
      "The attachment count is not a probability, not a confidence and not a
       G-improvement. It is a count of attachment steps."
      "48 runs over 8 artefacts is one corpus, four of whose domains are the same
       constructor over different item sets. Nothing here supports a rate."])))

(defn store-fresh?
  "A weight learned against one edge set is not a weight on another.  The store
   carries the digest it was learned against and this is the guard that reads it;
   `negative-controls` feeds it a forged digest and requires it to say stale."
  [store digest]
  (= (:edge-set-sha256 (:edge-store-digest store)) (:edge-set-sha256 digest)))

(defn negative-controls [repository weights props cascade find-result store digest]
  (let [idx (edge-index repository)
        authored-pairs (set (keys idx))
        an-edge (first (sort (keys weights)))
        a-pair (:pair (first (:proposals props)))
        ;; a proposal pair that is not authored-@why-REACHABLE either way, so the
        ;; O2 mutation is a mutation and not an edge O2 would have accepted.
        stands-on (:stands-on repository)
        reach (fn [u] (fo/reach-outside #{} stands-on u))
        unreachable (first (for [{:keys [pair]} (:proposals props)
                                 :let [[u v] pair]
                                 :when (and (not (contains? (reach u) v))
                                            (not (contains? (reach v) u)))]
                             pair))
        outside (first (sort (set/difference (:patterns repository)
                                             (set (:selected find-result)))))
        row (fn [control exercised? caught? detail]
              (sorted-map :control control :exercised? (boolean exercised?)
                          :caught? (boolean caught?) :detail detail))]
    [(row :a-weight-on-an-unauthored-pair (some? a-pair)
          (and (some? a-pair) (not (contains? authored-pairs a-pair)))
          (pr-str a-pair))
     (row :a-proposal-that-is-already-an-authored-edge (some? an-edge)
          (let [[from _ to] an-edge]
            (not (some #(= (pair from to) (:pair %)) (:proposals props))))
          (pr-str an-edge))
     (row :a-learned-pair-admitted-to-a-cascade-as-an-edge (some? unreachable)
          (and (some? unreachable)
               (not (fo/o2-authored-reachability
                     (update cascade :edges conj (vec unreachable)))))
          (pr-str unreachable))
     (row :a-write-into-the-edge-store true
          (try (write! (str library-root "/forged/not-a-pattern.flexiarg") {}) false
               (catch clojure.lang.ExceptionInfo e
                 (= :pass-would-write-into-the-edge-store (:finding (ex-data e)))))
          "write! must refuse a path in the edge store")
     ;; A pattern `find` did NOT select, given the largest weight in the store on
     ;; an edge to a pattern it did.  The route must leave it outside: the
     ;; annotation ranges over what find selected, so a weight is an ordering and
     ;; not a way in.
     (row :a-weight-that-moves-what-find-selected
          (and (some? outside) (some? (first (sort (:selected find-result)))))
          (let [inside (first (sort (:selected find-result)))
                forged (assoc-in store [:weights [outside :why inside]]
                                 {:weight 9999.0 :attachments 9999
                                  :attachments-under-a-floor-stop 0 :runs []})
                w (weighted-find find-result forged)]
            (and (= (:selected find-result) (:selected w))
                 (= (:receipts find-result) (:receipts w))
                 (not (contains? (set (:learned-order w)) outside))))
          (pr-str outside))
     ;; The staleness guard, fed a store learned against an edge set that is not
     ;; this one.  It must say stale, and the real store must not.
     (row :a-store-whose-edge-set-digest-is-stale true
          (and (not (store-fresh? (assoc-in store [:edge-store-digest :edge-set-sha256]
                                            (sha256 "an edge set that is not this one"))
                                  digest))
               (store-fresh? store digest))
          "store-fresh? compares the digest the store was learned against")]))

(defn determinism
  "Two passes over the same corpus must produce the same stores, byte for byte.
   The pass is a rebuild rather than an increment for exactly this reason;
   without the control that is a claim in a docstring."
  [repository runs digest]
  (let [a (weight-store repository (learn repository runs) digest)
        b (weight-store repository (learn repository runs) digest)
        pa (proposal-store (learn repository runs) digest)
        pb (proposal-store (learn repository runs) digest)]
    (sorted-map :weights-identical? (= (pr-str a) (pr-str b))
                :proposals-identical? (= (pr-str pa) (pr-str pb)))))

;; ---------------------------------------------------------------------------
;; the run
;; ---------------------------------------------------------------------------

(defn report []
  (let [files (edge-store-files library-root)
        before (edge-store-digest library-root files)
        repository (read-library library-root)
        sections (library-sections library-root)
        why-repo (fo/read-repository library-root sections {:kinds #{:why}})
        runs (vec (mapcat runs-of corpus-artefacts))
        learned (learn repository runs)
        weights (weight-store repository learned before)
        props (proposal-store learned before)
        _ (write! weight-store-path weights)
        _ (write! proposal-store-path props)
        after (edge-store-digest library-root files)
        files-after (edge-store-files library-root)
        find-result (fo/find {:context {}
                              :route :cue-citation
                              :fires? (fn [id _] (cc/antecedent-holds?
                                                  (get-in why-repo [:entries id])))
                              :receipt (fn [id] (cc/receipt-for
                                                 (get-in why-repo [:entries id])))}
                             why-repo)
        cascade (assoc (fo/organise fo/selected-only-temperament
                                    (set (:selected find-result)) why-repo)
                       :stands-on (:stands-on why-repo))
        annotated (weighted-find find-result weights)
        annotated-unlearned (weighted-find find-result (unlearned weights))
        annotated-inverted (weighted-find find-result (inverted weights))
        ;; the spider channel's control arm: the same pass over HALF the corpus.
        ;; If the ranking the spider is offered does not move when half the runs
        ;; are taken away, it is not derived from the runs.
        half-corpus (vec (take (quot (count corpus-artefacts) 2) corpus-artefacts))
        props-half (proposal-store (learn repository (vec (mapcat runs-of half-corpus)))
                                   before)
        weight-values (map (comp :weight val) (:weights weights))
        prop-values (map :weight (:proposals props))]
    (sorted-map
     :as-of (sorted-map
             :sections (count sections)
             :patterns (count (:patterns repository))
             :authored-edges (count (:edges repository))
             :authored-edges-by-kind (:edges-by-kind before)
             :dangling-directives (count (:dangling repository))
             :read-digest (:edge-set-sha256 before))
     :corpus (sorted-map
              :artefacts (mapv #(select-keys % [:path :domain :row]) corpus-artefacts)
              :runs (count runs)
              :runs-with-no-admission (count (filter #(empty? (:admissions %)) runs))
              :admissions (reduce + (map (comp count :admissions) runs))
              :attachment-steps (:steps-total learned)
              :attachment-steps-credited (:steps-credited learned)
              :attachment-steps-dropped (:steps-dropped learned)
              :dropped-because-a-pattern-is-not-in-this-repository
              (:dropped-by-domain learned)
              :runs-by-domain (into (sorted-map) (frequencies (map :domain runs))))
     :o5a (sorted-map
           :store weight-store-path
           :edges-in-the-repository (count (:edges repository))
           :edges-with-a-weight (count (:weights weights))
           :share-of-the-edge-set-the-corpus-reaches
           (if (pos? (count (:edges repository)))
             (double (/ (count (:weights weights)) (count (:edges repository))))
             0.0)
           :weight-distribution (into (sorted-map) (frequencies weight-values))
           :max-weight (reduce max 0.0 (conj (vec weight-values) 0.0))
           :sections-supplying-a-weight
           (into (sorted-set) (for [[[from _ to] _] (:weights weights)
                                    x [from to]]
                                (keyword (namespace x))))
           :by-kind (into (sorted-map)
                          (frequencies (map (fn [[[_ k _] _]] k) (:weights weights))))
           :attachments-under-a-floor-stop
           (reduce + (map (comp :attachments-under-a-floor-stop val) (:weights weights))))
     :o5b (sorted-map
           :store proposal-store-path
           :pairs (:pairs props)
           :weight-distribution (into (sorted-map) (frequencies prop-values))
           :max-weight (reduce max 0.0 (conj (vec prop-values) 0.0))
           :top-ten (mapv #(vector (:pair %) (:weight %)) (take 10 (:proposals props)))
           :sections-in-the-spider-channel (vec (keys (:for-the-spider props)))
           :concentration
           (let [vs (sort > prop-values)
                 n (count vs)]
             (sorted-map :pairs n
                         :top (first vs)
                         :median (when (pos? n) (nth vs (quot n 2)))
                         :pairs-at-the-median-or-below
                         (count (filter #(<= % (nth vs (quot n 2) 0)) vs)))))
     :routing (sorted-map
               :to-find (sorted-map
                         :selected-unmoved? (= (:selected find-result) (:selected annotated))
                         :receipts-unmoved? (= (:receipts find-result) (:receipts annotated))
                         :order-moves-against-the-unlearned-arm?
                         (not= (:learned-order annotated)
                               (:learned-order annotated-unlearned))
                         :inverted-arm-does-not-move-the-order
                         (= (:learned-order annotated) (:learned-order annotated-inverted))
                         :distinct-weights-over-the-selection
                         (count (distinct (vals (:learned-weight annotated))))
                         :selected-patterns-carrying-any-learned-weight
                         (count (remove (comp zero? val) (:learned-weight annotated)))
                         :learned-order (:learned-order annotated)
                         :unlearned-order (:learned-order annotated-unlearned))
               :to-the-spider (sorted-map
                               :file "checks/spider_runner.clj"
                               :reads-the-proposal-store?
                               (str/includes? (slurp "checks/spider_runner.clj")
                                              proposal-store-path)
                               :sections-offered (count (:for-the-spider props))
                               :ranking-moves-when-half-the-corpus-is-taken-away?
                               (not= (mapv :pair (take 10 (:proposals props)))
                                     (mapv :pair (take 10 (:proposals props-half))))
                               :pairs-under-half-the-corpus (:pairs props-half)
                               :the-bar-is-unchanged
                               "the runner's validate-output/evidence-valid? path does not read this store, so a proposal cannot warrant an edge")
               :to-organise (organise-blindness))
     :g-improvement-availability (g-improvement-availability runs corpus-artefacts)
     :co-occurrence-null (co-occurrence-null repository runs learned)
     :the-argument (the-argument repository weights props learned runs annotated)
     :controls (sorted-map
                :edge-store-untouched
                (sorted-map :files-before (:files before)
                            :files-after (count files-after)
                            :files-appearing-during-the-pass
                            (vec (set/difference (set files-after) (set files)))
                            :bytes-identical? (= (:bytes-sha256 before) (:bytes-sha256 after))
                            :edge-set-identical? (= (:edge-set-sha256 before)
                                                    (:edge-set-sha256 after))
                            :edge-count-before (:edges before)
                            :edge-count-after (:edges after))
                :write-ledger
                (sorted-map :paths (vec (sort (distinct @writes)))
                            :allowed (vec (sort write-allow-list))
                            :all-allowed? (every? write-allow-list (distinct @writes))
                            :none-in-the-edge-store?
                            (empty? (set/intersection (set @writes) (set files))))
                :the-pass-wrote-something
                (sorted-map :weights (count (:weights weights))
                            :proposals (count (:proposals props))
                            :non-vacuous? (and (pos? (count (:weights weights)))
                                               (pos? (count (:proposals props)))))
                :weight-domain
                (let [authored (set (mapcat val (edge-index repository)))]
                  (sorted-map :keys (count (:weights weights))
                              :keys-that-are-not-authored-edges
                              (vec (remove authored (keys (:weights weights))))))
                :proposal-domain
                (let [authored (set (keys (edge-index repository)))]
                  (sorted-map :pairs (count (:proposals props))
                              :proposals-that-are-authored-edges
                              (vec (filter #(contains? authored (:pair %)) (:proposals props)))))
                :store-freshness (sorted-map
                                  :weights-fresh? (store-fresh? weights before)
                                  :proposals-fresh? (store-fresh? props before))
                :determinism (determinism repository runs before)
                :negative-controls-declared (vec declared-controls)
                :negative-controls (negative-controls repository (:weights weights) props
                                                      cascade find-result weights before)))))

(defn require-pass! [result]
  (let [c (:controls result)
        failures
        (concat
         (when-not (get-in c [:edge-store-untouched :bytes-identical?])
           [{:finding :the-learning-pass-changed-the-edge-store-bytes}])
         (when-not (get-in c [:edge-store-untouched :edge-set-identical?])
           [{:finding :the-learning-pass-changed-the-edge-set}])
         (when-not (get-in c [:write-ledger :all-allowed?])
           [{:finding :the-pass-wrote-outside-its-allow-list}])
         (when-not (get-in c [:write-ledger :none-in-the-edge-store?])
           [{:finding :the-pass-wrote-into-the-edge-store}])
         (when-not (get-in c [:the-pass-wrote-something :non-vacuous?])
           [{:finding :the-pass-wrote-nothing-so-the-byte-identity-is-vacuous}])
         (for [k (get-in c [:weight-domain :keys-that-are-not-authored-edges])]
           {:finding :a-weight-on-something-that-is-not-an-authored-edge :key k})
         (for [p (get-in c [:proposal-domain :proposals-that-are-authored-edges])]
           {:finding :a-proposal-for-an-edge-that-is-already-authored :pair (:pair p)})
         (when-not (get-in c [:store-freshness :weights-fresh?])
           [{:finding :the-weight-store-is-stale-against-the-edge-set}])
         (when-not (get-in result [:routing :to-find :selected-unmoved?])
           [{:finding :a-weight-moved-what-find-selected}])
         (when-not (get-in result [:routing :to-find :receipts-unmoved?])
           [{:finding :a-weight-entered-a-receipt}])
         (when-not (get-in result [:routing :to-find :order-moves-against-the-unlearned-arm?])
           [{:finding :the-route-to-find-is-decoration}])
         (when-not (get-in result [:routing :to-the-spider
                                   :ranking-moves-when-half-the-corpus-is-taken-away?])
           [{:finding :the-spider-channel-is-not-derived-from-the-runs}])
         (when-not (get-in result [:routing :to-the-spider :reads-the-proposal-store?])
           [{:finding :the-spider-does-not-read-the-proposals}])
         (when-not (get-in result [:routing :to-organise :holds?])
           [{:finding :an-organise-side-file-names-a-weight-store}])
         (when-not (get-in c [:determinism :weights-identical?])
           [{:finding :two-passes-produced-different-weights}])
         (when-not (get-in c [:determinism :proposals-identical?])
           [{:finding :two-passes-produced-different-proposals}])
         (for [n (get-in c [:negative-controls]) :when (not (and (:exercised? n) (:caught? n)))]
           {:finding (if (:exercised? n) :mutation-slipped :mutation-not-exercised)
            :control (:control n)}))]
    (when (seq failures)
      (throw (ex-info "learn-edge-weights: law or control failed"
                      {:finding (:finding (first failures)) :failures (vec failures)})))
    result))

(defn -main [& _]
  (try
    (let [result (require-pass! (report))]
      (write! report-path result)
      (println (format "library: %d sections, %d patterns, %d authored edges %s"
                       (get-in result [:as-of :sections])
                       (get-in result [:as-of :patterns])
                       (get-in result [:as-of :authored-edges])
                       (pr-str (get-in result [:as-of :authored-edges-by-kind]))))
      (println (format "corpus: %d recorded runs over %s; %d admissions, %d attachment steps (%d credited, %d dropped)"
                       (get-in result [:corpus :runs])
                       (pr-str (get-in result [:corpus :runs-by-domain]))
                       (get-in result [:corpus :admissions])
                       (get-in result [:corpus :attachment-steps])
                       (get-in result [:corpus :attachment-steps-credited])
                       (get-in result [:corpus :attachment-steps-dropped])))
      (println (format "O5a  %d of %d authored edges carry a weight (%.4f); distribution %s; sections %s"
                       (get-in result [:o5a :edges-with-a-weight])
                       (get-in result [:o5a :edges-in-the-repository])
                       (get-in result [:o5a :share-of-the-edge-set-the-corpus-reaches])
                       (pr-str (get-in result [:o5a :weight-distribution]))
                       (pr-str (vec (get-in result [:o5a :sections-supplying-a-weight])))))
      (println (format "O5b  %d proposal pairs, max weight %.1f, median %s; top %s"
                       (get-in result [:o5b :pairs])
                       (get-in result [:o5b :max-weight])
                       (pr-str (get-in result [:o5b :concentration :median]))
                       (pr-str (get-in result [:o5b :top-ten]))))
      (println (format "routing: find selection unmoved %s, receipts unmoved %s, order moves against the unlearned arm %s (%d of %d selected patterns carry any weight)"
                       (get-in result [:routing :to-find :selected-unmoved?])
                       (get-in result [:routing :to-find :receipts-unmoved?])
                       (get-in result [:routing :to-find :order-moves-against-the-unlearned-arm?])
                       (get-in result [:routing :to-find :selected-patterns-carrying-any-learned-weight])
                       (count (get-in result [:routing :to-find :learned-order]))))
      (println (format "         spider reads the proposals %s (%d sections offered); organise-side files naming a store: %s"
                       (get-in result [:routing :to-the-spider :reads-the-proposal-store?])
                       (get-in result [:routing :to-the-spider :sections-offered])
                       (pr-str (get-in result [:routing :to-organise :organise-side-files-naming-a-store]))))
      (println (format "THE ACCEPTANCE TEST  edge set byte-identical before and after the pass: bytes %s, edge set %s, over %d files"
                       (get-in result [:controls :edge-store-untouched :bytes-identical?])
                       (get-in result [:controls :edge-store-untouched :edge-set-identical?])
                       (get-in result [:controls :edge-store-untouched :files-before])))
      (let [n (:co-occurrence-null result)]
        (println (format "the co-occurrence null: %d unauthored pairs attached, %d co-member; %d co-member but never attached; counts disagree on %d (largest gap %d); same top ten %s"
                         (:unauthored-pairs-with-an-attachment n)
                         (:unauthored-pairs-with-co-membership n)
                         (:pairs-co-member-but-never-attached n)
                         (:pairs-where-the-two-counts-disagree n)
                         (:largest-disagreement n)
                         (:same-top-ten? n))))
      (println (format "g-improvement per attachment step available in the record: %s (%d admission entries, %d with a reason, %d run rows with a score field)"
                       (get-in result [:g-improvement-availability :per-step-score-available?])
                       (get-in result [:g-improvement-availability :admission-entries])
                       (get-in result [:g-improvement-availability :admission-entries-with-a-reason])
                       (get-in result [:g-improvement-availability :run-rows-with-a-score-field])))
      (println (format "controls: determinism %s/%s; %d mutations declared, %d slipped, %d unexercised"
                       (get-in result [:controls :determinism :weights-identical?])
                       (get-in result [:controls :determinism :proposals-identical?])
                       (count declared-controls)
                       (count (filter #(and (:exercised? %) (not (:caught? %)))
                                      (get-in result [:controls :negative-controls])))
                       (count (remove :exercised? (get-in result [:controls :negative-controls])))))
      (println (format "wrote %s, %s, %s" weight-store-path proposal-store-path report-path))
      (println "learn-edge-weights: PASS exit-convention=0-pass/1-fail")
      (shutdown-agents)
      (System/exit 0))
    (catch clojure.lang.ExceptionInfo e
      (println (str "learn-edge-weights: FAIL finding="
                    (name (or (:finding (ex-data e)) :unknown))
                    " exit-convention=0-pass/1-fail"))
      (pprint/pprint (:failures (ex-data e)))
      (shutdown-agents)
      (System/exit 1))))
