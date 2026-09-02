(ns construct-retrodiction-cascade
  "Blind LA7 retrodiction constructor over the whole recorded library.

   The ledger-row cue licence is a removal rule: each cue must occur in the
   statement-only packet entry whose tension uses it, and each statement digest
   is verified before the library is read.  Unlike a source-line licence, this
   form also bounds blindness leakage: a cue copied from an unseen resolution
   cannot survive unless it already occurs in the statement supplied here."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [construct-cascade :as cc]
            [find-organise :as fo]))

(def packet-path "checks/retrodiction-items.edn")
(def report-path "checks/retrodiction-cascade.edn")

(defn sha256 [s]
  (let [d (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest d (.getBytes s "UTF-8"))))))

(def authored
  {:wm/D0
   {:clauses
    [{:id :recorded-but-not-applied
      :text "D2-D7 are recorded but Figure 5A does not change"
      :cues ["recorded" "does not change"]}
     {:id :generator-ignores-decisions
      :text "No generator applies control-map-edges.edn :decisions"
      :cues ["generator" "decisions"]}]
    :zero-mass-pattern :memory/verify-in-the-serving-process
    :zero-mass-reason "The statement concerns applying recorded topology decisions, not verifying a serving process."
    :expected-to-fire #{:futon-theory/single-source-of-truth}
    :must-not-retrodict :ticket/agency-desktop-save
    :must-not-reason "A generator ignoring decision edges is distinct from persisting an agent roster across restart."}

   :wm/C23
   {:clauses
    [{:id :false-claim-withdrawn
      :text "THE ENTRY CARRIED A FALSE CLAIM"
      :cues ["false claim" "withdrawn"]}
     {:id :arms-separated-not-adjudicated
      :text "the arms are SEPARATED, not ADJUDICATED"
      :cues ["arms" "separated" "adjudicated"]}
     {:id :live-path-measurement
      :text "the two arms COINCIDE on the live path"
      :cues ["live path" "measurement"]}]
    :zero-mass-pattern :social/explicit-exit-over-abandonment
    :zero-mass-reason "The statement keeps an interim decision open after falsifying its grounds; it is not about abandoning work."
    :expected-to-fire #{:baldwin/ablation-axes-must-not-disable-the-instrument
                        :math-informal/check-the-extreme-cases}
    :must-not-retrodict :wm/AC6
    :must-not-reason "Separating experimental arms without adjudication is not an exclusion-and-refusal policy for unscored actions."}

   :wm/AC6
   {:clauses
    [{:id :exclude-with-floor
      :text "Unscored moves: exclude-and-continue with a refuse floor"
      :cues ["exclude" "refuse" "floor"]}
     {:id :refuse-empty-or-action
      :text "refuse when exclusion empties the candidate set or when the rollout authorizes an action rather than diagnosing"
      :cues ["candidate set" "action" "diagnosing"]}]
    :zero-mass-pattern :ukrns/publication-cadence
    :zero-mass-reason "The statement is a decision guard for unscored moves, not a publication schedule."
    :expected-to-fire #{:aif/interoceptive-tripwires :social/explicit-exit-over-abandonment}
    :must-not-retrodict :wm/C23
    :must-not-reason "A refusal floor should not reproduce a measurement that deliberately leaves separated arms unadjudicated."}

   :ticket/agency-desktop-save
   {:clauses
    [{:id :durable-roster
      :text "Persist the agent registry roster to disk"
      :cues ["persist" "registry" "disk"]}
     {:id :warm-after-restart
      :text "restart brings the SAME agents back registered"
      :cues ["restart" "agents" "registered"]}
     {:id :no-live-surgery
      :text "No live `dev.clj` reload, no hot-swap surgery"
      :cues ["reload" "live" "surgery"]}]
    :zero-mass-pattern :math-strategy/route-exploration-and-pivot
    :zero-mass-reason "The statement fixes restart durability, not mathematical route selection."
    :expected-to-fire #{:futon-theory/single-source-of-truth
                        :memory/verify-in-the-serving-process}
    :must-not-retrodict :ticket/kangaroo-v1-pouch
    :must-not-reason "Roster persistence across JVM restart is not the persistent per-agent process requested by the pouch item."}

   :ticket/kangaroo-v1-pouch
   {:clauses
    [{:id :one-warm-process
      :text "ONE persistent warm `claude --input-format stream-json` process"
      :cues ["persistent" "warm" "process"]}
     {:id :serialized-turns
      :text "the durable queue already serializes per-agent"
      :cues ["durable" "queue" "serializes"]}
     {:id :shared-routing-surface
      :text "Both the REPL buffer and Agency bells already route through `invoke-agent!`"
      :cues ["buffer" "route" "agent"]}]
    :zero-mass-pattern :math-informal/check-the-extreme-cases
    :zero-mass-reason "The statement concerns warm process lifetime and routing, not checking a mathematical boundary case."
    :expected-to-fire #{:collaboration-coherence/isolation
                        :memory/verify-in-the-serving-process}
    :must-not-retrodict :ticket/agency-desktop-save
    :must-not-reason "A warm per-agent invocation process must not be confused with restoring registry membership after JVM restart."}})

(defn read-packet [] (edn/read-string (slurp packet-path)))

(def ^:dynamic *licence-scope*
  "THE ARM, added in review and run rather than sent as a question (Joe's rule of
   2026-09-01, encoded verbatim at worklist_check.bb:44-45: a choice the theory
   does not settle gets BRANCHES BUILT AND RUN).

   `:statement` is what this file shipped: a cue is licensed by occurring
   anywhere in its item's whole statement.  `:clause` is the arm: a cue must
   occur in the text of THE CLAUSE IT BELONGS TO.

   The arm exists because the shipped scope is strictly WEAKER than the rule it
   is the second form of.  `decisions.edn :cue-licence` licenses a cue by its own
   CLAUSE'S cited span, and nothing about a ledger row forced the widening --
   every clause here carries a `:text` that is a quotation from the statement, so
   the per-clause address was available and was not used.

   This is a REMOVAL rule and needs no cue to be authored, which is why the
   reviewer may run it: striking a cue is not writing one, and the reviewer has
   read the resolutions and so may not write one.  It writes its own artefact and
   leaves the primary one byte-identical."
  :statement)

(defn licence-text
  "The span a cue must occur in, under the scope in force."
  [row clause]
  (case *licence-scope*
    :statement (:statement row)
    :clause (:text clause)))

(defn cue-licence
  "Verify statement digests and license every cue.  A cue is licensed by a
   case-insensitive occurrence in the span `licence-text` names for it; any miss
   is a hard failure, and the licence never adds a cue."
  [packet]
  (let [rows (into {} (map (juxt :item identity)) (:items packet))]
    (vec
     (for [[id spec] authored
           :let [row (get rows id)
                 actual (some-> row :statement sha256)
                 unlicensed (when row
                              (vec (for [clause (:clauses spec)
                                         cue (:cues clause)
                                         :when (not (str/includes?
                                                     (str/lower-case (licence-text row clause))
                                                     (str/lower-case cue)))]
                                     {:clause (:id clause) :cue cue})))]
           :when (or (nil? row)
                     (not= actual (:statement-sha256 row))
                     (seq unlicensed))]
       {:item id :finding (cond (nil? row) :packet-item-missing
                                (not= actual (:statement-sha256 row)) :statement-sha-mismatch
                                :else :cue-not-in-its-licensed-span)
        :unlicensed unlicensed
        :expected-sha (:statement-sha256 row) :actual-sha actual}))))

(defn strike-unlicensed
  "Apply the licence as a REMOVAL: drop every cue that its span does not carry,
   and drop a clause left with no cue.  Under `:statement` this is the identity
   on the authored set, which is why the primary artefact does not move."
  [row spec]
  (->> (:clauses spec)
       (map (fn [c] (update c :cues
                            (fn [cs] (filterv #(str/includes?
                                                (str/lower-case (licence-text row c))
                                                (str/lower-case %))
                                              cs)))))
       (filterv (comp seq :cues))
       vec))

(defn tension-for [row]
  (let [spec (get authored (:item row))]
    {:id (:item row) :statement (:statement row) :source (:statement-address row)
     :clauses (mapv #(assoc % :source (:statement-address row))
                    (strike-unlicensed row spec))}))

(defn cascade-data [final seed repo]
  (let [c (cc/cascade-of final seed repo)]
    (sorted-map :members (vec (sort (:members final)))
                :precedence (into (sorted-map) (:precedence final))
                :authored-order (vec (:authored-order final))
                :ordered (fo/ordered final)
                :provenance (into (sorted-map) (:provenance final))
                :stop (:stop final) :steps (:steps final)
                :nodes (vec (sort (:nodes c))) :edges (vec (sort (:edges c)))
                ;; CORRECTED IN REVIEW.  These read `:addedByOrganise` and
                ;; `:admittedBy`, which `cc/cascade-of` does not return -- it
                ;; returns `:added-by-organise` and `:admitted-by` -- so both
                ;; fields were empty in every cascade of the first artefact
                ;; (sha256 c18a2198) whatever the construction had done.  The
                ;; LAWS were unaffected: `:laws` runs `fo/organise-laws` over
                ;; `(:cascade f)`, the map itself, not over this rendering. What
                ;; was wrong is what a reader of the artefact would have seen,
                ;; and O1's union has `admitted-by` as its third term, so the
                ;; term the report showed as empty is the one :LA3 recorded as
                ;; newly populated.
                :selected (vec (sort (:selected c)))
                :added-by-organise (vec (sort (:added-by-organise c)))
                :admitted-by (vec (sort (:admitted-by c))))))

(defn one-item [row why-repo related]
  (let [id (:item row) spec (get authored id) tension (tension-for row)]
    (binding [cc/tension tension]
      (let [{:keys [matches seed]} (cc/seed-and-candidates why-repo)
            ctx {:seed seed :matches matches :related related}
            found (fo/find {:context ctx :route :cue-citation
                            :fires? (fn [pid _] (cc/antecedent-holds? (get-in why-repo [:entries pid])))
                            :receipt (fn [pid] (cc/receipt-for (get-in why-repo [:entries pid])))} why-repo)
            find-row {:scenario [id] :repository (:patterns why-repo)
                      :selected (set (:selected found)) :receipted (set (keys (:receipts found)))
                      :non-self-certifying (into #{} (comp (filter (comp cc/cites-text? val)) (map key)) (:receipts found))
                      :zero-mass #{(:zero-mass-pattern spec)} :absence (:absence found)}
            runs (into (sorted-map)
                       (for [t cc/temperaments :let [f (cc/run t ctx)]]
                         [(:id t) (assoc f :cascade (cc/cascade-of f seed why-repo))]))
            rows (into (sorted-map)
                       (for [[pid entry] (:entries why-repo)
                             :let [acks (cc/acknowledgements entry)
                                   clauses (fn [b] (into (sorted-set) (map :clause) (filter #(= b (:block %)) acks)))]]
                         [pid {:if-clauses (clauses :if) :however-clauses (clauses :however)
                               :fires? (boolean (cc/antecedent-holds? entry))
                               :expected-to-fire? (contains? (:expected-to-fire spec) pid)}]))
            fired (into (sorted-set) (map key) (filter #(:fires? (val %)) rows))
            state0 (cc/initial-state ctx)
            scored (remove (:members state0) (keys (:candidates state0)))
            degrees (mapv #(cc/degree related (:members state0) %) scored)
            acks (vec (mapcat #(cc/acknowledgements (get-in why-repo [:entries %]))
                              (sort (into (set seed) (mapcat #(cc/admitted-of (val %)) runs)))))]
        {:tension {:id id :source (:statement-address row)
                   :statement-sha256 (:statement-sha256 row)
                   :clause-hits (into (sorted-map)
                                      (for [c (:clauses tension)]
                                        [(:id c) (count (filter (fn [e] (some #(= (:id c) (:clause %))
                                                                                (cc/acknowledgements e)))
                                                               (vals (:entries why-repo))))]))
                   :match-distribution (into (sorted-map) (frequencies (vals matches)))}
         :find {:selected (vec (:selected found)) :seed-size (count seed)
                :candidates (- (count matches) (count seed)) :laws (into (sorted-map) (for [[k f] fo/find-laws] [k (boolean (f find-row))]))
                :zero-mass-pattern (:zero-mass-pattern spec)
                :zero-mass-reason (:zero-mass-reason spec)
                :zero-mass-selected? (contains? (set (:selected found)) (:zero-mass-pattern spec))}
         :antecedent-reach {:rows rows :confusion {:expected (into (sorted-set) (:expected-to-fire spec))
                                                   :fired fired
                                                   :expected-but-did-not-fire (into (sorted-set) (remove fired (:expected-to-fire spec)))
                                                   :fired-but-not-expected (into (sorted-set) (remove (:expected-to-fire spec) fired))}}
         :temperaments (cc/differ-only-in-the-stop cc/budgeted-temperament cc/floored-temperament)
         :runs (into (sorted-map)
                     (for [[tid f] runs]
                       [tid {:stop (:stop f) :member-count (count (:members f))
                             ;; ADDED IN REVIEW (the Claude owner, not the blind
                             ;; constructor).  Member count alone hides the one
                             ;; thing a reader of this artefact most needs: on two
                             ;; items the seed is already at or over `budget` 20, so
                             ;; :widen-to-a-budget halts at step 0 and ADMITS
                             ;; NOTHING.  Its "cascade" is then `find`'s output
                             ;; verbatim and no construction happened.  That is the
                             ;; degeneracy :LA5's pre-run review found and the cue
                             ;; licence removed there; here the cues are all
                             ;; licensed and it is back.  Reported, not repaired --
                             ;; re-authoring cues is the blind constructor's act and
                             ;; the reviewer has read the resolutions.
                             :seed-size (count seed)
                             :admitted (vec (cc/admitted-of f))
                             :admitted-count (count (cc/admitted-of f))
                             :seed-at-or-over-budget? (>= (count seed) cc/budget)
                             :nothing-was-admitted? (zero? (count (cc/admitted-of f)))
                             :laws (into (sorted-map) (for [[k law] (dissoc fo/organise-laws :O4)]
                                                       [k (boolean (law (:cascade f)))]))
                             :record (:record f) :cascade (cascade-data f seed why-repo)}]))
         :degree-term {:candidates-scored (count scored)
                       :candidates-with-degree-above-zero (count (filter pos? degrees))
                       :degree-distribution (into (sorted-map) (frequencies degrees))
                       :distinguishable-from-uniform?
                       (boolean (some (fn [t] (let [a (cc/run t ctx) b (cc/run t (assoc ctx :uniform? true))]
                                                (or (not= (cc/admitted-of a) (cc/admitted-of b))
                                                    (not= (:stop a) (:stop b))))) cc/temperaments))}
         :acks acks :ctx ctx}))))

(defn report []
  (let [packet (read-packet)
        ;; Under the :clause arm the licence STRIKES rather than throws -- that
        ;; is the whole arm.  Under :statement nothing is struck and an
        ;; unlicensed cue is still a hard failure, unchanged.
        struck (when (= :clause *licence-scope*) (cue-licence packet))
        unlicensed (if (= :clause *licence-scope*) [] (cue-licence packet))]
    (when (seq unlicensed) (throw (ex-info "retrodiction packet or cue licence failed" {:failures unlicensed})))
    (let [sections (cc/library-sections cc/library-root)
          why-repo (fo/read-repository cc/library-root sections {:kinds #{:why}})
          wh-repo (fo/read-repository cc/library-root sections {:kinds #{:why :how}})
          all-edge-repo (fo/read-repository cc/library-root sections
                                            {:kinds #{:why :why-posthoc :how :see-also}})
          related (cc/related-adjacency wh-repo)
          items (into (sorted-map) (for [row (:items packet)] [(:item row) (one-item row why-repo related)]))
          acks (vec (mapcat :acks (vals items)))
          first-item (first (vals items))
          cascade (get-in first-item [:runs (:id cc/budgeted-temperament) :cascade])
          ack (first acks)
          controls [{:control :O1-unrecorded-node :exercised? true
                     :caught? (not (fo/o1-nodes-recorded (update cascade :nodes conj :not-a-pattern/forged)))}
                    {:control :citation-cue-not-in-the-cited-span :exercised? (some? ack)
                     :caught? (boolean (and ack (seq (cc/citations-verified [(assoc ack :cue "unlicensed mutation cue")]))))}
                    {:control :citation-span-off-the-end-of-the-file :exercised? (some? ack)
                     :caught? (boolean (and ack (seq (cc/citations-verified [(assoc ack :lines [999999 1000000])]))))}]]
      (sorted-map
       :schema :la7/retrodiction-cascade-v1
       :as-of (sorted-map :library-root cc/library-root :sections sections
                          :pattern-count (count (:patterns why-repo)) :read-digest (cc/read-digest wh-repo)
                          :authored-edge-counts
                          (into (sorted-map)
                                (for [kind [:why :why-posthoc :how :see-also]]
                                  [kind (count (filter #(= kind (:kind %))
                                                       (:edges all-edge-repo)))]))
                          :packet-path packet-path
                          :statement-sha256 (into (sorted-map) (map (juxt :item :statement-sha256)) (:items packet)))
       :pre-registered (into (sorted-map) (for [[id s] authored]
                                                [id {:must-not-retrodict (:must-not-retrodict s)
                                                     :reason (:must-not-reason s)}]))
       :items (into (sorted-map) (for [[id x] items] [id (dissoc x :acks :ctx)]))
       :degree-term (into (sorted-map) (for [[id x] items] [id (:degree-term x)]))
       :controls (sorted-map :cue-licence (case *licence-scope*
                                            :statement :ledger-row-statement-occurrence
                                            :clause :ledger-row-clause-occurrence)
                             :unlicensed-cues unlicensed
                             :struck-by-the-clause-arm (vec struck)
                             :determinism (into (sorted-map) (for [[id x] items] [id (cc/determinism (:ctx x))]))
                             :library-correspondence (cc/library-correspondence why-repo)
                             :grain-separation (vec (mapcat #(cc/grain-separation (:ctx %)) (vals items)))
                             :citation-count (count acks)
                             :citations-verified (cc/citations-verified acks)
                             :negative-controls controls)
       :o4 :not-exercised-nothing-is-played))))

(defn require-pass! [r]
  (let [failures (concat
                  (for [[id x] (:items r) [law ok?] (get-in x [:find :laws]) :when (not ok?)] [id :find law])
                  (for [[id x] (:items r) :when (get-in x [:find :zero-mass-selected?])] [id :f4])
                  (for [[id x] (:items r) :when (not (get-in x [:temperaments :holds?]))] [id :temperaments])
                  (for [[id x] (:items r) [_ run] (:runs x) [law ok?] (:laws run) :when (not ok?)] [id :organise law])
                  (for [[id xs] (get-in r [:controls :determinism]) x xs] [id :determinism x])
                  (for [x (get-in r [:controls :library-correspondence])
                        [_ v] x :when (false? v)] [:library-correspondence x])
                  (for [x (get-in r [:controls :grain-separation])] [:grain x])
                  (for [x (get-in r [:controls :citations-verified])] [:citation x])
                  (for [x (get-in r [:controls :negative-controls]) :when (not (and (:exercised? x) (:caught? x)))] [:control x])
                  (when (seq (get-in r [:controls :unlicensed-cues])) [[:cue-licence]]))]
    (when (seq failures) (throw (ex-info "construct-retrodiction-cascade failed" {:failures (vec failures)})))
    r))

(def per-clause-report-path "checks/retrodiction-cascade-per-clause.edn")

(defn -main [& args]
  (try
    (let [clause-arm? (some #{"--per-clause"} args)
          [scope path] (if clause-arm? [:clause per-clause-report-path]
                           [:statement report-path])
          r (binding [*licence-scope* scope] (require-pass! (report)))]
      (println "cue licence scope:" scope "->" path)
      (doseq [f (get-in r [:controls :struck-by-the-clause-arm])]
        (println "  struck" (:item f) (pr-str (:unlicensed f))))
      (spit path (with-out-str (pprint/pprint r)))
      (doseq [[id x] (:items r)]
        (println id "seed" (get-in x [:find :seed-size]) "candidates" (get-in x [:find :candidates])
                 "F4" (get-in x [:find :zero-mass-selected?])
                 ;; members AND admissions: a temperament that admitted nothing
                 ;; has a large cascade only because `find` handed it one.
                 "stops" (into {} (for [[t v] (:runs x)]
                                    [t [(:stop v) :members (:member-count v)
                                        :admitted (:admitted-count v)]]))))
      (println "construct-retrodiction-cascade: PASS")
      (shutdown-agents))
    (catch Throwable e
      (binding [*out* *err*] (println "construct-retrodiction-cascade: FAIL" (.getMessage e))
               (when-let [d (ex-data e)] (pprint/pprint d)))
      (shutdown-agents)
      (System/exit 1))))
