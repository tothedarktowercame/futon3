(ns construct-open-cascade
  "The CONSTRUCTOR HARNESS of worklist row `:LA8` -- the CONSTRUCTION class of
   P-validated-R5.md:697-702: run through real OPEN work items \"not to work on
   them, but only to see whether policies could be constructed that would
   plausibly allow working on them\".

   THE SPLIT, and why this file authors nothing.  A Tension's clauses and cues
   are AUTHORED -- they are the constructor's act, and the author of them cannot
   also be the row's reviewer.  So the authored content lives in its own file,
   `checks/open-tensions.edn`, written by the constructing agent; this harness
   reads it, LICENSES it, runs it, and writes the artefact.  Whoever wrote this
   harness can therefore review the construction without having made it, and the
   plausibility judge can read the authored file directly rather than inferring
   what was authored from a Clojure map.

   THE LICENCE IS TIGHTER THAN `:LA7`'s, IN TWO RESPECTS, and that is this file's
   substantive difference from `checks/construct_retrodiction_cascade.clj`.

   (1) PER-CLAUSE SCOPE IS THE ONLY SCOPE.  `decisions.edn
   :cue-licence-for-a-ledger-row` records that `:LA7` shipped the weak form (a
   cue licensed by occurring anywhere in the whole statement), ran the per-clause
   arm in review, and left the primary artefact alone under `:cue-licence
   :not-applied-retroactively` -- state the bound, apply the new rule FROM THE
   NEXT ROW.  `:LA8` is the next row, so there is no `*licence-scope*` here: a
   cue must occur in the text of its own clause, and an unlicensed cue is a hard
   failure rather than a strike, because the author can simply not write one.

   (2) THE CLAUSE TEXT MUST ITSELF BE A QUOTATION.  `:LA7`'s per-clause arm did
   NOT check this, and that is a hole in it: under per-clause scope alone, an
   author could write any clause `:text` and license any cue against it, so the
   licence would bound nothing.  `:LA7` relied on the observation that its clause
   texts happened to be quotations; here it is checked -- every clause `:text`
   must occur in the item's addressed statement as a substring after whitespace
   normalisation.  With (2) in force, (1) is a real restriction: a cue must occur
   in a quoted fragment of the record.

   WHAT IS WEAKER THAN `:LA7`, and it is not made up for anywhere.  An open item
   records no resolution, so there is no withheld half, no blind/resolutions
   split and no git-history check on the constructor.  See
   `checks/open_items.clj` and `:there-is-no-withheld-half` in the packet.

     bb -cp .:checks -m construct-open-cascade   ->  checks/open-cascade.edn"
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [construct-cascade :as cc]
            [find-organise :as fo]))

(def packet-path "checks/open-items.edn")

(def ^:dynamic *tensions-path*
  "The AUTHORED half.  Rebindable by `--tensions` so a second ARM can be authored
   in its own file and run through the same harness, leaving the first artefact
   byte-identical.  Cue granularity is a choice the design does not settle, so it
   gets branches built and run (Joe, 2026-09-01, worklist_check.bb:44-45) -- and
   an arm that shares the harness differs from its sibling in the authored cues
   and in nothing else, which is what makes the two comparable."
  "checks/open-tensions.edn")

(def ^:dynamic *report-path* "checks/open-cascade.edn")

(defn sha256 [s]
  (let [d (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest d (.getBytes s "UTF-8"))))))

(defn read-packet [] (edn/read-string (slurp packet-path)))

(defn read-tensions
  "The authored half.  Read, never written here."
  []
  (let [t (edn/read-string (slurp *tensions-path*))]
    (when-not (= :la8/open-tensions-v1 (:schema t))
      (throw (ex-info "open-tensions.edn: wrong schema" {:schema (:schema t)})))
    t))

(defn- norm
  "Whitespace-normalised, case-folded.  A clause quotation may be re-wrapped by
   the editor that wrote it; it may not be a different sentence."
  [s]
  (-> (str s) str/lower-case (str/replace #"\s+" " ") str/trim))

;; ---------------------------------------------------------------------------
;; the licence -- three hard checks, no strikes
;; ---------------------------------------------------------------------------

(defn cue-licence
  "Every finding here is a hard failure.  Three checks, in the order a reader
   should think about them: the packet is the one that was committed
   (`:statement-sha-mismatch`); each clause text is a quotation of that statement
   (`:clause-text-not-a-quotation-of-the-statement`); each cue occurs in its own
   clause's text (`:cue-not-in-its-own-clause`).  The licence never adds a cue
   and never removes one."
  [packet authored]
  (let [rows (into {} (map (juxt :item identity)) (:items packet))]
    (vec
     (concat
      ;; an authored tension for an item that is not in the packet
      (for [[id _] authored :when (nil? (get rows id))]
        {:item id :finding :packet-item-missing})
      ;; a packet item with no authored tension
      (for [[id _] rows :when (nil? (get authored id))]
        {:item id :finding :no-authored-tension})
      (for [[id spec] authored
            :let [row (get rows id)]
            :when row
            :let [stmt-n (norm (:statement row))
                  actual (sha256 (:statement row))
                  bad-quotes (vec (for [c (:clauses spec)
                                        :when (not (str/includes? stmt-n (norm (:text c))))]
                                    {:clause (:id c) :text (:text c)}))
                  bad-cues (vec (for [c (:clauses spec)
                                      cue (:cues c)
                                      :when (not (str/includes? (norm (:text c)) (norm cue)))]
                                  {:clause (:id c) :cue cue}))]
            :when (or (not= actual (:statement-sha256 row))
                      (seq bad-quotes) (seq bad-cues))]
        {:item id
         :finding (cond (not= actual (:statement-sha256 row)) :statement-sha-mismatch
                        (seq bad-quotes) :clause-text-not-a-quotation-of-the-statement
                        :else :cue-not-in-its-own-clause)
         :clause-texts-not-quotations bad-quotes
         :unlicensed-cues bad-cues
         :expected-sha (:statement-sha256 row) :actual-sha actual})))))

(defn tension-for [row authored]
  (let [spec (get authored (:item row))]
    {:id (:item row) :statement (:statement row) :source (:statement-address row)
     :clauses (mapv #(assoc % :source (:statement-address row)) (:clauses spec))}))

(defn cascade-data
  "The keys `cc/cascade-of` actually returns.  `:LA7`'s first artefact read
   `:addedByOrganise` and `:admittedBy`, which do not exist, so both fields were
   empty in every cascade it published whatever the construction had done; the
   correction is carried here rather than re-made."
  [final seed repo]
  (let [c (cc/cascade-of final seed repo)]
    (sorted-map :members (vec (sort (:members final)))
                :precedence (into (sorted-map) (:precedence final))
                :authored-order (vec (:authored-order final))
                :ordered (fo/ordered final)
                :provenance (into (sorted-map) (:provenance final))
                :stop (:stop final) :steps (:steps final)
                :nodes (vec (sort (:nodes c))) :edges (vec (sort (:edges c)))
                :selected (vec (sort (:selected c)))
                :added-by-organise (vec (sort (:added-by-organise c)))
                :admitted-by (vec (sort (:admitted-by c))))))

(defn one-item [row authored why-repo related]
  (let [id (:item row) spec (get authored id) tension (tension-for row authored)]
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
                               :expected-to-fire? (contains? (set (:expected-to-fire spec)) pid)}]))
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
         :remedy-language (:remedy-language row)
         :find {:selected (vec (:selected found)) :seed-size (count seed)
                :candidates (- (count matches) (count seed))
                :laws (into (sorted-map) (for [[k f] fo/find-laws] [k (boolean (f find-row))]))
                :zero-mass-pattern (:zero-mass-pattern spec)
                :zero-mass-reason (:zero-mass-reason spec)
                :zero-mass-selected? (contains? (set (:selected found)) (:zero-mass-pattern spec))}
         :antecedent-reach {:rows rows
                            :confusion {:expected (into (sorted-set) (:expected-to-fire spec))
                                        :fired fired
                                        :expected-but-did-not-fire (into (sorted-set) (remove fired (:expected-to-fire spec)))
                                        :fired-but-not-expected (into (sorted-set) (remove (set (:expected-to-fire spec)) fired))}}
         :temperaments (cc/differ-only-in-the-stop cc/budgeted-temperament cc/floored-temperament)
         :runs (into (sorted-map)
                     (for [[tid f] runs]
                       [tid {:stop (:stop f) :member-count (count (:members f))
                             ;; A temperament whose seed is already at or over
                             ;; `budget` halts at step 0 having ADMITTED NOTHING;
                             ;; its "cascade" is then `find`'s output verbatim and
                             ;; no construction happened.  Member count alone hides
                             ;; that, so these five fields are reported for every run.
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
        tfile (read-tensions)
        authored (:tensions tfile)
        unlicensed (cue-licence packet authored)]
    (when (seq unlicensed)
      (throw (ex-info "open-cascade: packet or cue licence failed" {:failures unlicensed})))
    (let [sections (cc/library-sections cc/library-root)
          why-repo (fo/read-repository cc/library-root sections {:kinds #{:why}})
          wh-repo (fo/read-repository cc/library-root sections {:kinds #{:why :how}})
          all-edge-repo (fo/read-repository cc/library-root sections
                                            {:kinds #{:why :why-posthoc :how :see-also}})
          related (cc/related-adjacency wh-repo)
          items (into (sorted-map)
                      (for [row (:items packet)] [(:item row) (one-item row authored why-repo related)]))
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
       :schema :la8/open-cascade-v1
       :authored-by (:authored-by tfile)
       :harness-by (:harness-by tfile)
       :nothing-is-enacted
       "Construct, persist, judge. No open item is worked on here, no ledger row moves, no ticket is edited, no .flexiarg is written and no @why/@why-posthoc/@how/@see-also directive is added anywhere. `:o4` below is :not-exercised-nothing-is-played for the same reason."
       :as-of (sorted-map :library-root cc/library-root :sections sections
                          :pattern-count (count (:patterns why-repo))
                          :read-digest (cc/read-digest wh-repo)
                          :authored-edge-counts
                          (into (sorted-map)
                                (for [kind [:why :why-posthoc :how :see-also]]
                                  [kind (count (filter #(= kind (:kind %)) (:edges all-edge-repo)))]))
                          :packet-path packet-path
                          :tensions-path *tensions-path*
                          :statement-sha256 (into (sorted-map) (map (juxt :item :statement-sha256)) (:items packet)))
       :pre-registered (into (sorted-map)
                             (for [[id s] authored]
                               [id {:must-not-help-with (:must-not-help-with s)
                                    :reason (:must-not-reason s)
                                    :zero-mass-pattern (:zero-mass-pattern s)
                                    :expected-to-fire (into (sorted-set) (:expected-to-fire s))}]))
       :items (into (sorted-map) (for [[id x] items] [id (dissoc x :acks :ctx)]))
       :degree-term (into (sorted-map) (for [[id x] items] [id (:degree-term x)]))
       :controls (sorted-map
                  :cue-licence :per-clause-quotation-of-the-addressed-statement
                  :licence-checks [:statement-sha256-recomputed
                                   :clause-text-is-a-quotation-of-the-statement
                                   :cue-occurs-in-its-own-clause-text]
                  :clause-text-quotation-check :enforced-here-and-not-in-LA7
                  :unlicensed-cues unlicensed
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
                  (for [x (get-in r [:controls :library-correspondence]) [_ v] x :when (false? v)]
                    [:library-correspondence x])
                  (for [x (get-in r [:controls :grain-separation])] [:grain x])
                  (for [x (get-in r [:controls :citations-verified])] [:citation x])
                  (for [x (get-in r [:controls :negative-controls])
                        :when (not (and (:exercised? x) (:caught? x)))] [:control x])
                  (when (seq (get-in r [:controls :unlicensed-cues])) [[:cue-licence]]))]
    (when (seq failures)
      (throw (ex-info "construct-open-cascade failed" {:failures (vec failures)})))
    r))

(defn- arg-after [args flag default]
  (or (second (drop-while #(not= flag %) args)) default))

(defn -main [& args]
  (try
    (binding [*tensions-path* (arg-after args "--tensions" *tensions-path*)
              *report-path* (arg-after args "--out" *report-path*)]
      (let [r (require-pass! (report))]
        (spit *report-path* (with-out-str (pprint/pprint r)))
        (println "cue licence:" (get-in r [:controls :cue-licence]) "->" *report-path*)
        (doseq [[id x] (:items r)]
          (println id "seed" (get-in x [:find :seed-size])
                   "candidates" (get-in x [:find :candidates])
                   "F4" (get-in x [:find :zero-mass-selected?])
                   "stops" (into {} (for [[t v] (:runs x)]
                                      [t [(:stop v) :members (:member-count v)
                                          :admitted (:admitted-count v)]]))))
        (println "construct-open-cascade: PASS")
        (shutdown-agents)))
    (catch Throwable e
      (binding [*out* *err*]
        (println "construct-open-cascade: FAIL" (.getMessage e))
        (when-let [d (ex-data e)] (pprint/pprint d)))
      (shutdown-agents)
      (System/exit 1))))
