(ns playout-snatch
  "A pattern-theoretic playout of Snatch or Share.

   At each round we ask three things and record the answers:
     1. COVERAGE  — which pattern's IF matches this situation? (none = a gap)
     2. PATH      — does the trajectory trace edges of the @how graph?
     3. Q         — how does item S-001's prediction fare?

   Deterministic and reproducible: P2's disposition is fixed per run, so the
   trace can be read rather than sampled.")

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
   {:id :probe-before-committing :grain :play
    :if-text "A counterpart whose disposition you do not know."
    :if (fn [s] (and (= :play (:grain s)) (nil? (:disposition-known s))))}
   {:id :escalate-only-as-far-as-you-can-lose :grain :play
    :if-text "A counterpart has accepted an offer."
    :if (fn [s] (and (= :play (:grain s)) (= :accepted (:last s))))}
   {:id :consult-the-remedy-before-exiting :grain :play
    :if-text "You were defected against and the arrangement provides a response."
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s)
                     (or (:shame-available? s) (:judge-available? s))))}
   {:id :a-free-mark-is-always-worth-assigning :grain :play
    :if-text "A mark may be attached and changes no payoff this round."
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s) (:shame-available? s)))}
   {:id :an-unmodelled-response-stops-the-line :grain :play
    :if-text "An outcome to which your model assigned no probability."
    :if (fn [s] (and (= :play (:grain s)) (:unmodelled? s)))}])

(defn applicable [s] (mapv :id (filter #((:if %) s) collection)))

;; ---- treatments, from the flowcharts -----------------------------------
(def treatments
  {:g1 {:p1-actions #{:abstain :offer} :shame-available? false :judge-available? false :chat-available? false}
   :g3 {:p1-actions #{:abstain :offer} :shame-available? true  :judge-available? false :chat-available? false}
   :g4 {:p1-actions #{:abstain :offer} :shame-available? true  :judge-available? true  :chat-available? false}})

;; ---- P2, by disposition -------------------------------------------------
(defn p2-response [disposition] (case disposition :sharer :O2 :snatcher :O4 :cautious :O3))

;; ---- pi: probe one token, grim trigger ----------------------------------
(defn p1-action [s] (if (:snatched? s) :abstain :offer))

(def ^:private modelled #{:O1 :O2 :O4})   ; S-001's support; O3 carries zero mass

(defn play
  "A PLAY-grain trajectory. Every situation carries :grain :play."
  [treatment disposition rounds]
  (loop [r 1, s (merge (treatments treatment) {:grain :play :round 1 :snatched? false :tokens 10 :shame 0}), trace []]
    (if (> r rounds)
      trace
      (let [pats (applicable s)
            act  (p1-action s)
            out  (if (= act :abstain) :O1 (p2-response disposition))
            s'   (cond-> (assoc s :round (inc r)
                                 :last (case out :O2 :accepted :O3 :refused :O4 :snatched :none)
                                 :disposition-known (or (:disposition-known s) (when (#{:O2 :O4} out) true))
                                 :unmodelled? (not (contains? modelled out)))
                   (= out :O4) (-> (assoc :snatched? true) (update :tokens dec)
                                   (update :shame #(if (:shame-available? s) (inc %) %)))
                   (= out :O2) (update :tokens inc))]
        (recur (inc r) s' (conj trace {:round r :patterns pats :action act :outcome out
                                       :tokens (:tokens s) :shame (:shame s)}))))))

(defn- show [treatment disposition]
  (println (format "\n── %s, P2 is a %s ──" (name treatment) (name disposition)))
  (doseq [t (play treatment disposition 5)]
    (println (format "  r%d  patterns %-58s -> %s %s"
                     (:round t) (pr-str (:patterns t)) (name (:action t)) (name (:outcome t)))))
  (let [gaps (count (filter #(empty? (:patterns %)) (play treatment disposition 5)))]
    (println (format "  coverage gaps (rounds matching NO pattern): %d" gaps))))

(defn -main [& _]
  (println "PATTERN-THEORETIC PLAYOUT — Snatch or Share")
  (println "\nDESIGN-grain situations (choosing an institution):")
  (doseq [[label s] [["designing G1 from scratch" {:grain :design :judge-available? false}]
                     ["a rule to force offers is proposed" {:grain :design :rule-proposed? true :judge-available? false}]
                     ["we want sanctions, no judge" {:grain :design :want-sanction? true :judge-available? false}]
                     ["a judge is available" {:grain :design :judge-available? true}]]]
    (println (format "  %-36s -> %s" label (pr-str (applicable s)))))
  (show :g1 :snatcher) (show :g1 :sharer) (show :g1 :cautious)
  (show :g4 :snatcher)
  (println "\n── item S-001 (pi = probe-one-token, G1, round 1) ──")
  (println "  Q = {O1 0.0, O2 0.5, O3 0.0, O4 0.5}; falsifier O3")
  (doseq [d [:sharer :snatcher :cautious]]
    (let [o (:outcome (first (play :g1 d 1)))]
      (println (format "  P2 %-9s -> %s  %s" (name d) (name o)
                       (case o :O2 "in support, posterior collapses to sharer"
                               :O4 "in support, posterior collapses to snatcher"
                               :O3 "*** FALSIFIER FIRES — the two-disposition model is refuted ***"
                               "outside support"))))))
