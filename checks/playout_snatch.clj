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
    :if (fn [s] (and (= :play (:grain s)) (:unmodelled? s)))}
   {:id :exchange-when-both-sides-gain :grain :play
    :if-text "Both players hold tokens and a positive exchange is available."
    :if (fn [s] (and (= :play (:grain s)) (:offer-made? s)
                     (pos? (:tokens s)) (pos? (:counterpart-tokens s))))}
   {:id :ask-for-surplus-not-surrender :grain :play
    :if-text "A positive offer is being composed and its ask remains part of the choice."
    :if (fn [s] (and (= :play (:grain s)) (:offer-made? s)
                     (pos? (:offer-size s)) (pos? (:ask-size s))))}
   {:id :accept-an-offer-that-beats-holding :grain :play
    :if-text "P2 receives a positive give-and-ask offer that benefits both sides."
    :if (fn [s] (and (= :play (:grain s)) (:offer-made? s)
                     (pos? (:offer-size s)) (pos? (:ask-size s))))}
   {:id :forced-play-needs-a-loss-floor :grain :play
    :if-text "Abstention is unavailable after seizure has become a live risk."
    :if (fn [s] (and (= :play (:grain s)) (:forced-offer? s) (:snatched? s)))}
   {:id :use-talk-to-make-a-testable-offer :grain :play
    :if-text "Cheap talk is available before the first observed response."
    :if (fn [s] (and (= :play (:grain s)) (:chat-available? s)
                     (nil? (:last s))))}
   {:id :price-the-final-round-as-final :grain :play
    :if-text "The known final round has an offer available."
    :if (fn [s] (and (= :play (:grain s)) (:last-round? s)
                     (:offer-made? s)))}
   {:id :re-enter-after-observed-repair :grain :play
    :if-text "A prior loss has been observably repaired and a bounded probe is available."
    :if (fn [s] (and (= :play (:grain s)) (:snatched? s)
                     (:repair-observed? s) (pos? (:tokens s))))}])

(defn applicable [s] (mapv :id (filter #((:if %) s) collection)))

;; ---- treatments, from the flowcharts -----------------------------------
(def treatments
  {:g1 {:p1-actions #{:abstain :offer} :shame-available? false :judge-available? false :chat-available? false}
   :g2 {:p1-actions #{:offer} :forced-offer? true :shame-available? false :judge-available? false :chat-available? false}
   :g3 {:p1-actions #{:abstain :offer} :shame-available? true  :judge-available? false :chat-available? false}
   :g4 {:p1-actions #{:abstain :offer} :shame-available? true  :judge-available? true  :chat-available? false}
   :g5 {:p1-actions #{:abstain :offer} :shame-available? false :judge-available? false :chat-available? true}})

;; ---- P2, by disposition -------------------------------------------------
(defn p2-response [disposition] (case disposition :sharer :O2 :snatcher :O4 :cautious :O3))

;; ---- pi: probe one token, grim trigger ----------------------------------
(defn p1-action [s]
  (if (and (:snatched? s) (not (:forced-offer? s))) :abstain :offer))

(def ^:private modelled #{:O1 :O2 :O4})   ; S-001's support; O3 carries zero mass

(defn play
  "A PLAY-grain trajectory. Every situation carries :grain :play."
  [treatment disposition rounds]
  (loop [r 1, s (merge (treatments treatment)
                        {:grain :play :treatment treatment :round 1
                         :snatched? false :repair-observed? false
                         :tokens 10 :counterpart-tokens 10 :shame 0}), trace []]
    (if (> r rounds)
      trace
      (let [act  (p1-action s)
            situation (assoc s
                             :last-round? (= r rounds)
                             :offer-made? (= act :offer)
                             :offer-size (if (= act :offer) 1 0)
                             :ask-size (if (= act :offer) 1 0))
            pats (applicable situation)
            out  (if (= act :abstain) :O1 (p2-response disposition))
            s'   (cond-> (assoc s :round (inc r)
                                 :last (case out :O2 :accepted :O3 :refused :O4 :snatched :none)
                                 :disposition-known (or (:disposition-known s) (when (#{:O2 :O4} out) true))
                                 :unmodelled? (not (contains? modelled out)))
                   (= out :O4) (-> (assoc :snatched? true) (update :tokens dec)
                                   (assoc :repair-observed? (:judge-available? s))
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
