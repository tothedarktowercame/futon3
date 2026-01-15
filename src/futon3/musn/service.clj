(ns futon3.musn.service
  "Session registry and thin wrappers around the MUSN router. Intended to be wrapped by HTTP/Drawbridge handlers.

   This module also handles light persistence (append-only log + snapshot) so MUSN sessions can survive crashes
   and be replayed or resumed. Logs are EDN lines under lab/musn/, keyed by session id."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [futon2.aif.engine :as aif-engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon3.fulab.pattern-competence :as pc]
            [futon3.logic-audit :as logic-audit]
            [futon3.musn.fulab :as fulab-musn]
            [futon3.musn.router :as router]
            [futon3.pattern-hints :as hints]))

(def default-policy
  {:off-trail {:free 3 :ratio 0.5 :action "off-trail"}
   :trail-allow {:patterns [] :namespaces []}
   :aif-config {}
   :mana {:start 100
          :action-cost 1
          :restore 10}
   :certificates []})

(def default-lab-root
  (or (System/getenv "MUSN_LAB_ROOT") "lab"))

(defonce sessions (atom {}))
(declare get-session append-lab-event! restored? note-restore! apply-mana! mana-config)
(defonce aif-tap-installed? (atom false))

(defn- tap->event [tap]
  {:event/type :aif/tap
   :at (fulab-musn/now-inst)
   :payload tap})

(defn- handle-aif-tap! [tap]
  (when (= :aif/fulab (:type tap))
    (when-let [sid (:session/id tap)]
      (when-let [entry (get-session sid)]
        (let [event (tap->event tap)]
          (reset! (:aif-tap entry) event)
          (append-lab-event! entry event))))))

(defn- ensure-aif-tap-listener! []
  (when (compare-and-set! aif-tap-installed? false true)
    (add-tap #'handle-aif-tap!)))

(defn- aif-engine-state [entry]
  (when-let [engine (:aif-engine entry)]
    (when-let [state (:state engine)]
      @state)))

(defn- aif-mu-summary [entry pattern-id]
  (let [state (aif-engine-state entry)
        evidence (:pattern-evidence state)
        pattern-evidence (when (and pattern-id (map? evidence))
                           (get evidence pattern-id))]
    (cond-> {:pattern-count (count (or evidence {}))}
      (seq pattern-evidence) (assoc :pattern-evidence pattern-evidence))))

(defn- candidate-precision-summary [candidate-details candidate-ids]
  (into {}
        (keep (fn [pid]
                (let [details (get candidate-details pid)
                      fallback (hints/pattern-entry pid)
                      payload (merge (when (map? details)
                                       (select-keys details
                                                    [:precision-prior
                                                     :evidence-count
                                                     :next-steps-count]))
                                     (when (map? fallback)
                                       {:precision-prior (:precision/prior fallback)
                                        :evidence-count (:evidence/count fallback)
                                        :next-steps-count (:next-steps/count fallback)}))]
                  (when (seq payload)
                    [pid payload]))))
        candidate-ids))

(defn- resolve-pattern-files [pattern-id files]
  (let [fallback (some-> (hints/pattern-entry pattern-id) :source/path)
        candidates (->> (concat (or files [])
                                (when fallback [fallback]))
                        (remove nil?)
                        distinct)]
    (->> candidates
         (map io/file)
         (filter #(.exists %))
         vec)))

(defn- current-pattern-maturity [pattern-id files]
  (let [targets (resolve-pattern-files pattern-id files)]
    (some #(hints/maturity-for-pattern-file % pattern-id) targets)))

(defn- previous-pattern-maturity [entry pattern-id]
  (or (get @(:pattern-maturity entry) pattern-id)
      (some-> (hints/pattern-entry pattern-id) :maturity/phase)))

(defn- note-pattern-maturity! [entry pattern-id phase]
  (swap! (:pattern-maturity entry) assoc pattern-id phase))

(defn- maybe-award-maturity-bonus! [entry pattern-id files action]
  (when (and (:mana entry)
             (contains? #{"update" "implement"} (str action))
             (string? pattern-id))
    (when-let [current (current-pattern-maturity pattern-id files)]
      (let [current-phase (:maturity/phase current)
            prev-phase (previous-pattern-maturity entry pattern-id)
            mana @(:mana entry)
            awarded? (restored? mana :maturity-upgrade pattern-id)]
        (note-pattern-maturity! entry pattern-id current-phase)
        (when (and (not awarded?)
                   (= prev-phase :stub)
                   (= current-phase :active))
          (let [{:keys [maturity-bonus]} (mana-config (:policy entry))
                bonus (double (or maturity-bonus 10))]
            (apply-mana! entry bonus :maturity-upgrade
                         {:pattern/id pattern-id
                          :from prev-phase
                          :to current-phase})
            (note-restore! entry :maturity-upgrade pattern-id)
            (append-lab-event! entry {:event/type :mana/bonus
                                      :at (fulab-musn/now-inst)
                                      :payload {:pattern/id pattern-id
                                                :from prev-phase
                                                :to current-phase
                                                :delta bonus}})))))))

(defn- tap-aif-context! [payload]
  (tap> (merge {:type :aif/context} payload)))

(defn- tap-aif-policy! [payload]
  (tap> (merge {:type :aif/policy} payload)))

(defn- session-agent [client-id]
  (keyword (or (some-> client-id str str/trim) "musn")))

(defn- session-file [lab-root sid]
  (io/file lab-root "sessions" (str sid ".edn")))

(defn- read-lab-session [lab-root sid client-id]
  (let [path (session-file lab-root sid)]
    (if (.exists path)
      (pc/read-session-file (str path))
      {:session/id sid
       :session/agent (session-agent client-id)
       :events []})))

(defn- write-lab-session! [entry session]
  (let [path (str (:session-file entry))]
    (io/make-parents path)
    (pc/write-session-file! path session)))

(defn- append-lab-event! [entry event]
  (let [session (pc/append-event @(:lab-session entry) event)]
    (reset! (:lab-session entry) session)
    (write-lab-session! entry session)
    session))

(defn- new-fulab-state []
  {:turn nil
   :candidates []
   :candidate-scores {}
   :candidate-details {}
   :psr nil
   :pur nil
   :psr-emitted? false
   :pur-emitted? false
   :deviation nil              ;; AIF-LM-3: track deviation from AIF suggestion
   :turn-ended nil
   :turn-ended-resp nil
   :aif-config-logged? false})

(defn- tau-cache-path [lab-root]
  (io/file lab-root "aif" "tau-cache.edn"))

(defn- read-tau-cache [lab-root]
  (let [path (tau-cache-path lab-root)]
    (when (.exists path)
      (try
        (edn/read-string (slurp path))
        (catch Throwable _ nil)))))

(defn- write-tau-cache! [lab-root cache]
  (let [path (tau-cache-path lab-root)]
    (io/make-parents path)
    (spit path (pr-str cache))))

(defn- mean [values]
  (/ (reduce + values) (double (count values))))

(defn- scores->tau [scores]
  (let [scores (->> scores
                    (keep #(when (number? %) (double %)))
                    sort
                    vec)]
    (cond
      (< (count scores) 2) 0.5
      :else (let [best (first scores)
                  second (second scores)
                  worst (last scores)
                  range (max 1.0e-6 (- (double worst) (double best)))
                  spread (/ (- (double second) (double best)) range)
                  scaled (+ 0.35 (* 0.6 (min 1.0 spread)))]
              (-> scaled (max 0.35) (min 0.95) double)))))

(defn- tau-scale [entry]
  (double (or (:tau/scale (:aif-config entry)) 1.0)))

(defn- tau->uncertainty [entry tau]
  (when (and (number? tau) (pos? tau))
    (/ (tau-scale entry) (double tau))))

(defn- scores->uncertainty [entry scores]
  (when (seq scores)
    (tau->uncertainty entry (scores->tau scores))))

(defn- uncertainty-detail [value source detail]
  (when (number? value)
    (merge {:value (double value)
            :source source}
           detail)))

(defn- precision->uncertainty [precision]
  (when (number? precision)
    (/ 1.0 (double (max 0.05 precision)))))

(declare candidate-id)

(defn- candidate-details-map [details]
  (->> details
       (keep (fn [entry]
               (when-let [id (candidate-id entry)]
                 [id entry])))
       (into {})))

(defn- candidate-precision [entry pattern-id]
  (or (get-in @(:fulab entry) [:candidate-details pattern-id :precision-prior])
      (some-> (hints/pattern-entry pattern-id) :precision/prior)
      0.5))

(defn- candidates-uncertainty-details
  ([entry candidate-ids]
   (candidates-uncertainty-details entry candidate-ids nil nil))
  ([entry candidate-ids candidate-details]
   (candidates-uncertainty-details entry candidate-ids candidate-details nil))
  ([entry candidate-ids candidate-details scores]
   (when (seq candidate-ids)
     (let [cache @(:tau-cache entry)
           taus (keep cache candidate-ids)
           cached (when (seq taus) (mean taus))
           precision-lookup (if (map? candidate-details)
                              (fn [pid] (get-in candidate-details [pid :precision-prior]))
                              (fn [pid] (candidate-precision entry pid)))
           precisions (keep precision-lookup candidate-ids)
           precision (when (seq precisions) (mean precisions))
           score-values (cond
                          (map? scores) (vals scores)
                          (sequential? scores) scores
                          :else nil)
           score-uncertainty (scores->uncertainty entry score-values)]
       (or (uncertainty-detail (tau->uncertainty entry cached)
                               :tau-cache
                               {:tau cached})
           (uncertainty-detail (precision->uncertainty precision)
                               :precision-prior
                               {:precision precision})
           (uncertainty-detail score-uncertainty
                               :score-spread
                               {}))))))

(defn- pattern-uncertainty-details [entry pattern-id]
  (when pattern-id
    (let [cached (get @(:tau-cache entry) pattern-id)
          precision (candidate-precision entry pattern-id)
          score (get-in @(:fulab entry) [:candidate-scores pattern-id])]
      (or (uncertainty-detail (tau->uncertainty entry cached)
                              :tau-cache
                              {:tau cached})
          (uncertainty-detail (precision->uncertainty precision)
                              :precision-prior
                              {:precision precision})
          (uncertainty-detail (scores->uncertainty entry [score])
                              :score-spread
                              {})))))

(defn- update-tau-cache! [entry pattern-id tau]
  (when (and pattern-id (number? tau))
    (let [next (assoc (or @(:tau-cache entry) {}) pattern-id (double tau))]
      (reset! (:tau-cache entry) next)
      (write-tau-cache! (:lab-root entry) next))))

(defn- mana-config [policy]
  (merge {:start 100
          :action-cost 1
          :restore 10
          :maturity-bonus 10}
         (:mana policy)))

(defn- new-mana-state [policy]
  (let [{:keys [start]} (mana-config policy)
        start (or start 100)]
    {:budget start
     :balance start
     :earned 0
     :spent 0
     :turn nil
     :restores {:psr->action #{}
                :action->pur #{}
                :maturity-upgrade #{}}
     :last-change nil}))

(defn- reset-mana-turn! [entry turn]
  (swap! (:mana entry)
         (fn [mana]
           (-> mana
               (assoc :turn turn)
               (assoc :restores {:psr->action #{}
                                 :action->pur #{}
                                 :maturity-upgrade #{}})))))

(defn- mana-view [mana]
  (dissoc mana :restores))

(defn- apply-mana! [entry delta reason meta]
  (let [delta (double delta)
        amount (Math/abs delta)]
    (swap! (:mana entry)
           (fn [mana]
             (-> mana
                 (update :balance + delta)
                 (update (if (pos? delta) :earned :spent) + amount)
                 (assoc :last-change (merge {:delta delta
                                             :reason reason
                                             :at (fulab-musn/now-inst)}
                                            meta)))))))

(defn- mana-balance [entry]
  (let [mana @(:mana entry)]
    (cond
      (number? (:balance mana)) (double (:balance mana))
      (number? (:budget mana)) (double (:budget mana))
      :else nil)))

(defn- mark-mana-depleted! [entry turn]
  (when-let [balance (mana-balance entry)]
    (when (neg? balance)
      (swap! (:state entry)
             (fn [st]
               (if (= :mana-depleted (get-in st [:halt :type]))
                 st
                 (assoc st :halt {:type :mana-depleted
                                  :note "mana below zero; turn ends"}))))
      (append-lab-event! entry {:event/type :mana/depleted
                                :at (fulab-musn/now-inst)
                                :payload {:turn turn
                                          :balance balance}}))))

(defn- restored? [mana kind pattern-id]
  (contains? (get-in mana [:restores kind] #{}) pattern-id))

(defn- note-restore! [entry kind pattern-id]
  (swap! (:mana entry)
         (fn [mana]
           (update-in mana [:restores kind] (fnil conj #{}) pattern-id))))

(defn- reset-fulab-state! [entry turn candidates scores candidate-details]
  (reset! (:fulab entry)
          (assoc (new-fulab-state)
                 :turn turn
                 :candidates (vec (or candidates []))
                 :candidate-scores (or scores {})
                 :candidate-details (or candidate-details {}))))

(defn- event-base [session-id turn]
  {:event/type :turn/event
   :session/id session-id
   :turn turn
   :at (fulab-musn/now-inst)})

(defn- turn-event [event-type session-id turn payload]
  (cond-> (merge (event-base session-id turn)
                 {:event/type event-type})
    (map? payload) (assoc :payload payload)))

(defn- pattern-action-event [session-id turn action]
  (let [payload {:session/id session-id
                 :pattern/id (:pattern/id action)
                 :pattern/action (:action action)
                 :pattern/note (:note action)
                 :pattern/files (:files action)
                 :pattern/source (:source action)}]
    (merge (event-base session-id turn)
           {:event/type :pattern/action
            :pattern/id (:pattern/id action)
            :pattern/action (:action action)}
           payload
           {:payload payload})))

(defn- action-anchors [actions pattern-id]
  (->> actions
       (filter #(= pattern-id (:pattern/id %)))
       (map (fn [action]
              (fulab-musn/pattern-action-anchor pattern-id (:action action))))
       distinct
       vec))

(defn- selection-metrics [selection]
  (let [aif (:aif selection)
        rejected (:G-rejected aif)]
    (cond-> {}
      (number? (:G-chosen aif)) (assoc :G (:G-chosen aif))
      (number? (:tau aif)) (assoc :tau (:tau aif))
      (and (map? rejected) (seq rejected)) (assoc :G-rejected rejected))))

(defn- selection-g-terms [selection]
  (let [aif (:aif selection)
        g (if (number? (:G-chosen aif))
            (double (:G-chosen aif))
            0.0)
        rejected (:G-rejected aif)
        rejected-count (if (map? rejected) (count rejected) 0)]
    [{:term-id :base
      :observation-keys [:candidates :anchors :forecast]
      :precision-channels [[:heuristic 1.0]]
      :intermediate-values {:g-chosen g
                            :g-rejected-count rejected-count}
      :final-contribution g}]))

(defn- enrich-reason [reason selection]
  (let [metrics (selection-metrics selection)]
    (if (seq metrics)
      (assoc reason :aif metrics)
      reason)))

(defn- psr-eligible? [candidates chosen]
  (and (seq candidates)
       (string? chosen)
       (some #{chosen} candidates)))
(def ^:private auto-select-sentinels #{"auto" "aif"})

(defn- auto-select-requested? [chosen]
  (cond
    (nil? chosen) true
    (keyword? chosen) (contains? auto-select-sentinels (name chosen))
    (string? chosen) (contains? auto-select-sentinels (str/lower-case chosen))
    :else false))

(defn- detect-deviation
  "Detect when agent's explicit choice differs from AIF-sampled suggestion.
   Returns deviation info or nil."
  [aif-suggested agent-chosen reason]
  (when (and (string? aif-suggested)
             (string? agent-chosen)
             (not= aif-suggested agent-chosen))
    (let [deviation-reason (get-in reason [:deviation :reason])]
      {:aif-suggested aif-suggested
       :agent-chosen agent-chosen
       :justified? (boolean (and (string? deviation-reason)
                                 (> (count deviation-reason) 0)))
       :reason deviation-reason})))

(defn- auto-selection-policy [selection chosen candidates]
  (let [aif (:aif selection)
        abstain? (boolean (:abstain? aif))
        mode (if (and chosen (not abstain?)) :auto-select :abstain)
        reason (cond
                 abstain? "aif-low-tau"
                 (empty? candidates) "no-candidates"
                 (nil? chosen) "no-choice"
                 :else "aif-sampled")]
    {:chosen chosen
     :tau (:tau aif)
     :min-sample (:min-sample aif)
     :mode mode
     :reason reason
     :sampled? (:sampled? aif)
     :abstain? abstain?}))
(defn- gen-session-id []
  (str "musn-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- compact [m]
  (into {} (remove (comp nil? val)) m))

(defn- candidate-id [candidate]
  (cond
    (map? candidate) (recur (or (:id candidate) (:pattern/id candidate)))
    (keyword? candidate) (subs (str candidate) 1)
    (string? candidate) candidate
    (nil? candidate) nil
    :else (str candidate)))

(defn- candidate-ids [candidates]
  (->> candidates (map candidate-id) (remove nil?) vec))

(defn- normalize-score-keys [scores]
  (when (map? scores)
    (into {}
          (keep (fn [[k v]]
                  (let [id (cond
                             (string? k) k
                             (keyword? k) (subs (str k) 1)
                             (nil? k) nil
                             :else (str k))]
                    (when id [id v]))))
          scores)))

(defn- ensure-dir [f]
  (when f
    (.mkdirs (.getParentFile f))))

(defn- log-path [lab-root sid]
  (io/file lab-root "musn" (str sid ".edn")))

(defn- snapshot-path [lab-root sid]
  (io/file lab-root "musn" (str sid ".snapshot.edn")))

(defn- read-snapshot [f]
  (when (.exists f)
    (edn/read-string (slurp f))))

(defn- write-snapshot! [f state]
  (ensure-dir f)
  (spit f (pr-str state)))

(defn- append-log! [f payload]
  (ensure-dir f)
  (spit f (str (pr-str payload) "\n") :append true))

(defn session-ids []
  (keys @sessions))

(defn get-session [sid]
  (get @sessions sid))

(defn current-state [sid]
  (when-let [entry (get-session sid)]
    @(:state entry)))

(defn ensure-session!
  [sid policy lab-root client-id]
  (if-let [existing (get @sessions sid)]
    existing
    (let [p (merge default-policy (or policy {}))
          lab-root (or lab-root default-lab-root)
          snap (read-snapshot (snapshot-path lab-root sid))
          st (atom (or snap (router/new-session p)))
          engine (aif-engine/new-engine (fulab/new-adapter (:aif-config p))
                                        {:beliefs {}})
          session-file (session-file lab-root sid)
          lab-session (atom (read-lab-session lab-root sid client-id))
          entry {:state st
                 :policy p
                 :lab-root lab-root
                 :log-file (log-path lab-root sid)
                 :snapshot-file (snapshot-path lab-root sid)
                 :id sid
                 :session-file session-file
	         :lab-session lab-session
	         :fulab (atom (new-fulab-state))
                 :aif-tap (atom nil)
	         :tau-cache (atom (or (read-tau-cache lab-root) {}))
                 :pattern-maturity (atom {})
	         :mana (atom (new-mana-state p))
                 :aif-engine engine
                 :aif-config (:aif-config p)
                 :certificates (:certificates p)
                 :client-id client-id}]
      (swap! sessions assoc sid entry)
      (ensure-aif-tap-listener!)
      entry)))

(defn- persist! [entry op req resp]
  (let [state @(:state entry)
        event {:ts (java.time.Instant/now)
               :session/id (:id entry)
               :op op
               :req req
               :resp resp
               :state (select-keys state [:turn :plan? :selection :actions :trail :warnings :halt :write?])}]
    (append-log! (:log-file entry) event)
    (write-snapshot! (:snapshot-file entry) state)))

(defn create-session!
  [req]
  (let [sid (or (:session/id req) (gen-session-id))
        client-id (get-in req [:client :id])
        entry (ensure-session! sid (:policy req) (:lab/root req) client-id)]
    (when (and (:aif-config entry)
               (not (:aif-config-logged? @(:fulab entry))))
      (append-lab-event! entry {:event/type :aif/config
                                :at (fulab-musn/now-inst)
                                :payload {:session/id sid
                                          :config (:aif-config entry)}})
      (swap! (:fulab entry) assoc :aif-config-logged? true))
    (persist! entry :session/create req {:ok true :session/id sid :policy (:policy entry)})
    {:ok true
     :session/id sid
     :policy (:policy entry)}))

(defn turn-start!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        candidates (get-in req [:hud :candidates])
        candidate-ids (candidate-ids candidates)
        candidate-details (candidate-details-map (get-in req [:hud :candidate-details]))
        scores (normalize-score-keys (get-in req [:hud :scores]))
        turn (:turn req)
        scores (when (seq candidate-ids)
                 (if (map? scores)
                   (select-keys scores candidate-ids)
                   scores))
        uncertainty-detail (candidates-uncertainty-details entry candidate-ids candidate-details scores)
        uncertainty (:value uncertainty-detail)
        selection (when (seq candidate-ids)
                    (aif-engine/select-pattern
                     (:aif-engine entry)
                     {:decision/id (str sid ":turn-" turn)
                      :session/id sid
                      :candidates candidate-ids
                      :candidate-scores scores
                      :uncertainty uncertainty
                      :anchors [(fulab-musn/turn-anchor turn)]
                      :forecast (fulab-musn/build-forecast turn)}))
        resp (router/handle-turn-start! (:state entry)
                                        {:session/id sid
                                         :turn (:turn req)
                                         :hud (:hud req)})]
    (reset-fulab-state! entry turn candidate-ids scores candidate-details)
    (when selection
      (tap-aif-context! {:event :select
                         :session/id sid
                         :turn turn
                         :decision/id (get-in selection [:decision/id])
                         :candidates candidate-ids
                         :candidate-scores scores
                         :precision-priors (candidate-precision-summary candidate-details candidate-ids)
                         :uncertainty uncertainty-detail
                         :aif (:aif selection)
                         :mu (aif-mu-summary entry (:chosen selection))}))
    (when (:mana entry)
      (reset-mana-turn! entry turn))
    (append-lab-event! entry (turn-event :turn/started sid turn {:session/id sid}))
    (persist! entry :turn/start req resp)
    (cond-> resp
      selection (assoc :aif (:aif selection))
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn turn-plan!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        resp (router/handle-turn-plan! (:state entry)
                                       {:session/id sid
                                        :turn (:turn req)
                                        :plan (:plan req)})]
    (persist! entry :turn/plan req resp)
    resp))

(defn turn-select!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        candidates (candidate-ids (:candidates req))
        raw-chosen (:chosen req)
        auto? (auto-select-requested? raw-chosen)
        chosen (when-not auto? (candidate-id raw-chosen))
        fulab-state @(:fulab entry)
        candidate-details (:candidate-details fulab-state)
        score-map (:candidate-scores fulab-state)
        score-map (when (map? score-map)
                    (select-keys score-map candidates))
        uncertainty-detail (or (pattern-uncertainty-details entry chosen)
                                (candidates-uncertainty-details entry candidates candidate-details score-map))
        uncertainty (:value uncertainty-detail)
        selection (aif-engine/select-pattern
                   (:aif-engine entry)
                   (cond-> {:decision/id (str sid ":turn-" turn)
                            :session/id sid
                            :candidates candidates
                            :candidate-scores (get-in @(:fulab entry) [:candidate-scores])
                            :uncertainty uncertainty
                            :anchors [(fulab-musn/turn-anchor turn)]
                            :forecast (fulab-musn/build-forecast turn)}
                     (not auto?) (assoc :chosen chosen)))
        aif (:aif selection)
        aif-suggested (:chosen selection)
        sampled (when auto? aif-suggested)
        abstain? (boolean (and auto? (:abstain? aif)))
        chosen (cond
                 (and auto? (not abstain?)) sampled
                 auto? nil
                 :else chosen)
        ;; AIF-LM-3: Detect deviation from AIF suggestion
        deviation (when-not auto?
                    (detect-deviation aif-suggested chosen (:reason req)))
        policy (when auto?
                 (auto-selection-policy selection chosen candidates))
        reason (enrich-reason (or (:reason req) {:mode :read}) selection)
        reason (cond-> reason
                 auto? (assoc :source (or (:source reason) :auto)))
        req* (compact (assoc req
                             :session/id sid
                             :turn turn
                             :candidates candidates
                             :chosen chosen
                             :reason reason))
        resp (router/handle-turn-select! (:state entry) req*)
        aif-summary (selection-metrics selection)
        g-terms (when selection (selection-g-terms selection))
        psr (when (psr-eligible? candidates chosen)
              (cond-> (fulab-musn/build-psr {:session-id sid
                                             :turn turn
                                             :candidates candidates
                                             :chosen chosen
                                             :reason reason
                                             :anchors (:anchors req)
                                             :certificates (:certificates entry)})
                (seq g-terms) (assoc :aif/g-terms g-terms)
                deviation (assoc :deviation deviation)))]
    (when policy
      (append-lab-event! entry {:event/type :aif/policy
                                :at (fulab-musn/now-inst)
                                :payload (select-keys policy [:chosen :tau :min-sample :mode :reason :sampled? :abstain?])})
      (tap-aif-policy! {:session/id sid
                        :turn turn
                        :policy (select-keys policy [:chosen :tau :min-sample :mode :reason :sampled? :abstain?])}))
    ;; AIF-LM-3: Log deviation event
    (when deviation
      (append-lab-event! entry {:event/type :aif/deviation
                                :at (fulab-musn/now-inst)
                                :payload {:session/id sid
                                          :turn turn
                                          :aif-suggested (:aif-suggested deviation)
                                          :agent-chosen (:agent-chosen deviation)
                                          :justified? (:justified? deviation)
                                          :reason (:reason deviation)}})
      (swap! (:fulab entry) assoc :deviation deviation))
    (when psr
      (append-lab-event! entry {:event/type :pattern/selection-claimed
                                :at (fulab-musn/now-inst)
                                :payload {:psr psr}})
      (swap! (:fulab entry) assoc
             :psr psr
             :psr-emitted? true))
    (when selection
      (tap-aif-context! {:event :select
                         :session/id sid
                         :turn turn
                         :decision/id (get selection :decision/id)
                         :candidates candidates
                         :candidate-scores score-map
                         :precision-priors (candidate-precision-summary candidate-details candidates)
                         :uncertainty uncertainty-detail
                         :chosen chosen
                         :aif (:aif selection)
                         :deviation deviation
                         :mu (aif-mu-summary entry chosen)})
      (append-lab-event! entry (fulab-musn/summary-event sid :psr selection)))
    (when-let [tau (get-in selection [:aif :tau])]
      (update-tau-cache! entry chosen tau))
    (persist! entry :turn/select req* resp)
    (cond-> resp
      psr (assoc :psr psr)
      (seq aif-summary) (assoc :aif aif-summary)
      deviation (assoc :deviation deviation)
      policy (assoc :aif/policy (select-keys policy [:chosen :tau :min-sample :mode :reason :sampled? :abstain?])))))

(defn turn-action!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        action {:pattern/id (:pattern/id req)
                :action (:action req)
                :note (:note req)
                :files (:files req)
                :source (or (:source req) :explicit)}
        resp (router/handle-turn-action! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :pattern/id (:pattern/id req)
                                                   :action (:action req)
                                                   :note (:note req)
                                                   :files (:files req)
                                                   :source (:source req)
                                                   :cost (:cost req)}))
        uncertainty-detail (pattern-uncertainty-details entry (:pattern/id req))
        belief-update (aif-engine/update-beliefs (:aif-engine entry)
                                                 {:decision/id (str sid ":action-" (java.util.UUID/randomUUID))
                                                  :session/id sid
                                                  :pattern/id (:pattern/id req)
                                                  :pattern/action (:action req)
                                                  :uncertainty (:value uncertainty-detail)
                                                  :status :observed})
        aif (:aif belief-update)]
    (tap-aif-context! {:event :action
                       :session/id sid
                       :turn (:turn req)
                       :pattern/id (:pattern/id req)
                       :pattern/action (:action req)
                       :uncertainty uncertainty-detail
                       :aif aif
                       :mu (aif-mu-summary entry (:pattern/id req))})
    (maybe-award-maturity-bonus! entry (:pattern/id req) (:files req) (:action req))
    (mark-mana-depleted! entry (:turn req))
    (when-let [tau (get-in belief-update [:aif :tau-updated])]
      (update-tau-cache! entry (:pattern/id req) tau))
    (when (:mana entry)
      (let [{:keys [action-cost restore]} (mana-config (:policy entry))
            cost (double (or action-cost 1))
            restore (double (or restore 10))
            pid (:pattern/id req)
            selection-id (get-in @(:state entry) [:selection :pattern/id])
            mana @(:mana entry)
            restore? (and pid
                          (= pid selection-id)
                          (not (restored? mana :psr->action pid)))]
        (apply-mana! entry (- cost) :action {:pattern/id pid
                                             :action (:action req)
                                             :turn (:turn req)})
        (when restore?
          (apply-mana! entry restore :psr->action {:pattern/id pid
                                                   :action (:action req)
                                                   :turn (:turn req)})
          (note-restore! entry :psr->action pid))))
    (append-lab-event! entry (pattern-action-event sid (:turn req) action))
    (persist! entry :turn/action req resp)
    (cond-> resp
      aif (assoc :aif aif)
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn turn-use!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        resp (router/handle-turn-use! (:state entry)
                                      (compact {:session/id sid
                                                :turn (:turn req)
                                                :pattern/id (:pattern/id req)
                                                :anchors (:anchors req)
                                                :note (:note req)
                                                :inferred? (:inferred? req)}))
        actions (:actions @(:state entry))
        anchors (vec (distinct (concat (:anchors req)
                                       (action-anchors actions (:pattern/id req)))))
        pur (fulab-musn/build-pur {:session-id sid
                                   :turn turn
                                   :pattern-id (:pattern/id req)
                                   :reason (:note req)
                                   :anchors anchors
                                   :inferred? (:inferred? req)
                                   :certificates (:certificates entry)})
        uncertainty-detail (pattern-uncertainty-details entry (:pattern/id req))
        belief-update (aif-engine/update-beliefs (:aif-engine entry)
                                                 {:decision/id (:decision/id pur)
                                                  :session/id sid
                                                  :pattern/id (:pattern/id req)
                                                  :outcome (:outcome/tags pur)
                                                  :uncertainty (:value uncertainty-detail)
                                                  :status :observed})]
    (tap-aif-context! {:event :use
                       :session/id sid
                       :turn turn
                       :pattern/id (:pattern/id req)
                       :outcome (:outcome/tags pur)
                       :uncertainty uncertainty-detail
                       :aif (:aif belief-update)
                       :mu (aif-mu-summary entry (:pattern/id req))})
    (mark-mana-depleted! entry turn)
    (when-let [tau (get-in belief-update [:aif :tau-updated])]
      (update-tau-cache! entry (:pattern/id req) tau))
    (when (:mana entry)
      (let [{:keys [restore]} (mana-config (:policy entry))
            restore (double (or restore 10))
            pid (:pattern/id req)
            mana @(:mana entry)
            action? (some #(= pid (:pattern/id %)) actions)
            restore? (and pid
                          action?
                          (not (restored? mana :action->pur pid)))]
        (when restore?
          (apply-mana! entry restore :action->pur {:pattern/id pid
                                                   :turn turn})
          (note-restore! entry :action->pur pid))))
    (append-lab-event! entry {:event/type :pattern/use-claimed
                              :at (fulab-musn/now-inst)
                              :payload {:pur pur}})
    (append-lab-event! entry (fulab-musn/summary-event sid :pur belief-update))
    (swap! (:fulab entry) assoc
           :pur pur
           :pur-emitted? true)
    (persist! entry :turn/use req resp)
    (cond-> (assoc resp :pur pur
                   :aif (:aif belief-update))
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn evidence-add!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        resp (router/handle-evidence-add! (:state entry)
                                          (compact {:session/id sid
                                                    :turn (:turn req)
                                                    :pattern/id (:pattern/id req)
                                                    :files (:files req)
                                                    :note (:note req)}))]
    (persist! entry :evidence/add req resp)
    resp))

(defn- turn-end-ready?
  [state]
  (or (:plan? state)
      (:selection state)
      (seq (:actions state))))

(defn- turn-summary
  [state]
  {:actions (frequencies (map :action (:actions state)))
   :trail (:trail state)
   :psr (get-in state [:selection :pattern/id])
   :pur (get-in state [:pur :pattern/id])
   :warnings (count (:warnings state))})

(def ^:private aif-auto-use-min-tau 0.55)

(defn- auto-use-policy [state fulab-state]
  (let [tau (or (get-in fulab-state [:psr :selection/reason :aif :tau])
                (get-in state [:hud :aif :tau]))
        deviation (:deviation fulab-state)
        unjustified-deviation? (and deviation (not (:justified? deviation)))
        tau-ok? (or (nil? tau) (>= (double tau) aif-auto-use-min-tau))
        allow? (and tau-ok? (not unjustified-deviation?))]
    (cond-> {:tau tau
             :mode (if allow? :auto-use :abstain)
             :reason (cond
                       unjustified-deviation? "unjustified-deviation"
                       (not tau-ok?) "aif-low-tau"
                       :else "aif-policy")}
      deviation (assoc :deviation deviation))))

(defn- auto-use-allowed? [policy]
  (and (map? policy)
       (not= :abstain (:mode policy))))

(defn turn-end!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        state @(:state entry)
        fulab-state @(:fulab entry)
        ended-turn (:turn-ended fulab-state)]
    (cond
      (= turn ended-turn)
      (let [resp (assoc (or (:turn-ended-resp fulab-state) {:ok true})
                        :duplicate? true)]
        (persist! entry :turn/end req resp)
        (cond-> resp
          (:mana entry) (assoc :mana (mana-view @(:mana entry)))))

      (not (turn-end-ready? state))
      (let [resp {:ok true
                  :ignored? true
                  :reason :turn/end-before-ready
                  :summary (turn-summary state)
                  :halt? false
                  :halt/reason nil}]
        (persist! entry :turn/end req resp)
        (cond-> resp
          (:mana entry) (assoc :mana (mana-view @(:mana entry)))))

      :else
      (let [resp (router/handle-turn-end! (:state entry)
                                          {:session/id sid
                                           :turn (:turn req)})
            fulab-state @(:fulab entry)
            psr (:psr fulab-state)
            pur (:pur fulab-state)
            actions (:actions @(:state entry))
            inferred-action (first (filter #(contains? #{"implement" "update"} (:action %)) actions))
            chosen (or (:chosen psr)
                       (:pattern/id inferred-action))
            inferred? (boolean (and chosen (not pur) inferred-action))
            policy (when inferred?
                     (assoc (auto-use-policy state fulab-state) :chosen chosen))
            allow-auto-use? (or (nil? policy) (auto-use-allowed? policy))
            resp (cond-> resp policy (assoc :aif/policy policy))]
        (append-lab-event! entry (turn-event :turn/completed sid turn {:session/id sid}))
        (when policy
          (append-lab-event! entry {:event/type :aif/policy
                                    :at (fulab-musn/now-inst)
                                    :payload (select-keys policy [:chosen :tau :mode :reason])}))
        (when policy
          (tap-aif-policy! {:session/id sid
                            :turn turn
                            :policy (select-keys policy [:chosen :tau :mode :reason])}))
        (when (and chosen (not pur) inferred? allow-auto-use?)
          (let [anchors (vec (distinct (concat (action-anchors actions chosen)
                                               [(fulab-musn/turn-anchor turn)])))
                pur (fulab-musn/build-pur {:session-id sid
                                           :turn turn
                                           :pattern-id chosen
                                           :reason "inferred from pattern action"
                                           :anchors anchors
                                           :inferred? true
                                           :certificates (:certificates entry)})
                belief-update (aif-engine/update-beliefs (:aif-engine entry)
                                                         {:decision/id (:decision/id pur)
                                                          :session/id sid
                                                          :outcome (:outcome/tags pur)
                                                          :status :observed})]
            (when (:mana entry)
              (let [{:keys [restore]} (mana-config (:policy entry))
                    restore (double (or restore 10))
                    mana @(:mana entry)
                    action? (some #(= chosen (:pattern/id %)) actions)
                    restore? (and action?
                                  (not (restored? mana :action->pur chosen)))]
                (when restore?
                  (apply-mana! entry restore :action->pur {:pattern/id chosen
                                                           :turn turn
                                                           :inferred? true})
                  (note-restore! entry :action->pur chosen))))
            (append-lab-event! entry {:event/type :pattern/use-claimed
                                      :at (fulab-musn/now-inst)
                                      :payload {:pur pur}})
            (append-lab-event! entry (fulab-musn/summary-event sid :pur belief-update))
            (swap! (:fulab entry) assoc
                   :pur pur
                   :pur-emitted? true)))
        (when (and chosen (not pur) inferred? (not allow-auto-use?))
          (append-lab-event! entry {:event/type :pattern/use-skipped
                                    :at (fulab-musn/now-inst)
                                    :payload {:session/id sid
                                              :turn turn
                                              :pattern/id chosen
                                              :reason "auto-use skipped (low tau)"}}))
        (when (and psr (not (:pur-emitted? fulab-state)) (not inferred?))
          (append-lab-event! entry {:event/type :pattern/selection-unapplied
                                    :at (fulab-musn/now-inst)
                                    :payload {:session/id sid
                                              :turn turn
                                              :decision/id (:decision/id psr)
                                              :pattern/id (:chosen psr)
                                              :note "No application recorded for selection."}}))
        (swap! (:fulab entry) assoc
               :turn nil
               :psr nil
               :pur nil
               :psr-emitted? false
               :pur-emitted? false
               :deviation nil
               :turn-ended turn
               :turn-ended-resp resp)
        (persist! entry :turn/end req resp)
        (cond-> resp
          (:mana entry) (assoc :mana (mana-view @(:mana entry))))))))

(defn turn-resume!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        resp (router/handle-turn-resume! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :note (:note req)}))]
    (persist! entry :turn/resume req resp)
    resp))

(defn session-state!
  "Return a snapshot of the current session state (no mutation)."
  [req]
  (let [sid (:session/id req)]
    (if-let [entry (get-session sid)]
      (let [st @(:state entry)]
        {:ok true
         :session/id sid
         :state (select-keys st [:turn :hud :selection :actions :trail :warnings :halt :plan? :resume-note])
         :logic/summary (logic-audit/summary-for sid)
         :aif/tap (when-let [tap (:aif-tap entry)]
                    @tap)
         :mana (when (:mana entry)
                 (mana-view @(:mana entry)))} )
      {:ok false :err "not-found"})))
