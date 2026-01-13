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
            [futon3.musn.router :as router]))

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
(declare get-session append-lab-event!)
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
   :psr nil
   :pur nil
   :psr-emitted? false
   :pur-emitted? false
   :turn-ended nil
   :turn-ended-resp nil
   :aif-config-logged? false})

(defn- mana-config [policy]
  (merge {:start 100 :action-cost 1 :restore 10}
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
                :action->pur #{}}
     :last-change nil}))

(defn- reset-mana-turn! [entry turn]
  (swap! (:mana entry)
         (fn [mana]
           (-> mana
               (assoc :turn turn)
               (assoc :restores {:psr->action #{}
                                 :action->pur #{}})))))

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

(defn- restored? [mana kind pattern-id]
  (contains? (get-in mana [:restores kind] #{}) pattern-id))

(defn- note-restore! [entry kind pattern-id]
  (swap! (:mana entry)
         (fn [mana]
           (update-in mana [:restores kind] (fnil conj #{}) pattern-id))))

(defn- reset-fulab-state! [entry turn candidates scores]
  (reset! (:fulab entry)
          (assoc (new-fulab-state)
                 :turn turn
                 :candidates (vec (or candidates []))
                 :candidate-scores (or scores {}))))

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

(defn- compact [m]
  (into {} (remove (comp nil? val)) m))

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
        scores (normalize-score-keys (get-in req [:hud :scores]))
        turn (:turn req)
        scores (when (seq candidate-ids)
                 (if (map? scores)
                   (select-keys scores candidate-ids)
                   scores))
        selection (when (seq candidate-ids)
                    (aif-engine/select-pattern
                     (:aif-engine entry)
                     {:decision/id (str sid ":turn-" turn)
                      :session/id sid
                      :candidates candidate-ids
                      :candidate-scores scores
                      :anchors [(fulab-musn/turn-anchor turn)]
                      :forecast (fulab-musn/build-forecast turn)}))
        resp (router/handle-turn-start! (:state entry)
                                        {:session/id sid
                                         :turn (:turn req)
                                         :hud (:hud req)})]
    (reset-fulab-state! entry turn candidate-ids scores)
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
        chosen (candidate-id (:chosen req))
        selection (aif-engine/select-pattern
                   (:aif-engine entry)
                   {:decision/id (str sid ":turn-" turn)
                    :session/id sid
                    :candidates candidates
                    :candidate-scores (get-in @(:fulab entry) [:candidate-scores])
                    :anchors [(fulab-musn/turn-anchor turn)]
                    :forecast (fulab-musn/build-forecast turn)
                    :chosen chosen})
        reason (enrich-reason (:reason req) selection)
        resp (router/handle-turn-select! (:state entry)
                                         (compact {:session/id sid
                                                   :turn turn
                                                   :candidates candidates
                                                   :chosen chosen
                                                   :reason reason
                                                   :anchors (:anchors req)}))
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
                (seq g-terms) (assoc :aif/g-terms g-terms)))]
    (when psr
      (append-lab-event! entry {:event/type :pattern/selection-claimed
                                :at (fulab-musn/now-inst)
                                :payload {:psr psr}})
      (swap! (:fulab entry) assoc
             :psr psr
             :psr-emitted? true))
    (when selection
      (append-lab-event! entry (fulab-musn/summary-event sid :psr selection)))
    (persist! entry :turn/select req resp)
    (cond-> resp
      psr (assoc :psr psr)
      (seq aif-summary) (assoc :aif aif-summary))))

(defn turn-action!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        resp (router/handle-turn-action! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :pattern/id (:pattern/id req)
                                                   :action (:action req)
                                                   :note (:note req)
                                                   :files (:files req)
                                                   :source (:source req)
                                                   :cost (:cost req)}))]
    (let [action {:pattern/id (:pattern/id req)
                  :action (:action req)
                  :note (:note req)
                  :files (:files req)
                  :source (or (:source req) :explicit)}
          belief-update (aif-engine/update-beliefs (:aif-engine entry)
                                                   {:decision/id (str sid ":action-" (java.util.UUID/randomUUID))
                                                    :session/id sid
                                                    :pattern/id (:pattern/id req)
                                                    :pattern/action (:action req)
                                                    :status :observed})
          aif (:aif belief-update)]
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
        (:mana entry) (assoc :mana (mana-view @(:mana entry)))))))

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
                                                :inferred? (:inferred? req)}))]
    (let [actions (:actions @(:state entry))
          anchors (vec (distinct (concat (:anchors req)
                                         (action-anchors actions (:pattern/id req)))))
          pur (fulab-musn/build-pur {:session-id sid
                                     :turn turn
                                     :pattern-id (:pattern/id req)
                                     :reason (:note req)
                                     :anchors anchors
                                     :inferred? (:inferred? req)
                                     :certificates (:certificates entry)})
          belief-update (aif-engine/update-beliefs (:aif-engine entry)
                                                   {:decision/id (:decision/id pur)
                                                    :session/id sid
                                                    :outcome (:outcome/tags pur)
                                                    :status :observed})]
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
        (:mana entry) (assoc :mana (mana-view @(:mana entry)))))))

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
                                           :turn (:turn req)})]
        (append-lab-event! entry (turn-event :turn/completed sid turn {:session/id sid}))
        (let [fulab-state @(:fulab entry)
              psr (:psr fulab-state)
              pur (:pur fulab-state)
              actions (:actions @(:state entry))
              inferred-action (first (filter #(contains? #{"implement" "update"} (:action %)) actions))
              chosen (or (:chosen psr)
                         (:pattern/id inferred-action))
              inferred? (boolean (and chosen (not pur) inferred-action))]
          (when (and chosen (not pur) inferred?)
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
                 :turn-ended turn
                 :turn-ended-resp resp))
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
