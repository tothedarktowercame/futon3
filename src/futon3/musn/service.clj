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
            [futon3.musn.fulab :as fulab-musn]
            [futon3.musn.router :as router]))

(def default-policy
  {:off-trail {:free 3 :ratio 0.5 :action "off-trail"}
   :trail-allow {:patterns [] :namespaces []}
   :aif-config {}
   :certificates []})

(def default-lab-root
  (or (System/getenv "MUSN_LAB_ROOT") "lab"))

(defonce sessions (atom {}))

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
   :aif-config-logged? false})

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
                 :aif-engine engine
                 :aif-config (:aif-config p)
                 :certificates (:certificates p)
                 :client-id client-id}]
      (swap! sessions assoc sid entry)
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
        scores (get-in req [:hud :scores])
        turn (:turn req)
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
    (append-lab-event! entry (turn-event :turn/started sid turn {:session/id sid}))
    (persist! entry :turn/start req resp)
    (cond-> resp
      selection (assoc :aif (:aif selection)))))

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
        candidates (:candidates req)
        chosen (:chosen req)
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
        psr (when (psr-eligible? candidates chosen)
              (fulab-musn/build-psr {:session-id sid
                                     :turn turn
                                     :candidates candidates
                                     :chosen chosen
                                     :reason reason
                                     :anchors (:anchors req)
                                     :certificates (:certificates entry)}))]
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
      psr (assoc :psr psr))))

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
                  :source (or (:source req) :explicit)}]
      (append-lab-event! entry (pattern-action-event sid (:turn req) action))
      (aif-engine/update-beliefs (:aif-engine entry)
                                 {:decision/id (str sid ":action-" (java.util.UUID/randomUUID))
                                  :session/id sid
                                  :pattern/id (:pattern/id req)
                                  :pattern/action (:action req)
                                  :status :observed}))
    (persist! entry :turn/action req resp)
    resp))

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
      (append-lab-event! entry {:event/type :pattern/use-claimed
                                :at (fulab-musn/now-inst)
                                :payload {:pur pur}})
      (append-lab-event! entry (fulab-musn/summary-event sid :pur belief-update))
      (swap! (:fulab entry) assoc
             :pur pur
             :pur-emitted? true)
      (persist! entry :turn/use req resp)
      (assoc resp :pur pur
             :aif (:aif belief-update)))))

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

(defn turn-end!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        resp (router/handle-turn-end! (:state entry)
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
             :pur-emitted? false))
    (persist! entry :turn/end req resp)
    resp))

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
    (if-let [st (current-state sid)]
      {:ok true
       :session/id sid
       :state (select-keys st [:turn :hud :selection :actions :trail :warnings :halt :plan? :resume-note])}
      {:ok false :err "not-found"})))
