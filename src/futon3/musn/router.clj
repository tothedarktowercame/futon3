(ns futon3.musn.router
  "In-memory MUSN state machine: validates requests against schemas, enforces core constraints,
   and emits pause/backtrace payloads. This is a pure-Clojure skeleton; wiring to HTTP/Drawbridge
   can wrap these functions."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [futon3.logic-audit :as logic-audit]
            [futon3.musn.logic :as musn-logic]
            [futon3.musn.schema :as schema]))

(declare pause-payload)

;; ------------------------------------------------------------------------------
;; Utilities

(defn now [] (java.time.Instant/now))

(defn validate [spec value]
  (when-not (m/validate spec value)
    (throw (ex-info "invalid request" {:spec spec :value value})))
  value)

(defn normalize-pattern-set [patterns namespaces]
  {:patterns (set patterns)
   :namespaces (set namespaces)})

(defn allowed-pattern? [allowed pattern-id]
  (let [namespace (some-> pattern-id (str/split #"/") first)]
    (or (contains? (:patterns allowed) pattern-id)
        (contains? (:namespaces allowed) namespace))))

(defn bump-trail [{:keys [trail] :as state} on-trail?]
  (let [{:keys [on off]} trail
        on (or on 0)
        off (or off 0)]
    (assoc state :trail {:on (if on-trail? (inc on) on)
                         :off (if on-trail? off (inc off))
                         :limit (:limit trail)})))

(defn off-trail-limit [{:keys [trail off-trail-policy]}]
  (let [{:keys [on]} trail
        {:keys [free ratio]} off-trail-policy]
    (+ (double free) (* (double ratio) (double (or on 0))))))

(defn trail-exceeded? [state]
  (let [{:keys [off]} (:trail state)]
    (> (double (or off 0)) (double (off-trail-limit state)))))

(defn record-action [state action]
  (update state :actions (fnil conj []) action))

(defn recent-moves [state n]
  (take-last n (:actions state)))

(defn note-warning [state type msg]
  (update state :warnings (fnil conj []) {:type type :msg msg :at (now)}))

;; ------------------------------------------------------------------------------
;; Constraints (core.logic kernel + routing to warnings/halts)

(defn note-write [state action]
  (if (#{"implement" "update"} (:action action))
    (assoc state :write? true)
    state))

(defn- apply-obligation [state {:keys [obligation]}]
  (case obligation
    :missing-plan
    (let [state (note-warning state :missing-plan "plan is required before tool use")]
      (if (get-in state [:plan-policy :enforce?])
        (assoc state :halt {:type :missing-plan
                            :note "plan is required before tool use"})
        state))
    :missing-selection
    (-> state
        (note-warning :missing-selection "implement/update requires selection")
        (assoc :halt {:type :missing-selection
                      :note "implement/update requires selection"}))
    :missing-consent
    (-> state
        (note-warning :missing-consent "expensive move without consent/plan")
        (assoc :halt {:type :missing-consent
                      :note "expensive move without consent/plan"}))
    :off-trail
    (note-warning state :off-trail "off-trail budget exceeded")
    :no-write
    (assoc state :halt {:type :no-write
                        :note "use-mode selection ended with no writes/evidence"})
    state))

(defn- apply-obligations [state obligations]
  (reduce apply-obligation state obligations))

(defn apply-action-constraints [state action]
  (let [logic-result (musn-logic/check-action state action
                                              {:off-trail? (trail-exceeded? state)
                                               :trail (:trail state)})
        obligations (get-in logic-result [:logic :obligations])]
    (-> state
        (apply-obligations obligations)
        (assoc :last-logic logic-result))))

(defn apply-turn-end-constraints [state]
  (let [logic-result (musn-logic/check-turn-end state)
        obligations (get-in logic-result [:logic :obligations])]
    (-> state
        (apply-obligations obligations)
        (assoc :last-logic logic-result))))

;; ------------------------------------------------------------------------------
;; Public API (stateful atom passed in)

(defn new-session [policy]
  {:turn 0
   :plan? false
   :selection nil
   :actions []
   :hud nil
   :trail {:on 0 :off 0 :limit (get-in policy [:off-trail :free] 0.0)}
   :off-trail-policy {:free (get-in policy [:off-trail :free] 0.0)
                      :ratio (get-in policy [:off-trail :ratio] 0.5)}
   :plan-policy (merge {:enforce? false} (:plan policy))
   :warnings []
   :halt nil
   :halt? false
   :write? false
   :last-logic nil})

(defn handle-turn-start! [state-atom req]
  (validate schema/TurnStartReq req)
  (swap! state-atom (fn [st]
                      (-> st
                          (assoc :turn (:turn req)
                                 :plan? false
                                 :selection nil
                                 :actions []
                                 :hud (:hud req)
                                 :warnings []
                                 :halt nil
                                 :write? false
                                 :last-logic nil
                                 :allowed (normalize-pattern-set (get-in req [:hud :candidates] [])
                                                                 (get-in req [:hud :namespaces] [])))
                          (update :trail #(assoc % :limit (off-trail-limit {:trail % :off-trail-policy (:off-trail-policy st)}))))))
  {:ok true
   :turn (:turn @state-atom)
   :trail (:trail @state-atom)})

(defn handle-turn-plan! [state-atom req]
  (validate schema/TurnPlanReq req)
  (swap! state-atom assoc :plan? true :plan (:plan req))
  {:ok true :plan/accepted true})

(defn handle-turn-select! [state-atom req]
  (validate schema/TurnSelectReq req)
  (let [chosen (:chosen req)
        decision-id (str (get req :session/id) ":turn-" (:turn req))]
    (if (string? chosen)
      (let [psr {:decision/id decision-id
                 :pattern/id chosen
                 :candidates (:candidates req)
                 :selection/reason (:reason req)
                 :selection/anchors (:anchors req)}]
        (swap! state-atom assoc :selection psr)
        {:ok true :psr psr})
      {:ok true})))

(defn handle-turn-action! [state-atom req]
  (validate schema/TurnActionReq req)
  (let [action {:pattern/id (:pattern/id req)
                :action (:action req)
                :files (:files req)
                :note (:note req)
                :cost (:cost req)
                :at (now)
                :on-trail (allowed-pattern? (:allowed @state-atom) (:pattern/id req))}]
    (swap! state-atom
           (fn [st]
             (let [next-state (-> st
                                  (bump-trail (:on-trail action))
                                  (record-action action)
                                  (note-write action))]
               (apply-action-constraints next-state action))))
    (let [st @state-atom]
      (when-let [logic (:last-logic st)]
        (logic-audit/record! {:scope :musn
                              :session/id (:session/id req)
                              :run/id (:session/id req)
                              :op :turn/action
                              :result logic}))
      {:ok (not (:halt st))
       :trail (:trail st)
       :warning (when (:halt st) {:type (:type (:halt st)) :msg (:note (:halt st))})
       :halt? (boolean (:halt st))
       :halt/reason (:halt st)
       :logic (get-in st [:last-logic :logic])
       :pause (pause-payload st)})))

(defn handle-turn-use! [state-atom req]
  (validate schema/TurnUseReq req)
  (let [pur {:pattern/id (:pattern/id req)
             :use/reason (:note req)
             :outcome/tags (cond-> []
                             (:inferred? req) (conj :outcome/inferred))}]
    (swap! state-atom assoc :pur pur)
    {:ok true :pur pur}))

(defn handle-evidence-add! [state-atom req]
  (validate schema/EvidenceAddReq req)
  (swap! state-atom assoc :write? true)
  {:ok true :record/id (str "ev-" (java.util.UUID/randomUUID))})

(defn pause-payload [state]
  (when-let [h (:halt state)]
    {:event/type :turn/pause
     :reason h
     :trail (:trail state)
     :moves (recent-moves state 5)
     :resume/hint "Provide clarification and call turn/resume with note."}))

(defn handle-turn-end! [state-atom req]
  (validate schema/TurnEndReq req)
  (swap! state-atom apply-turn-end-constraints)
  (let [st @state-atom
        halt (:halt st)]
    (when-let [logic (:last-logic st)]
      (logic-audit/record! {:scope :musn
                            :session/id (:session/id req)
                            :run/id (:session/id req)
                            :op :turn/end
                            :result logic}))
    {:ok (not halt)
     :summary {:actions (frequencies (map :action (:actions st)))
               :trail (:trail st)
               :psr (get-in st [:selection :pattern/id])
               :pur (get-in st [:pur :pattern/id])
               :warnings (count (:warnings st))}
     :halt? (boolean halt)
     :halt/reason halt
     :logic (get-in st [:last-logic :logic])
     :pause (pause-payload st)}))

(defn handle-turn-resume! [state-atom req]
  (validate schema/TurnResumeReq req)
  (swap! state-atom assoc
         :halt nil
         :halt? false
         :halt-reason nil
         :resume-note (:note req))
  {:ok true :turn (:turn req)})
