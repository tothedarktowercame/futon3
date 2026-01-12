(ns futon3.musn.service
  "Session registry and thin wrappers around the MUSN router. Intended to be wrapped by HTTP/Drawbridge handlers."
  (:require [clojure.string :as str]
            [futon3.musn.router :as router]))

(def default-policy
  {:off-trail {:free 3 :ratio 0.5 :action "off-trail"}
   :trail-allow {:patterns [] :namespaces []}
   :aif-config {}})

(defonce sessions (atom {}))

(defn- gen-session-id []
  (str "musn-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn session-ids []
  (keys @sessions))

(defn get-session [sid]
  (get @sessions sid))

(defn ensure-session! [sid policy]
  (if-let [existing (get @sessions sid)]
    existing
    (let [p (merge default-policy (or policy {}))
          st (atom (router/new-session p))
          entry {:state st :policy p}]
      (swap! sessions assoc sid entry)
      entry)))

(defn create-session!
  [{:keys [session/id policy]}]
  (let [sid (or id (gen-session-id))
        entry (ensure-session! sid policy)]
    {:ok true :session/id sid :policy (:policy entry)}))

(defn turn-start!
  [{:keys [session/id turn hud]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-turn-start! (:state entry) {:session/id id :turn turn :hud hud})))

(defn turn-plan!
  [{:keys [session/id turn plan]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-turn-plan! (:state entry) {:session/id id :turn turn :plan plan})))

(defn turn-select!
  [{:keys [session/id turn candidates chosen reason anchors]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-turn-select! (:state entry)
                                {:session/id id
                                 :turn turn
                                 :candidates candidates
                                 :chosen chosen
                                 :reason reason
                                 :anchors anchors})))

(defn turn-action!
  [{:keys [session/id turn pattern/id action note files source cost]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-turn-action! (:state entry)
                                {:session/id id
                                 :turn turn
                                 :pattern/id pattern/id
                                 :action action
                                 :note note
                                 :files files
                                 :source source
                                 :cost cost})))

(defn turn-use!
  [{:keys [session/id turn pattern/id anchors note inferred?]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-turn-use! (:state entry)
                             {:session/id id
                              :turn turn
                              :pattern/id pattern/id
                              :anchors anchors
                              :note note
                              :inferred? inferred?})))

(defn evidence-add!
  [{:keys [session/id turn pattern/id files note]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-evidence-add! (:state entry)
                                 {:session/id id
                                  :turn turn
                                  :pattern/id pattern/id
                                  :files files
                                  :note note})))

(defn turn-end!
  [{:keys [session/id turn]}]
  (let [entry (ensure-session! id nil)]
    (router/handle-turn-end! (:state entry)
                             {:session/id id
                              :turn turn})))
