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

(defn- compact [m]
  (into {} (remove (comp nil? val)) m))

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
  [req]
  (let [sid (or (:session/id req) (gen-session-id))
        entry (ensure-session! sid (:policy req))]
    {:ok true
     :session/id sid
     :policy (:policy entry)}))

(defn turn-start!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-turn-start! (:state entry)
                                {:session/id sid
                                 :turn (:turn req)
                                 :hud (:hud req)})))

(defn turn-plan!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-turn-plan! (:state entry)
                              {:session/id sid
                               :turn (:turn req)
                               :plan (:plan req)})))

(defn turn-select!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-turn-select! (:state entry)
                                (compact {:session/id sid
                                          :turn (:turn req)
                                          :candidates (:candidates req)
                                          :chosen (:chosen req)
                                          :reason (:reason req)
                                          :anchors (:anchors req)}))))

(defn turn-action!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-turn-action! (:state entry)
                                (compact {:session/id sid
                                          :turn (:turn req)
                                          :pattern/id (:pattern/id req)
                                          :action (:action req)
                                          :note (:note req)
                                          :files (:files req)
                                          :source (:source req)
                                          :cost (:cost req)}))))

(defn turn-use!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-turn-use! (:state entry)
                             (compact {:session/id sid
                                       :turn (:turn req)
                                       :pattern/id (:pattern/id req)
                                       :anchors (:anchors req)
                                       :note (:note req)
                                       :inferred? (:inferred? req)}))))

(defn evidence-add!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-evidence-add! (:state entry)
                                 (compact {:session/id sid
                                           :turn (:turn req)
                                           :pattern/id (:pattern/id req)
                                           :files (:files req)
                                           :note (:note req)}))))

(defn turn-end!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil)]
    (router/handle-turn-end! (:state entry)
                             {:session/id sid
                              :turn (:turn req)})))
