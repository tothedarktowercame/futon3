(ns futon3.musn.service
  "Session registry and thin wrappers around the MUSN router. Intended to be wrapped by HTTP/Drawbridge handlers.

   This module also handles light persistence (append-only log + snapshot) so MUSN sessions can survive crashes
   and be replayed or resumed. Logs are EDN lines under lab/musn/, keyed by session id."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [futon3.musn.router :as router]))

(def default-policy
  {:off-trail {:free 3 :ratio 0.5 :action "off-trail"}
   :trail-allow {:patterns [] :namespaces []}
   :aif-config {}})

(def default-lab-root
  (or (System/getenv "MUSN_LAB_ROOT") "lab"))

(defonce sessions (atom {}))

(defn- gen-session-id []
  (str "musn-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- compact [m]
  (into {} (remove (comp nil? val)) m))

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

(defn ensure-session! [sid policy lab-root]
  (if-let [existing (get @sessions sid)]
    existing
    (let [p (merge default-policy (or policy {}))
          lab-root (or lab-root default-lab-root)
          snap (read-snapshot (snapshot-path lab-root sid))
          st (atom (or snap (router/new-session p)))
          entry {:state st
                 :policy p
                 :lab-root lab-root
                 :log-file (log-path lab-root sid)
                 :snapshot-file (snapshot-path lab-root sid)
                 :id sid}]
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
        entry (ensure-session! sid (:policy req) (:lab/root req))]
    (persist! entry :session/create req {:ok true :session/id sid :policy (:policy entry)})
    {:ok true
     :session/id sid
     :policy (:policy entry)}))

(defn turn-start!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-start! (:state entry)
                                        {:session/id sid
                                         :turn (:turn req)
                                         :hud (:hud req)})]
    (persist! entry :turn/start req resp)
    resp))

(defn turn-plan!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-plan! (:state entry)
                                       {:session/id sid
                                        :turn (:turn req)
                                        :plan (:plan req)})]
    (persist! entry :turn/plan req resp)
    resp))

(defn turn-select!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-select! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :candidates (:candidates req)
                                                   :chosen (:chosen req)
                                                   :reason (:reason req)
                                                   :anchors (:anchors req)}))]
    (persist! entry :turn/select req resp)
    resp))

(defn turn-action!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-action! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :pattern/id (:pattern/id req)
                                                   :action (:action req)
                                                   :note (:note req)
                                                   :files (:files req)
                                                   :source (:source req)
                                                   :cost (:cost req)}))]
    (persist! entry :turn/action req resp)
    resp))

(defn turn-use!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-use! (:state entry)
                                      (compact {:session/id sid
                                                :turn (:turn req)
                                                :pattern/id (:pattern/id req)
                                                :anchors (:anchors req)
                                                :note (:note req)
                                                :inferred? (:inferred? req)}))]
    (persist! entry :turn/use req resp)
    resp))

(defn evidence-add!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
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
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-end! (:state entry)
                                      {:session/id sid
                                       :turn (:turn req)})]
    (persist! entry :turn/end req resp)
    resp))

(defn turn-resume!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req))
        resp (router/handle-turn-resume! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :note (:note req)}))]
    (persist! entry :turn/resume req resp)
    resp))
