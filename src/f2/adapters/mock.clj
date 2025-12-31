(ns f2.adapters.mock
  "Simple in-memory adapters for F1/F3 so router tests can run without external services."
  (:require [f0.clock :as clock])
  (:import (java.util UUID)))

(defn- random-id [prefix]
  (str prefix "-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn mock-f1 []
  (let [events (atom [])]
    {:put-event! (fn [_run-id event]
                   (let [eid (or (:id event) (random-id "E"))]
                     (swap! events conj (assoc event :eid eid))
                     {:eid eid}))
     :close-session! (fn [_ sid]
                       {:sid sid})
     :export-scenario! (fn [_ sid]
                         {:scenario-path (format "scenarios/%s.edn" sid)})}))

(defn mock-f3 []
  (let [jobs (atom {})]
    {:run-scenario! (fn [_ scenario-path policy]
                      (let [job-id (random-id "JOB")]
                        (swap! jobs assoc job-id {:path scenario-path
                                                  :policy policy
                                                  :state :pending
                                                  :started (clock/->iso-string)})
                        {:job-id job-id}))
     :job-status (fn [_ job-id]
                   (if-let [state (get @jobs job-id)]
                     (let [completed? (= :done (:state state))
                           now (clock/->iso-string)
                           next (cond-> state
                                   (not completed?) (assoc :state :done
                                                          :completed now))]
                       (swap! jobs assoc job-id next)
                       {:state (:state next)
                        :metrics {:job-id job-id
                                  :path (:path next)
                                  :started (:started next)
                                  :completed (:completed next)}})
                     {:state :unknown}))}))

(defn adapters
  "Return {:f1 .. :f3 ..} mocks."
  []
  {:f1 (mock-f1)
   :f3 (mock-f3)})
