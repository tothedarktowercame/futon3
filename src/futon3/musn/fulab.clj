;;; futon3.musn.fulab --- Fulab PSR/PUR helpers for MUSN -*- lexical-binding: t; -*-

(ns futon3.musn.fulab
  "Helpers to build PSR/PUR records and anchors for MUSN sessions."
  (:require [clojure.string :as str]))

(defn now-inst []
  (java.util.Date.))

(defn turn-anchor [turn]
  {:anchor/type :turn/completed
   :anchor/ref {:event/type :turn/completed
                :turn turn}})

(defn pattern-action-anchor [pattern-id action]
  {:anchor/type :pattern/action
   :anchor/ref {:event/type :pattern/action
                :pattern/id pattern-id
                :pattern/action action}})

(defn build-forecast [turn]
  {:benefits [{:tag :benefit/traceable-choice
               :locus (turn-anchor turn)
               :note "Decision captured at turn boundary."}]
   :risks [{:tag :risk/mismatch
            :locus (turn-anchor turn)
            :note "Pattern may not match actual change."}]
   :success [{:tag :success/traceable-outcome
              :locus (turn-anchor turn)
              :note "Outcome tied to decision."}]
   :failure [{:tag :failure/unresolved
              :locus (turn-anchor turn)
              :note "Decision does not explain outcome."}]})

(defn- normalize-anchors [anchors turn]
  (let [anchors (vec (remove nil? (or anchors [])))]
    (vec (distinct (conj anchors (turn-anchor turn))))))

(defn- build-rejections [candidates chosen]
  (into {}
        (for [p candidates :when (not= p chosen)]
          [p {:codes [:reject/fit]
              :note "Alternate pattern not selected."}])))

(defn- solo-override [candidates]
  (when (and (coll? candidates) (< (count candidates) 2))
    {:override/solo? true
     :override/note "Only one candidate available for this turn."}))

(defn build-psr
  [{:keys [session-id turn candidates chosen reason anchors certificates]}]
  (let [candidates (vec (or candidates []))
        chosen (or chosen "")
        base {:psr/id (str "psr-" turn)
              :session/id session-id
              :decision/id (str session-id ":turn-" turn)
              :candidates candidates
              :chosen chosen
              :context/anchors (normalize-anchors anchors turn)
              :forecast (build-forecast turn)
              :rejections (build-rejections candidates chosen)
              :horizon :immediate
              :certificates (vec (or certificates []))}]
    (cond-> base
      reason (assoc :selection/reason reason)
      (solo-override candidates) (merge (solo-override candidates)))))

(defn build-pur
  [{:keys [session-id turn pattern-id reason anchors inferred? certificates]}]
  (let [uid (str (java.util.UUID/randomUUID))
        tags (cond-> [:outcome/partial]
               inferred? (conj :outcome/inferred))]
    {:pur/id (str "pur-" turn "-" uid)
     :session/id session-id
     :pattern/id pattern-id
     :instance/id (str "pur-" turn "-" uid)
     :decision/id (str session-id ":turn-" turn)
     :fields {:context "Captured at turn completion."
              :if "A decision was needed."
              :however "Fit only known after the turn."
              :then "Recorded the observed outcome."
              :because "Need traceability for decisions."
              :next-steps "Compare decision vs outcome."}
     :anchors (normalize-anchors anchors turn)
     :outcome/tags (vec (distinct tags))
     :use/reason reason
     :certificates (vec (or certificates []))}))

(defn summary-event [session-id kind result]
  {:event/type :aif/summary
   :at (now-inst)
   :payload {:session/id session-id
             :aif/kind kind
             :aif/result result}})
