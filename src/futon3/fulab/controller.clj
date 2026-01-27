(ns futon3.fulab.controller
  "Agent controller protocol + baseline/diagram implementations for fulab."
  (:require [futon3.fulab.wiring :as wiring]))

(defprotocol AgentController
  (init [controller env]
    "Initialize controller state from environment.")
  (step [controller state obs]
    "Advance one step. Returns {:action ... :state ... :telemetry ...}.")
  (done? [controller state]
    "Return true when controller considers the episode finished."))

(defn- baseline-action
  [phase state obs]
  (case phase
    :sense {:type :observe :note "baseline/sense"}
    :decide {:type :plan
             :plan-id (or (:plan-id state) :baseline/plan)
             :note "baseline/decide"}
    :act (if (:hint obs)
           {:type :tool :tool :hint :note "baseline/use-hint"}
           {:type :act :note "baseline/act"})
    :reflect {:type :reflect :note "baseline/reflect"}
    :stop {:type :stop}
    {:type :act :note "baseline/act"}))

(defn- baseline-next-phase
  [phase obs]
  (cond
    (:done? obs) :stop
    (= phase :stop) :stop
    (= phase :sense) :decide
    (= phase :decide) :act
    (= phase :act) (if (> (double (or (:uncertainty obs) 0.0)) 0.55)
                     :sense
                     :reflect)
    (= phase :reflect) :sense
    :else :sense))

(defn- baseline-telemetry
  [state phase next-phase action obs env]
  (let [step (inc (or (:step state) 0))]
    {:ts (wiring/step-timestamp state step)
     :step step
     :node/id (keyword "baseline" (name phase))
     :node/type phase
     :edge/selected {:from phase :to next-phase :label "baseline"}
     :action action
     :tool (:tool action)
     :obs (select-keys obs [:budget :uncertainty :hint :done? :success?])
     :env (select-keys env [:budget :hints])}))

(defrecord BaselineController [policy]
  AgentController
  (init [_ env]
    (let [seed (:seed env)
          base-ms (when seed (* 1000 (long seed)))]
      {:phase :sense
       :step 0
       :seed seed
       :timestamp-base-ms base-ms
       :plan-id (or (:plan-id policy) :baseline/plan)}))
  (step [_ state obs]
    (let [phase (or (:phase state) :sense)
          env (:env obs)
          action (baseline-action phase state obs)
          next-phase (baseline-next-phase phase obs)
          step (inc (or (:step state) 0))
          state* (assoc state
                        :phase next-phase
                        :step step
                        :last/node phase
                        :last/action action
                        :done? (= next-phase :stop))
          telemetry (baseline-telemetry state phase next-phase action obs env)]
      {:action action
       :state state*
       :telemetry telemetry}))
  (done? [_ state]
    (true? (:done? state))))

(defrecord DiagramController [diagram]
  AgentController
  (init [_ env]
    (let [seed (:seed env)
          base-ms (when seed (* 1000 (long seed)))]
      {:node-id (:start diagram)
       :step 0
       :seed seed
       :timestamp-base-ms base-ms
       :plan-id (or (:plan-id diagram) :diagram/plan)}))
  (step [_ state obs]
    (let [env-state (:env obs)
          {:keys [action new-state telemetry]} (wiring/interpret-step diagram state env-state obs)
          done? (or (:done? new-state) (:done? obs))
          state* (assoc new-state :done? done?)]
      {:action action
       :state state*
       :telemetry telemetry}))
  (done? [_ state]
    (true? (:done? state))))

(defn baseline-controller
  ([] (->BaselineController {}))
  ([policy] (->BaselineController policy)))

(defn diagram-controller
  [diagram]
  (->DiagramController diagram))
