(ns futon3.fulab.wiring
  "Wiring diagram representation + interpreter for fulab controllers."
  (:require [clojure.string :as str]))

(defn- kwize [v]
  (cond
    (keyword? v) v
    (string? v) (keyword v)
    :else v))

(defn node-by-id [diagram node-id]
  (some #(when (= node-id (:id %)) %) (:nodes diagram)))

(defn edges-from [diagram node-id]
  (filter #(= node-id (:from %)) (:edges diagram)))

(defn- guard-type [guard]
  (kwize (:type guard)))

(defn- guard-value [guard]
  (:value guard))

(defn guard-ok?
  "Return true when guard allows traversal.
   Supports :always, :budget-gt, :budget-lte, :uncertainty-gt, :uncertainty-lt,
   :last-action, :last-node, :last-tool-ok?, :has-hint?, :done?, :success?."
  [guard {:keys [agent env obs action]}]
  (let [gtype (guard-type guard)]
    (case gtype
      nil true
      :always true
      :budget-gt (> (double (or (:budget env) 0.0))
                   (double (or (guard-value guard) 0.0)))
      :budget-lte (<= (double (or (:budget env) 0.0))
                     (double (or (guard-value guard) 0.0)))
      :uncertainty-gt (> (double (or (:uncertainty obs) 0.0))
                         (double (or (guard-value guard) 0.0)))
      :uncertainty-lt (< (double (or (:uncertainty obs) 0.0))
                         (double (or (guard-value guard) 0.0)))
      :last-action (= (kwize (get-in agent [:last/action :type]))
                      (kwize (guard-value guard)))
      :last-node (= (kwize (get-in agent [:last/node]))
                    (kwize (guard-value guard)))
      :last-tool-ok? (boolean (get-in agent [:last/action :ok?]))
      :has-hint? (boolean (:hint obs))
      :done? (if (contains? guard :value)
               (= (boolean (:done? obs)) (boolean (guard-value guard)))
               (boolean (:done? obs)))
      :success? (if (contains? guard :value)
                  (= (boolean (:success? obs)) (boolean (guard-value guard)))
                  (boolean (:success? obs)))
      false)))

(defn- select-edge [diagram node-id ctx]
  (some #(when (guard-ok? (:guard %) ctx) %) (edges-from diagram node-id)))

(defn- node-type [node]
  (kwize (or (:type node) (:node/type node))))

(defn- node-params [node]
  (or (:params node) {}))

(defn node-action
  "Map node to an action map.
   Params may include :action to override the default mapping.
   :tool in params yields a :tool action.
   :note and :cmd are carried through when present."
  [node ctx]
  (let [params (node-params node)
        forced (:action params)
        ntype (node-type node)
        note (:note params)
        cmd (:cmd params)
        tool (:tool params)]
    (cond
      (map? forced) forced
      (keyword? forced) {:type forced :note note}
      (= ntype :stop) {:type :stop}
      (= ntype :sense) {:type :observe :note note}
      (= ntype :infer) {:type :infer :note note}
      (= ntype :decide) {:type :plan :plan-id (or (:plan-id params) (get-in ctx [:agent :plan-id]))
                         :note note}
      (= ntype :reflect) {:type :reflect :note note}
      (= ntype :act) (cond
                       tool {:type :tool :tool tool :note note}
                       cmd {:type :command :cmd cmd :note note}
                       :else {:type :act :note note})
      (= ntype :tool) {:type :tool :tool tool :note note}
      (= ntype :command) {:type :command :cmd cmd :note note}
      (= ntype :file-edit) {:type :file-edit :note note}
      :else {:type :act :note note})))

(defn step-timestamp
  [agent step]
  (let [base (or (:timestamp-base-ms agent)
                 (when-let [seed (:seed agent)]
                   (* 1000 (long seed)))
                 (System/currentTimeMillis))]
    (java.time.Instant/ofEpochMilli (+ (long base) (long step)))))

(defn interpret-step
  "Execute a single wiring step.
   Returns {:action .. :new-state .. :telemetry ..}."
  [diagram agent-state env-state obs]
  (let [node-id (or (:node-id agent-state)
                    (:start diagram)
                    (some-> diagram :nodes first :id))
        node (node-by-id diagram node-id)
        ntype (node-type node)
        step (inc (or (:step agent-state) 0))
        action (node-action node {:agent agent-state :env env-state :obs obs})
        agent* (assoc agent-state
                      :last/node node-id
                      :last/action action)
        edge (when (and node (not= ntype :stop))
               (select-edge diagram node-id {:agent agent* :env env-state :obs obs :action action}))
        next-id (when edge (:to edge))
        done? (or (= ntype :stop) (nil? next-id))
        agent** (assoc agent*
                       :node-id next-id
                       :done? done?
                       :step step)
        telemetry {:ts (step-timestamp agent-state step)
                   :step step
                   :node/id node-id
                   :node/type ntype
                   :edge/selected (select-keys edge [:from :to :label :guard])
                   :action action
                   :tool (:tool action)
                   :obs (select-keys obs [:budget :uncertainty :hint])
                   :env (select-keys env-state [:budget :hints])}]
    {:action action
     :new-state agent**
     :telemetry telemetry}))
