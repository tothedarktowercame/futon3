(ns futon3.fulab.harness
  "Toy regime-sweep harness for fulab controllers."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [futon3.fulab.controller :as controller]))

(def tasks
  [{:id :task/scan-logs
    :spec "Find the root cause of a stack trace in logs."
    :difficulty 1
    :steps [{:type :observe}
            {:type :tool :tool :rg}
            {:type :act}]
    :hints ["Search for ERROR and WARN lines."
            "Look for the first exception cause."]}
   {:id :task/config-tweak
    :spec "Update a config value and verify behavior."
    :difficulty 2
    :steps [{:type :plan}
            {:type :file-edit}
            {:type :command :cmd :test}
            {:type :act}]
    :hints ["Locate config.edn and change the target key."
            "Run the test suite to verify."]}
   {:id :task/api-integration
    :spec "Wire a new endpoint into the router."
    :difficulty 2
    :steps [{:type :observe}
            {:type :plan}
            {:type :tool :tool :rg}
            {:type :file-edit}
            {:type :act}]
    :hints ["Search for similar routes."
            "Update schema validation."]}
   {:id :task/refactor-flow
    :spec "Refactor a flow without changing behavior."
    :difficulty 3
    :steps [{:type :plan}
            {:type :tool :tool :rg}
            {:type :file-edit}
            {:type :command :cmd :test}
            {:type :act}]
    :hints ["Scan for usages before editing."
            "Keep tests green."]}
   {:id :task/bug-repro
    :spec "Reproduce a failing case and isolate the trigger."
    :difficulty 3
    :steps [{:type :observe}
            {:type :tool :tool :rg}
            {:type :command :cmd :test}
            {:type :act}]
    :hints ["Search for recent changes."
            "Try a minimal repro input."]}
   {:id :task/doc-update
    :spec "Update README and sync examples."
    :difficulty 1
    :steps [{:type :plan}
            {:type :file-edit}
            {:type :act}]
    :hints ["Search for the outdated snippet."
            "Update and scan for consistency."]}])

(def regimes
  {:abundance {:id :abundance
               :budget 12
               :hints-available 2
               :uncertainty 0.2}
   :scarcity {:id :scarcity
              :budget 7
              :hints-available 1
              :uncertainty 0.65}})

(def sample-diagram
  {:id :diagram/fulab-min
   :start :sense
   :nodes [{:id :sense :type :sense}
           {:id :decide :type :decide :params {:plan-id :diagram/plan}}
           {:id :explore :type :tool :params {:tool :rg :note "probe signals"}}
           {:id :act :type :act}
           {:id :reflect :type :reflect}
           {:id :stop :type :stop}]
   :edges [{:from :sense :to :decide :label "sense->decide" :guard {:type :always}}
           {:from :decide :to :explore :label "explore" :guard {:type :uncertainty-gt :value 0.5}}
           {:from :decide :to :act :label "act" :guard {:type :uncertainty-lt :value 0.51}}
           {:from :explore :to :reflect :label "reflect" :guard {:type :always}}
           {:from :reflect :to :act :label "act" :guard {:type :always}}
           {:from :act :to :stop :label "done" :guard {:type :done? :value true}}
           {:from :act :to :sense :label "loop" :guard {:type :always}}]})

(defn- action-cost [action]
  (case (:type action)
    :tool 2
    :command 2
    :file-edit 2
    :stop 0
    1))

(defn- step-match?
  [step action]
  (and (= (:type step) (:type action))
       (or (nil? (:tool step)) (= (:tool step) (:tool action)))
       (or (nil? (:cmd step)) (= (:cmd step) (:cmd action)))))

(defn- take-hint
  [env action]
  (when (and (seq (:hints-left env))
             (#{:observe :tool :command} (:type action)))
    (first (:hints-left env))))

(defn- adjust-uncertainty
  [uncertainty action matched? hint?]
  (let [u0 (double (or uncertainty 0.0))
        u1 (-> u0
               (- (cond
                    hint? 0.25
                    (#{:observe :tool :command} (:type action)) 0.15
                    (= :reflect (:type action)) 0.05
                    :else 0.0))
               (+ (if (and (= :act (:type action)) (not matched?)) 0.1 0.0)))]
    (-> u1 (max 0.0) (min 1.0))))

(defn- init-env
  [{:keys [difficulty hints] :as task} {:keys [budget hints-available uncertainty] :as regime} seed]
  (let [difficulty (or difficulty 1)
        budget (max 2 (- budget (if (= (:id regime) :scarcity) (dec difficulty) 0)))
        hint-count (min (count hints) (or hints-available 0))
        uncertainty (min 1.0 (+ (double (or uncertainty 0.0))
                                (* 0.1 (dec difficulty))))]
    {:task task
     :regime regime
     :budget budget
     :budget-start budget
     :uncertainty uncertainty
     :hints-left (vec (take hint-count hints))
     :hints (vec (take hint-count hints))
     :progress 0
     :done? false
     :success? false
     :seed seed}))

(defn- initial-obs [env]
  {:budget (:budget env)
   :uncertainty (:uncertainty env)
   :hint nil
   :done? (:done? env)
   :success? (:success? env)
   :env env})

(defn- env-step [env action]
  (let [task (:task env)
        steps (:steps task)
        idx (:progress env)
        current-step (when (< idx (count steps)) (nth steps idx))
        matched? (and current-step (step-match? current-step action))
        progress (if matched? (inc idx) idx)
        success? (>= progress (count steps))
        hint (take-hint env action)
        budget (- (:budget env) (action-cost action))
        env* (-> env
                 (assoc :budget budget
                        :progress progress
                        :success? success?
                        :done? (or success? (<= budget 0)))
                 (update :hints-left #(if hint (vec (rest %)) %))
                 (update :hints #(if hint (vec (rest %)) %))
                 (assoc :uncertainty (adjust-uncertainty (:uncertainty env) action matched? (boolean hint))))
        tool-output (when (#{:tool :command} (:type action))
                      (or hint "ok"))
        obs {:budget (:budget env*)
             :uncertainty (:uncertainty env*)
             :hint hint
             :done? (:done? env*)
             :success? (:success? env*)
             :tool/output tool-output
             :env env*}
        event {:event/type :env/step
               :budget (:budget env*)
               :uncertainty (:uncertainty env*)
               :progress progress
               :matched? matched?
               :success? success?
               :done? (:done? env*)}]
    {:env env*
     :obs obs
     :event event}))

(defn- controller-id [controller]
  (cond
    (instance? futon3.fulab.controller.BaselineController controller) :baseline
    (instance? futon3.fulab.controller.DiagramController controller) :diagram
    :else :unknown))

(defn- strategy-signature [actions]
  (->> actions
       (map (fn [action]
              (cond-> {:type (:type action)}
                (:tool action) (assoc :tool (:tool action))
                (:cmd action) (assoc :cmd (:cmd action)))))
       vec))

(defn- tool-sequence [actions]
  (->> actions
       (filter #(#{:tool :command} (:type %)))
       (map (fn [action] (or (:tool action) (:cmd action))))
       vec))

(defn- plan-key [action]
  (when (= :plan (:type action))
    (or (:plan-id action)
        (when-let [plan (:plan action)] (hash plan))
        (:note action))))

(defn- plan-rewrites [actions]
  (let [plans (vec (keep plan-key actions))]
    (->> (partition 2 1 plans)
         (filter (fn [[a b]] (not= a b)))
         count)))

(defn- score-run [env logs]
  (let [success? (:success? env)
        base (if success? 1.0 0.0)
        efficiency (if (pos? (:budget-start env))
                     (/ (double (:budget env)) (double (:budget-start env)))
                     0.0)
        step-penalty (* 0.02 (count logs))]
    (-> (+ base (* 0.2 efficiency) (- step-penalty))
        (max 0.0)
        (min 1.2))))

(defn run-episode
  [controller env {:keys [max-steps] :or {max-steps 24}}]
  (let [state0 (controller/init controller env)
        controller-tag (controller-id controller)]
    (loop [env env
           obs (initial-obs env)
           state state0
           logs []]
      (if (or (:done? env)
              (controller/done? controller state)
              (>= (count logs) max-steps))
        (let [actions (map :action logs)
              plan-keys (vec (distinct (keep plan-key actions)))
              plan-rewrite-count (plan-rewrites actions)
              tool-count (count (filter #(#{:tool :command} (:type %)) actions))
              signature (strategy-signature actions)
              tools (tool-sequence actions)]
          {:task/id (get-in env [:task :id])
           :regime (get-in env [:regime :id])
           :controller controller-tag
           :success? (:success? env)
           :steps (count logs)
           :tool-count tool-count
           :plan-rewrites plan-rewrite-count
           :strategy/signature signature
           :strategy/tool-seq tools
           :plan/keys plan-keys
           :score (score-run env logs)
           :logs logs
           :env env})
        (let [{:keys [action state telemetry]} (controller/step controller state obs)
              {:keys [env obs event]} (env-step env action)
              log (merge telemetry
                         event
                         {:task/id (get-in env [:task :id])
                          :regime (get-in env [:regime :id])
                          :controller controller-tag
                          :action/type (:type action)})]
          (recur env obs state (conj logs log)))))))

(defn- aggregate-metrics [runs]
  (let [total (count runs)
        successes (count (filter :success? runs))
        tool-calls (reduce + (map :tool-count runs))
        avg-score (if (pos? total)
                    (/ (reduce + (map :score runs)) total)
                    0.0)
        strategy-diversity (count (distinct (map :strategy/tool-seq runs)))
        plan-diversity (count (distinct (mapcat :plan/keys runs)))
        hypothesis-turnover (reduce + (map :plan-rewrites runs))]
    {:tasks total
     :success-rate (if (pos? total) (/ successes total) 0.0)
     :tool-calls tool-calls
     :strategy/diversity strategy-diversity
     :plan/diversity plan-diversity
     :hypothesis/turnover hypothesis-turnover
     :avg-score avg-score}))

(defn- seed-for
  [base controller-id regime-id task-id]
  (long (+ (or base 0) (hash [controller-id regime-id task-id]))))

(defn run-sweep
  [{:keys [seed max-steps out-dir diagram]
    :or {seed 43 max-steps 24 out-dir "dev/fulab-regime" diagram sample-diagram}}]
  (let [controllers {:baseline (controller/baseline-controller)
                     :diagram (controller/diagram-controller diagram)}
        runs (for [[_ regime] regimes
                   [_ ctrl] controllers
                   task tasks
                   :let [cid (controller-id ctrl)
                         run-seed (seed-for seed cid (:id regime) (:id task))
                         env (init-env task regime run-seed)]]
               (run-episode ctrl env {:max-steps max-steps}))
        runs-by (group-by (juxt :regime :controller) runs)
        results (reduce (fn [acc [[regime-id controller-id] data]]
                          (assoc-in acc [:regimes regime-id :controllers controller-id]
                                    {:metrics (aggregate-metrics data)
                                     :runs data}))
                        {:generated-at (str (java.time.Instant/now))}
                        runs-by)
        out-dir (io/file out-dir)
        _ (.mkdirs out-dir)
        edn-path (io/file out-dir "fulab-regime-sweep.edn")
        json-path (io/file out-dir "fulab-regime-sweep.json")
        md-path (io/file out-dir "fulab-regime-sweep.md")]
    (spit edn-path (with-out-str (pprint/pprint results)))
    (spit json-path (json/generate-string results {:pretty true}))
    (spit md-path
          (with-out-str
            (println "# Fulab Regime Sweep")
            (println "")
            (println "| Regime | Controller | Success Rate | Tool Calls | Strategy Diversity | Plan Diversity | Hypothesis Turnover | Avg Score |")
            (println "| --- | --- | --- | --- | --- | --- | --- | --- |")
            (doseq [[regime-id {:keys [controllers]}] (:regimes results)
                    [controller-id {:keys [metrics]}] controllers]
              (println (format "| %s | %s | %.2f | %d | %d | %d | %d | %.2f |"
                               (name regime-id)
                               (name controller-id)
                               (double (:success-rate metrics))
                               (long (:tool-calls metrics))
                               (long (:strategy/diversity metrics))
                               (long (:plan/diversity metrics))
                               (long (:hypothesis/turnover metrics))
                               (double (:avg-score metrics)))))))
    (assoc results
           :output/paths {:edn (.getPath edn-path)
                          :json (.getPath json-path)
                          :md (.getPath md-path)})))

(defn -main [& args]
  (let [[out-dir seed max-steps] args
        opts (cond-> {}
               out-dir (assoc :out-dir out-dir)
               seed (assoc :seed (Long/parseLong seed))
               max-steps (assoc :max-steps (Long/parseLong max-steps)))]
    (let [result (run-sweep opts)]
      (println "Wrote regime sweep report to:")
      (println (get-in result [:output/paths :edn]))
      (println (get-in result [:output/paths :json]))
      (println (get-in result [:output/paths :md])))))
