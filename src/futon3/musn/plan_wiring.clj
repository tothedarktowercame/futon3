(ns futon3.musn.plan-wiring
  "Wiring-diagram evaluator for MUSN plans (MMCA-style schema)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def ^:private default-components-path
  "resources/musn-plan-components.edn")

(def ^:private default-eval-components-path
  "resources/musn-plan-eval-components.edn")

(def ^:private action-type-key :musn.action/type)
(def ^:private action-spec-key :musn.action/spec)

(def ^:private known-action-types
  #{:command_execution :tool_use :file_edit :reasoning})

(declare normalize-diagram)

(def ^:private default-eval-config
  {:thresholds {:steps 8 :nodes 12 :edges 16}
   :confidence/base 0.9
   :confidence/weights {:steps 0.15
                        :nodes 0.1
                        :edges 0.1
                        :unknown 0.3
                        :tool 0.15
                        :writes 0.1
                        :branches 0.05
                        :parallel 0.05
                        :blast 0.0}
   :risk/weights {:unknown 0.35
                  :tool 0.25
                  :writes 0.25
                  :branches 0.1
                  :parallel 0.05
                  :blast 0.0}
   :discount/exponent 1.0
   :mana/scale 10.0
   :mana/min 0.0
   :mana/max 10.0})

(defn- merge-eval-config
  [config]
  (let [config (or config {})]
    (-> default-eval-config
        (update :thresholds merge (:thresholds config))
        (update :confidence/weights merge (:confidence/weights config))
        (update :risk/weights merge (:risk/weights config))
        (merge (dissoc config :thresholds :confidence/weights :risk/weights)))))

(defn load-components
  ([] (load-components default-components-path))
  ([path]
   (edn/read-string (slurp (io/file path)))))

(defn load-eval-components
  ([] (load-eval-components default-eval-components-path))
  ([path]
   (edn/read-string (slurp (io/file path)))))

(defn component-def
  [lib component-id]
  (get-in lib [:components component-id]))

(defn- inputs-map
  [component]
  (into {} (map (fn [[name type]] [name type]) (:inputs component))))

(defn- outputs-map
  [component]
  (into {} (map (fn [[name type]] [name type]) (:outputs component))))

(defn- default-port
  [ports]
  (first (keys ports)))

(defn- node-id-set
  [diagram]
  (set (map :id (:nodes diagram))))

(defn- list-type?
  [t]
  (and (keyword? t)
       (str/ends-with? (name t) "-list")))

(defn- base-type
  [list-type]
  (when (list-type? list-type)
    (let [nm (name list-type)
          base (subs nm 0 (- (count nm) (count "-list")))]
      (keyword (namespace list-type) base))))

(defn- type-compatible?
  [out-type in-type]
  (or (= out-type in-type)
      (= in-type :any)
      (= out-type :any)
      (and (list-type? in-type)
           (= out-type (base-type in-type)))))

(defn- edge->input-type
  [lib diagram edge]
  (let [to-id (:to edge)
        node (first (filter #(= (:id %) to-id) (:nodes diagram)))
        component (component-def lib (:component node))
        inputs (inputs-map component)
        to-port (or (:to-port edge) (default-port inputs))]
    (get inputs to-port)))

(defn- edge->output-type
  [lib diagram edge]
  (let [from-id (:from edge)
        node (first (filter #(= (:id %) from-id) (:nodes diagram)))
        component (component-def lib (:component node))
        outputs (outputs-map component)
        from-port (or (:from-port edge) (default-port outputs))]
    (get outputs from-port)))

(defn- connected-node-ids
  [diagram]
  (let [output-id (:output diagram)
        nodes (:nodes diagram)
        edges (:edges diagram)
        reverse-adj (reduce (fn [m {:keys [from to]}]
                              (if from
                                (update m to (fnil conj #{}) from)
                                m))
                            {} edges)]
    (loop [queue (if output-id [output-id] [])
           seen #{}]
      (if (empty? queue)
        seen
        (let [id (first queue)
              more (seq (get reverse-adj id))
              next-queue (into (vec (rest queue)) (remove seen more))]
          (recur next-queue (conj seen id)))))))

(defn validate-diagram
  "Validate a wiring diagram against the component library.
  Returns {:ok? bool :errors [..] :warnings [..]}.
  Options: {:require-connected? true|false}."
  ([lib diagram] (validate-diagram lib diagram {}))
  ([lib diagram {:keys [require-connected?] :or {require-connected? true}}]
   (let [diagram (normalize-diagram diagram)
         nodes (:nodes diagram)
         edges (:edges diagram)
         node-ids (node-id-set diagram)
         errors (atom [])
         warnings (atom [])
         implicit-ports #{:ctx :state :context :plan :spec}
         add-error (fn [msg] (swap! errors conj msg))
         add-warning (fn [msg] (swap! warnings conj msg))]
     (when-not (:output diagram)
       (add-error "diagram missing :output"))
     (when (and (:output diagram) (not (contains? node-ids (:output diagram))))
       (add-error (str "diagram output is not a node id: " (:output diagram))))
     (doseq [node nodes]
       (when-not (:id node)
         (add-error "node missing :id"))
       (when-not (:component node)
         (add-error (str "node missing :component: " (:id node))))
       (when-not (component-def lib (:component node))
         (add-error (str "unknown component: " (:component node)))))
     (when (not= (count node-ids) (count nodes))
       (add-error "duplicate node ids"))
     (doseq [edge edges]
       (let [from-id (:from edge)
             to-id (:to edge)]
         (when (and from-id (not (contains? node-ids from-id)))
           (add-error (str "edge references unknown :from node " from-id)))
         (when (and to-id (not (contains? node-ids to-id)))
           (add-error (str "edge references unknown :to node " to-id)))
         (if (:value edge)
           (let [expected (edge->input-type lib diagram edge)
                 provided (:value-type edge)]
             (when (and expected provided (not (type-compatible? provided expected)))
               (add-error (str "edge value type mismatch to " to-id ": " expected " vs " provided))))
           (let [out-type (edge->output-type lib diagram edge)
                 in-type (edge->input-type lib diagram edge)]
             (when (nil? out-type)
               (add-error (str "edge missing output port type from " from-id)))
             (when (nil? in-type)
               (add-error (str "edge missing input port type to " to-id)))
             (when (and out-type in-type)
               (when (not (type-compatible? out-type in-type))
                 (add-error (str "edge type mismatch " from-id " -> " to-id ": " out-type " vs " in-type)))))))
       (when (and (nil? (:from edge)) (nil? (:value edge)))
         (add-warning "edge has neither :from nor :value")))
     (let [input-edges (group-by :to edges)]
       (doseq [node nodes]
         (let [component (component-def lib (:component node))
               inputs (inputs-map component)
               used-ports (set (map :to-port (get input-edges (:id node))))]
           (doseq [port (keys inputs)]
             (when-not (or (contains? used-ports port)
                           (contains? implicit-ports port))
               (add-warning (str "unwired input " (:id node) ":" port)))))))
     (when require-connected?
       (let [reachable (connected-node-ids diagram)
             orphaned (set/difference node-ids reachable)]
         (doseq [node-id (sort orphaned)]
           (add-error (str "disconnected node " node-id)))))
     {:ok? (empty? @errors)
      :errors @errors
      :warnings @warnings
      :diagram diagram})))

(defn validate-plan-diagram
  ([diagram] (validate-plan-diagram diagram {}))
  ([diagram opts]
   (validate-diagram (load-components) diagram opts)))

(defn- topo-order
  [nodes edges]
  (let [ids (map :id nodes)
        incoming (reduce (fn [m id] (assoc m id 0)) {} ids)
        adj (reduce (fn [m id] (assoc m id [])) {} ids)
        edge-pairs (for [edge edges :when (:from edge)]
                     [(:from edge) (:to edge)])
        incoming (reduce (fn [m [_ to]] (update m to (fnil inc 0))) incoming edge-pairs)
        adj (reduce (fn [m [from to]] (update m from (fnil conj []) to)) adj edge-pairs)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                       (filter #(zero? (get incoming % 0)) ids))
           incoming incoming
           order []]
      (if (empty? queue)
        (if (= (count order) (count ids)) order ids)
        (let [id (peek queue)
              queue (pop queue)
              [incoming queue] (reduce (fn [[inc q] to]
                                         (let [n (dec (get inc to 0))
                                               inc (assoc inc to n)
                                               q (if (zero? n) (conj q to) q)]
                                           [inc q]))
                                       [incoming queue]
                                       (get adj id []))]
          (recur queue incoming (conj order id)))))))

(defn- component-inputs
  [lib component-id]
  (into {} (map (fn [[name type]] [name type])
                (:inputs (component-def lib component-id)))))

(defn- component-outputs
  [lib component-id]
  (into {} (map (fn [[name type]] [name type])
                (:outputs (component-def lib component-id)))))

(defn- collect-inputs
  [lib node edges node-map node-values]
  (let [inputs (component-inputs lib (:component node))
        edges-for (group-by :to edges)
        relevant (get edges-for (:id node))
        result (atom {})]
    (doseq [[port port-type] inputs]
      (let [port-edges (filter #(= (or (:to-port %) (default-port inputs)) port) relevant)
            values (map (fn [edge]
                          (if (contains? edge :value)
                            (:value edge)
                            (let [from-id (:from edge)
                                  from-node (get node-map from-id)
                                  from-port (or (:from-port edge)
                                                (default-port (component-outputs lib (:component from-node))))]
                              (get-in node-values [from-id from-port]))))
                        port-edges)]
        (if (list-type? port-type)
          (let [values (mapcat (fn [v] (if (sequential? v) v [v])) values)
                values (remove nil? values)]
            (swap! result assoc port (vec values)))
          (swap! result assoc port (first values)))))
    @result))

(defn- ensure-action-list
  [value]
  (cond
    (nil? value) []
    (sequential? value) (vec value)
    :else [value]))

(defn- resolve-spec
  [inputs params]
  (if (contains? inputs :spec)
    (let [spec (:spec inputs)]
      (if (some? spec)
        (if (and (map? params) (map? spec))
          (merge params spec)
          spec)
        params))
    params))

(defn- emit-action
  [type inputs params]
  {:action {action-type-key type
            action-spec-key (resolve-spec inputs params)}})

(defn- clamp
  [x lo hi]
  (-> x (max lo) (min hi)))

(defn- clamp-01
  [x]
  (clamp (double (or x 0.0)) 0.0 1.0))

(defn- norm
  [value threshold]
  (if (pos? (double (or threshold 0.0)))
    (clamp-01 (/ (double (or value 0.0)) (double threshold)))
    0.0))

(defn- keywordize
  [value]
  (cond
    (keyword? value) value
    (string? value) (keyword value)
    :else nil))

(defn normalize-diagram
  "Normalize diagram fields so string keywords from JSON become keywords."
  [diagram]
  (let [kw (fn [v] (or (keywordize v) v))]
    (cond-> diagram
      (:output diagram)
      (update :output kw)
      (:nodes diagram)
      (update :nodes (fn [nodes]
                       (mapv (fn [node]
                               (-> node
                                   (update :id kw)
                                   (update :component kw)))
                             (or nodes []))))
      (:edges diagram)
      (update :edges (fn [edges]
                       (mapv (fn [edge]
                               (-> edge
                                   (update :from kw)
                                   (update :to kw)
                                   (update :from-port kw)
                                   (update :to-port kw)
                                   (update :value-type kw)))
                             (or edges [])))))))

(defn- action-type
  [action]
  (keywordize (or (get action action-type-key)
                  (:type action))))

(defn- count-actions
  [actions pred]
  (count (filter pred actions)))

(defn- blast-risk-level
  [context]
  (let [risk (keywordize (or (:blast/risk context)
                             (get-in context [:plan/context :blast/risk])
                             (get-in context [:blast :risk])))]
    (case risk
      :none 0.0
      :low 0.1
      :medium 0.3
      :med 0.3
      :high 0.6
      :critical 0.9
      :unknown 0.5
      nil 0.0
      0.0)))

(defn- feature-ratios
  [{:keys [steps unknown tool writes branches parallel] :as features}]
  (let [steps (max 1 steps)]
    (assoc features
           :unknown-ratio (/ (double unknown) (double steps))
           :tool-ratio (/ (double tool) (double steps))
           :writes-ratio (/ (double writes) (double steps))
           :branches-ratio (/ (double branches) (double steps))
           :parallel-ratio (/ (double parallel) (double steps)))))

(defn- plan-features
  ([diagram actions] (plan-features diagram actions {}))
  ([{:keys [nodes edges] :as diagram} actions context]
  (let [nodes (vec (or nodes []))
        edges (vec (or edges []))
        steps (count actions)
        branches (count (filter #(= (:component %) :musn.plan/if-then-else) nodes))
        parallel (count (filter #(= (:component %) :musn.plan/parallel) nodes))
        writes (count-actions actions #(= (action-type %) :file_edit))
        tool (count-actions actions #(contains? #{:tool_use :command_execution} (action-type %)))
        unknown (count-actions actions #(not (contains? known-action-types (action-type %))))
        blast-risk (blast-risk-level context)]
    (feature-ratios {:steps steps
                     :nodes (count nodes)
                     :edges (count edges)
                     :branches branches
                     :parallel parallel
                     :writes writes
                     :tool tool
                     :unknown unknown
                     :blast-risk blast-risk}))))

(defn- normalized-features
  [features {:keys [thresholds] :as _config}]
  (let [{:keys [steps nodes edges]} features
        {:keys [steps nodes edges]} thresholds]
    {:steps (norm (:steps features) steps)
     :nodes (norm (:nodes features) nodes)
     :edges (norm (:edges features) edges)
     :unknown (:unknown-ratio features)
     :tool (:tool-ratio features)
     :writes (:writes-ratio features)
     :branches (:branches-ratio features)
     :parallel (:parallel-ratio features)
     :blast (:blast-risk features)}))

(defn- weighted-sum
  [weights features]
  (reduce (fn [acc [k w]]
            (+ acc (* (double w) (double (or (get features k) 0.0)))))
          0.0
          weights))

(defn- score-from-features
  [features config]
  (let [weights (:confidence/weights config)
        base (:confidence/base config)
        normalized (normalized-features features config)
        penalty (weighted-sum weights normalized)]
    (clamp-01 (- (double base) penalty))))

(defn- risk-from-features
  [features config]
  (let [weights (:risk/weights config)
        normalized (normalized-features features config)]
    (clamp-01 (weighted-sum weights normalized))))

(defn- discount-confidence
  [score risk config]
  (let [exp (double (or (:discount/exponent config) 1.0))
        discount (Math/pow (double (or risk 0.0)) exp)]
    (clamp-01 (* (double (or score 0.0)) (- 1.0 discount)))))

(defn- mana-award
  [confidence config]
  (let [scale (double (or (:mana/scale config) 10.0))
        raw (* scale (double (or confidence 0.0)))
        minv (double (or (:mana/min config) 0.0))
        maxv (double (or (:mana/max config) scale))]
    {:raw raw
     :award (clamp raw minv maxv)
     :scale scale}))

(defn- eval-output
  [confidence risk score features config]
  {:musn.plan/confidence confidence
   :musn.plan/risk risk
   :musn.plan/score score
   :musn.plan/expected confidence
   :musn.plan/features features
   :musn.plan/mana (mana-award confidence config)})

(def ^:private default-registry
  {:musn.plan/action
   (fn [{:keys [type spec] :as inputs} params _]
     (let [action-type (or type (:type params) :unknown)
           inputs (cond-> inputs
                    spec (assoc :spec spec))]
       (emit-action action-type inputs params)))

   :musn.plan/action-command
   (fn [inputs params _]
     (emit-action :command_execution inputs params))

   :musn.plan/action-reasoning
   (fn [inputs params _]
     (emit-action :reasoning inputs params))

   :musn.plan/action-file-edit
   (fn [inputs params _]
     (emit-action :file_edit inputs params))

   :musn.plan/action-tool
   (fn [inputs params _]
     (emit-action :tool_use inputs params))

   :musn.plan/sequence
   (fn [{:keys [actions]} _ _]
     {:actions (ensure-action-list actions)})

   :musn.plan/parallel
   (fn [{:keys [actions]} _ _]
     {:actions (ensure-action-list actions)})

   :musn.plan/if-then-else
   (fn [{:keys [cond then else]} _ _]
     {:actions (ensure-action-list (if cond then else))})

   :musn.plan/gate
   (fn [{:keys [cond actions]} _ _]
     {:actions (if cond (ensure-action-list actions) [])})})

(def ^:private eval-registry
  (letfn [(ctx [context] (or (:context context) context))]
    {:musn.plan/feature-extract
     (fn [_ _ context]
       (let [{:keys [plan/diagram plan/actions] :as full} (ctx context)]
         {:features (plan-features diagram actions full)}))

     :musn.plan/feature-score
     (fn [{:keys [features]} _ context]
       (let [{:keys [plan/eval-config]} (ctx context)
             config (merge-eval-config eval-config)
           score (score-from-features features config)]
         {:score score}))

     :musn.plan/risk-score
     (fn [{:keys [features]} _ context]
       (let [{:keys [plan/eval-config]} (ctx context)
             config (merge-eval-config eval-config)
             risk (risk-from-features features config)]
         {:risk risk}))

     :musn.plan/risk-discount
     (fn [{:keys [score risk]} _ context]
       (let [{:keys [plan/eval-config]} (ctx context)
             config (merge-eval-config eval-config)
             confidence (discount-confidence score risk config)]
         {:confidence confidence}))

     :musn.plan/eval-output
     (fn [{:keys [confidence risk score features]} _ context]
       (let [{:keys [plan/eval-config]} (ctx context)
             config (merge-eval-config eval-config)
             base-score (or score confidence)]
         {:eval (eval-output confidence risk base-score features config)}))}))

(defn evaluate-diagram
  "Evaluate a plan wiring diagram and return {:output .. :node-values ..}."
  ([diagram] (evaluate-diagram diagram {}))
  ([diagram {:keys [components-path registry ctx state] :as context}]
   (let [diagram (normalize-diagram diagram)
         lib (load-components (or components-path default-components-path))
         registry (merge default-registry registry)
         nodes (:nodes diagram)
         edges (:edges diagram)
         order (topo-order nodes edges)
         node-map (into {} (map (fn [n] [(:id n) n]) nodes))]
     (loop [ids order
            node-values {}]
       (if (empty? ids)
         {:output (get node-values (:output diagram))
          :node-values node-values}
         (let [id (first ids)
               node (get node-map id)
               component-def (component-def lib (:component node))
               _ (when-not component-def
                   (throw (ex-info "Unknown component in plan wiring diagram"
                                   {:node-id id :component (:component node)})))
               inputs (collect-inputs lib node edges node-map node-values)
               component-fn (get registry (:component node))
               params (:params node)
               inputs-with-ctx (assoc inputs :ctx ctx :state state :context context)
               result (if component-fn
                        (component-fn inputs-with-ctx params {:ctx ctx :state state :context context})
                        {})]
           (recur (rest ids) (assoc node-values id result))))))))

(defn evaluate-plan
  "Evaluate a plan diagram and return a vector of action maps."
  ([diagram] (evaluate-plan diagram {}))
  ([diagram opts]
   (let [{:keys [output]} (evaluate-diagram diagram opts)
         actions (cond
                   (and (map? output) (contains? output :actions)) (:actions output)
                   (and (map? output) (contains? output :action)) [(:action output)]
                   (map? output) [output]
                   (sequential? output) output
                   :else [])]
     (vec actions))))

(defn default-eval-diagram
  []
  {:nodes [{:id :features :component :musn.plan/feature-extract}
           {:id :score :component :musn.plan/feature-score}
           {:id :risk :component :musn.plan/risk-score}
           {:id :discount :component :musn.plan/risk-discount}
           {:id :out :component :musn.plan/eval-output}]
   :edges [{:from :features :to :score :to-port :features}
           {:from :features :to :risk :to-port :features}
           {:from :score :to :discount :to-port :score}
           {:from :risk :to :discount :to-port :risk}
           {:from :discount :to :out :to-port :confidence}
           {:from :risk :to :out :to-port :risk}
           {:from :score :to :out :to-port :score}
           {:from :features :to :out :to-port :features}]
   :output :out})

(defn evaluate-plan-eval
  "Evaluate a plan diagram using the evaluation wiring; returns eval map."
  ([diagram actions] (evaluate-plan-eval diagram actions {}))
  ([diagram actions opts]
   (let [eval-diagram (or (:eval/diagram opts) (default-eval-diagram))
         eval-components-path (:eval/components-path opts)
         eval-config (:eval/config opts)
         result (evaluate-diagram eval-diagram
                                  {:components-path (or eval-components-path default-eval-components-path)
                                   :registry eval-registry
                                   :plan/diagram diagram
                                   :plan/actions actions
                                   :plan/context (:plan/context opts)
                                   :plan/eval-config eval-config})
         output (:output result)]
     (cond
       (and (map? output) (contains? output :eval)) (:eval output)
       (map? output) output
       :else {:musn.plan/confidence 0.0
              :musn.plan/risk 1.0
              :musn.plan/expected 0.0
              :musn.plan/features {}}))))

(defn evaluate-plan+eval
  "Return {:actions [...] :eval {...}} with auto-generated eval wiring."
  ([diagram] (evaluate-plan+eval diagram {}))
  ([diagram opts]
   (let [actions (evaluate-plan diagram opts)
         eval (evaluate-plan-eval diagram actions opts)]
     {:actions actions
      :eval eval})))
