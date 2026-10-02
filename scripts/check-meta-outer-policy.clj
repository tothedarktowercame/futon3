(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.set :as set])

(def contract
  (edn/read-string (slurp (io/file "library/meta/meta-outer-policy-cascade.edn"))))

(def required-slots
  (->> (:slots contract)
       (keep (fn [[slot spec]] (when (:required spec) slot)))
       set))

(defn arm-for [{:keys [injury-observation]}]
  (if (= :active injury-observation) :self-heal :ordinary-work))

(defn exclusion-reason [arm observation candidate]
  (let [task-kind (get-in candidate [:slots :task-kind])
        admitted (set (get-in contract [:arms arm :admit-task-kinds]))]
    (cond
      (not (contains? admitted task-kind))
      (if (= arm :self-heal) :machine-injury-active :task-kind-not-admitted)

      (and (= arm :self-heal)
           (not= (:injured-capability observation)
                 (:repairs-capability candidate)))
      :algorithm-capability-mismatch

      :else nil)))

(defn complete-slots? [candidate]
  (set/subset? required-slots (set (keys (:slots candidate)))))

(defn g [{:keys [risk ambiguity epistemic-value]}]
  (+ risk ambiguity (- epistemic-value)))

(defn evaluate [{:keys [id observation candidates expected]}]
  (let [arm (arm-for observation)
        missing (->> candidates
                     (remove complete-slots?)
                     (map :id)
                     set)
        exclusions (into {}
                         (keep (fn [candidate]
                                 (when-let [reason (exclusion-reason arm observation candidate)]
                                   [(:id candidate) reason])))
                         candidates)
        admitted (remove #(or (contains? missing (:id %))
                              (contains? exclusions (:id %)))
                         candidates)
        scored (mapv #(assoc % :g (g (:g-terms %))) admitted)
        selected (first (sort-by (juxt :g (comp str :id)) scored))
        actual {:arm arm
                :typed-exclusions exclusions
                :selected (:id selected)
                :g (:g selected)}]
    (assert (empty? missing) (str id " has candidates with unfilled slots: " missing))
    (assert (seq scored) (str id " has no admitted policies"))
    (assert (= expected actual)
            (str id " expected " expected ", computed " actual))
    {:example id
     :arm arm
     :admitted (mapv :id scored)
     :typed-exclusions exclusions
     :g (into {} (map (juxt :id :g)) scored)
     :selected (:id selected)}))

(assert (= :meta/outer-policy-cascade-v1 (:schema contract)))
(assert (= :argmin-G (get-in contract [:selection :law])))
(doseq [receipt (map evaluate (:executable-examples contract))]
  (prn receipt))

;; Fail closed when a load-bearing policy slot disappears.
(let [healthy (first (:executable-examples contract))
      missing-target (update-in healthy [:candidates 0 :slots] dissoc :target)]
  (assert
   (try (evaluate missing-target) false
        (catch AssertionError _ true))))

;; Injury does not make an unrelated repair algorithm admissible.
(let [injured (second (:executable-examples contract))
      no-matching-heal (assoc-in injured [:candidates 1 :repairs-capability] :network)]
  (assert
   (try (evaluate no-matching-heal) false
        (catch AssertionError _ true))))
