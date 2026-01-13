(ns futon3.musn.logic
  "core.logic constraint kernel for MUSN turn rules."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel flago name)
(pldb/db-rel actiono action)
(pldb/db-rel costo cost)
(pldb/db-rel selection-modeo mode)
(pldb/db-rel trailo status)

(def ^:private plan-required-actions
  #{"implement" "update" "off-trail" "wide-scan"})

(def ^:private write-actions
  #{"implement" "update"})

(def ^:private consent-costs
  #{:expensive :human-contact})

(defn- exists? [db goal]
  (not (empty? (pldb/with-db db
                 (l/run 1 [q]
                   goal
                   (l/== q true))))))

(defn- flag? [db flag]
  (exists? db (flago flag)))

(defn- action-matches? [db actions]
  (exists? db
           (l/fresh [action]
             (actiono action)
             (l/membero action (vec actions)))))

(defn- cost-matches? [db costs]
  (exists? db
           (l/fresh [cost]
             (costo cost)
             (l/membero cost (vec costs)))))

(defn- selection-mode? [db mode]
  (exists? db (selection-modeo mode)))

(defn- off-trail? [db]
  (exists? db (trailo :off-exceeded)))

(defn- build-db [state action {:keys [off-trail?]}]
  (let [mode (get-in state [:selection :selection/reason :mode])
        facts (cond-> []
                (:plan? state) (conj [flago :plan])
                (:selection state) (conj [flago :selection])
                (:write? state) (conj [flago :write])
                (some? mode) (conj [selection-modeo mode])
                (:action action) (conj [actiono (:action action)])
                (:cost action) (conj [costo (:cost action)])
                off-trail? (conj [trailo :off-exceeded]))]
    (if (seq facts)
      (apply pldb/db facts)
      (pldb/db))))

(defn- finalize-result [rule facts obligations errors]
  (let [ok? (and (empty? obligations) (empty? errors))]
    {:ok? ok?
     :errors errors
     :logic {:kernel :musn.logic/v1
             :judgement (if ok? :admissible :inadmissible)
             :witness {:rule rule
                       :facts facts}
             :obligations obligations}}))

(defn- action-facts [db action opts]
  (cond-> []
    (flag? db :plan) (conj {:fact :plan/present})
    (flag? db :selection) (conj {:fact :selection/present})
    (flag? db :write) (conj {:fact :write/present})
    (action-matches? db plan-required-actions)
    (conj {:fact :action/needs-plan
           :action (:action action)})
    (action-matches? db write-actions)
    (conj {:fact :action/writes
           :action (:action action)})
    (cost-matches? db consent-costs)
    (conj {:fact :action/costly
           :cost (:cost action)})
    (off-trail? db)
    (conj {:fact :trail/off-exceeded
           :trail (:trail opts)})))

(defn- action-obligations [db action opts]
  (cond-> []
    (and (action-matches? db plan-required-actions)
         (not (flag? db :plan)))
    (conj {:obligation :missing-plan
           :action (:action action)})
    (and (action-matches? db write-actions)
         (not (flag? db :selection)))
    (conj {:obligation :missing-selection
           :action (:action action)})
    (and (cost-matches? db consent-costs)
         (not (flag? db :plan)))
    (conj {:obligation :missing-consent
           :cost (:cost action)})
    (off-trail? db)
    (conj {:obligation :off-trail
           :trail (:trail opts)})))

(defn- turn-end-facts [db]
  (cond-> []
    (selection-mode? db :use) (conj {:fact :selection/mode :mode :use})
    (flag? db :write) (conj {:fact :write/present})))

(defn- turn-end-obligations [db]
  (cond-> []
    (and (selection-mode? db :use)
         (not (flag? db :write)))
    (conj {:obligation :no-write
           :mode :use})))

(defn check-action
  ([state action] (check-action state action {}))
  ([state action opts]
   (let [db (build-db state action opts)
         facts (action-facts db action opts)
         obligations (action-obligations db action opts)]
     (finalize-result :musn.action/constraints facts obligations []))))

(defn check-turn-end
  ([state] (check-turn-end state {}))
  ([state opts]
   (let [db (build-db state nil opts)
         facts (turn-end-facts db)
         obligations (turn-end-obligations db)]
     (finalize-result :musn.turn/end-constraints facts obligations []))))
