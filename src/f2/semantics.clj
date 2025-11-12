(ns f2.semantics
  "Lightweight reasoning helpers that operate on transport history entries."
  (:require [clojure.set :as set]))

(defn- recent-by-client [history]
  (group-by :client history))

(defn suggest-links
  "Suggest link candidates when multiple clients produce identical outputs."
  [history]
  (let [by-output (->> history
                       (filter #(contains? % :output))
                       (group-by :output))]
    (->> by-output
         (keep (fn [[output entries]]
                 (when (> (count (set (map :client entries))) 1)
                   {:type :shared-result
                    :support-eids (mapv #(select-keys % [:client :stamp]) entries)
                    :confidence 0.6
                    :output output})))
         vec)))

(defn instantiate-pattern
  "Given a naive pattern definition, fill slots with recent client entries."
  [history pattern]
  (let [slots (or (:pattern/slots pattern) [])
        grouped (recent-by-client history)]
    (->> grouped
         (map (fn [[client entries]]
                {:pattern (select-keys pattern [:pattern/id :pattern/name])
                 :client client
                 :confidence 0.4
                 :slots (mapv (fn [slot]
                                (let [latest (last entries)]
                                  {:slot slot
                                   :value (:output latest)
                                   :prov (:stamp latest)}))
                              slots)}))
         vec)))

(defn check-consistency
  "Flag clients that churn too quickly (rudimentary consistency check)."
  [history]
  (let [counts (frequencies (map :client history))]
    (->> counts
         (keep (fn [[client n]]
                 (when (> n 20)
                   {:client client
                    :issue :high-volume
                    :confidence 0.3
                    :samples n})))
         vec)))
