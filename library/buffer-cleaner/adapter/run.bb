#!/usr/bin/env bb
;; Run the adapter on a real state packet: classify under both wirings,
;; emit dry-run receipts + meters. NO live kill (execute! refuses).
(require '[cheshire.core :as json]
         '[clojure.pprint :as pp]
         '[buffer-cleaner.classify :as c])

(def raw (json/parse-string (slurp (or (first *command-line-args*) "runs/state-packet.json"))))
;; normalize JSON booleans absent->"false" style already handled by truthy? checks
(def packet {:buffers (mapv (fn [b] {:name (get b "name")
                                     :kind (get b "kind")
                                     :file (str (get b "file"))
                                     :modified (str (get b "modified"))
                                     :process-gone (get b "process-gone")
                                     :visible (get b "visible")
                                     :display-age-seconds (get b "display-age-seconds")})
                           (get raw "buffers"))})
(def categories (c/load-edn "params/categories.edn"))
(defn wiring [p] (assoc (c/load-edn p) :wiring/id (keyword (last (re-find #"/(\w+)\.edn$" p)))))
(def wirings [(wiring "wirings/aggressive.edn") (wiring "wirings/conservative.edn")])
(doseq [w wirings]
  (let [r (c/classify-packet packet w categories)]
    (spit (str "runs/receipts-" (name (:wiring r)) ".edn")
          (with-out-str (pp/pprint (select-keys r [:wiring :receipts :meters :sources]))))
    (pp/pprint {:wiring (:wiring r) :meters (:meters r)})))
