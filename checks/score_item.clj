(ns score-item
  "Score a realised outcome against a stated item: does it fall in the predicted
   support, does it fire the falsifier, and what is the posterior?

   This is the DERIVED check — Joe: Q(o|pi) cannot be known in advance but it can
   be derived. The item states Q; this says how Q fared."
  (:require [clojure.edn :as edn] [clojure.core.logic :as l]))

(def item (edn/read-string (slurp "checks/item-s001.edn")))

(defn predictedo
  "Relation: outcome `o` carries strictly positive predicted mass."
  [o]
  (l/membero o (vec (keep (fn [[k v]] (when (pos? v) k)) (:Q item)))))

(defn falsifiero [o] (l/== o (get-in item [:falsifier :outcome])))

(defn score [realised]
  (let [in-support? (seq (l/run* [q] (l/== q realised) (predictedo realised)))
        refuted?    (seq (l/run* [q] (l/== q realised) (falsifiero realised)))
        q           (:Q item)
        ;; posterior over the disposition, from the outcome that occurred
        post (cond (= realised :O2) {:sharer 1.0 :snatcher 0.0}
                   (= realised :O4) {:sharer 0.0 :snatcher 1.0}
                   :else            (get-in item [:hidden-state :prior]))]
    {:realised realised
     :predicted-mass (get q realised 0.0)
     :in-support? (boolean in-support?)
     :refuted? (boolean refuted?)
     :posterior post
     :prior-entropy-nats (:Q/entropy-nats item)
     :posterior-entropy-nats (if (#{:O2 :O4} realised) 0.0 (:Q/entropy-nats item))}))

(defn -main [& _]
  (println "ITEM" (:item/id item) "— status" (:item/status item) "\n")
  (println "Q:" (:Q item) " entropy" (:Q/entropy-nats item) "nats")
  (println "falsifier:" (get-in item [:falsifier :outcome]) "(zero predicted mass)\n")
  (doseq [o [:O2 :O4 :O3 :O1]]
    (let [r (score o)]
      (println (format "  if %s -> mass %.2f  in-support %-5s refuted %-5s  entropy %.4f -> %.4f"
                       (name o) (double (:predicted-mass r)) (:in-support? r) (:refuted? r)
                       (double (:prior-entropy-nats r)) (double (:posterior-entropy-nats r))))))
  (println)
  (println "The item is GOOD because two of the four outcomes would refute it")
  (println "(O1 and O3 carry zero mass), and the other two collapse the spread"))
