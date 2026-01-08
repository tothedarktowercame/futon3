(ns lab-explore-candidates
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn usage []
  (println "Usage: dev/lab-explore-candidates.clj --catalog PATH [--limit N] [--hotwords CSV] [--namespaces CSV]")
  (println "Prints: chosen<TAB>candidate1,candidate2,..."))

(defn parse-args [args]
  (loop [opts {:limit 4}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--catalog" (recur (assoc opts :catalog (second remaining)) (nnext remaining))
        "--limit" (recur (assoc opts :limit (Integer/parseInt (second remaining))) (nnext remaining))
        "--hotwords" (recur (assoc opts :hotwords (second remaining)) (nnext remaining))
        "--namespaces" (recur (assoc opts :namespaces (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn parse-csv [value]
  (when (and value (not (str/blank? value)))
    (vec (remove str/blank? (str/split value #",")))))

(defn read-catalog [path]
  (with-open [r (io/reader path)]
    (->> (line-seq r)
         (remove str/blank?)
         (map #(str/split % #"\t"))
         (map (fn [[pattern _tokipona _truth rationale hotwords]]
                {:pattern pattern
                 :namespace (first (str/split pattern #"/"))
                 :rationale (or rationale "")
                 :hotwords (parse-csv hotwords)}))
         (remove #(str/starts-with? (:pattern %) "#"))
         vec)))

(defn score-entry [entry hotwords]
  (let [tags (set (map str/lower-case (:hotwords entry)))
        targets (set (map str/lower-case hotwords))]
    (count (set/intersection tags targets))))

(defn select-candidates [entries {:keys [limit hotwords namespaces]}]
  (let [hotwords (or hotwords [])
        namespaces (set namespaces)
        filtered (cond
                   (seq namespaces) (filter #(contains? namespaces (:namespace %)) entries)
                   :else entries)
        scored (map (fn [entry]
                      (assoc entry :score (score-entry entry hotwords)))
                    filtered)
        sorted (if (seq hotwords)
                 (sort-by (juxt (comp - :score) :pattern) scored)
                 (sort-by :pattern scored))]
    (->> sorted
         (map :pattern)
         (take limit)
         vec)))

(defn -main [& args]
  (let [{:keys [help unknown catalog limit hotwords namespaces]} (parse-args args)]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? catalog) (do (println "--catalog is required") (usage) (System/exit 1))
      (not (.exists (io/file catalog))) (do (println "Catalog not found:" catalog) (System/exit 1))
      :else
      (let [entries (read-catalog catalog)
            hotwords (parse-csv hotwords)
            namespaces (parse-csv namespaces)
            candidates (select-candidates entries {:limit limit
                                                   :hotwords hotwords
                                                   :namespaces namespaces})]
        (when (empty? candidates)
          (println "No candidates found")
          (System/exit 1))
        (println (str (first candidates) "\t" (str/join "," candidates)))))))

(apply -main *command-line-args*)
