(ns pattern-hints-test
  (:require [clojure.test :refer :all]
            [futon3.pattern-hints :as hints]))

(defn top-pattern-ids [resp]
  (mapv :id (:patterns resp)))

(deftest sigil-selection-affects-nearest-patterns
  (let [warm (hints/hints {:sigils [{:emoji "🏡" :hanzi "工"}] :pattern-limit 3})
        reflective (hints/hints {:sigils [{:emoji "🍵" :hanzi "心"}] :pattern-limit 3})
        warm-ids (top-pattern-ids warm)
        reflective-ids (top-pattern-ids reflective)]
    (is (seq warm-ids))
    (is (seq reflective-ids))
    (is (not= warm-ids reflective-ids))
    (is (every? string? warm-ids))
    (is (every? string? reflective-ids))))

(deftest patterns-returned-with-ascending-scores
  (let [result (hints/hints {:sigils [{:emoji "🍊" :hanzi "入"}] :pattern-limit 4})
        scores (map :score (:patterns result))]
    (is (= scores (sort scores)))
    (is (every? number? scores))))

(deftest multiarg-clauses-carry-distinct-ids
  (let [result (hints/hints {:sigils [{:emoji "🎎" :hanzi "口"}]
                             :pattern-limit 1})
        first-id (some-> result :patterns first :id)]
    (is (= "or/license-as-story" first-id))))
