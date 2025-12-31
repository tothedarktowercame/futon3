(ns scripts.build-fruit-orb-corpus
  "Harvest descriptive text for fruits and pāramitās from the Markdown docs."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))


(def orbs-md (io/file "orbs.txt"))
(def output-path (io/file "resources/sigils/fruit-orb-corpus.edn"))

(defn- fruit-descriptions []
  (->> (edn/read-string (slurp "resources/sigils/fruits.edn"))
       (map (fn [fruit]
              {:fruit/id (:fruit/id fruit)
               :description (:summary fruit)}))))

(def orb-header-re #"^([🟡🔵⚪🟠🟤⚫🟣🟢🔮🔴])\s+\d+\.\s+([^–]+)")

(defn- parse-orb-blocks [text]
  (let [content (first (str/split text #"---PATTERN-VERDICT---"))
        lines (str/split-lines content)]
    (loop [remaining lines
           current nil
           acc []]
      (if-let [line (first remaining)]
        (if-let [[_ emoji title] (re-find orb-header-re line)]
          (recur (rest remaining)
                 {:emoji emoji :title (str/trim title) :body []}
                 (if current (conj acc current) acc))
          (let [trimmed (str/trim line)]
            (cond
              (str/blank? trimmed)
              (recur (rest remaining) current acc)

              :else
              (recur (rest remaining)
                     (if current
                       (update current :body conj trimmed)
                       current)
                     acc))))
        (if current (conj acc current) acc)))))

(defn- paramita-descriptions []
  (let [param-data (edn/read-string (slurp "resources/sigils/paramitas.edn"))]
    (for [block (parse-orb-blocks (slurp orbs-md))
          :let [match (some #(when (= (:orb %) (:emoji block)) %) param-data)]
          :when match]
      {:paramita/id (:paramita/id match)
       :emoji (:emoji block)
       :title (:title block)
       :description (str/join " " (:body block))})))

(defn -main [& _]
  (let [corpus {:fruits (fruit-descriptions)
                :paramitas (paramita-descriptions)}]
    (io/make-parents output-path)
    (spit output-path (with-out-str (prn corpus)))
    (println "Wrote" output-path)))
