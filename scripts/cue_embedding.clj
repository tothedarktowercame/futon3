(ns scripts.cue-embedding
  "Convert tatami-context entries into fruit/paramitā embeddings."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [futon3.cue-embedding :as cue]))

(def input-path "resources/tatami-context.edn")
(def output-path "resources/tatami-cues.edn")

(defn- read-log []
  (let [file (io/file input-path)]
    (if (.exists file)
      (first (edn/read-string (slurp file)))
      [])))

(defn- ensure-map [entry]
  (cond
    (map? entry) entry
    (vector? entry) (first entry)
    (sequential? entry) (apply hash-map entry)
    :else {}))

(defn -main [& _]
  (let [entries (map ensure-map (read-log))
        annotated (mapv cue/annotate-entry entries)]
    (with-open [w (io/writer output-path)]
      (binding [*out* w]
        (pprint/pprint annotated)))
    (println (format "Wrote %d cue embeddings to %s" (count annotated) output-path))))
