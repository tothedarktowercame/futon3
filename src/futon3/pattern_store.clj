(ns futon3.pattern-store
  "Legacy filesystem cache for pattern entries; XTDB (Futon1) is canonical."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def ^:private store-path "resources/pattern_store.edn")

(defn- read-store []
  (with-open [r (io/reader (io/file store-path))]
    (edn/read {:eof nil} (java.io.PushbackReader. r))))

(defonce ^:private store (delay (read-store)))

(defn entries
  "Return all pattern records."
  []
  @store)

(defn pattern-by-id [id]
  (some #(when (= (:pattern/id %) id) %) (entries)))

(defn by-futon [futon-id]
  (->> (entries)
       (filter #(= (get-in % [:devmap :futon]) futon-id))
       vec))

(defn status-summary []
  (->> (entries)
       (group-by :status)
       (reduce (fn [acc [status records]]
                 (assoc acc status (count records)))
               {})))
