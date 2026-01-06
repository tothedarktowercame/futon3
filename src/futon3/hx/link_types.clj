(ns futon3.hx.link-types
  "Link type registry with optional futon1 refresh."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def default-types
  #{:uses
    :implements
    :refines
    :related
    :defines
    :documents
    :applies-pattern})

(defonce cache (atom {:types default-types
                      :source :local
                      :fetched-at nil}))

(defn- load-local-types []
  (try
    (if-let [res (io/resource "schemas/hypertext-link-types.edn")]
      (let [data (edn/read-string (slurp res))]
        (if (set? data) data default-types))
      default-types)
    (catch Exception _
      default-types)))

(defn refresh-from-local! []
  (let [types (load-local-types)]
    (reset! cache {:types types :source :local :fetched-at nil})
    types))

(defn refresh-from-futon1!
  "Optionally refresh link types from a futon1 endpoint.
   Expects EDN (set, vector, or {:types [...]}) and falls back on error."
  [url]
  (try
    (let [raw (slurp url)
          parsed (try (edn/read-string raw) (catch Exception _ nil))
          as-set (cond
                   (set? parsed) parsed
                   (sequential? parsed) (->> parsed (map keyword) set)
                   (map? parsed) (->> (:types parsed) (map keyword) set)
                   :else nil)
          types (or (seq as-set) (seq (load-local-types)))]
      (reset! cache {:types (set types) :source :futon1 :fetched-at (System/currentTimeMillis)})
      (set types))
    (catch Exception _
      (refresh-from-local!))))

(defn allowed-types []
  (:types @cache))

(defn allowed-type? [link-type]
  (contains? (allowed-types) link-type))
