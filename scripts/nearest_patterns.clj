#!/usr/bin/env clj -M
;; Find nearest patterns by sigil or glove similarity.
;;
;; Usage:
;;   scripts/nearest-patterns <pattern-id> [--limit N] [--method glove|sigil|combined] [--json]

(ns scripts.nearest-patterns
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [futon3.pattern-hints :as hints]))

(defn- usage []
  (str "usage: nearest-patterns <pattern-id> [--limit N] [--method glove|sigil|combined] [--json]\n"))

(defn- parse-int [value fallback]
  (try
    (Integer/parseInt (str value))
    (catch Throwable _ fallback)))

(defn- normalize-pattern-id [pid]
  (cond
    (nil? pid) nil
    (str/starts-with? pid "library/") (subs pid (count "library/"))
    :else pid))

(defn- parse-method [value]
  (let [v (some-> value str/lower-case keyword)]
    (case v
      :sigil :sigil
      :glove :glove
      :combined :combined
      :combined)))

(defn- parse-args [args]
  (loop [opts {:limit 6
               :method :combined
               :json? false}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--limit" (recur (assoc opts :limit (parse-int (second remaining) (:limit opts))) (nnext remaining))
        "--method" (recur (assoc opts :method (parse-method (second remaining))) (nnext remaining))
        "--json" (recur (assoc opts :json? true) (next remaining))
        (if (:pattern-id opts)
          (recur opts (next remaining))
          (recur (assoc opts :pattern-id (first remaining)) (next remaining)))))))

(defn- emit [payload json?]
  (if json?
    (println (json/write-str payload))
    (prn payload)))

(defn -main [& args]
  (let [{:keys [pattern-id limit method json?]} (parse-args args)
        pattern-id (normalize-pattern-id pattern-id)]
    (when (str/blank? pattern-id)
      (println (usage))
      (System/exit 2))
    (let [neighbors (hints/nearest-patterns pattern-id {:limit limit :method method})
          payload {:pattern-id pattern-id
                   :method method
                   :limit limit
                   :neighbors neighbors}]
      (emit payload json?))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
