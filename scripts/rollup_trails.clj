#!/usr/bin/env clj -M
;; Daily rollups of pattern advancement from lab session trails.
;;
;; Usage:
;;   scripts/rollup_trails.clj [--lab-root PATH] [--out-dir PATH]

(ns scripts.rollup-trails
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant ZoneOffset)
           (java.time.format DateTimeFormatter)))

(def ^:private day-format (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
(def ^:private advance-actions #{"implement" "update"})

(defn- parse-args [args]
  (loop [opts {:lab-root "lab"
               :out-dir "lab/rollups"}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--out-dir" (recur (assoc opts :out-dir (second remaining)) (nnext remaining))
        (recur opts (next remaining))))))

(defn- normalize-pattern-id [value]
  (cond
    (keyword? value) (subs (str value) 1)
    (string? value) value
    :else nil))

(defn- parse-instant [value]
  (cond
    (instance? java.time.Instant value) value
    (instance? java.util.Date value) (.toInstant ^java.util.Date value)
    (string? value) (try (Instant/parse value) (catch Exception _ nil))
    :else nil))

(defn- day-string [instant]
  (when instant
    (-> instant
        (.atZone ZoneOffset/UTC)
        (.toLocalDate)
        (.format day-format))))

(defn- event-pattern-id [event]
  (case (:event/type event)
    :pattern/use-claimed (normalize-pattern-id (get-in event [:payload :pur :pattern/id]))
    :pattern/action (normalize-pattern-id (or (:pattern/id event)
                                              (get-in event [:payload :pattern/id])))
    nil))

(defn- event-action [event]
  (case (:event/type event)
    :pattern/use-claimed :use
    :pattern/action (let [action (or (:pattern/action event)
                                     (get-in event [:payload :pattern/action]))]
                      (when action
                        (keyword action)))
    nil))

(defn- advances? [action]
  (or (= action :use)
      (contains? advance-actions (name action))))

(defn- add-event [acc session-id event]
  (let [day (day-string (parse-instant (:at event)))
        pattern-id (event-pattern-id event)
        action (event-action event)]
    (if (and day pattern-id action (advances? action))
      (-> acc
          (update-in [day :patterns pattern-id :count] (fnil inc 0))
          (update-in [day :patterns pattern-id :actions action] (fnil inc 0))
          (update-in [day :sessions] (fnil conj #{}) session-id)
          (update-in [day :event-count] (fnil inc 0)))
      acc)))

(defn- read-session-file [file]
  (try
    (edn/read-string (slurp file))
    (catch Exception _ nil)))

(defn- rollup-sessions [session-files]
  (reduce (fn [acc file]
            (let [session (read-session-file file)
                  sid (:session/id session)
                  events (:events session)]
              (if (and sid (seq events))
                (reduce (fn [acc' event]
                          (add-event acc' sid event))
                        acc
                        events)
                acc)))
          {}
          session-files))

(defn- write-rollup! [out-dir day payload]
  (let [path (io/file out-dir (str day ".edn"))]
    (io/make-parents path)
    (spit path (pr-str payload))
    (println "Wrote" (.getPath path))))

(defn- finalize-rollup [day {:keys [patterns sessions event-count]}]
  {:date day
   :generated-at (str (Instant/now))
   :event-count (or event-count 0)
   :sessions (sort (seq (or sessions #{})))
   :patterns (into {}
                   (map (fn [[pid info]]
                          [pid (assoc info :actions (into {} (:actions info)))]))
                   (or patterns {}))})

(defn -main [& args]
  (let [{:keys [lab-root out-dir]} (parse-args args)
        session-dir (io/file lab-root "sessions")
        session-files (filter #(str/ends-with? (.getName %) ".edn")
                              (file-seq session-dir))
        rollups (rollup-sessions session-files)]
    (doseq [[day payload] rollups]
      (write-rollup! out-dir day (finalize-rollup day payload)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
