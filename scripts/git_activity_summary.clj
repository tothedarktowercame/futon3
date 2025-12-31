(ns scripts.git-activity-summary
  "Summarise git activity JSON into an EDN snapshot Futon3 HUD/Futon4 loaders can ingest."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint])
  (:import (java.time LocalDate)
           (java.time.format DateTimeFormatter)
           (java.time.temporal ChronoUnit)))

(def default-input "resources/vitality/git_activity.json")
(def default-output "resources/vitality/git_summary.edn")

(def iso-formatter DateTimeFormatter/ISO_LOCAL_DATE)

(defn parse-date [^String s]
  (LocalDate/parse s iso-formatter))

(defn fmt-date [^LocalDate d]
  (.format iso-formatter d))

(defn normalize-day-keys [m]
  (into {}
        (map (fn [[k v]]
               [(if (keyword? k) (name k) (str k)) v])
             m)))

(defn build-day-map [repos]
  (reduce
   (fn [acc {:keys [label sphere by_day]}]
     (let [days (normalize-day-keys by_day)]
       (reduce
        (fn [acc [day count]]
          (-> acc
              (update-in [day :total] (fnil + 0) count)
              (update-in [day :spheres sphere] (fnil + 0) count)
              (update-in [day :repos label] (fnil + 0) count)))
        acc
        days)))
   {}
   repos))

(defn intensity [total]
  (cond
    (>= total 5) :heavy
    (>= total 3) :steady
    (pos? total) :light
    :else :idle))

(defn top-n [m n]
  (->> (sort-by val > m)
       (take n)
       (map first)
       vec))

(defn build-grid [since through day-map]
  (loop [day since acc []]
    (if (.isAfter day through)
      acc
      (let [k (fmt-date day)
            raw (get day-map k {:total 0 :spheres {} :repos {}})
            total (int (:total raw 0))
            spheres (:spheres raw)
            repos (:repos raw)
            dominant (when (seq spheres)
                       (-> (sort-by val > spheres) ffirst))]
        (recur (.plusDays day 1)
               (conj acc {:date k
                          :total total
                          :intensity (intensity total)
                          :dominant-sphere dominant
                          :spheres spheres
                          :repos repos
                          :top-repos (top-n repos 3)}))))))

(defn streaks [grid]
  (let [{:keys [longest] :as state}
        (reduce (fn [{:keys [current longest]} {:keys [total]}]
                  (if (pos? total)
                    (let [n (inc current)]
                      {:current n :longest (max longest n)})
                    {:current 0 :longest longest}))
                {:current 0 :longest 0}
                grid)
        current (loop [days (reverse grid) streak 0]
                  (if-let [{:keys [total]} (first days)]
                    (if (pos? total)
                      (recur (rest days) (inc streak))
                      streak)
                    streak))]
    {:longest (:longest state) :current current}))

(defn last-active [grid]
  (some #(when (pos? (:total %)) %) (reverse grid)))

(defn summarize [data]
  (let [{:keys [window repos aggregate generated_at]} data
        since (parse-date (:since window))
        through (parse-date (:through window))
        day-map (build-day-map repos)
        grid (build-grid since through day-map)
        streak (streaks grid)
        last-active-day (last-active grid)
        quiet-days (when last-active-day
                     (.between ChronoUnit/DAYS
                               (parse-date (:date last-active-day))
                               through))
        activity-span (.between ChronoUnit/DAYS since through)
        heatmap {:range {:since (:since window)
                         :through (:through window)
                         :days (inc (int activity-span))}
                 :days grid}]
    {:generated-at generated_at
     :window window
     :aggregate aggregate
     :spheres (->> (:by_sphere aggregate)
                   (map (fn [[sphere commits]]
                          {:sphere sphere :commits commits}))
                   (sort-by :commits >)
                   vec)
     :repos (let [total (:total_commits aggregate)]
              (->> repos
                   (map (fn [{:keys [label sphere total_commits days_active first_day last_day]}]
                          {:label label
                           :sphere sphere
                           :total total_commits
                           :days-active days_active
                           :first-day first_day
                           :last-day last_day
                           :share (when (pos? total)
                                    (double (/ total_commits total)))}))
                   (sort-by :total >)
                   vec))
     :streak streak
     :last-active (when last-active-day
                    (assoc last-active-day
                           :quiet-days (int quiet-days)))
     :heatmap heatmap}))

(defn parse-args [args]
  (loop [opts {:input default-input :output default-output}
         args args]
    (if-let [arg (first args)]
      (case arg
        "--input" (if-let [path (second args)]
                     (recur (assoc opts :input path) (nnext args))
                     (throw (ex-info "--input requires a path" {})))
        "--output" (if-let [path (second args)]
                      (recur (assoc opts :output path) (nnext args))
                      (throw (ex-info "--output requires a path" {})))
        "--stdout" (recur (assoc opts :stdout true :output nil) (next args))
        (throw (ex-info (str "Unknown argument: " arg) {:arg arg})))
      opts)))

(defn -main [& args]
  (let [{:keys [input output stdout]} (parse-args args)
        file (io/file input)]
    (when-not (.exists file)
      (throw (ex-info (str "Input file not found: " input) {:input input})))
    (with-open [reader (io/reader file)]
      (let [data (json/parse-stream reader true)
            summary (summarize data)]
        (if stdout
          (pprint/pprint summary)
          (do
            (io/make-parents output)
            (with-open [w (io/writer output)]
              (binding [*out* w]
                (pprint/pprint summary)))
            (println "Wrote git vitality summary to" output))))))
)
