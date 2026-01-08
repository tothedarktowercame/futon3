(ns lab-aif-explain
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.adapters.fulab :as fulab]))

(defn usage []
  (println "Usage: dev/lab-aif-explain.clj [--config PATH] [--candidate ID] [--anchors N] [--forecast N] [--uncertainty N]")
  (println "Explains fulab AIF config and prints a worked example (defaults if no config)."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--config" (recur (assoc opts :config (second remaining)) (nnext remaining))
        "--candidate" (recur (assoc opts :candidate (second remaining)) (nnext remaining))
        "--anchors" (recur (assoc opts :anchors (second remaining)) (nnext remaining))
        "--forecast" (recur (assoc opts :forecast (second remaining)) (nnext remaining))
        "--uncertainty" (recur (assoc opts :uncertainty (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn parse-int [value default]
  (try
    (Integer/parseInt value)
    (catch Throwable _ default)))

(defn read-config [path]
  (when path
    (try
      (edn/read-string (slurp path))
      (catch Throwable _ nil))))

(defn clamp [value min-val max-val]
  (-> value (max min-val) (min max-val)))

(defn text-score [value]
  (cond
    (string? value) (count (remove str/blank? (str/split value #"\s+")))
    (coll? value) (count value)
    :else 1))

(defn explain-example [config {:keys [candidate uncertainty] :as sample}]
  (let [anchor-count (or (:anchors sample) 0)
        forecast-count (or (:forecast sample) 0)
        {:keys [base anchors forecast]} (:g/weights config)
        base-weight base
        anchor-weight anchors
        forecast-weight forecast
        base-score (text-score candidate)
        anchors-score (text-score (range anchor-count))
        forecast-score (text-score (range forecast-count))
        g (+ (* base-weight base-score)
             (* anchor-weight anchors-score)
             (* forecast-weight forecast-score))
        tau-raw (double (/ (:tau/scale config) (max 1 uncertainty)))
        tau (clamp tau-raw (:tau/min config) (:tau/max config))]
    (println)
    (println "Worked example:")
    (println (format "  candidate text-score: %s" base-score))
    (println (format "  anchors count: %s" anchor-count))
    (println (format "  forecast count: %s" forecast-count))
    (println (format "  G = (%s * %s) + (%s * %s) + (%s * %s) = %.3f"
                     base-weight base-score anchor-weight anchors-score forecast-weight forecast-score g))
    (println (format "  tau = clamp(%s / %s, %s..%s) = %.3f"
                     (:tau/scale config) (max 1 uncertainty) (:tau/min config) (:tau/max config) tau))))

(defn -main [& args]
  (let [{:keys [help unknown config candidate anchors forecast uncertainty]} (parse-args args)
        candidate (or candidate "ants/white-space-scout")
        anchors (parse-int anchors 1)
        forecast (parse-int forecast 4)
        uncertainty (parse-int uncertainty 1)
        config-path config]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (and config-path (not (.exists (io/file config-path))))
      (do (println "Config file not found:" config-path) (System/exit 1))
      :else
      (let [user-config (or (read-config config-path) {})
            effective (merge fulab/default-config user-config)]
        (println "Fulab AIF config (effective):")
        (println (pr-str effective))
        (println)
        (println "Scoring semantics:")
        (println "  text-score: string -> word count, collection -> count, other -> 1")
        (println "  G = base*text-score(candidate) + anchors*count(anchors) + forecast*count(forecast)")
        (println "  tau = clamp(tau/scale / max(1, uncertainty), tau/min..tau/max)")
        (explain-example effective {:candidate candidate
                                    :anchors anchors
                                    :forecast forecast
                                    :uncertainty uncertainty})))))

(apply -main *command-line-args*)
