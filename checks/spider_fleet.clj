#!/usr/bin/env bb
(ns checks.spider-fleet
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [checks.library-graph-lint :as lint]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def root (str (fs/absolutize ".")))
(def runner (str (fs/path root "checks/spider_runner.clj")))
(def fleet-path (str (fs/path root "library/.spider"
                              (str "fleet-" (java.time.LocalDate/now) ".edn"))))
(def spider-agent "codex-20")

(defn parse-args [args]
  (when (odd? (count args)) (throw (ex-info "arguments must be --key value pairs" {})))
  (into {} (map (fn [[k v]] [(keyword (subs k 2)) v]) (partition 2 args))))

(defn csv [s] (vec (remove str/blank? (str/split (or s "") #","))))
(defn read-edn [path fallback]
  (if (fs/exists? path) (edn/read-string (slurp (str path))) fallback))
(def evidence-cache (atom nil))

(defn pattern-id [file]
  (-> (str (fs/relativize (fs/path root "library") file))
      (str/replace #"\\" "/") (str/replace #"\.flexiarg$" "")))

(defn rung-one-coverage [section]
  (let [patterns (map pattern-id (fs/glob (fs/path root "library" section) "*.flexiarg"))
        cache @evidence-cache
        hits (map #(get (:index cache) % []) patterns)]
    {:patterns (count patterns)
     :any (count (filter seq hits))
     :clean (count (filter #(some lint/clean-hit? %) hits))
     :clean-non-reflection (count (filter #(some lint/clean-non-reflection-hit? %) hits))}))

(defn print-coverage-table! [report]
  (println "basis" (pr-str (:evidence-basis report)))
  (println "| section | patterns | any | clean | clean-non-reflection |")
  (println "|---|---:|---:|---:|---:|")
  (doseq [[section data] (:sections report)]
    (let [{:keys [patterns any clean clean-non-reflection]} (:rung-one-coverage data)]
      (println (format "| %s | %d | %d | %d | %d |"
                       section patterns any clean clean-non-reflection)))))

(defn section-status [section]
  (let [dir (fs/path root "library" section)
        state (read-edn (fs/path dir ".spider/state.edn") {})
        intents (fs/glob (fs/path dir ".spider/receipts") "turn-*-intent.edn")
        processed (set (keep #(try (:pattern (read-edn % {})) (catch Exception _ nil)) intents))
        atts (read-edn (fs/path dir "attestations.edn") [])
        absences (read-edn (fs/path dir ".spider/absences.edn") [])
        failures (read-edn (fs/path dir ".spider/seat-failures.edn") [])
        section-atts (filter #(str/starts-with? (get-in % [:edge :from] "") (str section "/")) atts)]
    {:status (case (:phase state) :complete :done :paused :paused :running)
     :patterns-processed (count processed)
     :attestations (count section-atts)
     :absences (count absences)
     :seat-failures (count failures)
     :rung-one-coverage (rung-one-coverage section)
     :organised-attested (count (filter #(and (vector? (:state %))
                                              (= :attested-by (first (:state %)))) section-atts))
     :organised-proposed (count (filter #(= :proposed (:state %)) section-atts))}))

(def write-lock (Object.))
(defn refresh! [assignments]
  (locking write-lock
    (let [sections (into (sorted-map)
                         (map (fn [[section seat]]
                                [section (assoc (section-status section) :seat seat)]))
                         assignments)
          totals (apply merge-with +
                        (map #(select-keys % [:patterns-processed :attestations :absences
                                              :seat-failures :organised-attested
                                              :organised-proposed])
                             (vals sections)))
          report {:fleet/schema 1 :at (.toString (java.time.Instant/now))
                  :evidence-basis (:basis @evidence-cache)
                  :sections sections :total totals
                  :acyclicity-gate :serialised-by-library-lock}]
      (spit fleet-path (with-out-str (pprint/pprint report)))
      report)))

(defn run-section! [assignments section seat budget evidence-cache-path]
  (loop [remaining budget]
    (let [status (:status (section-status section))]
      (when (and (pos? remaining) (= :running status))
        (process/shell {:continue true :out :string :err :string
                        :extra-env {"SPIDER_EVIDENCE_CACHE" evidence-cache-path}}
                       "bb" "-cp" root runner "--section" section "--seat" seat "--budget" "1")
        (refresh! assignments)
        (recur (dec remaining))))))

(defn -main [& args]
  (let [{:keys [sections seats budget]} (parse-args args)
        sections (csv sections)
        seats (csv seats)
        budget (parse-long budget)]
    (when-not (and (seq sections) (seq seats) (nat-int? budget)
                   (every? #(re-matches #"zai-[0-9]+" %) seats))
      (throw (ex-info "usage: spider_fleet.clj --sections a,b --seats zai-1,zai-2 --budget N (generic zai seats only)" {})))
    (let [assignments (into {} (map-indexed (fn [i section]
                                              [section (nth seats (mod i (count seats)))])
                                            sections))
          ;; Build or load one basis-pinned live index before parallel workers start.
          cache (lint/ensure-live-evidence-index!
                 (str (fs/path root "library")) (set seats) spider-agent)
          _ (reset! evidence-cache cache)
          cache-path (or (:cache-path cache)
                         (lint/evidence-cache-path (:basis cache) (set seats) spider-agent))]
      (refresh! assignments)
      ;; mapv twice: create EVERY future before awaiting any. A lazy map/deref
      ;; pair created one future, blocked on it, then created the next — the
      ;; fleet ran one section at a time (found by codex-20 at wave-1 launch).
      (run! deref (mapv (fn [[section seat]]
                          (future (run-section! assignments section seat budget cache-path)))
                        assignments))
      (print-coverage-table! (refresh! assignments)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
