#!/usr/bin/env clj -M
;; Export MUSN turn/start HUD payloads as proof artifacts.
;;
;; Usage:
;;   scripts/export_hud_artifacts.clj [--lab-root PATH] [--out-dir PATH]

(ns scripts.export-hud-artifacts
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant)))

(defn- parse-args [args]
  (loop [opts {:lab-root "lab"
               :out-dir "lab/artifacts/hud"}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--out-dir" (recur (assoc opts :out-dir (second remaining)) (nnext remaining))
        (recur opts (next remaining))))))

(defn- read-log-line [line]
  (try
    (edn/read-string {:readers {'object (fn [v] (last v))}} line)
    (catch Exception _ nil)))

(defn- export-entry! [out-dir source entry]
  (let [sid (:session/id entry)
        turn (get-in entry [:req :turn])
        hud (get-in entry [:req :hud])
        ts (or (:ts entry) (str (Instant/now)))]
    (when (and sid turn hud)
      (let [path (io/file out-dir (str sid "-turn-" turn ".edn"))
            payload {:session/id sid
                     :turn turn
                     :timestamp (str ts)
                     :hud hud
                     :source source}]
        (io/make-parents path)
        (spit path (pr-str payload))
        (println "Wrote" (.getPath path))))))

(defn- export-file! [out-dir file]
  (with-open [r (io/reader file)]
    (doseq [line (line-seq r)
            :let [entry (read-log-line line)]
            :when (and (map? entry) (= :turn/start (:op entry)))]
      (export-entry! out-dir (.getPath file) entry))))

(defn -main [& args]
  (let [{:keys [lab-root out-dir]} (parse-args args)
        log-dir (io/file lab-root "musn")
        log-files (filter #(and (.isFile %)
                                (str/ends-with? (.getName %) ".edn"))
                          (file-seq log-dir))]
    (doseq [file log-files]
      (export-file! out-dir file))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
