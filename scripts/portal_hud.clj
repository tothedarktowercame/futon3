#!/usr/bin/env clj -M
;; Build a HUD JSON payload from futon3a portal candidates.
;;
;; Usage:
;;   clj -M -m scripts.portal-hud --intent "..." [--limit N] [--namespace NS]
;;                              [--portal-root PATH] [--out PATH]

(ns scripts.portal-hud
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [futon3.fulab.hud :as hud]
            [futon3.pattern-hints :as hints]))

(defn- usage []
  (str "usage: portal_hud --intent TEXT [--limit N] [--namespace NS] "
       "[--portal-root PATH] [--out PATH]\n"))

(defn- now-inst [] (java.util.Date.))

(defn- generate-hud-id []
  (str "hud-" (java.util.UUID/randomUUID)))

(defn- normalize-pattern-id [pid]
  (cond
    (nil? pid) nil
    (str/starts-with? pid "library/") (subs pid (count "library/"))
    :else pid))

(defn- parse-int [value fallback]
  (try
    (Integer/parseInt (str value))
    (catch Throwable _ fallback)))

(defn- parse-args [args]
  (loop [opts {:limit 4
               :portal-root (str (io/file ".." "futon3a"))}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--intent" (recur (assoc opts :intent (second remaining)) (nnext remaining))
        "--limit" (recur (assoc opts :limit (parse-int (second remaining) (:limit opts))) (nnext remaining))
        "--namespace" (recur (assoc opts :namespace (second remaining)) (nnext remaining))
        "--portal-root" (recur (assoc opts :portal-root (second remaining)) (nnext remaining))
        "--out" (recur (assoc opts :out (second remaining)) (nnext remaining))
        (recur opts (next remaining))))))

(defn- portal-path [portal-root]
  (str (io/file portal-root "scripts" "portal")))

(defn- portal-suggest
  [{:keys [intent limit namespace portal-root]}]
  (let [portal (portal-path portal-root)
        args (cond-> [portal "suggest" intent "--limit" (str limit) "--json"]
               (and namespace (not (str/blank? namespace)))
               (conj "--namespace" namespace))
        {:keys [exit out err]} (apply sh/sh args)]
    (when-not (zero? exit)
      (throw (ex-info "portal suggest failed" {:exit exit :err err})))
    (json/read-str out :key-fn keyword)))

(defn- pattern-meta-map []
  (into {}
        (map (fn [entry]
               [(normalize-pattern-id (:id entry)) entry]))
        (hints/all-patterns)))

(defn- enrich-candidate [cand meta-map]
  (let [pid (normalize-pattern-id (:id cand))
        meta (get meta-map pid)]
    (cond-> {:id pid
             :name (:name cand)
             :score (:score cand)
             :score-source :portal}
      (and meta (:summary meta)) (assoc :summary (:summary meta))
      (and meta (:pattern-ref meta)) (assoc :pattern-ref (:pattern-ref meta))
      (and meta (:maturity/phase meta)) (assoc :maturity-phase (:maturity/phase meta))
      (and meta (:evidence/count meta)) (assoc :evidence-count (:evidence/count meta))
      (and meta (:next-steps/count meta)) (assoc :next-steps-count (:next-steps/count meta))
      (and meta (:precision/prior meta)) (assoc :precision-prior (:precision/prior meta))
      (and meta (:sigils meta)) (assoc :sigils (:sigils meta)))))

(defn- build-hud [intent candidates]
  (let [suggested (some-> candidates first :id)]
    {:hud/id (generate-hud-id)
     :hud/timestamp (now-inst)
     :intent (or intent "unspecified")
     :prototypes []
     :sigils []
     :candidates candidates
     :aif (when suggested {:suggested suggested})
     :musn-help true
     :musn-hud false
     :agent-report nil}))

(defn- write-output [path payload]
  (spit path (json/write-str payload))
  (println "Wrote:" path))

(defn -main [& args]
  (let [{:keys [intent portal-root out] :as opts} (parse-args args)]
    (when (str/blank? intent)
      (println (usage))
      (System/exit 2))
    (when-not (.exists (io/file (portal-path portal-root)))
      (throw (ex-info "portal script not found" {:portal-root portal-root})))
    (let [raw (portal-suggest opts)
          meta-map (pattern-meta-map)
          candidates (mapv #(enrich-candidate % meta-map) raw)
          hud (build-hud intent candidates)
          payload {:ok true
                   :hud hud
                   :prompt (hud/hud->prompt-block hud)}
          out (or out (str (io/file "/tmp" "futon3-portal-hud.json")))]
      (write-output out payload)
      (println (json/write-str payload)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
