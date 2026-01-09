#!/usr/bin/env clojure
;; CLI for HUD operations - used by fuclaude/fucodex wrappers

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[futon3.fulab.hud :as hud])

(defn usage []
  (println "Usage: dev/lab-hud.clj <command> [options]")
  (println)
  (println "Commands:")
  (println "  build    Build HUD from intent/context")
  (println "  format   Format HUD as prompt block")
  (println "  parse    Parse agent response for pattern report")
  (println)
  (println "Options:")
  (println "  --intent TEXT        Task/goal description")
  (println "  --prototypes CSV     Comma-separated prototype IDs")
  (println "  --sigils CSV         Comma-separated sigil pairs (emoji/hanzi)")
  (println "  --limit N            Max pattern candidates (default 4)")
  (println "  --hud-file PATH      HUD EDN file (for format/parse)")
  (println "  --response-file PATH Response text file (for parse)"))

(defn parse-args [args]
  (loop [opts {:command nil}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--intent" (recur (assoc opts :intent (second remaining)) (nnext remaining))
        "--prototypes" (recur (assoc opts :prototypes (second remaining)) (nnext remaining))
        "--sigils" (recur (assoc opts :sigils (second remaining)) (nnext remaining))
        "--limit" (recur (assoc opts :limit (Integer/parseInt (second remaining))) (nnext remaining))
        "--hud-file" (recur (assoc opts :hud-file (second remaining)) (nnext remaining))
        "--response-file" (recur (assoc opts :response-file (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (if (nil? (:command opts))
          (recur (assoc opts :command arg) (rest remaining))
          (recur opts (rest remaining))))
      opts)))

(defn parse-csv [s]
  (when (and s (not (str/blank? s)))
    (vec (remove str/blank? (str/split s #",")))))

(defn parse-sigils [s]
  (when (and s (not (str/blank? s)))
    (->> (str/split s #",")
         (map str/trim)
         (keep (fn [pair]
                 (let [[emoji hanzi] (str/split pair #"/")]
                   (when (and emoji hanzi)
                     {:emoji (str/trim emoji) :hanzi (str/trim hanzi)}))))
         vec)))

(defn cmd-build [{:keys [intent prototypes sigils limit]}]
  (let [hud (hud/build-hud {:intent intent
                            :prototypes (parse-csv prototypes)
                            :sigils (parse-sigils sigils)
                            :pattern-limit (or limit 4)})]
    (prn hud)
    hud))

(defn cmd-format [{:keys [hud-file intent prototypes sigils limit]}]
  (let [hud (if hud-file
              (edn/read-string (slurp hud-file))
              (hud/build-hud {:intent intent
                              :prototypes (parse-csv prototypes)
                              :sigils (parse-sigils sigils)
                              :pattern-limit (or limit 4)}))]
    (println (hud/hud->prompt-block hud))))

(defn cmd-parse [{:keys [response-file]}]
  (if response-file
    (let [response (slurp response-file)
          report (hud/parse-agent-report response)]
      (prn report))
    (do
      (println "Error: --response-file required for parse command")
      (System/exit 1))))

(defn -main [& args]
  (let [{:keys [command help] :as opts} (parse-args args)]
    (cond
      help (usage)
      (nil? command) (usage)
      :else
      (case command
        "build" (cmd-build opts)
        "format" (cmd-format opts)
        "parse" (cmd-parse opts)
        (do (println "Unknown command:" command)
            (usage)
            (System/exit 1))))))

(apply -main *command-line-args*)
