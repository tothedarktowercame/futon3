#!/usr/bin/env bb
;; vitality-sessions.clj — Infer work sessions from agent interaction timestamps
;;
;; Primary source (unified):
;; - lab/activity/log.ndjson — all agents POST here
;;
;; Fallback sources (legacy):
;; - Claude Code JSONL logs (~/.claude/projects/*/*.jsonl)
;; - aob-chatgpt EDN log (resources/tatami-context.edn)
;;
;; Output: Session records with arrival/departure times
;;
;; Usage:
;;   bb scripts/vitality-sessions.clj
;;   bb scripts/vitality-sessions.clj --gap-hours 8
;;   bb scripts/vitality-sessions.clj --json
;;   bb scripts/vitality-sessions.clj --unified-only

(ns vitality-sessions
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.time Instant Duration ZoneId]
           [java.time.format DateTimeFormatter]))

(def default-gap-hours 6)

(defn parse-iso-timestamp [s]
  (try
    (Instant/parse s)
    (catch Exception _ nil)))

(defn parse-java-date-string [s]
  ;; Parses "Wed Jan 28 19:56:17 UTC 2026" format
  (try
    (let [fmt (DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss zzz yyyy"
                                           java.util.Locale/ENGLISH)]
      (.toInstant (java.time.ZonedDateTime/parse s fmt)))
    (catch Exception _ nil)))

(defn parse-timestamp [s]
  ;; Try ISO first, then Java Date toString format
  (or (parse-iso-timestamp s)
      (parse-java-date-string s)))

(defn instant->local [^Instant inst]
  (-> inst
      (.atZone (ZoneId/systemDefault))
      (.toLocalDateTime)))

(defn format-local [^Instant inst]
  (-> (instant->local inst)
      (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))))

;; --- Unified activity log (primary source) ---

(defn parse-activity-log [file]
  (try
    (->> (slurp (str file))
         str/split-lines
         (keep (fn [line]
                 (when-not (str/blank? line)
                   (try
                     (let [entry (edn/read-string line)
                           ts (parse-timestamp (:at entry))]
                       (when ts
                         {:source (or (:source entry) (:agent entry))
                          :agent (:agent entry)
                          :timestamp ts
                          :session-id (:session/id entry)
                          :event-type (:event/type entry)}))
                     (catch Exception _ nil))))))
    (catch Exception _ [])))

(defn collect-unified-timestamps []
  (let [file (fs/file "lab/activity/log.ndjson")]
    (if (fs/exists? file)
      (parse-activity-log file)
      [])))

;; --- Claude Code JSONL logs (legacy fallback) ---

(defn claude-log-dirs []
  (let [base (fs/expand-home "~/.claude/projects")]
    (when (fs/exists? base)
      (fs/list-dir base))))

(defn parse-claude-jsonl [file]
  (try
    (->> (slurp (str file))
         str/split-lines
         (keep (fn [line]
                 (when-not (str/blank? line)
                   (try
                     (let [obj (json/parse-string line true)]
                       (when (and (= "user" (:type obj))
                                  (:timestamp obj))
                         {:source :claude
                          :timestamp (parse-iso-timestamp (:timestamp obj))
                          :session-id (:sessionId obj)}))
                     (catch Exception _ nil))))))
    (catch Exception _ [])))

(defn collect-claude-timestamps []
  (->> (claude-log-dirs)
       (mapcat (fn [dir]
                 (when (fs/directory? dir)
                   (fs/glob dir "*.jsonl"))))
       (mapcat parse-claude-jsonl)
       (filter :timestamp)))

;; --- aob-chatgpt EDN log ---

(defn parse-tatami-edn [file]
  (try
    (let [content (slurp (str file))
          entries (edn/read-string content)]
      (->> (if (vector? entries) entries [])
           (keep (fn [entry]
                   (when-let [ts (:timestamp entry)]
                     {:source :aob-chatgpt
                      :timestamp (parse-iso-timestamp ts)
                      :session-id (:session-id entry)})))))
    (catch Exception _ [])))

(defn collect-tatami-timestamps []
  (let [file (fs/file "resources/tatami-context.edn")]
    (if (fs/exists? file)
      (parse-tatami-edn file)
      [])))

;; --- Session inference ---

(defn group-into-sessions [timestamps gap-hours]
  (let [gap-ms (* gap-hours 60 60 1000)
        sorted (sort-by :timestamp timestamps)]
    (when (seq sorted)
      (loop [remaining (rest sorted)
             current-session [(first sorted)]
             sessions []]
        (if (empty? remaining)
          (conj sessions current-session)
          (let [prev-ts (:timestamp (last current-session))
                curr (first remaining)
                curr-ts (:timestamp curr)
                gap (when (and prev-ts curr-ts)
                      (.toMillis (Duration/between prev-ts curr-ts)))]
            (if (and gap (> gap gap-ms))
              ;; New session
              (recur (rest remaining)
                     [curr]
                     (conj sessions current-session))
              ;; Same session
              (recur (rest remaining)
                     (conj current-session curr)
                     sessions))))))))

(defn session-summary [session]
  (let [sorted (sort-by :timestamp session)
        first-ts (:timestamp (first sorted))
        last-ts (:timestamp (last sorted))
        duration (when (and first-ts last-ts)
                   (Duration/between first-ts last-ts))
        sources (set (keep :source session))
        agents (set (keep :agent session))]
    {:arrived (format-local first-ts)
     :left (format-local last-ts)
     :duration-hours (when duration
                       (/ (.toMinutes duration) 60.0))
     :interactions (count session)
     :sources sources
     :agents agents}))

;; --- Main ---

(defn -main [& args]
  (let [opts (into {} (map vec (partition 2 args)))
        gap-hours (or (some-> (get opts "--gap-hours") parse-long)
                      default-gap-hours)
        json-output? (contains? (set args) "--json")
        unified-only? (contains? (set args) "--unified-only")]

    (let [unified-ts (collect-unified-timestamps)
          ;; Use legacy sources as fallback if unified is empty or not --unified-only
          legacy-ts (when-not unified-only?
                      (concat (collect-claude-timestamps)
                              (collect-tatami-timestamps)))
          ;; Prefer unified, supplement with legacy
          all-ts (if (seq unified-ts)
                   (if unified-only?
                     unified-ts
                     (concat unified-ts legacy-ts))
                   legacy-ts)
          sessions (group-into-sessions all-ts gap-hours)
          summaries (map session-summary sessions)]

      (if json-output?
        (println (json/generate-string summaries {:pretty true}))
        (do
          (println (format "Found %d interactions across %d sessions (gap threshold: %dh)"
                           (count all-ts)
                           (count sessions)
                           gap-hours))
          (when (seq unified-ts)
            (println (format "  (unified log: %d entries)" (count unified-ts))))
          (println)
          (doseq [[i s] (map-indexed vector summaries)]
            (println (format "Session %d:" (inc i)))
            (println (format "  Arrived: %s" (:arrived s)))
            (println (format "  Left:    %s" (:left s)))
            (println (format "  Duration: %.1f hours" (or (:duration-hours s) 0.0)))
            (println (format "  Interactions: %d" (:interactions s)))
            (println (format "  Agents: %s" (str/join ", " (map name (filter some? (:agents s))))))
            (println (format "  Sources: %s" (str/join ", " (map name (filter some? (:sources s))))))
            (println)))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
