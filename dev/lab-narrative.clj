#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[clojure.java.shell :refer [sh]])

(defn parse-args [args]
  (loop [opts {:word-count 150
               :provider "claude"
               :style "digest"}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--words" (recur (assoc opts :word-count (Integer/parseInt (second remaining))) (nnext remaining))
        "--provider" (recur (assoc opts :provider (second remaining)) (nnext remaining))
        "--style" (recur (assoc opts :style (second remaining)) (nnext remaining))
        "--prompt-only" (recur (assoc opts :prompt-only true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn usage []
  (println "Usage: dev/lab-narrative.clj --session-id ID --lab-root PATH")
  (println "                             [--words N] [--provider claude|prompt] [--style digest|technical|casual]")
  (println "                             [--prompt-only]")
  (println)
  (println "Generates an AI-powered narrative summary of a session.")
  (println)
  (println "Styles:")
  (println "  digest    - Concise summary like a society paper abstract (default)")
  (println "  technical - Focus on technical accomplishments and decisions")
  (println "  casual    - Conversational recap"))

(defn truncate [s max-len]
  (if (and s (string? s) (> (count s) max-len))
    (str (subs s 0 (- max-len 3)) "...")
    (or s "")))

(defn build-prompt [{:keys [session-id repo-root timestamp-start timestamp-end duration
                            user-messages assistant-messages files-touched tool-counts
                            word-count style]}]
  (let [style-instruction (case style
                            "digest" "Write in the style of a learned society paper abstract: substantive, precise, and engaging without being dry or sensational."
                            "technical" "Write in a technical style focusing on architectural decisions, code changes, and engineering outcomes."
                            "casual" "Write in a casual, conversational tone as if explaining to a colleague over coffee."
                            "Write a clear, professional summary.")
        user-excerpts (->> user-messages
                           (take 8)
                           (map #(truncate (:text %) 300))
                           (str/join "\n---\n"))
        files-list (str/join ", " (take 10 files-touched))
        tools-list (->> tool-counts
                        (take 5)
                        (map (fn [[tool cnt]] (str tool "(" cnt ")")))
                        (str/join ", "))]
    (str "You are summarizing a coding session between a human developer and an AI assistant.\n\n"
         "SESSION METADATA:\n"
         "- Repository: " repo-root "\n"
         "- Duration: " (or duration "unknown") "\n"
         "- Messages: " (count user-messages) " from user, " (count assistant-messages) " from assistant\n"
         "- Files touched: " (count files-touched) " (" files-list (when (> (count files-touched) 10) ", ...") ")\n"
         "- Tool usage: " tools-list "\n"
         "\n"
         "USER MESSAGES (excerpts showing goals and progression):\n"
         user-excerpts "\n"
         "\n"
         "TASK: Write a " word-count "-word narrative summary of this session.\n"
         style-instruction "\n"
         "\n"
         "Cover: What was the developer trying to accomplish? What was achieved? "
         "What's the significance of the work? Any notable challenges or decisions?\n"
         "\n"
         "Do NOT use bullet points. Write flowing prose. Be specific about what was built or changed.")))

(defn call-claude [prompt]
  (let [result (sh "claude" "-p" :in prompt)]
    (if (zero? (:exit result))
      (str/trim (:out result))
      (throw (ex-info "Claude CLI failed" {:exit (:exit result) :err (:err result) :out (:out result)})))))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root word-count provider style prompt-only]} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      (nil? lab-root) (do (println "--lab-root is required") (usage) (System/exit 1))
      :else
      (let [raw-path (str (io/file lab-root "raw" (str session-id ".json")))]
        (if-not (.exists (io/file raw-path))
          (do (println "[lab-narrative] Session not exported:" session-id)
              (System/exit 1))
          (let [raw-data (json/read-str (slurp raw-path) :key-fn keyword)
                kw (fn [s] (keyword s))
                session-id (get raw-data (kw "lab/session-id"))
                repo-root (get raw-data (kw "lab/repo-root"))
                ts-start (get raw-data (kw "lab/timestamp-start"))
                ts-end (get raw-data (kw "lab/timestamp-end"))
                user-msgs (get raw-data (kw "lab/user-messages") [])
                asst-msgs (get raw-data (kw "lab/assistant-messages") [])
                files (get raw-data (kw "lab/files-touched") [])
                tools (get raw-data (kw "lab/tool-uses") [])
                tool-counts (->> tools (map :tool) frequencies (sort-by val >))
                ;; Parse duration
                duration (try
                           (let [t1 (java.time.Instant/parse ts-start)
                                 t2 (java.time.Instant/parse ts-end)
                                 dur (java.time.Duration/between t1 t2)
                                 mins (.toMinutes dur)]
                             (if (>= mins 60)
                               (format "%dh %dm" (quot mins 60) (mod mins 60))
                               (format "%dm" mins)))
                           (catch Exception _ nil))
                context {:session-id session-id
                         :repo-root repo-root
                         :timestamp-start ts-start
                         :timestamp-end ts-end
                         :duration duration
                         :user-messages user-msgs
                         :assistant-messages asst-msgs
                         :files-touched files
                         :tool-counts tool-counts
                         :word-count word-count
                         :style style}
                prompt (build-prompt context)]
            (if prompt-only
              (do
                (println "=== PROMPT FOR AI SUMMARY ===")
                (println prompt)
                (println "=== END PROMPT ==="))
              (case provider
                "claude" (do
                           (println "[lab-narrative] Generating summary via Claude CLI...")
                           (try
                             (let [summary (call-claude prompt)]
                               (println)
                               (println summary))
                             (catch Exception e
                               (println "[lab-narrative] Error calling Claude:" (.getMessage e))
                               (println "[lab-narrative] Try --prompt-only to get the prompt for manual use")
                               (System/exit 1))))
                "prompt" (do
                           (println prompt))
                (do
                  (println "[lab-narrative] Unknown provider:" provider)
                  (System/exit 1))))))))))

(apply -main *command-line-args*)
