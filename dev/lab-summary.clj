#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn usage []
  (println "Usage: dev/lab-summary.clj --session-id ID --lab-root PATH")
  (println "                           [--format org|json|text] [--output PATH]")
  (println)
  (println "Generates a summary of a completed lab session."))

(defn parse-args [args]
  (loop [opts {:format "org"}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--format" (recur (assoc opts :format (second remaining)) (nnext remaining))
        "--output" (recur (assoc opts :output (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn parse-duration [start end]
  (when (and start end)
    (try
      (let [fmt (java.time.format.DateTimeFormatter/ISO_INSTANT)
            t1 (java.time.Instant/parse start)
            t2 (java.time.Instant/parse end)
            dur (java.time.Duration/between t1 t2)
            hours (.toHours dur)
            mins (mod (.toMinutes dur) 60)
            secs (mod (.toSeconds dur) 60)]
        (cond
          (pos? hours) (format "%dh %dm" hours mins)
          (pos? mins) (format "%dm %ds" mins secs)
          :else (format "%ds" secs)))
      (catch Exception _ nil))))

(defn tool-summary [tool-uses]
  (->> tool-uses
       (map :tool)
       (frequencies)
       (sort-by val >)))

(defn extract-user-tasks [user-messages]
  "Extract likely task descriptions from user messages"
  (->> user-messages
       (map :text)
       (filter string?)
       (filter #(> (count %) 10))  ; skip very short messages
       (take 10)))  ; first 10 substantive messages

(defn truncate [s max-len]
  (if (and s (> (count s) max-len))
    (str (subs s 0 (- max-len 3)) "...")
    s))

(defn generate-summary [raw-data]
  (let [session-id (get raw-data (keyword "lab/session-id"))
        repo-root (get raw-data (keyword "lab/repo-root"))
        ts-start (get raw-data (keyword "lab/timestamp-start"))
        ts-end (get raw-data (keyword "lab/timestamp-end"))
        user-msgs (get raw-data (keyword "lab/user-messages") [])
        asst-msgs (get raw-data (keyword "lab/assistant-messages") [])
        files (get raw-data (keyword "lab/files-touched") [])
        tools (get raw-data (keyword "lab/tool-uses") [])
        duration (parse-duration ts-start ts-end)
        tool-counts (tool-summary tools)
        user-tasks (extract-user-tasks user-msgs)]
    {:session-id session-id
     :repo-root repo-root
     :timestamp-start ts-start
     :timestamp-end ts-end
     :duration duration
     :user-message-count (count user-msgs)
     :assistant-message-count (count asst-msgs)
     :total-turns (+ (count user-msgs) (count asst-msgs))
     :files-touched files
     :files-count (count files)
     :tool-counts tool-counts
     :total-tool-calls (count tools)
     :user-tasks user-tasks
     :first-message (truncate (:text (first user-msgs)) 200)
     :last-message (truncate (:text (last user-msgs)) 200)}))

(defn format-org [summary]
  (let [{:keys [session-id repo-root timestamp-start timestamp-end duration
                user-message-count assistant-message-count total-turns
                files-touched files-count tool-counts total-tool-calls
                user-tasks first-message last-message]} summary]
    (str "#+TITLE: Session Summary: " session-id "\n"
         "#+DATE: " timestamp-end "\n"
         "\n"
         "* Overview\n"
         "| Property | Value |\n"
         "|----------|-------|\n"
         "| Session ID | " session-id " |\n"
         "| Repository | " repo-root " |\n"
         "| Started | " timestamp-start " |\n"
         "| Ended | " timestamp-end " |\n"
         "| Duration | " (or duration "unknown") " |\n"
         "| Total turns | " total-turns " (" user-message-count " user, " assistant-message-count " assistant) |\n"
         "| Files touched | " files-count " |\n"
         "| Tool calls | " total-tool-calls " |\n"
         "\n"
         "* First Message\n"
         "#+BEGIN_QUOTE\n"
         (or first-message "(none)") "\n"
         "#+END_QUOTE\n"
         "\n"
         "* Last Message\n"
         "#+BEGIN_QUOTE\n"
         (or last-message "(none)") "\n"
         "#+END_QUOTE\n"
         "\n"
         "* Tool Usage\n"
         (if (seq tool-counts)
           (str "| Tool | Count |\n"
                "|------|-------|\n"
                (apply str (for [[tool cnt] tool-counts]
                             (str "| " tool " | " cnt " |\n"))))
           "(no tool calls)\n")
         "\n"
         "* Files Touched\n"
         (if (seq files-touched)
           (apply str (for [f files-touched]
                        (str "- " f "\n")))
           "(none)\n")
         "\n"
         "* User Messages (first 10)\n"
         (if (seq user-tasks)
           (apply str (map-indexed
                       (fn [i msg]
                         (str "** " (inc i) ". \n"
                              "#+BEGIN_QUOTE\n"
                              (truncate msg 500) "\n"
                              "#+END_QUOTE\n\n"))
                       user-tasks))
           "(none)\n"))))

(defn format-json [summary]
  (json/write-str summary {:escape-slash false}))

(defn format-text [summary]
  (let [{:keys [session-id duration total-turns files-count total-tool-calls
                first-message last-message]} summary]
    (str "Session: " session-id "\n"
         "Duration: " (or duration "?") " | "
         "Turns: " total-turns " | "
         "Files: " files-count " | "
         "Tools: " total-tool-calls "\n"
         "\n"
         "First: " (truncate first-message 100) "\n"
         "Last: " (truncate last-message 100) "\n")))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root format output]} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      (nil? lab-root) (do (println "--lab-root is required") (usage) (System/exit 1))
      :else
      (let [raw-path (str (io/file lab-root "raw" (str session-id ".json")))]
        (if-not (.exists (io/file raw-path))
          (do (println "[lab-summary] Raw file not found:" raw-path)
              (println "[lab-summary] Run fulab-export first.")
              (System/exit 1))
          (let [raw-data (json/read-str (slurp raw-path) :key-fn keyword)
                summary (generate-summary raw-data)
                formatted (case format
                            "org" (format-org summary)
                            "json" (format-json summary)
                            "text" (format-text summary)
                            (format-org summary))
                out-path (or output
                             (str (io/file lab-root "summaries" (str session-id "." format))))]
            (if output
              (do
                (io/make-parents out-path)
                (spit out-path formatted)
                (println "[lab-summary] Written to:" out-path))
              (println formatted))))))))

(apply -main *command-line-args*)
