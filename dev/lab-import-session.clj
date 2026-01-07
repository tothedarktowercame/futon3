#!/usr/bin/env clojure

;; lab-import-session.clj
;; Converts Claude Code JSONL transcript to session EDN format for PUR validation.
;;
;; Usage:
;;   clojure -M dev/lab-import-session.clj --jsonl PATH --session-id ID [--pattern-id PID] [--intent TEXT]

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[clojure.pprint :as pprint])
(require '[cheshire.core :as json])

(defn usage []
  (println "Usage: dev/lab-import-session.clj --jsonl PATH --session-id ID [OPTIONS]")
  (println)
  (println "Converts Claude Code JSONL transcript to session EDN for PUR validation.")
  (println)
  (println "Options:")
  (println "  --jsonl PATH        Path to Claude Code JSONL transcript (required)")
  (println "  --session-id ID     Session ID for output file (required)")
  (println "  --pattern-id PID    Pattern ID for clock-in (optional)")
  (println "  --intent TEXT       Intent description for clock-in (optional)")
  (println "  --lab-root PATH     Lab directory (default: ./lab)")
  (println "  --dry-run           Print output instead of writing file")
  (println "  --help              Show this help"))

(defn parse-args [args]
  (loop [opts {:dry-run false
               :lab-root "lab"}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--jsonl" (recur (assoc opts :jsonl (second remaining)) (nnext remaining))
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--pattern-id" (recur (assoc opts :pattern-id (second remaining)) (nnext remaining))
        "--intent" (recur (assoc opts :intent (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn read-json-line [line]
  (try
    (json/parse-string line true)
    (catch Exception _ nil)))

(defn parse-timestamp [ts-str]
  "Parse ISO timestamp string to #inst"
  (when (and ts-str (string? ts-str))
    (try
      (java.util.Date/from
       (java.time.Instant/parse ts-str))
      (catch Exception _ nil))))

(defn extract-tool-uses
  "Extract tool use blocks from assistant message content"
  [content]
  (when (sequential? content)
    (->> content
         (filter #(and (map? %) (= "tool_use" (:type %))))
         (map (fn [tu]
                {:tool (:name tu)
                 :id (:id tu)
                 :input (:input tu)})))))

(defn tool-use->event
  "Convert a tool use to a session event"
  [tool-use timestamp]
  (let [{:keys [tool input]} tool-use
        file-path (or (:file_path input) (:path input))]
    (case tool
      "Edit"
      {:event/type :code/edit
       :file file-path
       :fn nil
       :action :modified
       :description (str "Edit: " (when-let [old (:old_string input)]
                                    (str (subs old 0 (min 50 (count old))) "...")))
       :at timestamp}

      "Write"
      {:event/type :code/edit
       :file file-path
       :fn nil
       :action :added
       :description (str "Write: " file-path)
       :at timestamp}

      "Read"
      {:event/type :code/read
       :file file-path
       :at timestamp}

      "Bash"
      {:event/type :shell/command
       :command (:command input)
       :description (:description input)
       :at timestamp}

      "Glob"
      {:event/type :file/search
       :pattern (:pattern input)
       :path (:path input)
       :at timestamp}

      "Grep"
      {:event/type :content/search
       :pattern (:pattern input)
       :path (:path input)
       :at timestamp}

      ;; Default: capture as generic tool event
      {:event/type :tool/call
       :tool tool
       :input input
       :at timestamp})))

(defn process-message
  "Process a JSONL record into events"
  [record]
  (let [msg-type (:type record)
        message (:message record)
        role (get message :role)
        content (get message :content)
        timestamp (parse-timestamp (:timestamp record))]
    (cond
      ;; User message - could be used for context but not as events
      (= role "user")
      []

      ;; Assistant message - extract tool uses
      (= role "assistant")
      (let [tool-uses (extract-tool-uses content)]
        (mapv #(tool-use->event % timestamp) tool-uses))

      :else [])))

(defn extract-files-touched
  "Extract unique file paths from events"
  [events]
  (->> events
       (keep :file)
       (remove str/blank?)
       distinct
       sort
       vec))

(defn build-session
  "Build session EDN from JSONL records"
  [{:keys [session-id pattern-id intent]} records]
  (let [;; Extract metadata from first record
        first-record (first records)
        cwd (:cwd first-record)
        git-branch (:gitBranch first-record)

        ;; Process all records into events
        events (->> records
                    (mapcat process-message)
                    (remove nil?)
                    vec)

        ;; Filter to just code/edit events for artifacts
        edit-events (filter #(= :code/edit (:event/type %)) events)
        artifacts (extract-files-touched edit-events)

        ;; Timestamps
        timestamps (keep :at events)
        start-time (first (sort timestamps))
        end-time (last (sort timestamps))]

    (cond-> {:session/id session-id
             :session/agent :claude
             :session/cwd cwd
             :session/git-branch git-branch
             :events events
             :artifacts artifacts}

      ;; Add clock-in if pattern-id provided
      pattern-id
      (assoc :clock-in
             {:clock-in/pattern-id pattern-id
              :clock-in/intent (or intent "Imported session")
              :clock-in/session-id session-id
              :clock-in/timestamp (or start-time (java.util.Date.))})

      ;; Add clock-out
      pattern-id
      (assoc :clock-out
             {:session/id session-id
              :pattern/primary pattern-id
              :pattern/trail []
              :session/status :success
              :artifacts artifacts
              :clock-out/timestamp (or end-time (java.util.Date.))}))))

(defn write-session!
  "Write session EDN to file"
  [path session]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (binding [*print-namespace-maps* false]
      (.write w ";; FuLab Session (imported from JSONL)\n")
      (.write w (str ";; Session ID: " (:session/id session) "\n"))
      (when-let [pattern (:clock-in session)]
        (.write w (str ";; Pattern: " (:clock-in/pattern-id pattern) "\n")))
      (.write w "\n")
      (pprint/pprint session w))))

(defn -main [& args]
  (let [{:keys [help unknown jsonl session-id pattern-id intent lab-root dry-run] :as opts}
        (parse-args args)]
    (cond
      help
      (usage)

      unknown
      (do (println "Unknown argument:" unknown)
          (usage)
          (System/exit 1))

      (nil? jsonl)
      (do (println "Error: --jsonl is required")
          (usage)
          (System/exit 1))

      (nil? session-id)
      (do (println "Error: --session-id is required")
          (usage)
          (System/exit 1))

      (not (.exists (io/file jsonl)))
      (do (println "Error: JSONL file not found:" jsonl)
          (System/exit 1))

      :else
      (let [records (with-open [r (io/reader jsonl)]
                      (doall (keep read-json-line (line-seq r))))
            session (build-session opts records)
            output-path (str (io/file lab-root "sessions" (str session-id ".edn")))]

        (println (format "[lab-import] Processing %d records from %s" (count records) jsonl))
        (println (format "[lab-import] Generated %d events" (count (:events session))))
        (println (format "[lab-import] Files touched: %d" (count (:artifacts session))))

        (if dry-run
          (do
            (println "\n--- DRY RUN OUTPUT ---\n")
            (binding [*print-namespace-maps* false]
              (pprint/pprint session)))
          (do
            (write-session! output-path session)
            (println (format "[lab-import] Written to %s" output-path))))))))

(apply -main *command-line-args*)
