#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn usage []
  (println "Usage: dev/lab-export-claude.clj --session-file PATH [--repo-root PATH] [--lab-root PATH]")
  (println "                                 [--include-commands] [--write-stub] [--write-trace] [--dry-run]")
  (println)
  (println "Reads a Claude Code session JSONL and writes lab/raw + lab/stubs entries."))

(defn parse-args [args]
  (loop [opts {:include-commands false
               :write-stub true
               :write-trace true
               :dry-run false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--session-file" (recur (assoc opts :session-file (second remaining)) (nnext remaining))
        "--repo-root" (recur (assoc opts :repo-root (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--include-commands" (recur (assoc opts :include-commands true) (rest remaining))
        "--no-stub" (recur (assoc opts :write-stub false) (rest remaining))
        "--no-trace" (recur (assoc opts :write-trace false) (rest remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- read-json-line [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Exception _ nil)))

(defn- extract-text-from-content [content]
  "Extract text from Claude's content array (handles text, thinking, tool_use, etc.)"
  (cond
    (string? content) content
    (sequential? content)
    (->> content
         (keep (fn [item]
                 (cond
                   (string? item) item
                   (and (map? item) (= "text" (:type item))) (:text item)
                   :else nil)))
         (remove str/blank?)
         (str/join "\n"))
    :else nil))

(defn- extract-tool-uses [content]
  "Extract tool use info from content array"
  (when (sequential? content)
    (->> content
         (filter #(and (map? %) (= "tool_use" (:type %))))
         (map (fn [tu]
                {:tool (:name tu)
                 :input (:input tu)})))))

(defn- files-from-tool-uses [tool-uses]
  "Extract file paths from tool uses (Edit, Write, Read)"
  (->> tool-uses
       (keep (fn [{:keys [tool input]}]
               (when (and tool input)
                 (cond
                   (contains? #{"Edit" "Write" "Read"} tool)
                   (:file_path input)
                   (= "Bash" tool)
                   nil  ; Could parse commands for file refs
                   :else nil))))
       (remove nil?)
       set))

(defn- stub-template [{:keys [session-id repo-root timestamp-start timestamp-end files-touched]}]
  (let [files (or files-touched [])]
    (str "#+TITLE: Lab Session " session-id "\n"
         "#+PROPERTY: SESSION_ID " session-id "\n"
         "#+PROPERTY: REPO_ROOT " (or repo-root "") "\n"
         "#+PROPERTY: TIMESTAMP_START " (or timestamp-start "") "\n"
         "#+PROPERTY: TIMESTAMP_END " (or timestamp-end "") "\n"
         "#+PROPERTY: FILES " (str/join " " files) "\n"
         "\n"
         "* Context\n"
         "- TODO\n\n"
         "* Delta\n"
         "- TODO\n\n"
         "* Per-file Notes\n"
         (if (seq files)
           (apply str
                  (for [f files]
                    (str "** " f "\n- TODO\n\n")))
           "** (none)\n- TODO\n\n")
         "* Verification\n"
         "- TODO\n\n")))

(defn- write-json [path data]
  (io/make-parents path)
  (spit path (json/write-str data {:escape-slash false})))

(defn- write-stub [path content]
  (io/make-parents path)
  (spit path content))

(defn- write-trace [path session-id messages]
  (let [sorted (sort-by :timestamp messages)]
    (io/make-parents path)
    (spit path
          (str "#+TITLE: Lab Trace " session-id "\n\n"
               (apply str
                      (for [{:keys [id role timestamp text]} sorted
                            :when (and id text)]
                        (str "* msg:" id " (" role ") " (or timestamp "") "\n"
                             "#+BEGIN_EXAMPLE\n"
                             (str/trim (or text "")) "\n"
                             "#+END_EXAMPLE\n\n")))))))

(defn process-session [session-file {:keys [repo-root lab-root include-commands dry-run] :as opts}]
  (let [repo-root (or repo-root (System/getProperty "user.dir"))
        lab-root (or lab-root (str (io/file repo-root "lab")))
        write-stub? (:write-stub opts)
        write-trace? (:write-trace opts)
        records (with-open [r (io/reader session-file)]
                  (doall (keep read-json-line (line-seq r))))
        ;; Filter to user and assistant messages
        messages (->> records
                      (filter #(contains? #{"user" "assistant"} (:type %)))
                      (filter #(get-in % [:message :role])))
        session-id (or (some :sessionId messages)
                       (-> session-file io/file .getName (str/replace #"\.jsonl$" "")))
        timestamps (keep :timestamp messages)
        timestamp-start (first (sort timestamps))
        timestamp-end (last (sort timestamps))
        ;; Build user messages
        user-messages (->> messages
                           (filter #(= "user" (get-in % [:message :role])))
                           (map-indexed
                            (fn [idx msg]
                              {:id (format "%s:u%03d" session-id (inc idx))
                               :timestamp (:timestamp msg)
                               :text (extract-text-from-content (get-in msg [:message :content]))
                               :role "user"})))
        ;; Build assistant messages with tool use extraction
        assistant-messages (->> messages
                                (filter #(= "assistant" (get-in % [:message :role])))
                                (map-indexed
                                 (fn [idx msg]
                                   (let [content (get-in msg [:message :content])]
                                     {:id (format "%s:a%03d" session-id (inc idx))
                                      :timestamp (:timestamp msg)
                                      :text (extract-text-from-content content)
                                      :tool-uses (extract-tool-uses content)
                                      :role "assistant"}))))
        ;; Extract files touched from tool uses
        all-tool-uses (mapcat :tool-uses assistant-messages)
        files-touched (->> all-tool-uses
                           files-from-tool-uses
                           (remove str/blank?)
                           sort
                           vec)
        ;; Build paths
        trace-path (str (io/file "lab" "trace" (str session-id ".org")))
        draft-path (str (io/file "lab" "doc-drafts" (str session-id ".json")))
        raw {"lab/session-id" session-id
             "lab/repo-root" repo-root
             "lab/timestamp-start" timestamp-start
             "lab/timestamp-end" timestamp-end
             "lab/user-messages" (mapv #(dissoc % :role) user-messages)
             "lab/assistant-messages" (mapv #(dissoc % :role :tool-uses) assistant-messages)
             "lab/trace-path" trace-path
             "lab/doc-draft-path" draft-path
             "lab/files-touched" files-touched
             "lab/tool-uses" (vec all-tool-uses)}
        raw-path (str (io/file lab-root "raw" (str session-id ".json")))
        stub-path (str (io/file lab-root "stubs" (str session-id ".org")))
        trace-path-abs (str (io/file lab-root "trace" (str session-id ".org")))
        all-messages (concat user-messages assistant-messages)]
    (if dry-run
      (do
        (println "Would write:" raw-path)
        (println (format "  %d user messages, %d assistant messages"
                         (count user-messages) (count assistant-messages)))
        (println (format "  %d files touched" (count files-touched)))
        (when write-stub?
          (println "Would write:" stub-path))
        (when write-trace?
          (println "Would write:" trace-path-abs)))
      (do
        (write-json raw-path raw)
        (when write-stub?
          (write-stub stub-path
                      (stub-template {:session-id session-id
                                      :repo-root repo-root
                                      :timestamp-start timestamp-start
                                      :timestamp-end timestamp-end
                                      :files-touched files-touched})))
        (when write-trace?
          (write-trace trace-path-abs session-id all-messages))))
    {:session-id session-id
     :raw raw-path
     :stub (when write-stub? stub-path)
     :trace (when write-trace? trace-path-abs)
     :user-count (count user-messages)
     :assistant-count (count assistant-messages)
     :files-count (count files-touched)}))

(defn -main [& args]
  (let [{:keys [help unknown session-file] :as opts} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (nil? session-file) (do (println "--session-file is required") (usage) (System/exit 1))
      :else (let [result (process-session session-file opts)]
              (println (format "[lab-export-claude] session=%s" (:session-id result)))
              (println (format "[lab-export-claude] messages: %d user, %d assistant"
                               (:user-count result) (:assistant-count result)))
              (println (format "[lab-export-claude] files touched: %d" (:files-count result)))
              (println (format "[lab-export-claude] raw=%s" (:raw result)))
              (when-let [stub (:stub result)]
                (println (format "[lab-export-claude] stub=%s" stub)))
              (when-let [trace (:trace result)]
                (println (format "[lab-export-claude] trace=%s" trace)))))))

(apply -main *command-line-args*)
