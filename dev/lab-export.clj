#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn usage []
  (println "Usage: dev/lab-export.clj --session-file PATH [--repo-root PATH] [--lab-root PATH]")
  (println "                              [--include-commands] [--include-errors] [--write-stub]")
  (println "                              [--write-trace] [--dry-run]")
  (println)
  (println "Reads a Codex rollout JSONL and writes lab/raw + lab/stubs entries."))

(defn parse-args [args]
  (loop [opts {:include-commands false
               :include-errors false
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
        "--include-errors" (recur (assoc opts :include-errors true) (rest remaining))
        "--no-stub" (recur (assoc opts :write-stub false) (rest remaining))
        "--no-trace" (recur (assoc opts :write-trace false) (rest remaining))
        "--write-stub" (recur (assoc opts :write-stub true) (rest remaining))
        "--write-trace" (recur (assoc opts :write-trace true) (rest remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- read-json-line [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Exception _ nil)))

(defn- message-text [content]
  (let [texts (keep :text content)
        trimmed (remove str/blank? texts)]
    (when (seq trimmed)
      (str/join "\n" trimmed))))

(defn- attach-session-id [records session-id]
  (mapv #(assoc % :session-id session-id) records))

(defn- indexed-messages [records role prefix]
  (loop [idx 1
         remaining records
         acc []]
    (if-let [rec (first remaining)]
      (let [payload (:payload rec)]
        (if (and (= "response_item" (:type rec))
                 (= "message" (:type payload))
                 (= role (:role payload)))
          (let [text (message-text (:content payload))
                msg-id (format "%s:%s%03d" (get-in rec [:session-id] "") prefix idx)
                msg {:id msg-id
                     :timestamp (:timestamp rec)
                     :text text}]
            (recur (inc idx) (rest remaining)
                   (if text (conj acc msg) acc)))
          (recur idx (rest remaining) acc)))
      acc)))

(def ^:private patch-file-re
  #"^\*\*\* (?:Update File|Add File|Delete File|Move to): (.+)$")

(defn- files-from-patch [patch]
  (->> (str/split-lines (or patch ""))
       (keep (fn [line]
               (when-let [[_ path] (re-matches patch-file-re line)]
                 path)))
       set))

(def ^:private mv-re
  #"(?m)(?:^|\\s)mv\\s+([^\\s]+)\\s+([^\\s]+)")

(defn- files-from-command [cmd]
  (->> (re-seq mv-re (or cmd ""))
       (mapcat (fn [[_ src dst]] [src dst]))
       set))

(defn- parse-command-args [arg-str]
  (try
    (json/read-str arg-str :key-fn keyword)
    (catch Exception _ nil)))

(defn- extract-exit-code [output]
  (when-let [[_ code] (re-find #"Exit code:\\s+(\\d+)" (or output ""))]
    (Long/parseLong code)))

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
  (spit path (json/write-str data {:pretty true})))

(defn- write-stub [path content]
  (io/make-parents path)
  (spit path content))

(defn- write-trace [path session-id user-messages assistant-messages]
  (let [entries (concat (map #(assoc % :role "user") user-messages)
                        (map #(assoc % :role "assistant") assistant-messages))
        sorted (sort-by :timestamp entries)]
    (io/make-parents path)
    (spit path
          (str "#+TITLE: Lab Trace " session-id "\n\n"
               (apply str
                      (for [{:keys [id role timestamp text]} sorted
                            :when (and id text)]
                        (str "* msg:" id " (" role ") " (or timestamp "") "\n"
                             "#+BEGIN_EXAMPLE\n"
                             text "\n"
                             "#+END_EXAMPLE\n\n")))))))

(defn process-session [session-file {:keys [repo-root lab-root include-commands include-errors dry-run] :as opts}]
  (let [repo-root (or repo-root (System/getProperty "user.dir"))
        lab-root (or lab-root (str (io/file repo-root "lab")))
        write-stub? (:write-stub opts)
        write-trace? (:write-trace opts)
        records (with-open [r (io/reader session-file)]
                  (doall (keep read-json-line (line-seq r))))
        session-meta (some #(when (= "session_meta" (:type %)) %) records)
        session-id (or (get-in session-meta [:payload :id])
                       (some-> session-file io/file .getName (str/replace #".jsonl$" "")))
        records (attach-session-id records session-id)
        timestamps (keep :timestamp records)
        timestamp-start (or (get-in session-meta [:payload :timestamp]) (first timestamps))
        timestamp-end (last (sort timestamps))
        user-messages (indexed-messages records "user" "u")
        assistant-messages (indexed-messages records "assistant" "a")
        patch-files (->> records
                         (filter #(= "response_item" (:type %)))
                         (map :payload)
                         (filter #(and (= "custom_tool_call" (:type %)) (= "apply_patch" (:name %))))
                         (mapcat #(files-from-patch (:input %)))
                         set)
        command-files (->> records
                           (filter #(= "response_item" (:type %)))
                           (map :payload)
                           (filter #(and (= "function_call" (:type %)) (= "shell_command" (:name %))))
                           (map :arguments)
                           (keep parse-command-args)
                           (map :command)
                           (mapcat files-from-command)
                           set)
        files-touched (->> (concat patch-files command-files)
                           (remove str/blank?)
                           distinct
                           sort
                           vec)
        commands (when include-commands
                   (->> records
                        (filter #(= "response_item" (:type %)))
                        (map :payload)
                        (filter #(and (= "function_call" (:type %)) (= "shell_command" (:name %))))
                        (map :arguments)
                        (keep parse-command-args)
                        (map :command)
                        (remove str/blank?)
                        vec))
        errors (when include-errors
                 (->> records
                      (filter #(= "response_item" (:type %)))
                      (map :payload)
                      (filter #(= "function_call_output" (:type %)))
                      (map :output)
                      (filter (fn [out]
                                (when-let [code (extract-exit-code out)]
                                  (pos? code))))
                      vec))
        trace-path (str (io/file "lab" "trace" (str session-id ".org")))
        draft-path (str (io/file "lab" "doc-drafts" (str session-id ".json")))
        raw {"lab/session-id" session-id
             "lab/repo-root" repo-root
             "lab/timestamp-start" timestamp-start
             "lab/timestamp-end" timestamp-end
             "lab/user-messages" user-messages
             "lab/assistant-messages" assistant-messages
             "lab/trace-path" trace-path
             "lab/doc-draft-path" draft-path
             "lab/files-touched" files-touched
             "lab/commands" (or commands [])
             "lab/errors" (or errors [])}
        raw-path (str (io/file lab-root "raw" (str session-id ".json")))
        stub-path (str (io/file lab-root "stubs" (str session-id ".org")))
        trace-path-abs (str (io/file lab-root "trace" (str session-id ".org")))]
    (if dry-run
      (do
        (println "Would write:" raw-path)
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
          (write-trace trace-path-abs session-id user-messages assistant-messages))))
    {:session-id session-id
     :raw raw-path
     :stub (when write-stub? stub-path)
     :trace (when write-trace? trace-path-abs)}))

(defn -main [& args]
  (let [{:keys [help unknown session-file] :as opts} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (nil? session-file) (do (println "--session-file is required") (usage) (System/exit 1))
      :else (let [result (process-session session-file opts)]
              (println (format "[lab-export] session=%s" (:session-id result)))
              (println (format "[lab-export] raw=%s" (:raw result)))
              (when-let [stub (:stub result)]
                (println (format "[lab-export] stub=%s" stub)))
              (when-let [trace (:trace result)]
                (println (format "[lab-export] trace=%s" trace)))))))

(apply -main *command-line-args*)
