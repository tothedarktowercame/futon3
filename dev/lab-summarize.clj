#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[futon5.llm.relay :as relay])

(defn usage []
  (println "Usage: dev/lab-summarize.clj --raw PATH [--stub PATH] [--prompt PATH] [--model MODEL] [--docbook-book BOOK] [--docbook-map PATH] [--no-docbook] [--dry-run]")
  (println)
  (println "Generates an Org stub from a lab/raw JSON entry using the OpenAI API."))

(defn parse-args [args]
  (loop [opts {:dry-run false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--raw" (recur (assoc opts :raw (second remaining)) (nnext remaining))
        "--stub" (recur (assoc opts :stub (second remaining)) (nnext remaining))
        "--prompt" (recur (assoc opts :prompt (second remaining)) (nnext remaining))
        "--model" (recur (assoc opts :model (second remaining)) (nnext remaining))
        "--docbook-book" (recur (assoc opts :docbook-book (second remaining)) (nnext remaining))
        "--docbook-map" (recur (assoc opts :docbook-map (second remaining)) (nnext remaining))
        "--no-docbook" (recur (assoc opts :docbook false) (rest remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- read-json [path]
  (json/read-str (slurp path) :key-fn identity))

(defn- lab-key [m k]
  (get m k))

(defn- header-block [raw]
  (let [session-id (or (lab-key raw "lab/session-id") "unknown")
        repo-root (or (lab-key raw "lab/repo-root") "")
        ts-start (or (lab-key raw "lab/timestamp-start") "")
        ts-end (or (lab-key raw "lab/timestamp-end") "")
        files (or (lab-key raw "lab/files-touched") [])
        files-str (str/join " " files)]
    (str "#+TITLE: Lab Session " session-id "\n"
         "#+PROPERTY: SESSION_ID " session-id "\n"
         "#+PROPERTY: REPO_ROOT " repo-root "\n"
         "#+PROPERTY: TIMESTAMP_START " ts-start "\n"
         "#+PROPERTY: TIMESTAMP_END " ts-end "\n"
         "#+PROPERTY: FILES " files-str "\n\n")))

(defn- response->text [resp]
  (let [choices (or (get-in resp [:body "choices"])
                    (get-in resp [:body :choices]))]
    (if (seq choices)
      (or (get-in (first choices) ["message" "content"])
          (get-in (first choices) [:message :content])
          (json/write-str (:body resp)))
      (json/write-str (:body resp)))))

(defn- extract-org [text]
  (if-let [[_ body] (re-find #"(?s)```org\\s*(.*?)```" text)]
    (str/trim body)
    (str/trim text)))

(defn- parse-summary-sections [summary]
  (let [lines (str/split-lines summary)
        state (atom {:section nil :current-file nil :files {}})
        trim (fn [s] (str/trim (or s "")))]
    (doseq [line lines]
      (cond
        (re-matches #"^\*\s+Context" line)
        (swap! state assoc :section :context)

        (re-matches #"^\*\s+Delta" line)
        (swap! state assoc :section :delta :current-file nil)

        (re-matches #"^\*\s+Verification" line)
        (swap! state assoc :section :verification :current-file nil)

        (and (= :delta (:section @state))
             (re-find #"^\*\*\s+" line))
        (let [file (trim (str/replace line #"^\*\*\s+" ""))]
          (swap! state assoc :current-file file)
          (swap! state update :files #(assoc % file [])))

        (and (= :delta (:section @state))
             (:current-file @state)
             (re-find #"^- " line))
        (let [file (:current-file @state)]
          (swap! state update-in [:files file] conj (trim (str/replace line #"^- " ""))))

        :else nil))
    {:files (:files @state)}))

(defn- read-edn [path]
  (when (.exists (io/file path))
    (read-string (slurp path))))

(defn- read-json [path]
  (json/read-str (slurp path) :key-fn identity))

(defn- read-json-keys [path]
  (json/read-str (slurp path) :key-fn keyword))

(defn- toc-doc-map [path]
  (let [entries (read-json-keys path)]
    (reduce (fn [acc entry]
              (assoc acc (:path_string entry)
                     {:doc-id (:doc_id entry)
                      :outline-path (:outline_path entry)}))
            {}
            entries)))

(defn- load-entry-map [path]
  (let [raw (read-edn path)]
    (cond
      (map? raw) (map (fn [[k v]] {:pattern k :outline-path v}) raw)
      (sequential? raw) raw
      :else [])))

(defn- match-outline-path [entry-map file-path]
  (some (fn [{:keys [pattern outline-path path_string]}]
          (let [pat (cond
                      (instance? java.util.regex.Pattern pattern) pattern
                      (string? pattern) (re-pattern pattern)
                      :else nil)]
            (when (and pat (re-find pat file-path))
              (or outline-path (when path_string [path_string])))))
        entry-map))

(defn- docbook-entry
  [{:keys [book doc-id outline-path run-id timestamp version files context delta verification]}]
  {"book_id" book
   "doc_id" doc-id
   "version" version
   "replaces" nil
   "timestamp" timestamp
   "run_id" run-id
   "outline_path" outline-path
   "agent_summary" ""
   "files_touched" files
   "doc/context" context
   "doc/delta" delta
   "doc/verification" verification})

(defn- docbook-stub
  [{:keys [doc-id version title context delta verification]}]
  (str "#+TITLE: " title "\n"
       ":PROPERTIES:\n:DOC_ID: " doc-id "\n:VERSION: " version "\n:REPLACES:\n:END:\n\n"
       "* Context\n" context "\n\n"
       "* Delta\n" delta "\n\n"
       "* Verification\n" verification "\n"))

(defn- bullet-text [items]
  (if (seq items)
    (str/join "\n" (map #(str "- " %) items))
    "- (none)"))

(defn- ensure-dir [path]
  (io/make-parents (io/file path "placeholder"))
  path)

(defn- draft-json [raw summary]
  (let [session-id (lab-key raw "lab/session-id")
        ts (or (lab-key raw "lab/timestamp-end")
               (lab-key raw "lab/timestamp-start"))
        files (or (lab-key raw "lab/files-touched") [])
        doc-id (str "lab/" session-id)]
    {"doc/id" doc-id
     "doc/entry-id" (str doc-id "#draft")
     "doc/status" "draft"
     "doc/timestamp" ts
     "doc/files" files
     "doc/lab-session" session-id
     "doc/context" summary}))

(defn -main [& args]
  (let [{:keys [help unknown raw stub prompt model dry-run docbook docbook-book docbook-map]} (merge {:docbook true} (parse-args args))]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (nil? raw) (do (println "--raw is required") (usage) (System/exit 1))
      :else
      (let [raw-path (io/file raw)
            raw-data (read-json raw-path)
            raw-data-k (read-json-keys raw-path)
            prompt-path (or prompt (str (io/file "lab" "prompts" "lab-session-summary.prompt")))
            system (slurp prompt-path)
            user (slurp raw-path)
            payload (relay/chat-request {:model (or model "gpt-5.2-chat-latest")
                                         :system system
                                         :user user})
            resp (relay/call-openai! payload)
            summary (response->text resp)
            header (header-block raw-data)
            summary-body (extract-org summary)
            output (str header summary-body "\n")
            stub-path (or stub
                          (str (io/file "lab" "stubs"
                                        (str (lab-key raw-data "lab/session-id") ".org"))))]
        (if dry-run
          (do
            (println "Would write:" stub-path)
            (println output))
          (do
            (io/make-parents stub-path)
            (spit stub-path output)
            (let [draft-path (str (io/file "lab" "doc-drafts"
                                           (str (lab-key raw-data "lab/session-id") ".json")))
                  draft (draft-json raw-data summary-body)]
              (io/make-parents draft-path)
              (spit draft-path (json/write-str draft {:pretty true}))
              (println (format "[lab-summarize] stub=%s" stub-path))
              (println (format "[lab-summarize] draft=%s" draft-path)))
            (when docbook
              (let* [book (or docbook-book "futon4")
                     map-path (or docbook-map (str (io/file "dev" "logs" "books" book "entry-map.edn")))
                     toc-path (str (io/file "dev" "logs" "books" book "toc.json"))
                     entry-map (load-entry-map map-path)
                     toc-map (toc-doc-map toc-path)
                     parsed (parse-summary-sections summary-body)
                     files->changes (:files parsed)
                     timestamp (or (lab-key raw-data "lab/timestamp-end")
                                   (lab-key raw-data "lab/timestamp-start"))
                     version (or (lab-key raw-data "lab/version") "lab-draft")
                     session-id (lab-key raw-data "lab/session-id")
                     base-dir (io/file "dev" "logs" "books" book)
                     raw-dir (ensure-dir (str (io/file base-dir "raw")))
                     stub-dir (ensure-dir (str (io/file base-dir "stubs")))]
                    (doseq [[file changes] files->changes]
                      (let [outline (or (match-outline-path entry-map file)
                                        ["Recent changes (futon4, pilot)"])
                            path-str (str/join " / " outline)
                            heading (get toc-map path-str)
                            doc-id (or (:doc-id heading) (str "futon4-" (hash path-str)))
                            run-id (str session-id "-" (str/replace doc-id "/" "_"))
                            delta (bullet-text (map #(str file " — " %) changes))
                            context (bullet-text [(str "Lab session " session-id)])
                            verification (bullet-text ["(unverified)"])
                            entry (docbook-entry {:book book
                                                  :doc-id doc-id
                                                  :outline-path outline
                                                  :run-id run-id
                                                  :timestamp timestamp
                                                  :version version
                                                  :files [file]
                                                  :context context
                                                  :delta delta
                                                  :verification verification})
                            stub (docbook-stub {:doc-id doc-id
                                                :version version
                                                :title (str (last outline) " — " session-id)
                                                :context context
                                                :delta delta
                                                :verification verification})
                            raw-path (io/file raw-dir (str run-id ".json"))
                            stub-path (io/file stub-dir (str run-id ".org"))]
                        (spit raw-path (json/write-str entry {:pretty true}))
                        (spit stub-path stub)))
                    (println (format "[lab-summarize] docbook=%s entries=%d"
                                     book (count files->changes)))))))))))

(apply -main *command-line-args*)
