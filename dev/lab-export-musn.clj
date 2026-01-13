#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.edn :as edn])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn usage []
  (println "Usage: dev/lab-export-musn.clj --session-id ID | --musn-file PATH")
  (println "                                 [--lab-root PATH] [--repo-root PATH] [--dry-run]")
  (println)
  (println "Reads a MUSN session EDN stream and writes lab/raw + trace + doc-draft entries."))

(defn parse-args [args]
  (loop [opts {:dry-run false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--musn-file" (recur (assoc opts :musn-file (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--repo-root" (recur (assoc opts :repo-root (second remaining)) (nnext remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- read-edn-line [line]
  (when-not (str/blank? line)
    (try
      (edn/read-string {:readers {'object (fn [v] (last v))}} line)
      (catch Exception _ nil))))

(defn- write-json [path data]
  (io/make-parents path)
  (spit path (json/write-str data {:escape-slash false})))

(defn- timestamp-str [value]
  (when value
    (cond
      (string? value) value
      (instance? java.time.Instant value) (str value)
      :else (str value))))

(defn- files-touched [events]
  (->> events
       (filter #(= :turn/action (:op %)))
       (map #(get-in % [:req :files]))
       (mapcat (fn [files]
                 (cond
                   (string? files) [files]
                   (sequential? files) (map str files)
                   :else [])))
       (remove str/blank?)
       distinct
       sort
       vec))

(defn- collect-intents [events]
  (->> events
       (keep #(or (get-in % [:req :hud :intent])
                  (get-in % [:req :intent])))
       (remove str/blank?)
       distinct
       vec))

(defn- collect-resume-notes [events]
  (->> events
       (keep (fn [event]
               (when (= :turn/resume (:op event))
                 (get-in event [:req :note]))))
       (remove str/blank?)
       distinct
       vec))

(defn- format-lines [label items]
  (when (seq items)
    (str label ": " (str/join " | " items))))

(defn- user-text [events]
  (let [parts [(format-lines "Intent" (collect-intents events))
               (format-lines "Resume note" (collect-resume-notes events))]]
    (some->> parts (remove nil?) (str/join "\n") str/trim)))

(defn- collect-psr [events]
  (let [from-summary (keep #(get-in % [:resp :summary :psr]) events)
        from-decision (keep #(get-in % [:resp :psr]) events)]
    (->> (concat from-summary from-decision)
         (remove nil?)
         (map #(if (string? %) % (pr-str %)))
         distinct
         vec)))

(defn- collect-pur [events]
  (->> events
       (keep #(get-in % [:resp :summary :pur]))
       (remove nil?)
       (map #(if (string? %) % (pr-str %)))
       distinct
       vec))

(defn- collect-aif [events]
  (->> events
       (keep (fn [event]
               (or (get-in event [:resp :summary :aif])
                   (get-in event [:resp :summary :aif/summary])
                   (get-in event [:resp :aif :summary])
                   (get-in event [:resp :aif])
                   (get-in event [:resp :psr :selection/reason :aif])
                   (get-in event [:resp :summary :selection/reason :aif])
                   (get-in event [:state :selection :selection/reason :aif]))))
       (remove nil?)
       (map #(if (string? %) % (pr-str %)))
       distinct
       vec))

(defn- assistant-text [events]
  (let [parts [(format-lines "PSR" (collect-psr events))
               (format-lines "PUR" (collect-pur events))
               (format-lines "AIF" (collect-aif events))]]
    (some->> parts (remove nil?) (str/join "\n") str/trim)))

(defn- write-trace [path session-id events]
  (io/make-parents path)
  (spit path
        (str "#+TITLE: Lab Trace " session-id "\n\n"
             (apply str
                    (map-indexed
                      (fn [idx event]
                        (let [ts (timestamp-str (:ts event))
                              op (:op event)
                              op-name (cond
                                        (keyword? op) (name op)
                                        (string? op) op
                                        :else (str op))]
                          (str "* " (inc idx) " " op-name " " (or ts "") "\n"
                               "#+BEGIN_EXAMPLE\n"
                               (pr-str event) "\n"
                               "#+END_EXAMPLE\n\n")))
                      events)))))

(defn process-session [{:keys [repo-root lab-root session-id musn-file dry-run]}]
  (let [repo-root (or repo-root (System/getProperty "user.dir"))
        lab-root (or lab-root (str (io/file repo-root "lab")))
        musn-file (or musn-file
                      (when session-id
                        (str (io/file lab-root "musn" (str session-id ".edn")))))
        _ (when (or (nil? musn-file) (str/blank? musn-file))
            (println "Missing --session-id or --musn-file")
            (usage)
            (System/exit 1))
        file (io/file musn-file)
        _ (when-not (.exists file)
            (println "MUSN file not found:" musn-file)
            (System/exit 1))
        events (with-open [r (io/reader file)]
                 (doall (keep read-edn-line (line-seq r))))
        session-id (or session-id
                       (some :session/id events)
                       (-> musn-file io/file .getName (str/replace #"\.edn$" "")))
        timestamps (->> events (keep :ts) (map timestamp-str) (remove nil?) vec)
        timestamp-start (first (sort timestamps))
        timestamp-end (last (sort timestamps))
        user-text (user-text events)
        assistant-text (assistant-text events)
        user-messages (cond-> []
                        (not (str/blank? user-text))
                        (conj {:id (format "%s:u%03d" session-id 1)
                               :timestamp timestamp-start
                               :text user-text
                               :role "user"}))
        assistant-messages (cond-> []
                             (not (str/blank? assistant-text))
                             (conj {:id (format "%s:a%03d" session-id 1)
                                    :timestamp timestamp-end
                                    :text assistant-text
                                    :role "assistant"}))
        files (files-touched events)
        trace-path (str (io/file "lab" "trace" (str session-id ".org")))
        draft-path (str (io/file "lab" "doc-drafts" (str session-id ".json")))
        raw {"lab/session-id" session-id
             "lab/repo-root" repo-root
             "lab/timestamp-start" timestamp-start
             "lab/timestamp-end" timestamp-end
             "lab/user-messages" (mapv #(dissoc % :role) user-messages)
             "lab/assistant-messages" (mapv #(dissoc % :role) assistant-messages)
             "lab/files-touched" files
             "lab/trace-path" trace-path
             "lab/doc-draft-path" draft-path
             "lab/errors" []}
        raw-path (str (io/file lab-root "raw" (str session-id ".json")))
        trace-path-abs (str (io/file lab-root "trace" (str session-id ".org")))
        draft-path-abs (str (io/file lab-root "doc-drafts" (str session-id ".json")))]
    (if dry-run
      (do
        (println "Would write:" raw-path)
        (println (format "  %d user messages, %d assistant messages"
                         (count user-messages) (count assistant-messages)))
        (println (format "  %d files touched" (count files)))
        (println "Would write:" trace-path-abs)
        (println "Would write:" draft-path-abs))
      (do
        (write-json raw-path raw)
        (write-trace trace-path-abs session-id events)
        (write-json draft-path-abs {})))
    {:session-id session-id
     :raw raw-path
     :trace trace-path-abs
     :doc-draft draft-path-abs
     :user-count (count user-messages)
     :assistant-count (count assistant-messages)
     :files-count (count files)}))

(defn -main [& args]
  (let [{:keys [help unknown] :as opts} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      :else (let [result (process-session opts)]
              (println (format "[lab-export-musn] session=%s" (:session-id result)))
              (println (format "[lab-export-musn] messages: %d user, %d assistant"
                               (:user-count result) (:assistant-count result)))
              (println (format "[lab-export-musn] files touched: %d" (:files-count result)))
              (println (format "[lab-export-musn] raw=%s" (:raw result)))
              (println (format "[lab-export-musn] trace=%s" (:trace result)))
              (println (format "[lab-export-musn] doc-draft=%s" (:doc-draft result)))))))

(apply -main *command-line-args*)
