(ns scripts.proof-commit
  "Validate commits against fulab proof trails and generate PR summaries."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def ^:private default-root
  (or (System/getenv "FUTON3_ROOT") "."))

;; --- EDN loading ---

(defn- load-edn [path]
  (when (.exists (io/file path))
    (edn/read-string (slurp path))))

(defn- fubar-events-path [root]
  (io/file root "resources/fubar-events.edn"))

(defn- fulab-patterns-path [root pattern-id]
  (let [[prefix name] (str/split pattern-id #"/" 2)]
    (io/file root "library" prefix (str name ".flexiarg"))))

;; --- Session queries ---

(defn- session-events [events session-id]
  (filter #(= session-id (:event/session %)) events))

(defn- find-clock-in [events]
  (first (filter #(= :clock-in/start (:event/type %)) events)))

(defn- find-clock-out [events]
  (first (filter #(= :clock-out/complete (:event/type %)) events)))

(defn- find-pattern-deps [events]
  (filter #(= :pattern/used (:event/type %)) events))

(defn- session-artifacts [events]
  (let [clock-out (find-clock-out events)]
    (or (:artifacts clock-out) [])))

;; --- Git queries ---

(defn- staged-files [root]
  (let [{:keys [exit out]} (sh "git" "diff" "--cached" "--name-only" :dir root)]
    (when (zero? exit)
      (set (remove str/blank? (str/split-lines out))))))

(defn- unstaged-changes? [root]
  (let [{:keys [exit out]} (sh "git" "status" "--porcelain" :dir root)]
    (and (zero? exit)
         (some #(re-find #"^.[MD]" %) (str/split-lines out)))))

;; --- Pattern loading ---

(defn- parse-flexiarg-field [content field]
  (when-let [match (re-find (re-pattern (str "(?m)^@" field " (.+)$")) content)]
    (second match)))

(defn- parse-evidence-shape [content]
  (when-let [match (re-find #"evidence-shape:\s*(\{[^}]+\})" content)]
    (try
      (edn/read-string (second match))
      (catch Exception _ nil))))

(defn- load-pattern [root pattern-id]
  (let [path (fulab-patterns-path root pattern-id)]
    (when (.exists path)
      (let [content (slurp path)]
        {:pattern/id pattern-id
         :pattern/title (parse-flexiarg-field content "title")
         :pattern/then (when-let [m (re-find #"(?m)^\s*\+\s*then:\s*(.+)$" content)]
                         (second m))
         :pattern/evidence-shape (parse-evidence-shape content)}))))

;; --- Validation checks ---

(defn structural-check
  "Level 1: Does session have clock-in and clock-out?"
  [session-events]
  (let [clock-in (find-clock-in session-events)
        clock-out (find-clock-out session-events)]
    {:check :structural
     :pass? (and clock-in clock-out)
     :clock-in? (boolean clock-in)
     :clock-out? (boolean clock-out)
     :pattern-id (:clock-in/pattern-id clock-in)
     :intent (:clock-in/intent clock-in)
     :status (:session/status clock-out)}))

(defn correspondence-check
  "Level 2: Do session artifacts cover staged files?"
  [session-events staged root]
  (let [artifacts (session-artifacts session-events)
        artifact-paths (set (map #(or (:path %) %) artifacts))
        ;; Normalize to relative paths
        normalize (fn [p] (str/replace-first p (str root "/") ""))
        normalized-artifacts (set (map normalize artifact-paths))
        missing (set/difference staged normalized-artifacts)]
    {:check :correspondence
     :pass? (empty? missing)
     :artifacts normalized-artifacts
     :staged staged
     :missing missing}))

(defn shape-check
  "Level 3: Do trail events contain pattern's evidence-shape fields?"
  [session-events patterns]
  (let [clock-in (find-clock-in session-events)
        clock-out (find-clock-out session-events)
        results
        (for [pattern patterns
              :let [shape (:pattern/evidence-shape pattern)]
              :when shape]
          (let [required-keys (set (keys shape))
                present-keys (set (concat (keys clock-in) (keys clock-out)))
                missing (set/difference required-keys present-keys)]
            {:pattern (:pattern/id pattern)
             :pass? (empty? missing)
             :required required-keys
             :missing missing}))]
    {:check :shape
     :pass? (every? :pass? results)
     :patterns results}))

;; --- PR Summary generation ---

(defn- pattern-dep-summary [deps]
  (when (seq deps)
    (str "**Pattern dependencies:**\n"
         (str/join "\n" (map #(format "- %s (%s)"
                                      (:pattern/id %)
                                      (or (:pattern/reason %) "unspecified"))
                             deps)))))

(defn- artifact-summary [artifacts]
  (when (seq artifacts)
    (str "**Artifacts:**\n"
         (str/join "\n" (map #(format "- `%s` [%s]"
                                      (or (:path %) %)
                                      (name (or (:action %) :modified)))
                             artifacts)))))

(defn generate-pr-summary
  "Generate a structured PR summary from session trail."
  [session-events patterns root]
  (let [clock-in (find-clock-in session-events)
        clock-out (find-clock-out session-events)
        deps (find-pattern-deps session-events)
        artifacts (session-artifacts session-events)
        primary-pattern (first patterns)]
    (str/join
     "\n\n"
     (remove nil?
             [(format "## Summary\n\nThis PR implements work clocked under **%s**."
                      (or (:clock-in/pattern-id clock-in) "unknown"))
              (when-let [intent (:clock-in/intent clock-in)]
                (format "**Intent:** %s" intent))
              (when-let [then (:pattern/then primary-pattern)]
                (format "**Pattern prescription (THEN):**\n> %s" then))
              (pattern-dep-summary deps)
              (artifact-summary artifacts)
              (format "## Validation\n\n- Clock-in: %s\n- Clock-out: %s (%s)\n- Artifacts tracked: %d"
                      (if clock-in "present" "MISSING")
                      (if clock-out "present" "MISSING")
                      (or (some-> clock-out :session/status name) "unknown")
                      (count artifacts))
              "---\n*Generated by proof-commit*"]))))

;; --- Main ---

(defn validate-session
  "Run all validation checks for a session."
  [root session-id]
  (let [events (load-edn (fubar-events-path root))
        session-evts (session-events events session-id)
        staged (staged-files root)
        clock-in (find-clock-in session-evts)
        pattern-ids (cons (:clock-in/pattern-id clock-in)
                          (map :pattern/id (find-pattern-deps session-evts)))
        patterns (remove nil? (map #(load-pattern root %) pattern-ids))]
    (when (empty? session-evts)
      (throw (ex-info "No events found for session" {:session-id session-id})))
    {:session-id session-id
     :structural (structural-check session-evts)
     :correspondence (correspondence-check session-evts staged root)
     :shape (shape-check session-evts patterns)
     :pr-summary (generate-pr-summary session-evts patterns root)}))

(defn- print-check [{:keys [check pass?] :as result}]
  (let [icon (if pass? "✓" "✗")]
    (println (format "[%s] %s" icon (name check)))
    (when-not pass?
      (case check
        :structural
        (do
          (when-not (:clock-in? result) (println "    - Missing clock-in"))
          (when-not (:clock-out? result) (println "    - Missing clock-out")))
        :correspondence
        (doseq [f (:missing result)]
          (println (format "    - Staged but not tracked: %s" f)))
        :shape
        (doseq [p (:patterns result)
                :when (not (:pass? p))]
          (println (format "    - %s missing: %s" (:pattern p) (:missing p))))
        nil))))

(defn -main [& args]
  (let [[session-id & rest-args] args
        root (or (first rest-args) default-root)]
    (when (or (nil? session-id) (= "--help" session-id) (= "-h" session-id))
      (println "Usage: clj -M -m scripts.proof-commit SESSION_ID [ROOT]")
      (println "\nValidates a commit against the fulab proof trail for SESSION_ID.")
      (println "Runs structural, correspondence, and shape checks.")
      (println "Generates a PR summary suitable for review.")
      (System/exit (if (nil? session-id) 1 0)))
    (try
      (let [result (validate-session root session-id)
            all-pass? (and (get-in result [:structural :pass?])
                           (get-in result [:correspondence :pass?])
                           (get-in result [:shape :pass?]))]
        (println (format "\n=== Proof Commit Validation: %s ===\n" session-id))
        (print-check (:structural result))
        (print-check (:correspondence result))
        (print-check (:shape result))
        (println "\n--- PR Summary ---\n")
        (println (:pr-summary result))
        (System/exit (if all-pass? 0 1)))
      (catch Exception e
        (println (format "Error: %s" (.getMessage e)))
        (System/exit 1)))))
