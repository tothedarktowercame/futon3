#!/usr/bin/env bb
;; Scribe Demo - Shows the append-only notebook from a fuclaude session
;;
;; This demonstrates the scribe architecture:
;; 1. Conversation turns are recorded to lab/sessions/<session-id>.edn
;; 2. Recent turns are retrieved for HUD context
;; 3. Sigils update based on conversation content
;;
;; Usage:
;;   bb dev/scribe_demo.clj [session-id]
;;   bb dev/scribe_demo.clj musn-fuclaude-scribe-test

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.pprint :as pp])

(def default-session "musn-fuclaude-scribe-test")

(defn load-session [session-id]
  (let [path (io/file "lab" "sessions" (str session-id ".edn"))]
    (when (.exists path)
      (edn/read-string (slurp path)))))

(defn format-turn [event]
  (let [role (get-in event [:payload :role])
        content (get-in event [:payload :content])
        at (:at event)
        role-str (case role
                   :user   "👤 USER"
                   :agent  "🤖 AGENT"
                   "?")]
    (str role-str " [" at "]\n"
         (str/replace content #"(.{80})" "$1\n")
         "\n")))

(defn format-event [event]
  (let [etype (:event/type event)]
    (case etype
      :turn/user  (format-turn event)
      :turn/agent (format-turn event)
      :aif/config (str "⚙️  AIF CONFIG [" (:at event) "]\n\n")
      (str "📌 " (name etype) " [" (:at event) "]\n\n"))))

(defn print-notebook [session]
  (println "=" (apply str (repeat 70 "=")))
  (println "SCRIBE NOTEBOOK: " (:session/id session))
  (println "Agent: " (:session/agent session))
  (println "=" (apply str (repeat 70 "=")))
  (println)

  (let [events (:events session)
        turns (filter #(#{:turn/user :turn/agent} (:event/type %)) events)]
    (println (str "Total events: " (count events)))
    (println (str "Conversation turns: " (count turns)))
    (println)
    (println "-" (apply str (repeat 70 "-")))
    (println)

    (doseq [event events]
      (print (format-event event)))))

(defn print-summary [session]
  (let [events (:events session)
        turns (filter #(#{:turn/user :turn/agent} (:event/type %)) events)
        user-turns (filter #(= :turn/user (:event/type %)) turns)
        agent-turns (filter #(= :turn/agent (:event/type %)) turns)]
    (println)
    (println "=" (apply str (repeat 70 "=")))
    (println "SUMMARY")
    (println "=" (apply str (repeat 70 "=")))
    (println)
    (println "Session ID:    " (:session/id session))
    (println "User turns:    " (count user-turns))
    (println "Agent turns:   " (count agent-turns))
    (println "Total events:  " (count events))
    (println)
    (println "This notebook demonstrates:")
    (println "  1. Real-time conversation recording via record-turn!")
    (println "  2. Append-only event log in EDN format")
    (println "  3. Foundation for context-aware HUD sigils")
    (println)))

(defn -main [& args]
  (let [session-id (or (first args) default-session)
        session (load-session session-id)]
    (if session
      (do
        (print-notebook session)
        (print-summary session))
      (do
        (println "Session not found:" session-id)
        (println "Available sessions:")
        (doseq [f (filter #(str/ends-with? (.getName %) ".edn")
                          (.listFiles (io/file "lab" "sessions")))]
          (println "  " (str/replace (.getName f) ".edn" "")))
        (System/exit 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
