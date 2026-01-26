#!/usr/bin/env bb
;; Scribe Notebook - Export conversation sessions to styled HTML
;;
;; Usage:
;;   bb dev/scribe_notebook.clj [session-id] [--out file.html]
;;   bb dev/scribe_notebook.clj musn-fuclaude-scribe-test --out notebook.html
;;   bb dev/scribe_notebook.clj musn-fuclaude-scribe-test  # prints to stdout

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def default-session "musn-fuclaude-scribe-test")

(def html-template "
<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>%s - Scribe Notebook</title>
  <style>
    :root {
      --bg-primary: #1a1a2e;
      --bg-secondary: #16213e;
      --bg-user: #1e3a5f;
      --bg-agent: #2d2d44;
      --text-primary: #eaeaea;
      --text-secondary: #a0a0a0;
      --accent-user: #6ec1e4;
      --accent-agent: #c792ea;
      --accent-system: #89ddff;
      --border-color: #3a3a5a;
    }

    * { box-sizing: border-box; }

    body {
      font-family: 'SF Mono', 'Fira Code', 'JetBrains Mono', monospace;
      background: var(--bg-primary);
      color: var(--text-primary);
      margin: 0;
      padding: 20px;
      line-height: 1.6;
    }

    .container {
      max-width: 900px;
      margin: 0 auto;
    }

    header {
      border-bottom: 2px solid var(--border-color);
      padding-bottom: 20px;
      margin-bottom: 30px;
    }

    h1 {
      color: var(--accent-user);
      font-size: 1.5rem;
      margin: 0 0 10px 0;
      font-weight: 500;
    }

    .meta {
      color: var(--text-secondary);
      font-size: 0.85rem;
    }

    .meta span {
      margin-right: 20px;
    }

    .turn {
      margin-bottom: 24px;
      border-radius: 8px;
      overflow: hidden;
    }

    .turn-header {
      padding: 8px 16px;
      font-size: 0.75rem;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .turn-content {
      padding: 16px;
      white-space: pre-wrap;
      word-wrap: break-word;
    }

    .turn.user {
      background: var(--bg-user);
      border-left: 3px solid var(--accent-user);
    }

    .turn.user .turn-header {
      background: rgba(110, 193, 228, 0.1);
      color: var(--accent-user);
    }

    .turn.user .role::before {
      content: '❯ ';
    }

    .turn.agent {
      background: var(--bg-agent);
      border-left: 3px solid var(--accent-agent);
    }

    .turn.agent .turn-header {
      background: rgba(199, 146, 234, 0.1);
      color: var(--accent-agent);
    }

    .turn.system {
      background: var(--bg-secondary);
      border-left: 3px solid var(--accent-system);
      font-size: 0.85rem;
    }

    .turn.system .turn-header {
      background: rgba(137, 221, 255, 0.1);
      color: var(--accent-system);
    }

    .timestamp {
      color: var(--text-secondary);
      font-size: 0.7rem;
    }

    footer {
      margin-top: 40px;
      padding-top: 20px;
      border-top: 1px solid var(--border-color);
      color: var(--text-secondary);
      font-size: 0.8rem;
      text-align: center;
    }

    .summary {
      background: var(--bg-secondary);
      border-radius: 8px;
      padding: 20px;
      margin-top: 30px;
    }

    .summary h2 {
      color: var(--accent-system);
      font-size: 1rem;
      margin: 0 0 15px 0;
      font-weight: 500;
    }

    .summary-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
      gap: 15px;
    }

    .stat {
      text-align: center;
    }

    .stat-value {
      font-size: 1.5rem;
      color: var(--accent-user);
      font-weight: bold;
    }

    .stat-label {
      font-size: 0.75rem;
      color: var(--text-secondary);
    }
  </style>
</head>
<body>
  <div class=\"container\">
    <header>
      <h1>%s</h1>
      <div class=\"meta\">
        <span>Agent: %s</span>
        <span>Events: %d</span>
        <span>Generated: %s</span>
      </div>
    </header>

    <main>
%s
    </main>

    <div class=\"summary\">
      <h2>Session Summary</h2>
      <div class=\"summary-grid\">
        <div class=\"stat\">
          <div class=\"stat-value\">%d</div>
          <div class=\"stat-label\">User Turns</div>
        </div>
        <div class=\"stat\">
          <div class=\"stat-value\">%d</div>
          <div class=\"stat-label\">Agent Turns</div>
        </div>
        <div class=\"stat\">
          <div class=\"stat-value\">%d</div>
          <div class=\"stat-label\">Total Events</div>
        </div>
      </div>
    </div>

    <footer>
      Scribe Notebook &middot; Futon3 Lab Session Archive
    </footer>
  </div>
</body>
</html>
")

(defn load-session [session-id]
  (let [path (io/file "lab" "sessions" (str session-id ".edn"))]
    (when (.exists path)
      (edn/read-string (slurp path)))))

(defn escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn format-timestamp [inst]
  (if inst
    (str inst)
    ""))

(defn event->html [event]
  (let [etype (:event/type event)
        at (format-timestamp (:at event))]
    (case etype
      :turn/user
      (let [content (escape-html (get-in event [:payload :content]))]
        (format "      <div class=\"turn user\">
        <div class=\"turn-header\">
          <span class=\"role\">USER</span>
          <span class=\"timestamp\">%s</span>
        </div>
        <div class=\"turn-content\">%s</div>
      </div>\n" at content))

      :turn/agent
      (let [content (escape-html (get-in event [:payload :content]))]
        (format "      <div class=\"turn agent\">
        <div class=\"turn-header\">
          <span class=\"role\">AGENT</span>
          <span class=\"timestamp\">%s</span>
        </div>
        <div class=\"turn-content\">%s</div>
      </div>\n" at content))

      :aif/config
      (format "      <div class=\"turn system\">
        <div class=\"turn-header\">
          <span class=\"role\">SYSTEM</span>
          <span class=\"timestamp\">%s</span>
        </div>
        <div class=\"turn-content\">Session initialized</div>
      </div>\n" at)

      ;; Other event types
      (format "      <div class=\"turn system\">
        <div class=\"turn-header\">
          <span class=\"role\">%s</span>
          <span class=\"timestamp\">%s</span>
        </div>
        <div class=\"turn-content\">%s</div>
      </div>\n"
              (name etype)
              at
              (escape-html (pr-str (:payload event)))))))

(defn session->html [session]
  (let [session-id (:session/id session)
        agent (name (or (:session/agent session) :unknown))
        events (:events session)
        turns (filter #(#{:turn/user :turn/agent} (:event/type %)) events)
        user-turns (count (filter #(= :turn/user (:event/type %)) turns))
        agent-turns (count (filter #(= :turn/agent (:event/type %)) turns))
        events-html (str/join "" (map event->html events))
        now (str (java.time.Instant/now))]
    (format html-template
            session-id           ; title
            session-id           ; h1
            agent                ; agent meta
            (count events)       ; events meta
            now                  ; generated meta
            events-html          ; main content
            user-turns           ; summary
            agent-turns
            (count events))))

(defn parse-args [args]
  (loop [args args
         opts {:session default-session :out nil}]
    (if (empty? args)
      opts
      (let [[arg val & rest] args]
        (case arg
          "--out" (recur rest (assoc opts :out val))
          "-o" (recur rest (assoc opts :out val))
          (recur (cons val rest) (assoc opts :session arg)))))))

(defn -main [& args]
  (let [{:keys [session out]} (parse-args args)
        session-data (load-session session)]
    (if session-data
      (let [html (session->html session-data)]
        (if out
          (do
            (spit out html)
            (println (str "Wrote " out)))
          (println html)))
      (do
        (binding [*out* *err*]
          (println "Session not found:" session)
          (println "Available sessions:")
          (doseq [f (filter #(str/ends-with? (.getName %) ".edn")
                            (.listFiles (io/file "lab" "sessions")))]
            (println "  " (str/replace (.getName f) ".edn" ""))))
        (System/exit 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
