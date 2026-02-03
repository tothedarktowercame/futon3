(ns futon3.drawbridge.codex
  "Drawbridge for OpenAI Codex - multi-headed REPL routing to Codex subprocess.

   Routes HTTP/WebSocket to Codex CLI subprocess. Supports:
   - Multiple input heads (HTTP, WebSocket, Agency direct)
   - Streaming output via WebSocket (Java-WebSocket)
   - Session resume via Codex's resume subcommand
   - Hot-reloadable (lives in the JVM with MUSN)

   Usage:
     (start! {:http-port 6769 :ws-port 6771 :resume-id \"abc123\"})
     ;; POST /codex with body to send input
     ;; WS ws://localhost:6771 for streaming output

   References:
   - https://developers.openai.com/codex/cli/reference/
   - https://developers.openai.com/codex/noninteractive/"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3.drawbridge.core :as core]))

;; =============================================================================
;; Codex-specific Invocation
;; =============================================================================

(defn- invoke-codex
  "Invoke Codex CLI with the given text. Returns {:result :session-id :exit-code}.

   Uses:
   - codex exec for non-interactive mode
   - --json --output-last-message for structured output
   - --full-auto for autonomous operation (no approval prompts)
   - codex exec resume <session-id> for session continuity

   Unlike Claude, Codex takes the prompt as an argument, not stdin."
  [text session-id]
  (let [;; Build command: codex exec [resume <id>] --json --output-last-message --full-auto "prompt"
        base-cmd (if session-id
                   ["codex" "exec" "resume" session-id]
                   ["codex" "exec"])
        cmd (into base-cmd ["--json" "--output-last-message" "--full-auto" text])
        _ (println (format "[codex-bridge] Executing: codex exec%s (prompt: %s)"
                           (if session-id (str " resume " session-id) "")
                           (subs text 0 (min 50 (count text)))))
        pb (ProcessBuilder. ^java.util.List cmd)
        _ (.redirectErrorStream pb true)
        proc (.start pb)
        stdout (io/reader (.getInputStream proc))]

    ;; Read JSON output
    (println "[codex-bridge] Reading JSON response...")
    (let [output (StringBuilder.)]
      (loop []
        (when-let [line (.readLine ^java.io.BufferedReader stdout)]
          (.append output line)
          (.append output "\n")
          (recur)))
      (.waitFor proc)
      (let [exit-code (.exitValue proc)
            out-str (str output)]
        (if (zero? exit-code)
          (try
            ;; Codex --json outputs newline-delimited JSON events
            ;; The last line with type "result" contains the final output
            (let [lines (str/split-lines out-str)
                  json-lines (keep #(try (json/parse-string % true) (catch Exception _ nil)) lines)
                  ;; Find the result event or last message
                  result-event (last (filter #(= (:type %) "result") json-lines))
                  last-message (or (:message result-event)
                                   (:content (last (filter :content json-lines)))
                                   out-str)
                  ;; Session ID is in the session event
                  session-event (first (filter #(= (:type %) "session") json-lines))
                  new-session-id (:session_id session-event)]
              (println (format "[codex-bridge] Session: %s" new-session-id))
              {:result last-message
               :session-id new-session-id
               :exit-code 0})
            (catch Exception e
              (println "[codex-bridge] JSON parse error:" (.getMessage e))
              ;; Fall back to raw output
              {:result out-str :exit-code 0}))
          (do
            (println "[codex-bridge] Process failed:" out-str)
            {:error out-str :exit-code exit-code}))))))

;; =============================================================================
;; Public API (wraps core with Codex defaults)
;; =============================================================================

(defn start!
  "Start the Codex Drawbridge.

   Options:
     :http-port  - HTTP port for REST API (default 6769)
     :ws-port    - WebSocket port for streaming (default 6771)
     :bind       - Bind address (default 127.0.0.1)
     :token      - Auth token (default from .admintoken or 'change-me')
     :resume-id  - Codex session ID to resume (optional)
     :agent-id   - Agent ID to register with Agency (optional)"
  [{:keys [http-port ws-port bind token resume-id agent-id]
    :or {http-port 6769
         ws-port 6771
         bind "127.0.0.1"}}]
  (core/start! {:http-port http-port
                :ws-port ws-port
                :bind bind
                :token token
                :invoke-fn invoke-codex
                :endpoint-prefix "codex"
                :resume-id resume-id
                :agent-id agent-id}))

(defn stop!
  "Stop the Codex Drawbridge."
  []
  (core/stop!))

;; Re-export useful functions from core
(def agent-alive? core/agent-alive?)
(def session-id core/session-id)
(def send-input! core/send-input!)
(def connect-irc! core/connect-irc!)
(def disconnect-irc! core/disconnect-irc!)
(def send-to-irc! core/send-to-irc!)
(def irc-connected? core/irc-connected?)

(comment
  ;; Development / REPL usage
  (start! {:http-port 6769
           :ws-port 6771
           :agent-id "codex"})
  (stop!)

  (agent-alive?)
  (send-input! "Hello from the REPL!")

  ;; From another process:
  ;; HTTP: curl -X POST http://localhost:6769/codex -d "Hello" -H "X-Admin-Token: your-token"
  ;; WebSocket: connect to ws://localhost:6771
  )
