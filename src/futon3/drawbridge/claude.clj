(ns futon3.drawbridge.claude
  "Drawbridge for Claude Code - multi-headed REPL routing to Claude subprocess.

   Like cemerick/drawbridge routes HTTP to nREPL, this routes HTTP/WebSocket
   to a Claude Code subprocess. Supports:
   - Multiple input heads (HTTP, WebSocket, Agency direct)
   - Streaming output via WebSocket (Java-WebSocket, not http-kit)
   - Session resume via Claude's --resume flag
   - Hot-reloadable (lives in the JVM with MUSN)

   Usage:
     (start! {:http-port 6768 :ws-port 6770 :resume-id \"abc123\"})
     ;; POST /claude with body to send input
     ;; WS ws://localhost:6770 for streaming output + push"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3.drawbridge.core :as core]))

;; =============================================================================
;; Claude-specific Invocation
;; =============================================================================

(defn- invoke-claude
  "Invoke Claude CLI with the given text. Returns {:result :session-id :exit-code}.

   Uses:
   - --print for non-interactive mode
   - --output-format json for structured output
   - --permission-mode bypassPermissions for autonomous operation
   - --resume <session-id> for thread-safe session persistence"
  [text session-id]
  (let [cmd (cond-> ["claude" "--print" "--output-format" "json"
                     "--permission-mode" "bypassPermissions"]
              session-id (into ["--resume" session-id]))
        _ (println (format "[claude-bridge] Executing: claude --print%s (stdin: %s)"
                           (if session-id (str " --resume " session-id) "")
                           (subs text 0 (min 50 (count text)))))
        pb (ProcessBuilder. ^java.util.List cmd)
        ;; Set working directory to user home for session lookup
        _ (.directory pb (java.io.File. (System/getProperty "user.home")))
        _ (.redirectErrorStream pb true)
        proc (.start pb)
        stdin (io/writer (.getOutputStream proc))
        stdout (io/reader (.getInputStream proc))]

    ;; Send prompt via stdin (not as CLI argument - that hangs)
    (doto stdin
      (.write text)
      (.flush)
      (.close))

    ;; Read JSON output
    (println "[claude-bridge] Reading JSON response...")
    (let [output (StringBuilder.)]
      (loop []
        (when-let [line (.readLine ^java.io.BufferedReader stdout)]
          (.append output line)
          (recur)))
      (.waitFor proc)
      (let [exit-code (.exitValue proc)
            out-str (str output)]
        (if (zero? exit-code)
          (try
            (let [parsed (json/parse-string out-str true)
                  result (:result parsed)
                  new-session-id (:session_id parsed)]
              (println (format "[claude-bridge] Session: %s" new-session-id))
              {:result result
               :session-id new-session-id
               :exit-code 0})
            (catch Exception e
              (println "[claude-bridge] JSON parse error:" (.getMessage e))
              {:result out-str :exit-code 0}))
          (do
            (println "[claude-bridge] Process failed:" out-str)
            {:error out-str :exit-code exit-code}))))))

;; =============================================================================
;; Public API (wraps core with Claude defaults)
;; =============================================================================

(defn start!
  "Start the Claude Drawbridge.

   Options:
     :http-port  - HTTP port for REST API (default 6768)
     :ws-port    - WebSocket port for streaming (default 6770)
     :bind       - Bind address (default 127.0.0.1)
     :token      - Auth token (default from .admintoken or 'change-me')
     :resume-id  - Claude session ID to resume (optional)
     :agent-id   - Agent ID to register with Agency (optional)
     :agency-ws-url - Agency WS URL (optional)
     :agency-http-url - Agency HTTP base URL (optional)
     :agency-ws-agent-id - Agent id to use when connecting to Agency WS (optional)
     :agency-ws-reconnect-ms - Reconnect delay for Agency WS (optional)
     :agency-ws-ping-ms - Ping interval for Agency WS (optional)
     :register-local? - Register local handler with Agency (default true)"
  [{:keys [http-port ws-port bind token resume-id agent-id
           agency-ws-url agency-http-url agency-ws-agent-id agency-ws-reconnect-ms agency-ws-ping-ms register-local?]
    :or {http-port 6768
         ws-port 6770
         bind "127.0.0.1"}}]
  (core/start! {:http-port http-port
                :ws-port ws-port
                :bind bind
                :token token
                :invoke-fn invoke-claude
                :endpoint-prefix "claude"
                :resume-id resume-id
                :agent-id agent-id
                :agency-ws-url agency-ws-url
                :agency-http-url agency-http-url
                :agency-ws-agent-id agency-ws-agent-id
                :agency-ws-reconnect-ms agency-ws-reconnect-ms
                :agency-ws-ping-ms agency-ws-ping-ms
                :register-local? (if (nil? register-local?) true register-local?)}))

(defn stop!
  "Stop the Claude Drawbridge."
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
  (start! {:http-port 6768
           :ws-port 6770
           :resume-id "64570417-4354-40b8-b6a5-db804f69a1d0"})
  (stop!)

  (agent-alive?)
  (send-input! "Hello from the REPL!")

  ;; From another process:
  ;; HTTP: curl -X POST http://localhost:6768/claude -d "Hello" -H "X-Admin-Token: your-token"
  ;; WebSocket: connect to ws://localhost:6770
  )
