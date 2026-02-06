(ns futon3.drawbridge.claude
  "Drawbridge for Claude Code - multi-headed REPL routing to Claude subprocess.

   Like cemerick/drawbridge routes HTTP to nREPL, this routes HTTP/WebSocket
   to a Claude Code subprocess. Supports:
   - Multiple input heads (HTTP, WebSocket, Agency direct)
   - Streaming output via WebSocket (Java-WebSocket, not http-kit)
   - Persistent subprocess with streaming I/O (one process, many inputs)
   - Session resume via Claude's --resume flag
   - Hot-reloadable (lives in the JVM with MUSN)

   Usage:
     (start! {:http-port 6768 :ws-port 6770 :resume-id \"abc123\"})
     ;; POST /claude with body to send input
     ;; WS ws://localhost:6770 for streaming output + push"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3.drawbridge.core :as core])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; =============================================================================
;; Persistent Claude Subprocess (streaming mode)
;; =============================================================================

(defonce ^:private claude-process (atom nil))
(defonce ^:private claude-stdin (atom nil))
(defonce ^:private claude-stdout (atom nil))
(defonce ^:private response-queue (atom nil))
(defonce ^:private reader-thread (atom nil))
(defonce ^:private current-session-id (atom nil))

(defn- start-claude-process!
  "Start a persistent Claude subprocess with streaming JSON I/O.
   Returns true if started successfully."
  [session-id]
  (when @claude-process
    (println "[claude-bridge] Stopping existing process...")
    (try (.destroy ^Process @claude-process) (catch Exception _)))

  (let [cmd (cond-> ["claude"
                     "--input-format" "stream-json"
                     "--output-format" "stream-json"
                     "--permission-mode" "bypassPermissions"
                     "--verbose"]
              session-id (into ["--resume" session-id]))
        _ (println (format "[claude-bridge] Starting persistent subprocess: %s"
                           (str/join " " cmd)))
        pb (ProcessBuilder. ^java.util.List cmd)
        _ (.directory pb (java.io.File. (System/getProperty "user.home")))
        proc (.start pb)
        stdin (io/writer (.getOutputStream proc))
        stdout (io/reader (.getInputStream proc))
        queue (LinkedBlockingQueue.)]

    (reset! claude-process proc)
    (reset! claude-stdin stdin)
    (reset! claude-stdout stdout)
    (reset! response-queue queue)
    (reset! current-session-id session-id)

    ;; Start reader thread to consume stdout
    (reset! reader-thread
            (Thread.
             (fn []
               (println "[claude-bridge] Reader thread started")
               (try
                 (loop []
                   (when-let [line (.readLine ^java.io.BufferedReader stdout)]
                     (println (format "[claude-bridge] Got line: %.100s..." line))
                     (try
                       (let [parsed (json/parse-string line true)]
                         (.put queue parsed))
                       (catch Exception e
                         (println (format "[claude-bridge] Parse error: %s" (.getMessage e)))))
                     (when (.isAlive proc)
                       (recur))))
                 (catch Exception e
                   (println (format "[claude-bridge] Reader error: %s" (.getMessage e)))))
               (println "[claude-bridge] Reader thread exiting"))))
    (.start ^Thread @reader-thread)

    (println "[claude-bridge] Persistent subprocess started")
    true))

(defn- stop-claude-process!
  "Stop the persistent Claude subprocess."
  []
  (when-let [proc @claude-process]
    (println "[claude-bridge] Stopping subprocess...")
    (try
      (when-let [stdin @claude-stdin]
        (.close stdin))
      (.destroy ^Process proc)
      (.waitFor ^Process proc 5 TimeUnit/SECONDS)
      (when (.isAlive proc)
        (.destroyForcibly proc))
      (catch Exception e
        (println (format "[claude-bridge] Stop error: %s" (.getMessage e)))))
    (reset! claude-process nil)
    (reset! claude-stdin nil)
    (reset! claude-stdout nil)
    (reset! response-queue nil)
    (reset! current-session-id nil)))

(defn- send-to-claude!
  "Send a message to the persistent Claude subprocess.
   Returns the response or nil on timeout."
  [text timeout-ms]
  (if-let [proc @claude-process]
    (if (.isAlive proc)
      (let [stdin @claude-stdin
            queue @response-queue
            msg {:type "user"
                 :message {:role "user"
                           :content text}}]
        (try
          ;; Send JSON message
          (println (format "[claude-bridge] Sending: %.50s..." text))
          (locking stdin
            (.write ^java.io.Writer stdin (json/generate-string msg))
            (.write ^java.io.Writer stdin "\n")
            (.flush ^java.io.Writer stdin))

          ;; Wait for response
          (loop [deadline (+ (System/currentTimeMillis) timeout-ms)
                 result nil]
            (if (> (System/currentTimeMillis) deadline)
              (do
                (println "[claude-bridge] Timeout waiting for response")
                {:error "timeout" :exit-code -1})
              (if-let [resp (.poll ^LinkedBlockingQueue queue 100 TimeUnit/MILLISECONDS)]
                (let [msg-type (:type resp)]
                  (cond
                    ;; Final result
                    (= msg-type "result")
                    (do
                      (println (format "[claude-bridge] Got result"))
                      {:result (:result resp)
                       :session-id (:session_id resp)
                       :exit-code 0})

                    ;; Assistant message (accumulate)
                    (= msg-type "assistant")
                    (let [content (get-in resp [:message :content])
                          text-content (when (sequential? content)
                                         (->> content
                                              (filter #(= (:type %) "text"))
                                              (map :text)
                                              (str/join "")))]
                      (recur deadline (or text-content result)))

                    ;; Other message types - keep waiting
                    :else
                    (recur deadline result)))
                (recur deadline result))))
          (catch Exception e
            (println (format "[claude-bridge] Send error: %s" (.getMessage e)))
            {:error (.getMessage e) :exit-code -1})))
      (do
        (println "[claude-bridge] Process not alive, restarting...")
        (start-claude-process! @current-session-id)
        {:error "process restarted, retry" :exit-code -1}))
    {:error "no process" :exit-code -1}))

;; =============================================================================
;; Invoke function for core (uses persistent subprocess)
;; =============================================================================

(defn- invoke-claude
  "Send text to the persistent Claude subprocess."
  [text session-id]
  ;; Start process if not running
  (when (or (nil? @claude-process)
            (not (.isAlive ^Process @claude-process)))
    (start-claude-process! session-id))

  ;; Send and get response
  (send-to-claude! text 60000))

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
  "Stop the Claude Drawbridge and persistent subprocess."
  []
  (stop-claude-process!)
  (core/stop!))

;; Re-export useful functions from core
(def agent-alive? core/agent-alive?)
(def session-id core/session-id)
(def send-input! core/send-input!)
(def connect-irc! core/connect-irc!)
(def disconnect-irc! core/disconnect-irc!)
(def send-to-irc! core/send-to-irc!)
(def irc-connected? core/irc-connected?)

;; Persistent subprocess management
(defn subprocess-alive?
  "Check if the persistent Claude subprocess is alive."
  []
  (and @claude-process (.isAlive ^Process @claude-process)))

(defn subprocess-session
  "Get the session ID of the persistent subprocess."
  []
  @current-session-id)

(defn release-session!
  "Stop the persistent subprocess and release the session for CLI takeover.
   Returns the session ID so you can resume it elsewhere."
  []
  (let [sid @current-session-id]
    (stop-claude-process!)
    (println (format "[claude-bridge] Session released: %s" sid))
    (println (format "[claude-bridge] Resume with: claude --resume %s" sid))
    sid))

(comment
  ;; Development / REPL usage
  (start! {:http-port 6768
           :ws-port 6770
           :resume-id "64570417-4354-40b8-b6a5-db804f69a1d0"})
  (stop!)

  (agent-alive?)
  (subprocess-alive?)
  (subprocess-session)
  (send-input! "Hello from the REPL!")

  ;; Release session for CLI takeover
  (release-session!)
  ;; Then run: claude --resume <session-id>

  ;; From another process:
  ;; HTTP: curl -X POST http://localhost:6768/claude -d "Hello" -H "X-Admin-Token: your-token"
  ;; WebSocket: connect to ws://localhost:6770

  ;; Workflow:
  ;; 1. Start drawbridge with resume-id -> persistent subprocess owns session
  ;; 2. IRC/HTTP/WS pages flow into subprocess
  ;; 3. (release-session!) -> subprocess stops, session released
  ;; 4. claude --resume <id> -> CLI takes over same session
  ;; 5. Exit CLI, restart drawbridge -> back to multi-input mode
  )
