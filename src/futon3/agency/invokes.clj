(ns futon3.agency.invokes
  "Invoke functions for different agent types.

   Extracted from drawbridge/claude.clj and drawbridge/codex.clj for use
   with the unified Agency registry. Each function factory returns an
   invoke-fn suitable for registration.

   Invoke-fn contract:
     (invoke-fn prompt session-id) -> {:result ... :session-id ... :exit-code ... :error ...}

   See: M-agency-unified-routing.md
   Credits: Codex improvements from f24b55a (stdin, JSONL parsing)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private default-invoke-timeout-ms
  (or (some-> (System/getenv "AGENCY_INVOKE_TIMEOUT_MS") Long/parseLong)
      60000))

(def ^:private codex-bin
  (or (System/getenv "AGENCY_CODEX_BIN") "codex"))

(def ^:private claude-bin
  (or (System/getenv "AGENCY_CLAUDE_BIN") "claude"))

;; =============================================================================
;; Codex Invocation (one-shot exec)
;; Extracted from drawbridge/codex.clj with improvements from f24b55a
;; =============================================================================

(defn invoke-codex
  "Invoke Codex CLI with the given text. Returns {:result :session-id :exit-code}.

   Uses:
   - codex exec for non-interactive mode
   - --json for structured output
   - config override approval_policy=\"never\" to prevent interactive approval prompts
     (important for paging/HTTP invocation where no human can approve)
   - --sandbox workspace-write to keep a safe default execution policy
   - codex exec resume <session-id> for session continuity

   Binary path configurable via AGENCY_CODEX_BIN env var.

   NOTE: We pass the prompt via stdin (`-`), not argv, to avoid shell quoting
   hazards and length limits (from f24b55a)."
  ([text session-id]
   (invoke-codex text session-id default-invoke-timeout-ms))
  ([text session-id timeout-ms]
   (let [timeout-ms (long (or timeout-ms default-invoke-timeout-ms))
         ;; IMPORTANT: `resume` is a subcommand of `codex exec`, so exec-level opts must
         ;; come before the subcommand (otherwise they are parsed as resume args).
         exec-opts ["--json"
                    "--sandbox" "workspace-write"
                    "-c" "approval_policy=\"never\""]
         cmd (if session-id
               (into [codex-bin "exec"] (concat exec-opts ["resume" session-id "-"]))
               (into [codex-bin "exec"] (concat exec-opts ["-"])))
         _ (println (format "[invoke-codex] Executing: %s exec%s (timeout=%sms, prompt: %.50s...)"
                            codex-bin
                            (if session-id (str " resume " session-id) "")
                            timeout-ms
                            text))
         pb (ProcessBuilder. ^java.util.List cmd)
         _ (.redirectErrorStream pb true)
         proc (.start pb)
         stdin (io/writer (.getOutputStream proc))
         stdout (io/reader (.getInputStream proc))
         output (StringBuilder.)
         session-id* (atom nil)
         agent-texts* (atom [])]

     ;; Write prompt to stdin then close.
     (try
       (locking stdin
         (.write ^java.io.Writer stdin (str text "\n"))
         (.flush ^java.io.Writer stdin))
       (catch Exception e
         (println "[invoke-codex] Failed writing prompt to stdin:" (.getMessage e))))
     (try (.close ^java.io.Writer stdin) (catch Exception _))

     ;; Read output asynchronously so we can enforce a process timeout.
     (let [reader-fut
           (future
             (try
               (loop []
                 (when-let [line (.readLine ^java.io.BufferedReader stdout)]
                   (.append output line)
                   (.append output "\n")
                   (when-not (str/blank? line)
                     (try
                       (let [evt (json/parse-string line true)
                             t (:type evt)]
                         (when (and (= t "thread.started") (nil? @session-id*))
                           (reset! session-id* (or (:thread_id evt) (:session_id evt))))
                         (when (= t "item.completed")
                           (let [item (:item evt)]
                             (when (= (:type item) "agent_message")
                               (let [content (or (:text item) (:content item))
                                     text-content (cond
                                                    (string? content) content
                                                    (sequential? content)
                                                    (->> content
                                                         (filter #(= (:type %) "text"))
                                                         (map :text)
                                                         (remove nil?)
                                                         (str/join ""))
                                                    :else nil)]
                                 (when (seq text-content)
                                   (swap! agent-texts* conj text-content)))))))
                       (catch Exception _)))
                   (recur)))
               (catch Exception _
                 nil)))]

       (println "[invoke-codex] Reading JSON response...")
       (let [finished? (.waitFor proc timeout-ms TimeUnit/MILLISECONDS)]
         (when-not finished?
           (println (format "[invoke-codex] Timeout after %sms; killing process" timeout-ms))
           (try (.destroy ^Process proc) (catch Exception _))
           (try (.waitFor ^Process proc 1000 TimeUnit/MILLISECONDS) (catch Exception _))
           (when (.isAlive ^Process proc)
             (try (.destroyForcibly ^Process proc) (catch Exception _))))

         ;; Best effort: let the reader drain/exit.
         (deref reader-fut 2000 nil)

         (let [out-str (str output)]
           (if-not finished?
             {:error "timeout" :exit-code -1}
             (let [exit-code (.exitValue proc)]
               (if (zero? exit-code)
                 (let [last-message (or (last @agent-texts*) out-str)
                       new-session-id @session-id*]
                   (println (format "[invoke-codex] Session: %s" new-session-id))
                   {:result last-message
                    :session-id new-session-id
                    :exit-code 0})
                 (do
                   (println "[invoke-codex] Process failed:" out-str)
                   {:error out-str :exit-code exit-code}))))))))))

(defn make-codex-invoke-fn
  "Factory: Create a Codex invoke function.

   Returns an invoke-fn suitable for registry registration.
   Codex uses one-shot exec, so no persistent state needed."
  []
  invoke-codex)

;; =============================================================================
;; Claude Invocation (persistent subprocess)
;; Extracted from drawbridge/claude.clj
;; =============================================================================

(defn start-claude-subprocess!
  "Start a persistent Claude subprocess with streaming JSON I/O.

   Binary path configurable via AGENCY_CLAUDE_BIN env var.
   Returns {:process :stdin :stdout :queue :reader-thread} or nil on failure."
  [session-id]
  (let [cmd (cond-> [claude-bin
                     "--input-format" "stream-json"
                     "--output-format" "stream-json"
                     "--permission-mode" "bypassPermissions"
                     "--verbose"]
              session-id (into ["--resume" session-id]))
        _ (println (format "[invoke-claude] Starting persistent subprocess: %s"
                           (str/join " " cmd)))
        pb (ProcessBuilder. ^java.util.List cmd)
        _ (.directory pb (java.io.File. (System/getProperty "user.home")))
        proc (.start pb)
        stdin (io/writer (.getOutputStream proc))
        stdout (io/reader (.getInputStream proc))
        queue (LinkedBlockingQueue.)]

    ;; Start reader thread to consume stdout
    (let [reader-thread
          (Thread.
           (fn []
             (println "[invoke-claude] Reader thread started")
             (try
               (loop []
                 (when-let [line (.readLine ^java.io.BufferedReader stdout)]
                   (try
                     (let [parsed (json/parse-string line true)]
                       (.put queue parsed))
                     (catch Exception e
                       (println (format "[invoke-claude] Parse error: %s" (.getMessage e)))))
                   (when (.isAlive proc)
                     (recur))))
               (catch Exception e
                 (println (format "[invoke-claude] Reader error: %s" (.getMessage e)))))
             (println "[invoke-claude] Reader thread exiting")))]
      (.start reader-thread)
      {:process proc
       :stdin stdin
       :stdout stdout
       :queue queue
       :reader-thread reader-thread
       :session-id session-id})))

(defn stop-claude-subprocess!
  "Stop a Claude subprocess and clean up resources."
  [{:keys [process stdin]}]
  (when process
    (println "[invoke-claude] Stopping subprocess...")
    (try
      (when stdin (.close stdin))
      (.destroy ^Process process)
      (.waitFor ^Process process 5 TimeUnit/SECONDS)
      (when (.isAlive process)
        (.destroyForcibly process))
      (catch Exception e
        (println (format "[invoke-claude] Stop error: %s" (.getMessage e)))))))

(defn send-to-claude!
  "Send a message to a Claude subprocess.

   Returns the response or error map."
  [{:keys [process stdin queue]} text timeout-ms]
  (if (and process (.isAlive process))
    (let [msg {:type "user"
               :message {:role "user"
                         :content text}}]
      (try
        (println (format "[invoke-claude] Sending: %.50s..." text))
        (locking stdin
          (.write ^java.io.Writer stdin (json/generate-string msg))
          (.write ^java.io.Writer stdin "\n")
          (.flush ^java.io.Writer stdin))

        ;; Wait for response
        (loop [deadline (+ (System/currentTimeMillis) timeout-ms)
               result nil]
          (if (> (System/currentTimeMillis) deadline)
            (do
              (println "[invoke-claude] Timeout waiting for response")
              {:error "timeout" :exit-code -1})
            (if-let [resp (.poll ^LinkedBlockingQueue queue 100 TimeUnit/MILLISECONDS)]
              (let [msg-type (:type resp)]
                (cond
                  (= msg-type "result")
                  (do
                    (println "[invoke-claude] Got result")
                    {:result (:result resp)
                     :session-id (:session_id resp)
                     :exit-code 0})

                  (= msg-type "assistant")
                  (let [content (get-in resp [:message :content])
                        text-content (when (sequential? content)
                                       (->> content
                                            (filter #(= (:type %) "text"))
                                            (map :text)
                                            (str/join "")))]
                    (recur deadline (or text-content result)))

                  :else
                  (recur deadline result)))
              (recur deadline result))))
        (catch Exception e
          (println (format "[invoke-claude] Send error: %s" (.getMessage e)))
          {:error (.getMessage e) :exit-code -1})))
    {:error "process not alive" :exit-code -1}))

(defn make-claude-invoke-fn
  "Factory: Create a Claude invoke function with persistent subprocess.

   Returns [invoke-fn subprocess-state] where:
   - invoke-fn is suitable for registry registration
   - subprocess-state contains the process for cleanup

   The subprocess is started immediately. Call stop-claude-subprocess!
   on the subprocess-state when unregistering."
  [initial-session-id]
  (let [subprocess (atom nil)]
    ;; Start subprocess
    (reset! subprocess (start-claude-subprocess! initial-session-id))
    (if @subprocess
      ;; Return invoke-fn and subprocess state
      [(fn invoke-claude
         ([text session-id]
          (invoke-claude text session-id default-invoke-timeout-ms))
         ([text session-id timeout-ms]
          ;; Restart if dead
          (when (or (nil? @subprocess)
                    (not (.isAlive ^Process (:process @subprocess))))
            (println "[invoke-claude] Subprocess dead, restarting...")
            (reset! subprocess (start-claude-subprocess! session-id)))
          (if @subprocess
            (send-to-claude! @subprocess text (or timeout-ms default-invoke-timeout-ms))
            {:error "failed to start subprocess" :exit-code -1})))
       @subprocess]
      ;; Failed to start
      [nil nil])))

;; =============================================================================
;; Helper: Determine agent type from ID or config
;; =============================================================================

(defn infer-agent-type
  "Infer agent type from agent-id prefix or explicit type.

   Convention:
   - IDs starting with 'claude' -> :claude
   - IDs starting with 'codex' -> :codex
   - Otherwise: explicit type required"
  [agent-id]
  (cond
    (str/starts-with? (str agent-id) "claude") :claude
    (str/starts-with? (str agent-id) "codex") :codex
    :else nil))

(comment
  ;; Test Codex invoke (requires codex CLI)
  (invoke-codex "Say hello" nil)

  ;; Test Claude invoke factory
  (let [[invoke-fn subprocess] (make-claude-invoke-fn nil)]
    (when invoke-fn
      (println (invoke-fn "Hello Claude!" nil))
      (stop-claude-subprocess! subprocess)))
  )
