(ns futon3.drawbridge.codex
  "Codex agent registration for the multi-agent Drawbridge router.

   Routes HTTP/WebSocket to Codex CLI subprocess. Each invocation spawns
   a new `codex exec` process (stateless per-invocation model).

   Usage:
     ;; Register a new Codex agent:
     (register! \"codex\" {:session-id \"thread_abc123\"})

     ;; Or use backwards-compat start! for remote agent mode:
     (start! {:http-port 6769 :ws-port 6771 :agent-id \"codex\"})

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
   - --json for structured output
   - config override approval_policy=\"never\" to prevent interactive approval prompts
     (important for paging/HTTP invocation where no human can approve)
   - --sandbox workspace-write to keep a safe default execution policy
   - codex exec resume <session-id> for session continuity

   NOTE: We pass the prompt via stdin (`-`), not argv, to avoid shell quoting
   hazards and length limits, and to keep parity with Codex CLI behavior:
   `codex exec -` and `codex exec resume <id> -`."
  [text session-id]
  (let [;; IMPORTANT: `resume` is a subcommand of `codex exec`, so exec-level opts must
        ;; come before the subcommand (otherwise they are parsed as resume args).
        exec-opts ["--json"
                   "--sandbox" "workspace-write"
                   "-c" "approval_policy=\"never\""]
        ;; Use "-" so Codex reads the prompt from stdin.
        cmd (if session-id
              (into ["codex" "exec"] (concat exec-opts ["resume" session-id "-"]))
              (into ["codex" "exec"] (concat exec-opts ["-"])))
        _ (println (format "[codex-bridge] Executing: codex exec%s (prompt: %s)"
                           (if session-id (str " resume " session-id) "")
                           (subs text 0 (min 50 (count text)))))
        pb (ProcessBuilder. ^java.util.List cmd)
        _ (.redirectErrorStream pb true)
        proc (.start pb)
        stdin (io/writer (.getOutputStream proc))
        stdout (io/reader (.getInputStream proc))]

    ;; Write prompt to stdin then close (Codex reads the whole prompt).
    (try
      (locking stdin
        (.write ^java.io.Writer stdin (str text "\n"))
        (.flush ^java.io.Writer stdin))
      (catch Exception e
        (println "[codex-bridge] Failed writing prompt to stdin:" (.getMessage e))))
    (try (.close ^java.io.Writer stdin) (catch Exception _))

    ;; Read JSON output
    (println "[codex-bridge] Reading JSON response...")
    (let [output (StringBuilder.)
          ;; Track session/thread id from events.
          session-id* (atom nil)
          ;; Accumulate agent text across events (Codex can emit multiple agent messages).
          agent-texts* (atom [])]
      (loop []
        (when-let [line (.readLine ^java.io.BufferedReader stdout)]
          (.append output line)
          (.append output "\n")
          ;; Best-effort parse each JSONL event as it arrives.
          (when-not (str/blank? line)
            (try
              (let [evt (json/parse-string line true)
                    t (:type evt)]
                (when (and (= t "thread.started") (nil? @session-id*))
                  (reset! session-id* (or (:thread_id evt) (:session_id evt))))

                (when (= t "item.completed")
                  (let [item (:item evt)
                        item-type (:type item)]
                    (when (= item-type "agent_message")
                      (let [content (or (:text item) (:content item))
                            text-content (cond
                                           (string? content) content
                                           (sequential? content) (->> content
                                                                     (filter #(= (:type %) "text"))
                                                                     (map :text)
                                                                     (remove nil?)
                                                                     (str/join ""))
                                           :else nil)]
                        (when (seq text-content)
                          (swap! agent-texts* conj text-content)))))))
              (catch Exception _)))
          (recur)))
      (.waitFor proc)
      (let [exit-code (.exitValue proc)
            out-str (str output)]
        (if (zero? exit-code)
          (try
            ;; Codex --json outputs newline-delimited JSON events
            (let [lines (str/split-lines out-str)
                  json-lines (keep #(try (json/parse-string % true) (catch Exception _ nil)) lines)
                  ;; Find the last agent message item
                  last-agent-message (->> json-lines
                                          (filter #(= (:type %) "item.completed"))
                                          (map :item)
                                          (filter #(= (:type %) "agent_message"))
                                          (map #(or (:text %) (:content %)))
                                          (remove nil?)
                                          last)
                  last-message (or last-agent-message
                                   (last @agent-texts*)
                                   (:message (last (filter :message json-lines)))
                                   (:content (last (filter :content json-lines)))
                                   out-str)
                  ;; Session ID is in the thread.started event
                  thread-event (first (filter #(= (:type %) "thread.started") json-lines))
                  new-session-id (or (:thread_id thread-event)
                                     (:session_id thread-event)
                                     @session-id*)]
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
;; Public API — Registration
;; =============================================================================

(defn register!
  "Register a Codex agent with the Drawbridge router.

   Options:
     :session-id      - Codex thread ID to resume (optional)
     :irc-nick        - IRC nickname (defaults to agent-id)
     :agency-http-url - Agency HTTP base URL (optional)"
  [agent-id {:keys [session-id irc-nick agency-http-url]}]
  (core/register-agent! agent-id
    {:invoke-fn invoke-codex
     :session-id session-id
     :irc-nick irc-nick
     :agency-http-url agency-http-url}))

(defn deregister!
  "Deregister a Codex agent."
  [agent-id]
  (core/deregister-agent! agent-id))

;; =============================================================================
;; Backwards-Compatible API (wraps core with Codex defaults)
;; =============================================================================

(defn start!
  "Start the Codex Drawbridge (backwards-compat).

   For multi-agent mode, use register! instead.

   Options:
     :http-port  - HTTP port for REST API (default 6769)
     :ws-port    - WebSocket port for streaming (default 6771)
     :bind       - Bind address (default 127.0.0.1)
     :token      - Auth token (default from .admintoken or 'change-me')
     :resume-id  - Codex session ID to resume (optional)
     :agent-id   - Agent ID to register with Agency (optional)
     :agency-ws-url - Agency WS URL (optional)
     :agency-http-url - Agency HTTP base URL (optional)
     :agency-ws-agent-id - Agent id to use when connecting to Agency WS (optional)
     :agency-ws-reconnect-ms - Reconnect delay for Agency WS (optional)
     :agency-ws-ping-ms - Ping interval for Agency WS (optional)
     :register-local? - Register local handler with Agency (default true)"
  [{:keys [http-port ws-port bind token resume-id agent-id
           agency-ws-url agency-http-url agency-ws-agent-id agency-ws-reconnect-ms agency-ws-ping-ms register-local?]
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
                :agent-id agent-id
                :agency-ws-url agency-ws-url
                :agency-http-url agency-http-url
                :agency-ws-agent-id agency-ws-agent-id
                :agency-ws-reconnect-ms agency-ws-reconnect-ms
                :agency-ws-ping-ms agency-ws-ping-ms
                :register-local? (if (nil? register-local?) true register-local?)}))

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

;; Per-agent exports
(def register-agent! core/register-agent!)
(def deregister-agent! core/deregister-agent!)
(def registered-agents core/registered-agents)
(def send-input-to! core/send-input-to!)
(def connect-irc-for! core/connect-irc-for!)
(def disconnect-irc-for! core/disconnect-irc-for!)
(def send-to-irc-for! core/send-to-irc-for!)
(def irc-connected-for? core/irc-connected-for?)

(comment
  ;; === Multi-agent usage ===
  (register! "codex" {:session-id "thread_abc123"})
  (registered-agents)
  (send-input-to! "codex" "Hello from the REPL!")
  (deregister! "codex")

  ;; === Backwards-compat ===
  (start! {:http-port 6769
           :ws-port 6771
           :agent-id "codex"})
  (stop!)

  ;; From another process:
  ;; HTTP: curl -X POST http://localhost:6769/codex -d "Hello" -H "X-Admin-Token: your-token"
  ;; WebSocket: connect to ws://localhost:6771
  )
