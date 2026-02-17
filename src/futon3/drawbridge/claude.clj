(ns futon3.drawbridge.claude
  "Claude agent registration for the multi-agent Drawbridge router.

   Manages persistent Claude Code subprocesses (streaming mode) and registers
   them with the Drawbridge for Agency routing. Multiple Claude agents can run
   simultaneously with different agent-ids and session-ids.

   Usage:
     ;; Register a new Claude agent:
     (register! \"claude-agency\" {:session-id \"abc123\"})

     ;; Or use backwards-compat start! for remote agent mode:
     (start! {:http-port 6768 :ws-port 6770 :agent-id \"claude\" :resume-id \"abc123\"})

   Each registered agent gets its own persistent subprocess."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3.drawbridge.core :as core])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; =============================================================================
;; Per-Agent Claude Subprocess Management
;; =============================================================================

(defonce ^:private claude-processes
  (atom {}))
;; {agent-id -> {:process Process
;;               :stdin Writer
;;               :stdout Reader
;;               :queue LinkedBlockingQueue
;;               :reader-thread Thread
;;               :session-id str}}

(defn- start-claude-process!
  "Start a persistent Claude subprocess for a specific agent.
   Returns true if started successfully."
  [agent-id session-id]
  (let [aid (name agent-id)]
    ;; Stop existing process for this agent if any
    (when-let [existing (get @claude-processes aid)]
      (println (format "[claude-bridge:%s] Stopping existing process..." aid))
      (try (.destroy ^Process (:process existing)) (catch Exception _)))

    (let [cmd (cond-> ["claude"
                       "--input-format" "stream-json"
                       "--output-format" "stream-json"
                       "--permission-mode" "bypassPermissions"
                       "--verbose"]
                session-id (into ["--resume" session-id]))
          _ (println (format "[claude-bridge:%s] Starting persistent subprocess: %s"
                             aid (str/join " " cmd)))
          pb (ProcessBuilder. ^java.util.List cmd)
          _ (.directory pb (java.io.File. (System/getProperty "user.home")))
          proc (.start pb)
          stdin (io/writer (.getOutputStream proc))
          stdout (io/reader (.getInputStream proc))
          queue (LinkedBlockingQueue.)
          reader-thread
          (Thread.
           (fn []
             (println (format "[claude-bridge:%s] Reader thread started" aid))
             (try
               (loop []
                 (when-let [line (.readLine ^java.io.BufferedReader stdout)]
                   (println (format "[claude-bridge:%s] Got line: %.100s..." aid line))
                   (try
                     (let [parsed (json/parse-string line true)]
                       (.put queue parsed))
                     (catch Exception e
                       (println (format "[claude-bridge:%s] Parse error: %s" aid (.getMessage e)))))
                   (when (.isAlive proc)
                     (recur))))
               (catch Exception e
                 (println (format "[claude-bridge:%s] Reader error: %s" aid (.getMessage e)))))
             (println (format "[claude-bridge:%s] Reader thread exiting" aid))))]

      (swap! claude-processes assoc aid
             {:process proc
              :stdin stdin
              :stdout stdout
              :queue queue
              :reader-thread reader-thread
              :session-id session-id})

      (.start reader-thread)
      (println (format "[claude-bridge:%s] Persistent subprocess started" aid))
      true)))

(defn- stop-claude-process!
  "Stop the persistent Claude subprocess for a specific agent."
  [agent-id]
  (let [aid (name agent-id)]
    (when-let [{:keys [process stdin]} (get @claude-processes aid)]
      (println (format "[claude-bridge:%s] Stopping subprocess..." aid))
      (try
        (when stdin (.close stdin))
        (.destroy ^Process process)
        (.waitFor ^Process process 5 TimeUnit/SECONDS)
        (when (.isAlive process)
          (.destroyForcibly process))
        (catch Exception e
          (println (format "[claude-bridge:%s] Stop error: %s" aid (.getMessage e)))))
      (swap! claude-processes dissoc aid))))

(defn- send-to-claude!
  "Send a message to a specific agent's Claude subprocess.
   Returns the response or error on timeout."
  [agent-id text timeout-ms]
  (let [aid (name agent-id)]
    (if-let [{:keys [process stdin queue session-id]} (get @claude-processes aid)]
      (if (.isAlive ^Process process)
        (let [msg {:type "user"
                   :message {:role "user"
                             :content text}}]
          (try
            (println (format "[claude-bridge:%s] Sending: %.50s..." aid text))
            (locking stdin
              (.write ^java.io.Writer stdin (json/generate-string msg))
              (.write ^java.io.Writer stdin "\n")
              (.flush ^java.io.Writer stdin))

            (loop [deadline (+ (System/currentTimeMillis) timeout-ms)
                   result nil]
              (if (> (System/currentTimeMillis) deadline)
                (do
                  (println (format "[claude-bridge:%s] Timeout waiting for response" aid))
                  {:error "timeout" :exit-code -1})
                (if-let [resp (.poll ^LinkedBlockingQueue queue 100 TimeUnit/MILLISECONDS)]
                  (let [msg-type (:type resp)]
                    (cond
                      (= msg-type "result")
                      (do
                        (println (format "[claude-bridge:%s] Got result" aid))
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
              (println (format "[claude-bridge:%s] Send error: %s" aid (.getMessage e)))
              {:error (.getMessage e) :exit-code -1})))
        (do
          (println (format "[claude-bridge:%s] Process not alive, restarting..." aid))
          (start-claude-process! aid session-id)
          {:error "process restarted, retry" :exit-code -1}))
      {:error "no process" :exit-code -1})))

(defn- invoke-claude
  "Send text to a specific agent's persistent Claude subprocess."
  [agent-id text session-id]
  (let [aid (name agent-id)]
    ;; Start process if not running
    (when (or (nil? (get @claude-processes aid))
              (not (.isAlive ^Process (:process (get @claude-processes aid)))))
      (start-claude-process! aid session-id))
    (send-to-claude! aid text 60000)))

;; =============================================================================
;; Public API — Registration
;; =============================================================================

(defn register!
  "Register a Claude agent with the Drawbridge router.
   Starts a persistent Claude subprocess and registers its invoke-fn.

   Options:
     :session-id      - Claude session ID to resume (optional)
     :irc-nick        - IRC nickname (defaults to agent-id)
     :agency-http-url - Agency HTTP base URL (optional)"
  [agent-id {:keys [session-id irc-nick agency-http-url]}]
  (let [aid (name agent-id)]
    (start-claude-process! aid session-id)
    (core/register-agent! aid
      {:invoke-fn (fn [text sid] (invoke-claude aid text sid))
       :session-id session-id
       :irc-nick irc-nick
       :agency-http-url agency-http-url})
    (println (format "[claude-bridge:%s] Registered with Drawbridge" aid))
    aid))

(defn deregister!
  "Deregister a Claude agent. Stops its subprocess."
  [agent-id]
  (let [aid (name agent-id)]
    (stop-claude-process! aid)
    (core/deregister-agent! aid)
    (println (format "[claude-bridge:%s] Deregistered" aid))))

;; =============================================================================
;; Backwards-Compatible API (wraps core with Claude defaults)
;; =============================================================================

(defn start!
  "Start the Claude Drawbridge (backwards-compat).

   For multi-agent mode, use register! instead.

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
  (let [aid (or agent-id "claude")]
    ;; Start the subprocess
    (start-claude-process! aid resume-id)
    ;; Start the drawbridge (HTTP/WS + registration)
    (core/start! {:http-port http-port
                  :ws-port ws-port
                  :bind bind
                  :token token
                  :invoke-fn (fn [text sid] (invoke-claude aid text sid))
                  :endpoint-prefix "claude"
                  :resume-id resume-id
                  :agent-id aid
                  :agency-ws-url agency-ws-url
                  :agency-http-url agency-http-url
                  :agency-ws-agent-id agency-ws-agent-id
                  :agency-ws-reconnect-ms agency-ws-reconnect-ms
                  :agency-ws-ping-ms agency-ws-ping-ms
                  :register-local? (if (nil? register-local?) true register-local?)})))

(defn stop!
  "Stop the Claude Drawbridge and all Claude subprocesses."
  []
  ;; Stop all Claude subprocesses
  (doseq [aid (keys @claude-processes)]
    (stop-claude-process! aid))
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

;; Claude-specific subprocess management
(defn subprocess-alive?
  "Check if a Claude subprocess is alive."
  ([] (some (fn [[_ p]] (.isAlive ^Process (:process p))) @claude-processes))
  ([agent-id]
   (when-let [{:keys [process]} (get @claude-processes (name agent-id))]
     (.isAlive ^Process process))))

(defn subprocess-session
  "Get the session ID of a Claude subprocess."
  ([] (some-> (first @claude-processes) val :session-id))
  ([agent-id] (:session-id (get @claude-processes (name agent-id)))))

(defn release-session!
  "Stop a Claude subprocess and release the session for CLI takeover.
   Returns the session ID so you can resume it elsewhere."
  [agent-id]
  (let [aid (name agent-id)
        sid (:session-id (get @claude-processes aid))]
    (stop-claude-process! aid)
    (println (format "[claude-bridge:%s] Session released: %s" aid sid))
    (println (format "[claude-bridge:%s] Resume with: claude --resume %s" aid sid))
    sid))

(comment
  ;; === Multi-agent usage ===
  (register! "claude-agency" {:session-id "64570417-4354-40b8-b6a5-db804f69a1d0"})
  (register! "claude-forum" {})

  (registered-agents)
  (subprocess-alive? "claude-agency")
  (subprocess-session "claude-agency")

  (send-input-to! "claude-agency" "Hello from the REPL!")

  ;; Release session for CLI takeover
  (release-session! "claude-agency")
  ;; Then run: claude --resume <session-id>

  (deregister! "claude-forum")

  ;; === Backwards-compat ===
  (start! {:http-port 6768
           :ws-port 6770
           :agent-id "claude"
           :resume-id "64570417-4354-40b8-b6a5-db804f69a1d0"})
  (stop!))
