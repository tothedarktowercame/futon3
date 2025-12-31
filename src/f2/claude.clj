(ns f2.claude
  "Claude subprocess management with real-time event streaming.

   Provides an RPC interface to Claude CLI:
   - Start sessions with prompts
   - Stream events (tool calls, responses, deltas)
   - Approve/deny tool calls
   - Cancel running sessions

   Events are persisted to XTDB via the lab capture pipeline."
  (:require [cheshire.core :as json]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [f0.clock :as clock])
  (:import (java.lang ProcessBuilder)
           (java.util UUID)))

;; --- Session state ---

(defonce ^:private !sessions (atom {}))

(defn- random-session-id []
  (str "claude-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn- now-ms []
  (System/currentTimeMillis))

;; --- Event schema ---
;; These mirror the lab capture format so they can be ingested directly

(defn- make-event [session-id type data]
  (merge {:event/id (str (UUID/randomUUID))
          :event/session session-id
          :event/type type
          :event/ts (now-ms)}
         data))

(defn- session-started-event [session-id opts]
  (make-event session-id :session/started
              {:session/cwd (:cwd opts)
               :session/model (:model opts "claude")
               :session/prompt (:prompt opts)}))

(defn- tool-call-event [session-id tool-name args call-id]
  (make-event session-id :tool/call
              {:tool/name tool-name
               :tool/args args
               :tool/call-id call-id
               :tool/status :pending}))

(defn- tool-result-event [session-id call-id result]
  (make-event session-id :tool/result
              {:tool/call-id call-id
               :tool/exit (:exit result 0)
               :tool/stdout (:stdout result)
               :tool/stderr (:stderr result)
               :tool/duration-ms (:duration-ms result)}))

(defn- assistant-delta-event [session-id text]
  (make-event session-id :assistant/delta
              {:assistant/text text}))

(defn- assistant-message-event [session-id text]
  (make-event session-id :assistant/message
              {:assistant/text text}))

(defn- session-finished-event [session-id status]
  (make-event session-id :session/finished
              {:session/status status
               :session/finished-at (now-ms)}))

(defn- error-event [session-id error]
  (make-event session-id :session/error
              {:error/message (str error)
               :error/type (type error)}))

;; --- Subprocess management ---

(defn- build-claude-command [{:keys [prompt dangerously-skip-permissions?]}]
  (let [base (cond-> ["claude" "--print" "--output-format" "json"]
               dangerously-skip-permissions? (conj "--dangerously-skip-permissions")
               prompt (conj "-p" prompt))]
    ;; Join into a single string for script -c
    (str/join " " (map #(if (str/includes? % " ")
                          (str "\"" (str/replace % "\"" "\\\"") "\"")
                          %)
                       base))))

(defn- build-pty-command
  "Wrap claude command in `script` to get a real PTY.
   This allows us to inject keystrokes for interactive approval."
  [opts]
  (let [claude-cmd (build-claude-command opts)]
    ;; script -q /dev/null -c "command" gives us a PTY
    ;; -q = quiet (no 'Script started' message)
    ["script" "-q" "/dev/null" "-c" claude-cmd]))

(defn- start-process! [{:keys [cwd] :as opts}]
  (let [cmd (build-pty-command opts)
        pb (doto (ProcessBuilder. ^java.util.List cmd)
             (.directory (when cwd (io/file cwd)))
             (.redirectErrorStream true))]
    (.start pb)))

(defn- write-to-process!
  "Write a string to the process's stdin (for PTY injection)."
  [^Process process ^String input]
  (let [out (.getOutputStream process)]
    (.write out (.getBytes input))
    (.flush out)))

(defn- read-json-line [line]
  (try
    (json/parse-string line true)
    (catch Exception _
      nil)))

;; --- Session lifecycle ---

(defn- create-session! [opts]
  (let [session-id (random-session-id)
        events-ch (async/chan 256)
        session {:id session-id
                 :status :starting
                 :opts opts
                 :events-ch events-ch
                 :subscribers (atom #{})
                 :pending-approvals (atom {})
                 :process (atom nil)
                 :started-at (now-ms)}]
    (swap! !sessions assoc session-id session)
    session))

(defn- emit-event! [session event]
  (let [subscribers @(:subscribers session)]
    ;; Put on events channel for persistence
    (async/put! (:events-ch session) event)
    ;; Notify all subscribers
    (doseq [sub-ch subscribers]
      (async/offer! sub-ch event))))

(defn- process-output-line! [session line]
  (when-not (str/blank? line)
    (if-let [parsed (read-json-line line)]
      ;; Structured JSON output from --output-format json
      (let [msg-type (:type parsed)]
        (case msg-type
          "assistant"
          (emit-event! session (assistant-delta-event (:id session) (:content parsed)))

          "tool_use"
          (let [call-id (or (:id parsed) (str (UUID/randomUUID)))]
            (emit-event! session (tool-call-event (:id session)
                                                   (:name parsed)
                                                   (:input parsed)
                                                   call-id))
            ;; Store for approval flow
            (swap! (:pending-approvals session) assoc call-id parsed))

          "tool_result"
          (emit-event! session (tool-result-event (:id session)
                                                   (:tool_use_id parsed)
                                                   {:stdout (:content parsed)
                                                    :exit 0}))

          "error"
          (emit-event! session (error-event (:id session) (:message parsed)))

          ;; Default: emit as raw
          (emit-event! session (make-event (:id session) :raw {:line line :parsed parsed}))))
      ;; Plain text output
      (emit-event! session (assistant-delta-event (:id session) line)))))

(defn- run-session-loop! [session]
  (async/thread
    (try
      (let [process @(:process session)
            reader (io/reader (.getInputStream process))]
        (swap! !sessions assoc-in [(:id session) :status] :running)
        (doseq [line (line-seq reader)]
          (process-output-line! session line))
        ;; Process finished
        (let [exit-code (.waitFor process)]
          (emit-event! session (session-finished-event (:id session)
                                                        (if (zero? exit-code) :success :failed)))
          (swap! !sessions assoc-in [(:id session) :status] :finished)))
      (catch Exception e
        (emit-event! session (error-event (:id session) e))
        (swap! !sessions assoc-in [(:id session) :status] :error)))))

;; --- Public API ---

(defn start-session!
  "Start a new Claude session with the given prompt.
   Returns {:ok true :session-id ...} or {:ok false :error ...}"
  [{:keys [prompt cwd model] :as opts}]
  (try
    (let [session (create-session! opts)
          process (start-process! opts)]
      (reset! (:process session) process)
      (emit-event! session (session-started-event (:id session) opts))
      (run-session-loop! session)
      {:ok true
       :session-id (:id session)
       :started-at (:started-at session)})
    (catch Exception e
      {:ok false
       :error (.getMessage e)})))

(defn subscribe!
  "Subscribe to events from a session. Returns a channel that receives events."
  [session-id]
  (when-let [session (get @!sessions session-id)]
    (let [ch (async/chan 64)]
      (swap! (:subscribers session) conj ch)
      ch)))

(defn unsubscribe!
  "Unsubscribe from session events."
  [session-id ch]
  (when-let [session (get @!sessions session-id)]
    (swap! (:subscribers session) disj ch)
    (async/close! ch)))

(defn cancel-session!
  "Cancel a running session."
  [session-id]
  (when-let [session (get @!sessions session-id)]
    (when-let [process @(:process session)]
      (.destroyForcibly process)
      (emit-event! session (session-finished-event session-id :cancelled))
      (swap! !sessions assoc-in [session-id :status] :cancelled)
      {:ok true :session-id session-id})))

(defn approve-tool-call!
  "Approve a pending tool call by injecting 'y' into the PTY."
  [session-id call-id]
  (if-let [session (get @!sessions session-id)]
    (if-let [process @(:process session)]
      (try
        (write-to-process! process "y\n")
        (swap! (:pending-approvals session) dissoc call-id)
        (emit-event! session (make-event session-id :tool/approved
                                         {:tool/call-id call-id}))
        {:ok true :session-id session-id :call-id call-id :action :approved}
      (catch Exception e
        {:ok false :error (.getMessage e)}))
      {:ok false :error "no-process"})
    {:ok false :error "session-not-found"}))

(defn deny-tool-call!
  "Deny a pending tool call by injecting 'n' into the PTY."
  [session-id call-id reason]
  (if-let [session (get @!sessions session-id)]
    (if-let [process @(:process session)]
      (try
        (write-to-process! process "n\n")
        (swap! (:pending-approvals session) dissoc call-id)
        (emit-event! session (make-event session-id :tool/denied
                                         {:tool/call-id call-id
                                          :tool/deny-reason reason}))
        {:ok true :session-id session-id :call-id call-id :action :denied}
      (catch Exception e
        {:ok false :error (.getMessage e)}))
      {:ok false :error "no-process"})
    {:ok false :error "session-not-found"}))

(defn send-input!
  "Send arbitrary input to the session's PTY (for voice-driven interaction)."
  [session-id input]
  (if-let [session (get @!sessions session-id)]
    (if-let [process @(:process session)]
      (try
        (write-to-process! process input)
        {:ok true :session-id session-id :sent input}
      (catch Exception e
        {:ok false :error (.getMessage e)}))
      {:ok false :error "no-process"})
    {:ok false :error "session-not-found"}))

(defn get-session
  "Get session info."
  [session-id]
  (when-let [session (get @!sessions session-id)]
    {:id (:id session)
     :status (:status session)
     :started-at (:started-at session)
     :opts (dissoc (:opts session) :prompt) ;; Don't leak full prompt
     :pending-approvals (keys @(:pending-approvals session))}))

(defn list-sessions
  "List all sessions."
  []
  (->> @!sessions
       vals
       (map #(get-session (:id %)))
       vec))

;; --- Cleanup ---

(defn cleanup-finished-sessions!
  "Remove finished sessions older than max-age-ms."
  [max-age-ms]
  (let [cutoff (- (now-ms) max-age-ms)]
    (swap! !sessions
           (fn [sessions]
             (->> sessions
                  (remove (fn [[_ s]]
                            (and (#{:finished :cancelled :error} (:status s))
                                 (< (:started-at s) cutoff))))
                  (into {}))))))
