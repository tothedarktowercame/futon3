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

;; --- Clock-in/out protocol (fulab) ---

(defn- clock-in-event [session-id {:keys [pattern-id intent context]}]
  (make-event session-id :clock-in/start
              {:clock-in/pattern-id pattern-id
               :clock-in/session-id session-id
               :clock-in/intent intent
               :clock-in/timestamp (java.util.Date.)
               :clock-in/context (or context {})}))

(defn- pattern-used-event
  ([session-id pattern-id reason]
   (pattern-used-event session-id pattern-id reason nil nil))
  ([session-id pattern-id reason parent-id channels]
   (make-event session-id :pattern/used
               (cond-> {:pattern/id pattern-id
                        :pattern/dep pattern-id
                        :pattern/reason reason}
                 parent-id (assoc :pattern/parent parent-id)
                 (seq channels) (assoc :pattern/channels channels)))))

(defn- doc-written-event [session-id doc]
  (make-event session-id :doc/written
              {:doc/path (:path doc)
               :doc/inline (:inline doc)
               :doc/type (:type doc)
               :doc/topic (:topic doc)
               :doc/anchors (:anchors doc)
               :doc/pattern-refs (:pattern-refs doc)
               :doc/summary (:summary doc)
               :doc/visibility (or (:visibility doc) :public)}))

(defn- clock-out-event [session-id {:keys [pattern-id patterns-trail status artifacts docs stats society-paper]}]
  (make-event session-id :clock-out/complete
              {:pattern/primary pattern-id
               :clock-out/session-id session-id
               :pattern/trail (or patterns-trail [])
               :session/status status
               :artifacts (or artifacts [])
               :docs-written (or docs [])
               :stats (or stats {})
               :society-paper society-paper
               :clock-out/timestamp (java.util.Date.)}))

;; --- Subprocess management ---

(defn- build-claude-command [{:keys [prompt dangerously-skip-permissions?]}]
  (let [base (cond-> ["claude" "--print" "--output-format" "stream-json" "--verbose"]
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

(defn- parse-tool-params [value]
  (if (string? value)
    (try
      (json/parse-string value true)
      (catch Exception _ value))
    value))

(defn- tool-metadata-from-pending [pending]
  (let [tool-name (or (:name pending)
                      (:tool_name pending)
                      (:tool-name pending)
                      (:tool/name pending))
        raw-params (or (:input pending)
                       (:arguments pending)
                       (:params pending)
                       (:tool-params pending)
                       (:tool_params pending))
        params (parse-tool-params raw-params)]
    (cond-> {}
      tool-name (assoc :tool/name tool-name)
      (some? params) (assoc :tool/params params))))

;; --- Session lifecycle ---

(defn- initial-stats []
  {:events 0
   :tool-calls 0
   :tool-approvals 0
   :tool-denials 0
   :assistant-messages 0
   :patterns-used 0
   :artifacts 0
   :docs-written 0
   :cost-usd 0.0
   :duration-ms 0
   :first-event-at nil
   :last-event-at nil})

(defn- create-session! [opts]
  (let [session-id (random-session-id)
        events-ch (async/chan 256)
        session {:id session-id
                 :status :starting
                 :opts opts
                 :events-ch events-ch
                 :events-log (atom [])  ;; For polling-based retrieval
                 :subscribers (atom #{})
                 :pending-approvals (atom {})
                 :patterns-used (atom [])  ;; fulab: pattern dependencies
                 :artifacts (atom [])       ;; fulab: files/artifacts produced
                 :docs-written (atom [])    ;; fulab: documentation trail
                 :society-paper (atom nil)  ;; fulab: session retrospective
                 :stats (atom (initial-stats))  ;; fulab: realtime stats
                 :process (atom nil)
                 :started-at (now-ms)}]
    (swap! !sessions assoc session-id session)
    session))

(defn- update-stats!
  "Update session stats based on event type."
  [session event]
  (let [now (now-ms)
        event-type (:event/type event)]
    (swap! (:stats session)
           (fn [s]
             (-> s
                 (update :events inc)
                 (assoc :last-event-at now)
                 (cond->
                   (nil? (:first-event-at s))
                   (assoc :first-event-at now)

                   (= event-type :tool/call)
                   (update :tool-calls inc)

                   (= event-type :tool/approved)
                   (update :tool-approvals inc)

                   (= event-type :tool/denied)
                   (update :tool-denials inc)

                   (= event-type :assistant/message)
                   (update :assistant-messages inc)

                   (= event-type :pattern/used)
                   (update :patterns-used inc)

                   (= event-type :doc/written)
                   (update :docs-written inc)

                   (= event-type :session/result)
                   (-> (assoc :cost-usd (or (:result/cost-usd event) 0))
                       (assoc :duration-ms (or (:result/duration-ms event) 0)))))))))

(defn- emit-event! [session event]
  (let [subscribers @(:subscribers session)]
    ;; Update realtime stats
    (update-stats! session event)
    ;; Store in events log for polling
    (swap! (:events-log session) conj event)
    ;; Put on events channel for persistence
    (async/put! (:events-ch session) event)
    ;; Notify all subscribers
    (doseq [sub-ch subscribers]
      (async/offer! sub-ch event))))

(defn- extract-assistant-text
  "Extract text content from stream-json assistant message format.
   Format: {:message {:content [{:type \"text\" :text \"...\"}]}}"
  [parsed]
  (let [content (or (get-in parsed [:message :content])
                    (:content parsed))]
    (if (sequential? content)
      (->> content
           (filter #(= (:type %) "text"))
           (map :text)
           (str/join ""))
      content)))

(defn- process-output-line! [session line]
  (when-not (str/blank? line)
    (if-let [parsed (read-json-line line)]
      ;; Structured JSON output from --output-format stream-json
      (let [msg-type (:type parsed)]
        (case msg-type
          "system"
          (emit-event! session (make-event (:id session) :system/init
                                           {:system/tools (:tools parsed)
                                            :system/model (:model parsed)}))

          "assistant"
          (when-let [text (extract-assistant-text parsed)]
            (emit-event! session (assistant-message-event (:id session) text)))

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

          "result"
          (emit-event! session (make-event (:id session) :session/result
                                           {:result/text (:result parsed)
                                            :result/cost-usd (:total_cost_usd parsed)
                                            :result/duration-ms (:duration_ms parsed)}))

          "error"
          (emit-event! session (error-event (:id session) (:message parsed)))

          ;; Default: emit as raw for debugging
          (emit-event! session (make-event (:id session) :raw {:line line :parsed parsed}))))
      ;; Plain text output (e.g., script noise)
      (when-not (str/starts-with? line "[?")  ;; Filter ANSI control sequences
        (emit-event! session (assistant-delta-event (:id session) line))))))

(defn- run-session-loop! [session]
  (async/thread
    (try
      (let [process @(:process session)
            reader (io/reader (.getInputStream process))]
        (swap! !sessions assoc-in [(:id session) :status] :running)
        (doseq [line (line-seq reader)]
          (process-output-line! session line))
        ;; Process finished
        (let [exit-code (.waitFor process)
              status (if (zero? exit-code) :success :failed)
              pattern-id (get-in session [:opts :pattern-id])]
          (emit-event! session (session-finished-event (:id session) status))
          ;; fulab: emit clock-out if we clocked in
          (when pattern-id
            (emit-event! session (clock-out-event (:id session)
                                                  {:pattern-id pattern-id
                                                   :patterns-trail @(:patterns-used session)
                                                   :status status
                                                   :artifacts @(:artifacts session)
                                                   :docs @(:docs-written session)
                                                   :stats @(:stats session)
                                                   :society-paper @(:society-paper session)})))
          (swap! !sessions assoc-in [(:id session) :status] :finished)))
      (catch Exception e
        (emit-event! session (error-event (:id session) e))
        ;; fulab: emit clock-out on error too
        (when-let [pattern-id (get-in session [:opts :pattern-id])]
          (emit-event! session (clock-out-event (:id session)
                                                {:pattern-id pattern-id
                                                 :patterns-trail @(:patterns-used session)
                                                 :status :error
                                                 :artifacts @(:artifacts session)
                                                 :docs @(:docs-written session)
                                                 :stats @(:stats session)
                                                 :society-paper @(:society-paper session)})))
        (swap! !sessions assoc-in [(:id session) :status] :error)))))

;; --- Public API ---

(defn start-session!
  "Start a new Claude session with the given prompt.
   Accepts optional :pattern-id and :intent for fulab clock-in.
   Returns {:ok true :session-id ...} or {:ok false :error ...}"
  [{:keys [prompt cwd model pattern-id intent] :as opts}]
  (try
    (let [session (create-session! opts)
          process (start-process! opts)]
      (reset! (:process session) process)
      (emit-event! session (session-started-event (:id session) opts))
      ;; fulab: emit clock-in if pattern-id provided
      (when pattern-id
        (emit-event! session (clock-in-event (:id session)
                                             {:pattern-id pattern-id
                                              :intent intent
                                              :context (select-keys opts [:cwd :model])})))
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
        (let [pending (get @(:pending-approvals session) call-id)
              tool-meta (tool-metadata-from-pending pending)]
          (swap! (:pending-approvals session) dissoc call-id)
          (emit-event! session (make-event session-id :tool/approved
                                           (merge {:tool/call-id call-id}
                                                  tool-meta))))
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

;; --- fulab: Pattern tracking API ---

(defn record-pattern-use!
  "Record that a session used a pattern as a dependency.
   Emits :pattern/used event and adds to session's pattern trail."
  [session-id pattern-id & [{:keys [reason parent-id channels]}]]
  (if-let [session (get @!sessions session-id)]
    (let [usage (cond-> {:pattern/id pattern-id
                         :used-at (now-ms)
                         :reason reason}
                  parent-id (assoc :pattern/parent parent-id)
                  (seq channels) (assoc :pattern/channels channels))]
      (swap! (:patterns-used session) conj usage)
      (emit-event! session (pattern-used-event session-id pattern-id reason parent-id channels))
      {:ok true :session-id session-id :pattern-id pattern-id})
    {:ok false :error "session-not-found"}))

(defn record-artifact!
  "Record that a session produced or modified an artifact (file, resource, etc.)."
  [session-id artifact-path & [{:keys [action]}]]
  (if-let [session (get @!sessions session-id)]
    (let [artifact {:path artifact-path
                    :action (or action :modified)
                    :at (now-ms)}]
      (swap! (:artifacts session) conj artifact)
      (swap! (:stats session) update :artifacts inc)
      {:ok true :session-id session-id :artifact artifact-path})
    {:ok false :error "session-not-found"}))

(defn record-doc!
  "Record documentation written during a session.
   Doc types: :explanation, :rationale, :howto, :decision, :trail
   Anchors link to code (fn names, file:line). Pattern-refs link to patterns."
  [session-id doc]
  (if-let [session (get @!sessions session-id)]
    (let [doc-record (merge {:at (now-ms)} doc)]
      (swap! (:docs-written session) conj doc-record)
      (emit-event! session (doc-written-event session-id doc-record))
      {:ok true :session-id session-id :doc doc-record})
    {:ok false :error "session-not-found"}))

(defn set-society-paper!
  "Set the session's retrospective summary (society paper).
   This is typically written at session end to summarize what was accomplished,
   decisions made, and context for future agents."
  [session-id summary]
  (if-let [session (get @!sessions session-id)]
    (do
      (reset! (:society-paper session) summary)
      {:ok true :session-id session-id})
    {:ok false :error "session-not-found"}))

(defn get-session
  "Get session info including realtime stats."
  [session-id]
  (when-let [session (get @!sessions session-id)]
    {:id (:id session)
     :status (:status session)
     :started-at (:started-at session)
     :opts (dissoc (:opts session) :prompt) ;; Don't leak full prompt
     :pending-approvals (keys @(:pending-approvals session))
     ;; fulab: pattern tracking
     :pattern-id (get-in session [:opts :pattern-id])
     :patterns-used @(:patterns-used session)
     :artifacts @(:artifacts session)
     ;; fulab: documentation trail
     :docs-written @(:docs-written session)
     :society-paper @(:society-paper session)
     ;; fulab: realtime stats
     :stats @(:stats session)}))

(defn get-events
  "Get events for a session, optionally starting from an offset."
  [session-id & [{:keys [offset] :or {offset 0}}]]
  (when-let [session (get @!sessions session-id)]
    (let [events @(:events-log session)]
      {:session-id session-id
       :offset offset
       :count (count events)
       :events (vec (drop offset events))})))

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
