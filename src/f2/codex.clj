(ns f2.codex
  "Codex subprocess management with real-time event streaming.

   Provides an RPC interface to Codex CLI:
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
(defonce ^:private !cli-session-cwds (atom {}))

(defn- random-session-id []
  (str "codex-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- session-env
  "Build environment variables for the Codex subprocess."
  [session]
  (let [cwd (get-in session [:opts :cwd])
        scripts-dir (when cwd (.getAbsolutePath (io/file cwd "scripts")))
        base-path (or (System/getenv "PATH") "")
        server-url (or (get-in session [:opts :server-url])
                       (System/getenv "FUTON3_CODEX_SERVER_URL")
                       "http://localhost:5050")
        env {"FUTON3_CODEX_SESSION_ID" (:id session)
             "FUTON3_CODEX_SERVER_URL" server-url}]
    (if scripts-dir
      (assoc env "PATH" (str scripts-dir ":" base-path))
      env)))

(defn- record-cli-session-cwd! [cli-session-id session]
  (when-let [cwd (get-in session [:opts :cwd])]
    (swap! !cli-session-cwds
           assoc
           (str cli-session-id)
           {:cwd cwd
            :session-id (:id session)
            :updated-at (now-ms)})))

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
              (cond-> {:session/cwd (:cwd opts)
                       :session/model (:model opts "codex")
                       :session/prompt (:prompt opts)}
                (:ancestor-id opts) (assoc :session/ancestor-id (:ancestor-id opts))
                (:codex-cli-session-id opts) (assoc :session/fucodex-cli-id (:codex-cli-session-id opts))
                (:resume/at opts) (assoc :resume/at (:resume/at opts)
                                         :resume/prompt (:prompt opts))
                (:resume/count opts) (assoc :resume/count (:resume/count opts)))))

(defn- tool-call-event
  ([session-id tool-name args call-id]
   (tool-call-event session-id tool-name args call-id :pending))
  ([session-id tool-name args call-id status]
   (make-event session-id :tool/call
               {:tool/name tool-name
                :tool/args args
                :tool/call-id call-id
                :tool/status status})))

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

(declare emit-event! pattern-action-event)

(def ^:private pattern-action-re
  (re-pattern
   (str "^"
        (java.util.regex.Pattern/quote "[PATTERN-ACTION]")
        "\\s+(\\S+)\\s+(\\S+)(?:\\s+-\\s+(.+))?$")))

(defn- emit-pattern-actions-from-text! [session text]
  (doseq [line (str/split-lines (or text ""))]
    (when-let [[_ action pattern-id note] (re-matches pattern-action-re (str/trim line))]
      (emit-event! session (pattern-action-event (:id session) pattern-id action note)))))

(defn- emit-assistant-message! [session text]
  (when (and session (some? text))
    (emit-event! session (assistant-message-event (:id session) text))
    (emit-pattern-actions-from-text! session text)))

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

(defn- pattern-selection-event [session-id psr]
  (make-event session-id :pattern/selection-claimed
              {:psr psr}))

(defn- pattern-use-claimed-event [session-id pur]
  (make-event session-id :pattern/use-claimed
              {:pur pur}))

(defn- pattern-action-event [session-id pattern-id action note]
  (make-event session-id :pattern/action
              (cond-> {:pattern/id pattern-id
                       :pattern/action action}
                note (assoc :pattern/note note))))

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

(defn- shell-quote
  "Return S as a single-quoted shell literal.
   This prevents backticks/$-expansion inside prompts."
  [s]
  (let [s (str s)
        escaped (str/replace s "'" "'\\''")]
    (if (str/blank? s)
      "''"
      (str "'" escaped "'"))))

(defn- base64-encode [value]
  (when-not (nil? value)
    (.encodeToString (java.util.Base64/getEncoder)
                     (.getBytes (str value) "UTF-8"))))

(defn- python-codex-command
  "Build a python wrapper that invokes codex with a base64 prompt."
  [{:keys [prompt dangerously-skip-permissions? resume-id approval-policy]}]
  (let [prompt-b64 (base64-encode prompt)
        has-prompt (if prompt-b64 "1" "0")
        args-base (cond-> "[\"codex\",\"exec\",\"--json\",\"--skip-git-repo-check\""
                    dangerously-skip-permissions?
                    (str ",\"--dangerously-bypass-approvals-and-sandbox\"")
                    approval-policy
                    (str ",\"-c\",\"approval_policy=\\\"" approval-policy "\\\"\"")
                    resume-id
                    (str ",\"resume\",\"" resume-id "\""))
        args-base (str args-base "]")
        code (str "import base64,subprocess,sys;"
                  "prompt=base64.b64decode(\"" (or prompt-b64 "") "\").decode()"
                  " if \"" has-prompt "\" == \"1\" else None;"
                  "args=" args-base ";"
                  "args.append(prompt) if prompt else None;"
                  "sys.exit(subprocess.call(args))")]
    (str "python3 -c " (shell-quote code))))

(defn- build-codex-command
  [{:keys [prompt dangerously-skip-permissions? approval-policy]}]
  (python-codex-command {:prompt prompt
                         :dangerously-skip-permissions? dangerously-skip-permissions?
                         :approval-policy approval-policy}))

(defn- build-resume-command
  [{:keys [codex-cli-session-id prompt dangerously-skip-permissions? approval-policy]}]
  (python-codex-command {:prompt prompt
                         :resume-id codex-cli-session-id
                         :dangerously-skip-permissions? dangerously-skip-permissions?
                         :approval-policy approval-policy}))

(defn- build-pty-command
  "Wrap codex command in `script` to get a real PTY.
   This allows us to inject keystrokes for interactive approval."
  [command]
  ;; script -q /dev/null -c "command" gives us a PTY
  ;; -q = quiet (no 'Script started' message)
  (let [script? (some (fn [dir]
                        (let [candidate (io/file dir "script")]
                          (and (.exists candidate) (.canExecute candidate))))
                      (str/split (or (System/getenv "PATH") "") #":"))]
    (if script?
      ["script" "-q" "/dev/null" "-c" command]
      ;; Fallback when script isn't available; no PTY/interactive approvals.
      ["/bin/bash" "-lc" command])))

(defn- start-process! [{:keys [cwd env] :as opts} command]
  (let [cmd (build-pty-command command)
        pb (doto (ProcessBuilder. ^java.util.List cmd)
             (.directory (when cwd (io/file cwd)))
             (.redirectErrorStream true))]
    (when (seq env)
      (let [pb-env (.environment pb)]
        (doseq [[k v] env]
          (.put pb-env (str k) (str v)))))
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

(defn- parse-output-json [value]
  (when (string? value)
    (try
      (json/parse-string value true)
      (catch Exception _ nil))))

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
        params (cond
                 (string? raw-params) (or (parse-output-json raw-params) raw-params)
                 :else raw-params)]
    (cond-> {}
      tool-name (assoc :tool/name tool-name)
      (some? params) (assoc :tool/params params))))

(defn- parse-exit-code [value]
  (when (string? value)
    (when-let [[_ code] (re-find #"Exit code:\\s+(\\d+)" value)]
      (Long/parseLong code))))

(defn- coerce-output-text [value]
  (cond
    (string? value) value
    (map? value) (or (coerce-output-text (:aggregated_output value))
                     (coerce-output-text (:aggregated-output value))
                     (coerce-output-text (:text value))
                     (coerce-output-text (:output value))
                     (coerce-output-text (:stdout value))
                     (pr-str value))
    (sequential? value) (->> value
                             (map coerce-output-text)
                             (remove nil?)
                             (str/join ""))
    (nil? value) nil
    :else (pr-str value)))

;; --- Session lifecycle ---

(defn- initial-stats []
  {:events 0
   :tool-calls 0
   :tool-approvals 0
   :tool-denials 0
   :assistant-messages 0
   :patterns-used 0
   :pattern-selections 0
   :pattern-uses 0
   :pattern-actions 0
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
                 :codex-cli-session-id nil
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

                   (= event-type :pattern/selection-claimed)
                   (update :pattern-selections inc)

                   (= event-type :pattern/use-claimed)
                   (update :pattern-uses inc)

                   (= event-type :pattern/action)
                   (update :pattern-actions inc)

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
  (let [payload (:payload parsed)
        content (or (get-in parsed [:message :content])
                    (:content parsed)
                    (:delta parsed)
                    (:text parsed)
                    (get-in payload [:message :content])
                    (:content payload)
                    (:message payload))]
    (cond
      (sequential? content)
      (->> content
           (filter (fn [item]
                     (contains? #{"text" "output_text"} (:type item))))
           (map :text)
           (str/join ""))
      (string? content) content
      :else nil)))

(defn- process-output-line! [session line]
  (when-not (str/blank? line)
    (if-let [parsed (read-json-line line)]
      ;; Structured JSON output from --output-format stream-json
      (let [msg-type (:type parsed)
            payload (:payload parsed)
            cli-session-id (or (:session_id parsed)
                               (:session-id parsed)
                               (:sessionId parsed)
                               (:session parsed)
                               (:session_id payload)
                               (:session-id payload)
                               (:sessionId payload)
                               (:session payload))]
        (when cli-session-id
          (swap! !sessions assoc-in [(:id session) :codex-cli-session-id] cli-session-id)
          (record-cli-session-cwd! cli-session-id session))
        (cond
          (and (nil? msg-type) (:text parsed))
          (emit-assistant-message! session (:text parsed))

          (= msg-type "thread.started")
          (let [thread-id (:thread_id parsed)]
            (when thread-id
              (swap! !sessions assoc-in [(:id session) :codex-cli-session-id] thread-id)))

          (contains? #{"item.started" "item.completed" "item.failed"} msg-type)
          (let [item (or (:item parsed) (:item payload))
                item-type (:type item)
                item-id (or (:id item)
                            (:item_id item)
                            (:item-id item)
                            (:id parsed)
                            (str (UUID/randomUUID)))]
            (cond
              (and (= item-type "agent_message")
                   (= msg-type "item.completed"))
              (when-let [text (:text item)]
                (emit-assistant-message! session text))

              (and (= item-type "command_execution")
                   (= msg-type "item.started"))
              (let [cmd (or (:command item)
                            (:cmd item)
                            (:command_line item)
                            (:command-line item))]
                (emit-event! session (tool-call-event (:id session)
                                                      "command_execution"
                                                      (cond-> {} cmd (assoc :cmd cmd))
                                                      item-id
                                                      :observed))
                (swap! (:pending-approvals session)
                       assoc
                       item-id
                       {:name "command_execution"
                        :input (cond-> {} cmd (assoc :cmd cmd))}))

              (and (= item-type "command_execution")
                   (contains? #{"item.completed" "item.failed"} msg-type))
              (let [stdout (coerce-output-text (or (:aggregated_output item)
                                                   (:aggregated-output item)
                                                   (:output item)
                                                   (:stdout item)))
                    stderr (coerce-output-text (:stderr item))
                    exit (or (:exit_code item)
                             (:exitCode item)
                             (:exit-code item)
                             (when (= msg-type "item.failed") 1)
                             0)
                    duration-ms (or (:duration_ms item) (:duration-ms item))]
                (emit-event! session (tool-result-event (:id session)
                                                        item-id
                                                        {:stdout stdout
                                                         :stderr stderr
                                                         :exit exit
                                                         :duration-ms duration-ms})))
              :else nil))

          (and (= msg-type "event_msg")
               (= "agent_message" (:type payload)))
          (when-let [text (or (:message payload) (:text payload))]
            (emit-assistant-message! session text))

          (and (= msg-type "response_item")
               (= "message" (:type payload))
               (= "assistant" (:role payload)))
          (when-let [text (extract-assistant-text payload)]
            (emit-assistant-message! session text))

          (and (= msg-type "response_item")
               (= "custom_tool_call_output" (:type payload)))
          (let [call-id (:call_id payload)
                output-json (parse-output-json (:output payload))
                metadata (:metadata output-json)
                duration-sec (or (:duration_seconds metadata) 0)
                stdout (or (:output output-json) (:output payload))]
            (emit-event! session (tool-result-event (:id session)
                                                    call-id
                                                    {:stdout stdout
                                                     :exit (or (:exit_code metadata) 0)
                                                     :duration-ms (long (* 1000 duration-sec))})))

          (and (= msg-type "response_item")
               (= "custom_tool_call" (:type payload)))
          (let [call-id (:call_id payload)
                tool-name (:name payload)
                args (:input payload)]
            (emit-event! session (tool-call-event (:id session)
                                                  tool-name
                                                  args
                                                  call-id))
            (swap! (:pending-approvals session) assoc call-id payload))

          (and (= msg-type "response_item")
               (= "function_call" (:type payload)))
          (let [call-id (:call_id payload)
                tool-name (:name payload)
                args (or (:arguments payload) (:input payload))]
            (emit-event! session (tool-call-event (:id session)
                                                  tool-name
                                                  args
                                                  call-id))
            (swap! (:pending-approvals session) assoc call-id payload))

          (and (= msg-type "response_item")
               (= "function_call_output" (:type payload)))
          (let [call-id (:call_id payload)
                stdout (:output payload)
                exit (or (parse-exit-code stdout) 0)]
            (emit-event! session (tool-result-event (:id session)
                                                    call-id
                                                    {:stdout stdout
                                                     :exit exit})))

          (and (= msg-type "event_msg")
               (= "token_count" (:type payload)))
          nil

          (= msg-type "session_meta")
          (let [session-id (or (:id payload) (:session-id payload) (:session_id payload))]
            (when session-id
              (swap! !sessions assoc-in [(:id session) :codex-cli-session-id] session-id)))

          (= msg-type "system")
          (emit-event! session (make-event (:id session) :system/init
                                           {:system/tools (:tools parsed)
                                            :system/model (:model parsed)}))

          (= msg-type "assistant")
          (when-let [text (extract-assistant-text parsed)]
            (emit-assistant-message! session text))

          (contains? #{"tool_use" "tool_call"} msg-type)
          (let [call-id (or (:id parsed)
                            (:tool_use_id parsed)
                            (:tool_call_id parsed)
                            (str (UUID/randomUUID)))]
            (emit-event! session (tool-call-event (:id session)
                                                  (:name parsed)
                                                  (:input parsed)
                                                  call-id))
            ;; Store for approval flow
            (swap! (:pending-approvals session) assoc call-id parsed))

          (contains? #{"tool_result" "tool_output"} msg-type)
          (emit-event! session (tool-result-event (:id session)
                                                  (or (:tool_use_id parsed)
                                                      (:tool_call_id parsed)
                                                      (:id parsed))
                                                  {:stdout (:content parsed)
                                                   :exit 0}))

          (= msg-type "result")
          (let [event-data {:result/text (:result parsed)
                            :result/cost-usd (:total_cost_usd parsed)
                            :result/duration-ms (:duration_ms parsed)
                            :result/codex-session-id cli-session-id}]
            (emit-event! session (make-event (:id session) :session/result
                                             (cond-> event-data
                                               (nil? cli-session-id) (dissoc :result/codex-session-id)))))

          (= msg-type "error")
          (emit-event! session (error-event (:id session) (:message parsed)))

          :else
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
  "Start a new Codex session with the given prompt.
   Accepts optional :pattern-id and :intent for fulab clock-in.
   Returns {:ok true :session-id ...} or {:ok false :error ...}"
  [{:keys [prompt cwd model pattern-id intent] :as opts}]
  (try
    (let [session (create-session! opts)
          process (start-process! (assoc opts :env (session-env session))
                                  (build-codex-command opts))]
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

(defn record-pattern-selection!
  "Record a Pattern Selection Record (PSR) for a session."
  [session-id psr]
  (if-let [session (get @!sessions session-id)]
    (do
      (emit-event! session (pattern-selection-event session-id psr))
      {:ok true :session-id session-id})
    {:ok false :error "session-not-found"}))

(defn record-pattern-use-claimed!
  "Record a Pattern Use Record (PUR) for a session."
  [session-id pur]
  (if-let [session (get @!sessions session-id)]
    (do
      (emit-event! session (pattern-use-claimed-event session-id pur))
      {:ok true :session-id session-id})
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

(defn record-pattern-action!
  "Record a lightweight pattern action (read/implement/update)."
  [session-id pattern-id action & [{:keys [note]}]]
  (if-let [session (get @!sessions session-id)]
    (do
      (emit-event! session (pattern-action-event session-id pattern-id action note))
      {:ok true :session-id session-id :pattern-id pattern-id :action action})
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
     :codex-cli-session-id (:codex-cli-session-id session)
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
(defn resume-session!
  "Resume an existing Codex session with a follow-up prompt.
   Returns {:ok true :session-id ...} or {:ok false :error ...}"
  [{:keys [codex-cli-session-id cwd] :as opts}]
  (try
    (if (str/blank? (str codex-cli-session-id))
      {:ok false :error "missing-codex-cli-session-id"}
      (let [stored (get @!cli-session-cwds (str codex-cli-session-id))
            stored-cwd (:cwd stored)
            ancestor-id (:session-id stored)
            final-cwd (cond
                        (and stored-cwd cwd (not= stored-cwd cwd)) stored-cwd
                        stored-cwd stored-cwd
                        :else cwd)
            warning (when (and stored-cwd cwd (not= stored-cwd cwd))
                      {:warning "resume-cwd-mismatch"
                       :expected-cwd stored-cwd
                       :requested-cwd cwd})
            opts* (cond-> (assoc opts
                                 :cwd final-cwd
                                 :resumed? true
                                 :resume/at (java.util.Date.)
                                 :resume/count (inc (or (:resume/count opts) 0)))
                    ancestor-id (assoc :ancestor-id ancestor-id))
            session (create-session! opts*)
            process (start-process! (assoc opts* :env (session-env session))
                                    (build-resume-command opts*))]
        (reset! (:process session) process)
        (record-cli-session-cwd! codex-cli-session-id session)
        (emit-event! session (session-started-event (:id session) opts*))
        (run-session-loop! session)
        (cond-> {:ok true
                 :session-id (:id session)
                 :started-at (:started-at session)}
          warning (merge warning))))
    (catch Exception e
      {:ok false :error (.getMessage e)})))
