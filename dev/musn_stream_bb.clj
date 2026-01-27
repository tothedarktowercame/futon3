#!/usr/bin/env bb
;; Babashka-compatible MUSN stream processor
;; Lightweight shim that forwards Claude JSON events to MUSN HTTP service
;; Heavy lifting (HUD building, pattern matching) happens in the running JVM

(ns musn-stream-bb
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def musn-url (or (System/getenv "FUTON3_MUSN_URL") "http://localhost:6065"))
(def musn-intent (System/getenv "FUTON3_MUSN_INTENT"))
(def musn-session-id (System/getenv "FUTON3_MUSN_SESSION_ID"))
(def musn-client-id (or (System/getenv "FUTON3_MUSN_CLIENT_ID") "fucodex"))
(def debug-log (or (System/getenv "FUTON3_MUSN_LOG") "/tmp/musn_stream_bb.log"))

(defn log! [msg & more]
  (try
    (spit debug-log
          (str (java.time.Instant/now) " " msg
               (when (seq more) (str " " (pr-str more)))
               "\n")
          :append true)
    (catch Throwable _)))

(defn musn-post! [endpoint body]
  (try
    (let [url (str musn-url endpoint)
          resp (http/post url
                          {:headers {"Content-Type" "application/json"
                                     "X-MUSN-Client" musn-client-id}
                           :body (json/generate-string body)
                           :timeout 5000})]
      (when (= 200 (:status resp))
        (json/parse-string (:body resp) true)))
    (catch Throwable t
      (log! "musn-post error" endpoint (.getMessage t))
      nil)))

(defn build-hud! [intent]
  (when intent
    (let [resp (musn-post! "/musn/hud/build" {:intent intent :pattern-limit 8})]
      (when (:ok resp)
        (:hud resp)))))

(defn create-session! [intent]
  (musn-post! "/musn/session/create"
              {:intent (or intent "unspecified")
               :session/id musn-session-id
               :client/id musn-client-id}))

(defn turn-start! [session-id turn]
  (musn-post! "/musn/turn/start"
              {:session/id session-id
               :turn turn}))

(defn turn-end! [session-id turn]
  (musn-post! "/musn/turn/end"
              {:session/id session-id
               :turn turn}))

(defn turn-plan! [session-id turn plan]
  (musn-post! "/musn/turn/plan"
              {:session/id session-id
               :turn turn
               :plan plan}))

(defn turn-plan-diagram! [session-id turn diagram]
  (musn-post! "/musn/turn/plan"
              {:session/id session-id
               :turn turn
               :plan/diagram diagram}))

(def ^:private plan-diagram-re
  #"(?is)\[plan/diagram\]\s*(.*?)\s*\[/plan/diagram\]")

(defn- parse-plan-diagram [text]
  (when (string? text)
    (when-let [match (re-find plan-diagram-re text)]
      (let [payload (second match)]
        (try
          (edn/read-string payload)
          (catch Throwable t
            (log! "plan-diagram-parse-failed" (.getMessage t))
            nil))))))

(defn scribe-turn! [session-id role content]
  (when (and session-id (seq content))
    (musn-post! "/musn/scribe/turn"
                {:session/id session-id
                 :role (name role)
                 :content content})))

(defn note-native-planning! [session-id tool-name opts]
  "Notify MUSN that native planning tools were detected."
  (when session-id
    (musn-post! "/musn/scribe/native-planning"
                (merge {:session/id session-id
                        :tool tool-name}
                       opts))))

(defn record-native-plan! [session-id tasks]
  "Record a plan derived from native task list."
  (when (and session-id (seq tasks))
    (musn-post! "/musn/scribe/native-plan"
                {:session/id session-id
                 :tasks tasks})))

;; State atom
(def state (atom {:session-id nil
                  :turn 0
                  :started? false
                  :current-content (StringBuilder.)
                  :native-planning-detected false
                  :pending-tasks []}))

(defn extract-type [event]
  (or (:type event)
      (when (map? event)
        (cond
          (:message event) "message"
          (:result event) "result"
          (:content_block_start event) "content_block_start"
          (:content_block_delta event) "content_block_delta"
          (:content_block_stop event) "content_block_stop"
          (:tool_use event) "tool_use"
          (:tool_result event) "tool_result"
          :else nil))))

(def ^:private native-planning-tools
  #{"TaskCreate" "TaskUpdate" "TaskList" "TaskGet"})

(defn- extract-text-content [content]
  "Extract text from content which may be a string or array of content blocks.
   Only extracts text blocks, NOT tool_result blocks (those are noise)."
  (cond
    (string? content) content
    (sequential? content)
    (->> content
         (keep (fn [block]
                 (when (and (map? block) (= "text" (:type block)))
                   (:text block))))
         (str/join "\n"))
    :else nil))

(defn handle-event [event]
  (let [event-type (extract-type event)]
    (case event-type
      ;; User message - record immediately (streaming format)
      "human"
      (when-let [session-id (:session-id @state)]
        (let [content (get-in event [:message :content])]
          (when (string? content)
            (scribe-turn! session-id :user content))))

      ;; JSONL checkpoint format - user message
      "user"
      (when-let [session-id (:session-id @state)]
        (let [raw-content (get-in event [:message :content])
              content (extract-text-content raw-content)]
          (when (and content (not (str/blank? content)))
            (scribe-turn! session-id :user content))))

      ;; JSONL checkpoint format - assistant message (complete)
      "assistant"
      (when-let [session-id (:session-id @state)]
        (let [raw-content (get-in event [:message :content])
              content (extract-text-content raw-content)]
          (when (and content (not (str/blank? content)))
            (swap! state update :turn inc)
            (scribe-turn! session-id :agent content))))

      ;; Assistant message start - reset content accumulator (streaming format)
      "message"
      (let [msg (:message event)]
        (when (= "assistant" (:role msg))
          (swap! state update :turn inc)
          (swap! state assoc :current-content (StringBuilder.))
          (when-let [session-id (:session-id @state)]
            (turn-start! session-id (:turn @state)))))

      ;; Content delta - accumulate text
      "content_block_delta"
      (when-let [text (get-in event [:delta :text])]
        (.append (:current-content @state) text))

      ;; Tool use - detect native planning tools
      "tool_use"
      (let [tool-name (or (:name event)
                          (get-in event [:tool_use :name])
                          (get-in event [:content_block :name]))]
        (when (contains? native-planning-tools tool-name)
          (swap! state assoc :native-planning-detected true)
          (when-let [session-id (:session-id @state)]
            (let [input (or (:input event)
                            (get-in event [:tool_use :input])
                            (get-in event [:content_block :input]))]
              (note-native-planning! session-id tool-name
                                     {:task-id (get input :taskId)
                                      :subject (get input :subject)})
              ;; Track TaskCreate for batch recording
              (when (= "TaskCreate" tool-name)
                (swap! state update :pending-tasks conj
                       {:id (get input :taskId)
                        :subject (get input :subject)
                        :description (get input :description)
                        :status "pending"}))))))

      ;; Tool result - capture task list if TaskList was called
      "tool_result"
      (when (:native-planning-detected @state)
        (when-let [tasks (get-in event [:result :tasks])]
          (swap! state assoc :pending-tasks (vec tasks))))

      ;; Result - scribe accumulated content and end turn
      "result"
      (when-let [session-id (:session-id @state)]
        (let [content (str (:current-content @state))]
          (when-let [diagram (parse-plan-diagram content)]
            (turn-plan-diagram! session-id (:turn @state) diagram))
          (when (seq content)
            (scribe-turn! session-id :agent content)))
        ;; Record native plan at turn end if planning was detected
        (when (:native-planning-detected @state)
          (let [tasks (:pending-tasks @state)]
            (when (seq tasks)
              (record-native-plan! session-id tasks)))
          (swap! state assoc :native-planning-detected false :pending-tasks []))
        (turn-end! session-id (:turn @state)))

      ;; Pass through
      nil)

    ;; Always echo the original event
    (println (json/generate-string event))
    (flush)))

(defn process-stream []
  ;; Initialize session
  (let [intent (or musn-intent "stream session")
        hud (build-hud! intent)
        session (create-session! intent)
        session-id (or (:session/id session) musn-session-id (str "bb-" (System/currentTimeMillis)))]

    (swap! state assoc :session-id session-id)
    (log! "session started" {:session-id session-id :intent intent :hud? (some? hud)})

    ;; Process stdin line by line
    (with-open [rdr (io/reader *in*)]
      (doseq [line (line-seq rdr)]
        (when-not (str/blank? line)
          (try
            (let [event (json/parse-string line true)]
              (handle-event event))
            (catch Throwable t
              (log! "parse error" (.getMessage t) line)
              ;; Echo raw line on parse error
              (println line)
              (flush))))))))

;; Main
(process-stream)
