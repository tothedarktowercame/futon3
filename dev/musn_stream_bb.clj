#!/usr/bin/env bb
;; Babashka-compatible MUSN stream processor
;; Lightweight shim that forwards Claude JSON events to MUSN HTTP service
;; Heavy lifting (HUD building, pattern matching) happens in the running JVM

(ns musn-stream-bb
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
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

(defn scribe-turn! [session-id role content]
  (when (and session-id (seq content))
    (musn-post! "/musn/scribe/turn"
                {:session/id session-id
                 :role (name role)
                 :content content})))

;; State atom
(def state (atom {:session-id nil
                  :turn 0
                  :started? false
                  :current-content (StringBuilder.)}))

(defn extract-type [event]
  (or (:type event)
      (when (map? event)
        (cond
          (:message event) "message"
          (:result event) "result"
          (:content_block_start event) "content_block_start"
          (:content_block_delta event) "content_block_delta"
          (:content_block_stop event) "content_block_stop"
          :else nil))))

(defn handle-event [event]
  (let [event-type (extract-type event)]
    (case event-type
      ;; User message - record immediately
      "human"
      (when-let [session-id (:session-id @state)]
        (let [content (get-in event [:message :content])]
          (when (string? content)
            (scribe-turn! session-id :user content))))

      ;; Assistant message start - reset content accumulator
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

      ;; Result - scribe accumulated content and end turn
      "result"
      (when-let [session-id (:session-id @state)]
        (let [content (str (:current-content @state))]
          (when (seq content)
            (scribe-turn! session-id :agent content)))
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
