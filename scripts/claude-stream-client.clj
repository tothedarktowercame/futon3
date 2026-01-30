#!/usr/bin/env bb
;; Generic Claude Code JSONL stream client
;;
;; Usage:
;;   bb scripts/claude-stream-client.clj <jsonl-path> [server-url]
;;
;; Example:
;;   bb scripts/claude-stream-client.clj ~/.claude/projects/.../session.jsonl
;;   bb scripts/claude-stream-client.clj ~/.claude/projects/.../session.jsonl ws://remote:5050
;;
;; Output:
;;   Streams events to stdout as they arrive, formatted for terminal.
;;   Use for Codex, remote monitoring, or piping to other tools.

(require '[babashka.http-client.websocket :as ws]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def default-server "ws://localhost:5050")

(defn format-timestamp [ts]
  (when (and ts (> (count ts) 19))
    (subs ts 11 19)))

(defn truncate [s max-len]
  (if (> (count s) max-len)
    (str (subs s 0 (- max-len 3)) "...")
    s))

(defn format-event [event]
  (let [type (get event "type" "unknown")
        ts (format-timestamp (get event "timestamp"))
        text (get event "text" "")
        tool-name (get event "tool-name")]
    (str
     (format "[%s] " (or ts "??:??:??"))
     (case type
       "user" (str "\033[36mUSER:\033[0m " (truncate text 200))
       "assistant" (str "\033[32mAGENT:\033[0m " (truncate text 200))
       "tool_use" (str "\033[35mTOOL:\033[0m " (or tool-name "?"))
       "tool_result" "\033[35mRESULT:\033[0m ..."
       "summary" "\033[33m[CONTEXT COMPACTED]\033[0m"
       (format "[%s]" type)))))

(defn on-message [_ws data _last]
  (try
    (let [msg (str data)  ;; data is already a string from http-kit
          parsed (json/parse-string msg)
          msg-type (get parsed "type")]
      (case msg-type
        "init"
        (let [events (get parsed "events" [])
              line-count (get parsed "line-count" 0)]
          (println (format "--- Connected: %d lines, showing %d recent ---"
                           line-count (count events)))
          (println)
          (doseq [event events]
            (println (format-event event))))

        "event"
        (let [event (get parsed "event")]
          (println (format-event event)))

        "pong"
        nil ;; ignore keepalive

        "error"
        (println (format "ERROR: %s" (get parsed "err")))

        nil))
    (catch Exception e
      (println "Parse error:" (.getMessage e)))))

(defn on-close [_ws code reason]
  (println)
  (println (format "--- Disconnected: %s %s ---" code reason))
  (System/exit 0))

(defn on-error [_ws err]
  (println (format "--- Error: %s ---" err)))

(defn on-open [ws]
  (println "WebSocket connected."))

(defn start-stream [jsonl-path server-url]
  (let [encoded-path (java.net.URLEncoder/encode jsonl-path "UTF-8")
        url (format "%s/fulab/claude-stream/ws?path=%s&tail=50"
                    server-url encoded-path)]
    (println (format "Connecting to stream: %s" jsonl-path))
    (println (format "Server: %s" server-url))
    (println)

    (let [ws (ws/websocket {:uri url
                            :on-open on-open
                            :on-message on-message
                            :on-close on-close
                            :on-error on-error})]
      ;; Keepalive ping loop
      (future
        (loop []
          (Thread/sleep 30000)
          (try
            (ws/send! ws "{\"type\":\"ping\"}")
            (catch Exception _ nil))
          (recur)))

      ;; Wait forever (websocket runs in background)
      (loop []
        (Thread/sleep 1000)
        (recur)))))

;; CLI
(let [args *command-line-args*]
  (if (empty? args)
    (do
      (println "Usage: claude-stream-client.clj <jsonl-path> [server-url]")
      (println)
      (println "Example:")
      (println "  bb scripts/claude-stream-client.clj ~/.claude/projects/.../session.jsonl")
      (System/exit 1))
    (let [jsonl-path (first args)
          server-url (or (second args) default-server)]
      (if (not (.exists (java.io.File. jsonl-path)))
        (do
          (println "File not found:" jsonl-path)
          (System/exit 1))
        (start-stream jsonl-path server-url)))))
