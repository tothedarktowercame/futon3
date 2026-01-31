#!/usr/bin/env bb
;; WSS bridge - connects via WSS and outputs JSON lines to stdout
;; Emacs can start this as a subprocess and read from it
;;
;; Usage: bb scripts/wss-bridge.clj <wss-url>
;; Example: bb scripts/wss-bridge.clj "wss://host:5057?path=/path/to/session.jsonl"

(require '[babashka.http-client.websocket :as ws])

(def url (or (first *command-line-args*)
             (do (println "Usage: wss-bridge.clj <wss-url>")
                 (System/exit 1))))

(def running (atom true))

(let [conn (ws/websocket
            {:uri url
             :on-open (fn [ws]
                        (binding [*out* *err*]
                          (println "[wss-bridge] Connected")))
             :on-message (fn [ws msg last?]
                           ;; Output each message as a line to stdout
                           (println (str msg))
                           (flush))
             :on-close (fn [ws status reason]
                         (binding [*out* *err*]
                           (println "[wss-bridge] Closed:" status reason))
                         (reset! running false))
             :on-error (fn [ws err]
                         (binding [*out* *err*]
                           (println "[wss-bridge] Error:" err))
                         (reset! running false))})]

  ;; Keep running until closed
  (while @running
    (Thread/sleep 100))

  (shutdown-agents))
