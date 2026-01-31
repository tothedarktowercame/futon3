#!/usr/bin/env bb
;; Test WSS client - compare what we send vs what Emacs sends
;; Run: bb scripts/test-wss-client.clj

(require '[babashka.http-client.websocket :as ws])

(def url "wss://172-236-28-208.ip.linodeusercontent.com:5057")
(def path "/home/joe/.claude/projects/-home-joe/64570417-4354-40b8-b6a5-db804f69a1d0.jsonl")

(println "=== Clojure WSS Test Client ===")
(println "URL:" url)
(println "Path:" path)
(println)

(try
  (let [messages (atom [])
        conn (ws/websocket
              {:uri (str url "?path=" (java.net.URLEncoder/encode path "UTF-8"))
               :on-open (fn [ws]
                          (println "[OPEN] Connected!"))
               :on-message (fn [ws msg last?]
                             (swap! messages conj msg)
                             (println "[MSG]" (subs (str msg) 0 (min 200 (count (str msg)))) "..."))
               :on-close (fn [ws status reason]
                           (println "[CLOSE] status:" status "reason:" reason))
               :on-error (fn [ws err]
                           (println "[ERROR]" err))})]
    (println "WebSocket opened, waiting for messages...")
    (Thread/sleep 3000)
    (println "Received" (count @messages) "messages")
    (ws/close! conn)
    (println "Done."))
  (catch Exception e
    (println "Exception:" (.getMessage e))
    (when-let [cause (.getCause e)]
      (println "Cause:" (.getMessage cause)))))
