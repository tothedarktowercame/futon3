#!/usr/bin/env bb
;; Minimal WSS echo server for Emacs bug report
;; Requires: Java 11+, runs on port 5059
;;
;; Test with:
;;   bb scripts/test-wss-client.clj  -> works
;;   Emacs 31 websocket.el           -> fails with 400
;;
;; This is a plain WS server - use nginx for SSL termination

(require '[org.httpkit.server :as server])

(def clients (atom #{}))

(defn ws-handler [req]
  (server/with-channel req channel
    (swap! clients conj channel)
    (println "[echo] Client connected")
    (server/send! channel "{\"type\":\"hello\",\"msg\":\"Echo server ready\"}")

    (server/on-receive channel
      (fn [msg]
        (println "[echo] Received:" msg)
        (server/send! channel (str "{\"type\":\"echo\",\"msg\":" (pr-str msg) "}"))))

    (server/on-close channel
      (fn [status]
        (swap! clients disj channel)
        (println "[echo] Client disconnected:" status)))))

(defn -main [& args]
  (let [port (or (some-> (first args) parse-long) 5059)]
    (println (str "Echo server starting on ws://localhost:" port))
    (server/run-server ws-handler {:port port})
    (println "Server running. Press Ctrl+C to stop.")
    @(promise)))  ; Block forever

(-main)
