(ns futon3.lab.ws
  "WebSocket server for Lab session streaming using Java-WebSocket library.
   Runs on separate port (default 5056) to avoid http-kit WebSocket masking issues."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.java_websocket WebSocket)
           (org.java_websocket.handshake ClientHandshake)
           (org.java_websocket.server WebSocketServer)
           (java.net InetSocketAddress URI)
           (java.time Instant)))

(defonce ^:private server-state (atom nil))
(defonce ^:private clients (atom {})) ; WebSocket -> {:path :stop-flag}

(defn- now-iso []
  (str (Instant/now)))

(defn- parse-query-params [uri-str]
  (try
    (let [uri (URI. uri-str)
          query (.getQuery uri)]
      (when query
        (->> (str/split query #"&")
             (map #(str/split % #"=" 2))
             (map (fn [[k v]] [(keyword k) (java.net.URLDecoder/decode (or v "") "UTF-8")]))
             (into {}))))
    (catch Exception _ {})))

(defn- send-json! [^WebSocket conn data]
  (when (.isOpen conn)
    (try
      (.send conn (json/encode data))
      (catch Exception e
        (println "[lab-ws] Send failed:" (.getMessage e))))))

(defn- parse-claude-jsonl-line
  "Parse a single JSONL line from Claude Code transcript."
  [line]
  (try
    (let [entry (json/parse-string line true)
          msg-type (:type entry)
          message (:message entry)
          timestamp (:timestamp entry)]
      (case msg-type
        "user"
        (let [content (or (:content message) (when (string? message) message) "")]
          {:type "user"
           :timestamp timestamp
           :text (if (string? content)
                   content
                   (->> content
                        (filter #(= "text" (:type %)))
                        (map :text)
                        (str/join "\n")))})

        "assistant"
        (let [content (or (:content message) (when (string? message) message) "")]
          {:type "assistant"
           :timestamp timestamp
           :text (if (string? content)
                   content
                   (->> content
                        (filter #(= "text" (:type %)))
                        (map :text)
                        (str/join "\n")))})

        "summary"
        {:type "summary"
         :timestamp timestamp
         :text (or (:summary entry) "")}

        nil))
    (catch Exception _ nil)))

(defn- read-session-history
  "Read and parse full session history from JSONL file."
  [path]
  (when (.exists (io/file path))
    (with-open [rdr (io/reader path)]
      (let [all-lines (vec (line-seq rdr))
            events (->> all-lines
                        (map parse-claude-jsonl-line)
                        (filter some?)
                        (filter #(seq (:text %)))
                        vec)]
        {:line-count (count all-lines)
         :events events}))))

(defn- start-file-watcher!
  "Start watching a JSONL file for changes, sending new events to conn."
  [^WebSocket conn path stop-flag]
  (future
    (try
      (let [file (io/file path)
            last-size (atom (.length file))
            last-line-count (atom (with-open [r (io/reader file)]
                                    (count (line-seq r))))]
        (while (and (.isOpen conn) (not @stop-flag))
          (Thread/sleep 500)
          (when (.exists file)
            (let [current-size (.length file)]
              (when (> current-size @last-size)
                (try
                  (with-open [rdr (io/reader file)]
                    (let [all-lines (vec (line-seq rdr))
                          current-count (count all-lines)]
                      (when (> current-count @last-line-count)
                        (doseq [line (subvec all-lines @last-line-count)]
                          (when-let [event (parse-claude-jsonl-line line)]
                            (when (seq (:text event))
                              (send-json! conn {:type "event" :event event}))))
                        (reset! last-line-count current-count))))
                  (catch Exception e
                    (println "[lab-ws] File read error:" (.getMessage e))))
                (reset! last-size current-size))))))
      (catch Exception e
        (println "[lab-ws] Watcher error:" (.getMessage e))))))

(defn- create-server [port]
  (let [addr (InetSocketAddress. port)]
    (proxy [WebSocketServer] [addr]

      (onOpen [^WebSocket conn ^ClientHandshake handshake]
        (let [uri (.getResourceDescriptor handshake)
              params (parse-query-params uri)
              path (:path params)]
          (println "[lab-ws] Client connected:" (.getRemoteSocketAddress conn) "path:" path)

          (if (and path (.exists (io/file path)))
            (let [stop-flag (atom false)
                  history (read-session-history path)]
              (swap! clients assoc conn {:path path :stop-flag stop-flag})

              ;; Send full history
              (send-json! conn {:type "init"
                                :path path
                                :line-count (:line-count history)
                                :events (:events history)})

              ;; Start watching for new events
              (start-file-watcher! conn path stop-flag))

            ;; Invalid path
            (do
              (send-json! conn {:type "error" :err "invalid-path" :path path})
              (.close conn)))))

      (onClose [^WebSocket conn code reason remote]
        (println "[lab-ws] Client disconnected:" (.getRemoteSocketAddress conn) "code:" code)
        (when-let [{:keys [stop-flag]} (get @clients conn)]
          (reset! stop-flag true))
        (swap! clients dissoc conn))

      (onMessage [^WebSocket conn ^String message]
        (try
          (let [msg (json/parse-string message true)]
            (case (:type msg)
              "ping" (send-json! conn {:type "pong" :at (now-iso)})
              nil))
          (catch Exception e
            (println "[lab-ws] Message parse error:" (.getMessage e)))))

      (onError [^WebSocket conn ^Exception ex]
        (println "[lab-ws] WebSocket error:" (.getMessage ex))
        (when conn
          (when-let [{:keys [stop-flag]} (get @clients conn)]
            (reset! stop-flag true))
          (swap! clients dissoc conn)))

      (onStart []
        (println "[lab-ws] Lab WebSocket server started on port" port)))))

(defn start!
  ([] (start! {}))
  ([{:keys [port]}]
   (let [port (or port
                  (some-> (System/getenv "LAB_WS_PORT") Integer/parseInt)
                  5056)]
     (try
       (let [server (create-server port)]
         (.start server)
         (reset! server-state {:server server :port port})
         (println "[lab-ws] Lab WebSocket server running on port" port)
         server)
       (catch Exception e
         (println "[lab-ws] ERROR: Failed to start WebSocket server on port" port)
         (println "[lab-ws] Exception:" (.getMessage e))
         (.printStackTrace e)
         nil)))))

(defn stop! []
  (when-let [{:keys [server]} @server-state]
    ;; Stop all watchers
    (doseq [[_ {:keys [stop-flag]}] @clients]
      (reset! stop-flag true))
    (.stop server 1000)
    (reset! server-state nil)
    (reset! clients {})
    (println "[lab-ws] Lab WebSocket server stopped")))

(defn status
  "Return current WebSocket server status."
  []
  (if-let [{:keys [port]} @server-state]
    {:running true
     :port port
     :clients (count @clients)}
    {:running false}))
