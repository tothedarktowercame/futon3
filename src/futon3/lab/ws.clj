(ns futon3.lab.ws
  "WebSocket server for Lab session streaming using Java-WebSocket library.
   Runs on separate port (default 5056) to avoid http-kit WebSocket masking issues.

   Merges Claude Code JSONL with PAR sidecar files for unified session view."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (org.java_websocket WebSocket)
           (org.java_websocket.handshake ClientHandshake)
           (org.java_websocket.server WebSocketServer)
           (java.net InetSocketAddress URI)
           (java.time Instant)))

(defonce ^:private server-state (atom nil))
(defonce ^:private clients (atom {})) ; WebSocket -> {:path :stop-flag :mode}

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

(defn- upload-root []
  (let [root (or (System/getenv "LAB_UPLOAD_ROOT")
                 (System/getenv "AGENCY_LAB_UPLOAD_ROOT")
                 "lab/remote")]
    (io/file root)))

(defn- safe-session-id [session-id]
  (-> (str session-id)
      (str/replace #"[^A-Za-z0-9._-]" "_")
      (str/replace #"__+" "_")))

(defn- upload-session-path [session-id]
  (io/file (upload-root) (str (safe-session-id session-id) ".jsonl")))

(defn- upload-meta-path [session-id]
  (io/file (upload-root) (str (safe-session-id session-id) ".meta.edn")))

(defn- read-upload-meta [session-id]
  (let [path (upload-meta-path session-id)]
    (when (.exists path)
      (try
        (edn/read-string (slurp path))
        (catch Exception _ nil)))))

(defn- write-upload-meta! [session-id meta]
  (let [path (upload-meta-path session-id)
        merged (merge {:session-id session-id
                       :created-at (now-iso)}
                      meta)]
    (io/make-parents path)
    (spit path (pr-str merged))))

(defn- update-upload-meta! [session-id meta]
  (let [path (upload-meta-path session-id)
        merged (merge (or (read-upload-meta session-id) {})
                      meta
                      {:session-id session-id
                       :updated-at (now-iso)})]
    (io/make-parents path)
    (spit path (pr-str merged))))

(defn- upload-init-complete? [session-id]
  (true? (:init/complete (read-upload-meta session-id))))

(defn- append-upload-line! [session-id line]
  (let [path (upload-session-path session-id)]
    (io/make-parents path)
    (spit path (str line "\n") :append true)))

(defn- append-upload-event! [session-id event]
  (cond
    (string? event) (append-upload-line! session-id event)
    (map? event) (append-upload-line! session-id (json/encode event))
    :else nil))

(defn- send-json! [^WebSocket conn data]
  (when (.isOpen conn)
    (try
      (.send conn (json/encode data))
      (catch Exception e
        (println "[lab-ws] Send failed:" (.getMessage e))))))

(defn- codex-content-text
  [content]
  (cond
    (string? content) content
    (sequential? content) (->> content
                               (map :text)
                               (filter some?)
                               (str/join "\n"))
    :else nil))

(defn- parse-claude-jsonl-line
  "Parse a single JSONL line from Claude/Codex transcript."
  [line]
  (try
    (let [entry (json/parse-string line true)
          entry-type (:type entry)
          payload (:payload entry)
          timestamp (:timestamp entry)]
      (case entry-type
        ;; Claude JSONL
        "user"
        (let [content (or (:content (:message entry))
                          (when (string? (:message entry)) (:message entry))
                          "")]
          {:type "user"
           :timestamp timestamp
           :text (if (string? content)
                   content
                   (->> content
                        (filter #(= "text" (:type %)))
                        (map :text)
                        (str/join "\n")))})

        "assistant"
        (let [content (or (:content (:message entry))
                          (when (string? (:message entry)) (:message entry))
                          "")]
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

        "par"
        {:type "par"
         :timestamp timestamp
         :text (or (:text entry)
                   (:summary entry)
                   (get-in entry [:message :content])
                   "")
         :par-id (:par-id entry)
         :tags (:tags entry)}

        ;; Codex JSONL
        "response_item"
        (let [payload-type (:type payload)]
          (case payload-type
            "message"
            (let [role (:role payload)
                  text (codex-content-text (:content payload))
                  role-type (case role
                              "user" "user"
                              "assistant" "assistant"
                              "system" "summary"
                              "developer" "summary"
                              (or role "summary"))]
              (when (seq text)
                {:type role-type
                 :timestamp timestamp
                 :text text}))

            "function_call"
            {:type "tool_use"
             :timestamp timestamp
             :tool-name (or (:name payload) "unknown")
             :input (or (:arguments payload) "")}

            "custom_tool_call"
            {:type "tool_use"
             :timestamp timestamp
             :tool-name (or (:name payload) "unknown")
             :input (or (:arguments payload) "")}

            "function_call_output"
            {:type "tool_result"
             :timestamp timestamp
             :content (str (or (:output payload) ""))}

            "custom_tool_call_output"
            {:type "tool_result"
             :timestamp timestamp
             :content (str (or (:output payload) ""))}

            nil))

        "event_msg"
        ;; Codex JSONL duplicates user/agent text in response_item messages.
        ;; Ignore event_msg text to avoid double-rendering.
        nil

        nil))
    (catch Exception _ nil)))

(defn- par-sidecar-path
  "Compute PAR sidecar path from JSONL path.
   /path/to/session.jsonl -> /path/to/session.par.edn"
  [jsonl-path]
  (str/replace jsonl-path #"\.jsonl$" ".par.edn"))

(defn- read-par-sidecar
  "Read PAR events from sidecar file. Returns vector of PAR events."
  [sidecar-path]
  (when (.exists (io/file sidecar-path))
    (try
      (let [content (slurp sidecar-path)
            pars (edn/read-string content)]
        (mapv (fn [par]
                {:type "par"
                 :timestamp (:timestamp par)
                 :text (str "## PAR: " (or (:title par) "Session Review") "\n\n"
                            (when-let [q (:questions par)]
                              (str "**Intention:** " (:intention q) "\n\n"
                                   "**Happening:** " (:happening q) "\n\n"
                                   "**Perspectives:** " (:perspectives q) "\n\n"
                                   "**Learned:** " (:learned q) "\n\n"
                                   "**Forward:** " (:forward q))))
                 :par-id (:id par)
                 :tags (:tags par)})
              (if (vector? pars) pars [pars])))
      (catch Exception e
        (println "[lab-ws] Error reading PAR sidecar:" (.getMessage e))
        []))))

(defn- merge-events-by-timestamp
  "Merge two event vectors, sorted by timestamp."
  [events1 events2]
  (->> (concat events1 events2)
       (sort-by :timestamp)
       vec))

(defn- read-session-history
  "Read and parse full session history from JSONL file, merged with PAR sidecar."
  [path]
  (when (.exists (io/file path))
    (with-open [rdr (io/reader path)]
      (let [all-lines (vec (line-seq rdr))
            jsonl-events (->> all-lines
                              (map parse-claude-jsonl-line)
                              (filter some?)
                              (filter #(seq (:text %)))
                              vec)
            sidecar-path (par-sidecar-path path)
            par-events (read-par-sidecar sidecar-path)
            merged-events (merge-events-by-timestamp jsonl-events par-events)]
        {:line-count (count all-lines)
         :par-count (count par-events)
         :events merged-events}))))

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
              path-only (first (str/split uri #"\?"))
              params (parse-query-params uri)
              path (:path params)]
          (println "[lab-ws] Client connected:" (.getRemoteSocketAddress conn) "path:" path "uri:" path-only)

          (cond
            (and path (.exists (io/file path)))
            (let [stop-flag (atom false)
                  history (read-session-history path)]
              (swap! clients assoc conn {:path path :stop-flag stop-flag :mode :stream})

              ;; Send full history (JSONL + PARs merged)
              (send-json! conn {:type "init"
                                :path path
                                :line-count (:line-count history)
                                :par-count (:par-count history 0)
                                :events (:events history)})

              ;; Start watching for new events
              (start-file-watcher! conn path stop-flag))

            (= path-only "/fulab/lab/upload/ws")
            (swap! clients assoc conn {:mode :upload})

            :else
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
          (let [msg (json/parse-string message true)
                {:keys [mode]} (get @clients conn)]
            (cond
              (= (:type msg) "ping")
              (send-json! conn {:type "pong" :at (now-iso)})

              (= mode :upload)
              (case (:type msg)
                "init" (let [session-id (or (:session-id msg) (:session/id msg) (:id msg))]
                         (when session-id
                           (write-upload-meta! session-id
                                               (select-keys msg [:project :source :originator :cwd :remote-source]))
                           (if (upload-init-complete? session-id)
                             (send-json! conn {:type "ack" :session-id session-id :dedup true})
                             (do
                               (doseq [event (:events msg)]
                                 (append-upload-event! session-id event))
                               (update-upload-meta! session-id {:init/complete true
                                                                :init/event-count (count (:events msg))})
                               (send-json! conn {:type "ack" :session-id session-id})))))
                "event" (let [session-id (or (:session-id msg) (:session/id msg) (:id msg))]
                          (when session-id
                            (append-upload-event! session-id (:event msg))
                            (update-upload-meta! session-id {})
                            (send-json! conn {:type "ack" :session-id session-id})))
                "par" (let [session-id (or (:session-id msg) (:session/id msg) (:id msg))]
                        (when session-id
                          (append-upload-event! session-id (merge {:type "par"} (:par msg)))
                          (update-upload-meta! session-id {})
                          (send-json! conn {:type "ack" :session-id session-id})))
                nil)
              :else nil))
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
         (.setReuseAddr server true)  ; Allow quick restarts
         (.start server)
         ;; Wait briefly for server thread to bind
         (Thread/sleep 100)
         ;; Verify the port is actually listening
         (let [bound? (try
                        (with-open [sock (java.net.Socket.)]
                          (.connect sock (java.net.InetSocketAddress. "127.0.0.1" port) 100)
                          true)
                        (catch Exception _ false))]
           (if bound?
             (do
               (reset! server-state {:server server :port port})
               (println "[lab-ws] Lab WebSocket server running on port" port)
               server)
             (do
               (println "[lab-ws] ERROR: Server started but port" port "not listening")
               (.stop server 0)
               nil))))
       (catch Exception e
         (println "[lab-ws] ERROR: Failed to start WebSocket server on port" port)
         (println "[lab-ws] Exception:" (.getMessage e))
         (.printStackTrace e)
         nil)))))

(defn stop! []
  (when-let [{:keys [server]} @server-state]
    ;; Stop all watchers (safely handle nil stop-flags)
    (doseq [[_ {:keys [stop-flag]}] @clients]
      (when stop-flag
        (reset! stop-flag true)))
    (try
      (.stop server 1000)
      (catch Exception e
        (println "[lab-ws] Error stopping server:" (.getMessage e))))
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
