(ns futon3.forum.ws
  "WebSocket server for Forum streaming using Java-WebSocket library.
   Runs on separate port (default 5055) to avoid http-kit WebSocket issues."
  (:require [cheshire.core :as json]
            [futon3.forum.service :as forum])
  (:import (org.java_websocket WebSocket)
           (org.java_websocket.handshake ClientHandshake)
           (org.java_websocket.server WebSocketServer)
           (java.net InetSocketAddress URI)
           (java.time Instant)))

(defonce ^:private server-state (atom nil))
(defonce ^:private clients (atom {})) ; WebSocket -> {:thread-id :author :tag}

(defn- now-iso []
  (str (Instant/now)))

(defn- parse-query-params [uri-str]
  (try
    (let [uri (URI. uri-str)
          query (.getQuery uri)]
      (when query
        (->> (clojure.string/split query #"&")
             (map #(clojure.string/split % #"=" 2))
             (map (fn [[k v]] [(keyword k) (java.net.URLDecoder/decode (or v "") "UTF-8")]))
             (into {}))))
    (catch Exception _ {})))

(defn- send-json! [^WebSocket conn data]
  (when (.isOpen conn)
    (try
      (.send conn (json/encode data))
      (catch Exception e
        (println "Send failed:" (.getMessage e))))))

(defn- broadcast-to-subscribers! [event]
  (let [thread-id (get-in event [:post :post/thread-id])
        author (get-in event [:post :post/author])
        tags (get-in event [:post :post/tags] [])]
    (doseq [[conn filters] @clients]
      (let [{:keys [thread-id filter-author filter-tag]} filters]
        ;; Check filters
        (when (and (or (nil? thread-id) (= thread-id thread-id))
                   (or (nil? filter-author) (= filter-author author))
                   (or (nil? filter-tag) (some #{filter-tag} tags)))
          (send-json! conn event))))))

;; Register as forum subscriber
(defn- forum-event-handler [event]
  (broadcast-to-subscribers! event))

(defn- create-server [port]
  (let [addr (InetSocketAddress. port)]
    (proxy [WebSocketServer] [addr]

      (onOpen [^WebSocket conn ^ClientHandshake handshake]
        (let [uri (.getResourceDescriptor handshake)
              params (parse-query-params uri)
              filters {:thread-id (:thread-id params)
                       :filter-author (:author params)
                       :filter-tag (some-> (:tag params) keyword)}]
          (println "Client connected:" (.getRemoteSocketAddress conn) "filters:" filters)
          (swap! clients assoc conn filters)

          ;; Send init message
          (let [recent-threads (forum/list-threads :limit 10)
                recent-posts (->> recent-threads
                                  (mapcat #(forum/get-thread-posts (:thread/id %)))
                                  (sort-by :post/timestamp)
                                  (take-last 20))]
            (send-json! conn {:type "init"
                              :thread-count (count @forum/threads)
                              :post-count (count @forum/posts)
                              :recent-posts (vec recent-posts)}))))

      (onClose [^WebSocket conn code reason remote]
        (println "Client disconnected:" (.getRemoteSocketAddress conn) "code:" code)
        (swap! clients dissoc conn))

      (onMessage [^WebSocket conn ^String message]
        (try
          (let [msg (json/parse-string message true)]
            (case (:type msg)
              "ping" (send-json! conn {:type "pong" :at (now-iso)})
              "filter" (swap! clients assoc conn
                              {:thread-id (:thread-id msg)
                               :filter-author (:author msg)
                               :filter-tag (some-> (:tag msg) keyword)})
              nil))
          (catch Exception e
            (println "Message parse error:" (.getMessage e)))))

      (onError [^WebSocket conn ^Exception ex]
        (println "WebSocket error:" (.getMessage ex))
        (when conn
          (swap! clients dissoc conn)))

      (onStart []
        (println "[forum-ws] WebSocket server started on port" port)))))

(defn start!
  ([] (start! {}))
  ([{:keys [port]}]
   (let [port (or port
                  (some-> (System/getenv "FORUM_WS_PORT") Integer/parseInt)
                  5055)]
     (try
       (let [server (create-server port)]
         ;; Register with forum service for events
         (forum/register-event-handler! forum-event-handler)

         (.start server)
         (reset! server-state {:server server :port port})

         (println "[forum-ws] Forum WebSocket server running on port" port)
         server)
       (catch Exception e
         (println "[forum-ws] ERROR: Failed to start WebSocket server on port" port)
         (println "[forum-ws] Exception:" (.getMessage e))
         (.printStackTrace e)
         nil)))))

(defn stop! []
  (when-let [{:keys [server]} @server-state]
    (forum/unregister-event-handler! forum-event-handler)
    (.stop server 1000)
    (reset! server-state nil)
    (reset! clients {})
    (println "[forum-ws] Forum WebSocket server stopped")))

(defn status
  "Return current WebSocket server status."
  []
  (if-let [{:keys [server port]} @server-state]
    {:running true
     :port port
     :clients (count @clients)}
    {:running false}))
