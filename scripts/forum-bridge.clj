#!/usr/bin/env bb
;; Forum bridge - routes forum posts to Agency and posts replies back via Agency callbacks.
;;
;; Environment:
;;   FORUM_SERVER        - Forum base URL (default: http://localhost:5050)
;;   FORUM_THREAD        - Thread id to watch (required)
;;   FORUM_MENTION       - Optional mention gate (e.g. "@fucodex")
;;   FORUM_SEEN_PATH     - Path to seen-state EDN (default: lab/agency/forum-seen-<thread>.edn)
;;   FORUM_BRIDGE_PATTERN - Pattern-applied value to treat as bridge output (default: peripheral id)
;;   FORUM_TOKEN         - Optional shared secret for Forum (sent as X-Agency-Token via Agency)
;;
;;   AGENCY_SERVER       - Agency base URL (default: http://localhost:7070)
;;   AGENCY_AGENT_ID     - Agent id (default: fucodex)
;;   AGENCY_PERIPHERAL   - Peripheral name (default: chat)
;;   AGENCY_TOKEN        - Optional token for Agency endpoint (if added later)
;;
;;   AGENCY_MUSN_URL     - Optional MUSN URL to pass through
;;   AGENCY_MUSN_SESSION_ID - Optional MUSN session id

(require '[babashka.http-client :as http]
         '[babashka.http-client.websocket :as ws]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def forum-server (or (System/getenv "FORUM_SERVER") "http://localhost:5050"))
(def forum-ws-server (or (System/getenv "FORUM_WS_SERVER") "ws://localhost:5055"))
(def forum-thread (System/getenv "FORUM_THREAD"))
(def mention (System/getenv "FORUM_MENTION"))
(def agent-id (or (System/getenv "AGENCY_AGENT_ID") "fucodex"))
(def peripheral (or (System/getenv "AGENCY_PERIPHERAL") "chat"))
(def bridge-pattern (or (System/getenv "FORUM_BRIDGE_PATTERN") peripheral))
(def forum-token (System/getenv "FORUM_TOKEN"))
(def agency-server (or (System/getenv "AGENCY_SERVER") "http://localhost:7070"))
(def agency-token (System/getenv "AGENCY_TOKEN"))
(def musn-url (System/getenv "AGENCY_MUSN_URL"))
(def musn-session-id (System/getenv "AGENCY_MUSN_SESSION_ID"))

(when (str/blank? forum-thread)
  (binding [*out* *err*]
    (println "FORUM_THREAD is required"))
  (System/exit 2))

(def seen-path
  (or (System/getenv "FORUM_SEEN_PATH")
      (str (io/file "lab" "agency" (str "forum-seen-" forum-thread ".edn")))))

(defn load-seen []
  (try
    (when (.exists (io/file seen-path))
      (edn/read-string (slurp seen-path)))
    (catch Exception _ nil)))

(defn save-seen! [state]
  (try
    (io/make-parents seen-path)
    (spit seen-path (pr-str state))
    (catch Exception _ nil)))

(defonce seen-state (atom (or (load-seen) {:seen #{}})))

(defn mark-seen! [post-id]
  (swap! seen-state update :seen (fnil conj #{}) post-id)
  (save-seen! @seen-state))

(defn seen? [post-id]
  (contains? (:seen @seen-state) post-id))

(defn post-bridge? [post]
  (= bridge-pattern (:post/pattern-applied post)))

(defn mention-ok? [body]
  (if (and mention (not (str/blank? mention)))
    (str/includes? (str/lower-case (or body "")) (str/lower-case mention))
    true))

(defn agency-run! [prompt]
  (let [payload (cond-> {:agent-id agent-id
                         :peripheral peripheral
                         :prompt prompt
                         :forum {:server forum-server
                                 :thread-id forum-thread
                                 :token forum-token
                                 :author agent-id}}
                 (or musn-url musn-session-id)
                 (assoc :musn (cond-> {}
                                musn-url (assoc :url musn-url)
                                musn-session-id (assoc :session-id musn-session-id))))
        headers (cond-> {"Content-Type" "application/json"}
                  agency-token (assoc "X-Agency-Token" agency-token))
        resp (http/post (str (str/replace agency-server #"/+$" "") "/agency/run")
                        {:headers headers
                         :body (json/generate-string payload)})]
    (json/parse-string (:body resp) true)))

(defn handle-post [post]
  (let [post-id (:post/id post)
        body (:post/body post)
        thread-id (:post/thread-id post)]
    (when (and (= forum-thread thread-id)
               (not (seen? post-id)))
      (mark-seen! post-id)
      (when (and (not (post-bridge? post))
                 (mention-ok? body))
        (println "Dispatching to Agency:" post-id)
        (try
          (agency-run! body)
          (catch Exception e
            (binding [*out* *err*]
              (println "Agency run failed:" (.getMessage e)))))))))

(defn ws-url []
  ;; Use dedicated Java-WebSocket server (port 5055 by default)
  (str forum-ws-server "?thread-id=" forum-thread))

(let [url (ws-url)]
  (println "Connecting to" url)
  (ws/websocket
   {:uri url
    :on-open (fn [_ws] (println "Connected."))
    :on-message (fn [_ws data _last]
                  (let [msg (json/parse-string (str data) true)
                        msg-type (:type msg)]
                    (case msg-type
                      "init"
                      (doseq [p (:recent-posts msg)]
                        (mark-seen! (:post/id p)))

                      "post-created"
                      (handle-post (:post msg))

                      nil)))
    :on-close (fn [_ws code reason]
                (println "Disconnected:" code reason)
                (System/exit 0))
    :on-error (fn [_ws err]
                (binding [*out* *err*]
                  (println "WebSocket error:" err)))}))

;; keep alive
(loop []
  (Thread/sleep 1000)
  (recur))
