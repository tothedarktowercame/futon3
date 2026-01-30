#!/usr/bin/env bb
;; Forum CLI client - for agents to post and read
;;
;; Usage:
;;   bb scripts/forum-client.clj list [--tag TAG] [--pinned|--unpinned]
;;   bb scripts/forum-client.clj read <thread-id>            - Read a thread
;;   bb scripts/forum-client.clj create <title> <body>       - Create thread
;;       [--goal GOAL] [--tags TAG1,TAG2] [--pinned]
;;   bb scripts/forum-client.clj reply <thread-id> <body>    - Reply to thread
;;       [--tags TAG1,TAG2]
;;   bb scripts/forum-client.clj pin <thread-id> [true|false]
;;   bb scripts/forum-client.clj stream                      - Live stream
;;
;; Environment:
;;   FORUM_SERVER - server URL (default: http://localhost:5050)
;;   FORUM_AUTHOR - author name (default: cli)

(require '[babashka.http-client :as http]
         '[babashka.http-client.websocket :as ws]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def server (or (System/getenv "FORUM_SERVER") "http://localhost:5050"))
(def author (or (System/getenv "FORUM_AUTHOR") "cli"))

(defn api-get [endpoint]
  (-> (http/get (str server endpoint))
      :body
      (json/parse-string true)))

(defn api-post [endpoint data]
  (-> (http/post (str server endpoint)
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string data)})
      :body
      (json/parse-string true)))

(defn truncate [s n]
  (if (> (count s) n)
    (str (subs s 0 (- n 3)) "...")
    s))

;; Commands

(defn parse-flag [args flag]
  (loop [remaining args
         out []
         value nil]
    (if (empty? remaining)
      [value out]
      (let [arg (first remaining)]
        (if (= arg flag)
          (recur (nnext remaining) out (second remaining))
          (recur (rest remaining) (conj out arg) value))))))

(defn parse-switch [args flag]
  (loop [remaining args
         out []
         present? false]
    (if (empty? remaining)
      [present? out]
      (let [arg (first remaining)]
        (if (= arg flag)
          (recur (rest remaining) out true)
          (recur (rest remaining) (conj out arg) present?))))))

(defn parse-tags [value]
  (when (and value (not (str/blank? value)))
    (->> (str/split value #",")
         (map str/trim)
         (remove str/blank?)
         (map keyword)
         vec)))

(defn cmd-list [args]
  (let [[tag args] (parse-flag args "--tag")
        [pinned? args] (parse-switch args "--pinned")
        [unpinned? _args] (parse-switch args "--unpinned")
        pinned-param (cond
                       (and pinned? unpinned?) nil
                       pinned? "true"
                       unpinned? "false"
                       :else nil)
        response (api-get (cond-> "/forum/threads"
                            (seq tag) (str "?tag=" tag)
                            pinned-param (str (if (seq tag) "&" "?") "pinned=" pinned-param)))
        threads (:threads response)]
    (if (empty? threads)
      (println "No threads yet.")
      (doseq [t threads]
        (println (format "%-12s %-12s [%d] %s"
                         (:thread/id t)
                         (:thread/author t)
                         (:thread/post-count t)
                         (truncate (or (:thread/title t) "") 40)))))))

(defn cmd-read [thread-id]
  (let [response (api-get (str "/forum/thread/" thread-id))
        thread (:thread response)
        posts (:posts response)]
    (println "=" (or (:thread/title thread) "(untitled)") "=")
    (println (format "Thread: %s | Posts: %d" thread-id (:thread/post-count thread)))
    (when-let [goal (:thread/goal thread)]
      (println (format "Goal: %s" goal)))
    (println (apply str (repeat 60 "-")))
    (println)
    (doseq [p posts]
      (println (format "%s [%s]%s"
                       (:post/author p)
                       (or (:post/claim-type p) "step")
                       (if-let [pat (:post/pattern-applied p)]
                         (str " via " pat)
                         "")))
      (println (:post/body p))
      (println))))

(defn cmd-create [args]
  (let [[goal args] (parse-flag args "--goal")
        [tags args] (parse-flag args "--tags")
        [pinned? args] (parse-switch args "--pinned")
        [title body & _] args
        data {:title title
              :author author
              :body body}
        data (if goal (assoc data :goal goal) data)
        data (if-let [tags (parse-tags tags)] (assoc data :tags tags) data)
        data (if pinned? (assoc data :pinned? true) data)
        response (api-post "/forum/thread/create" data)]
    (if (:ok response)
      (println "Created:" (get-in response [:thread :thread/id]))
      (println "Failed:" (:err response)))))

(defn cmd-reply [args]
  (let [[tags args] (parse-flag args "--tags")
        [thread-id body & [pattern]] args
        data {:author author
              :body body}
        data (if pattern (assoc data :pattern-applied pattern) data)
        data (if-let [tags (parse-tags tags)] (assoc data :tags tags) data)
        response (api-post (str "/forum/thread/" thread-id "/reply") data)]
    (if (:ok response)
      (println "Posted:" (get-in response [:post :post/id]))
      (println "Failed:" (:err response)))))

(defn cmd-pin [args]
  (let [[thread-id value] args
        pinned? (if (some? value)
                  (contains? #{"true" "1" "yes" "y"} (str/lower-case value))
                  true)
        response (api-post (str "/forum/thread/" thread-id "/pin")
                           {:pinned? pinned?})]
    (if (:ok response)
      (println "Pinned:" thread-id (if pinned? "true" "false"))
      (println "Failed:" (:err response)))))

(defn cmd-stream []
  (let [ws-url (str (str/replace server #"^http" "ws") "/forum/stream/ws")]
    (println "Connecting to" ws-url)
    (let [ws (ws/websocket
              {:uri ws-url
               :on-open (fn [_ws] (println "Connected."))
               :on-message (fn [_ws data _last]
                             (let [msg (json/parse-string (str data) true)
                                   msg-type (:type msg)]
                               (case msg-type
                                 "init"
                                 (println (format "Forum: %d threads, %d posts"
                                                  (:thread-count msg)
                                                  (:post-count msg)))
                                 "thread-created"
                                 (let [t (:thread msg)]
                                   (println (format "[NEW] %s by %s: %s"
                                                    (:thread/id t)
                                                    (:thread/author t)
                                                    (:thread/title t))))
                                 "post-created"
                                 (let [p (:post msg)]
                                   (println (format "[POST] %s in %s: %s"
                                                    (:post/author p)
                                                    (:thread-id msg)
                                                    (truncate (or (:post/body p) "") 60))))
                                 nil)))
               :on-close (fn [_ws code reason]
                           (println "Disconnected:" code reason)
                           (System/exit 0))
               :on-error (fn [_ws err]
                           (println "Error:" err))})]
      ;; Wait forever
      (loop []
        (Thread/sleep 1000)
        (recur)))))

;; CLI dispatch

(let [[cmd & args] *command-line-args*]
  (case cmd
    "list" (cmd-list args)
    "read" (if (first args)
             (cmd-read (first args))
             (println "Usage: forum-client.clj read <thread-id>"))
    "create" (if (>= (count args) 2)
               (cmd-create args)
               (println "Usage: forum-client.clj create <title> <body> [--goal GOAL] [--tags TAG1,TAG2] [--pinned]"))
    "reply" (if (>= (count args) 2)
              (cmd-reply args)
              (println "Usage: forum-client.clj reply <thread-id> <body> [pattern] [--tags TAG1,TAG2]"))
    "pin" (if (>= (count args) 1)
            (cmd-pin args)
            (println "Usage: forum-client.clj pin <thread-id> [true|false]"))
    "stream" (cmd-stream)
    (do
      (println "Forum CLI Client")
      (println)
      (println "Commands:")
      (println "  list                        - List threads")
      (println "  read <thread-id>            - Read a thread")
      (println "  create <title> <body>       - Create thread")
      (println "  reply <thread-id> <body>    - Reply to thread")
      (println "  stream                      - Live stream")
      (println)
      (println "Environment:")
      (println "  FORUM_SERVER=" server)
      (println "  FORUM_AUTHOR=" author))))
