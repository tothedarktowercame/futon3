(ns futon3.forum.http
  "HTTP + WebSocket API for Forum service."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3.forum.service :as forum]
            [org.httpkit.server :as http])
  (:import (java.time Instant)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"}
   :body (json/encode body)})

(defn- parse-json-body [request]
  (try
    (when-let [body (:body request)]
      (let [s (if (string? body) body (slurp body))]
        (when (seq s)
          (json/parse-string s true))))
    (catch Exception _ nil)))

(defn- now-iso []
  (str (Instant/now)))

;; =============================================================================
;; Thread endpoints
;; =============================================================================

(defn handle-create-thread [request]
  (let [payload (parse-json-body request)]
    (if (and (:title payload) (:author payload) (:body payload))
      (let [result (forum/create-thread! payload)]
        (json-response (if (:ok result) 201 400) result))
      (json-response 400 {:ok false :err "missing-required-fields"
                          :required [:title :author :body]}))))

(defn handle-list-threads [request]
  (let [params (or (:query-params request) {})
        tag (some-> (get params "tag") keyword)
        author (get params "author")
        status (some-> (get params "status") keyword)
        limit (some-> (get params "limit") Integer/parseInt)]
    (json-response 200
      {:ok true
       :threads (forum/list-threads
                  :tag tag
                  :author author
                  :status status
                  :limit (or limit 50))})))

(defn handle-get-thread [thread-id]
  (if-let [thread (forum/get-thread thread-id)]
    (json-response 200 {:ok true :thread thread
                        :posts (forum/get-thread-posts thread-id)})
    (json-response 404 {:ok false :err "thread-not-found"})))

(defn handle-get-thread-tree [thread-id]
  (if-let [tree (forum/get-post-tree thread-id)]
    (json-response 200 {:ok true :tree tree})
    (json-response 404 {:ok false :err "thread-not-found"})))

;; =============================================================================
;; Post endpoints
;; =============================================================================

(defn handle-create-post [thread-id request]
  (let [payload (parse-json-body request)]
    (if (and (:author payload) (:body payload))
      (let [result (forum/create-post! (assoc payload :thread-id thread-id))]
        (json-response (if (:ok result) 201 400) result))
      (json-response 400 {:ok false :err "missing-required-fields"
                          :required [:author :body]}))))

(defn handle-get-post [post-id]
  (if-let [post (forum/get-post post-id)]
    (json-response 200 {:ok true :post post})
    (json-response 404 {:ok false :err "post-not-found"})))

;; =============================================================================
;; WebSocket streaming
;; =============================================================================

(defn handle-forum-stream-ws [request]
  (let [params (or (:query-params request) {})
        thread-id (get params "thread-id")
        author (get params "author")
        tag (some-> (get params "tag") keyword)]
    (http/with-channel request channel
      ;; Create a wrapped channel with send capability
      (let [send-fn (fn [event]
                      (when (http/open? channel)
                        (http/send! channel (json/encode event) false)))
            wrapped-channel (with-meta channel {:send-fn send-fn})]

        ;; Subscribe
        (forum/subscribe! wrapped-channel
                          :thread-id thread-id
                          :author author
                          :tag tag)

        ;; Send init with recent activity
        (let [recent-threads (forum/list-threads :limit 10)
              recent-posts (->> recent-threads
                               (mapcat #(forum/get-thread-posts (:thread/id %)))
                               (sort-by :post/timestamp)
                               (take-last 20))]
          (http/send! channel
            (json/encode {:type "init"
                          :thread-count (count @forum/threads)
                          :post-count (count @forum/posts)
                          :recent-posts (vec recent-posts)})
            false))

        ;; Handle client messages
        (http/on-receive channel
          (fn [raw]
            (try
              (let [msg (json/parse-string raw true)]
                (case (:type msg)
                  "ping" (http/send! channel
                           (json/encode {:type "pong" :at (now-iso)}) false)
                  "filter" (forum/subscribe! wrapped-channel
                             :thread-id (:thread-id msg)
                             :author (:author msg)
                             :tag (some-> (:tag msg) keyword))
                  nil))
              (catch Exception _ nil))))

        ;; Handle close
        (http/on-close channel
          (fn [_status]
            (forum/unsubscribe! wrapped-channel)))))))

;; =============================================================================
;; Analytics endpoints
;; =============================================================================

(defn handle-thread-patterns [thread-id]
  (if (forum/get-thread thread-id)
    (json-response 200 {:ok true :patterns (forum/thread-patterns thread-id)})
    (json-response 404 {:ok false :err "thread-not-found"})))

(defn handle-successful-patterns [_request]
  (json-response 200 {:ok true :patterns (forum/successful-thread-patterns)}))

;; =============================================================================
;; Router
;; =============================================================================

(defn route [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      ;; Thread operations
      (and (= method :post) (= uri "/forum/thread/create"))
      (handle-create-thread request)

      (and (= method :get) (= uri "/forum/threads"))
      (handle-list-threads request)

      (and (= method :get) (re-matches #"/forum/thread/[^/]+" uri))
      (let [thread-id (last (str/split uri #"/"))]
        (handle-get-thread thread-id))

      (and (= method :get) (re-matches #"/forum/thread/[^/]+/tree" uri))
      (let [thread-id (second (re-find #"/forum/thread/([^/]+)/tree" uri))]
        (handle-get-thread-tree thread-id))

      (and (= method :post) (re-matches #"/forum/thread/[^/]+/reply" uri))
      (let [thread-id (second (re-find #"/forum/thread/([^/]+)/reply" uri))]
        (handle-create-post thread-id request))

      ;; Post operations
      (and (= method :get) (re-matches #"/forum/post/[^/]+" uri))
      (let [post-id (last (str/split uri #"/"))]
        (handle-get-post post-id))

      ;; WebSocket stream
      (and (= method :get) (= uri "/forum/stream/ws"))
      (handle-forum-stream-ws request)

      ;; Analytics
      (and (= method :get) (re-matches #"/forum/thread/[^/]+/patterns" uri))
      (let [thread-id (second (re-find #"/forum/thread/([^/]+)/patterns" uri))]
        (handle-thread-patterns thread-id))

      (and (= method :get) (= uri "/forum/patterns/successful"))
      (handle-successful-patterns request)

      ;; Not found
      :else nil)))
