(ns futon3.forum.http
  "HTTP + WebSocket API for Forum service."
  (:require [cheshire.core :as json]
            [clj-http.client :as http-client]
            [clojure.string :as str]
            [futon3.forum.service :as forum]
            [org.httpkit.server :as http])
  (:import (java.time Instant)))

;; Agency server URL (default localhost, override via env)
(def ^:private agency-server
  (or (System/getenv "FORUM_AGENCY_SERVER") "http://localhost:7070"))

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

(defn- parse-bool [value]
  (when value
    (contains? #{"true" "1" "yes" "y"} (str/lower-case (str value)))))

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
        limit (some-> (get params "limit") Integer/parseInt)
        pinned-param (get params "pinned")
        pinned? (when (some? pinned-param) (parse-bool pinned-param))]
    (json-response 200
      {:ok true
       :threads (forum/list-threads
                  :tag tag
                  :author author
                  :status status
                  :pinned? pinned?
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

(defn handle-pin-thread [thread-id request]
  (let [payload (parse-json-body request)
        pinned? (parse-bool (get payload :pinned?))]
    (if (nil? pinned?)
      (json-response 400 {:ok false :err "missing-or-invalid-pinned"})
      (let [result (forum/set-thread-pinned! thread-id pinned?)]
        (json-response (if (:ok result) 200 404) result)))))

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
;; Front page endpoint
;; =============================================================================

(defn handle-frontpage [request]
  (let [params (or (:query-params request) {})
        tag (some-> (get params "tag") keyword)
        limit (some-> (get params "limit") Integer/parseInt)]
    (json-response 200
      {:ok true
       :posts (forum/recent-posts
               :tag tag
               :limit (or limit 20))})))

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
;; Agency dispatch endpoint
;; =============================================================================

(def ^:private forum-server
  (or (System/getenv "FORUM_PUBLIC_SERVER") "http://localhost:5050"))

(defn handle-dispatch
  "Dispatch a task to Agency and configure it to reply back to this thread.
   POST /forum/thread/:id/dispatch
   Body: {peripheral, prompt, agent-id, ?musn, ?cwd, ?approval-policy}"
  [thread-id request]
  (if-not (forum/get-thread thread-id)
    (json-response 404 {:ok false :err "thread-not-found"})
    (let [payload (parse-json-body request)
          {:keys [peripheral prompt agent-id musn cwd approval-policy no-sandbox]} payload
          token (get-in request [:headers "x-agency-token"])]
      (if (or (str/blank? peripheral) (str/blank? prompt) (str/blank? agent-id))
        (json-response 400 {:ok false :err "missing-required-fields"
                            :required [:peripheral :prompt :agent-id]})
        (try
          ;; Call Agency with forum callback config
          (let [agency-payload {:agent-id agent-id
                                :peripheral peripheral
                                :prompt prompt
                                :musn musn
                                :cwd cwd
                                :approval-policy approval-policy
                                :no-sandbox no-sandbox
                                :forum {:server forum-server
                                        :thread-id thread-id
                                        :token token
                                        :author agent-id}}
                resp (http-client/post (str agency-server "/agency/run")
                       {:content-type :json
                        :body (json/encode agency-payload)
                        :throw-exceptions false
                        :as :json})]
            (if (= 200 (:status resp))
              (json-response 200 {:ok true
                                  :dispatched true
                                  :agency-response (:body resp)})
              (json-response 502 {:ok false
                                  :err "agency-call-failed"
                                  :status (:status resp)
                                  :detail (:body resp)})))
          (catch Exception e
            (json-response 500 {:ok false
                                :err "dispatch-failed"
                                :detail (.getMessage e)})))))))

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

      (and (= method :post) (re-matches #"/forum/thread/[^/]+/pin" uri))
      (let [thread-id (second (re-find #"/forum/thread/([^/]+)/pin" uri))]
        (handle-pin-thread thread-id request))

      ;; Dispatch to Agency
      (and (= method :post) (re-matches #"/forum/thread/[^/]+/dispatch" uri))
      (let [thread-id (second (re-find #"/forum/thread/([^/]+)/dispatch" uri))]
        (handle-dispatch thread-id request))

      ;; Post operations
      (and (= method :get) (re-matches #"/forum/post/[^/]+" uri))
      (let [post-id (last (str/split uri #"/"))]
        (handle-get-post post-id))

      ;; WebSocket stream
      (and (= method :get) (= uri "/forum/stream/ws"))
      (handle-forum-stream-ws request)

      ;; Front page
      (and (= method :get) (= uri "/forum/frontpage"))
      (handle-frontpage request)

      ;; Analytics
      (and (= method :get) (re-matches #"/forum/thread/[^/]+/patterns" uri))
      (let [thread-id (second (re-find #"/forum/thread/([^/]+)/patterns" uri))]
        (handle-thread-patterns thread-id))

      (and (= method :get) (= uri "/forum/patterns/successful"))
      (handle-successful-patterns request)

      ;; Not found
      :else nil)))
