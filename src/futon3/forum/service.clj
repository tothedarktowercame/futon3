(ns futon3.forum.service
  "Forum service - collaborative proof trees for multi-agent communication.

   Posts are proof steps, links (replies) are inference rules (patterns).
   Threads form proof trees toward goals."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util UUID)
           (java.time Instant)))

;; =============================================================================
;; State
;; =============================================================================

(defonce threads (atom {}))   ;; thread-id -> thread
(defonce posts (atom {}))     ;; post-id -> post
(defonce subscribers (atom {})) ;; channel -> filter-opts
(defonce event-handlers (atom #{})) ;; set of handler fns for Java-WebSocket bridge

;; Forward declarations
(declare notify-subscribers!)

(def forum-dir (io/file "lab/forum"))

(defn- ensure-dir! []
  (when-not (.exists forum-dir)
    (.mkdirs forum-dir)))

;; =============================================================================
;; IDs and timestamps
;; =============================================================================

(defn- gen-id [prefix]
  (str prefix "-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn- now-iso []
  (str (Instant/now)))

;; =============================================================================
;; Persistence
;; =============================================================================

(defn- threads-file [] (io/file forum-dir "threads.edn"))
(defn- posts-file [] (io/file forum-dir "posts.edn"))

(defn persist! []
  (ensure-dir!)
  (spit (threads-file) (pr-str @threads))
  (spit (posts-file) (pr-str @posts)))

(defn load-state! []
  (ensure-dir!)
  (when (.exists (threads-file))
    (reset! threads (read-string (slurp (threads-file)))))
  (when (.exists (posts-file))
    (reset! posts (read-string (slurp (posts-file))))))

;; =============================================================================
;; Thread operations
;; =============================================================================

(defn- normalize-tag [tag]
  (cond
    (keyword? tag) tag
    (string? tag) (keyword tag)
    (nil? tag) nil
    :else (keyword (str tag))))

(defn- normalize-tags [tags]
  (when (seq tags)
    (->> tags
         (map normalize-tag)
         (remove nil?)
         vec)))

(defn- tag->str [tag]
  (cond
    (keyword? tag) (subs (str tag) 1)
    (string? tag) tag
    (nil? tag) ""
    :else (str tag)))

(defn- tag-match? [tag tags]
  (let [needle (tag->str tag)]
    (some #(= needle (tag->str %)) tags)))

(defn create-thread!
  "Create a new thread (proof tree root).

   Options:
   - :title - thread title (required)
   - :author - agent id (required)
   - :body - initial post body (required)
   - :goal - optional goal/theorem this thread addresses
   - :tags - optional vector of keyword tags
   - :pinned? - optional boolean to pin thread"
  [{:keys [title author body goal tags pinned?]}]
  (let [thread-id (gen-id "t")
        post-id (gen-id "p")
        now (now-iso)
        tags (normalize-tags tags)
        post {:post/id post-id
              :post/thread-id thread-id
              :post/author author
              :post/timestamp now
              :post/body body
              :post/claim-type :goal
              :post/pattern-applied nil
              :post/in-reply-to nil
              :post/tags (or tags [])}
        thread {:thread/id thread-id
                :thread/title title
                :thread/author author
                :thread/created now
                :thread/updated now
                :thread/goal goal
                :thread/root-post-id post-id
                :thread/post-count 1
                :thread/participants #{author}
                :thread/tags (or tags [])
                :thread/pinned? (boolean pinned?)
                :thread/status :open}]
    (swap! threads assoc thread-id thread)
    (swap! posts assoc post-id post)
    (persist!)
    (notify-subscribers! {:type :thread-created :thread thread :post post})
    {:ok true :thread thread :post post}))

(defn get-thread [thread-id]
  (get @threads thread-id))

(defn list-threads
  "List threads, optionally filtered.

   Options:
   - :tag - filter by tag (string or keyword)
   - :author - filter by author
   - :status - filter by status (:open, :closed)
   - :pinned? - filter by pinned boolean
   - :limit - max results (default 50)"
  [& {:keys [tag author status pinned? limit] :or {limit 50}}]
  (let [all (vals @threads)
        filtered (cond->> all
                   tag (filter #(tag-match? tag (:thread/tags %)))
                   author (filter #(= author (:thread/author %)))
                   status (filter #(= status (:thread/status %)))
                   (some? pinned?) (filter #(= (boolean pinned?) (boolean (:thread/pinned? %)))))]
    (if (some? pinned?)
      (->> filtered
           (sort-by :thread/updated)
           reverse
           (take limit)
           vec)
      (let [sorted (->> filtered (sort-by :thread/updated) reverse)
            pinned (filter :thread/pinned? sorted)
            unpinned (remove :thread/pinned? sorted)]
        (->> (concat pinned unpinned)
             (take limit)
             vec)))))

(defn set-thread-pinned!
  "Set pinned state for a thread. Does not modify :thread/updated." 
  [thread-id pinned?]
  (if-let [thread (get-thread thread-id)]
    (let [updated (assoc thread :thread/pinned? (boolean pinned?))]
      (swap! threads assoc thread-id updated)
      (persist!)
      (notify-subscribers! {:type :thread-pinned :thread updated})
      {:ok true :thread updated})
    {:ok false :err "thread-not-found"}))

;; =============================================================================
;; Post operations
;; =============================================================================

(defn create-post!
  "Create a reply post (proof step).

   Options:
   - :thread-id - thread to post in (required)
   - :author - agent id (required)
   - :body - post content (required)
   - :in-reply-to - post id this replies to (optional, defaults to root)
   - :pattern-applied - pattern used as inference rule (optional)
   - :claim-type - :step, :evidence, :conclusion, :question (default :step)
   - :tags - optional vector of keyword tags"
  [{:keys [thread-id author body in-reply-to pattern-applied claim-type tags]}]
  (if-let [thread (get-thread thread-id)]
    (let [post-id (gen-id "p")
          now (now-iso)
          tags (normalize-tags tags)
          reply-to (or in-reply-to (:thread/root-post-id thread))
          post {:post/id post-id
                :post/thread-id thread-id
                :post/author author
                :post/timestamp now
                :post/body body
                :post/claim-type (or claim-type :step)
                :post/pattern-applied pattern-applied
                :post/in-reply-to reply-to
                :post/tags (or tags [])}]
      (swap! posts assoc post-id post)
      (swap! threads update thread-id
             (fn [t]
               (-> t
                   (assoc :thread/updated now)
                   (update :thread/post-count inc)
                   (update :thread/participants conj author))))
      (persist!)
      (notify-subscribers! {:type :post-created :thread-id thread-id :post post})
      {:ok true :post post})
    {:ok false :err "thread-not-found"}))

(defn get-post [post-id]
  (get @posts post-id))

(defn get-thread-posts
  "Get all posts in a thread, ordered by timestamp."
  [thread-id]
  (->> (vals @posts)
       (filter #(= thread-id (:post/thread-id %)))
       (sort-by :post/timestamp)
       vec))

(defn recent-posts
  "Return recent posts across all threads.

   Options:
   - :limit - max results (default 20)
   - :tag - filter by tag (matches post tags or thread tags)"
  [& {:keys [limit tag] :or {limit 20}}]
  (let [all (vals @posts)
        tagged (if tag
                 (filter (fn [post]
                           (let [ptags (:post/tags post)
                                 thread (get-thread (:post/thread-id post))
                                 ttags (:thread/tags thread)]
                             (or (tag-match? tag ptags)
                                 (tag-match? tag ttags))))
                         all)
                 all)]
    (->> tagged
         (sort-by :post/timestamp)
         reverse
         (take limit)
         vec)))

(defn get-post-tree
  "Get posts as a tree structure (for proof tree visualization)."
  [thread-id]
  (let [thread-posts (get-thread-posts thread-id)
        by-parent (group-by :post/in-reply-to thread-posts)
        build-tree (fn build-tree [post-id]
                     (let [post (get-post post-id)
                           children (get by-parent post-id [])]
                       (assoc post :children (mapv #(build-tree (:post/id %)) children))))]
    (when-let [thread (get-thread thread-id)]
      (build-tree (:thread/root-post-id thread)))))

;; =============================================================================
;; Subscriptions (for WebSocket streaming)
;; =============================================================================

(defn subscribe! [channel & {:keys [thread-id author tag]}]
  (swap! subscribers assoc channel {:thread-id thread-id :author author :tag tag}))

(defn unsubscribe! [channel]
  (swap! subscribers dissoc channel))

(defn- matches-filter? [event {:keys [thread-id author tag]}]
  (and (or (nil? thread-id) (= thread-id (:thread-id event)))
       (or (nil? author) (= author (get-in event [:post :post/author])))
       (or (nil? tag) (some #{tag} (get-in event [:post :post/tags])))))

(defn notify-subscribers! [event]
  ;; Notify http-kit WebSocket subscribers
  (doseq [[channel filter-opts] @subscribers]
    (when (matches-filter? event filter-opts)
      (try
        ;; Channel send is injected by http layer (as map with :send-fn key)
        (when-let [send-fn (or (:send-fn channel) (:send-fn (meta channel)))]
          (send-fn event))
        (catch Exception _ nil))))
  ;; Notify Java-WebSocket event handlers
  (doseq [handler @event-handlers]
    (try
      (handler event)
      (catch Exception _ nil))))

(defn register-event-handler!
  "Register a handler fn to receive all forum events (for Java-WebSocket bridge)."
  [handler-fn]
  (swap! event-handlers conj handler-fn))

(defn unregister-event-handler!
  "Unregister an event handler."
  [handler-fn]
  (swap! event-handlers disj handler-fn))

;; =============================================================================
;; MUSN integration
;; =============================================================================

(defn post->musn-event
  "Convert a post to MUSN event format."
  [post]
  {:event/type :forum/post
   :post/id (:post/id post)
   :post/thread-id (:post/thread-id post)
   :post/author (:post/author post)
   :post/body (:post/body post)
   :post/pattern-applied (:post/pattern-applied post)
   :post/claim-type (:post/claim-type post)
   :post/tags (:post/tags post)
   :timestamp (:post/timestamp post)})

;; =============================================================================
;; Analytics / Pattern mining
;; =============================================================================

(defn thread-patterns
  "Extract patterns used in a thread (for mining)."
  [thread-id]
  (->> (get-thread-posts thread-id)
       (map :post/pattern-applied)
       (filter some?)
       frequencies))

(defn successful-thread-patterns
  "Get patterns from threads marked as successful/closed."
  []
  (->> (list-threads :status :closed)
       (mapcat #(keys (thread-patterns (:thread/id %))))
       frequencies))

;; =============================================================================
;; Init
;; =============================================================================

(defn init! []
  (load-state!)
  (println "[forum] Loaded" (count @threads) "threads," (count @posts) "posts"))
