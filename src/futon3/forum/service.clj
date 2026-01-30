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

(defn create-thread!
  "Create a new thread (proof tree root).

   Options:
   - :title - thread title (required)
   - :author - agent id (required)
   - :body - initial post body (required)
   - :goal - optional goal/theorem this thread addresses
   - :tags - optional vector of keyword tags"
  [{:keys [title author body goal tags]}]
  (let [thread-id (gen-id "t")
        post-id (gen-id "p")
        now (now-iso)
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
   - :tag - filter by tag
   - :author - filter by author
   - :status - filter by status (:open, :closed)
   - :limit - max results (default 50)"
  [& {:keys [tag author status limit] :or {limit 50}}]
  (let [all (vals @threads)
        filtered (cond->> all
                   tag (filter #(some #{tag} (:thread/tags %)))
                   author (filter #(= author (:thread/author %)))
                   status (filter #(= status (:thread/status %))))]
    (->> filtered
         (sort-by :thread/updated)
         reverse
         (take limit)
         vec)))

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
                             (or (some #{tag} ptags)
                                 (some #{tag} ttags))))
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
  (doseq [[channel filter-opts] @subscribers]
    (when (matches-filter? event filter-opts)
      (try
        ;; Channel send is injected by http layer
        (when-let [send-fn (:send-fn (meta channel))]
          (send-fn event))
        (catch Exception _ nil)))))

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
