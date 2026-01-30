#!/usr/bin/env bb
;; Forum bridge for fuclaude - POLLING version (workaround for WebSocket compression issue)
;;
;; Polls /forum/thread/:id every few seconds instead of WebSocket streaming.
;;
;; Environment:
;;   FORUM_SERVER        - Forum base URL (default: http://localhost:5050)
;;   FORUM_THREAD        - Thread id to watch (required)
;;   FORUM_POLL_INTERVAL - Poll interval in ms (default: 3000)
;;   FORUM_IGNORE_AUTHORS - Comma-separated authors to ignore (default: fuclaude)
;;
;;   AGENCY_SERVER       - Agency base URL (default: http://localhost:7070)
;;   AGENCY_AGENT_ID     - Agent id (default: fuclaude)
;;   AGENCY_PERIPHERAL   - Peripheral name (default: chat)

(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def forum-server (or (System/getenv "FORUM_SERVER") "http://localhost:5050"))
(def forum-thread (System/getenv "FORUM_THREAD"))
(def poll-interval (or (some-> (System/getenv "FORUM_POLL_INTERVAL") Long/parseLong) 3000))
(def agent-id (or (System/getenv "AGENCY_AGENT_ID") "fuclaude"))
(def peripheral (or (System/getenv "AGENCY_PERIPHERAL") "chat"))
(def forum-token (System/getenv "FORUM_TOKEN"))
(def agency-server (or (System/getenv "AGENCY_SERVER") "http://localhost:7070"))

(def ignore-authors
  (let [env-val (System/getenv "FORUM_IGNORE_AUTHORS")]
    (if (str/blank? env-val)
      #{agent-id}
      (set (map str/trim (str/split env-val #","))))))

(when (str/blank? forum-thread)
  (binding [*out* *err*]
    (println "FORUM_THREAD is required"))
  (System/exit 2))

(def seen-path
  (or (System/getenv "FORUM_SEEN_PATH")
      (str (io/file "lab" "agency" (str "forum-seen-fuclaude-poll-" forum-thread ".edn")))))

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

(defn ignored-author? [author]
  (contains? ignore-authors author))

(defn fetch-thread []
  (try
    (let [resp (http/get (str forum-server "/forum/thread/" forum-thread)
                         {:headers {"Accept" "application/json"}})]
      (when (= 200 (:status resp))
        (json/parse-string (:body resp) true)))
    (catch Exception e
      (binding [*out* *err*]
        (println "Fetch failed:" (.getMessage e)))
      nil)))

(defn agency-run! [prompt author]
  (let [payload {:agent-id agent-id
                 :peripheral peripheral
                 :prompt (str "Forum post from " author ":\n\n" prompt)
                 :forum {:server forum-server
                         :thread-id forum-thread
                         :token forum-token
                         :author agent-id}}
        resp (http/post (str (str/replace agency-server #"/+$" "") "/agency/run")
                        {:headers {"Content-Type" "application/json"}
                         :body (json/generate-string payload)})]
    (json/parse-string (:body resp) true)))

(defn process-post [post]
  (let [post-id (:post/id post)
        body (:post/body post)
        author (:post/author post)]
    (when-not (seen? post-id)
      (mark-seen! post-id)
      (cond
        (ignored-author? author)
        (println "  Ignoring post" post-id "from" author "(in ignore list)")

        :else
        (do
          (println "  Dispatching post" post-id "from" author "to Agency")
          (try
            (let [result (agency-run! body author)]
              (println "  Agency response:" (if (:ok result) "ok" (:err result))))
            (catch Exception e
              (binding [*out* *err*]
                (println "  Agency run failed:" (.getMessage e))))))))))

(println "fuclaude forum bridge (polling) starting...")
(println "  Thread:" forum-thread)
(println "  Agent:" agent-id)
(println "  Peripheral:" peripheral)
(println "  Poll interval:" poll-interval "ms")
(println "  Ignoring authors:" ignore-authors)

;; Initial fetch to mark existing posts as seen
(println "Fetching initial state...")
(when-let [data (fetch-thread)]
  (let [posts (:posts data)]
    (println "  Found" (count posts) "existing posts, marking as seen")
    (doseq [p posts]
      (mark-seen! (:post/id p)))))

(println "Polling for new posts...")

(loop []
  (Thread/sleep poll-interval)
  (when-let [data (fetch-thread)]
    (let [posts (:posts data)
          new-posts (remove #(seen? (:post/id %)) posts)]
      (when (seq new-posts)
        (println "Found" (count new-posts) "new post(s)")
        (doseq [p (sort-by :post/timestamp new-posts)]
          (process-post p)))))
  (recur))
