(ns futon3.hx.store
  "In-memory hypertext state with append-only log + replay."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-log-path "futon3/logs/hypertext.edn")

(defonce ^:private log-path (atom default-log-path))
(def ^:private lock (Object.))

(defonce artifacts (atom {}))
(defonce anchors (atom {}))
(defonce links (atom {}))

(defn set-log-path!
  "Override the hypertext log path (mainly for tests)."
  [path]
  (reset! log-path path))

(defn current-log-path []
  @log-path)

(defn reset-state!
  "Clear in-memory state."
  []
  (reset! artifacts {})
  (reset! anchors {})
  (reset! links {})
  nil)

(defn state []
  {:artifacts @artifacts
   :anchors @anchors
   :links @links})

(defn- ensure-log-file! []
  (let [file (io/file @log-path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    file))

(defn- append-log! [entry]
  (locking lock
    (let [file (ensure-log-file!)]
      (spit file (str (pr-str entry) "\n") :append true)
      entry)))

(defn- apply-entry! [entry]
  (case (:op entry)
    :artifact/register
    (when-let [artifact (:artifact entry)]
      (swap! artifacts assoc (:artifact/id artifact) artifact))

    :anchors/replace
    (when-let [artifact-id (:artifact/id entry)]
      (swap! anchors assoc artifact-id (vec (:anchors entry))))

    :link/suggest
    (when-let [link (:link entry)]
      (swap! links assoc (:link/id link) link))

    :link/accept
    (when-let [link-id (:link/id entry)]
      (let [update-map (cond-> {:link/status :accepted
                                :link/decided (:link/decided entry)
                                :link/decided-by (:link/decided-by entry)
                                :link/reason (:link/reason entry)}
                         (:link/validation entry)
                         (assoc :link/validation (:link/validation entry)))]
        (swap! links update link-id merge update-map)))

    :link/reject
    (when-let [link-id (:link/id entry)]
      (let [update-map (cond-> {:link/status :rejected
                                :link/decided (:link/decided entry)
                                :link/decided-by (:link/decided-by entry)
                                :link/reason (:link/reason entry)}
                         (:link/validation entry)
                         (assoc :link/validation (:link/validation entry)))]
        (swap! links update link-id merge update-map)))

    nil)
  entry)

(defn append-entry!
  "Apply ENTRY to in-memory state and append to the log."
  [entry]
  (apply-entry! entry)
  (append-log! entry))

(defn load-log!
  "Replay the log file into memory. Returns {:ok true :count n}.
   Pass {:reset? false} to keep existing state before replay."
  ([] (load-log! {:reset? true}))
  ([{:keys [reset?] :or {reset? true}}]
   (when reset?
     (reset-state!))
   (let [file (io/file @log-path)]
     (if-not (.exists file)
       {:ok true :count 0 :path @log-path}
       (with-open [r (io/reader file)]
         (let [entries (->> (line-seq r)
                            (map str/trim)
                            (remove str/blank?)
                            (map edn/read-string)
                            doall)]
           (doseq [entry entries]
             (apply-entry! entry))
           {:ok true :count (count entries) :path @log-path}))))))
