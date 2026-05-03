#!/usr/bin/env bb
;; substrate-2 phase-4.5 multi-repo watcher: poll-based, per-file ingest.
;;
;; Usage:
;;   bb multi_watcher.clj [--root <repo>=<label>]... [--vocab path]...
;;       [--interval-ms 2000] [--max-events N] [--no-cold-scan]
;;
;; Each --root is <repo-path>=<label>; e.g.
;;   --root /home/joe/code/futon2=futon2-d
;;   --root /home/joe/code/futon3=futon3-d
;;
;; Cold scan: on startup, walk every root and ingest every supported file.
;; Watch loop: every --interval-ms (default 2000), walk roots, compute file
;; mtimes, dispatch per-file ingest for any file whose mtime moved forward.
;; Per-cycle heartbeat: emit a `code/v05/watcher-event` hyperedge.
;;
;; Why polling: babashka.fs has no `watch`; inotify-tools and the
;; pod-babashka-fswatcher are not installed; polling is correct and proven
;; in phase 4. Phase 4.6 can swap in inotify if/when the dependency lands.
;;
;; Inside-out invariants (same shape as phase 4):
;;   L4 — supported extension; not under noise paths.
;;   L3 — penholder=api enforced by post.
;;   L2 — ingest_one_file.clj enforces endpoint resolution per file.
;;   L1 — heartbeat IDs include (root, run-id, cycle-n) for uniqueness.
;;   L0 — heartbeat POST must succeed for cycle to count.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :refer [sh]]
         '[clojure.set]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

;; commit_ingest_lib provides ingest-new-commits! for the per-cycle
;; commit-vertex ingestion path. See E-live-means-live (futon3/holes/
;; excursions/) for the design and the reachable-from-boot reasoning.
(load-file (str (fs/parent *file*) "/commit_ingest_lib.clj"))

(import '[java.util.concurrent TimeUnit])

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(def WATCHED-EXTS #{"clj" "cljs" "cljc" "el" "py" "flexiarg" "md"})
(def NOISE-PATTERN
  #"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo|pytest_cache|venv)/|/node_modules/|/target/|/out/|/__pycache__/")

(def INGEST-SCRIPT "/home/joe/code/futon3/scripts/ingest_one_file.clj")
(def VERTEX-TYPES ["code/v05/namespace" "code/v05/var" "code/v05/test"])
(def RENAMED-LINK-TYPE "edge/renamed-to")

;; ---------- HTTP write surface ----------

(defn directed-endpoints [hx-type endpoints]
  (if (and (#{"code/v05/watcher-event" RENAMED-LINK-TYPE} hx-type)
           (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

(defn post-hyperedge!
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))]
    (try
      (let [resp (http/post (str FUTON1A "/api/alpha/hyperedge")
                            {:headers {"Content-Type" "application/json"
                                       "X-Penholder" PENHOLDER}
                             :body (json/generate-string payload)
                             :throw false})]
        {:ok? (= 200 (:status resp))})
      (catch Exception _ {:ok? false}))))

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER}
                            :throw false})]
    (if (= 200 (:status resp))
      (edn/read-string (:body resp))
      (throw (ex-info "HTTP non-200"
                      {:url url :status (:status resp) :body (:body resp)})))))

(defn- type-str [x]
  (cond
    (keyword? x) (if (namespace x)
                   (str (namespace x) "/" (name x))
                   (name x))
    (string? x) x
    :else (str x)))

(defn- prop-get [h k]
  (let [props (:hx/props h)
        ks (cond
             (keyword? k) [k (name k)]
             (string? k) [k (keyword k)]
             :else [k])]
    (some #(when (contains? props %) (get props %)) ks)))

(defn- real-endpoints [h]
  (vec (remove #(str/starts-with? % "dir:") (:hx/endpoints h))))

(defn fetch-of-type
  "Fetch all hyperedges of `hx-type`, filtered to one repo label."
  ([hx-type label]
   (fetch-of-type hx-type label nil))
  ([hx-type label source-file]
   (let [query (cond-> (str FUTON1A "/api/alpha/hyperedges?type="
                            (java.net.URLEncoder/encode hx-type "UTF-8")
                            "&repo="
                            (java.net.URLEncoder/encode label "UTF-8"))
                 source-file
                 (str "&source-file="
                      (java.net.URLEncoder/encode source-file "UTF-8")))
         r (http-get-edn query)]
     (vec (or (:hyperedges r) [])))))

(defn source-file-vertices
  "Return var/test/namespace hyperedges for (label, source-file path)."
  [label path]
  (->> VERTEX-TYPES
       (mapcat #(fetch-of-type % label path))
       vec))

(defn- primary-endpoint [h]
  (first (real-endpoints h)))

(defn- local-name [qname]
  (when (string? qname)
    (let [i (str/last-index-of qname "/")]
      (if (neg? i) qname (subs qname (inc i))))))

(defn- rename-pairing-key [h]
  (let [t (type-str (:hx/type h))]
    (case t
      "code/v05/var"       [t (local-name (prop-get h :var/qname))]
      "code/v05/test"      [t (local-name (prop-get h :test/qname))]
      "code/v05/namespace" [t :namespace]
      [t (primary-endpoint h)])))

(defn deterministic-rename-pairs
  "Pair old and new vertices when the mapping is unambiguous.

   First drop survivors whose stable endpoint is unchanged (same logical
   vertex, just re-upserted at a new source-file). Then pair remaining
   old/new vertices by a conservative per-kind key only when each side has
   exactly one match."
  [old-vertices new-vertices]
  (let [survivor-eps (clojure.set/intersection
                      (set (map primary-endpoint old-vertices))
                      (set (map primary-endpoint new-vertices)))
        old-left (remove #(survivor-eps (primary-endpoint %)) old-vertices)
        new-left (remove #(survivor-eps (primary-endpoint %)) new-vertices)
        old-by-key (group-by rename-pairing-key old-left)
        new-by-key (group-by rename-pairing-key new-left)]
    (vec
     (for [[k olds] old-by-key
           :let [news (get new-by-key k)]
           :when (and (= 1 (count olds)) (= 1 (count news)))]
       {:from (first olds)
        :to   (first news)}))))

(defn reupsert-hyperedge!
  "Write back an existing hyperedge with merged props, preserving labels."
  [h merged-props]
  (post-hyperedge! (type-str (:hx/type h))
                   (real-endpoints h)
                   (:hx/labels h)
                   merged-props))

(defn mark-vertices-stale!
  "Set :edge/witness-stale on each vertex hyperedge and preserve prior props."
  [vertices reason extra-props]
  (let [ts (System/currentTimeMillis)]
    (reduce (fn [acc h]
              (let [merged (merge (or (:hx/props h) {})
                                  {"edge/witness-stale" true
                                   "edge/witness-stale-ts" ts
                                   "edge/witness-stale-reason" reason}
                                  extra-props)
                    ok? (:ok? (reupsert-hyperedge! h merged))]
                (update acc (if ok? :written :failed) inc)))
            {:written 0 :failed 0}
            vertices)))

(defn emit-renamed-link!
  [{:keys [from to label from-path to-path hash from-label to-label]}]
  (post-hyperedge!
   RENAMED-LINK-TYPE
   [(primary-endpoint from) (primary-endpoint to)]
   (cond-> ["phase-4.6" label "renamed-to"]
     (and from-label to-label) (conj "cross-root"))
   (cond-> {"repo" label
            "phase" 4.6
            "from-file" from-path
            "to-file" to-path
            "hash" hash
            "ts" (System/currentTimeMillis)}
     from-label (assoc "from-repo" from-label)
     to-label (assoc "to-repo" to-label))))

;; ---------- file walk ----------

(defn watched? [path]
  (let [norm (str/replace (str path) "\\" "/")
        ext (fs/extension path)
        mission-doc? (boolean (re-find #"/holes/missions/M-[^/]+\.md$" norm))]
    (and (WATCHED-EXTS ext)
         (not (re-find NOISE-PATTERN norm))
         (or (not= ext "md") mission-doc?))))

(defn noise-dir?
  "True when DIR is under a subtree we never want to watch."
  [dir]
  (let [norm (str/replace (str dir) "\\" "/")]
    (boolean (re-find NOISE-PATTERN (str norm "/")))))

(defn file-fingerprint
  "Return the cheap per-file fingerprint used to detect potential changes."
  [^String path]
  (let [f (java.io.File. path)]
    {:mtime (.lastModified f)
     :size (.length f)}))

(defn walk-root
  "Returns map of path → {:mtime ms :size bytes} for watched files."
  [root]
  (->> (file-seq (java.io.File. ^String root))
       (filter #(.isFile ^java.io.File %))
       (map #(.getPath ^java.io.File %))
       (filter watched?)
       (map (fn [p] [p (file-fingerprint p)]))
       (into {})))

;; ---------- B-3 v0: deletion + rename detection ----------

(defn sha-256 [^String path]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bs (try (.readAllBytes (io/input-stream path))
                (catch Exception _ (byte-array 0)))]
    (.update md bs)
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn enriched-snapshot
  "Walk root and return {path → {:mtime ms :size bytes :hash sha}} for all
   watched files. Used to seed cache state so future delete/rename handling
   has a last-known hash for every file."
  [root]
  (->> (walk-root root)
       (map (fn [[p meta]] [p (assoc meta :hash (sha-256 p))]))
       (into {})))

(defn changed-fingerprint?
  "True when the cheap fingerprint moved enough to justify re-hashing."
  [old-meta new-meta]
  (or (nil? old-meta)
      (not= (select-keys old-meta [:mtime :size])
            (select-keys new-meta [:mtime :size]))))

(defn incremental-snapshot
  "Refresh snapshot state without re-hashing unchanged files.

   Unchanged files inherit their cached hash. New or fingerprint-changed files
   are re-hashed so ingest dispatch and rename detection still observe content
   changes exactly."
  [root cache]
  (let [fingerprints (walk-root root)]
    (reduce-kv
     (fn [acc path meta]
       (let [old-meta (get cache path)
             hash (if (changed-fingerprint? old-meta meta)
                    (sha-256 path)
                    (or (:hash old-meta)
                        (sha-256 path)))]
         (assoc acc path (assoc meta :hash hash))))
     {}
     fingerprints)))

(defn detect-moves-and-deletes
  "Given two enriched snapshots {path → {:mtime :hash}}, return:
     :vacated  paths in cache, not in snapshot
     :appeared paths in snapshot, not in cache
     :renamed  [{:from from-path :to to-path :hash h}] — content match
     :deleted  vacated paths whose hash matched no appeared path
     :added    appeared paths whose hash matched no vacated path."
  [cache snapshot]
  (let [vacated  (sort (clojure.set/difference (set (keys cache))
                                                (set (keys snapshot))))
        appeared (sort (clojure.set/difference (set (keys snapshot))
                                                (set (keys cache))))
        ;; index appeared paths by hash for fast lookup
        appeared-by-hash (group-by #(get-in snapshot [% :hash]) appeared)
        vacated-by-hash  (group-by #(get-in cache    [% :hash]) vacated)
        rename-pairs (for [[h v-paths] vacated-by-hash
                           :let [a-paths (get appeared-by-hash h)]
                           :when (and h (seq a-paths))
                           :let [pairs (map vector v-paths a-paths)]
                           [v a] pairs]
                       {:from v :to a :hash h})
        renamed-from (set (map :from rename-pairs))
        renamed-to   (set (map :to rename-pairs))
        deleted (vec (remove renamed-from vacated))
        added   (vec (remove renamed-to appeared))]
    {:vacated (vec vacated)
     :appeared (vec appeared)
     :renamed (vec rename-pairs)
     :deleted deleted
     :added added}))

;; ---------- ingest dispatch ----------

(defn ingest-event!
  "Run per-file ingest for `path` under (root, label). Emit watcher-event
   evidence hyperedge."
  [{:keys [path root label run-id event-n source]}]
  (let [t-start (System/currentTimeMillis)
        {:keys [exit err]}
        (sh "bb" INGEST-SCRIPT path "--root" root "--label" label)
        dur (- (System/currentTimeMillis) t-start)
        evidence-id (str root "/run-" run-id "/event-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.5" label "watcher-event"]
     {"repo" label "phase" 4.5 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis) "file" path "ingest-exit" exit
      "duration-ms" dur "source" source})
    (println (format "[%s] %s exit=%d %dms" source path exit dur))
    (when-not (zero? exit)
      (when err (println "  stderr:" (str/trim err))))))

;; ---------- heartbeat ----------

(defn heartbeat!
  "Emit a per-cycle heartbeat hyperedge per root, even with no fs activity."
  [{:keys [root label run-id cycle-n files-seen files-changed
            n-deleted n-renamed n-added n-cross-root-moves]}]
  (let [evidence-id (str root "/run-" run-id "/heartbeat-" cycle-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str cycle-n)]
     ["v05" "phase-4.5" label "heartbeat"]
     {"repo" label "phase" 4.5 "run-id" run-id "cycle" cycle-n
      "ts" (System/currentTimeMillis) "files-seen" files-seen
      "files-changed" files-changed
      "n-deleted" (or n-deleted 0)
      "n-renamed" (or n-renamed 0)
      "n-added" (or n-added 0)
      "n-cross-root-moves" (or n-cross-root-moves 0)
      "source" "heartbeat"})))

(defn deletion-event!
  "Record a typed deletion event so downstream consumers (especially phase-5
   completion-rot signature emitters) can see what's been removed. v0 does
   NOT yet write :edge/witness-stale on dependent edges (phase 4.6 cascade
   work); the deletion is purely an observation hyperedge."
  [{:keys [path root label run-id event-n hash]}]
  (let [evidence-id (str root "/run-" run-id "/deletion-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.5" label "deletion-event"]
     {"repo" label "phase" 4.5 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis)
      "file" path "last-known-hash" hash
      "source" "deletion"})
    (println (format "[deletion] %s" path))))

(defn rename-event!
  "Record a typed rename event (content-hash match between a vacated and an
   appeared path). The substrate keeps both old and new vertices because of
   no-DELETE; phase 4.6 cascade-staleness will mark the old vertex's edges
   stale and link old→new via :edge/renamed-to."
  [{:keys [from to root label run-id event-n hash]}]
  (let [evidence-id (str root "/run-" run-id "/rename-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.5" label "rename-event"]
     {"repo" label "phase" 4.5 "run-id" run-id "event-n" event-n
      "ts" (System/currentTimeMillis)
      "from" from "to" to "hash" hash
      "source" "rename"})
    (println (format "[rename] %s → %s" from to))))

(defn cross-root-move-event!
  "Record a typed cross-root move event (delete under one watched root,
   add under another, same content hash)."
  [{:keys [from to from-root to-root from-label to-label run-id event-n hash]}]
  (let [evidence-id (str from-root "/run-" run-id "/cross-root-move-" event-n)]
    (post-hyperedge!
     "code/v05/watcher-event"
     [evidence-id (str event-n)]
     ["v05" "phase-4.6" from-label to-label "cross-root-move-event"]
     {"repo" from-label
      "from-repo" from-label
      "to-repo" to-label
      "phase" 4.6
      "run-id" run-id
      "event-n" event-n
      "ts" (System/currentTimeMillis)
      "from" from
      "to" to
      "from-root" from-root
      "to-root" to-root
      "hash" hash
      "source" "cross-root-move"})
    (println (format "[cross-root-move] %s → %s" from to))))

(defn handle-deletion!
  "Mark source-file vertices stale, then record the deletion event."
  [{:keys [path root label run-id event-n hash]}]
  (let [victims (source-file-vertices label path)
        stale (mark-vertices-stale! victims "deletion"
                                    {"edge/witness-stale-source-file" path
                                     "edge/witness-stale-last-known-hash" hash})]
    (deletion-event! {:path path :root root :label label
                      :run-id run-id :event-n event-n :hash hash})
    (println (format "[deletion-stale] %s vertices=%d failed=%d"
                     path (:written stale) (:failed stale)))))

(defn handle-rename!
  "Re-ingest the destination file, stale any non-surviving old vertices,
   and emit deterministic old→new links when they can be paired."
  [{:keys [from to root label run-id event-n hash]}]
  (let [from-path from
        to-path to
        old-vertices (source-file-vertices label from-path)]
    ;; A rename must re-ingest the destination path so same-qname vertices
    ;; update their :source-file, and path-sensitive projectors can materialize
    ;; the new location under the live store.
    (ingest-event! {:path to-path :root root :label label
                    :run-id run-id :event-n event-n
                    :source "rename-ingest"})
    (let [new-vertices (source-file-vertices label to-path)
          survivor-eps (clojure.set/intersection
                        (set (map primary-endpoint old-vertices))
                        (set (map primary-endpoint new-vertices)))
          stale-old (vec (remove #(survivor-eps (primary-endpoint %)) old-vertices))
          stale (mark-vertices-stale! stale-old "rename"
                                      {"edge/witness-stale-source-file" from-path
                                       "edge/witness-stale-renamed-to" to-path
                                       "edge/witness-stale-last-known-hash" hash})
          pairs (deterministic-rename-pairs old-vertices new-vertices)
          link-stats (reduce (fn [acc {:keys [from to]}]
                               (let [from-vertex from
                                     to-vertex to
                                     ok? (:ok? (emit-renamed-link! {:from from-vertex
                                                                    :to to-vertex
                                                                    :label label
                                                                    :from-path from-path
                                                                    :to-path to-path
                                                                    :hash hash}))]
                                 (update acc (if ok? :written :failed) inc)))
                             {:written 0 :failed 0}
                             pairs)]
      (rename-event! {:from from-path :to to-path :hash hash
                      :root root :label label
                      :run-id run-id :event-n event-n})
      (println (format "[rename-cascade] %s → %s stale=%d stale-failed=%d links=%d link-failed=%d"
                       from-path to-path
                       (:written stale) (:failed stale)
                       (:written link-stats) (:failed link-stats))))))

(defn handle-cross-root-move!
  "Re-ingest the destination file under a different watched root, stale the
   old root's file-owned vertices, and emit deterministic old→new links when
   the pairing is unambiguous."
  [{:keys [from to from-root to-root from-label to-label run-id event-n hash]}]
  (let [old-vertices (source-file-vertices from-label from)]
    (ingest-event! {:path to :root to-root :label to-label
                    :run-id run-id :event-n event-n
                    :source "cross-root-move-ingest"})
    (let [new-vertices (source-file-vertices to-label to)
          stale (mark-vertices-stale! old-vertices "cross-root-move"
                                      {"edge/witness-stale-source-file" from
                                       "edge/witness-stale-renamed-to" to
                                       "edge/witness-stale-last-known-hash" hash
                                       "edge/witness-stale-to-repo" to-label})
          pairs (deterministic-rename-pairs old-vertices new-vertices)
          link-stats (reduce (fn [acc {:keys [from to]}]
                               (let [ok? (:ok? (emit-renamed-link! {:from from
                                                                    :to to
                                                                    :label from-label
                                                                    :from-path from
                                                                    :to-path to
                                                                    :hash hash
                                                                    :from-label from-label
                                                                    :to-label to-label}))]
                                 (update acc (if ok? :written :failed) inc)))
                             {:written 0 :failed 0}
                             pairs)]
      (cross-root-move-event! {:from from :to to
                               :from-root from-root :to-root to-root
                               :from-label from-label :to-label to-label
                               :run-id run-id :event-n event-n :hash hash})
      (println (format "[cross-root-cascade] %s → %s stale=%d stale-failed=%d links=%d link-failed=%d"
                       from to
                       (:written stale) (:failed stale)
                       (:written link-stats) (:failed link-stats))))))

(defn detect-cross-root-moves
  "Pair a unique deletion under one watched root with a unique addition under
   another watched root when their content hashes match exactly.

   Conservative by construction: if a hash appears more than once on either
   side in a cycle, no cross-root move is inferred for that hash."
  [plans]
  (let [deleteds (for [{:keys [root label cache moves]} plans
                       path (:deleted moves)
                       :let [hash (get-in cache [path :hash])]
                       :when hash]
                   {:from path
                    :from-root root
                    :from-label label
                    :hash hash})
        addeds (for [{:keys [root label snapshot moves]} plans
                     path (:added moves)
                     :let [hash (get-in snapshot [path :hash])]
                     :when hash]
                 {:to path
                  :to-root root
                  :to-label label
                  :hash hash})
        deleted-by-hash (group-by :hash deleteds)
        added-by-hash (group-by :hash addeds)]
    (vec
     (for [[hash ds] deleted-by-hash
           :let [as (get added-by-hash hash)]
           :when (and (= 1 (count ds))
                      (= 1 (count as))
                      (not= (:from-root (first ds))
                            (:to-root (first as))))]
       (merge (first ds) (first as))))))

;; ---------- watchservice mode ----------

(def ^:private watch-kinds-class
  (delay (Class/forName "java.nio.file.StandardWatchEventKinds")))

(defn watch-kind [field-name]
  (-> @watch-kinds-class (.getField field-name) (.get nil)))

(defn watch-kinds []
  (into-array [(watch-kind "ENTRY_CREATE")
               (watch-kind "ENTRY_DELETE")
               (watch-kind "ENTRY_MODIFY")]))

(defn ensure-watchservice-available!
  []
  (try
    @watch-kinds-class
    true
    (catch Throwable t
      (println "ERROR: watchservice mode is unavailable in this babashka runtime.")
      (println "  cause:" (.getMessage t))
      (println "  use poll mode here, or run a JVM-based watcher replacement.")
      (System/exit 2))))

(defn register-dir!
  [watcher key-index root label dir]
  (when-not (noise-dir? (.toString dir))
    (let [key (.register dir watcher (watch-kinds))]
      (swap! key-index assoc key {:root root :label label :dir dir}))))

(defn register-root-tree!
  [watcher key-index root label scan-root]
  (doseq [dir (->> (file-seq (java.io.File. ^String scan-root))
                   (filter #(.isDirectory ^java.io.File %))
                   (map #(.toPath ^java.io.File %))
                   (remove #(noise-dir? (.toString %))))]
    (register-dir! watcher key-index root label dir)))

(defn process-watch-key!
  [watcher key-index key dirty-roots]
  (let [{:keys [root dir]} (get @key-index key)]
    (doseq [event (.pollEvents key)]
      (let [kind (.kind event)]
        (when root
          (swap! dirty-roots conj root))
        (when (and root (not= kind (watch-kind "OVERFLOW")))
          (let [ctx (.context event)
                full (.resolve dir ctx)
                full-str (.toString full)]
            (when (and (= kind (watch-kind "ENTRY_CREATE"))
                       (not (noise-dir? full-str))
                       (.isDirectory (.toFile full)))
              (register-root-tree! watcher key-index root (:label (get @key-index key)) full-str))))))
    (when-not (.reset key)
      (swap! key-index dissoc key))))

(defn collect-dirty-roots!
  [watcher key-index first-key debounce-ms]
  (let [dirty-roots (atom #{})]
    (process-watch-key! watcher key-index first-key dirty-roots)
    (loop [deadline (+ (System/currentTimeMillis) debounce-ms)]
      (let [remaining (- deadline (System/currentTimeMillis))]
        (if (pos? remaining)
          (if-let [key (.poll watcher remaining TimeUnit/MILLISECONDS)]
            (do
              (process-watch-key! watcher key-index key dirty-roots)
              (recur (+ (System/currentTimeMillis) debounce-ms)))
            @dirty-roots)
          @dirty-roots)))))

;; ---------- cycle driver ----------

(defn build-plan
  [{:keys [path label]} per-root-cache dirty-roots]
        (let [cache (get @per-root-cache path)
        dirty? (contains? dirty-roots path)
        snapshot (cond
                   dirty? (if (seq cache)
                            (incremental-snapshot path cache)
                            (enriched-snapshot path))
                   (seq cache) cache
                   :else {})
        changed (if dirty?
                  (filter (fn [[p {:keys [hash]}]]
                            (not= hash (get-in cache [p :hash])))
                          snapshot)
                  [])
        changed-paths (sort (map first changed))
        moves (if dirty?
                (detect-moves-and-deletes cache snapshot)
                {:vacated [] :appeared [] :renamed [] :deleted [] :added []})
        renamed-to (set (map :to (:renamed moves)))]
    {:root path
     :label label
     :snapshot snapshot
     :cache cache
     :moves moves
     :dirty? dirty?
     :ingest-paths (vec (remove renamed-to changed-paths))}))

;; ---------- commit-vertex live ingestion (E-live-means-live closure path) ----------

(defn query-repo-vars-by-file
  "Queries substrate-2 for all var/test vertices in repo-label, returns
   { rel-path → [unprefixed-qnames…] }. Used to build the file->vars
   function passed to commit-ingest-lib/ingest-new-commits! for the
   per-commit :edits resolution.

   Cost: one HTTP call to /api/alpha/hyperedges per vertex-type per
   repo per cycle. With current scale (~12 repos, types {var,test},
   ~2s interval), that's ~12 calls/cycle. Acceptable for now;
   optimize via file-event-driven cache if it becomes a bottleneck.

   Returns {} on any HTTP failure (caller treats absence as 'no vars';
   :edits emission for that repo's new commits is skipped this cycle
   and retried next cycle)."
  [repo-label]
  (try
    (reduce
     (fn [acc t]
       (let [resp (http-get-edn
                   (str FUTON1A "/api/alpha/hyperedges?type=" t
                        "&repo=" repo-label))
             edges (:hyperedges resp)]
         (reduce (fn [m e]
                   (let [src-file (some-> e :hx/props :source-file)
                         qname (first (:hx/endpoints e))]
                     (cond-> m
                       (and (string? src-file) (string? qname))
                       (update src-file (fnil conj []) qname))))
                 acc edges)))
     {}
     ["code/v05/var" "code/v05/test"])
    (catch Exception _ {})))

(defn ingest-new-commits-for-root!
  "Per-cycle commit-vertex ingestion for one watched root. Queries
   substrate-2 for the latest indexed commit, runs `git rev-list` for
   any newer commits, posts commit/author/authored/precedes/edits
   hyperedges via commit-ingest-lib.

   Failures are caught and logged but do NOT propagate — file-watch
   ingestion must remain healthy even if commit ingestion has a
   transient hiccup."
  [{:keys [root label cycle-n]}]
  (try
    (let [vars-by-file (query-repo-vars-by-file label)
          file->vars (fn [path] (get vars-by-file path))
          report (commit-ingest-lib/ingest-new-commits!
                  {:repo-root root
                   :repo-label label
                   :file->vars file->vars})]
      (when (pos? (:n-ingested report))
        (println (format "[cycle %d] %s: ingested %d new commit(s); latest=%s%s"
                         @cycle-n label
                         (:n-ingested report)
                         (str/join (take 7 (or (:latest-sha report) "")))
                         (if (pos? (:n-failed report))
                           (format " (FAILED=%d)" (:n-failed report))
                           "")))))
    (catch Exception e
      (binding [*out* *err*]
        (println (format "[cycle %d] %s: commit-ingest error: %s"
                         @cycle-n label (.getMessage e)))))))

(defn run-cycle!
  [{:keys [roots per-root-cache cold-scan? max-events run-id event-n cycle-n dirty-roots]}]
  (swap! cycle-n inc)
  (let [plans (vec (map #(build-plan % per-root-cache dirty-roots) roots))
        cross-root-moves (detect-cross-root-moves plans)
        cross-root-froms (set (map :from cross-root-moves))
        cross-root-tos (set (map :to cross-root-moves))]
    (doseq [{:keys [root label snapshot cache moves ingest-paths dirty?]} plans]
      (let [{:keys [renamed deleted added]} moves
            ingest-paths (vec (remove cross-root-tos ingest-paths))
            deleted (vec (remove cross-root-froms deleted))
            cross-root-count (count (filter #(or (= root (:from-root %))
                                                 (= root (:to-root %)))
                                           cross-root-moves))
            dispatch? (and dirty?
                           (or cold-scan? (seq cache))
                           (seq ingest-paths))]
        (when dispatch?
          (println (format "[cycle %d] %s: %d/%d files changed"
                           @cycle-n label (count ingest-paths) (count snapshot)))
          (doseq [p ingest-paths]
            (let [n (swap! event-n inc)]
              (ingest-event! {:path p :root root :label label
                              :run-id run-id :event-n n
                              :source (if (= 1 @cycle-n) "cold-scan" "fs-watch")})
              (when (and max-events (>= n max-events))
                (println "[multi_watcher] reached --max-events; exiting")
                (System/exit 0)))))
        (doseq [p deleted]
          (let [n (swap! event-n inc)]
            (handle-deletion! {:path p :root root :label label
                               :run-id run-id :event-n n
                               :hash (get-in cache [p :hash])})))
        (doseq [{:keys [from to hash]} renamed]
          (let [n (swap! event-n inc)]
            (handle-rename! {:from from :to to :hash hash
                             :root root :label label
                             :run-id run-id :event-n n})))
        (heartbeat! {:root root :label label :run-id run-id
                     :cycle-n @cycle-n
                     :files-seen (count snapshot)
                     :files-changed (count ingest-paths)
                     :n-deleted (count deleted)
                     :n-renamed (count renamed)
                     :n-added (count added)
                     :n-cross-root-moves cross-root-count})
        ;; Per-cycle commit-vertex ingestion (E-live-means-live closure).
        ;; Runs every cycle, every root — queries substrate-2 for the
        ;; latest-indexed sha and ingests any newer commits. Failures
        ;; are isolated; do not break file-watch ingestion.
        (ingest-new-commits-for-root!
         {:root root :label label :cycle-n cycle-n})
        (swap! per-root-cache assoc root snapshot)))
    (doseq [{:keys [from to from-root to-root from-label to-label hash]} cross-root-moves]
      (let [n (swap! event-n inc)]
        (handle-cross-root-move! {:from from :to to
                                  :from-root from-root :to-root to-root
                                  :from-label from-label :to-label to-label
                                  :run-id run-id :event-n n :hash hash})))))

;; ---------- main loop ----------

(defn parse-args [argv]
  (loop [a argv opts {:roots [] :interval-ms 2000 :cold-scan? true
                      :watch-mode "poll" :debounce-ms 250
                      :max-events nil :max-cycles nil}]
    (cond
      (empty? a) opts
      (= "--root" (first a))
      (let [parts (str/split (second a) #"=" 2)]
        (recur (drop 2 a)
               (update opts :roots conj
                       {:path (first parts)
                        :label (or (second parts)
                                   (str (fs/file-name (first parts))))})))
      (= "--interval-ms" (first a)) (recur (drop 2 a) (assoc opts :interval-ms (parse-long (second a))))
      (= "--watch-mode" (first a)) (recur (drop 2 a) (assoc opts :watch-mode (second a)))
      (= "--debounce-ms" (first a)) (recur (drop 2 a) (assoc opts :debounce-ms (parse-long (second a))))
      (= "--max-events" (first a))  (recur (drop 2 a) (assoc opts :max-events (parse-long (second a))))
      (= "--max-cycles" (first a))  (recur (drop 2 a) (assoc opts :max-cycles (parse-long (second a))))
      (= "--no-cold-scan" (first a)) (recur (rest a) (assoc opts :cold-scan? false))
      :else (recur (rest a) opts))))

(defn -main [& argv]
  (let [{:keys [roots interval-ms cold-scan? watch-mode debounce-ms max-events max-cycles]} (parse-args argv)
        run-id (System/currentTimeMillis)
        event-n (atom 0)
        cycle-n (atom 0)
        per-root-cache (atom (zipmap (map :path roots) (repeat {})))]
    (when (empty? roots)
      (println "ERROR: at least one --root <path>=<label> required") (System/exit 2))
    (println "[multi_watcher] starting; run-id =" run-id)
    (println "  watch-mode:" watch-mode
             " interval-ms:" interval-ms
             " debounce-ms:" debounce-ms
             " cold-scan?:" cold-scan?)
    (when max-events (println "  max-events:" max-events "(test mode)"))
    (when max-cycles (println "  max-cycles:" max-cycles "(test mode)"))
    (doseq [{:keys [path label]} roots]
      (println (format "  watch %s  →  label=%s" path label)))
    (println)
    (let [all-roots (set (map :path roots))
          run-cycle!*
          (fn [dirty-roots]
            (run-cycle! {:roots roots
                         :per-root-cache per-root-cache
                         :cold-scan? cold-scan?
                         :max-events max-events
                         :run-id run-id
                         :event-n event-n
                         :cycle-n cycle-n
                         :dirty-roots dirty-roots})
            (when (and max-cycles (>= @cycle-n max-cycles))
              (println "[multi_watcher] reached --max-cycles; exiting")
              (System/exit 0)))]
      (if (= watch-mode "watchservice")
        (let [watcher (.newWatchService (java.nio.file.FileSystems/getDefault))
              key-index (atom {})]
          (ensure-watchservice-available!)
          (doseq [{:keys [path label]} roots]
            (register-root-tree! watcher key-index path label path))
          ;; Prime once so later delete/rename handling has a last-known hash.
          (run-cycle!* all-roots)
          (loop []
            (if-let [first-key (.poll watcher interval-ms TimeUnit/MILLISECONDS)]
              (do
                (run-cycle!* (collect-dirty-roots! watcher key-index first-key debounce-ms))
                (recur))
              (do
                (run-cycle!* #{})
                (recur)))))
        (loop []
          (run-cycle!* all-roots)
          (Thread/sleep interval-ms)
          (recur))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
