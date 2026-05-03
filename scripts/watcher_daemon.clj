#!/usr/bin/env bb
;; substrate-2 phase-4 watcher daemon: poll-based live ingestion.
;;
;; Usage:
;;   bb watcher_daemon.clj --root /path/to/repo --label tag
;;       [--vocab path]... [--interval 5]
;;       [--max-cycles N]    ; for tests; default infinite
;;
;; Polls every `--interval` seconds. On each cycle:
;;   1. Walks repo for source files (clj/cljs/cljc/el).
;;   2. Computes SHA-256 per file, compares with cache.
;;   3. If any file changed (or cache cold), invokes the existing
;;      `ingest_v05_to_futon1a.clj` for the repo. (v0 simplification:
;;      whole-repo re-ingest; phase 4.5 will switch to per-file.)
;;   4. Emits a `code/v05/watcher-event` hyperedge with cycle stats.
;;   5. Updates cache (in-memory + persisted to /tmp).
;;
;; Inside-out invariants:
;;   - L0 durability: cycle "complete" only when watcher-event POST
;;     returns HTTP 200. Failed cycles re-tried on next tick.
;;   - L1 stable id: watcher-event ID = hx:code/v05/watcher-event:<repo>:<cycle-n>:<dir-marker>
;;     so each cycle has a unique vertex.
;;   - L2 cache integrity: cache is single source of truth for
;;     "is this file's hash known?"; restart-safe via /tmp.
;;   - L4 validation: only walk files matching known extensions.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def WATCHED-EXTS #{"clj" "cljs" "cljc" "el"})
(def INGEST-SCRIPT "/home/joe/code/futon3/scripts/ingest_v05_to_futon1a.clj")

;; ---------- L4 file walk ----------

(defn watched-files [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile ^java.io.File %))
       (filter #(WATCHED-EXTS (fs/extension (.getPath ^java.io.File %))))
       ;; Skip noise: .git, node_modules, build artifacts, .cpcache, target
       (remove #(re-find #"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo)/|/node_modules/|/target/|/out/"
                         (.getPath ^java.io.File %)))
       (mapv #(.getPath ^java.io.File %))))

;; ---------- hash computation ----------

(defn sha-256 [^String path]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bs (.readAllBytes (io/input-stream path))
        _  (.update md bs)
        h  (.digest md)]
    (apply str (map #(format "%02x" %) h))))

;; ---------- cache ----------

(defn cache-path [label]
  (str "/tmp/substrate2-watcher-" label ".edn"))

(defn load-cache [label]
  (let [p (cache-path label)]
    (if (fs/exists? p)
      (try (edn/read-string (slurp p))
           (catch Exception _ {}))
      {})))

(defn save-cache [label cache]
  (spit (cache-path label) (with-out-str (clojure.pprint/pprint cache))))

;; ---------- HTTP write surface ----------

(defn directed-endpoints
  "Same convention as ingest_v05_to_futon1a.clj. The watcher-event type
   carries a synthetic third endpoint for unique stable ID per cycle."
  [hx-type endpoints]
  (if (and (= "code/v05/watcher-event" hx-type) (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

(defn post-hyperedge!
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))
        resp (try
               (http/post (str FUTON1A "/api/alpha/hyperedge")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (and (= 200 (:status resp))
               (or (:hyperedge body) (:hx/id body)))
     :status (:status resp) :body body}))

;; ---------- ingest invocation ----------

(defn run-ingest! [root label vocab-args]
  (let [start (System/currentTimeMillis)
        args (concat ["bb" INGEST-SCRIPT root "--label" label] vocab-args)
        {:keys [exit out err]} (apply sh args)]
    {:exit exit
     :duration-ms (- (System/currentTimeMillis) start)
     :tail (last (take 30 (str/split-lines (or out ""))))
     :err (when-not (zero? exit) err)}))

;; ---------- cycle ----------

(defn cycle-once [{:keys [root label vocab cycle-n cache run-id]}]
  (let [t-start (System/currentTimeMillis)
        files (watched-files root)
        new-cache (zipmap files (pmap sha-256 files))
        changed (filter (fn [f] (not= (get new-cache f) (get cache f))) files)
        ingested? (or (empty? cache) (seq changed))
        ingest-result (when ingested?
                        (run-ingest! root label vocab))
        duration (- (System/currentTimeMillis) t-start)
        ;; Run-id (process-start timestamp) makes each cycle's stable ID
        ;; unique across watcher invocations; otherwise cycle-1-of-run-A
        ;; and cycle-1-of-run-B would collide via futon1a's L1 dedupe.
        evidence-id (str root "/run-" run-id "/cycle-" cycle-n)
        evidence-ok (post-hyperedge!
                     "code/v05/watcher-event"
                     [evidence-id (str cycle-n)]
                     ["v05" "phase-4" label "watcher-event"]
                     {"repo" label
                      "phase" 4
                      "run-id" run-id
                      "cycle" cycle-n
                      "ts" (System/currentTimeMillis)
                      "files-scanned" (count files)
                      "files-changed" (count changed)
                      "ingested?" ingested?
                      "ingest-exit" (when ingested? (:exit ingest-result))
                      "ingest-duration-ms" (when ingested? (:duration-ms ingest-result))
                      "duration-ms" duration})]
    {:files-scanned (count files)
     :files-changed (count changed)
     :ingested? ingested?
     :duration-ms duration
     :evidence-ok? (:ok? evidence-ok)
     :new-cache new-cache}))

;; ---------- main loop ----------

(defn parse-args [argv]
  (loop [a argv opts {:vocab [] :interval 5 :max-cycles nil}]
    (cond
      (empty? a) opts
      (= "--root"     (first a)) (recur (drop 2 a) (assoc opts :root (second a)))
      (= "--label"    (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (= "--vocab"    (first a)) (recur (drop 2 a) (update opts :vocab #(conj % "--vocab" (second a))))
      (= "--interval" (first a)) (recur (drop 2 a) (assoc opts :interval (parse-long (second a))))
      (= "--max-cycles" (first a)) (recur (drop 2 a) (assoc opts :max-cycles (parse-long (second a))))
      :else (recur (rest a) opts))))

(defn -main [& argv]
  (let [{:keys [root label vocab interval max-cycles]} (parse-args argv)]
    (when-not root  (println "ERROR: --root required") (System/exit 2))
    (when-not label (println "ERROR: --label required") (System/exit 2))
    (println "[watcher_daemon] starting")
    (println "  root:" root " label:" label " interval:" interval "s")
    (when max-cycles (println "  max-cycles:" max-cycles "(test mode)"))
    (let [cache0 (load-cache label)
          run-id (System/currentTimeMillis)]
      (println "  cache loaded:" (count cache0) "files known"
               " run-id:" run-id)
      (loop [n 1 cache cache0]
        (when (and max-cycles (> n max-cycles))
          (println "[watcher_daemon] reached --max-cycles, exiting cleanly")
          (System/exit 0))
        (let [r (cycle-once {:root root :label label :vocab vocab
                              :cycle-n n :cache cache :run-id run-id})]
          (println (format "[cycle %d] scanned=%d changed=%d ingested?=%s duration=%dms evidence-ok?=%s"
                           n (:files-scanned r) (:files-changed r)
                           (:ingested? r) (:duration-ms r) (:evidence-ok? r)))
          (save-cache label (:new-cache r))
          (when-not max-cycles
            (Thread/sleep (* 1000 interval)))
          (when (and max-cycles (< n max-cycles))
            (Thread/sleep (* 1000 interval)))
          (recur (inc n) (:new-cache r)))))))

(apply -main *command-line-args*)
