#!/usr/bin/env bb
;; substrate-2 phase-4.5 per-file ingest: process a single file and POST
;; only its contribution to the substrate. Designed to be called from the
;; multi_watcher daemon on each fs event.
;;
;; Usage:
;;   bb ingest_one_file.clj <file-path> --root <repo-root> --label <tag>
;;       [--vocab path]...
;;
;; Dispatch:
;;   .clj/.cljs/.cljc → vendored Clojure projector
;;   .el              → elisp-projection/collect-file
;;   .py              → python-projection/collect-file (when present)
;;   holes/missions/M-*.md → mission sync push into futon3c + futon1a mission vertex
;;   *-terminal-vocabulary.md → vocab term registration
;;   else            → no-op (returns OK; substrate-2 has no projector)
;;
;; For category-C files (.flexiarg, docbook entries), this script returns an
;; :unhandled status WITHOUT failing — the dispatcher defers to the existing
;; per-type ingest paths. Mission docs are now handled as a push-sync bridge.
;;
;; Symbol resolution: per-file edges (calls, coverage, vocabulary-use)
;; require the *current repo's* by-ns map. v0 rebuilds this map by walking
;; the repo on each invocation (~1-3 seconds for futon3c-scale). Phase 4.6
;; can cache it across invocations if measurement shows this is a hotspot.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[babashka.fs :as fs]
         '[clojure.walk :as walk]
         '[clojure.set]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(load-file (str (fs/parent *file*) "/elisp_projection.clj"))
(when (fs/exists? (str (fs/parent *file*) "/python_projection.clj"))
  (load-file (str (fs/parent *file*) "/python_projection.clj")))
(when (fs/exists? (str (fs/parent *file*) "/flexiarg_projection.clj"))
  (load-file (str (fs/parent *file*) "/flexiarg_projection.clj")))

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def FUTON3C   (or (System/getenv "FUTON3C_URL") "http://localhost:7070"))

(def directed-types
  #{"code/v05/calls" "code/v05/coverage" "code/v05/vocabulary-use"
    "code/v05/term-defines" "code/v05/contains"})

(defn directed-endpoints [hx-type endpoints]
  (if (and (directed-types hx-type) (= 2 (count endpoints)))
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
               (or (:hyperedge body) (:hx/id body)))}))

;; ---------- Clojure projection (vendored from v0) ----------

(def src-exts #{"clj" "cljs" "cljc"})
(def def-forms #{'defn 'defn- 'def 'defmulti 'defmethod 'defprotocol 'defrecord 'deftype})

(defn read-forms [^java.io.File f]
  (with-open [pbr (java.io.PushbackReader. (io/reader f))]
    (binding [*default-data-reader-fn* (fn [_t v] v)]
      (loop [acc []]
        (let [form (try (read {:read-cond :allow :features #{:clj :cljs} :eof ::eof} pbr)
                        (catch Exception _ ::eof))]
          (if (= form ::eof) acc (recur (conj acc form))))))))

(defn ns-form [forms] (some #(when (and (seq? %) (= 'ns (first %))) %) forms))

(defn parse-requires [ns-form]
  (let [reqs (some (fn [c] (when (and (seq? c) (= :require (first c))) (rest c))) ns-form)
        flat (mapcat (fn [r]
                       (cond
                         (symbol? r) [[r r]]
                         (vector? r)
                         (let [n (first r) opts (apply hash-map (rest r))]
                           (if-let [a (:as opts)] [[a n]] [[n n]]))
                         :else [])) reqs)]
    (into {} flat)))

(defn defn-body [form] (drop 2 form))

(defn collect-symbols [body]
  (let [syms (atom #{})]
    (walk/postwalk (fn [x] (when (symbol? x) (swap! syms conj x)) x) body)
    @syms))

(defn test-file? [path]
  (or (str/includes? path "/test/")
      (str/includes? path "/tests/")
      (str/ends-with? path "_test.clj")
      (str/ends-with? path "_test.cljs")
      (str/ends-with? path "_test.cljc")))

(defn collect-clj-file [path]
  (let [forms (read-forms (io/file path))
        nf (ns-form forms)
        nsym (when nf (second nf))]
    (when nsym
      (let [aliases (parse-requires nf)
            test? (test-file? path)
            vars (atom []) tests (atom [])]
        (doseq [f forms :when (and (seq? f) (symbol? (first f)))]
          (let [head (first f)]
            (cond
              (= head 'deftest)
              (when-let [tname (second f)]
                (when (symbol? tname)
                  (swap! tests conj
                         {:test/ns (str nsym) :test/name (str tname)
                          :test/qname (str nsym "/" tname)
                          :test/syms (collect-symbols (defn-body f))})))
              (def-forms head)
              (when-let [vname (second f)]
                (when (symbol? vname)
                  (let [docstr (when (string? (nth f 2 nil)) (nth f 2 nil))]
                    (swap! vars conj
                           {:var/ns (str nsym) :var/name (str vname)
                            :var/qname (str nsym "/" vname)
                            :var/kind (str head)
                            :var/has-doc (some? docstr)
                            :var/syms (collect-symbols (defn-body f))})))))))
        {:ns (str nsym) :aliases aliases
         :vars (if test? [] @vars)
         :tests (if test? @tests [])}))))

(defn collect-file [path]
  (let [ext (fs/extension path)]
    (cond
      (src-exts ext) (collect-clj-file path)
      (and (resolve 'elisp-projection/src-exts)
           ((var-get (resolve 'elisp-projection/src-exts)) ext))
      ((var-get (resolve 'elisp-projection/collect-file)) path)
      (and (resolve 'python-projection/src-exts)
           ((var-get (resolve 'python-projection/src-exts)) ext))
      ((var-get (resolve 'python-projection/collect-file)) path)
      (and (resolve 'flexiarg-projection/src-exts)
           ((var-get (resolve 'flexiarg-projection/src-exts)) ext))
      ((var-get (resolve 'flexiarg-projection/collect-file)) path)
      :else nil)))

(defn mission-doc-path?
  [path]
  (boolean
   (re-find #"/holes/missions/M-[^/]+\.md$"
            (str/replace (str path) "\\" "/"))))

(defn sync-mission!
  "Push a single mission markdown file into futon3c's evidence layer.
   Returns the parsed mission payload on success."
  [path root]
  (let [repo-name (.getName (io/file root))
        payload {:path (str path) :repo repo-name}
        resp (try
               (http/post (str FUTON3C "/api/alpha/mc/sync-mission")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (= 200 (:status resp))
     :status (:status resp)
     :body body}))

(defn ingest-mission-doc!
  "Materialize a mission vertex in futon1a from the mission-control sync
   payload. The watcher is push-first here: a file write triggers mission
   sync, then the synced payload is projected into the substrate."
  [{:keys [path label root]}]
  (let [sync (sync-mission! path root)
        mission (:mission (:body sync))
        mission-id (or (:mission/id mission) (get mission "mission/id"))
        mission-title (or (:mission/title mission) (get mission "mission/title"))
        mission-status (or (:mission/status mission) (get mission "mission/status"))
        mission-repo (or (:mission/repo mission) (get mission "mission/repo"))
        mission-date (or (:mission/date mission) (get mission "mission/date"))
        vertex-id (str label "/mission/" mission-id)
        labels ["v05" "phase-4.5" label "mission-doc"]
        props {"repo" label
               "phase" 4.5
               "source-file" path
               "mission/id" mission-id
               "mission/title" mission-title
               "mission/status" (cond
                                  (keyword? mission-status) (name mission-status)
                                  :else mission-status)
               "mission/repo" mission-repo
               "mission/date" mission-date
               "mission/sync-status" (:status sync)
               "mission/sync-created" (boolean (or (get-in sync [:body :created])
                                                   (get-in sync [:body "created"])))
               "mission/sync-skipped" (boolean (or (get-in sync [:body :skipped])
                                                   (get-in sync [:body "skipped"])))}
        hx-ok? (and mission-id
                    (:ok? sync)
                    (:ok? (post-hyperedge! "code/v05/mission-doc" [vertex-id] labels props)))]
    {:vertices (if hx-ok? 1 0)
     :edges 0
     :failed (if hx-ok? 0 1)
     :sync sync}))

;; ---------- repo-wide by-ns rebuild for resolution ----------

;; P-3 by-ns cache (TTL=60s): the per-file ingest invokes `collect-repo`
;; on every dispatch, which on a futon3c-scale repo costs ~300-500ms of
;; pure CPU walking. Watcher dispatch happens often during heavy editing.
;; Cache to /tmp keyed by root path; reuse if fresh.
(def ^:private byns-ttl-ms (* 60 1000))

(defn- byns-cache-path [root]
  ;; Hash the absolute root path into a short hex; one cache file per repo.
  (let [h (java.security.MessageDigest/getInstance "SHA-256")
        bs (.getBytes ^String (str (fs/absolutize root)))]
    (.update h bs)
    (str "/tmp/substrate2-byns-"
         (subs (apply str (map #(format "%02x" %) (.digest h))) 0 16)
         ".edn")))

(defn- read-fresh-byns [root]
  (let [p (byns-cache-path root)]
    (when (and (fs/exists? p)
               (let [age (- (System/currentTimeMillis)
                            (.lastModified (java.io.File. ^String p)))]
                 (< age byns-ttl-ms)))
      (try (clojure.edn/read-string (slurp p))
           (catch Exception _ nil)))))

(defn- write-byns! [root data]
  (try (spit (byns-cache-path root)
             (binding [*print-length* nil
                       *print-level*  nil]
               (pr-str data)))
       (catch Exception _ nil)))

(defn- collect-repo* [root]
  (let [supported? (fn [path]
                     (let [ext (fs/extension path)]
                       (or (src-exts ext)
                           (and (resolve 'elisp-projection/src-exts)
                                ((var-get (resolve 'elisp-projection/src-exts)) ext))
                           (and (resolve 'python-projection/src-exts)
                                ((var-get (resolve 'python-projection/src-exts)) ext))
                           (and (resolve 'flexiarg-projection/src-exts)
                                ((var-get (resolve 'flexiarg-projection/src-exts)) ext)))))
        files (->> (file-seq (io/file root))
                   (filter #(.isFile ^java.io.File %))
                   (filter #(supported? (.getPath ^java.io.File %)))
                   (remove #(re-find #"/\.(git|cpcache|shadow-cljs|lsp|clj-kondo)/|/node_modules/|/target/"
                                     (.getPath ^java.io.File %))))
        out (atom {:vars [] :tests [] :ns-set #{} :ns→aliases {}})]
    (doseq [f files]
      (when-let [{:keys [ns aliases vars tests]}
                 (try (collect-file (.getPath ^java.io.File f))
                      (catch Exception _ nil))]
        (swap! out update :ns-set conj ns)
        (swap! out update :vars into vars)
        (swap! out update :tests into tests)
        (swap! out assoc-in [:ns→aliases ns] aliases)))
    (let [{:keys [vars]} @out]
      (assoc @out :by-ns
             (reduce (fn [acc v]
                       (assoc-in acc [(:var/ns v) (:var/name v)] (:var/qname v)))
                     {} vars)))))

(defn collect-repo
  "Walk repo, return {:by-ns ..., :all-vars ..., :ns-set ..., :ns→aliases ...}.
   Cached under /tmp with a 60s TTL so back-to-back per-file dispatches
   under a single watcher session amortise the walk cost.

   Cache invalidation is time-based, not content-based. A file rename or
   deletion within the TTL window may produce one or two cycles of stale
   resolution; the next refresh corrects it."
  [root]
  (or (when-let [cached (read-fresh-byns root)]
        (binding [*out* *err*] (println "[collect-repo] hit by-ns cache"))
        cached)
      (let [data (collect-repo* root)]
        (write-byns! root data)
        data)))

(defn resolve-symbol [s cur-ns aliases by-ns]
  (let [nm (name s) nsp (namespace s)]
    (cond
      (and (nil? nsp) (get-in by-ns [cur-ns nm])) (get-in by-ns [cur-ns nm])
      (and nsp aliases)
      (let [a-sym (symbol nsp)
            target-ns (some-> (get aliases a-sym) str)]
        (when (and target-ns (get-in by-ns [target-ns nm]))
          (get-in by-ns [target-ns nm])))
      (and nsp (get-in by-ns [nsp nm])) (get-in by-ns [nsp nm]))))

;; ---------- per-file ingest ----------

(defn ingest-one-file!
  "Parse `path`, POST its vertices and edges to futon1a. Returns stats.
   B-2 v0: per-repo prefix applied to per-repo qname endpoints."
  [{:keys [path label root-ctx]}]
  (let [{:keys [ns vars tests aliases]} (or (collect-file path) {})
        labels ["v05" "phase-4.5" label "per-file"]
        base-props {"repo" label "phase" 4.5 "source-file" path}
        pf (fn [q] (str label "/" q))   ; per-repo prefix
        post! (fn [t eps & [extra-props]]
                (post-hyperedge! t eps labels (merge base-props extra-props)))
        stats (atom {:vertices 0 :edges 0 :failed 0})]
    (when ns
      ;; Vertex writes (per-repo prefixed)
      (post! "code/v05/namespace" [(pf ns)] {"namespace" ns})
      (swap! stats update :vertices inc)
      (doseq [v vars]
        (let [r (post! "code/v05/var" [(pf (:var/qname v))]
                       {"var/ns" (:var/ns v) "var/qname" (:var/qname v)
                        "var/kind" (:var/kind v)
                        "var/has-doc" (:var/has-doc v)})]
          (swap! stats update (if (:ok? r) :vertices :failed) inc)))
      (doseq [t tests]
        (let [r (post! "code/v05/test" [(pf (:test/qname t))]
                       {"test/ns" (:test/ns t) "test/qname" (:test/qname t)})]
          (swap! stats update (if (:ok? r) :vertices :failed) inc)))
      ;; Edge writes — endpoints use per-repo prefix on per-repo qnames.
      (let [{:keys [by-ns ns-set]} root-ctx]
        (doseq [v vars
                s (:var/syms v)
                :let [qn (resolve-symbol s ns aliases by-ns)]
                :when (and qn (not= qn (:var/qname v)))]
          (let [r (post! "code/v05/calls" [(pf (:var/qname v)) (pf qn)])]
            (swap! stats update (if (:ok? r) :edges :failed) inc)))
        (doseq [t tests
                s (:test/syms t)
                :let [qn (resolve-symbol s ns aliases by-ns)]
                :when (and qn (not= qn (:test/qname t)))]
          (let [r (post! "code/v05/coverage" [(pf (:test/qname t)) (pf qn)])]
            (swap! stats update (if (:ok? r) :edges :failed) inc)))
        (doseq [v vars]
          (let [r (post! "code/v05/contains" [(pf ns) (pf (:var/qname v))])]
            (swap! stats update (if (:ok? r) :edges :failed) inc)))))
    @stats))

;; ---------- main ----------

(defn parse-args [argv]
  (loop [a argv opts {:vocab []}]
    (cond
      (empty? a) opts
      (= "--root"  (first a)) (recur (drop 2 a) (assoc opts :root (second a)))
      (= "--label" (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (= "--vocab" (first a)) (recur (drop 2 a) (update opts :vocab conj (second a)))
      (str/starts-with? (first a) "--") (recur (rest a) opts)
      :else (recur (rest a) (assoc opts :path (first a))))))

(defn -main [& argv]
  (let [{:keys [path root label]} (parse-args argv)]
    (when-not path  (println "ERROR: <file-path> required") (System/exit 2))
    (when-not root  (println "ERROR: --root required") (System/exit 2))
    (when-not label (println "ERROR: --label required") (System/exit 2))
    (when-not (fs/exists? path)
      (println "[skip] file removed:" path) (System/exit 0))
    (let [ext (fs/extension path)
          mission-doc? (mission-doc-path? path)
          handled? (or mission-doc?
                       (src-exts ext)
                       (and (resolve 'elisp-projection/src-exts)
                            ((var-get (resolve 'elisp-projection/src-exts)) ext))
                       (and (resolve 'python-projection/src-exts)
                            ((var-get (resolve 'python-projection/src-exts)) ext))
                       (and (resolve 'flexiarg-projection/src-exts)
                            ((var-get (resolve 'flexiarg-projection/src-exts)) ext)))]
      (cond
        (not handled?)
        (do (println "[unhandled]" path "(extension" ext "— see file-type-inventory.md)")
            (System/exit 0))
        mission-doc?
        (let [t-start (System/currentTimeMillis)
              stats (ingest-mission-doc! {:path path :label label :root root})
              dur (- (System/currentTimeMillis) t-start)]
          (println (format "[mission-sync] %s  v=%d e=%d failed=%d  %dms"
                           path (:vertices stats) (:edges stats)
                           (:failed stats) dur))
          (when (or (pos? (:failed stats))
                    (not (:ok? (:sync stats))))
            (println "  futon3c sync status:" (get-in stats [:sync :status]))
            (when-let [body (get-in stats [:sync :body])]
              (println "  futon3c sync body:" body))
            (System/exit 1)))
        :else
        (let [t-start (System/currentTimeMillis)
              ;; root-ctx rebuilt per call; phase 4.6 will cache
              root-ctx (collect-repo root)
              stats (ingest-one-file! {:path path :label label :root-ctx root-ctx})
              dur (- (System/currentTimeMillis) t-start)]
          (println (format "[ingest] %s  v=%d e=%d failed=%d  %dms"
                           path (:vertices stats) (:edges stats)
                           (:failed stats) dur))
          (when (pos? (:failed stats)) (System/exit 1)))))))

(apply -main *command-line-args*)
