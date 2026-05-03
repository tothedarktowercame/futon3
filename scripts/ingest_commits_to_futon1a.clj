#!/usr/bin/env bb
;; substrate-2 phase-3 ingest: commits as first-class hyperedges (BACKFILL).
;;
;; Usage:
;;   bb ingest_commits_to_futon1a.clj <repo-root> --label <tag>
;;
;; This script is the BACKFILL TOOL. Live ingestion (per-cycle) is owned
;; by multi_watcher.clj via the same shared library (commit_ingest_lib.clj).
;;
;; The standalone script remains useful for: initial population of a new
;; watched repo, recovery after substrate-2 reset, manual re-runs.
;;
;; Idempotent: re-running on an already-indexed repo is a no-op
;; semantically (futon1a deduplicates by stable hx/id). `:commit` count
;; is counter-ratchet-monotonic.

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[babashka.fs :as fs]
         '[clojure.set])

(load-file (str (fs/parent *file*) "/commit_ingest_lib.clj"))
(load-file (str (fs/parent *file*) "/elisp_projection.clj"))
(load-file (str (fs/parent *file*) "/python_projection.clj"))

;; ---------- HEAD var index (heavyweight; computed once, supplied to lib) ----------

(def src-exts #{"clj" "cljs" "cljc"})
(def el-exts elisp-projection/src-exts)
(def py-exts python-projection/src-exts)
(def excluded-path-segments
  #{".git" ".venv" "venv" "node_modules" "__pycache__" ".mypy_cache" ".pytest_cache"})
(def var-def-forms
  #{'defn 'defn- 'def 'defmulti 'defmethod 'defprotocol 'defrecord 'deftype})
(def test-def-form 'deftest)

(defn excluded-path? [path]
  (let [parts (set (map str (fs/components path)))]
    (boolean (seq (clojure.set/intersection excluded-path-segments parts)))))

(defn pruned-file-seq [root]
  (letfn [(children [^java.io.File f]
            (when (.isDirectory f)
              (->> (.listFiles f)
                   (remove #(or (and (.isDirectory ^java.io.File %)
                                     (excluded-path? (.getPath ^java.io.File %)))
                                (java.nio.file.Files/isSymbolicLink
                                 (.toPath ^java.io.File %)))))))]
    (->> (tree-seq #(.isDirectory ^java.io.File %) children (io/file root))
         (filter #(.isFile ^java.io.File %)))))

(defn read-forms [^java.io.File f]
  (with-open [pbr (java.io.PushbackReader. (io/reader f))]
    (binding [*default-data-reader-fn* (fn [_t v] v)]
      (loop [acc []]
        (let [form (try (read {:read-cond :allow :features #{:clj :cljs} :eof ::eof} pbr)
                        (catch Exception _ ::eof))]
          (if (= form ::eof) acc (recur (conj acc form))))))))

(defn ns-form [forms]
  (some #(when (and (seq? %) (= 'ns (first %))) %) forms))

(defn test-file? [path]
  (or (str/includes? path "/test/")
      (str/includes? path "/tests/")
      (str/ends-with? path "_test.clj")
      (str/ends-with? path "_test.cljs")
      (str/ends-with? path "_test.cljc")))

(defn vars-in-clj-file [path]
  (let [forms (read-forms (io/file path))
        nsym (when-let [nf (ns-form forms)] (second nf))]
    (when nsym
      (let [test? (test-file? path)
            accept? (if test? #(= test-def-form %) var-def-forms)]
        (keep (fn [f]
                (when (and (seq? f) (accept? (first f)) (symbol? (second f)))
                  (str nsym "/" (second f))))
              forms)))))

(defn vars-in-file [path]
  (when (fs/exists? path)
    (let [ext (fs/extension path)]
      (cond
        (src-exts ext) (vars-in-clj-file path)
        (el-exts ext)
        (let [{:keys [vars tests]} (elisp-projection/collect-file path)]
          (concat (map :var/qname vars) (map :test/qname tests)))
        (py-exts ext)
        (let [{:keys [vars tests]} (python-projection/collect-file path)]
          (concat (map :var/qname vars) (map :test/qname tests)))
        :else nil))))

(defn build-file→vars
  "Map of relative path → seq of var qnames at HEAD."
  [repo]
  (let [supported? (fn [path]
                     (let [ext (fs/extension path)]
                       (or (src-exts ext) (el-exts ext) (py-exts ext))))
        files (->> (pruned-file-seq repo)
                   (filter #(supported? (.getPath ^java.io.File %))))
        py-files (->> files
                      (map #(.getPath ^java.io.File %))
                      (filter #(py-exts (fs/extension %))))
        py-projections (when (seq py-files)
                         (python-projection/collect-files py-files))
        vars-for-path (fn [path]
                        (if (py-exts (fs/extension path))
                          (let [{:keys [vars tests]} (get py-projections path)]
                            (concat (map :var/qname vars) (map :test/qname tests)))
                          (vars-in-file path)))]
    (into {}
          (for [f files
                :let [rel (str/replace (.getPath ^java.io.File f)
                                       (str repo "/") "")
                      vs (vars-for-path (.getPath ^java.io.File f))]
                :when (seq vs)]
            [rel vs]))))

;; ---------- main ----------

(defn parse-args [argv]
  (loop [a argv opts {}]
    (cond
      (empty? a) opts
      (= "--label" (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (str/starts-with? (first a) "--") (recur (rest a) opts)
      :else (recur (rest a) (assoc opts :root (first a))))))

(defn -main [& argv]
  (let [{:keys [root label]} (parse-args argv)]
    (when-not root  (println "ERROR: <repo-root> required") (System/exit 2))
    (when-not label (println "ERROR: --label required") (System/exit 2))
    (println "[ingest_commits_to_futon1a] root=" root " label=" label " (BACKFILL)")
    (let [file->vars (build-file→vars root)
          _ (println "  files with vars at HEAD:" (count file->vars))
          _ (println)
          stats (commit-ingest-lib/ingest-all-commits!
                 {:repo-root root :repo-label label :file->vars file->vars})]
      (println)
      (println "=== ingest summary ===")
      (println " label:     " label)
      (println " n-ingested:" (:n-ingested stats))
      (println " latest-sha:" (:latest-sha stats))
      (println " n-failed:  " (:n-failed stats))
      (when (pos? (:n-failed stats))
        (println "WARN: some writes failed")
        (System/exit 1)))))

(apply -main *command-line-args*)
