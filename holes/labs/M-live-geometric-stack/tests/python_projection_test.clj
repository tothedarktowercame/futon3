#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.edn :as edn]
         '[clojure.java.shell :refer [sh]]
         '[clojure.test :refer [deftest is run-tests]])

(load-file "/home/joe/code/futon3/scripts/python_projection.clj")

(def projector-script
  "/home/joe/code/futon3/scripts/v0_codebase_hypergraph.clj")

(def paper-file
  "/home/joe/code/futon6/src/futon6/paper_hypergraph.py")

(def paper-test-file
  "/home/joe/code/futon6/tests/test_paper_hypergraph.py")

(def extract-se-threads-file
  "/home/joe/code/futon6/scripts/extract_se_threads.py")

(defn copy-text-file! [src dst]
  (fs/create-dirs (fs/parent dst))
  (spit dst (slurp src)))

(defn with-mini-futon6-repo [f]
  (let [root (str (fs/create-temp-dir {:prefix "python-projection-"}))
        src-init (str root "/src/futon6/__init__.py")
        src-file (str root "/src/futon6/paper_hypergraph.py")
        test-init (str root "/tests/__init__.py")
        test-file (str root "/tests/test_paper_hypergraph.py")
        pyproject (str root "/pyproject.toml")]
    (try
      (copy-text-file! "/home/joe/code/futon6/src/futon6/__init__.py" src-init)
      (copy-text-file! paper-file src-file)
      (copy-text-file! "/home/joe/code/futon6/tests/__init__.py" test-init)
      (copy-text-file! paper-test-file test-file)
      (spit pyproject "[project]\nname = \"futon6-mini\"\nversion = \"0.0.0\"\n")
      (f root)
      (finally
        (fs/delete-tree root)))))

(deftest parses-paper-hypergraph-file
  (let [{:keys [ns vars tests is-test?]}
        (python-projection/collect-file paper-file)
        qnames (set (map :var/qname vars))]
    (is (= "futon6.paper_hypergraph" ns))
    (is (false? is-test?))
    (is (contains? qnames
                   "futon6.paper_hypergraph/extract_paper_hypergraph_classical"))
    (is (contains? qnames
                   "futon6.paper_hypergraph/parse_latex_blocks"))
    (is (empty? tests))))

(deftest parses-pytest-file
  (let [{:keys [ns aliases vars tests is-test?]}
        (python-projection/collect-file paper-test-file)
        test-qnames (set (map :test/qname tests))]
    (is (= "tests.test_paper_hypergraph" ns))
    (is is-test?)
    (is (empty? vars))
    (is (= 'futon6.paper_hypergraph
           (get aliases 'extract_paper_hypergraph_classical)))
    (is (contains? test-qnames
                   "tests.test_paper_hypergraph/test_classical_emits_typed_nodes"))))

(deftest derives-repo-root-relative-script-modules
  (let [{:keys [ns vars is-test?]}
        (python-projection/collect-file extract-se-threads-file)
        qnames (set (map :var/qname vars))]
    (is (= "scripts.extract_se_threads" ns))
    (is (false? is-test?))
    (is (contains? qnames
                   "scripts.extract_se_threads/process_site"))))

(deftest resolves-cross-file-call
  (with-mini-futon6-repo
    (fn [root]
      (let [dump-path (str root "/dump.edn")
            out-path (str root "/report.edn")
            {:keys [exit err]} (sh "bb" projector-script root
                                   "--dump" dump-path
                                   "--out" out-path)]
        (when-not (zero? exit)
          (throw (ex-info "Mini futon6 projector run failed"
                          {:exit exit :err err})))
        (let [dump (edn/read-string (slurp dump-path))
              coverage? (fn [src dst]
                          (some #(and (= src (:edge/source %))
                                      (= dst (:edge/target %)))
                                (:cov-edges dump)))]
          (is (coverage? "tests.test_paper_hypergraph/test_classical_emits_typed_nodes"
                         "futon6.paper_hypergraph/extract_paper_hypergraph_classical")))))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (pos? (+ fail error)) 1 0)))
