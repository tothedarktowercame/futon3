#!/usr/bin/env bb

(require '[clojure.edn :as edn]
         '[clojure.java.shell :refer [sh]]
         '[clojure.test :refer [deftest is run-tests]])

(load-file "/home/joe/code/futon3/scripts/elisp_projection.clj")

(def projector-script
  "/home/joe/code/futon3/scripts/v0_codebase_hypergraph.clj")

(def futon4-root
  "/home/joe/code/futon4")

(def store-file
  "/home/joe/code/futon4/dev/arxana-store.el")

(def store-test-file
  "/home/joe/code/futon4/test/arxana-store-test.el")

(defonce futon4-projection
  (delay
    (let [dump-path "/tmp/elisp-projection-test-dump.edn"
          {:keys [exit err]} (sh "bb" projector-script futon4-root
                                 "--dump" dump-path
                                 "--out" "/tmp/elisp-projection-test-report.edn")]
      (when-not (zero? exit)
        (throw (ex-info "Projector run failed"
                        {:exit exit :err err})))
      (edn/read-string (slurp dump-path)))))

(deftest parses-arxana-store-vars
  (let [{:keys [ns vars tests is-test?]}
        (elisp-projection/collect-file store-file)]
    (is (= "arxana-store" ns))
    (is (not is-test?))
    (is (= 77 (count vars)))
    (is (empty? tests))))

(deftest parses-ert-tests-from-test-file
  (let [{:keys [ns vars tests is-test?]}
        (elisp-projection/collect-file store-test-file)]
    (is (= "arxana-store-test" ns))
    (is is-test?)
    (is (empty? vars))
    (is (= 32 (count tests)))))

(deftest resolves-browser-to-store-calls
  (let [{:keys [call-edges]} @futon4-projection
        call? (fn [src dst]
                (some #(and (= src (:edge/source %))
                            (= dst (:edge/target %)))
                      call-edges))]
    (is (call? "arxana-browser-core/arxana-browser--header-line"
               "arxana-store/arxana-store-remote-status"))
    (is (call? "arxana-browser-core/arxana-browser--root-items"
               "arxana-store/arxana-store-sync-enabled-p"))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (pos? (+ fail error)) 1 0)))
