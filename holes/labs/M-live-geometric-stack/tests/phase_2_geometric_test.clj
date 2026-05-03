#!/usr/bin/env bb
;; Phase-2 invariant tests: substrate-2 geometric layer must reproduce
;; the v0.5 bb projection numbers byte-for-byte (modulo the L1 dedupe
;; correction noted in phase 1's regression).
;;
;; Usage:
;;   bb phase_2_geometric_test.clj --repo /path/to/codebase --label tag
;;       [--vocab path]...
;;
;; Compares geometric_layer_phase2.clj output (XTDB-derived) against
;; v0_codebase_hypergraph.clj output (bb-projected from source).

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.set]
         '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs])

(def ^:dynamic *fail* (atom 0))
(def ^:dynamic *pass* (atom 0))

(defn assert! [pred msg]
  (if pred
    (do (swap! *pass* inc) (println "  PASS:" msg))
    (do (swap! *fail* inc) (println "  FAIL:" msg))))

(defn run-bb [script & args]
  (let [{:keys [exit out err]} (apply sh "bb" script args)]
    (when-not (zero? exit)
      (println "  stderr:" err))
    out))

(defn parse-args [argv]
  (loop [a argv opts {:vocab []}]
    (cond
      (empty? a) opts
      (= "--repo"  (first a)) (recur (drop 2 a) (assoc opts :repo (second a)))
      (= "--label" (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (= "--vocab" (first a)) (recur (drop 2 a) (update opts :vocab conj (second a)))
      :else (recur (rest a) opts))))

(defn -main [& argv]
  (let [{:keys [repo label vocab]} (parse-args argv)]
    (when-not repo  (println "ERROR: --repo required") (System/exit 2))
    (when-not label (println "ERROR: --label required") (System/exit 2))
    (println "=== Phase-2 geometric-layer regression tests ===")
    (println " repo:" repo " label:" label)
    (println)

    (println "[setup] running bb v0.5 projection from source")
    (apply run-bb "/home/joe/code/futon3/scripts/v0_codebase_hypergraph.clj"
           (concat [repo "--out" "/tmp/p2-bb-v05.edn"]
                   (mapcat #(vector "--vocab" %) vocab)))
    (println "[setup] running phase-2 geometric layer from XTDB")
    (run-bb "/home/joe/code/futon3/scripts/geometric_layer_phase2.clj"
            "--label" label "--out" "/tmp/p2-xtdb.edn")

    (let [bb (edn/read-string (slurp "/tmp/p2-bb-v05.edn"))
          x  (edn/read-string (slurp "/tmp/p2-xtdb.edn"))
          bbc (:counts bb) xc (:counts x)]
      (println)
      (println "[regression] counts")
      ;; var counts: bb over-counts duplicate defns; xtdb dedupes via L1.
      ;; Normalise bb's paired-vars by distinct qname to make a fair comparison.
      (apply sh "bb" "/home/joe/code/futon3/scripts/v0_codebase_hypergraph.clj"
             repo "--out" "/tmp/_p2.edn" "--dump" "/tmp/p2-bb-dump.edn"
             (mapcat #(vector "--vocab" %) vocab))
      (let [d (edn/read-string (slurp "/tmp/p2-bb-dump.edn"))
            covered-qnames (set (map :edge/target (:cov-edges d)))
            bb-paired-distinct (count (clojure.set/intersection
                                        covered-qnames
                                        (set (map :var/qname (:vars d)))))]
        (assert! (= bb-paired-distinct (:paired-vars xc))
                 (str "paired-vars (distinct qname): bb=" bb-paired-distinct
                      " xtdb=" (:paired-vars xc)
                      " (bb raw=" (:paired-vars bbc)
                      "; difference of " (- (:paired-vars bbc) bb-paired-distinct)
                      " is duplicate-defn over-counting)")))
      (assert! (= (:tests bbc) (:tests xc))
               (str "tests: bb=" (:tests bbc) " xtdb=" (:tests xc)))
      (assert! (= (:coverage-edges bbc) (:coverage-edges xc))
               (str "coverage-edges: bb=" (:coverage-edges bbc) " xtdb=" (:coverage-edges xc)))
      (assert! (= (:call-edges bbc) (:call-edges xc))
               (str "call-edges: bb=" (:call-edges bbc) " xtdb=" (:call-edges xc)))
      (assert! (= (:vocab-edges bbc) (:vocab-edges xc))
               (str "vocab-edges: bb=" (:vocab-edges bbc) " xtdb=" (:vocab-edges xc)))
      (assert! (= (:components bbc) (:components xc))
               (str "components: bb=" (:components bbc) " xtdb=" (:components xc)))
      (assert! (= (:nonzero-laplacian-count bb) (:nonzero-laplacian-count x))
               (str "nonzero ΔT: bb=" (:nonzero-laplacian-count bb)
                    " xtdb=" (:nonzero-laplacian-count x)))

      (println)
      (println "[regression] top ±ΔT")
      (let [bb-pos (set (map (juxt :var :delta-T) (:top-+deltaT bb)))
            x-pos  (set (map (juxt :var :delta-T) (:top-+deltaT x)))
            bb-neg (set (map (juxt :var :delta-T) (:top--deltaT bb)))
            x-neg  (set (map (juxt :var :delta-T) (:top--deltaT x)))]
        (assert! (= bb-pos x-pos)
                 (str "top +ΔT identical: |bb|=" (count bb-pos) " |xtdb|=" (count x-pos)
                      " |Δ|=" (count (clojure.set/difference bb-pos x-pos))))
        (assert! (= bb-neg x-neg)
                 (str "top −ΔT identical: |bb|=" (count bb-neg) " |xtdb|=" (count x-neg)
                      " |Δ|=" (count (clojure.set/difference bb-neg x-neg)))))

      (println)
      (println "[regression] component summaries")
      (let [bb-comp-sizes (sort (map :size (:components-summary bb)))
            x-comp-sizes  (sort (map :size (:components-summary x)))]
        (assert! (= bb-comp-sizes x-comp-sizes)
                 (str "component sizes (top-10) identical: " bb-comp-sizes " vs " x-comp-sizes)))

      (println)
      (println "[regression] drift hotspots")
      (let [bb-drift (set (map (juxt :c1 :c2 :overlap)
                                (or (:drift-hotspots bb) [])))
            x-drift  (set (map (juxt :c1 :c2 :overlap)
                                (or (:drift-hotspots x) [])))]
        (assert! (= bb-drift x-drift)
                 (str "drift-hotspots identical: |bb|=" (count bb-drift)
                      " |xtdb|=" (count x-drift))))

      (println)
      (println "[determinism] re-running phase-2 produces identical output")
      (run-bb "/home/joe/code/futon3/scripts/geometric_layer_phase2.clj"
              "--label" label "--out" "/tmp/p2-xtdb-2.edn")
      (let [x2 (edn/read-string (slurp "/tmp/p2-xtdb-2.edn"))]
        (assert! (= (dissoc x :label) (dissoc x2 :label))
                 "phase-2 output is deterministic across runs")))

    (println)
    (println "=== summary ===")
    (println " PASS:" @*pass*)
    (println " FAIL:" @*fail*)
    (System/exit (if (pos? @*fail*) 1 0))))

(apply -main *command-line-args*)
