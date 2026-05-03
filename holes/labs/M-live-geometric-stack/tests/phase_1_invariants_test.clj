#!/usr/bin/env bb
;; Phase-1 inside-out invariant tests for substrate-2.
;; Runs against a live futon1a at FUTON1A_URL (default http://localhost:7071).
;;
;; Usage:
;;   bb phase_1_invariants_test.clj  [--repo /path/to/codebase]
;;                                   [--vocab path]...
;;                                   [--label tag]
;;                                   [--ingest-script path]
;;
;; Pre-condition: ingest script must already exist at --ingest-script
;; (default: futon3/scripts/ingest_v05_to_futon1a.clj). Tests:
;;   1. L1 stable-id: every emitted hyperedge has :hx/id of correct shape.
;;   2. L1 idempotency: 2nd ingest run produces zero new hyperedges.
;;   3. L2 endpoint-resolution: every code/v05/calls edge has both
;;      endpoints present as code/v05/var hyperedges.
;;   4. L2 vocab-target-resolution: every vocabulary-use edge target
;;      is a term hyperedge present at same valid-time.
;;   5. L2 counter-ratchet: per-type hyperedge count never decreases
;;      between runs.
;;   6. Regression vs bb v0.5: paired-vars / coverage-edges /
;;      call-edges / vocab-edges match v0_codebase_hypergraph.clj.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.shell :refer [sh]]
         '[babashka.http-client :as http]
         '[babashka.fs :as fs]
         '[cheshire.core :as json])

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(def ^:dynamic *fail-count* (atom 0))
(def ^:dynamic *pass-count* (atom 0))

(defn assert! [pred msg]
  (if pred
    (do (swap! *pass-count* inc) (println "  PASS:" msg))
    (do (swap! *fail-count* inc) (println "  FAIL:" msg))))

(defn http-get-json [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER}
                            :throw false})]
    (when (= 200 (:status resp))
      (edn/read-string (:body resp)))))

(def ^:dynamic *label-filter* nil)

(defn fetch-of-type [hx-type]
  (let [r (http-get-json (str FUTON1A "/api/alpha/hyperedges?type="
                              (java.net.URLEncoder/encode hx-type "UTF-8")))
        all (or (:hyperedges r) [])]
    (if *label-filter*
      (filter (fn [h]
                (let [repo-prop (get-in h [:hx/props :repo])
                      labels (:hx/labels h)
                      labels-vec (cond
                                   (sequential? labels) (vec labels)
                                   (map? labels) (vec (vals labels))
                                   :else [])]
                  (or (= *label-filter* repo-prop)
                      (some #(= *label-filter* %) labels-vec))))
              all)
      all)))

(defn count-of-type [hx-type]
  (count (fetch-of-type hx-type)))

(defn run-ingest [script repo vocab label]
  (let [args (concat ["bb" script repo "--label" label]
                     (mapcat #(vector "--vocab" %) vocab))
        {:keys [exit out err]} (apply sh args)]
    (when-not (zero? exit)
      (println "  ingest stderr:" err))
    {:exit exit :out out :err err}))

(defn parse-args [argv]
  (loop [a argv opts {:vocab []
                      :ingest-script "/home/joe/code/futon3/scripts/ingest_v05_to_futon1a.clj"
                      :label "phase1-test"}]
    (cond
      (empty? a) opts
      (= "--repo"   (first a)) (recur (drop 2 a) (assoc opts :repo (second a)))
      (= "--vocab"  (first a)) (recur (drop 2 a) (update opts :vocab conj (second a)))
      (= "--label"  (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (= "--ingest-script" (first a)) (recur (drop 2 a) (assoc opts :ingest-script (second a)))
      :else (recur (rest a) opts))))

(defn check-l1-stable-id [type-key]
  (println (str "[L1-stable-id] " type-key))
  (let [hes (fetch-of-type type-key)
        ;; Stable ID convention from substrate-1: hx:{type}:{sorted-endpoints}
        ;; or futon1a may compute its own; we assert it's deterministic and starts with hx:
        ids (map :hx/id hes)
        ok-shape (every? #(and % (str/starts-with? (str %) "hx:")) ids)
        unique?  (= (count ids) (count (distinct ids)))]
    (assert! (or (zero? (count hes)) ok-shape)
             (str "all :hx/id values start with hx: (saw " (count ids) ")"))
    (assert! unique? "all :hx/id values are unique within type")))

(defn check-l1-idempotency [script repo vocab label]
  (println "[L1-idempotency] re-running ingest")
  (let [before-counts (into {} (for [t ["code/v05/var" "code/v05/test" "code/v05/namespace"
                                       "code/v05/term" "code/v05/calls" "code/v05/coverage"
                                       "code/v05/vocabulary-use" "code/v05/term-defines"]]
                                 [t (count-of-type t)]))
        _ (run-ingest script repo vocab label)
        after-counts (into {} (for [[t _] before-counts]
                                [t (count-of-type t)]))]
    (doseq [[t before] before-counts]
      (let [after (get after-counts t)]
        (assert! (= before after)
                 (str "type " t ": before=" before " after=" after " (Δ=" (- after before) ")"))))))

(defn check-l2-endpoint-resolution []
  (println "[L2-endpoint-resolution] :calls edges resolve to :var endpoints")
  (let [vars  (set (mapcat :hx/endpoints (fetch-of-type "code/v05/var")))
        calls (fetch-of-type "code/v05/calls")
        ;; Skip synthetic direction-marker endpoints (dir:src→dst); they are
        ;; added by the ingest for stable-ID disambiguation, not real vertices.
        real-eps (fn [e] (remove #(str/starts-with? % "dir:") (:hx/endpoints e)))
        unresolved (filter (fn [e] (some #(not (vars %)) (real-eps e))) calls)]
    (assert! (zero? (count unresolved))
             (str "calls edges with unresolved (non-synthetic) endpoints: "
                  (count unresolved) "/" (count calls)))))

(defn check-l2-vocab-resolution []
  (println "[L2-vocab-target-resolution] vocabulary-use edges target term vertices")
  (let [terms (set (mapcat :hx/endpoints (fetch-of-type "code/v05/term")))
        uses  (fetch-of-type "code/v05/vocabulary-use")
        ;; By ingest convention, the first two endpoints are (src ns, dst term).
        ;; Index 2 is the synthetic dir-marker (skipped here).
        unresolved (filter (fn [e]
                             (let [target (second (:hx/endpoints e))]
                               (not (terms target))))
                           uses)]
    (assert! (zero? (count unresolved))
             (str "vocabulary-use edges with unresolved term targets: "
                  (count unresolved) "/" (count uses)))))

(defn check-counter-ratchet [before after]
  (println "[L2-counter-ratchet] per-type counts never decrease")
  (doseq [[t before-n] before]
    (let [after-n (get after t 0)]
      (assert! (>= after-n before-n)
               (str "type " t ": before=" before-n " after=" after-n)))))

(defn read-bb-v05 [repo vocab]
  (let [args (concat ["bb" "/home/joe/code/futon3/scripts/v0_codebase_hypergraph.clj"
                      repo "--out" "/tmp/phase1-bb-v05.edn"]
                     (mapcat #(vector "--vocab" %) vocab))
        {:keys [exit err]} (apply sh args)]
    (when-not (zero? exit)
      (println "  bb v0.5 stderr:" err))
    (edn/read-string (slurp "/tmp/phase1-bb-v05.edn"))))

(defn check-regression-vs-bb-v05 [repo vocab]
  (println "[regression-vs-bb-v05] XTDB counts match bb v0.5 projection (distinct-qname normalised)")
  (let [bb (:counts (read-bb-v05 repo vocab))
        ;; XTDB applies L1 stable-id dedupe; bb v0 counts each occurrence.
        ;; Authoritative count is distinct-qname; bb-vars over-counts when a
        ;; var is defined twice in the same namespace (real code-coherence
        ;; smell — see substrate-2 phase-1 PUR for the ants.war finding).
        bb-vars-distinct (do
                           (require '[clojure.edn :as edn])
                           (let [d-path "/tmp/phase1-bb-v05-dump.edn"
                                 _ (apply sh (concat ["bb" "/home/joe/code/futon3/scripts/v0_codebase_hypergraph.clj"
                                                      repo "--out" "/tmp/_x.edn" "--dump" d-path]
                                                     (mapcat #(vector "--vocab" %) vocab)))
                                 d (edn/read-string (slurp d-path))]
                             (count (distinct (map :var/qname (:vars d))))))
        bb-tests (:tests bb)
        bb-cov  (:coverage-edges bb)
        bb-cal  (:call-edges bb)
        bb-voc  (:vocab-edges bb)
        x-vars  (count-of-type "code/v05/var")
        x-tests (count-of-type "code/v05/test")
        x-cov   (count-of-type "code/v05/coverage")
        x-cal   (count-of-type "code/v05/calls")
        x-voc   (count-of-type "code/v05/vocabulary-use")]
    (assert! (= bb-vars-distinct x-vars)
             (str "vars (distinct qname): bb=" bb-vars-distinct " xtdb=" x-vars
                  " (bb raw=" (:vars bb) "; difference of "
                  (- (:vars bb) bb-vars-distinct)
                  " duplicate defns is a real code-coherence finding)"))
    (assert! (= bb-tests x-tests)
             (str "tests: bb=" bb-tests " xtdb=" x-tests))
    (assert! (= bb-cov x-cov)
             (str "coverage edges: bb=" bb-cov " xtdb=" x-cov))
    ;; Directed-edge IDs are now distinct per direction (synthetic 3rd
    ;; endpoint marker; see E-substrate-2-directed-edge-id.md fix). Strict
    ;; equality expected.
    (assert! (= bb-cal x-cal)
             (str "call edges: bb=" bb-cal " xtdb=" x-cal))
    (assert! (= bb-voc x-voc)
             (str "vocab edges: bb=" bb-voc " xtdb=" x-voc))))

(defn -main [& argv]
  (let [{:keys [repo vocab ingest-script label]} (parse-args argv)]
    (when-not repo
      (println "ERROR: --repo is required") (System/exit 2))
    (when-not (fs/exists? ingest-script)
      (println "ERROR: ingest script not found at" ingest-script) (System/exit 2))
    (println "=== Phase-1 invariant tests ===")
    (println " repo: " repo)
    (println " vocab:" vocab)
    (println " label:" label "(used as :repo label-filter)")
    (println " script:" ingest-script)
    (println " futon1a:" FUTON1A)
    (println)
    (alter-var-root #'*label-filter* (constantly label))
    (println "[setup] run ingest once")
    (let [before (into {} (for [t ["code/v05/var" "code/v05/test" "code/v05/namespace"
                                   "code/v05/term" "code/v05/doc"
                                   "code/v05/calls" "code/v05/coverage"
                                   "code/v05/vocabulary-use" "code/v05/term-defines"]]
                            [t (count-of-type t)]))]
      (run-ingest ingest-script repo vocab label)
      (println)
      ;; L1 stable-id over each major type
      (doseq [t ["code/v05/var" "code/v05/test" "code/v05/namespace"
                 "code/v05/term" "code/v05/calls" "code/v05/coverage"
                 "code/v05/vocabulary-use" "code/v05/term-defines"]]
        (check-l1-stable-id t))
      (println)
      ;; L1 idempotency
      (check-l1-idempotency ingest-script repo vocab label)
      (println)
      ;; L2 endpoint-resolution
      (check-l2-endpoint-resolution)
      (println)
      (check-l2-vocab-resolution)
      (println)
      ;; counter-ratchet vs initial state
      (let [after (into {} (for [[t _] before]
                             [t (count-of-type t)]))]
        (check-counter-ratchet before after))
      (println)
      ;; regression
      (check-regression-vs-bb-v05 repo vocab))
    (println)
    (println "=== summary ===")
    (println " PASS:" @*pass-count*)
    (println " FAIL:" @*fail-count*)
    (System/exit (if (pos? @*fail-count*) 1 0))))

(apply -main *command-line-args*)
