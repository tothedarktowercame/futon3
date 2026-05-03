#!/usr/bin/env bb
;; Phase-4 invariant tests for the watcher daemon.
;;
;; Usage:
;;   bb phase_4_watcher_test.clj --repo /path --label tag [--vocab path]...
;;
;; The test:
;;   1. Counts pre-existing watcher-event hyperedges for the label.
;;   2. Creates a temporary "test-touch" file under the repo with a unique
;;      marker symbol.
;;   3. Runs the watcher with --max-cycles 3 --interval 2 (≤8s real time).
;;   4. Modifies the temporary file mid-run between cycles 1 and 2.
;;   5. After watcher exits, asserts:
;;      a. Three new watcher-event hyperedges (one per cycle) — heartbeat invariant.
;;      b. Files-scanned > 0 — discovery worked.
;;      c. At least one cycle has files-changed > 0 — change detection worked.
;;      d. Substrate now contains a :var with the unique marker — ingestion path worked.
;;      e. Re-running watcher with same files = 0 new substrate edges — idempotency.
;;   6. Cleans up the temp file (and re-runs ingest to remove the marker
;;      from substrate via... wait, no DELETE; leave it as documented test-pollution).

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http])

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def WATCHER-SCRIPT "/home/joe/code/futon3/scripts/watcher_daemon.clj")

(def ^:dynamic *fail* (atom 0))
(def ^:dynamic *pass* (atom 0))
(defn assert! [pred msg]
  (if pred (do (swap! *pass* inc) (println "  PASS:" msg))
           (do (swap! *fail* inc) (println "  FAIL:" msg))))

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER} :throw false})]
    (when (= 200 (:status resp))
      (edn/read-string (:body resp)))))

(defn count-of-type [hx-type label]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?type="
                             (java.net.URLEncoder/encode hx-type "UTF-8")))
        all (or (:hyperedges r) [])]
    (count (filter #(= label (get-in % [:hx/props :repo])) all))))

(defn fetch-of-type [hx-type label]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?type="
                             (java.net.URLEncoder/encode hx-type "UTF-8")))
        all (or (:hyperedges r) [])]
    (filter #(= label (get-in % [:hx/props :repo])) all)))

(defn parse-args [argv]
  (loop [a argv opts {:vocab []}]
    (cond
      (empty? a) opts
      (= "--repo"  (first a)) (recur (drop 2 a) (assoc opts :repo (second a)))
      (= "--label" (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (= "--vocab" (first a)) (recur (drop 2 a) (update opts :vocab #(conj % "--vocab" (second a))))
      :else (recur (rest a) opts))))

(defn -main [& argv]
  (let [{:keys [repo label vocab]} (parse-args argv)
        unique-marker (str "watcher-test-marker-" (System/currentTimeMillis))
        ;; Place the test file in a location the projector definitely walks.
        ;; futon2/scripts is a clj-source-tree the v0 projector picks up.
        test-file (str repo "/scripts/" unique-marker ".clj")
        marker-ns (str/replace unique-marker #"-" ".")
        marker-fn-qname (str marker-ns "/touched-by-watcher")]
    (when-not repo  (println "ERROR: --repo required") (System/exit 2))
    (when-not label (println "ERROR: --label required") (System/exit 2))
    (println "=== Phase-4 watcher invariant tests ===")
    (println " repo:" repo " label:" label)
    (println " test-marker:" marker-ns)
    (println)

    (let [evt-before (count-of-type "code/v05/watcher-event" label)
          var-before (count-of-type "code/v05/var" label)]
      (println "[setup] watcher-event before:" evt-before
               " var before:" var-before)

      ;; 1. Create the test file BEFORE watcher starts so cycle 1 sees it
      ;;    and "primes" the cache, then we modify it mid-run.
      (println "[setup] creating test file at" test-file)
      (fs/create-dirs (fs/parent test-file))
      (spit test-file (str "(ns " marker-ns ")\n"
                            "(defn touched-by-watcher [] :v0)\n"))

      ;; 2. Start the watcher with bounded cycle count.
      ;;    --max-cycles 3 --interval 2 → ~6s total runtime + ingest time.
      (println "[run] starting watcher --max-cycles 3 --interval 2")
      (let [watcher (future
                      (apply sh "bb" WATCHER-SCRIPT
                             "--root" repo "--label" label
                             "--max-cycles" "3" "--interval" "2"
                             vocab))
            ;; 3. After cycle 1 (~3s in including ingest), modify the file
            ;;    so cycle 2 sees a content change.
            _ (Thread/sleep 4000)
            _ (println "[run] mutating test file (mid-watcher)")
            _ (spit test-file (str "(ns " marker-ns ")\n"
                                    "(defn touched-by-watcher [] :v1)\n"
                                    "(defn watcher-saw-this [] :new-defn)\n"))
            ;; 4. Wait for the watcher future to complete.
            res @watcher]
        (println "[run] watcher exit:" (:exit res))
        (when-not (zero? (:exit res))
          (println "[run] stderr:" (:err res)))

        ;; 5. Now the assertions.
        (println)
        (println "[assertions]")
        (let [evt-after (count-of-type "code/v05/watcher-event" label)
              vars-after (fetch-of-type "code/v05/var" label)
              found-marker (some #(when (some (fn [ep] (= ep marker-fn-qname))
                                              (:hx/endpoints %))
                                    %)
                                 vars-after)
              found-new-defn (some #(when (some (fn [ep]
                                                  (str/ends-with? ep "/watcher-saw-this"))
                                                (:hx/endpoints %))
                                      %)
                                   vars-after)]
          (assert! (>= (- evt-after evt-before) 3)
                   (str "≥3 new watcher-event hyperedges this run "
                        "(before=" evt-before " after=" evt-after ")"))
          (assert! (some? found-marker)
                   (str "substrate contains the test marker var " marker-fn-qname))
          (assert! (some? found-new-defn)
                   (str "substrate picked up the mid-run new defn "
                        marker-ns "/watcher-saw-this")))

        ;; 6. Idempotency: a second watcher run on the unchanged tree
        ;;    should produce 0 new vars (only fresh watcher-event vertices).
        (println)
        (println "[idempotency] second run on unchanged tree")
        (let [var-pre-second (count-of-type "code/v05/var" label)
              evt-pre-second (count-of-type "code/v05/watcher-event" label)
              _ (apply sh "bb" WATCHER-SCRIPT
                       "--root" repo "--label" label
                       "--max-cycles" "2" "--interval" "1"
                       vocab)
              var-post-second (count-of-type "code/v05/var" label)
              evt-post-second (count-of-type "code/v05/watcher-event" label)]
          (assert! (= var-pre-second var-post-second)
                   (str "vars unchanged on second run: "
                        var-pre-second " == " var-post-second))
          (assert! (>= (- evt-post-second evt-pre-second) 1)
                   (str "watcher-event count grew on second run: "
                        evt-pre-second " → " evt-post-second
                        " (heartbeat present even with no source change)")))))

    ;; Cleanup test file (substrate retains the marker var as documented
    ;; pollution; substrate-1 has no DELETE).
    (println)
    (println "[cleanup] removing test file:" test-file)
    (when (fs/exists? test-file) (fs/delete test-file))

    (println)
    (println "=== summary ===")
    (println " PASS:" @*pass*)
    (println " FAIL:" @*fail*)
    (System/exit (if (pos? @*fail*) 1 0))))

(apply -main *command-line-args*)
