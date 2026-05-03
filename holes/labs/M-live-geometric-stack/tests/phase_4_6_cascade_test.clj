#!/usr/bin/env bb
;; Phase-4.6 regression test for rename/deletion cascade semantics.
;;
;; Exercises the current multi-repo watcher in one live run:
;;   1. Create one Clojure source file in a temporary repo root.
;;   2. Start the watcher with bounded cycles.
;;   3. Rename the file while the watcher is running.
;;   4. Delete the renamed file while the watcher is still running.
;;   5. Assert:
;;      - rename-event observed
;;      - deletion-event observed
;;      - old source-file no longer owns vertices
;;      - surviving vertices for the renamed path are marked stale
;;
;; Also unit-tests deterministic old→new vertex pairing used for
;; :edge/renamed-to emission.

(require '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests]]
         '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http]
         '[clojure.edn :as edn])

(load-file "/home/joe/code/futon3/scripts/multi_watcher.clj")

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def WATCHER-SCRIPT "/home/joe/code/futon3/scripts/multi_watcher.clj")

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER}
                            :throw false})]
    (when (= 200 (:status resp))
      (edn/read-string (:body resp)))))

(defn fetch-of-type [hx-type label]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?type="
                             (java.net.URLEncoder/encode hx-type "UTF-8")))]
    (->> (or (:hyperedges r) [])
         (filter #(= label (or (get-in % [:hx/props :repo])
                               (get-in % [:hx/props "repo"]))))
         vec)))

(defn fetch-by-end [eid]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?end="
                             (java.net.URLEncoder/encode eid "UTF-8")))]
    (or (:hyperedges r) [])))

(defn prop-get [h k]
  (or (get-in h [:hx/props k])
      (get-in h [:hx/props (name k)])))

(deftest deterministic-rename-pairs-test
  (let [old [{:hx/type "code/v05/namespace"
              :hx/endpoints ["demo/old.ns"]
              :hx/props {:namespace "old.ns"}}
             {:hx/type "code/v05/var"
              :hx/endpoints ["demo/old.ns/foo"]
              :hx/props {:var/qname "old.ns/foo"}}]
        new [{:hx/type "code/v05/namespace"
              :hx/endpoints ["demo/new.ns"]
              :hx/props {:namespace "new.ns"}}
             {:hx/type "code/v05/var"
              :hx/endpoints ["demo/new.ns/foo"]
              :hx/props {:var/qname "new.ns/foo"}}]
        pairs (deterministic-rename-pairs old new)]
    (is (= 2 (count pairs)))
    (is (= #{["demo/old.ns" "demo/new.ns"]
             ["demo/old.ns/foo" "demo/new.ns/foo"]}
           (set (map (fn [{:keys [from to]}]
                       [(first (:hx/endpoints from))
                        (first (:hx/endpoints to))])
                     pairs))))))

(deftest live-rename-then-delete-cascade-test
  (let [root (str (fs/create-temp-dir {:prefix "substrate2-cascade-"}))
        stamp (System/currentTimeMillis)
        label (str "cascade-test-" stamp)
        ns-name (str "watcher.cascade.t" stamp)
        old-file (str root "/alpha.clj")
        new-file (str root "/beta.clj")
        qname (str ns-name "/touched")
        content (str "(ns " ns-name ")\n"
                     "(defn touched [] :ok)\n")]
    (try
      (spit old-file content)
      (let [rename-watcher (future
                             (sh "bb" WATCHER-SCRIPT
                                 "--root" (str root "=" label)
                                 "--interval-ms" "500"
                                 "--max-cycles" "12"))]
        (Thread/sleep 1400)
        (fs/move old-file new-file)
        (let [{:keys [exit err out]} @rename-watcher
              tracked-var (->> (fetch-by-end (str label "/" qname))
                               (filter #(= "code/v05/var" (type-str (:hx/type %))))
                               first)]
          (is (zero? exit) (or err "rename watcher exited 0"))
          (is (re-find (re-pattern (java.util.regex.Pattern/quote
                                    (str "[rename] " old-file " → " new-file)))
                       out)
              "rename-event observed in watcher output")
          (is (re-find (re-pattern (java.util.regex.Pattern/quote
                                    (str "[rename-cascade] " old-file " → " new-file)))
                       out)
              "rename-cascade observed in watcher output")
          (is (some? tracked-var) "tracked var can be fetched directly by endpoint after rename")
          (is (= new-file (prop-get tracked-var :source-file))
              "logical var was rebound to the renamed source-file")))
      (let [delete-watcher (future
                             (sh "bb" WATCHER-SCRIPT
                                 "--root" (str root "=" label)
                                 "--interval-ms" "500"
                                 "--max-cycles" "12"))]
        (Thread/sleep 1400)
        (when (fs/exists? new-file)
          (fs/delete new-file))
        (let [{:keys [exit err out]} @delete-watcher
              tracked-var (->> (fetch-by-end (str label "/" qname))
                               (filter #(= "code/v05/var" (type-str (:hx/type %))))
                               first)]
          (is (zero? exit) (or err "delete watcher exited 0"))
          (is (re-find (re-pattern (java.util.regex.Pattern/quote
                                    (str "[deletion] " new-file)))
                       out)
              "deletion-event observed in watcher output")
          (is (re-find (re-pattern (java.util.regex.Pattern/quote
                                    (str "[deletion-stale] " new-file)))
                       out)
              "deletion stale-cascade observed in watcher output")
          (is (some? tracked-var) "tracked var still exists after delete (ghosted, not removed)")
          (is (= new-file (prop-get tracked-var :source-file))
              "logical var still points at the last live source-file after delete")
          (is (true? (prop-get tracked-var :edge/witness-stale))
              "logical var is stale after deletion of the renamed path")))
      (finally
        (when (fs/exists? root)
          (fs/delete-tree root))))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (pos? (+ fail error)) 1 0)))
