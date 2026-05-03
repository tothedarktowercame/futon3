#!/usr/bin/env bb
;; Phase-4.6 regression test for cross-root move semantics.
;;
;; Exercises the watcher across two roots:
;;   1. Create one source file under root X.
;;   2. Cold-scan both roots.
;;   3. Move the file from root X to root Y.
;;   4. Assert:
;;      - cross-root move event observed
;;      - old root vertices are marked stale
;;      - new root vertices are live under the new source-file
;;      - a directed :edge/renamed-to link connects old→new logical vertices

(require '[clojure.test :refer [deftest is run-tests]]
         '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http]
         '[clojure.edn :as edn])

(declare detect-cross-root-moves)

(load-file "/home/joe/code/futon3/scripts/multi_watcher.clj")

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def WATCHER-SCRIPT "/home/joe/code/futon3/scripts/multi_watcher.clj")

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER}
                            :throw false})]
    (when (= 200 (:status resp))
      (edn/read-string (:body resp)))))

(defn fetch-by-end [eid]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?end="
                             (java.net.URLEncoder/encode eid "UTF-8")))]
    (or (:hyperedges r) [])))

(defn prop-get [h k]
  (or (get-in h [:hx/props k])
      (get-in h [:hx/props (name k)])))

(defn type-str [x]
  (cond
    (keyword? x) (if (namespace x)
                   (str (namespace x) "/" (name x))
                   (name x))
    (string? x) x
    :else (str x)))

(deftest detect-cross-root-moves-test
  (let [plans [{:root "/tmp/futonX"
                :label "futonX"
                :cache {"/tmp/futonX/a.clj" {:hash "abc"}}
                :snapshot {}
                :moves {:deleted ["/tmp/futonX/a.clj"] :added [] :renamed []}}
               {:root "/tmp/futonY"
                :label "futonY"
                :cache {}
                :snapshot {"/tmp/futonY/b.clj" {:hash "abc"}}
                :moves {:deleted [] :added ["/tmp/futonY/b.clj"] :renamed []}}]
        moves (detect-cross-root-moves plans)]
    (is (= [{:from "/tmp/futonX/a.clj"
             :from-root "/tmp/futonX"
             :from-label "futonX"
             :hash "abc"
             :to "/tmp/futonY/b.clj"
             :to-root "/tmp/futonY"
             :to-label "futonY"}]
           moves))))

(deftest detect-cross-root-moves-ambiguous-hash-test
  (let [plans [{:root "/tmp/futonX"
                :label "futonX"
                :cache {"/tmp/futonX/a.py" {:hash "dup"}
                        "/tmp/futonX/b.py" {:hash "dup"}}
                :snapshot {}
                :moves {:deleted ["/tmp/futonX/a.py" "/tmp/futonX/b.py"]
                        :added []
                        :renamed []}}
               {:root "/tmp/futonY"
                :label "futonY"
                :cache {}
                :snapshot {"/tmp/futonY/a.py" {:hash "dup"}
                           "/tmp/futonY/b.py" {:hash "dup"}}
                :moves {:deleted []
                        :added ["/tmp/futonY/a.py" "/tmp/futonY/b.py"]
                        :renamed []}}]]
    (is (= [] (detect-cross-root-moves plans))
        "ambiguous same-hash batches should not be auto-paired")))

(deftest live-cross-root-move-test
  (let [parent (str (fs/create-temp-dir {:prefix "substrate2-cross-root-"}))
        root-x (str parent "/futonX")
        root-y (str parent "/futonY")
        _ (fs/create-dirs root-x)
        _ (fs/create-dirs root-y)
        stamp (System/currentTimeMillis)
        label-x (str "futonX-cross-" stamp)
        label-y (str "futonY-cross-" stamp)
        ns-name (str "watcher.cross.root.t" stamp)
        old-file (str root-x "/alpha.clj")
        new-file (str root-y "/alpha.clj")
        qname (str ns-name "/hopped")
        old-end (str label-x "/" qname)
        new-end (str label-y "/" qname)
        content (str "(ns " ns-name ")\n"
                     "(defn hopped [] :ok)\n")]
    (try
      (spit old-file content)
      (let [watcher (future
                      (sh "bb" WATCHER-SCRIPT
                          "--root" (str root-x "=" label-x)
                          "--root" (str root-y "=" label-y)
                          "--interval-ms" "500"
                          "--max-cycles" "12"))]
        (try
          (Thread/sleep 1800)
          (fs/move old-file new-file)
          (let [{:keys [exit err out]} @watcher
                old-var (->> (fetch-by-end old-end)
                             (filter #(= "code/v05/var" (type-str (:hx/type %))))
                             first)
                new-var (->> (fetch-by-end new-end)
                             (filter #(= "code/v05/var" (type-str (:hx/type %))))
                             first)
                rename-link (->> (fetch-by-end old-end)
                                 (filter #(= "edge/renamed-to" (type-str (:hx/type %))))
                                 (filter #(some #{new-end} (:hx/endpoints %)))
                                 first)]
            (is (zero? exit) (or err "watcher exited 0"))
            (is (re-find (re-pattern (java.util.regex.Pattern/quote
                                      (str "[cross-root-move] " old-file " → " new-file)))
                         out)
                "cross-root move event observed in watcher output")
            (is (re-find (re-pattern (java.util.regex.Pattern/quote
                                      (str "[cross-root-cascade] " old-file " → " new-file)))
                         out)
                "cross-root cascade observed in watcher output")
            (is (some? old-var) "old logical var can still be fetched directly")
            (is (some? new-var) "new logical var can be fetched directly")
            (is (= old-file (prop-get old-var :source-file))
                "old logical var still points at the vacated source-file")
            (is (= new-file (prop-get new-var :source-file))
                "new logical var points at the destination source-file")
            (is (true? (prop-get old-var :edge/witness-stale))
                "old logical var is stale after the cross-root move")
            (is (not (true? (prop-get new-var :edge/witness-stale)))
                "new logical var remains live")
            (is (some? rename-link)
                "directed old→new renamed-to edge was emitted"))
          (finally
            @watcher)))
      (finally
        (when (fs/exists? parent)
          (fs/delete-tree parent))))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (pos? (+ fail error)) 1 0)))
