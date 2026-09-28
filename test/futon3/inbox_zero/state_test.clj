(ns futon3.inbox-zero.state-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.state :as state]))

(def fixture-records
  (edn/read-string (slurp (io/resource "fixtures/inbox_zero_state.edn"))))

(def seat (first fixture-records))
(def observation (second fixture-records))
(def claim (nth fixture-records 2))

(defn temp-state-path []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "inbox-zero-state-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.getPath (io/file dir "state.edn"))))

(deftest fixture-replays-to-canonical-state
  (let [stored (state/replay fixture-records)]
    (is (= 3 (count (:records stored))))
    (is (= [observation]
           (state/records-of-type stored :inbox-zero/file-observation)))
    (is (= stored (reduce state/apply-record stored fixture-records))
        "identical record replay is idempotent")))

(deftest claim-requires-a-witnessed-seat
  (let [error (try
                (state/apply-record (state/empty-state) claim)
                nil
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :inbox-zero/unknown-seat (:error/type (ex-data error))))))

(deftest ids-are-immutable
  (let [stored (state/apply-record (state/empty-state) seat)
        error (try
                (state/apply-record stored (assoc seat :host/id "zone"))
                nil
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :inbox-zero/id-conflict (:error/type (ex-data error))))))

(deftest registry-witness-must-match-seat
  (let [error (try
                (state/validate-record
                 (assoc-in seat [:registry-witness :session/id] "other-session"))
                nil
                (catch clojure.lang.ExceptionInfo e e))]
    (is (= :inbox-zero/invalid-record (:error/type (ex-data error))))))

(deftest durable-store-round-trips-and-replays
  (let [path (temp-state-path)]
    (doseq [record fixture-records]
      (state/append-record! path record))
    (let [before (state/load-state path)
          after (state/append-record! path claim)]
      (is (= before after))
      (is (= 3 (count (:records after)))))))

(deftest corrupt-store-fails-closed
  (let [path (temp-state-path)]
    (spit path "{not-edn")
    (is (thrown? Exception (state/load-state path)))
    (is (thrown? Exception (state/append-record! path seat)))
    (is (= "{not-edn" (slurp path))
        "failed append must not replace corrupt evidence with an empty store")))

(deftest deleted-observation-cannot-retain-content-hash
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Deleted observations"
       (state/validate-record
        (assoc observation :git/status :deleted :content/hash "sha256:stale")))))

(defn- cursor [n prior]
  {:record/type :inbox-zero/commit-scan-cursor
   :cursor/id (str "cursor:" n) :worktree/id "worktree:w"
   :cursor/sha (str "sha-" n) :cursor/reason (if prior :advance :baseline)
   :prior/cursor-id prior :observed-at (java.util.Date. (* 1000 n))})

(deftest second-root-cursor-for-a-worktree-fails-closed
  (let [stored (state/apply-record (state/empty-state) (cursor 0 nil))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"would fork"
                          (state/apply-record stored (assoc (cursor 1 nil) :cursor/id "cursor:other-root"))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"would fork"
                          (-> stored
                              (state/apply-record (cursor 1 "cursor:0"))
                              (state/apply-record (assoc (cursor 2 "cursor:0") :cursor/id "cursor:fork")))))))

(deftest large-snapshot-loads-in-linear-time
  ;; Quadratic cursor checks made a ~290k-record snapshot take minutes per
  ;; load (2026-09-28). 2,000 chained cursors beside 20,000 observations must
  ;; load, re-validated from disk, well inside a second or two.
  (let [path (temp-state-path)
        cursors (map #(cursor % (when (pos? %) (str "cursor:" (dec %)))) (range 2000))
        observations (map #(assoc observation
                                  :observation/id (str "obs:" %)
                                  :path (str "src/f" % ".clj"))
                          (range 20000))
        built (reduce state/apply-record (state/empty-state)
                      (concat [seat] observations cursors))]
    (spit path (pr-str built))
    (let [t0 (System/nanoTime)
          loaded (state/load-state path)
          ms (/ (- (System/nanoTime) t0) 1e6)]
      (is (= built loaded))
      (is (< ms 5000) (str "load took " ms " ms")))))

(deftest cached-load-is-revalidated-after-an-outside-write
  (let [path (temp-state-path)]
    (doseq [record fixture-records]
      (state/append-record! path record))
    (is (= 3 (count (:records (state/load-state path)))))
    (is (identical? (state/load-state path) (state/load-state path))
        "an unchanged file is not re-read")
    (Thread/sleep 5)
    (spit path "{not-edn")
    (is (thrown? Exception (state/load-state path))
        "an outside write is read and validated again")))
