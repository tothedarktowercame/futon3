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
