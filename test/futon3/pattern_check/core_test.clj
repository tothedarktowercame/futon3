(ns futon3.pattern-check.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3.pattern-check.core :as core]))

(deftest extract-sigils-basic
  (testing "extracts emoji/hanzi tokens"
    (is (= ["🐜/予"] (core/extract-sigils "hello 🐜/予 world")))
    (is (= ["🐜/予"] (core/extract-sigils "(🐜/予),")))))

(deftest validate-sigils-basic
  (testing "validates sigils and reports invalid"
    (let [{:keys [sigils-checked sigils-valid invalid-sigils]} (core/validate-sigils ["🐜/予" "🐜/A"] false)]
      (is (= 2 sigils-checked))
      (is (= 1 sigils-valid))
      (is (= ["🐜/A"] invalid-sigils)))))

(deftest detect-psr-pur-basic
  (testing "detects structured PSR/PUR events"
    (let [lines [{:payload {:event :pattern/selection-claimed
                            :pattern/id "stack-blocker-detection"
                            :session/id "s1"
                            :decision/id "d1"}}
                 {:payload {:event :pattern/use-claimed
                            :pattern/id "stack-blocker-detection"
                            :session/id "s1"
                            :decision/id "d1"}}]
          {:keys [psr-seen pur-seen psr-ids pur-ids]} (core/detect-psr-pur lines true)]
      (is (= 1 psr-seen))
      (is (= 1 pur-seen))
      (is (= ["stack-blocker-detection"] psr-ids))
      (is (= ["stack-blocker-detection"] pur-ids)))))

(deftest buffer-ingest-and-flush
  (testing "ingest and flush by max-lines"
    (let [state (core/new-buffer {:max-lines 2 :max-buffer 2})
          state (core/ingest-lines state ["a" "b"] 0)
          [state batch] (core/flush-ready state 0)]
      (is (some? batch))
      (is (= 2 (count (:lines batch))))
      (is (= 0 (:dropped-lines batch)))
      (is (empty? (:queue state)))))
  (testing "drops oldest when max-buffer exceeded"
    (let [state (core/new-buffer {:max-lines 10 :max-buffer 2})
          state (core/ingest-lines state ["a" "b" "c"] 0)]
      (is (= 2 (count (:queue state))))
      (is (= 1 (:dropped-lines state))))))

(deftest duplicate-index-basic
  (testing "detects duplicate sigils across files"
    (let [dir (java.nio.file.Files/createTempDirectory "sigil-test" (make-array java.nio.file.attribute.FileAttribute 0))
          f1 (.toFile (java.nio.file.Files/createTempFile dir "a" ".txt" (make-array java.nio.file.attribute.FileAttribute 0)))
          f2 (.toFile (java.nio.file.Files/createTempFile dir "b" ".txt" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (spit f1 "🐜/予")
      (spit f2 "🐜/予")
      (let [index (core/build-duplicate-index [(.getPath f1) (.getPath f2)] 0)]
        (is (contains? (:duplicate-sigils index) "🐜/予"))))))

(deftest duplicate-index-glob
  (testing "expands recursive globs for duplicate detection"
    (let [dir (java.nio.file.Files/createTempDirectory "sigil-glob" (make-array java.nio.file.attribute.FileAttribute 0))
          sub (java.nio.file.Files/createDirectories (.resolve dir "nested") (make-array java.nio.file.attribute.FileAttribute 0))
          f1 (.toFile (java.nio.file.Files/createTempFile dir "a" ".edn" (make-array java.nio.file.attribute.FileAttribute 0)))
          f2 (.toFile (java.nio.file.Files/createTempFile sub "b" ".edn" (make-array java.nio.file.attribute.FileAttribute 0)))
          pattern (str (.toString dir) "/**/*.edn")]
      (spit f1 "🐜/予")
      (spit f2 "🐜/予")
      (let [index (core/build-duplicate-index [pattern] 0)]
        (is (contains? (:duplicate-sigils index) "🐜/予"))
        (is (<= 2 (count (get (:sigil->locations index) "🐜/予"))))))))

(deftest duplicate-index-dir-path
  (testing "accepts directory paths"
    (let [dir (java.nio.file.Files/createTempDirectory "sigil-dir" (make-array java.nio.file.attribute.FileAttribute 0))
          f1 (.toFile (java.nio.file.Files/createTempFile dir "a" ".txt" (make-array java.nio.file.attribute.FileAttribute 0)))
          f2 (.toFile (java.nio.file.Files/createTempFile dir "b" ".txt" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (spit f1 "🐜/予")
      (spit f2 "🐜/予")
      (let [index (core/build-duplicate-index [(.getPath (.toFile dir))] 0)]
        (is (contains? (:duplicate-sigils index) "🐜/予"))))))
