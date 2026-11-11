(ns futon3.pattern-index.derivation-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3.pattern-index.derivation :as derivation]))

(def ^:private repo-root
  (.getCanonicalPath (io/file ".")))

(defn- temp-dir []
  (let [d (io/file (System/getProperty "java.io.tmpdir")
                   (str "pattern-derivation-" (System/nanoTime)))]
    (.mkdirs d)
    (.getCanonicalPath d)))

(defn- copy! [root rel]
  (let [dst (io/file root rel)]
    (.mkdirs (.getParentFile dst))
    (io/copy (io/file repo-root rel) dst)))

(defn- fixture-root []
  (let [root (temp-dir)]
    (doseq [rel ["holes/tokipona.org"
                 "holes/features/futon3a.devmap"
                 "library/agent/handoff-preserves-context.flexiarg"
                 "library/cascade-construction/read-what-exists-first.flexiarg"]]
      (copy! root rel))
    root))

(deftest fixture-sidecar-is-the-canonical-export
  (let [root (fixture-root)
        db (str (temp-dir) "/index.sqlite")
        export (str (temp-dir) "/patterns-index.tsv")
        before (derivation/canonical-entries root)]
    (derivation/write-sidecar! db before "fixture-head-1")
    (is (= (derivation/render-tsv before)
           (derivation/render-tsv (derivation/sidecar-entries db))))
    (is (seq (filter #(= "devmap" (:kind %)) before))
        "a real devmap prototype is in the canonical snapshot")
    (is (true? (derivation/write-tsv! export
                                      (derivation/sidecar-entries db))))
    (is (nil? (derivation/write-tsv! export
                                     (derivation/sidecar-entries db)))
        "unchanged compatibility exports are not rewritten")
    (testing "one pattern edit changes exactly its lexical row"
      (let [file (io/file root "library/agent/handoff-preserves-context.flexiarg")]
        (spit file (.replace (slurp file)
                             "@title Handoff Preserves Context"
                             "@title Handoff Carries Context")))
      (let [after (derivation/canonical-entries root)
            changed (->> (map vector before after)
                         (keep (fn [[a b]] (when (not= a b) [(:id a) (:id b)]))))]
        (is (= [["agent/handoff-preserves-context"
                 "agent/handoff-preserves-context"]]
               changed))
        (derivation/write-sidecar! db after "fixture-head-2")
        (is (= (derivation/render-tsv after)
               (derivation/render-tsv (derivation/sidecar-entries db))))))))

(deftest whole-live-index-equality
  (let [entries (derivation/canonical-entries repo-root)
        db (str (temp-dir) "/index.sqlite")
        exported (slurp "resources/sigils/patterns-index.tsv")]
    (is (= (dec (count (str/split-lines exported))) (count entries)))
    (derivation/write-sidecar! db entries "read-only-live-check")
    (is (= exported
           (derivation/render-tsv entries)
           (derivation/render-tsv (derivation/sidecar-entries db))))))
