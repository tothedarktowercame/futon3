(ns scripts.pattern-pull-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [scripts.pattern-pull :as pattern-pull])
  (:import (java.nio.file Files)))

(defn- fake-index [& ids]
  (let [entries (->> ids
                     (map (fn [id]
                            {:id id
                             :basename (last (str/split id #"/"))
                             :relative-path (str id ".flexiarg")}))
                     vec)]
    {:by-id (into {} (map (juxt :id identity) entries))
     :by-basename (reduce (fn [acc entry]
                            (update acc (:basename entry) (fnil conj []) entry))
                          {}
                          entries)
     :top-level-dirs (->> entries
                          (map :id)
                          (map #(first (str/split % #"/")))
                          set)}))

(deftest resolves-joe-style-pattern-names
  (let [index (pattern-pull/pattern-library-index ".")
        text (str/join
              "\n"
              ["| Pattern |"
               "|---------|"
               "| handoff-preserves-context |"
               "| scope-before-action |"
               "| evidence-over-assertion |"
               "| coordination-has-cost |"
               "| student-dispatch |"
               "| mission-lifecycle |"])
        {:keys [patterns errors]} (pattern-pull/resolve-mentioned-patterns index text)]
    (is (empty? errors))
    (is (= ["agent/handoff-preserves-context"
            "agent/scope-before-action"
            "agent/evidence-over-assertion"
            "agent/coordination-has-cost"
            "agent/student-dispatch"
            "futon-theory/mission-lifecycle"]
           (mapv :id patterns)))))

(deftest ambiguous-bare-mentions-fail
  (let [index (fake-index "storage/durability-first" "futon-theory/durability-first")
        {:keys [patterns errors]} (pattern-pull/resolve-mentioned-patterns index "durability-first")]
    (is (empty? patterns))
    (is (= 1 (count errors)))
    (is (re-find #"Ambiguous pattern mention durability-first" (first errors)))))

(deftest explicit-missing-library-reference-fails
  (let [index (fake-index "agent/handoff-preserves-context")
        {:keys [patterns errors]} (pattern-pull/resolve-mentioned-patterns index "library/agent/not-real.flexiarg")]
    (is (empty? patterns))
    (is (= ["Missing explicit library pattern reference: library/agent/not-real.flexiarg"]
           errors))))

(deftest non-pattern-code-paths-are-ignored
  (let [index (fake-index "agency/invariants" "agent/handoff-preserves-context")
        {:keys [patterns errors]} (pattern-pull/resolve-mentioned-patterns index "`agency/logic.clj` and handoff-preserves-context")]
    (is (empty? errors))
    (is (= ["agent/handoff-preserves-context"]
           (mapv :id patterns)))))

(deftest copy-patterns-preserve-library-layout
  (let [index (pattern-pull/pattern-library-index ".")
        {:keys [patterns errors]} (pattern-pull/resolve-mentioned-patterns index "handoff-preserves-context")
        tmpdir (-> (Files/createTempDirectory "pattern-pull-test"
                                              (make-array java.nio.file.attribute.FileAttribute 0))
                   (.toFile))]
    (is (empty? errors))
    (with-out-str
      (pattern-pull/copy-patterns! patterns (.getAbsolutePath tmpdir) false))
    (is (.exists (io/file tmpdir "agent/handoff-preserves-context.flexiarg")))))
