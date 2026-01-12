(ns scripts.sigil-allowlist
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io PushbackReader)))

(defn tokipona-emoji-order []
  (let [file (io/file "holes/tokipona.org")]
    (when (.exists file)
      (with-open [r (io/reader file)]
        (->> (line-seq r)
             (filter #(and (str/starts-with? % "|")
                           (not (str/starts-with? % "|---"))))
             (map #(map str/trim (str/split % #"\|")))
             (keep (fn [cols]
                     (when (>= (count cols) 2)
                       (let [emoji (nth cols 1)]
                         (when (and (seq emoji)
                                    (not= (str/lower-case emoji) "emoji"))
                           emoji)))))
             distinct
             vec)))))

(defn truth-table-hanzi-order []
  (let [file (io/file "holes/256ca.el")]
    (when (.exists file)
      (with-open [r (PushbackReader. (io/reader file))]
        (loop []
          (let [form (read r false ::eof)]
            (cond
              (= form ::eof) nil
              (and (seq? form)
                   (= 'defvar (first form))
                   (= 'truth-table-8 (second form)))
              (let [table (nth form 2)
                    entries (if (and (seq? table) (= 'quote (first table)))
                              (second table)
                              table)]
                (->> entries
                     (map second)
                     (remove str/blank?)
                     vec))
              :else (recur))))))))

(defn tokipona-emoji-set []
  (set (tokipona-emoji-order)))

(defn truth-table-hanzi-set []
  (set (truth-table-hanzi-order)))

(defn ensure-allowlist!
  [emoji-order hanzi-order]
  (when-not (seq emoji-order)
    (throw (ex-info "tokipona emoji allowlist missing or empty"
                    {:file "holes/tokipona.org"})))
  (when-not (seq hanzi-order)
    (throw (ex-info "truth-table hanzi allowlist missing or empty"
                    {:file "holes/256ca.el"}))))
