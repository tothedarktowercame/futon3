(ns scripts.build_sigil_matrices
  "Scan devmaps/library flexiargs for sigil pairs and emit fake adjacency matrices."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io PushbackReader)))

(def search-root ["holes" "library"])
(def ^:private repo-root (.toPath (io/file ".")))

(defn- rel-path [file]
  (str (.relativize repo-root (.toPath file))))

(def file-pattern #"\.(devmap|flexiarg|md|org)$")

(defn candidate-files []
  (->> search-root
       (map io/file)
       (filter #(.exists %))
       (mapcat file-seq)
       (filter #(.isFile %))
       (filter #(re-find file-pattern (.getName %)))))

(defn sanitize [segment]
  (-> segment
      (str/replace #"^[\[\(\{]+" "")
      (str/replace #"[\]\)\},]+$" "")
      str/trim))

(def block-regex #"\[[^\]]+\]")
(def token-regex #"[^\s\[\]]+/[^\s\[\]]+")

(defn- emoji-like? [s]
  (and (string? s)
       (not (str/blank? s))
       (some #(> (int %) 255) s)))

(defn extract-sigils [text]
  (for [block (re-seq block-regex text)
        token (re-seq token-regex block)
        :let [[emoji hanzi] (str/split token #"/" 2)]
        :when (and (emoji-like? emoji)
                   hanzi
                   (not (str/blank? hanzi)))]
    {:emoji (sanitize emoji)
     :hanzi (sanitize hanzi)}))

(defn gather-sigils []
  (reduce (fn [acc file]
            (let [content (slurp file)
                  location (rel-path file)]
              (reduce (fn [acc {:keys [emoji hanzi]}]
                        (let [pair (str emoji "/" hanzi)]
                          (-> acc
                              (update :emoji conj emoji)
                              (update :hanzi conj hanzi)
                              (update-in [:pair-sources pair] (fnil conj #{}) location))))
                      acc
                      (extract-sigils content))))
          {:emoji #{} :hanzi #{} :pair-sources {}}
          (candidate-files)))

(defn- read-truth-table-order []
  (let [file (io/file "holes/256ca.el")]
    (when (.exists file)
      (with-open [r (java.io.PushbackReader. (io/reader file))]
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
                (map second entries))
              :else (recur))))))))

(defn- read-tokipona-order []
  (let [file (io/file "holes/tokipona.org")]
    (when (.exists file)
      (with-open [r (io/reader file)]
        (let [lines (line-seq r)
              emojis (keep (fn [line]
                              (when (and (str/starts-with? line "|")
                                         (not (str/starts-with? line "|---")))
                                (let [cols (map str/trim (str/split line #"\|"))]
                                  (when (>= (count cols) 2)
                                    (let [emoji (nth cols 1)]
                                      (when (and (seq emoji)
                                                 (not= (str/lower-case emoji) "emoji"))
                                        emoji))))))
                            lines)]
          (vec (distinct emojis)))))))

(defn ordered-vec [baseline coll]
  (letfn [(normalize-items [xs]
            (let [xs (cond
                       (nil? xs) []
                       (string? xs) [xs]
                       (coll? xs) xs
                       :else [xs])]
              (map (fn [v] (if (string? v) v (str v))) xs)))]
    (let [base (vec (remove str/blank? (normalize-items baseline)))
          extras (->> (normalize-items coll)
                      (remove str/blank?)
                    (remove (set base))
                    distinct
                    sort
                    vec)]
      (if (seq base)
        (vec (concat base extras))
        extras))))

(defn load-pattern-index []
  (let [file (io/file "resources/sigils/patterns-index.tsv")]
    (when (.exists file)
      (->> (str/split-lines (slurp file))
           (remove #(str/blank? %))
           (remove #(str/starts-with? % "#"))
           (map #(str/split % #"\t"))
           (keep (fn [[pattern tokipona truth rationale hotwords]]
                   (when (and tokipona truth)
                     {:pattern pattern
                      :tokipona tokipona
                      :truth truth
                      :rationale rationale
                      :hotwords hotwords})))))))

(defn adjacency [items]
  (let [order (if (seq items) items ["∅"])
        positions (zipmap order (range))
        normalizer (max 1 (dec (count order)))]
    {:order order
     :matrix (mapv (fn [a]
                     (mapv (fn [b]
                             (format "%.4f" (/ (double (Math/abs ^long (- (positions a) (positions b)))) normalizer)))
                           order))
                   order)}))

(defn ensure-dir [path]
  (let [dir (io/file path)]
    (.mkdirs dir)
    dir))

(defn write-csv [{:keys [order matrix]} file-path]
  (let [rows (map (fn [label row]
                    (str label "," (str/join "," row)))
                  order
                  matrix)
        header (str "," (str/join "," order))]
    (spit file-path (str header "\n" (str/join "\n" rows) "\n"))))

(defn write-index [emoji hanzi]
  (spit "resources/sigils/index.edn"
        (pr-str {:emoji emoji :hanzi hanzi})))

(defn -main [& _]
  (let [{:keys [emoji hanzi pair-sources]} (gather-sigils)
        emoji-base (or (read-tokipona-order) [])
        hanzi-base (vec (concat (or (read-truth-table-order) []) ["卯"]))
        emoji-order (ordered-vec emoji-base emoji)
        hanzi-order (ordered-vec hanzi-base hanzi)
        emoji-missing (seq (remove (set emoji-order) emoji))
        hanzi-missing (seq (remove (set hanzi-order) hanzi))
        collisions (->> pair-sources
                        (map (fn [[pair files]] [pair (sort files)]))
                        (filter (fn [[_ files]] (> (count files) 1)))
                        (sort-by (fn [[_ files]] (- (count files)))))]
    (ensure-dir "resources/sigils")
    (write-index emoji-order hanzi-order)
    (write-csv (adjacency emoji-order) "resources/sigils/emoji-adjacency.csv")
    (write-csv (adjacency hanzi-order) "resources/sigils/hanzi-adjacency.csv")
    (when emoji-missing
      (println "Unrecognized emoji sigils (not in baseline):" emoji-missing))
    (when hanzi-missing
      (println "Unrecognized hanzi sigils (not in baseline):" hanzi-missing))
    (if (seq collisions)
      (do
        (println (format "Sigil collisions detected (%d pairs with repeated sigils):" (count collisions)))
        (doseq [[pair files] collisions]
          (let [count-files (count files)
                alarm? (>= count-files 7)]
            (println (format " - %s (%d)%s ← %s"
                             pair
                             count-files
                             (if alarm? " ⚠" "")
                             (str/join ", " files))))))
      (println "No repeated sigil pairs detected."))
    (println (format "Wrote %d emoji and %d hanzi entries." (count emoji-order) (count hanzi-order)))))
