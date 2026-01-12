(ns scripts.build_sigil_matrices
  "Scan devmaps/library flexiargs for sigil pairs and emit fake adjacency matrices."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [scripts.sigil-allowlist :as allow]))

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

(defn gather-sigils [emoji-set hanzi-set]
  (reduce (fn [acc file]
            (let [content (slurp file)
                  location (rel-path file)]
              (reduce (fn [acc {:keys [emoji hanzi]}]
                        (let [pair (str emoji "/" hanzi)]
                          (if (and (contains? emoji-set emoji)
                                   (contains? hanzi-set hanzi))
                            (-> acc
                                (update :emoji conj emoji)
                                (update :hanzi conj hanzi)
                                (update-in [:pair-sources pair] (fnil conj #{}) location))
                            (update-in acc [:invalid pair] (fnil conj #{}) location))))
                      acc
                      (extract-sigils content))))
          {:emoji #{} :hanzi #{} :pair-sources {} :invalid {}}
          (candidate-files)))

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

(defn- strict? [args]
  (or (some #{"--strict"} args)
      (seq (System/getenv "SIGIL_STRICT"))))

(defn -main [& args]
  (let [emoji-base (or (allow/tokipona-emoji-order) [])
        hanzi-base (or (allow/truth-table-hanzi-order) [])
        _ (allow/ensure-allowlist! emoji-base hanzi-base)
        emoji-set (set emoji-base)
        hanzi-set (set hanzi-base)
        {:keys [emoji hanzi pair-sources invalid]} (gather-sigils emoji-set hanzi-set)
        emoji-order (ordered-vec emoji-base emoji)
        hanzi-order (ordered-vec hanzi-base hanzi)
        collisions (->> pair-sources
                        (map (fn [[pair files]] [pair (sort files)]))
                        (filter (fn [[_ files]] (> (count files) 1)))
                        (sort-by (fn [[_ files]] (- (count files)))))
        strict (strict? args)]
    (ensure-dir "resources/sigils")
    (write-index emoji-order hanzi-order)
    (write-csv (adjacency emoji-order) "resources/sigils/emoji-adjacency.csv")
    (write-csv (adjacency hanzi-order) "resources/sigils/hanzi-adjacency.csv")
    (when (seq invalid)
      (println (format "Ignored %d invalid sigil tokens (not in allowlist)." (count invalid)))
      (doseq [[pair files] (take 20 (sort-by (comp - count val) invalid))]
        (println (format " - %s ← %s" pair (str/join ", " (sort files)))))
      (when strict
        (throw (ex-info "invalid-sigils"
                        {:count (count invalid)}))))
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
