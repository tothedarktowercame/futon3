(ns library-coherence-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def pattern-root (io/file "library"))

(def included-prefixes
  ["library/devmap-coherence"
   "library/library-coherence"
   "library/stack-coherence"
   "library/contributing"])

(defn- flexiarg-files []
  (->> (file-seq pattern-root)
       (filter (fn [^java.io.File file]
                 (let [path (str/replace (.getPath file) "\\" "/")]
                   (and (.isFile file)
                        (str/ends-with? (.getName file) ".flexiarg")
                        (some (fn [prefix]
                                (str/starts-with? path prefix))
                              included-prefixes)))))))

(defn- canonical-id [^java.io.File file]
  (-> (.getPath file)
      (str/replace "\\" "/")
      (str/replace #"^./" "")
      (str/replace #"\.flexiarg$" "")))

(def required-clauses ["IF:" "HOWEVER:" "THEN:" "BECAUSE:"])

(defn- sigil-tokens [text]
  (->> (str/split-lines text)
       (map str/trim)
       (filter #(str/starts-with? % "@sigils"))
       (mapcat #(re-seq #"[^\s\[\]]+/[^\s\[\]]+" %))
       (keep (fn [token]
               (let [tok (str/trim token)]
                 (when (and (not (str/blank? tok))
                            (str/includes? tok "/"))
                   tok))))
       set))

(defn- normalize-arg [arg]
  (some-> arg
          str/trim
          (str/split #"/")
          last
          (str/replace #"[^A-Za-z0-9_-]" "")))

(defn- candidate-ids [file text index-ids]
  (let [base (canonical-id file)
        trimmed (when (str/starts-with? base "library/")
                  (subs base (count "library/")))
        anchors (->> (re-seq #"(?m)^@arg\s+([^\s]+)" text)
                      (map second)
                      (keep normalize-arg))
        prefixes (remove nil? [base trimmed
                               (when (and (str/starts-with? base "library/")
                                          (some #(str/starts-with? % "futon3/") index-ids))
                                 (str "futon3/" base))
                               (when (and trimmed
                                          (some #(str/starts-with? % "futon3/") index-ids))
                                 (str "futon3/" trimmed))])]
    (->> prefixes
         (mapcat (fn [prefix]
                   (if (seq anchors)
                     (for [anchor anchors]
                       (str prefix "#" anchor))
                     [prefix])))
         (remove nil?)
         set)))

(defn- clause-present? [text clause]
  (let [token (-> clause str/lower-case (str/replace #":" ""))
        pattern (re-pattern (str "(?im)^\\s*[+•!-]*\\s*" token "\\s*:"))]
    (boolean (re-find pattern text))))

(defn- classic-template? [text]
  (boolean (re-find #"(?im)^!\\s+conclusion" text)))

(defn- missing-clauses [text]
  (if (classic-template? text)
    []
    (remove #(clause-present? text %) required-clauses)))

(defn- pattern-index-ids []
  (->> (str/split-lines (slurp "resources/sigils/patterns-index.tsv"))
       (keep (fn [line]
               (let [trim (str/trim line)]
                 (when (and (not (str/blank? trim))
                            (not (str/starts-with? trim "#")))
                   (first (str/split trim #"\t"))))))
       set))

(deftest flexiargs-follow-template
  (let [violations (reduce (fn [acc file]
                             (let [text (slurp file)
                                   sigils-present? (seq (sigil-tokens text))
                                   missing (missing-clauses text)]
                               (cond-> acc
                                 (not sigils-present?) (conj (str (canonical-id file) " missing sigils"))
                                 (seq missing) (conj (format "%s missing clauses: %s"
                                                             (canonical-id file)
                                                             (str/join ", " missing))))))
                           []
                           (flexiarg-files))]
    (is (empty? violations)
        (str "Pattern template violations:\n" (str/join "\n" violations)))))

(deftest patterns-present-in-embedding-index
  (let [index-ids (pattern-index-ids)
        missing (->> (flexiarg-files)
                     (keep (fn [file]
                             (let [text (slurp file)
                                   ids (candidate-ids file text index-ids)]
                               (when-not (some #(contains? index-ids %) ids)
                                 (canonical-id file)))))
                     vec)]
    (is (empty? missing)
        (str "Patterns missing from sigil embeddings: " (str/join ", " missing)))))

(deftest sigil-collisions-stay-below-threshold
  (let [pair->patterns (reduce (fn [acc file]
                                 (let [pairs (sigil-tokens (slurp file))]
                                   (reduce (fn [m pair]
                                             (update m pair (fnil conj []) (canonical-id file)))
                                           acc
                                           pairs)))
                               {}
                               (flexiarg-files))
        [worst-pair patterns] (apply max-key (comp count val) pair->patterns)
        max-count (count patterns)
        limit 7]
    (is (<= max-count limit)
        (format "Sigil pair %s appears %d times (limit %d). Patterns: %s"
                worst-pair max-count limit (str/join ", " patterns)))))
