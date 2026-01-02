;; scripts/build_pattern_index.clj
(ns scripts.build-pattern-index
  "Generate resources/sigils/patterns-index.tsv and rationale examples."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private default-output "resources/sigils/patterns-index.tsv")
(def ^:private default-examples "resources/sigils/rationale-examples.edn")
(def ^:private default-examples-count 12)

(def ^:private stopwords
  #{"the" "and" "of" "to" "a" "in" "for" "with" "on" "by" "is"
    "are" "as" "that" "this" "be" "an" "or" "it" "at" "from"
    "not" "have" "has" "but" "if" "you" "your" "we" "our" "their"
    "they" "them" "into" "about" "can" "will" "may" "must" "should"})

(def ^:private flexiarg-exts #{".flexiarg" ".multiarg"})

(def ^:private sigil-block-re #"\[[^\]]+\]")
(def ^:private sigil-token-re #"[^\s\[\]]+/[^\s\[\]]+")

(defn- read-lines [file]
  (-> file io/file slurp str/split-lines))

(defn- normalize-space [value]
  (some-> value str/trim not-empty))

(defn- single-line [value]
  (some-> value
          str
          (str/replace #"\s+" " ")
          str/trim
          not-empty))

(defn- ensure-dir [path]
  (let [dir (io/file path)]
    (.mkdirs dir)
    dir))

(defn- tokipona-emoji-map []
  (let [file (io/file "holes/tokipona.org")]
    (when (.exists file)
      (reduce
       (fn [acc line]
         (if (or (not (str/starts-with? line "|"))
                 (str/starts-with? line "|---"))
           acc
           (let [cols (->> (str/split line #"\|")
                           (map str/trim)
                           (remove str/blank?)
                           vec)
                 emoji (nth cols 0 nil)
                 word (some-> (nth cols 1 nil) str/lower-case)]
             (if (and emoji word (not (str/includes? word "toki pona")))
               (assoc acc emoji word)
               acc))))
       {}
       (read-lines file)))))

(defn- parse-sigils [text]
  (->> (re-seq sigil-block-re text)
       (mapcat #(re-seq sigil-token-re %))
       (keep (fn [token]
               (let [[emoji hanzi] (str/split token #"/" 2)]
                 (when (and emoji hanzi)
                   {:emoji (str/trim emoji)
                    :hanzi (str/trim hanzi)}))))
       vec))

(defn- parse-inline-sigils [text]
  (->> (re-seq sigil-token-re (or text ""))
       (keep (fn [token]
               (let [[emoji hanzi] (str/split token #"/" 2)]
                 (when (and emoji hanzi)
                   {:emoji (str/trim emoji)
                    :hanzi (str/trim hanzi)}))))
       vec))

(defn- extract-meta [text key]
  (some->> (re-find (re-pattern (str "@" key "\\s+(.*)")) text)
           second
           str/trim
           not-empty))

(defn- split-arg-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [remaining lines
           current []
           has-arg? false
           blocks []]
      (if-let [line (first remaining)]
        (let [rest-lines (rest remaining)
              starts-arg? (str/starts-with? line "@arg ")]
          (cond
            (and starts-arg? has-arg?)
            (recur rest-lines
                   [line]
                   true
                   (conj blocks (str/join "\n" current)))

            starts-arg?
            (recur rest-lines
                   (conj current line)
                   true
                   blocks)

            :else
            (recur rest-lines
                   (conj current line)
                   has-arg?
                   blocks)))
        (if has-arg?
          (conj blocks (str/join "\n" current))
          [text])))))

(defn- parse-components [block]
  (let [lines (str/split-lines block)
        header-re (re-pattern "^\\s*[!+]\\s+([^:]+):\\s*(.*)$")]
    (loop [remaining lines
           current nil
           sections []]
      (if-let [line (first remaining)]
        (if-let [[_ label trailing] (re-matches header-re line)]
          (let [next-section (when current
                               {:label (:label current)
                                :text (str/trim (str/join "\n" (:lines current)))})
                new-lines (cond-> []
                            (and trailing (not (str/blank? trailing)))
                            (conj trailing))]
            (recur (rest remaining)
                   {:label (str/lower-case (str/trim label)) :lines new-lines}
                   (cond-> sections next-section (conj next-section))))
          (recur (rest remaining)
                 (if current
                   (update current :lines conj line)
                   current)
                 sections))
        (let [final-section (when current
                              {:label (:label current)
                               :text (str/trim (str/join "\n" (:lines current)))})]
          (cond-> sections final-section (conj final-section)))))))

(defn- hotwords-from-text [text]
  (->> (re-seq #"[a-z0-9]+" (str/lower-case (or text "")))
       (remove stopwords)
       distinct
       vec))

(defn- derive-hotwords [{:keys [title id rationale summary]}]
  (->> (concat (hotwords-from-text title)
               (hotwords-from-text id)
               (hotwords-from-text rationale)
               (hotwords-from-text summary))
       distinct
       (take 10)
       vec))

(defn- derive-rationale [{:keys [title tokipona hanzi because]}]
  (let [base (or (single-line because) "No rationale clause supplied.")
        tok (or (normalize-space tokipona) "unknown")
        han (or (normalize-space hanzi) "unknown")]
    (format "%s -> %s/%s: %s" (or (normalize-space title) "Pattern")
            tok
            han
            base)))

(defn- choose-primary-sigil [sigils]
  (first (remove (fn [{:keys [emoji hanzi]}]
                   (or (str/blank? emoji) (str/blank? hanzi)))
                 sigils)))

(defn- flexiarg-entry [tok-map file block]
  (let [arg (or (extract-meta block "arg")
                (extract-meta block "flexiarg")
                (extract-meta block "multiarg"))
        title (extract-meta block "title")
        sigils-meta (some-> (extract-meta block "sigils") parse-inline-sigils)
        sigils-block (parse-sigils block)
        sigils (vec (distinct (concat sigils-meta sigils-block)))
        primary (choose-primary-sigil sigils)
        components (parse-components block)
        by-label (into {} (map (fn [{:keys [label text]}] [label text]) components))
        summary (or (get by-label "conclusion")
                    (get by-label "claim")
                    (get by-label "then")
                    (get by-label "because"))
        because (get by-label "because")
        tokipona (when primary (get tok-map (:emoji primary)))]
    (when (and arg primary)
      {:id arg
       :title (or title arg)
       :sigils sigils
       :tokipona tokipona
       :hanzi (:hanzi primary)
       :rationale (derive-rationale {:title (or title arg)
                                     :tokipona tokipona
                                     :hanzi (:hanzi primary)
                                     :because because})
       :hotwords (derive-hotwords {:title title
                                   :id arg
                                   :rationale because
                                   :summary summary})})))

(defn- flexiarg-files []
  (let [ext-of (fn [^java.io.File file]
                 (let [name (.getName file)
                       match (re-find #"\.[^.]+$" name)]
                   (when match
                     (str/lower-case match))))]
    (->> ["library" "holes"]
         (map io/file)
         (filter #(.exists %))
         (mapcat file-seq)
         (filter #(.isFile ^java.io.File %))
         (filter (fn [file]
                   (contains? flexiarg-exts (ext-of file)))))))

(defn- scan-flexiargs [tok-map]
  (->> (flexiarg-files)
       (mapcat (fn [file]
                 (let [text (slurp file)]
                   (keep #(flexiarg-entry tok-map file %) (split-arg-blocks text)))))
       vec))

(defn- futon-number [name]
  (some->> (re-find #"futon(\d+)" name)
           second
           (format "f%s")))

(defn- parse-devmap-blocks [text]
  (let [lines (str/split-lines text)
        header-re (re-pattern "^!\\s+instantiated-by: Prototype\\s+(\\d+)\\s+—\\s+(.*)\\s+\\[(.*)\\]$")]
    (loop [remaining lines
           current nil
           blocks []]
      (if-let [line (first remaining)]
        (if-let [[_ proto title sigils] (re-matches header-re line)]
          (let [next-block (when current
                             (assoc current :body (str/join "\n" (:lines current))))]
            (recur (rest remaining)
                   {:proto proto :title title :sigils sigils :lines []}
                   (cond-> blocks next-block (conj next-block))))
          (recur (rest remaining)
                 (if current
                   (update current :lines conj line)
                   current)
                 blocks))
        (let [final (when current
                      (assoc current :body (str/join "\n" (:lines current))))]
          (cond-> blocks final (conj final)))))))

(defn- devmap-entry [tok-map futon-id {:keys [proto title sigils body]}]
  (let [sigil-list (parse-inline-sigils sigils)
        primary (choose-primary-sigil sigil-list)
        components (parse-components (str body))
        by-label (into {} (map (fn [{:keys [label text]}] [label text]) components))
        because (get by-label "because")
        summary (or (get by-label "then")
                    (get by-label "context")
                    (get by-label "if")
                    because)
        tokipona (when primary (get tok-map (:emoji primary)))]
    (when primary
      {:id (format "%s/p%s" futon-id proto)
       :title title
       :sigils sigil-list
       :tokipona tokipona
       :hanzi (:hanzi primary)
       :rationale (derive-rationale {:title title
                                     :tokipona tokipona
                                     :hanzi (:hanzi primary)
                                     :because because})
       :hotwords (derive-hotwords {:title title
                                   :id (format "%s/p%s" futon-id proto)
                                   :rationale because
                                   :summary summary})})))

(defn- scan-devmaps [tok-map]
  (->> (file-seq (io/file "holes"))
       (filter #(and (.isFile ^java.io.File %)
                     (str/ends-with? (.getName %) ".devmap")
                     (str/starts-with? (.getName %) "futon")))
       (mapcat (fn [file]
                 (let [futon-id (futon-number (.getName file))
                       text (slurp file)]
                   (keep #(devmap-entry tok-map futon-id %)
                         (parse-devmap-blocks text)))))
       vec))

(defn- write-tsv [path entries]
  (let [header "# pattern\tokipona\ttruth\trationale\thotwords\n"
        tsv-safe (fn [value]
                   (-> (or value "")
                       str
                       (str/replace #"\t" " ")
                       (str/replace #"\s+" " ")
                       str/trim))
        rows (for [{:keys [id tokipona hanzi rationale hotwords]} entries]
               (str (tsv-safe id)
                    "\t" (tsv-safe tokipona)
                    "\t" (tsv-safe hanzi)
                    "\t" (tsv-safe rationale)
                    "\t" (tsv-safe (str/join ", " hotwords))))]
    (ensure-dir (.getParent (io/file path)))
    (spit path (str header (str/join "\n" rows) "\n"))))

(defn- write-examples [path entries max-count]
  (let [devmap? (fn [id] (boolean (re-matches #"f\d+/p\d+" (or id ""))))
        devmaps (filter #(devmap? (:id %)) entries)
        library (remove #(devmap? (:id %)) entries)
        devmap-count (min (count devmaps) (long (Math/floor (/ max-count 2))))
        library-count (- max-count devmap-count)
        examples (->> (concat (take devmap-count devmaps)
                              (take library-count library))
                      (mapv (fn [{:keys [id title sigils rationale tokipona hanzi]}]
                              {:pattern/id id
                               :pattern/title title
                               :sigils sigils
                               :tokipona tokipona
                               :hanzi hanzi
                               :rationale (single-line rationale)})))]
    (ensure-dir (.getParent (io/file path)))
    (spit path (pr-str examples))))

(defn- parse-args [args]
  (loop [opts {:out default-output
               :examples default-examples
               :examples-count default-examples-count
               :write-examples? true}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--out" (recur (assoc opts :out (second remaining)) (nnext remaining))
        "--examples" (recur (assoc opts :examples (second remaining)) (nnext remaining))
        "--examples-count" (recur (assoc opts :examples-count (Long/parseLong (second remaining)))
                                  (nnext remaining))
        "--no-examples" (recur (assoc opts :write-examples? false) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- usage []
  (str/join
   "\n"
   ["Usage: clj -M -m scripts.build-pattern-index [--out PATH] [--examples PATH]"
    "                                   [--examples-count N] [--no-examples]"
    ""
    "Defaults:"
    (format "  --out %s" default-output)
    (format "  --examples %s" default-examples)
    (format "  --examples-count %d" default-examples-count)]))

(defn -main [& args]
  (let [{:keys [out examples examples-count write-examples? help?]} (parse-args args)]
    (when help?
      (println (usage))
      (System/exit 0))
    (let [tok-map (tokipona-emoji-map)
          flexiargs (scan-flexiargs tok-map)
          devmaps (scan-devmaps tok-map)
          entries (->> (concat flexiargs devmaps)
                       (remove nil?)
                       (sort-by :id)
                       vec)]
      (write-tsv out entries)
      (when write-examples?
        (write-examples examples entries examples-count))
      (println (format "Wrote %d entries to %s" (count entries) out))
      (when write-examples?
        (println (format "Wrote %d examples to %s" examples-count examples))))))
