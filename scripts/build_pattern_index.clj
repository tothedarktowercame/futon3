;; scripts/build_pattern_index.clj
(ns scripts.build-pattern-index
  "Generate resources/sigils/patterns-index.tsv and rationale examples."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon.flexiarg.projection :as projection]
            [scripts.sigil-allowlist :as allow]))

(def ^:private default-output "resources/sigils/patterns-index.tsv")
(def ^:private default-examples "resources/sigils/rationale-examples.edn")
(def ^:private default-examples-count 12)

(def ^:private stopwords
  #{"the" "and" "of" "to" "a" "in" "for" "with" "on" "by" "is"
    "are" "as" "that" "this" "be" "an" "or" "it" "at" "from"
    "not" "have" "has" "but" "if" "you" "your" "we" "our" "their"
    "they" "them" "into" "about" "can" "will" "may" "must" "should"})

(def ^:private sigil-block-re #"\[[^\]]+\]")
(def ^:private sigil-token-re #"[^\s\[\]]+/[^\s\[\]]+")

(def ^:private invalid-sigils (atom {}))

(defn- emoji-like? [s]
  (and (string? s)
       (not (str/blank? s))
       (some #(> (int %) 255) s)))

(defn- note-invalid!
  [source emoji hanzi]
  (swap! invalid-sigils update (str emoji "/" hanzi) (fnil conj #{}) source))

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

(defn- parse-sigils [text source emoji-set hanzi-set]
  (->> (re-seq sigil-block-re text)
       (mapcat #(re-seq sigil-token-re %))
       (keep (fn [token]
               (let [[emoji hanzi] (str/split token #"/" 2)]
                 (when (and emoji hanzi)
                   (let [emoji (str/trim emoji)
                         hanzi (str/trim hanzi)]
                     (cond
                       (not (emoji-like? emoji)) nil
                       (and (contains? emoji-set emoji)
                            (contains? hanzi-set hanzi))
                       {:emoji emoji
                        :hanzi hanzi}
                       :else
                       (do
                         (note-invalid! source emoji hanzi)
                         nil)))))))
       vec))

(defn- parse-inline-sigils [text source emoji-set hanzi-set]
  (->> (re-seq sigil-token-re (or text ""))
       (keep (fn [token]
               (let [[emoji hanzi] (str/split token #"/" 2)]
                 (when (and emoji hanzi)
                   (let [emoji (str/trim emoji)
                         hanzi (str/trim hanzi)]
                     (if (and (contains? emoji-set emoji)
                              (contains? hanzi-set hanzi))
                       {:emoji emoji
                        :hanzi hanzi}
                       (do
                         (note-invalid! source emoji hanzi)
                         nil)))))))
       vec))

(defn- clauses-by-key [packet]
  (into {} (map (juxt :name-key :text) (:pattern/clauses packet))))

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

(defn- flexiarg-entry [tok-map emoji-set hanzi-set packet]
  (let [arg (:pattern/id packet)
        title (:pattern/title packet)
        source (str (:pattern/source-path packet) (when arg (str ":" arg)))
        sigils (parse-inline-sigils (str/join " " (:pattern/sigils packet))
                                    source
                                    emoji-set
                                    hanzi-set)
        primary (choose-primary-sigil sigils)
        by-label (clauses-by-key packet)
        summary (or (get by-label "conclusion")
                    (get by-label "claim")
                    (get by-label "then")
                    (get by-label "because"))
        because (get by-label "because")
        tokipona (when primary (get tok-map (:emoji primary)))]
    (when arg
      {:id arg
       :title (or title arg)
       :sigils sigils
       :tokipona tokipona
       :hanzi (when primary (:hanzi primary))
       :rationale (derive-rationale {:title (or title arg)
                                     :tokipona tokipona
                                     :hanzi (when primary (:hanzi primary))
                                     :because because})
       :hotwords (derive-hotwords {:title title
                                   :id arg
                                   :rationale because
                                   :summary summary})})))

(defn- scan-flexiargs [tok-map emoji-set hanzi-set]
  (->> (projection/parse-roots {:futon3-root "."
                                :source-roots ["library" "holes"]})
       (keep #(when (= :ok (:pattern/status %))
                (flexiarg-entry tok-map emoji-set hanzi-set %)))
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

(defn- devmap-entry [tok-map emoji-set hanzi-set file futon-id {:keys [proto title sigils body]}]
  (let [source (str (.getPath (io/file file)) ":p" proto)
        sigil-list (parse-inline-sigils sigils source emoji-set hanzi-set)
        primary (choose-primary-sigil sigil-list)
        by-label (into {} (map (juxt :name-key :text) (projection/parse-components (str body))))
        because (get by-label "because")
        summary (or (get by-label "then")
                    (get by-label "context")
                    (get by-label "if")
                    because)
        tokipona (when primary (get tok-map (:emoji primary)))]
    {:id (format "%s/p%s" futon-id proto)
     :title title
     :sigils sigil-list
     :tokipona tokipona
     :hanzi (when primary (:hanzi primary))
     :rationale (derive-rationale {:title title
                                   :tokipona tokipona
                                   :hanzi (when primary (:hanzi primary))
                                   :because because})
     :hotwords (derive-hotwords {:title title
                                 :id (format "%s/p%s" futon-id proto)
                                 :rationale because
                                 :summary summary})}))

(defn- scan-devmaps [tok-map emoji-set hanzi-set]
  (->> (file-seq (io/file "holes"))
       (filter #(and (.isFile ^java.io.File %)
                     (str/ends-with? (.getName %) ".devmap")
                     (str/starts-with? (.getName %) "futon")))
       (mapcat (fn [file]
                 (let [futon-id (futon-number (.getName file))
                       text (slurp file)]
                   (keep #(devmap-entry tok-map emoji-set hanzi-set file futon-id %)
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
               :write-examples? true
               :strict? false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--out" (recur (assoc opts :out (second remaining)) (nnext remaining))
        "--examples" (recur (assoc opts :examples (second remaining)) (nnext remaining))
        "--examples-count" (recur (assoc opts :examples-count (Long/parseLong (second remaining)))
                                  (nnext remaining))
        "--no-examples" (recur (assoc opts :write-examples? false) (rest remaining))
        "--strict" (recur (assoc opts :strict? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- usage []
  (str/join
   "\n"
   ["Usage: clj -M -m scripts.build-pattern-index [--out PATH] [--examples PATH]"
    "                                   [--examples-count N] [--no-examples] [--strict]"
    ""
    "Defaults:"
    (format "  --out %s" default-output)
    (format "  --examples %s" default-examples)
    (format "  --examples-count %d" default-examples-count)]))

(defn -main [& args]
  (let [{:keys [out examples examples-count write-examples? help? strict?]} (parse-args args)
        strict? (or strict? (seq (System/getenv "SIGIL_STRICT")))]
    (when help?
      (println (usage))
      (System/exit 0))
    (reset! invalid-sigils {})
    (let [emoji-order (or (allow/tokipona-emoji-order) [])
          hanzi-order (or (allow/truth-table-hanzi-order) [])
          _ (allow/ensure-allowlist! emoji-order hanzi-order)
          emoji-set (set emoji-order)
          hanzi-set (set hanzi-order)
          tok-map (tokipona-emoji-map)
          flexiargs (scan-flexiargs tok-map emoji-set hanzi-set)
          devmaps (scan-devmaps tok-map emoji-set hanzi-set)
          entries (->> (concat flexiargs devmaps)
                       (remove nil?)
                       (sort-by :id)
                       vec)]
      (write-tsv out entries)
      (when write-examples?
        (write-examples examples entries examples-count))
      (println (format "Wrote %d entries to %s" (count entries) out))
      (when write-examples?
        (println (format "Wrote %d examples to %s" examples-count examples)))
      (when (seq @invalid-sigils)
        (println (format "Ignored %d invalid sigil tokens (not in allowlist)." (count @invalid-sigils)))
        (doseq [[pair sources] (take 20 (sort-by (comp - count val) @invalid-sigils))]
          (println (format " - %s ← %s" pair (str/join ", " (sort sources)))))
        (when strict?
          (throw (ex-info "invalid-sigils"
                          {:count (count @invalid-sigils)})))))))
