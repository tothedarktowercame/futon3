;; scripts/build_pattern_index_incremental.clj
(ns scripts.build-pattern-index-incremental
  "Incrementally generate resources/sigils/patterns-index.tsv and rationale examples."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scripts.sigil-allowlist :as allow]))

(def ^:private default-output "resources/sigils/patterns-index.tsv")
(def ^:private default-examples "resources/sigils/rationale-examples.edn")
(def ^:private default-examples-count 12)
(def ^:private default-cache "data/pattern-index-cache.edn")

(def ^:private stopwords
  #{"the" "and" "of" "to" "a" "in" "for" "with" "on" "by" "is"
    "are" "as" "that" "this" "be" "an" "or" "it" "at" "from"
    "not" "have" "has" "but" "if" "you" "your" "we" "our" "their"
    "they" "them" "into" "about" "can" "will" "may" "must" "should"})

(def ^:private flexiarg-exts #{".flexiarg" ".multiarg"})

(def ^:private sigil-block-re #"\[[^\]]+\]")
(def ^:private sigil-token-re #"[^\s\[\]]+/[^\s\[\]]+")

(def ^:private invalid-sigils (atom {}))

(defn- emoji-like? [s]
  (and (string? s)
       (not (str/blank? s))
       (some #(> (int %) 255) s)))

(defn- note-invalid! [source emoji hanzi]
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

(defn- derive-hotwords [{:keys [title id rationale summary keywords]}]
  (->> (concat (hotwords-from-text keywords)  ;; keywords first for priority
               (hotwords-from-text title)
               (hotwords-from-text id)
               (hotwords-from-text rationale)
               (hotwords-from-text summary))
       distinct
       (take 20)
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

(defn- flexiarg-entry [tok-map emoji-set hanzi-set file block]
  (let [arg (or (extract-meta block "arg")
                (extract-meta block "flexiarg")
                (extract-meta block "multiarg"))
        title (extract-meta block "title")
        keywords (extract-meta block "keywords")
        source (str (.getPath (io/file file)) (when arg (str ":" arg)))
        sigils-meta (some-> (extract-meta block "sigils")
                            (parse-inline-sigils source emoji-set hanzi-set))
        sigils-block (parse-sigils block source emoji-set hanzi-set)
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
                                   :summary summary
                                   :keywords keywords})})))

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

(defn- flexiarg-entries-from-file [tok-map emoji-set hanzi-set file]
  (let [text (slurp file)]
    (->> (split-arg-blocks text)
         (keep #(flexiarg-entry tok-map emoji-set hanzi-set file %))
         vec)))

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
        components (parse-components (str body))
        by-label (into {} (map (fn [{:keys [label text]}] [label text]) components))
        because (get by-label "because")
        keywords (get by-label "keywords")
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
                                 :summary summary
                                 :keywords keywords})}))

(defn- devmap-files []
  (->> (file-seq (io/file "holes"))
       (filter #(and (.isFile ^java.io.File %)
                     (str/ends-with? (.getName %) ".devmap")
                     (str/starts-with? (.getName %) "futon")))))

(defn- devmap-entries-from-file [tok-map emoji-set hanzi-set file]
  (let [futon-id (futon-number (.getName file))
        text (slurp file)]
    (->> (parse-devmap-blocks text)
         (keep #(devmap-entry tok-map emoji-set hanzi-set file futon-id %))
         vec)))

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

(defn- load-cache [path]
  (let [file (io/file path)]
    (if (.exists file)
      (try
        (let [data (edn/read-string (slurp file))]
          (merge {:files {} :entries {}} data))
        (catch Exception _ {:files {} :entries {}}))
      {:files {} :entries {}})))

(defn- save-cache! [path cache]
  (ensure-dir (.getParent (io/file path)))
  (spit path (pr-str cache)))

(defn- drop-ids [entries ids]
  (apply dissoc entries ids))

(defn- update-cache-for-file [cache file entries]
  (let [path (.getPath (io/file file))
        mtime (.lastModified (io/file file))
        prev-ids (get-in cache [:files path :ids] [])
        entries-map (:entries cache)
        entries-map (drop-ids entries-map prev-ids)
        entries-map (reduce (fn [acc entry]
                              (assoc acc (:id entry) entry))
                            entries-map
                            entries)
        file-info {:mtime mtime
                   :ids (mapv :id entries)}]
    (-> cache
        (assoc :entries entries-map)
        (assoc-in [:files path] file-info))))

(defn- process-files [cache files parse-fn]
  (reduce
   (fn [state file]
     (let [path (.getPath (io/file file))
           mtime (.lastModified (io/file file))
           cached (get-in state [:files path])]
       (if (and cached (= mtime (:mtime cached)))
         state
         (update-cache-for-file state file (parse-fn file)))))
   cache
   files))

(defn- prune-stale-files [cache current-paths]
  (let [stale (remove current-paths (keys (:files cache)))]
    (reduce
     (fn [state path]
       (let [ids (get-in state [:files path :ids] [])]
         (-> state
             (update :entries drop-ids ids)
             (update :files dissoc path))))
     cache
     stale)))

(defn- parse-args [args]
  (loop [opts {:out default-output
               :examples default-examples
               :examples-count default-examples-count
               :write-examples? true
               :strict? false
               :cache default-cache}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--out" (recur (assoc opts :out (second remaining)) (nnext remaining))
        "--examples" (recur (assoc opts :examples (second remaining)) (nnext remaining))
        "--examples-count" (recur (assoc opts :examples-count (Long/parseLong (second remaining)))
                                  (nnext remaining))
        "--no-examples" (recur (assoc opts :write-examples? false) (rest remaining))
        "--strict" (recur (assoc opts :strict? true) (rest remaining))
        "--cache" (recur (assoc opts :cache (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- usage []
  (str/join
   "\n"
   ["Usage: clj -M -m scripts.build-pattern-index-incremental [--out PATH] [--examples PATH]"
    "                                   [--examples-count N] [--no-examples] [--cache PATH] [--strict]"
    ""
    "Defaults:"
    (format "  --out %s" default-output)
    (format "  --examples %s" default-examples)
    (format "  --examples-count %d" default-examples-count)
    (format "  --cache %s" default-cache)]))

(defn -main [& args]
  (let [{:keys [out examples examples-count write-examples? help? strict? cache]} (parse-args args)
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
          flex-files (vec (flexiarg-files))
          dev-files (vec (devmap-files))
          all-files (concat flex-files dev-files)
          current-paths (set (map #(.getPath (io/file %)) all-files))
          cache-path cache
          cache-data (load-cache cache-path)
          cache (-> cache-data
                    (process-files flex-files #(flexiarg-entries-from-file tok-map emoji-set hanzi-set %))
                    (process-files dev-files #(devmap-entries-from-file tok-map emoji-set hanzi-set %))
                    (prune-stale-files current-paths))
          entries (->> (:entries cache) vals (sort-by :id) vec)]
      (write-tsv out entries)
      (when write-examples?
        (write-examples examples entries examples-count))
      (save-cache! cache-path cache)
      (println (format "Wrote %d entries to %s" (count entries) out))
      (when write-examples?
        (println (format "Wrote %d examples to %s" examples-count examples)))
      (when (seq @invalid-sigils)
        (println (format "Ignored %d invalid sigil tokens (not in allowlist)." (count @invalid-sigils)))
        (doseq [[pair sources] (take 20 (sort-by (comp - count val) @invalid-sigils))]
          (println (format " - %s ← %s" pair (str/join ", " (sort sources)))))
        (when strict?
          (throw (ex-info "invalid-sigils" {:count (count @invalid-sigils)})))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
