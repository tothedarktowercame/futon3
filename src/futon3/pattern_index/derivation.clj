;; scripts/build_pattern_index.clj
(ns futon3.pattern-index.derivation
  "Canonical lexical/sigil derivation for the pattern-index sidecar.

   This is the single authority for the former patterns-index.tsv fields.
   It can write those rows transactionally to SQLite and render the legacy
   TSV export back from SQLite while readers are migrated."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon.flexiarg.projection :as projection]
            [scripts.sigil-allowlist :as allow])
  (:import [java.sql DriverManager]))

(def ^:private default-output "resources/sigils/patterns-index.tsv")
(def ^:private default-examples "resources/sigils/rationale-examples.edn")
(def ^:private default-examples-count 12)

(def ^:private stopwords
  #{"the" "and" "of" "to" "a" "in" "for" "with" "on" "by" "is"
    "are" "as" "that" "this" "be" "an" "or" "it" "at" "from"
    "not" "have" "has" "but" "if" "you" "your" "we" "our" "their"
    "they" "them" "into" "about" "can" "will" "may" "must" "should"})

(def ^:private sigil-token-re #"[^\s\[\]]+/[^\s\[\]]+")

(def ^:private invalid-sigils (atom {}))

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

(defn- tokipona-emoji-map [futon3-root]
  (let [file (io/file futon3-root "holes/tokipona.org")]
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
       :kind "library"
       :source (:pattern/source-path packet)
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

(defn- scan-flexiargs [futon3-root tok-map emoji-set hanzi-set]
  (->> (projection/parse-roots {:futon3-root futon3-root
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

(defn- devmap-entry [futon3-root tok-map emoji-set hanzi-set file futon-id {:keys [proto title sigils body]}]
  (let [source (str (.relativize (.toPath (io/file futon3-root))
                                (.toPath (io/file file))) ":p" proto)
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
     :kind "devmap"
     :source source
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

(defn- scan-devmaps [futon3-root tok-map emoji-set hanzi-set]
  (->> (file-seq (io/file futon3-root "holes"))
       (filter #(and (.isFile ^java.io.File %)
                     (str/ends-with? (.getName %) ".devmap")
                     (str/starts-with? (.getName %) "futon")))
       (mapcat (fn [file]
                 (let [futon-id (futon-number (.getName file))
                       text (slurp file)]
                   (keep #(devmap-entry futon3-root tok-map emoji-set hanzi-set file futon-id %)
                         (parse-devmap-blocks text)))))
       vec))

(defn render-tsv
  "Render canonical entries in the historical byte-for-byte TSV shape."
  [entries]
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
    (str header (str/join "\n" rows) "\n")))

(defn write-if-changed!
  "Write CONTENT only when its bytes differ, avoiding watcher rewrite churn."
  [path content]
  (let [file (io/file path)]
    (ensure-dir (.getParent (io/file path)))
    (when (or (not (.exists file)) (not= content (slurp file)))
      (spit file content)
      true)))

(defn write-tsv! [path entries]
  (write-if-changed! path (render-tsv entries)))

(defn canonical-entries
  "Return every canonical lexical/sigil row for FUTON3-ROOT."
  [futon3-root]
  (let [emoji-order (or (allow/tokipona-emoji-order) [])
        hanzi-order (or (allow/truth-table-hanzi-order) [])
        _ (allow/ensure-allowlist! emoji-order hanzi-order)
        emoji-set (set emoji-order)
        hanzi-set (set hanzi-order)
        tok-map (tokipona-emoji-map futon3-root)]
    (->> (concat (scan-flexiargs futon3-root tok-map emoji-set hanzi-set)
                 (scan-devmaps futon3-root tok-map emoji-set hanzi-set))
         (remove nil?)
         (sort-by :id)
         vec)))

(defn write-sidecar!
  "Replace the canonical sigil snapshot and append its checkpoint in one
   SQLite transaction. Existing embedding/pattern tables are untouched."
  ([db-path entries library-head]
   (write-sidecar! db-path entries library-head 0))
  ([db-path entries library-head embedding-seq]
   (Class/forName "org.sqlite.JDBC")
   (with-open [conn (DriverManager/getConnection (str "jdbc:sqlite:" db-path))]
    (.setAutoCommit conn false)
    (try
      (with-open [st (.createStatement conn)]
        (.execute st (str "CREATE TABLE IF NOT EXISTS pattern_sigil ("
                          "ordinal INTEGER PRIMARY KEY, id TEXT NOT NULL, "
                          "tokipona TEXT, truth TEXT, "
                          "rationale TEXT NOT NULL, hotwords TEXT NOT NULL, "
                          "kind TEXT NOT NULL, source_path TEXT NOT NULL)"))
        (.execute st (str "CREATE TABLE IF NOT EXISTS sigil_checkpoint ("
                          "seq INTEGER PRIMARY KEY, library_head TEXT, "
                          "embedding_seq INTEGER NOT NULL, rows_indexed INTEGER NOT NULL, "
                          "finished_at TEXT NOT NULL)"))
        ;; A sidecar created before embedding_seq existed keeps its old
        ;; table under CREATE TABLE IF NOT EXISTS; add the column (claude-13,
        ;; 2026-11-11: the live sidecar's table was made by an earlier draft
        ;; of this code and every build then failed at this step).
        (let [cols (with-open [rs (.executeQuery st "PRAGMA table_info(sigil_checkpoint)")]
                     (loop [acc #{}]
                       (if (.next rs) (recur (conj acc (.getString rs "name"))) acc)))]
          (when-not (contains? cols "embedding_seq")
            (.execute st (str "ALTER TABLE sigil_checkpoint ADD COLUMN "
                              "embedding_seq INTEGER NOT NULL DEFAULT 0"))))
        (.executeUpdate st "DELETE FROM pattern_sigil"))
      (with-open [ps (.prepareStatement conn
                                        (str "INSERT INTO pattern_sigil "
                                             "(ordinal,id,tokipona,truth,rationale,hotwords,kind,source_path) "
                                             "VALUES (?,?,?,?,?,?,?,?)"))]
        (doseq [[ordinal {:keys [id tokipona hanzi rationale hotwords kind source]}]
                (map-indexed vector entries)]
          (.setLong ps 1 ordinal)
          (.setString ps 2 id)
          (.setString ps 3 tokipona)
          (.setString ps 4 hanzi)
          (.setString ps 5 rationale)
          (.setString ps 6 (json/write-str hotwords))
          (.setString ps 7 kind)
          (.setString ps 8 source)
          (.addBatch ps))
        (.executeBatch ps))
      (with-open [ps (.prepareStatement conn
                                        (str "INSERT INTO sigil_checkpoint "
                                             "(seq,library_head,embedding_seq,rows_indexed,finished_at) "
                                             "VALUES ((SELECT COALESCE(MAX(seq),0)+1 "
                                             "FROM sigil_checkpoint),?,?,?,?)"))]
        (.setString ps 1 library-head)
        (.setLong ps 2 embedding-seq)
        (.setLong ps 3 (count entries))
        (.setString ps 4 (str (java.time.Instant/now)))
        (.executeUpdate ps))
      (.commit conn)
      (catch Throwable t
        (.rollback conn)
        (throw t))))
   {:rows (count entries) :embedding-seq embedding-seq}))

(defn sidecar-entries
  "Read the canonical export rows from the sidecar in TSV order."
  [db-path]
  (with-open [conn (DriverManager/getConnection (str "jdbc:sqlite:" db-path))
              ps (.prepareStatement conn (str "SELECT id,tokipona,truth,rationale,hotwords,"
                                              "kind,source_path FROM pattern_sigil ORDER BY ordinal"))
              rs (.executeQuery ps)]
    (loop [out []]
      (if (.next rs)
        (recur (conj out {:id (.getString rs 1)
                          :tokipona (.getString rs 2)
                          :hanzi (.getString rs 3)
                          :rationale (.getString rs 4)
                          :hotwords (json/read-str (.getString rs 5))
                          :kind (.getString rs 6)
                          :source (.getString rs 7)}))
        out))))

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
               :futon3-root "."
               :db nil
               :library-head nil
               :embedding-seq 0
               :examples default-examples
               :examples-count default-examples-count
               :write-examples? true
               :strict? false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--out" (recur (assoc opts :out (second remaining)) (nnext remaining))
        "--export-tsv" (recur (assoc opts :out (second remaining)) (nnext remaining))
        "--futon3-root" (recur (assoc opts :futon3-root (second remaining)) (nnext remaining))
        "--db" (recur (assoc opts :db (second remaining)) (nnext remaining))
        "--library-head" (recur (assoc opts :library-head (second remaining)) (nnext remaining))
        "--embedding-seq" (recur (assoc opts :embedding-seq (Long/parseLong (second remaining)))
                                  (nnext remaining))
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
   ["Usage: clojure -M -m futon3.pattern-index.derivation [--out PATH] [--examples PATH]"
    "                 [--futon3-root PATH] [--db PATH] [--library-head SHA]"
    "                 [--examples-count N] [--no-examples] [--strict]"
    ""
    "Defaults:"
    (format "  --out %s" default-output)
    (format "  --examples %s" default-examples)
    (format "  --examples-count %d" default-examples-count)]))

(defn -main [& args]
  (let [{:keys [out examples examples-count write-examples? help? strict?
                futon3-root db library-head embedding-seq]} (parse-args args)
        strict? (or strict? (seq (System/getenv "SIGIL_STRICT")))]
    (when help?
      (println (usage))
      (System/exit 0))
    (reset! invalid-sigils {})
    (let [entries (canonical-entries futon3-root)
          export-entries (if db
                           (do (write-sidecar! db entries library-head embedding-seq)
                               (sidecar-entries db))
                           entries)]
      (write-tsv! out export-entries)
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
