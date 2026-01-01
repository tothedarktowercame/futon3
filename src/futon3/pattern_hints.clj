(ns futon3.pattern-hints
  "Structured pattern / fruit / paramita hints for ChatGPT integration."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- read-edn [path]
  (-> path io/file slurp edn/read-string))

(defn- read-json [path]
  (try
    (with-open [reader (io/reader (io/file path))]
      (json/parse-stream reader keyword))
    (catch Exception _ nil)))

(def ^:private sigil-index
  (delay (read-edn "resources/sigils/index.edn")))

(def ^:private emoji-order (delay (:emoji @sigil-index)))
(def ^:private hanzi-order (delay (:hanzi @sigil-index)))
(def ^:private emoji-pos (delay (zipmap @emoji-order (range))))
(def ^:private hanzi-pos (delay (zipmap @hanzi-order (range))))
(def ^:private emoji-norm (delay (max 1 (dec (count @emoji-order)))))
(def ^:private hanzi-norm (delay (max 1 (dec (count @hanzi-order)))))

(def ^:private fruits-data (delay (read-edn "resources/sigils/fruits.edn")))
(def ^:private paramita-data (delay (read-edn "resources/sigils/paramitas.edn")))

(def ^:private glove-patterns-data
  (delay (or (read-json "data/sigils/glove_pattern_neighbors.json") [])))

(def ^:private glove-patterns-by-id
  (delay (into {}
               (keep (fn [entry]
                       (when-let [id (:id entry)]
                         [id entry])))
               @glove-patterns-data)))

(def ^:private clause-re
  (re-pattern "!\\s+(?:conclusion|claim|instantiated-by):\\s*(.*?)\\s*\\[(.*?)\\]"))

(defn- split-sigils [block]
  (->> (str/split block #"\s+")
       (keep (fn [token]
               (when (str/includes? token "/")
                 (let [[emoji hanzi] (str/split token #"/" 2)]
                   {:emoji (str/trim emoji)
                    :hanzi (str/trim hanzi)}))))))

(defn- read-lines [text]
  (str/split text #"\n"))

(defn- extract-meta [text key]
  (some->> (re-find (re-pattern (str "@" key "\\s+(.*)")) text)
           second
           str/trim))

(defn- read-file [file]
  (slurp (io/file file)))

(def ^:private pattern-roots [(io/file "holes/LDTS")
                              (io/file "library")])

(defn- flexiarg-file? [file]
  (let [name (.getName file)]
    (or (str/ends-with? name ".flexiarg")
        (str/ends-with? name ".multiarg"))))

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

(defn- scan-library []
  (for [root pattern-roots
        :when (.exists root)
        file (file-seq root)
        :when (and (.isFile file)
                   (flexiarg-file? file))
        :let [text (slurp file)]
        block (split-arg-blocks text)
        :let [title (extract-meta block "title")
              arg (extract-meta block "arg")]
        match (re-seq clause-re block)]
    {:id (or arg (.getName file))
     :title title
     :summary (str/trim (second match))
     :sigils (split-sigils (nth match 2))}))

(def ^:private ldts-patterns (delay (vec (scan-library))))

(defn all-patterns
  "Return the catalog of flexiarg/devmap patterns with sigils.
  Used by higher-level components that need raw entries (e.g. intent seeding)."
  []
  @ldts-patterns)

(defn- futon-number [name]
  (some->> (re-find #"futon(\d+)" name)
           second
           (format "f%s")))

(defn- scan-devmaps []
  (reduce (fn [acc file]
            (let [text (slurp file)
                  futon (futon-number (.getName file))]
              (reduce
               (fn [acc' match]
                 (let [proto (format "%s/p%s" futon (second match))
                       sigils (split-sigils (nth match 2))]
                   (assoc acc' proto sigils)))
               acc
               (re-seq (re-pattern "!\\s+instantiated-by: Prototype\\s+(\\d+) — .*?\\[(.*?)\\]")
                       text))))
          {}
          (filter #(and (.isFile %) (str/ends-with? (.getName %) ".devmap"))
                  (file-seq (io/file "holes")))))

(def ^:private prototype->sigils (delay (scan-devmaps)))

(def ^:private patterns-by-id
  (delay (into {}
               (map (juxt :id identity))
               @ldts-patterns)))

(def ^:private max-missing-target-warnings 5)
(defonce ^:private !missing-target-warnings (atom 0))

(defn- warn-missing-targets [sigils prototypes]
  (let [count (swap! !missing-target-warnings inc)
        label (if (seq prototypes)
                "no sigils resolved from prototypes"
                "no sigils/prototypes supplied")]
    (when (<= count max-missing-target-warnings)
      (println (str "[pattern-hints] " label)
               {:sigils (boolean (seq sigils))
                :prototypes (boolean (seq prototypes))}))))

(defn- emoji-distance [a b]
  (if (= a b)
    0.0
    (let [pa (get @emoji-pos a)
          pb (get @emoji-pos b)]
      (when (and pa pb)
        (/ (Math/abs ^long (- pa pb)) (double @emoji-norm))))))

(defn- hanzi-distance [a b]
  (if (= a b)
    0.0
    (let [pa (get @hanzi-pos a)
          pb (get @hanzi-pos b)]
      (when (and pa pb)
        (/ (Math/abs ^long (- pa pb)) (double @hanzi-norm))))))

(defn- pair-distance [target clause]
  (when-let [ed (emoji-distance (:emoji target) (:emoji clause))]
    (when-let [hd (hanzi-distance (:hanzi target) (:hanzi clause))]
      (/ (+ ed hd) 2.0))))

(defn- entry-distance [targets entry]
  (some->> (for [t targets
                 sig (:sigils entry)
                 :let [d (pair-distance t sig)]
                 :when d]
             d)
           seq
           (reduce min)))

(defn- nearest [targets entries limit]
  (->> entries
       (keep (fn [entry]
               (when-let [d (entry-distance targets entry)]
                 (assoc entry :score d))))
       (sort-by :score)
       (take limit)
       vec))

(defn- score->distance [score]
  (when (number? score)
    (-> 1.0 (- (min 1.0 (double score))) (max 0.0))))

(defn- glove-neighbors
  [seed-ids limit]
  (if (seq seed-ids)
    (let [seed-set (set seed-ids)]
      (->> seed-ids
           (keep #(get @glove-patterns-by-id %))
           (mapcat :neighbors)
           (remove #(contains? seed-set (:id %)))
           (keep (fn [entry]
                   (when-let [score (:score entry)]
                     (assoc entry
                            :score/similarity (double score)
                            :score (score->distance score)))))
           (group-by :id)
           (map (fn [[_ entries]]
                  (apply max-key :score/similarity entries)))
           (sort-by :score)
           (take limit)
           vec))
    []))

(defn- merge-patterns [sigil-patterns glove-patterns limit]
  (let [glove-enriched (->> glove-patterns
                            (map (fn [entry]
                                   (merge (get @patterns-by-id (:id entry)) entry))))
        deduped (->> (concat sigil-patterns glove-enriched)
                     (group-by :id)
                     (map (fn [[_ entries]]
                            (apply min-key :score entries)))
                     (sort-by :score))]
    (if limit
      (vec (take limit deduped))
      (vec deduped))))

(defn- nearest-fruits [targets limit]
  (let [primary (first targets)]
    (if-let [emoji (:emoji primary)]
      (->> @fruits-data
           (keep (fn [fruit]
                   (when-let [d (emoji-distance emoji (:emoji fruit))]
                     (assoc fruit :score d))))
           (sort-by :score)
           (take limit)
           vec)
      [])))

(defn- nearest-paramitas [targets limit]
  (let [hanzi-targets (keep :hanzi targets)]
    (if (seq hanzi-targets)
      (->> @paramita-data
           (keep (fn [paramita]
                   (when-let [d (some->> hanzi-targets
                                         (keep #(hanzi-distance % (:zh paramita)))
                                         seq
                                         (reduce min))]
                     (assoc paramita :score d))))
           (sort-by :score)
           (take limit)
           vec)
      [])))

(defn- resolve-target-sigils [{:keys [sigils prototypes]}]
  (cond
    (seq sigils) sigils
    (seq prototypes)
    (some->> prototypes
             (map #(get @prototype->sigils %))
             (filter seq)
             first)
    :else nil))

(defn hints [{:keys [sigils prototypes pattern-limit fruit-limit paramita-limit glove-pattern-limit]
              :or {pattern-limit 4
                   fruit-limit 2
                   paramita-limit 2}}]
  (let [targets (resolve-target-sigils {:sigils sigils :prototypes prototypes})
        patterns (if (seq targets)
                   (nearest targets @ldts-patterns pattern-limit)
                   [])
        glove-limit (or glove-pattern-limit pattern-limit)
        seed-ids (cond
                   (seq patterns) (map :id patterns)
                   (seq prototypes) prototypes
                   :else nil)
        glove-patterns (if (seq seed-ids)
                         (glove-neighbors seed-ids glove-limit)
                         [])
        merged-patterns (merge-patterns patterns glove-patterns pattern-limit)]
    (when-not (seq targets)
      (warn-missing-targets sigils prototypes))
    {:patterns merged-patterns
     :glove-patterns glove-patterns
     :fruits (if (seq targets)
               (nearest-fruits targets fruit-limit)
               [])
     :paramitas (if (seq targets)
                  (nearest-paramitas targets paramita-limit)
                  [])}))

(defn fruits-for-sigils
  "Return the nearest fruit entries for the supplied SIGILS vector."
  [sigils {:keys [limit] :or {limit 2}}]
  (if (seq sigils)
    (nearest-fruits sigils limit)
    []))

(defn paramitas-for-sigils
  "Return the nearest pāramitā entries for the supplied SIGILS vector."
  [sigils {:keys [limit] :or {limit 2}}]
  (if (seq sigils)
    (nearest-paramitas sigils limit)
    []))

(defn fruit-definitions
  "Return the cached fruit definition vector (see resources/sigils/fruits.edn)."
  []
  @fruits-data)

(defn paramita-definitions
  "Return the cached paramita definition vector (see resources/sigils/paramitas.edn)."
  []
  @paramita-data)
