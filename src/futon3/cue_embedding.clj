(ns futon3.cue-embedding
  "Embed Tatami fruit/paramitā scores into fixed vectors.
   Also project intent→pattern matches into the fruit/orb (pāramitā) field."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3.pattern-hints :as hints]))

;; ---------------------------------------------------------------------------
;; Token helpers
;; ---------------------------------------------------------------------------

(def ^:private min-token-length 3)

(defn- normalize-text [value]
  (when-let [raw (cond
                   (string? value) value
                   (keyword? value) (name value)
                   (symbol? value) (name value)
                   (nil? value) nil
                   :else (str value))]
    (let [trimmed (str/trim raw)]
      (when (seq trimmed)
        trimmed))))

(defn tokenize [text]
  (when-let [body (normalize-text text)]
    (->> (str/split (str/lower-case body) #"[^\p{L}\p{N}]+")
         (remove #(or (empty? %) (< (count %) min-token-length)))
         distinct
         vec)))

(defn- similarity-from-distance [distance]
  (if (number? distance)
    (-> 1.0 (- (min 1.0 (double distance))) (max 0.0))
    0.0))

(defn- pattern-text [pattern]
  (some->> [(normalize-text (get pattern :summary))
            (normalize-text (get pattern :title))
            (normalize-text (get pattern :id))]
           (remove #(or (nil? %) (str/blank? %)))
           (str/join " ")))

(defn- score-pattern-match [intent-set pattern]
  (when-let [body (pattern-text pattern)]
    (let [pattern-set (set (tokenize body))
          overlap (set/intersection intent-set pattern-set)]
      (when (seq overlap)
        (let [similarity (similarity-from-distance (:score pattern))
              weight (+ (count overlap) similarity)]
          {:pattern pattern
           :tokens (vec overlap)
           :weight weight})))))

(defn- select-intent-sigils [{:keys [intent patterns max-patterns]}]
  (let [tokens (set (tokenize intent))
        matches (->> patterns
                     (keep #(score-pattern-match tokens %))
                     (sort-by :weight >)
                     (take (or max-patterns 3))
                     vec)
        sigils (->> matches
                    (mapcat (comp (fnil identity []) :sigils :pattern))
                    (remove nil?)
                    vec)]
    (when (seq sigils)
      {:sigils sigils
       :matches matches
       :tokens (vec tokens)})))

(defn intent-pattern-cues
  "Project INTENT tokens through PATTERNS (with sigils) and return cue info."
  [{:keys [intent patterns fruit-limit paramita-limit max-patterns]
    :or {fruit-limit 2
         paramita-limit 2
         max-patterns 3}}]
  (let [intent-text (normalize-text intent)]
    (when (and (seq intent-text) (seq patterns))
      (when-let [{:keys [sigils matches tokens]}
                 (select-intent-sigils {:intent intent-text
                                        :patterns patterns
                                        :max-patterns max-patterns})]
        {:sigils sigils
         :tokens tokens
         :matches (mapv (fn [{:keys [pattern tokens weight]}]
                          {:pattern/id (:id pattern)
                           :pattern/title (:title pattern)
                           :tokens tokens
                           :weight weight})
                        matches)
         :fruits (hints/fruits-for-sigils sigils {:limit fruit-limit})
         :paramitas (hints/paramitas-for-sigils sigils {:limit paramita-limit})}))))

;; ---------------------------------------------------------------------------
;; Embedding helpers
;; ---------------------------------------------------------------------------

(defn- canonical-id [value]
  (cond
    (keyword? value) value
    (string? value) (-> value str/lower-case keyword)
    (symbol? value) (-> value name str/lower-case keyword)
    :else nil))

(defn- normal-order [entries id-key]
  (->> entries (map id-key) (map canonical-id) vec))

(def ^:private fruit-order
  (delay (normal-order (hints/fruit-definitions) :fruit/id)))

(def ^:private paramita-order
  (delay (normal-order (hints/paramita-definitions) :paramita/id)))

(defn- vector-from [order entries id-key]
  (let [positions (zipmap order (range))
        zeroes    (vec (repeat (count order) 0.0))]
    (reduce (fn [acc entry]
              (if-let [idx (some-> (get entry id-key) canonical-id positions)]
                (assoc acc idx (double (or (:score entry) 0.0)))
                acc))
            zeroes
            entries)))

(defn- embedding-block [order entries id-key]
  (let [vector (vector-from order entries id-key)]
    {:order order
     :vector vector
     :scores (zipmap order vector)
     :entries (vec entries)}))

(defn- normalize-score-seq [entries id-key]
  (cond
    (vector? entries) entries
    (map? entries) (mapv (fn [[k score]] {id-key k :score (double score)}) entries)
    :else []))

(defn embed-cues [{:keys [fruits paramitas]}]
  (let [fruit-seq (normalize-score-seq fruits :fruit/id)
        paramita-seq (normalize-score-seq paramitas :paramita/id)]
    {:fruits (embedding-block @fruit-order fruit-seq :fruit/id)
     :paramitas (embedding-block @paramita-order paramita-seq :paramita/id)}))

;; ---------------------------------------------------------------------------
;; English corpus fallback (fruits/orbs descriptions)
;; ---------------------------------------------------------------------------

(def ^:private fruit-orb-corpus
  (delay (try
           (edn/read-string (slurp (io/file "resources/sigils/fruit-orb-corpus.edn")))
           (catch Exception _ {:fruits [] :paramitas []}))))

(def ^:private fruit-info
  (delay (into {} (map (juxt :fruit/id identity)) (hints/fruit-definitions))))

(def ^:private paramita-info
  (delay (into {} (map (juxt :paramita/id identity)) (hints/paramita-definitions))))

(defn- summarize-description [desc]
  (cond
    (string? desc) (str/trim desc)
    (map? desc) (->> [:professional :karmic :somatic]
                     (map #(get desc %))
                     (remove str/blank?)
                     (str/join " "))
    :else nil))

(defn- fruit-corpus-patterns []
  (for [entry (:fruits @fruit-orb-corpus)
        :let [id (:fruit/id entry)
              info (get @fruit-info id)
              summary (summarize-description (:description entry))]
        :when (and id info summary)]
    {:id (keyword (str "corpus/fruit-" (name id)))
     :title (format "%s fruit" (name id))
     :summary summary
     :sigils [{:emoji (:emoji info)
               :hanzi (:zh info)}]
     :score 0.2}))

(defn- paramita-corpus-patterns []
  (let [fallback (:paramitas @fruit-orb-corpus)
        data (if (seq fallback)
               fallback
               (map (fn [param]
                      {:paramita/id (:paramita/id param)
                       :emoji (:orb param)
                       :description (:sense param)})
                    (hints/paramita-definitions)))]
    (for [entry data
          :let [id (:paramita/id entry)
                info (get @paramita-info id)
                summary (summarize-description (:description entry))]
          :when (and id info summary)]
      {:id (keyword (str "corpus/paramita-" (name id)))
       :title (format "%s paramita" (name id))
       :summary summary
       :sigils [{:emoji (or (:emoji entry) (:orb info))
                 :hanzi (:zh info)}]
       :score 0.2})))

(defn- fallback-patterns []
  (vec (concat (fruit-corpus-patterns)
               (paramita-corpus-patterns))))

;; ---------------------------------------------------------------------------
;; Entry helpers
;; ---------------------------------------------------------------------------

(defn- entry-intent [entry]
  (some-> (or (:intent entry)
              (get-in entry [:tatami :intent])
              (get-in entry [:payload :intent]))
          normalize-text))

(defn- normalize-sigils [sigils]
  (->> sigils
       (keep (fn [pair]
               (let [pair-map (cond
                                (map? pair) pair
                                (sequential? pair) (apply hash-map pair)
                                :else nil)
                    emoji (some-> (:emoji pair-map) str/trim)
                    hanzi (some-> (:hanzi pair-map) str/trim)]
                 (when (or (and emoji (not (str/blank? emoji)))
                           (and hanzi (not (str/blank? hanzi))))
                   {:emoji (when (and emoji (not (str/blank? emoji))) emoji)
                    :hanzi (when (and hanzi (not (str/blank? hanzi))) hanzi)}))))
       vec))

(defn- normalize-pattern [pattern]
  (-> (cond
        (map? pattern) pattern
        (sequential? pattern) (apply hash-map pattern)
        :else {})
      (update :sigils normalize-sigils)))

(defn entry-intent-cues
  "Given a Tatami context entry (or map with :tatami), derive intent cues."
  [entry]
  (let [tatami (:tatami entry)
        intent (entry-intent entry)
        raw-patterns (or (:patterns tatami)
                         (:patterns entry)
                         [])
        patterns (->> raw-patterns
                      (map normalize-pattern)
                      (remove #(empty? (:sigils %))))]
    (when (seq intent)
      (or (when (seq patterns)
            (intent-pattern-cues {:intent intent :patterns patterns}))
          (let [fallback (fallback-patterns)]
            (when (seq fallback)
              (intent-pattern-cues {:intent intent
                                    :patterns fallback
                                    :fruit-limit 3
                                    :paramita-limit 3})))))))

(defn annotate-entry
  "Attach cue information to a Tatami context log entry."
  [entry]
  (let [entry-map (if (map? entry) entry (apply hash-map entry))
        tatami (:tatami entry-map)
        embedding (when tatami (embed-cues tatami))
        intent-cues (entry-intent-cues entry-map)]
    (cond-> entry-map
      embedding (assoc :cue/embedding embedding)
      intent-cues (assoc :cue/intent intent-cues))))
