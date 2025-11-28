(ns futon3.pattern-hints
  "Structured pattern / fruit / paramita hints for ChatGPT integration."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- read-edn [path]
  (-> path io/file slurp edn/read-string))

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

(def ^:private clause-re
  (re-pattern "!\\s+(?:conclusion|claim):\\s*(.*?)\\s*\\[(.*?)\\]"))

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

(defn- scan-ldts []
  (for [file (file-seq (io/file "holes/LDTS"))
        :when (and (.isFile file)
                   (str/ends-with? (.getName file) ".flexiarg"))
        :let [text (slurp file)
              title (extract-meta text "title")
              arg (extract-meta text "arg")]
        match (re-seq clause-re text)]
    {:id (or arg (.getName file))
     :title title
     :summary (str/trim (second match))
     :sigils (split-sigils (nth match 2))}))

(def ^:private ldts-patterns (delay (vec (scan-ldts))))

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

(defn- emoji-distance [a b]
  (let [pa (get @emoji-pos a)
        pb (get @emoji-pos b)]
    (when (and pa pb)
      (/ (Math/abs ^long (- pa pb)) (double @emoji-norm)))))

(defn- hanzi-distance [a b]
  (let [pa (get @hanzi-pos a)
        pb (get @hanzi-pos b)]
    (when (and pa pb)
      (/ (Math/abs ^long (- pa pb)) (double @hanzi-norm)))))

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

(defn hints [{:keys [sigils prototypes limit pattern-limit fruit-limit paramita-limit]
              :or {pattern-limit 4
                   fruit-limit 2
                   paramita-limit 2}}]
  (let [targets (resolve-target-sigils {:sigils sigils :prototypes prototypes})]
    {:patterns (if (seq targets)
                 (nearest targets @ldts-patterns pattern-limit)
                 [])
     :fruits (if (seq targets)
               (nearest-fruits targets fruit-limit)
               [])
     :paramitas (if (seq targets)
                  (nearest-paramitas targets paramita-limit)
                  [])}))
