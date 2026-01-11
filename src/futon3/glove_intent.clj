;;;;
;; futon3.glove-intent
;;;;
(ns futon3.glove-intent
  "Compute intent-to-pattern embedding distances using local GloVe vectors."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.cue-embedding :as cue]))

(def ^:private default-glove-path
  "data/glove/glove.6B.50d.txt")

(defonce ^:private !glove-cache
  (atom {:vectors {}
         :loaded-tokens #{}}))

(defn- glove-path []
  (or (System/getenv "FUTON3_GLOVE_PATH") default-glove-path))

(defn- glove-intent-enabled? []
  (let [flag (some-> (System/getenv "FUTON3_GLOVE_INTENT") str/lower-case)]
    (and flag (not (str/blank? flag)) (not (#{"0" "false" "no"} flag)))))

(defn- parse-glove-line [line]
  (let [parts (str/split line #" ")
        token (first parts)
        values (rest parts)]
    (when (and token (seq values))
      [token (double-array (map #(Double/parseDouble %) values))])))

(defn- load-tokens!
  "Ensure TOKENS have vectors loaded into the cache.
Returns true if vectors were added, false otherwise."
  [tokens]
  (let [path (glove-path)
        file (io/file path)]
    (if (not (.exists file))
      false
      (let [tokens (set (remove str/blank? tokens))
            missing (remove #(contains? (:loaded-tokens @!glove-cache) %) tokens)]
        (if (seq missing)
          (with-open [reader (io/reader file)]
            (let [missing (set missing)]
              (doseq [line (line-seq reader)]
                (when-let [[token vec] (parse-glove-line line)]
                  (when (contains? missing token)
                    (swap! !glove-cache (fn [{:keys [vectors loaded-tokens] :as cache}]
                                          (-> cache
                                              (assoc :vectors (assoc vectors token vec))
                                              (assoc :loaded-tokens (conj loaded-tokens token))))))
                  (when (= (count missing) (count (:loaded-tokens @!glove-cache)))
                    (reduced nil)))))
            true)
          false)))))

(defn- vector-mean [vectors]
  (when (seq vectors)
    (let [dim (alength (first vectors))
          acc (double-array dim 0.0)
          count (double (count vectors))]
      (doseq [vec vectors]
        (dotimes [idx dim]
          (aset acc idx (+ (aget acc idx) (aget vec idx)))))
      (dotimes [idx dim]
        (aset acc idx (/ (aget acc idx) count)))
      acc)))

(defn- cosine-similarity [a b]
  (let [dim (alength a)]
    (loop [idx 0
           dot 0.0
           norm-a 0.0
           norm-b 0.0]
      (if (= idx dim)
        (when (and (> norm-a 0.0) (> norm-b 0.0))
          (/ dot (Math/sqrt (* norm-a norm-b))))
        (let [va (aget a idx)
              vb (aget b idx)]
          (recur (inc idx)
                 (+ dot (* va vb))
                 (+ norm-a (* va va))
                 (+ norm-b (* vb vb))))))))

(defn- text->tokens [text]
  (or (cue/tokenize text) []))

(defn- pattern-text [pattern]
  (->> [(get pattern :summary)
        (get pattern :title)
        (get pattern :id)]
       (map #(when (some? %) (str %)))
       (remove str/blank?)
       (str/join " ")))

(defn- text->vector [text]
  (let [tokens (text->tokens text)
        _ (load-tokens! tokens)
        vectors (keep #(get-in @!glove-cache [:vectors %]) tokens)]
    (vector-mean vectors)))

(defn attach-intent-embed-distance
  "Attach intent embedding distance/similarity to PATTERNS if GloVe vectors exist."
  [intent patterns]
  (let [intent-vec (text->vector intent)]
    (if (and (glove-intent-enabled?) intent-vec (seq patterns))
      (mapv (fn [pattern]
              (let [ptext (pattern-text pattern)
                    pvec (text->vector ptext)
                    sim (when (and pvec intent-vec)
                          (cosine-similarity intent-vec pvec))
                    dist (when (number? sim) (- 1.0 sim))]
                (cond-> pattern
                  (number? sim) (assoc :score/intent-embed-similarity sim)
                  (number? dist) (assoc :score/intent-embed-distance dist))))
            patterns)
      patterns)))
