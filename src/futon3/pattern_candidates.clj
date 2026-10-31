(ns futon3.pattern-candidates
  "Hybrid candidate-set builder for pattern retrieval (mission
  M-pattern-retrieval-calibration, P-2).

  Produces the M-2 candidate-set pool: a deduplicated list of at most
  six pattern packets, each carrying the retrieval sources that surfaced
  it (:classical and/or :embedding), from two rankers:

  - classical: hotword-overlap + title-overlap + BECAUSE-clause overlap
    between the query and the canonical packet emitted by
    futon.flexiarg.projection;
  - embedding: cosine between a caller-supplied query vector and the
    precomputed MiniLM pattern vectors (data/notions/minilm_pattern_embeddings.json).

  Determinism contract (mission acceptance, 2026-09-30 line 1028):
  identical query and corpus inputs produce byte-identical
  serialized candidate sets.  Ordering is a total order (score desc,
  then pattern id asc); input corpus order cannot leak into the
  output.  The embedding half runs only when a query vector is
  supplied; without one the pool is classical-only, still capped and
  deterministic.  Wiring run-futon3a-search (futon3c) to this builder
  is deliberately out of scope here."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def default-projections-path "data/notions/pattern-projections.edn")
(def default-embeddings-path "data/notions/minilm_pattern_embeddings.json")
(def pool-cap 6)

;; ---------------------------------------------------------------------------
;; Tokenization (shared shape with futon3.cue-embedding)
;; ---------------------------------------------------------------------------

(def ^:private min-token-length 3)

(defn tokenize
  "Lowercase word tokens of length >= 3, distinct, in first-seen order."
  [text]
  (when-not (str/blank? text)
    (->> (str/split (str/lower-case text) #"[^\p{L}\p{N}]+")
         (remove #(or (empty? %) (< (count %) min-token-length)))
         (distinct)
         (vec))))

;; ---------------------------------------------------------------------------
;; Corpus loading
;; ---------------------------------------------------------------------------

(defn load-packets
  "Read canonical pattern packets from a pattern-projections EDN file."
  ([]
   (load-packets default-projections-path))
  ([path]
   (with-open [r (io/reader path)]
     (edn/read (java.io.PushbackReader. r)))))

;; ---------------------------------------------------------------------------
;; Rankers
;; ---------------------------------------------------------------------------

(defn- because-text
  "Text of the first BECAUSE clause of a canonical packet, if any."
  [packet]
  (some->> (:pattern/clauses packet)
           (filter #(= "because" (:name-key %)))
           (first)
           (:text)))

(defn- overlap-count
  [query-tokens ^String text]
  (when-not (str/blank? text)
    (count (set/intersection
            (set query-tokens)
            (set (tokenize text))))))

(defn classical-score
  "Score one packet against tokenized query terms: 2 per hotword
  (:pattern/keywords) overlap, 1 per title overlap, 1 per BECAUSE-clause
  overlap.  nil when every component is zero (no candidate)."
  [query-tokens packet]
  (let [kw (overlap-count query-tokens (str/join " " (:pattern/keywords packet)))
        title (overlap-count query-tokens (:pattern/title packet))
        because (overlap-count query-tokens (because-text packet))
        total (+ (* 2 (or kw 0)) (or title 0) (or because 0))]
    (when (pos? total) total)))

(defn- dot
  [xs ys]
  (reduce + (map * xs ys)))

(defn cosine
  "Cosine similarity of two equal-length numeric vectors; 0.0 on
  shape mismatch rather than an exception (a malformed corpus entry
  is not a candidate, and fails visibly by being absent)."
  [xs ys]
  (if (and (sequential? xs) (sequential? ys)
           (= (count xs) (count ys))
           (pos? (count xs)))
    (let [n (dot xs ys)
          d (Math/sqrt (* (dot xs xs) (dot ys ys)))]
      (if (pos? d) (/ n d) 0.0))
    0.0))

(defn embedding-score
  "Cosine between the query vector and a pattern vector."
  [query-vector pattern-vector]
  (when query-vector
    (let [c (cosine query-vector pattern-vector)]
      (when (pos? c) c))))

;; ---------------------------------------------------------------------------
;; Candidate set
;; ---------------------------------------------------------------------------

(defn- rank
  "Top-n (score desc, id asc) of [id score] pairs; total order so the
  output cannot depend on corpus input order."
  [n score-by-id]
  (->> score-by-id
       (sort-by (fn [[id score]] [(- score) id]))
       (take n)))

(defn candidate-set
  "Build the M-2 candidate pool.

  Input map:
    :query         query text (required)
    :query-vector  optional embedding of the query (enables the
                   embedding ranker)
    :packets       canonical packets (required)
    :embeddings    map pattern-id -> vector, optional

  Output: {:query ..., :candidates [{:pattern/id ...
                                     :retrieval/sources #{...}
                                     :retrieval/scores {...}} ...]}
  Deduplicated by :pattern/id, capped at pool-cap (6), totally ordered."
  [{:keys [query query-vector packets embeddings]}]
  {:pre [(string? query) (sequential? packets)]}
  (let [query-tokens (set (tokenize query))
        indexed (into {} (comp (filter :pattern/id)
                               (map (juxt :pattern/id identity)))
                      packets)
        classical (into {}
                        (keep (fn [[id packet]]
                                (when-some [s (classical-score query-tokens packet)]
                                  [id s])))
                        indexed)
        embedding (if (and query-vector (map? embeddings))
                    (into {}
                          (keep (fn [[id vec]]
                                  (when (contains? indexed id)
                                    (when-some [s (embedding-score query-vector vec)]
                                      [id s]))))
                          embeddings)
                    {})
        pool (->> (rank pool-cap (merge-with max classical embedding))
                  (map (fn [[id _score]]
                         (let [sources (cond-> #{}
                                         (contains? classical id) (conj :classical)
                                         (contains? embedding id) (conj :embedding))
                               scores (cond-> {}
                                        (contains? classical id) (assoc :classical (get classical id))
                                        (contains? embedding id) (assoc :embedding (get embedding id)))]
                           {:pattern/id id
                            :retrieval/sources sources
                            :retrieval/scores scores})))
                  (vec))]
    {:query query
     :candidates pool}))

(defn candidate-set-str
  "Deterministic EDN serialization of a candidate set (newline
  terminated).  Byte-identical for identical query and corpus inputs."
  [candidate-set]
  (str (pr-str candidate-set) "\n"))
