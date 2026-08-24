(ns futon3.inbox-zero.infer
  "Pure, fail-closed attribution inference.

  Adapters normalize evidence into plain data; this namespace only ranks it.
  Path-bearing evidence joins solely on worktree id plus path. Repository ids
  are labels in the returned path key, never identity for an internal join."
  (:require [clojure.string :as str]))

(def ^:private confidence-rank
  {:direct 0 :corroborated 1 :weak 2})

(defn- path-key [value]
  (select-keys value [:worktree/id :path]))

(defn- same-path? [path-fact evidence]
  (= (path-key path-fact) (path-key evidence)))

(defn- time-ms [value]
  (cond
    (number? value) (long value)
    (instance? java.util.Date value) (.getTime ^java.util.Date value)
    (instance? java.time.Instant value) (.toEpochMilli ^java.time.Instant value)
    (string? value) (.toEpochMilli (java.time.Instant/parse value))
    :else nil))

(defn- in-window? [at {:keys [from to]}]
  (let [at-ms (time-ms at)
        from-ms (time-ms from)
        to-ms (time-ms to)]
    (and at-ms from-ms to-ms
         (<= from-ms at-ms to-ms))))

(defn- evidence-view [evidence]
  (select-keys evidence [:evidence/type :at :source/id]))

(defn- typed [kind evidence]
  (evidence-view (assoc evidence :evidence/type kind
                                 :at (or (:at evidence) (:from evidence)))))

(defn- candidate-seats [bundle]
  (->> (concat (:structured-writes bundle)
               (filter #(and (true? (:candidate-authored? %))
                             (true? (:names-modification? %)))
                       (:substrate-mentions bundle))
               (:same-worktree-claims bundle))
       (keep :seat/id)
       set))

(defn- windows-for [bundle seat-id worktree-id]
  (filter #(and (= seat-id (:seat/id %))
                (= worktree-id (:worktree/id %)))
          (:activity-windows bundle)))

(defn- direct-evidence [path-fact bundle seat-id]
  (->> (:structured-writes bundle)
       (filter #(and (= seat-id (:seat/id %))
                     (same-path? path-fact %)))
       (map #(typed :structured-path-write %))))

(defn- corroborated-evidence [path-fact bundle seat-id]
  (let [windows (windows-for bundle seat-id (:worktree/id path-fact))
        eligible-mentions (filter #(and (= seat-id (:seat/id %))
                                        (same-path? path-fact %)
                                        (true? (:candidate-authored? %))
                                        (true? (:names-modification? %)))
                                  (:substrate-mentions bundle))
        eligible-mtimes (filter (partial same-path? path-fact) (:mtimes bundle))
        corroborating-windows
        (filter #(and (some (fn [mention] (in-window? (:at mention) %))
                            eligible-mentions)
                      (some (fn [mtime] (in-window? (:at mtime) %))
                            eligible-mtimes))
                windows)
        mentions (filter #(some (partial in-window? (:at %)) corroborating-windows)
                         eligible-mentions)
        mtimes (filter #(some (partial in-window? (:at %)) corroborating-windows)
                       eligible-mtimes)]
    (when (seq corroborating-windows)
      (concat (map #(typed :substrate-modification-mention %) mentions)
              (map #(typed :filesystem-mtime %) mtimes)
              (map #(typed :activity-window %) corroborating-windows)))))

(defn- worktree-claims [path-fact bundle seat-id]
  (->> (:same-worktree-claims bundle)
       (filter #(and (= seat-id (:seat/id %))
                     (= (:worktree/id path-fact) (:worktree/id %))))
       (map #(typed :same-worktree-claim %))))

(defn- weak-evidence [path-fact bundle seat-id]
  (let [windows (windows-for bundle seat-id (:worktree/id path-fact))
        mtimes (filter #(and (same-path? path-fact %)
                             (some (partial in-window? (:at %)) windows))
                       (:mtimes bundle))
        overlapping-windows
        (filter #(some (fn [mtime] (in-window? (:at mtime) %)) mtimes) windows)]
    (when (and (seq mtimes) (seq overlapping-windows))
      (concat (worktree-claims path-fact bundle seat-id)
              (map #(typed :filesystem-mtime %) mtimes)
              (map #(typed :activity-window %) overlapping-windows)))))

(defn- candidate [path-fact bundle unique-claim-seat seat-id]
  (let [direct (vec (direct-evidence path-fact bundle seat-id))
        corroborated (vec (corroborated-evidence path-fact bundle seat-id))
        claims (vec (worktree-claims path-fact bundle seat-id))
        weak (when (= seat-id unique-claim-seat)
               (vec (weak-evidence path-fact bundle seat-id)))
        confidence (cond
                     (seq direct) :direct
                     (seq corroborated) :corroborated
                     (seq weak) :weak
                     :else nil)
        evidence (case confidence
                   :direct (concat direct corroborated claims)
                   :corroborated (concat corroborated claims)
                   :weak weak
                   [])]
    (when confidence
      {:seat/id seat-id
       :confidence confidence
       :evidence (->> evidence distinct (sort-by (juxt :at :evidence/type :source/id)) vec)
       :against []})))

(defn infer-attribution
  "Rank candidate seats for PATH-FACT without minting or mutating anything.

  EVIDENCE-BUNDLE uses the normalized adapter schema documented by the
  discovery slice. Weak evidence proposes only when OPTIONS contains
  `:allow-weak? true`; otherwise it remains visible but insufficient."
  ([path-fact evidence-bundle]
   (infer-attribution path-fact evidence-bundle {}))
  ([path-fact evidence-bundle {:keys [allow-weak?] :or {allow-weak? false}}]
   (let [claim-seats (->> (:same-worktree-claims evidence-bundle)
                          (filter #(= (:worktree/id path-fact) (:worktree/id %)))
                          (keep :seat/id)
                          set)
         unique-claim-seat (when (= 1 (count claim-seats)) (first claim-seats))
         candidates (->> (candidate-seats evidence-bundle)
                         (keep #(candidate path-fact evidence-bundle
                                           unique-claim-seat %))
                         (sort-by (juxt (comp confidence-rank :confidence) :seat/id))
                         (map-indexed #(assoc %2 :rank (inc %1)))
                         vec)
         best-confidence (:confidence (first candidates))
         tied-best (count (take-while #(= best-confidence (:confidence %)) candidates))
         verdict (cond
                   (empty? candidates) :insufficient
                   (> tied-best 1) :ambiguous
                   (and (= :weak best-confidence) (not allow-weak?)) :insufficient
                   :else :propose)]
     {:path/key (select-keys path-fact [:repo/id :worktree/id :path])
      :verdict verdict
      :candidates candidates})))

(defn confirmation-followup-text
  "Render a confirmation request that names exact identity and evidence ids."
  [result candidate]
  (let [key (:path/key result)
        repo-id (:repo/id key)
        worktree-id (:worktree/id key)
        path (:path key)
        evidence-ids (->> (:evidence candidate) (keep :source/id) distinct sort)]
    (str "Attribution proposal: did " (:seat/id candidate) " modify "
         repo-id ":" path " in " worktree-id "? Evidence: "
         (if (seq evidence-ids) (str/join ", " evidence-ids) "none")
         ". Confirm / reject / not sure. Confirming mints the file claim; "
         "inference alone changes nothing.")))
