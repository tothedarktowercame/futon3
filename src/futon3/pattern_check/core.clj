(ns futon3.pattern-check.core
  "Core batcher + matchers for realtime pattern-check loop.

   This namespace is pure and side-effect free except for optional timestamps.
   Integration should handle IO (IRC ingest, JSONL output).")
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.chops :as chops]))

(def default-config
  {:max-wait-ms 2000
   :max-lines 50
   :max-buffer 1000
   :duplicate-cache-ttl-ms 60000
   :include-ids? true
   :include-decoded? false})

(defn now-ms
  ([] (System/currentTimeMillis))
  ([^long override-ms] override-ms))

(defn new-buffer
  "Initialize a buffer state map."
  ([opts]
   {:config (merge default-config opts)
    :queue []
    :opened-at nil
    :dropped-lines 0
    :batch-id 0}))

(defn- line->raw
  "Extract a raw string from a line item or string."
  [line]
  (cond
    (string? line) line
    (map? line) (or (:raw line) (:line line) (:text line) (pr-str line))
    :else (str line)))

(defn- parse-edn-map
  "Best-effort EDN map parsing for structured events."
  [s]
  (when (and (string? s)
             (str/includes? s "{")
             (str/includes? s ":"))
    (try
      (let [v (edn/read-string {:readers {} :default (fn [_ _] nil)} s)]
        (when (map? v) v))
      (catch Exception _ nil))))

(defn- line->payload
  "Extract structured event payload from a line item, if present."
  [line]
  (cond
    (map? line) (cond
                  (map? (:payload line)) (:payload line)
                  (or (:event line) (:type line) (:name line)) line
                  :else nil)
    (string? line) (parse-edn-map line)
    :else nil))

(defn- strip-punct
  "Trim common punctuation around tokens."
  [s]
  (-> s
      (str/replace #"^[\"'`\(\)\[\]\{\},.;:!?]+" "")
      (str/replace #"[\"'`\(\)\[\]\{\},.;:!?]+$" "")))

(defn extract-sigils
  "Extract candidate sigil tokens (emoji/hanzi) from a string."
  [s]
  (let [tokens (->> (str/split (or s "") #"\s+")
                    (map strip-punct)
                    (filter #(str/includes? % "/")))]
    (->> tokens
         (map #(str/split % #"/" 2))
         (filter #(= 2 (count %)))
         (map #(str (first %) "/" (second %))))))

(defn validate-sigils
  "Validate sigils via futon3.chops. Returns counts, invalid list, and optional decoded reads."
  [sigils include-decoded?]
  (let [results (map (fn [sigil]
                       (assoc (chops/validate-sigil sigil) :sigil sigil))
                     sigils)
        valid (filter :valid? results)
        invalid (filter (comp not :valid?) results)
        decoded (when include-decoded?
                  (map (fn [{:keys [sigil decoded]}]
                         (merge {:sigil sigil} decoded))
                       valid))]
    {:sigils-checked (count sigils)
     :sigils-valid (count valid)
     :invalid-sigils (->> invalid (map :sigil) distinct vec)
     :decoded (when include-decoded? (vec decoded))}))

(def ^:private psr-events
  #{:pattern/selection-claimed :turn/select})

(def ^:private pur-events
  #{:pattern/use-claimed :turn/use})

(defn- event-type
  [m]
  (when-let [v (or (:event m) (:type m) (:name m))]
    (cond
      (keyword? v) v
      (string? v) (keyword v)
      :else nil)))

(defn- pattern-id
  [m]
  (or (:pattern/id m) (:pattern-id m) (get-in m [:pattern :id])))

(defn detect-psr-pur
  "Detect PSR/PUR structured events from payloads."
  [lines include-ids?]
  (let [events (keep line->payload lines)
        psr (filter #(contains? psr-events (event-type %)) events)
        pur (filter #(contains? pur-events (event-type %)) events)
        psr-ids (when include-ids?
                  (->> psr (map pattern-id) (filter some?) distinct vec))
        pur-ids (when include-ids?
                  (->> pur (map pattern-id) (filter some?) distinct vec))]
    {:psr-seen (count psr)
     :pur-seen (count pur)
     :psr-ids psr-ids
     :pur-ids pur-ids}))

(defn- expand-glob
  "Expand a simple glob within its parent directory (non-recursive)."
  [pattern]
  (let [path (io/file pattern)
        parent (.getParentFile path)
        name (.getName path)
        dir (or parent (io/file "."))
        matcher (.getPathMatcher (java.nio.file.FileSystems/getDefault)
                                 (str "glob:" name))]
    (when (.exists dir)
      (->> (.listFiles dir)
           (filter #(.isFile ^java.io.File %))
           (filter #(matcher (.toPath ^java.io.File %)))
           (map #(.getPath ^java.io.File %))))))

(defn- expand-paths
  [paths]
  (->> paths
       (mapcat (fn [p]
                 (if (re-find #"[\*\?\[]" p)
                   (or (expand-glob p) [])
                   [p])))
       distinct
       vec))

(defn- sigils-in-file
  [path]
  (try
    (let [content (slurp path)]
      (distinct (extract-sigils content)))
    (catch Exception _ [])))

(defn build-duplicate-index
  "Build sigil->locations index and duplicate set from file paths."
  [paths now-ms]
  (let [expanded (expand-paths paths)
        sigil->locations (reduce (fn [acc path]
                                   (let [sigils (sigils-in-file path)]
                                     (reduce (fn [m sigil]
                                               (update m sigil (fnil conj #{}) path))
                                             acc
                                             sigils)))
                                 {}
                                 expanded)
        duplicate-sigils (->> sigil->locations
                              (filter #(> (count (val %)) 1))
                              (map key)
                              set)]
    {:paths expanded
     :built-at now-ms
     :sigil->locations sigil->locations
     :duplicate-sigils duplicate-sigils}))

(defn refresh-duplicate-index
  "Refresh duplicate index if missing or stale."
  [index paths now-ms ttl-ms]
  (if (or (nil? index)
          (not= (set paths) (set (:paths index)))
          (> (- now-ms (:built-at index 0)) ttl-ms))
    (build-duplicate-index paths now-ms)
    index))

(defn ingest-lines
  "Add lines into the buffer; drop oldest if max-buffer exceeded."
  [state lines now-ms]
  (let [{:keys [max-buffer]} (:config state)
        ensure-open (fn [st]
                      (if (:opened-at st) st (assoc st :opened-at now-ms)))]
    (reduce (fn [st line]
              (let [st (ensure-open st)
                    queue (conj (:queue st) line)
                    overflow (max 0 (- (count queue) max-buffer))
                    queue (if (pos? overflow)
                            (subvec queue overflow)
                            queue)]
                (-> st
                    (assoc :queue queue)
                    (update :dropped-lines + overflow))))
            state
            lines)))

(defn ready?
  "Return true if batch should flush."
  [state now-ms]
  (let [{:keys [max-lines max-wait-ms]} (:config state)
        {:keys [queue opened-at]} state]
    (or (>= (count queue) max-lines)
        (and opened-at (>= (- now-ms opened-at) max-wait-ms)))))

(defn flush-ready
  "Flush current batch if ready; returns [new-state batch-or-nil]."
  [state now-ms]
  (if (ready? state now-ms)
    (let [batch-id (inc (:batch-id state))
          batch {:batch-id batch-id
                 :opened-at (:opened-at state)
                 :closed-at now-ms
                 :lines (:queue state)
                 :dropped-lines (:dropped-lines state)}
          new-state (-> state
                        (assoc :queue []
                               :opened-at nil
                               :dropped-lines 0
                               :batch-id batch-id))]
      [new-state batch])
    [state nil]))

(defn analyze-batch
  "Analyze a batch and return {:report ...} using provided duplicate index."
  [batch duplicate-index config]
  (let [lines (:lines batch)
        raw-lines (map line->raw lines)
        sigils (mapcat extract-sigils raw-lines)
        sigil-set (set sigils)
        {:keys [sigils-checked sigils-valid invalid-sigils decoded]} (validate-sigils sigils (:include-decoded? config))
        {:keys [psr-seen pur-seen psr-ids pur-ids]} (detect-psr-pur lines (:include-ids? config))
        dup-sigils (when duplicate-index
                     (->> (:duplicate-sigils duplicate-index)
                          (filter sigil-set)
                          vec))
        source (let [sources (->> lines (keep :source) distinct vec)]
                 (cond
                   (= 1 (count sources)) (first sources)
                   (seq sources) "mixed"
                   :else nil))
        report (cond-> {:ts (:closed-at batch)
                        :batch-id (:batch-id batch)
                        :source source
                        :batch-lines (count lines)
                        :sigils-checked sigils-checked
                        :sigils-valid sigils-valid
                        :invalid-sigils invalid-sigils
                        :duplicate-pairs (or dup-sigils [])
                        :psr-seen psr-seen
                        :pur-seen pur-seen}
                 (:include-ids? config) (assoc :psr-ids psr-ids :pur-ids pur-ids)
                 (:include-decoded? config) (assoc :sigil-reads decoded)
                 (pos? (:dropped-lines batch)) (assoc :dropped-lines (:dropped-lines batch)
                                                      :overload true))]
    {:report report}))

(defn process-batch
  "Public entrypoint: analyze batch with duplicate index refresh.

   Returns {:report map :duplicate-index updated-index}."
  [batch duplicate-index paths now-ms config]
  (let [index (refresh-duplicate-index duplicate-index paths now-ms (:duplicate-cache-ttl-ms config))
        {:keys [report]} (analyze-batch batch index config)]
    {:report report
     :duplicate-index index}))
