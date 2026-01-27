(ns futon3.pattern-check.integration
  "Integration layer for realtime pattern-check loop.

   Connects IRC line events to the core pattern-check engine and writes
   JSONL output. This namespace handles all IO; the core module is pure.

   Architecture:
   - Ingest thread: reads lines from IRC, enqueues to buffer
   - Worker thread: builds batches, runs core.process-batch, writes JSONL
   - Bounded queue with drop-oldest overflow behavior

   Usage:
     (def checker (start! {:jsonl-path \"/tmp/musn_pattern_checks.jsonl\"}))
     (ingest! checker {:ts \"2026-01-27T18:00:00Z\" :source :irc :raw \"...\"})
     (stop! checker)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [futon3.chops :as chops])
  (:import [java.time Instant]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; --- Configuration defaults (per spec) ---

(def ^:private default-config
  {:max-buffer-lines 200
   :max-wait-ms 2000
   :max-lines 50
   :jsonl-path "/tmp/musn_pattern_checks.jsonl"
   :devmap-globs ["resources/devmaps/*.edn" "resources/sigils/patterns-index.tsv"]
   :cache-refresh-ms 60000})

;; --- Line item normalization ---

(defn normalize-line
  "Normalize a raw line into a line item with standard fields."
  [raw & {:keys [source channel nick]}]
  {:ts (str (Instant/now))
   :source (or source :unknown)
   :raw raw
   :channel channel
   :nick nick})

;; --- Sigil extraction ---

(defn- extract-sigil-candidates
  "Extract potential sigil strings from text.
   Sigils match: emoji/hanzi or tokipona/hanzi patterns.
   We use a permissive pattern and let chops.validate-sigil filter valid ones."
  [text]
  (when text
    ;; Match: X/Y where X is non-whitespace and Y is a single character
    ;; This catches both emoji/hanzi (🐜/予) and tokipona/hanzi (lili/予)
    ;; The chops validator will filter out invalid candidates
    (let [pattern #"\S+/\S"]
      (->> (re-seq pattern text)
           ;; Filter to likely sigils: contains / and second part is 1 char
           (filter (fn [s]
                     (let [parts (str/split s #"/")]
                       (and (= 2 (count parts))
                            (= 1 (count (second parts)))
                            ;; Exclude EDN keywords like :pattern/id
                            (not (str/starts-with? s ":"))
                            ;; Exclude URLs
                            (not (str/includes? s "//"))))))))))

(defn- validate-sigil-candidates
  "Validate extracted sigil candidates using chops."
  [candidates]
  (let [results (map (fn [sigil]
                       (let [validation (chops/validate-sigil sigil)]
                         (assoc validation :sigil sigil)))
                     candidates)]
    {:checked (count results)
     :valid (count (filter :valid? results))
     :invalid (filterv (comp not :valid?) results)
     :readings (filterv identity
                        (map (fn [{:keys [valid? decoded sigil]}]
                               (when (and valid? decoded)
                                 {:sigil sigil
                                  :word1 (:word1 decoded)
                                  :word2 (:word2 decoded)
                                  :reading (:reading decoded)}))
                             results))}))

;; --- Structured event detection (PSR/PUR) ---

(defn- try-parse-edn
  "Try to parse a string as EDN, return nil on failure."
  [s]
  (try
    (when (and s (str/includes? s "{"))
      (let [start (str/index-of s "{")
            edn-str (subs s start)]
        (read-string edn-str)))
    (catch Exception _ nil)))

(defn- extract-psr-event
  "Extract PSR info from a structured event if present."
  [event]
  (when (and (map? event)
             (#{:pattern/selection-claimed :turn/select} (:event event)))
    {:pattern-id (:pattern/id event)
     :session-id (:session/id event)
     :decision-id (:decision/id event)
     :event-type (:event event)}))

(defn- extract-pur-event
  "Extract PUR info from a structured event if present."
  [event]
  (when (and (map? event)
             (#{:pattern/use-claimed :turn/use} (:event event)))
    {:pattern-id (:pattern/id event)
     :session-id (:session/id event)
     :decision-id (:decision/id event)
     :event-type (:event event)
     :outcome (:outcome event)
     :evidence (:evidence event)}))

(defn- detect-psr-pur
  "Detect PSR and PUR events in a batch of lines."
  [lines]
  (let [events (keep (fn [line]
                       (when-let [event (try-parse-edn (:raw line))]
                         event))
                     lines)
        psrs (keep extract-psr-event events)
        purs (keep extract-pur-event events)]
    {:psr {:seen (count psrs)
           :ids (vec (distinct (keep :pattern-id psrs)))}
     :pur {:seen (count purs)
           :ids (vec (distinct (keep :pattern-id purs)))}}))

;; --- Batch processing ---

(defn process-batch
  "Process a batch of line items and return a summary report.
   This is a lightweight wrapper that will delegate to core.clj when available."
  [lines {:keys [dropped skipped-batches]}]
  (let [all-text (str/join " " (map :raw lines))
        sigil-candidates (extract-sigil-candidates all-text)
        sigil-results (validate-sigil-candidates sigil-candidates)
        psr-pur (detect-psr-pur lines)
        ts (str (Instant/now))]
    {:ts ts
     :source (or (some :source lines) :unknown)
     :batch {:lines (count lines)
             :dropped (or dropped 0)
             :skipped_batches (or skipped-batches 0)
             :latency_ms 0}  ; TODO: track actual latency
     :sigils {:checked (:checked sigil-results)
              :valid (:valid sigil-results)
              :invalid (mapv :sigil (:invalid sigil-results))}
     :sigil_reads (:readings sigil-results)
     :duplicates {:pairs [] :count 0}  ; TODO: delegate to core.clj
     :psr (:psr psr-pur)
     :pur (:pur psr-pur)
     :errors []}))

;; --- JSONL writer ---

(defn- write-jsonl!
  "Append a record to the JSONL file."
  [path record]
  (let [json-line (json/write-str record)]
    (spit path (str json-line "\n") :append true)))

;; --- Async runtime ---

(defrecord PatternChecker [config buffer worker-future running?])

(defn- worker-loop
  "Worker loop that processes batches from the buffer."
  [checker]
  (let [{:keys [config buffer running?]} checker
        {:keys [max-wait-ms max-lines jsonl-path]} config]
    (loop [batch []
           batch-start nil]
      (when @running?
        (let [timeout-ms (if batch-start
                           (max 0 (- max-wait-ms (- (System/currentTimeMillis) batch-start)))
                           max-wait-ms)
              item (.poll ^LinkedBlockingQueue buffer timeout-ms TimeUnit/MILLISECONDS)]
          (cond
            ;; Received item
            item
            (let [new-batch (conj batch item)
                  new-start (or batch-start (System/currentTimeMillis))]
              (if (>= (count new-batch) max-lines)
                ;; Batch full, process it
                (do
                  (when (seq new-batch)
                    (let [report (process-batch new-batch {})]
                      (write-jsonl! jsonl-path report)))
                  (recur [] nil))
                ;; Continue batching
                (recur new-batch new-start)))

            ;; Timeout - flush current batch
            (seq batch)
            (do
              (let [report (process-batch batch {})]
                (write-jsonl! jsonl-path report))
              (recur [] nil))

            ;; Nothing to do
            :else
            (recur [] nil)))))))

(defn start!
  "Start the pattern checker with the given config."
  [config-overrides]
  (let [config (merge default-config config-overrides)
        buffer (LinkedBlockingQueue. ^int (:max-buffer-lines config))
        running? (atom true)
        checker (map->PatternChecker {:config config
                                      :buffer buffer
                                      :running? running?})]
    (assoc checker
           :worker-future
           (future (worker-loop checker)))))

(defn stop!
  "Stop the pattern checker."
  [checker]
  (reset! (:running? checker) false)
  (when-let [f (:worker-future checker)]
    (future-cancel f)))

(defn ingest!
  "Ingest a line item into the buffer. Drops oldest if full."
  [checker line-item]
  (let [{:keys [buffer config]} checker
        {:keys [max-buffer-lines]} config]
    ;; Drop oldest if at capacity
    (while (>= (.size ^LinkedBlockingQueue buffer) max-buffer-lines)
      (.poll ^LinkedBlockingQueue buffer))
    (.offer ^LinkedBlockingQueue buffer line-item)))

;; --- IRC integration helpers ---

(defn parse-irc-privmsg
  "Parse an IRC PRIVMSG line into a line item.
   Format: :nick!user@host PRIVMSG #room :message"
  [raw-line]
  (when (and raw-line (str/includes? raw-line " PRIVMSG "))
    (let [[prefix rest] (str/split (subs raw-line 1) #" " 2)]
      (when (and rest (str/includes? rest " :"))
        (let [[cmd-part text] (str/split rest #" :" 2)
              parts (str/split cmd-part #" ")
              target (second parts)
              nick (first (str/split prefix #"!"))]
          (normalize-line text
                          :source :irc
                          :channel target
                          :nick nick))))))

(defn make-irc-handler
  "Create an IRC line handler that ingests into a pattern checker."
  [checker]
  (fn [raw-line]
    (when-let [line-item (parse-irc-privmsg raw-line)]
      (ingest! checker line-item))))

;; --- REPL / testing helpers ---

(comment
  ;; Start checker
  (def checker (start! {:jsonl-path "/tmp/test_pattern_checks.jsonl"}))

  ;; Simulate IRC lines
  (ingest! checker (normalize-line "Test message with sigil 🐜/予" :source :irc))
  (ingest! checker (normalize-line "{:event :pattern/selection-claimed :pattern/id \"test\"}" :source :irc))

  ;; Wait for batch
  (Thread/sleep 3000)

  ;; Check output
  (slurp "/tmp/test_pattern_checks.jsonl")

  ;; Stop
  (stop! checker)
  )
