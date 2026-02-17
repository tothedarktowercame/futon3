(ns futon3.chops
  "印章 (yìnzhāng) — Sigil validation and assignment service.

   Ensures sigils are canonical before they propagate through the stack.
   The canonical mapping lives in resources/tokizh/tokizh.org.

   Sigils encode TWO toki pona words as emoji/hanzi pairs:
   - emoji encodes word₁ (via tokizh emoji→tokipona mapping)
   - hanzi encodes word₂ (via tokizh hanzi→tokipona mapping)

   So 🎑/了 means 'alasa X' where X is whatever toki pona word maps to 了.
   The emoji and hanzi do NOT need to come from the same row.

   Valid sigils: any known emoji + any known hanzi (combinatorial).
   This gives ~124 × 256 ≈ 31,700 possible two-word sigils.

   Sigil formats:
   - emoji/hanzi (used in devmaps): e.g., 🐜/予 = lili/e
   - tokipona/hanzi (used in patterns): e.g., lili/予

   Bridge-derived assignment:
   - `assign-sigil` looks up the informed sigil for a pattern-id
   - The bridge chains: pattern → MiniLM embedding → PCA+Ridge → 8-bit
     exotype → truth-table-8 hanzi → tokizh emoji (where available)
   - Loaded from resources/sigils/bridge-assignments.edn
   - See scripts/update_sigils_from_bridge.clj to update flexiarg files"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Canonical data loading ---

(def ^:private tokizh-path "tokizh/tokizh.org")
(def ^:private truth-table-path "truth-table-8/truth-table-8.el")
(def ^:private canonical-cache (atom nil))
(def ^:private hanzi-256-cache (atom nil))

(defn- parse-tokizh-line [line]
  "Parse a tokizh.org table row into a map."
  (when (and (str/starts-with? line "|")
             (not (str/includes? line "---"))
             (not (str/includes? line "emoji")))
    (let [cells (->> (str/split line #"\|")
                     (map str/trim)
                     (filter not-empty))]
      (when (>= (count cells) 5)
        (let [[emoji tokipona _shortcode _english hanzi] cells]
          (when (and (not-empty emoji)
                     (not-empty tokipona)
                     (not-empty hanzi))
            {:emoji (str/trim emoji)
             :tokipona (str/trim tokipona)
             :hanzi (str/trim hanzi)}))))))

(defn- load-tokizh []
  "Load the canonical tokizh mapping from resources."
  (let [resource (io/resource tokizh-path)]
    (when resource
      (->> (slurp resource)
           str/split-lines
           (keep parse-tokizh-line)
           vec))))

(defn- parse-truth-table-line [line]
  "Parse a truth-table-8.el line into {:binary :hanzi :color}."
  (when-let [[_ binary hanzi color] (re-find #"\(\"([01]+)\"\s+\"(.)\"\s+\"(#[0-9a-fA-F]+)\"\)" line)]
    {:binary binary
     :hanzi hanzi
     :color color
     :index (Integer/parseInt binary 2)}))

(defn- load-hanzi-256 []
  "Load the 256-character hanzi set from truth-table-8."
  (let [resource (io/resource truth-table-path)]
    (when resource
      (->> (slurp resource)
           str/split-lines
           (keep parse-truth-table-line)
           vec))))

(defn hanzi-256
  "Return the 256-character hanzi set. Cached after first load."
  []
  (or @hanzi-256-cache
      (let [entries (load-hanzi-256)]
        (reset! hanzi-256-cache entries)
        entries)))

(defn canonical-sigils
  "Return the canonical sigil mappings. Cached after first load."
  []
  (or @canonical-cache
      (let [sigils (load-tokizh)]
        (reset! canonical-cache sigils)
        sigils)))

(defn reload-canonical!
  "Force reload of canonical sigils (e.g., after sync)."
  []
  (reset! canonical-cache nil)
  (canonical-sigils))

;; --- Index builders ---

(defn- build-emoji->entry []
  (into {} (map (juxt :emoji identity) (canonical-sigils))))

(defn- build-tokipona->entry []
  (into {} (map (juxt :tokipona identity) (canonical-sigils))))

(defn- build-hanzi->tokizh-entry []
  "Map hanzi from tokizh.org to their entries (for toki pona lookup)."
  (into {} (map (juxt :hanzi identity) (canonical-sigils))))

(defn- build-hanzi-256-set []
  "Set of all 256 valid hanzi characters."
  (into #{} (map :hanzi (hanzi-256))))

(defn- build-hanzi-256->entry []
  "Map hanzi to their truth-table entries (for binary/color lookup)."
  (into {} (map (juxt :hanzi identity) (hanzi-256))))

(def ^:private emoji->entry (delay (build-emoji->entry)))
(def ^:private tokipona->entry (delay (build-tokipona->entry)))
(def ^:private hanzi->tokizh (delay (build-hanzi->tokizh-entry)))
(def ^:private hanzi-256-set (delay (build-hanzi-256-set)))
(def ^:private hanzi-256->entry (delay (build-hanzi-256->entry)))

;; --- Validation functions ---

(defn valid-emoji?
  "Check if emoji exists in canonical set."
  [emoji]
  (contains? @emoji->entry emoji))

(defn valid-tokipona?
  "Check if toki pona word exists in canonical set."
  [tp]
  (contains? @tokipona->entry tp))

(defn valid-hanzi?
  "Check if hanzi exists in the 256-character canonical set."
  [hanzi]
  (contains? @hanzi-256-set hanzi))

(defn hanzi-info
  "Get full info for a hanzi: binary encoding, color, and toki pona (if mapped)."
  [hanzi]
  (let [tt-entry (get @hanzi-256->entry hanzi)
        tp-entry (get @hanzi->tokizh hanzi)]
    (when tt-entry
      (cond-> {:hanzi hanzi
               :binary (:binary tt-entry)
               :index (:index tt-entry)
               :color (:color tt-entry)}
        tp-entry (assoc :tokipona (:tokipona tp-entry)
                        :emoji (:emoji tp-entry))))))

(defn- parse-sigil-pair
  "Parse a sigil string like '🐜/予' or 'lili/予' into [left hanzi]."
  [s]
  (when s
    (let [parts (str/split s #"/")]
      (when (= 2 (count parts))
        [(str/trim (first parts))
         (str/trim (second parts))]))))

(defn hanzi->tokipona
  "Get toki pona word for a hanzi character (if it has a tokizh mapping)."
  [hanzi]
  (:tokipona (get @hanzi->tokizh hanzi)))

(defn validate-emoji-sigil
  "Validate an emoji/hanzi sigil pair.
   - Emoji must be in tokizh.org (~124 entries)
   - Hanzi must be in truth-table-8 (256 entries)
   The sigil encodes: emoji→toki pona word, hanzi→8-bit value (+ optional toki pona).
   Returns {:valid? true/false :errors [...] :decoded {...}}"
  [sigil-str]
  (if-let [[emoji hanzi] (parse-sigil-pair sigil-str)]
    (let [emoji-entry (get @emoji->entry emoji)
          hanzi-valid? (contains? @hanzi-256-set hanzi)
          hanzi-tp (hanzi->tokipona hanzi)  ; may be nil if not in tokizh
          hanzi-tt (get @hanzi-256->entry hanzi)
          errors (cond-> []
                   (nil? emoji-entry)
                   (conj {:type :unknown-emoji
                          :emoji emoji
                          :message (str "Unknown emoji: " emoji
                                       " (not in tokizh.org)")})

                   (not hanzi-valid?)
                   (conj {:type :unknown-hanzi
                          :hanzi hanzi
                          :message (str "Unknown hanzi: " hanzi
                                       " (not in truth-table-8)")}))]
      {:valid? (empty? errors)
       :errors errors
       :input sigil-str
       :decoded (when (and emoji-entry hanzi-valid?)
                  {:word1 (:tokipona emoji-entry)
                   :word2 hanzi-tp  ; may be nil
                   :emoji emoji
                   :hanzi hanzi
                   :binary (:binary hanzi-tt)
                   :reading (if hanzi-tp
                              (str (:tokipona emoji-entry) " " hanzi-tp)
                              (str (:tokipona emoji-entry) " [" hanzi "]"))})})
    {:valid? false
     :errors [{:type :parse-error
               :message (str "Cannot parse sigil: " sigil-str)}]
     :input sigil-str}))

(defn validate-tokipona-sigil
  "Validate a tokipona/hanzi sigil pair.
   - Toki pona word must be in tokizh.org (~124 entries)
   - Hanzi must be in truth-table-8 (256 entries)
   Returns {:valid? true/false :errors [...] :decoded {...}}"
  [sigil-str]
  (if-let [[tp hanzi] (parse-sigil-pair sigil-str)]
    (let [tp-entry (get @tokipona->entry tp)
          hanzi-valid? (contains? @hanzi-256-set hanzi)
          hanzi-tp (hanzi->tokipona hanzi)
          hanzi-tt (get @hanzi-256->entry hanzi)
          errors (cond-> []
                   (nil? tp-entry)
                   (conj {:type :unknown-tokipona
                          :tokipona tp
                          :message (str "Unknown toki pona word: " tp)})

                   (not hanzi-valid?)
                   (conj {:type :unknown-hanzi
                          :hanzi hanzi
                          :message (str "Unknown hanzi: " hanzi
                                       " (not in truth-table-8)")}))]
      {:valid? (empty? errors)
       :errors errors
       :input sigil-str
       :decoded (when (and tp-entry hanzi-valid?)
                  {:word1 tp
                   :word2 hanzi-tp
                   :emoji (:emoji tp-entry)
                   :hanzi hanzi
                   :binary (:binary hanzi-tt)
                   :reading (if hanzi-tp
                              (str tp " " hanzi-tp)
                              (str tp " [" hanzi "]"))})})
    {:valid? false
     :errors [{:type :parse-error
               :message (str "Cannot parse sigil: " sigil-str)}]
     :input sigil-str}))

(defn validate-tokipona-pair
  "Validate a tokipona/tokipona pair. Note: tokipona/tokipona is not a valid
   sigil format; use tokipona/hanzi by mapping the right-hand word via tokizh.org."
  [sigil-str]
  (if-let [[tp1 tp2] (parse-sigil-pair sigil-str)]
    (let [entry1 (get @tokipona->entry tp1)
          entry2 (get @tokipona->entry tp2)
          errors (cond-> []
                   (nil? entry1)
                   (conj {:type :unknown-tokipona
                          :tokipona tp1
                          :message (str "Unknown toki pona word: " tp1)})

                   (nil? entry2)
                   (conj {:type :unknown-tokipona
                          :tokipona tp2
                          :message (str "Unknown toki pona word: " tp2)}))]
      {:valid? (empty? errors)
       :errors errors
       :input sigil-str
       :decoded (when (and entry1 entry2)
                  {:word1 tp1
                   :word2 tp2
                   :emoji1 (:emoji entry1)
                   :emoji2 (:emoji entry2)
                   :hanzi1 (:hanzi entry1)
                   :hanzi2 (:hanzi entry2)
                   :reading (str tp1 " " tp2)
                   :emoji-sigil (str (:emoji entry1) "/" (:hanzi entry2))})})
    {:valid? false
     :errors [{:type :parse-error
               :message (str "Cannot parse sigil: " sigil-str)}]
     :input sigil-str}))

(defn validate-sigil
  "Validate a sigil string, auto-detecting emoji vs tokipona format.
   Returns {:valid? true/false :errors [...] :canonical {...} :format :emoji|:tokipona|:tokipona-pair}"
  [sigil-str]
  (if-let [[left _] (parse-sigil-pair sigil-str)]
    ;; Heuristic: if left part contains ASCII letters, it's tokipona
    (if (re-find #"[a-z]" left)
      (let [[tp1 tp2] (parse-sigil-pair sigil-str)]
        (if (re-find #"[a-z]" tp2)
          (let [entry1 (get @tokipona->entry tp1)
                entry2 (get @tokipona->entry tp2)
                errors (cond-> []
                         (nil? entry1)
                         (conj {:type :unknown-tokipona
                                :tokipona tp1
                                :message (str "Unknown toki pona word: " tp1)})

                         (nil? entry2)
                         (conj {:type :unknown-tokipona
                                :tokipona tp2
                                :message (str "Unknown toki pona word: " tp2)}))
                suggested (when (and entry1 entry2)
                            (str tp1 "/" (:hanzi entry2)))]
            {:valid? false
             :format :tokipona-pair
             :input sigil-str
             :suggested suggested
             :errors (conj errors
                           {:type :tokipona-pair
                            :message "tokipona/tokipona is invalid; use tokipona/hanzi"
                            :suggested suggested})})
          (assoc (validate-tokipona-sigil sigil-str) :format :tokipona)))
      (assoc (validate-emoji-sigil sigil-str) :format :emoji))
    {:valid? false
     :errors [{:type :parse-error
               :message (str "Cannot parse sigil: " sigil-str)}]
     :input sigil-str}))

(defn validate-sigils
  "Validate multiple sigils. Returns {:valid? bool :results [...]}"
  [sigil-strs]
  (let [results (mapv validate-sigil sigil-strs)
        all-valid? (every? :valid? results)]
    {:valid? all-valid?
     :results results
     :invalid-count (count (remove :valid? results))}))

;; --- Stamping (chop) functions ---

(defn stamp
  "Stamp content with validation result.
   Returns content with :chop/status :valid|:invalid and :chop/timestamp."
  [content sigil-strs]
  (let [validation (validate-sigils sigil-strs)
        now (java.time.Instant/now)]
    (assoc content
           :chop/status (if (:valid? validation) :valid :invalid)
           :chop/timestamp (str now)
           :chop/validation validation)))

(defn stamped?
  "Check if content has been stamped."
  [content]
  (contains? content :chop/status))

(defn valid-stamp?
  "Check if content has a valid stamp."
  [content]
  (= :valid (:chop/status content)))

;; --- Lookup helpers ---

(defn emoji->tokipona
  "Get toki pona word for an emoji."
  [emoji]
  (:tokipona (get @emoji->entry emoji)))

(defn tokipona->emoji
  "Get emoji for a toki pona word."
  [tp]
  (:emoji (get @tokipona->entry tp)))

(defn emoji->hanzi
  "Get hanzi for an emoji."
  [emoji]
  (:hanzi (get @emoji->entry emoji)))

(defn tokipona->hanzi
  "Get hanzi for a toki pona word."
  [tp]
  (:hanzi (get @tokipona->entry tp)))

;; --- Bridge-derived sigil assignments ---

(def ^:private bridge-assignments-path "sigils/bridge-assignments.edn")
(def ^:private bridge-cache (atom nil))

(defn- load-bridge-assignments []
  "Load bridge-derived sigil assignments from resources."
  (let [resource (io/resource bridge-assignments-path)]
    (when resource
      (let [data (edn/read-string (slurp resource))]
        {:meta (:meta data)
         :by-id (into {} (map (juxt :pattern-id identity)
                              (:assignments data)))}))))

(defn bridge-assignments
  "Return the bridge-derived sigil assignments. Cached after first load."
  []
  (or @bridge-cache
      (let [data (load-bridge-assignments)]
        (reset! bridge-cache data)
        data)))

(defn reload-bridge!
  "Force reload of bridge assignments."
  []
  (reset! bridge-cache nil)
  (bridge-assignments))

(defn assign-sigil
  "Look up the bridge-derived sigil for a pattern-id.
   Returns {:pattern-id :bits :hanzi :sigil :emoji :tokipona :confidence}
   or nil if not found.

   The sigil is grounded: pattern → MiniLM embedding → PCA+Ridge → 8-bit →
   truth-table-8 hanzi → tokizh emoji (where available)."
  [pattern-id]
  (get (:by-id (bridge-assignments)) pattern-id))

(defn assign-sigil-str
  "Return just the sigil string for a pattern-id (e.g., '🎎/公' or '允').
   Returns nil if pattern not found."
  [pattern-id]
  (:sigil (assign-sigil pattern-id)))

(defn bits->sigil
  "Given an 8-bit string, return the corresponding sigil.
   Uses truth-table-8 for hanzi and tokizh for optional emoji."
  [bits]
  (let [tt-entry (first (filter #(= bits (:binary %)) (hanzi-256)))
        hanzi (:hanzi tt-entry)
        tok (when hanzi (get @hanzi->tokizh hanzi))]
    (when hanzi
      (cond-> {:bits bits
               :hanzi hanzi
               :sigil hanzi}
        tok (assoc :emoji (:emoji tok)
                   :tokipona (:tokipona tok)
                   :sigil (str (:emoji tok) "/" hanzi))))))

;; --- Reporting ---

(defn list-canonical
  "List all canonical sigils as a sequence of [emoji tokipona hanzi] tuples."
  []
  (map (juxt :emoji :tokipona :hanzi) (canonical-sigils)))

(defn print-validation-report
  "Print a human-readable validation report."
  [validation]
  (println "Sigil Validation Report")
  (println "=======================")
  (println (str "Status: " (if (:valid? validation) "✓ VALID" "✗ INVALID")))
  (println (str "Checked: " (count (:results validation)) " sigils"))
  (println (str "Invalid: " (:invalid-count validation)))
  (println)
  (doseq [result (:results validation)]
    (if (:valid? result)
      (println (str "  ✓ " (:input result)))
      (do
        (println (str "  ✗ " (:input result)))
        (doseq [err (:errors result)]
          (println (str "    → " (:message err))))))))

(comment
  ;; Usage examples:

  ;; Load and inspect canonical sigils
  (count (canonical-sigils))                    ; => 124 entries
  (first (canonical-sigils))

  ;; Validate individual sigils (combinatorial: any emoji + any hanzi)
  (validate-sigil "🐜/予")      ; lili + e = "small giving" ✓
  (validate-sigil "🎑/了")      ; alasa + ? (了 not in set) - may fail
  (validate-sigil "🐜/无")      ; lili + ala = "small nothing" ✓

  ;; Decode a sigil to its two-word reading
  (:decoded (validate-sigil "🐜/支"))
  ;; => {:word1 "lili", :word2 "pana", :reading "lili pana"}

  ;; Validate tokipona/tokipona pairs
  (validate-tokipona-pair "lili/pana")
  ;; => {:decoded {:word1 "lili", :word2 "pana", :emoji-sigil "🐜/支"}}

  ;; Validate multiple
  (validate-sigils ["🐜/予" "🎑/勺" "pana/支"])

  ;; Stamp content
  (stamp {:pattern-id "test"} ["🐜/予" "🐺/内"])

  ;; Lookups
  (emoji->tokipona "🐜")        ; => "lili"
  (tokipona->emoji "lili")      ; => "🐜"
  (hanzi->tokipona "支")        ; => "pana"
  (emoji->hanzi "🐜")           ; => "与" (the hanzi assigned to lili)

  ;; Report
  (print-validation-report
   (validate-sigils ["🐜/予" "🎑/勺" "pana/支"]))

  ;; Bridge-derived assignment (grounded sigils)
  (assign-sigil "ants/cargo-return-discipline")
  ;; => {:pattern-id "ants/cargo-return-discipline" :bits "10101000"
  ;;     :hanzi "允" :sigil "允" :confidence 0.66}
  ;; (no emoji because 允 isn't in tokizh)

  (assign-sigil "agent/scope-before-action")
  ;; => {:pattern-id "agent/scope-before-action" :bits "10001000"
  ;;     :hanzi "公" :emoji "🎎" :tokipona "kulupu" :sigil "🎎/公"
  ;;     :confidence 0.68}

  ;; Direct 8-bit → sigil lookup
  (bits->sigil "10001000")  ;; => {:bits "10001000" :hanzi "公" :emoji "🎎" ...}
  (bits->sigil "10101000")  ;; => {:bits "10101000" :hanzi "允" :sigil "允"}

  )
