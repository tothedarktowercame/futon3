(ns scripts.cyoa-room
  "Run a single devmap room as a fixed interview + relay payload generator."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import (java.time LocalDate LocalDateTime)
           (java.time.format DateTimeFormatter)))

(def default-devmap "holes/futon3.devmap")
(def default-output-dir "holes/logs/cyoa")
(def default-relay-root "../futon5")
(def default-prompt "resources/prompts/evidence_engineer.prompt")
(def default-critic-prompt "resources/prompts/pattern_critic.prompt")
(def default-skip-rules "resources/cyoa/skip_rules.edn")
(def tatami-in-marker "FROM-TATAMI-EDN")
(def tatami-out-marker "FROM-CHATGPT-EDN")
(def stanza-delimiter "\n\n---SYSTEM-STANZA---\n\n")

(def timestamp-format (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))
(def iso-date-format DateTimeFormatter/ISO_LOCAL_DATE)

(defn now-stamp []
  (.format timestamp-format (LocalDateTime/now)))

(def date-pattern
  #"(?<!\d)\d{4}-\d{2}-\d{2}(?!\d)")

(defn extract-date [s]
  (when-let [match (re-find date-pattern (or s ""))]
    match))

(defn future-date-warning [timebox]
  (when-let [date-str (extract-date timebox)]
    (let [date (LocalDate/parse date-str iso-date-format)
          today (LocalDate/now)]
      (when-not (.isAfter date today)
        (str "Timebox date is not in the future: " date-str)))))

(defn starts-block? [line]
  (str/starts-with? line "! instantiated-by:"))

(defn block-header [line]
  (str/trim (subs line (count "! instantiated-by:"))))

(defn parse-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [remaining lines
           blocks []
           current nil]
      (if (empty? remaining)
        (cond-> blocks
          current (conj (update current :lines vec)))
        (let [line (first remaining)
              rest-lines (rest remaining)]
          (if (starts-block? line)
            (recur rest-lines
                   (cond-> blocks
                     current (conj (update current :lines vec)))
                   {:lines [line]
                    :header (block-header line)})
            (recur rest-lines
                   blocks
                   (when current
                     (update current :lines conj line)))))))))

(defn extract-then
  [block]
  (let [line (some #(when (re-find #"^\s*\+ then:" %) %) (str/split-lines block))]
    (when line
      (-> line
          (str/replace #"^\s*\+ then:\s*" "")
          str/trim))))

(defn evidence-count
  [then-text]
  (let [text (str/lower-case (or then-text ""))
        separators (+ (count (re-seq #"," text))
                      (count (re-seq #"\band\b" text)))]
    (if (>= separators 2) 3 2)))

(defn build-scaffold
  [{:keys [evidence-types timebox falsifier then]}]
  (let [expected-timebox (extract-date timebox)
        item-count (evidence-count then)
        type (when (= 1 (count evidence-types))
               (keyword (first evidence-types)))]
    {:next-evidence
     (vec
      (for [idx (range 1 (inc item-count))]
        {:type type
         :id (str "<fill-id-" idx ">")
         :description (str "<fill-description-" idx ">")
         :acceptance-criteria [(str "<fill-criterion-" idx "-1>")
                               (or falsifier "")]
         :suggested-location (str "<fill-path-" idx ">")
         :timebox expected-timebox}))}))

(defn materialize-blocks [blocks]
  (map-indexed
   (fn [idx {:keys [lines header]}]
     {:index idx
      :header header
      :block (str/join "\n" lines)})
   blocks))

(defn list-blocks [blocks]
  (doseq [{:keys [index header]} blocks]
    (println (format "%02d" index) "-" header)))

(defn prompt-line
  [& args]
  (apply println args)
  (flush))

(defn prompt-inline
  [& args]
  (apply print args)
  (flush))

(defn parse-evidence-types [raw]
  (->> (str/split (or raw "") #"[, ]+")
       (map str/trim)
       (remove str/blank?)
       vec))

(def questions
  [{:key :smallest-proof
    :prompt "Smallest observable proof of THEN?"}
   {:key :evidence-types
    :prompt "Acceptable evidence types (ERT/Golden/Transcript/Spec/CI, comma-separated)?"}
   {:key :falsifier
    :prompt "One falsifier you would accept?"}
   {:key :timebox
    :prompt "Tight timebox for next-evidence?"}])

(defn ask-questions []
  (reduce (fn [acc {:keys [key prompt]}]
            (prompt-line prompt)
            (prompt-inline "> ")
            (let [answer (read-line)]
              (cond
                (= key :evidence-types)
                (assoc acc key (parse-evidence-types answer))

                :else
                (assoc acc key (or answer "")))))
          {}
          questions))

(defn ensure-parent [path]
  (io/make-parents (io/file path)))

(defn abs-path [path]
  (.getAbsolutePath (io/file path)))

(defn write-edn [path data]
  (ensure-parent path)
  (with-open [writer (io/writer path)]
    (binding [*out* writer]
      (pprint/pprint data))))

(defn slurp-when [path]
  (when path
    (let [file (io/file path)]
      (when (.exists file)
        (slurp file)))))

(defn split-prompts [raw]
  (->> (str/split (or raw "") #",")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn latest-input-path
  [dir]
  (when (and dir (.exists (io/file dir)))
    (some->> (file-seq (io/file dir))
             (filter #(and (.isFile %)
                           (str/ends-with? (.getName %) "-input.edn")))
             (sort-by #(.lastModified %) >)
             first
             .getAbsolutePath)))

(defn load-skip-rules
  [path]
  (when-let [raw (slurp-when path)]
    (edn/read-string raw)))

(defn block-has-next-evidence?
  [block]
  (boolean (some #(re-find #"^\s*\+ next-evidence:" %) (str/split-lines block))))

(defn block-has-critic?
  [block]
  (boolean (some #(re-find #"^\s*\+ pattern-critic:" %) (str/split-lines block))))

(defn should-skip-next-evidence?
  [skip-rules block]
  (let [rule (get skip-rules :next-evidence)]
    (and rule
         (= :present (:skip-when rule))
         (block-has-next-evidence? block))))

(defn should-skip-critic?
  [skip-rules block]
  (let [rule (get skip-rules :pattern-critic)]
    (and rule
         (= :present (:skip-when rule))
         (block-has-critic? block))))

(defn format-edn-block
  [marker payload]
  (str "---" marker "---\n"
       (pr-str payload) "\n"
       "---END-" marker "---"))

(defn build-message
  [payload]
  (str "Here is the FROM-TATAMI-EDN block for this turn. Use it directly.\n"
       (format-edn-block tatami-in-marker payload)))

(defn extract-edn-block
  [text marker]
  (when (and text marker)
    (let [start-marker (str "---" marker "---")
          end-marker (str "---END-" marker "---")
          start (.indexOf text start-marker)]
      (when (>= start 0)
        (let [end (.indexOf text end-marker (+ start (count start-marker)))]
          (when (>= end 0)
            (subs text (+ start (count start-marker)) end)))))))

(defn relay-path [relay-root prompt]
  (.getPath (io/file relay-root prompt)))

(defn run-relay!
  [{:keys [relay-root prompt input-path input-message model dry-run]}]
  (let [prompt-path (relay-path relay-root prompt)
        actual-input (or input-message input-path)
        args (cond-> ["clj" "-M" "-m" "futon5.llm.relay"
                      "--prompt" prompt-path
                      "--input" actual-input]
               model (conj "--model" model)
               dry-run (conj "--dry-run"))]
    (merge {:prompt-path prompt-path}
           (apply shell/sh (concat args [:dir relay-root])))))

(defn parse-relay-content
  [relay-out]
  (try
    (let [data (json/parse-string relay-out)
          content (get-in data ["choices" 0 "message" "content"])
          block (or (some-> (extract-edn-block content tatami-out-marker) str/trim)
                    (some-> content str/trim))
          edn-value (when (and block (not (str/blank? block)))
                      (edn/read-string block))]
      {:raw data
       :content content
       :edn edn-value})
    (catch Exception _
      {:raw nil
       :content nil
       :edn nil})))

(defn occurrences [haystack needle]
  (if (str/blank? needle)
    0
    (count (re-seq (re-pattern (java.util.regex.Pattern/quote needle)) haystack))))

(defn apply-minimal-edits
  [block edits]
  (reduce (fn [{:keys [text applied warnings]} {:keys [find replace]}]
            (cond
              (or (str/blank? find) (nil? replace))
              {:text text
               :applied applied
               :warnings (conj warnings "Edit missing :find or :replace")}

              (not= 1 (occurrences text find))
              {:text text
               :applied applied
               :warnings (conj warnings (str "Edit not applied; expected single occurrence: " find))}

              :else
              {:text (str/replace text find replace)
               :applied (conj applied {:find find :replace replace})
               :warnings warnings}))
          {:text block :applied [] :warnings []}
          (or edits [])))

(defn replace-block-once
  [full-text old-block new-block]
  (let [count (occurrences full-text old-block)]
    (cond
      (zero? count) {:text full-text :replaced? false :error "Block not found in devmap"}
      (> count 1) {:text full-text :replaced? false :error "Block appears multiple times in devmap"}
      :else {:text (str/replace full-text old-block new-block)
             :replaced? true})))

(defn normalize-question [q]
  (-> (or q "")
      str/trim
      (str/replace #"\s+" " ")))

(defn append-questions
  [block questions]
  (let [qs (->> questions
                (map normalize-question)
                (remove str/blank?)
                distinct
                vec)]
    (if (empty? qs)
      {:text block :applied []}
      (let [existing (set (filter #(str/starts-with? (str/trim %) "@question:")
                                  (str/split-lines block)))
            new-lines (->> qs
                           (map #(str "  @question: " %))
                           (remove #(contains? existing (str/trim %)))
                           vec)]
        {:text (if (seq new-lines)
                 (str block "\n" (str/join "\n" new-lines))
                 block)
         :applied new-lines}))))

(defn format-next-evidence-item
  [item]
  (let [{:keys [type id description acceptance-criteria suggested-location]} item
        criteria (or acceptance-criteria [])]
    (str "    - {:type " type
         " :id \"" id "\""
         " :description \"" description "\""
         " :acceptance-criteria " (pr-str (vec criteria))
         " :suggested-location \"" suggested-location "\"}")))

(defn append-next-evidence
  [block next-evidence]
  (if (seq next-evidence)
    (str block
         "\n"
         "  + next-evidence:\n"
         (str/join "\n" (map format-next-evidence-item next-evidence)))
    block))

(def allowed-location-prefixes
  ["docs/" "resources/" "src/" "test/" "scripts/"])

(defn normalize-type [t]
  (cond
    (keyword? t) (-> (name t) str/lower-case)
    (string? t) (-> t str/lower-case)
    :else ""))

(defn words-from [s]
  (->> (str/split (str/lower-case (or s "")) #"[^a-z0-9]+")
       (remove str/blank?)
       (filter #(>= (count %) 5))
       set))

(defn any-word-present? [text words]
  (let [haystack (str/lower-case (or text ""))]
    (some #(str/includes? haystack %) words)))

(defn evidence-errors
  [{:keys [items allowed-types expected-timebox falsifier]}]
  (let [allowed (set (map str/lower-case (or allowed-types [])))
        falsifier-words (words-from falsifier)
        falsifier-text (str/lower-case (or falsifier ""))
        count-errors (when (or (< (count items) 2) (> (count items) 3))
                       [(str "Expected 2-3 evidence items, got " (count items))])
        timebox-errors (when (str/blank? expected-timebox)
                         ["Interview timebox missing YYYY-MM-DD date"])
        single-type-required (when (= 1 (count allowed-types))
                               (first allowed-types))]
    (loop [remaining (or items [])
           errs (into (or count-errors []) timebox-errors)]
      (if (empty? remaining)
        errs
        (let [{:keys [type id description acceptance-criteria suggested-location timebox]} (first remaining)
              item-id (or id "<missing-id>")
              placeholders? (fn [s] (str/includes? (or s "") "<fill-"))
              item-errors
              (cond-> []
                (or (nil? type)
                    (not (contains? allowed (normalize-type type))))
                (conj (str "Invalid :type for " item-id ", allowed: " (pr-str allowed-types)))

                (and single-type-required
                     (not= (normalize-type type)
                           (str/lower-case single-type-required)))
                (conj (str "Expected :type " single-type-required " for " item-id))

                (or (str/blank? timebox)
                    (not= timebox expected-timebox))
                (conj (str "Missing or mismatched :timebox for " item-id))

                (or (str/blank? suggested-location)
                    (not (some #(str/starts-with? suggested-location %) allowed-location-prefixes)))
                (conj (str "Suggested location must be inside repo for " item-id))

                (or (placeholders? id)
                    (placeholders? description)
                    (some placeholders? (or acceptance-criteria []))
                    (placeholders? suggested-location))
                (conj (str "Placeholders not filled for " item-id))

                (and (seq falsifier-words)
                     (not (some #(any-word-present? % falsifier-words)
                                (or acceptance-criteria []))))
                (conj (str "Acceptance criteria missing falsifier signal for " item-id))

                (and (not (str/blank? falsifier-text))
                     (not (some #(str/includes? (str/lower-case (or % "")) falsifier-text)
                                (or acceptance-criteria []))))
                (conj (str "Acceptance criteria missing exact falsifier phrase for " item-id)))]
          (recur (rest remaining) (into errs item-errors)))))))

(defn parse-args [args]
  (loop [opts {:devmap default-devmap
               :output-dir default-output-dir
               :relay-root default-relay-root
               :prompt default-prompt
               :critic-prompt default-critic-prompt
               :skip-rules default-skip-rules}
         args args]
    (if-let [arg (first args)]
      (case arg
        "--devmap" (recur (assoc opts :devmap (second args)) (nnext args))
        "--output-dir" (recur (assoc opts :output-dir (second args)) (nnext args))
        "--relay-root" (recur (assoc opts :relay-root (second args)) (nnext args))
        "--prompt" (recur (assoc opts :prompt (second args)) (nnext args))
        "--critic-prompt" (recur (assoc opts :critic-prompt (second args)) (nnext args))
        "--model" (recur (assoc opts :model (second args)) (nnext args))
        "--skip-rules" (recur (assoc opts :skip-rules (second args)) (nnext args))
        "--reuse-input" (recur (assoc opts :reuse-input (second args)) (nnext args))
        "--reuse-latest" (recur (assoc opts :reuse-latest true) (next args))
        "--index" (recur (assoc opts :index (Integer/parseInt (second args))) (nnext args))
        "--list" (recur (assoc opts :list true) (next args))
        "--relay" (recur (assoc opts :relay true) (next args))
        "--dry-run" (recur (assoc opts :dry-run true) (next args))
        "--apply-critic" (recur (assoc opts :apply-critic true) (next args))
        "--help" (recur (assoc opts :help true) (next args))
        (throw (ex-info (str "Unknown argument: " arg) {:arg arg})))
      opts)))

(defn usage []
  (println "Usage: clj -M -m scripts.cyoa-room --list|--index N [options]")
  (println "Options:")
  (println "  --devmap PATH        Devmap file (default holes/futon3.devmap)")
  (println "  --output-dir PATH    Log/output directory (default holes/logs/cyoa)")
  (println "  --relay-root PATH    Futon5 repo root (default ../futon5)")
  (println "  --prompt PATH        Prompt path relative to relay root")
  (println "  --critic-prompt PATH Critic prompt path relative to relay root")
  (println "  --model MODEL        Override model")
  (println "  --skip-rules PATH    Skip rules EDN (default resources/cyoa/skip_rules.edn)")
  (println "  --apply-critic       Apply critic questions/edits into the devmap")
  (println "  --reuse-input PATH   Reuse a prior room input EDN instead of re-answering questions")
  (println "  --reuse-latest       Reuse the most recent room input EDN from the output dir")
  (println "  --relay              Call futon5.llm.relay after interview")
  (println "  --dry-run            Use --dry-run with relay (no network)")
  (println "  --list               List available pattern blocks")
  (println "  --index N            Select block by index")
  (println "  --help               Show help"))

(defn -main [& args]
  (let [{:keys [devmap output-dir relay-root prompt critic-prompt model index list relay dry-run help skip-rules apply-critic reuse-input reuse-latest]} (parse-args args)]
    (when help
      (usage)
      (System/exit 0))
    (let [file (io/file devmap)]
      (when-not (.exists file)
        (throw (ex-info (str "Devmap file not found: " devmap) {:devmap devmap})))
      (let [blocks (-> (slurp file) parse-blocks materialize-blocks)]
        (when list
          (list-blocks blocks)
          (System/exit 0))
        (when (nil? index)
          (prompt-line "Missing --index. Use --list to see available blocks.")
          (usage)
          (System/exit 1))
        (let [selection (nth blocks index nil)]
          (when-not selection
            (throw (ex-info (str "No block at index " index) {:index index})))
          (let [skip-rules (load-skip-rules skip-rules)
                skip-next? (should-skip-next-evidence? skip-rules (:block selection))
                skip-critic? (should-skip-critic? skip-rules (:block selection))]
            (prompt-line "Selected:" (:header selection))
            (when skip-next?
              (prompt-line "Skip: next-evidence already present per skip rules."))
            (when skip-critic?
              (prompt-line "Skip: pattern critic already present per skip rules."))
            (let [reuse-path (or reuse-input
                                 (when reuse-latest
                                   (latest-input-path output-dir)))
                  prior-input (when reuse-path
                                (try
                                  (edn/read-string (slurp reuse-path))
                                  (catch Exception _ nil)))
                  answers (or (:interview prior-input) (ask-questions))
                  warning (future-date-warning (:timebox answers))
                  timebox-date (extract-date (:timebox answers))
                  stamp (now-stamp)
                  base (str "room-" stamp)
                  input-path (abs-path (io/file output-dir (str base "-input.edn")))
                  message-path (abs-path (io/file output-dir (str base "-message.txt")))
                  output-path (abs-path (io/file output-dir (str base "-evidence.edn")))
                  relay-json-path (abs-path (io/file output-dir (str base "-relay.json")))
                  patch-path (abs-path (io/file output-dir (str base "-patched.devmap")))
                  prompt-path (abs-path (io/file output-dir (str base "-prompt.txt")))
                  critic-message-path (abs-path (io/file output-dir (str base "-critic-message.txt")))
                  critic-output-path (abs-path (io/file output-dir (str base "-critic.edn")))
                  critic-relay-path (abs-path (io/file output-dir (str base "-critic-relay.json")))
                  critic-patch-path (abs-path (io/file output-dir (str base "-critic-patched.devmap")))
                  critic-prompt-path (abs-path (io/file output-dir (str base "-critic-prompt.txt")))
                  log-path (abs-path (io/file output-dir (str base "-log.edn")))
                  input {:pattern {:block (:block selection)
                                   :header (:header selection)
                                   :index (:index selection)
                                   :devmap devmap
                                   :then (extract-then (:block selection))}
                         :interview answers
                         :scaffold (build-scaffold (assoc answers :then (extract-then (:block selection))))}]
              (when reuse-path
                (prompt-line "Reused interview answers from" reuse-path))
              (when warning
                (prompt-line "Warning:" warning))
              (write-edn input-path input)
              (spit message-path (build-message input))
              (let [prompt-files (map #(relay-path relay-root %) (split-prompts prompt))
                    prompt-body (->> prompt-files
                                     (map slurp-when)
                                     (remove nil?)
                                     (str/join stanza-delimiter))]
                (when (seq prompt-body)
                  (spit prompt-path prompt-body)))
              (prompt-line "Wrote interview input to" input-path)
              (let [relay-result (when (and relay (not skip-next?))
                                   (run-relay! {:relay-root relay-root
                                                :prompt prompt
                                                :input-path input-path
                                                :input-message message-path
                                                :model model
                                                :dry-run dry-run}))
                    parsed (when relay-result
                             (parse-relay-content (:out relay-result)))
                    edn (:edn parsed)
                    next-evidence (:next-evidence edn)
                    allowed-types (:evidence-types answers)
                    validation-errors (when (seq next-evidence)
                                        (evidence-errors {:items next-evidence
                                                          :allowed-types allowed-types
                                                          :expected-timebox timebox-date
                                                          :falsifier (:falsifier answers)}))
                    patched-block (append-next-evidence (:block selection) next-evidence)
                    critic-input {:pattern {:block (:block selection)
                                            :header (:header selection)}}
                    critic-message (build-message critic-input)
                    _ (spit critic-message-path critic-message)
                    critic-prompt-files (map #(relay-path relay-root %) (split-prompts critic-prompt))
                    critic-prompt-body (->> critic-prompt-files
                                            (map slurp-when)
                                            (remove nil?)
                                            (str/join stanza-delimiter))
                    _ (when (seq critic-prompt-body)
                        (spit critic-prompt-path critic-prompt-body))
                    critic-result (when (and relay (not skip-critic?))
                                    (run-relay! {:relay-root relay-root
                                                 :prompt critic-prompt
                                                 :input-path critic-message-path
                                                 :input-message critic-message-path
                                                 :model model
                                                 :dry-run dry-run}))
                    _ (when relay-result
                        (spit relay-json-path (:out relay-result)))
                    _ (when critic-result
                        (spit critic-relay-path (:out critic-result)))
                    critic-parsed (when critic-result
                                    (parse-relay-content (:out critic-result)))
                    critic-edn (:edn critic-parsed)
                    minimal-edits (:minimal-edits critic-edn)
                    critic-edits (apply-minimal-edits (:block selection) minimal-edits)
                  critic-questions (append-questions (:text critic-edits) (:questions critic-edn))
                  devmap-text (slurp file)
                  critic-block (:text critic-questions)
                  devmap-update (when (and apply-critic
                                           (not= critic-block (:block selection)))
                                  (replace-block-once devmap-text (:block selection) critic-block))]
                (when relay-result
                  (when-let [content (:content parsed)]
                    (spit output-path content)
                    (prompt-line "Wrote relay output to" output-path)))
                (when (and relay-result (nil? edn))
                  (prompt-line "Validation errors:")
                  (prompt-line "- Missing FROM-CHATGPT-EDN block"))
                (when (seq validation-errors)
                  (prompt-line "Validation errors:")
                  (doseq [err validation-errors]
                    (prompt-line "- " err)))
                (when (and (seq next-evidence) (empty? validation-errors))
                  (spit patch-path patched-block)
                  (prompt-line "Wrote patched block to" patch-path)
                  (prompt-line "Patched block:\n" patched-block))
                (when-let [content (:content critic-parsed)]
                  (spit critic-output-path content))
                (when (or (seq (:applied critic-edits)) (seq (:applied critic-questions)))
                  (spit critic-patch-path (:text critic-questions)))
                (when (and apply-critic devmap-update)
                  (if (:replaced? devmap-update)
                    (do
                      (spit devmap (:text devmap-update))
                      (prompt-line "Applied critic updates to" devmap))
                    (prompt-line "Did not apply critic updates:" (:error devmap-update))))
                (write-edn log-path
                           {:timestamp stamp
                            :devmap devmap
                            :pattern {:index (:index selection)
                                      :header (:header selection)}
                            :warnings (when warning [warning])
                            :skip {:next-evidence (when skip-next? {:reason :present})
                                   :pattern-critic (when skip-critic? {:reason :present})}
                            :validation-errors (cond
                                                 (and relay-result (nil? edn)) ["Missing FROM-CHATGPT-EDN block"]
                                                 (seq validation-errors) (vec validation-errors))
                            :input input-path
                            :message message-path
                            :prompt prompt-path
                            :relay (when relay-result
                                     {:relay-root relay-root
                                      :prompt prompt
                                      :dry-run (boolean dry-run)
                                      :exit (:exit relay-result)
                                      :err (:err relay-result)
                                      :output output-path
                                      :raw relay-json-path
                                      :patched (when relay-result patch-path)})
                            :critic {:message critic-message-path
                                     :prompt critic-prompt-path
                                     :output critic-output-path
                                     :raw critic-relay-path
                                     :skipped (when skip-critic? {:reason :present})
                                     :applied (concat (:applied critic-edits) (:applied critic-questions))
                                     :warnings (concat (:warnings critic-edits) (:warnings critic-questions))
                                     :questions (:questions critic-edn)
                                     :applied-to-devmap (when apply-critic
                                                          (select-keys devmap-update [:replaced? :error]))}})
                (prompt-line "Wrote log to" log-path))))))
      (System/exit 0))))
