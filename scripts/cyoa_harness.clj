(ns scripts.cyoa-harness
  "Run a small model comparison harness for devmap CYOA rooms."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [scripts.cyoa-room :as room]))

(def default-devmap "holes/futon3.devmap")
(def default-output-dir "holes/logs/cyoa")
(def default-relay-root "../futon5")
(def default-prompt "resources/prompts/evidence_engineer.prompt")
(def default-critic-prompt "resources/prompts/pattern_critic.prompt")
(def default-skip-rules "resources/cyoa/skip_rules.edn")

(def default-models
  ["gpt-4o-mini"
   "gpt-4o"
   "gpt-4.1"
   "gpt-5.2-chat-latest"])

(def default-cases
  [{:name "prototype-0"
    :index 0
    :interview {:smallest-proof "Define the transport contract; create transcripts for check jobs; explain sandbox usage and how to run it from the Clojure runner."
                :evidence-types ["Spec"]
                :falsifier "Fails if there is no reliable A->B association between proposal and evidence collection; must be machine-checkable now and upgradeable for stronger logics later."
                :timebox "By 2026-01-05."}}
   {:name "prototype-1"
    :index 1
    :interview {:smallest-proof "We already imported patterns into Futon1, but we need a test that illustrates completeness of the store."
                :evidence-types ["Golden"]
                :falsifier "Fails if the canonical store only exists on disk and is not accessible to the stack."
                :timebox "By 2026-01-06."}}])

(defn now-stamp []
  (room/now-stamp))

(defn parse-models [raw]
  (->> (str/split (or raw "") #"[, ]+")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn parse-args [args]
  (loop [opts {:devmap default-devmap
               :output-dir default-output-dir
               :relay-root default-relay-root
               :prompt default-prompt
               :critic-prompt default-critic-prompt
               :skip-rules default-skip-rules
               :models default-models}
         args args]
    (if-let [arg (first args)]
      (case arg
        "--devmap" (recur (assoc opts :devmap (second args)) (nnext args))
        "--output-dir" (recur (assoc opts :output-dir (second args)) (nnext args))
        "--relay-root" (recur (assoc opts :relay-root (second args)) (nnext args))
        "--prompt" (recur (assoc opts :prompt (second args)) (nnext args))
        "--critic-prompt" (recur (assoc opts :critic-prompt (second args)) (nnext args))
        "--skip-rules" (recur (assoc opts :skip-rules (second args)) (nnext args))
        "--models" (recur (assoc opts :models (parse-models (second args))) (nnext args))
        "--cases" (recur (assoc opts :cases (second args)) (nnext args))
        "--dry-run" (recur (assoc opts :dry-run true) (next args))
        "--help" (recur (assoc opts :help true) (next args))
        (throw (ex-info (str "Unknown argument: " arg) {:arg arg})))
      opts)))

(defn usage []
  (println "Usage: clj -M -m scripts.cyoa-harness [options]")
  (println "Options:")
  (println "  --devmap PATH        Devmap file (default holes/futon3.devmap)")
  (println "  --output-dir PATH    Output directory (default holes/logs/cyoa)")
  (println "  --relay-root PATH    Futon5 repo root (default ../futon5)")
  (println "  --prompt PATH        Prompt path relative to relay root")
  (println "  --critic-prompt PATH Critic prompt path relative to relay root")
  (println "  --models LIST        Comma-separated model list")
  (println "  --cases PATH         EDN file with cases (vector of maps)")
  (println "  --skip-rules PATH    Skip rules EDN (default resources/cyoa/skip_rules.edn)")
  (println "  --dry-run            Call relay with --dry-run (no grading)")
  (println "  --help               Show help"))

(defn load-cases [path]
  (with-open [reader (io/reader path)]
    (edn/read reader)))

(defn ensure-parent [path]
  (io/make-parents (io/file path)))

(defn write-edn [path data]
  (ensure-parent path)
  (with-open [writer (io/writer path)]
    (binding [*out* writer]
      (pprint/pprint data))))

(defn abs-path [path]
  (.getAbsolutePath (io/file path)))

(def stanza-delimiter "\n\n---SYSTEM-STANZA---\n\n")

(defn split-prompts [raw]
  (->> (clojure.string/split (or raw "") #",")
       (map clojure.string/trim)
       (remove clojure.string/blank?)
       vec))

(defn log-line
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn case-input [selection devmap interview]
  (let [then-text (room/extract-then (:block selection))]
    {:pattern {:block (:block selection)
               :header (:header selection)
               :index (:index selection)
               :devmap devmap
               :then then-text}
     :interview interview
     :scaffold (room/build-scaffold (assoc interview :then then-text))}))

(defn grade-evidence [interview next-evidence]
  (let [expected-timebox (room/extract-date (:timebox interview))]
    (room/evidence-errors {:items next-evidence
                           :allowed-types (:evidence-types interview)
                           :expected-timebox expected-timebox
                           :falsifier (:falsifier interview)})))

(defn run-case
  [{:keys [case model selection devmap output-dir relay-root prompt critic-prompt dry-run skip-rules]}]
  (let [stamp (now-stamp)
        case-name (:name case)
        base (str case-name "-" model "-" stamp)
        input-path (abs-path (io/file output-dir (str base "-input.edn")))
        message-path (abs-path (io/file output-dir (str base "-message.txt")))
        prompt-path (abs-path (io/file output-dir (str base "-prompt.txt")))
        relay-path (abs-path (io/file output-dir (str base "-relay.json")))
        output-path (abs-path (io/file output-dir (str base "-evidence.edn")))
        log-path (abs-path (io/file output-dir (str base "-log.edn")))
        input (case-input selection devmap (:interview case))
        rules (room/load-skip-rules skip-rules)
        skip-next? (room/should-skip-next-evidence? rules (:block selection))
        skip-critic? (room/should-skip-critic? rules (:block selection))]
    (write-edn input-path input)
    (spit message-path (room/build-message input))
    (let [prompt-files (map #(room/relay-path relay-root %) (split-prompts prompt))
          prompt-body (->> prompt-files (map room/slurp-when) (remove nil?) (clojure.string/join stanza-delimiter))]
      (when (seq prompt-body)
        (spit prompt-path prompt-body)))
    (log-line "Running" case-name "model=" model "dry-run=" (boolean dry-run) "skip=" (boolean skip-next?) "critic-skip=" (boolean skip-critic?))
    (let [relay-result (when (and (not skip-next?) (not dry-run))
                         (room/run-relay! {:relay-root relay-root
                                           :prompt prompt
                                           :input-path input-path
                                           :input-message message-path
                                           :model model
                                           :dry-run dry-run}))
          raw-out (:out relay-result)]
      (when raw-out
        (spit relay-path raw-out))
      (if dry-run
        (let [log {:case case-name
                   :model model
                   :input input-path
                   :relay relay-path
                   :message message-path
                   :prompt prompt-path
                   :dry-run true
                   :skip (cond-> {}
                           skip-next? (assoc :next-evidence {:reason :present})
                           skip-critic? (assoc :pattern-critic {:reason :present}))
                   :status :skipped}]
          (write-edn log-path log)
          (log-line "Skipped (dry-run)" case-name "model=" model "log=" log-path)
          log)
        (let [{:keys [edn content]} (when raw-out (room/parse-relay-content raw-out))
              next-evidence (:next-evidence edn)
              errors (if skip-next?
                       []
                       (if (nil? edn)
                         ["Missing FROM-CHATGPT-EDN block"]
                         (grade-evidence (:interview case) (or next-evidence []))))
              critic-input {:pattern {:block (:block selection)
                                      :header (:header selection)}}
              critic-message (room/build-message critic-input)
              critic-message-path (abs-path (io/file output-dir (str base "-critic-message.txt")))
              critic-output-path (abs-path (io/file output-dir (str base "-critic.edn")))
              critic-relay-path (abs-path (io/file output-dir (str base "-critic-relay.json")))
              critic-prompt-path (abs-path (io/file output-dir (str base "-critic-prompt.txt")))
              _ (spit critic-message-path critic-message)
              critic-prompt-files (map #(room/relay-path relay-root %) (split-prompts critic-prompt))
              critic-prompt-body (->> critic-prompt-files (map room/slurp-when) (remove nil?) (clojure.string/join stanza-delimiter))
              _ (when (seq critic-prompt-body)
                  (spit critic-prompt-path critic-prompt-body))
              critic-result (when (and (not skip-critic?) (not dry-run))
                              (room/run-relay! {:relay-root relay-root
                                                :prompt critic-prompt
                                                :input-path critic-message-path
                                                :input-message critic-message-path
                                                :model model
                                                :dry-run dry-run}))
              _ (when critic-result
                  (spit critic-relay-path (:out critic-result)))
              critic-parsed (when critic-result
                              (room/parse-relay-content (:out critic-result)))
              critic-edn (:edn critic-parsed)
              minimal-edits (:minimal-edits critic-edn)
              critic-edits (room/apply-minimal-edits (:block selection) minimal-edits)
              critic-questions (room/append-questions (:text critic-edits) (:questions critic-edn))
              log {:case case-name
                   :model model
                   :input input-path
                   :relay relay-path
                   :message message-path
                   :prompt prompt-path
                   :output output-path
                   :exit (:exit relay-result)
                   :errors (vec errors)
                   :skip (when skip-next? {:next-evidence {:reason :present}})
                   :status (if (seq errors) :fail (if skip-next? :skipped :pass))
                   :critic {:message critic-message-path
                            :prompt critic-prompt-path
                            :output critic-output-path
                            :raw critic-relay-path
                            :skipped (when skip-critic? {:reason :present})
                            :applied (concat (:applied critic-edits) (:applied critic-questions))
                            :warnings (concat (:warnings critic-edits) (:warnings critic-questions))
                            :questions (:questions critic-edn)}}]
          (when content
            (spit output-path content))
          (when-let [ccontent (:content critic-parsed)]
            (spit critic-output-path ccontent))
          (write-edn log-path log)
          (log-line "Finished" case-name "model=" model "status=" (:status log) "log=" log-path)
          log)))))

(defn -main [& args]
  (let [{:keys [devmap output-dir relay-root prompt critic-prompt models cases dry-run help skip-rules]} (parse-args args)]
    (when help
      (usage)
      (System/exit 0))
    (let [file (io/file devmap)]
      (when-not (.exists file)
        (throw (ex-info (str "Devmap file not found: " devmap) {:devmap devmap})))
      (let [blocks (-> (slurp file) room/parse-blocks room/materialize-blocks)
            case-list (if cases (load-cases cases) default-cases)
            run-id (now-stamp)
            summary-path (str (io/file output-dir (str "harness-" run-id "-summary.edn")))
            results (for [case case-list
                          model models]
                      (let [selection (nth blocks (:index case) nil)]
                        (when-not selection
                          (throw (ex-info (str "No block at index " (:index case)) {:index (:index case)})))
                        (run-case {:case case
                                   :model model
                                   :selection selection
                                   :devmap devmap
                                   :output-dir output-dir
                                   :relay-root relay-root
                                   :prompt prompt
                                   :critic-prompt critic-prompt
                                   :skip-rules skip-rules
                                   :dry-run dry-run})))]
        (log-line "Harness run" run-id "cases=" (map :name case-list) "models=" models "dry-run=" (boolean dry-run))
        (write-edn summary-path {:run-id run-id
                                 :devmap devmap
                                 :models models
                                 :cases (map :name case-list)
                                 :results (vec results)})
        (log-line "Wrote summary to" summary-path)
        (doseq [{:keys [case model status errors]} results]
          (log-line case model status (when (seq errors) (str "errors=" (count errors)))))))))
