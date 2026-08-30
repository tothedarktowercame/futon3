#!/usr/bin/env bb
(ns checks.spider-runner
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [checks.library-graph-lint :as lint]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def root (str (fs/absolutize ".")))
(def library (str (fs/path root "library")))
(def baseline (str (fs/path library ".spider/baseline-edges.edn")))
(def linter (str (fs/path root "checks/library_graph_lint.clj")))
(def agency-send "/home/joe/code/futon3c/scripts/agency_send.py")
(def schema-path (str (fs/path library ".spider/attestation-schema.edn")))
(def edge-kinds #{:why :how :see-also})
(def proposal-kinds #{:retire :specialise :merge :split})

(defn now [] (.toString (java.time.Instant/now)))
(defn date [] (.toString (java.time.LocalDate/now java.time.ZoneOffset/UTC)))

(defn atomic-spit! [path value]
  (let [path (fs/path path)
        tmp (fs/path (str path ".tmp"))
        bytes (.getBytes (str value) "UTF-8")]
    (fs/create-dirs (fs/parent path))
    (with-open [out (java.io.FileOutputStream. (str tmp))]
      (.write out bytes)
      (.flush out)
      (.force (.getChannel out) true))
    (fs/move tmp path {:replace-existing true :atomic-move true})))

(defn write-edn! [path value]
  (atomic-spit! path (with-out-str (pprint/pprint value))))

(defn read-edn [path fallback]
  (if (fs/exists? path) (edn/read-string (slurp (str path))) fallback))

(defn parse-args [args]
  (when (odd? (count args)) (throw (ex-info "arguments must be --key value pairs" {})))
  (into {} (map (fn [[k v]] [(keyword (str/replace k #"^--" "")) v]) (partition 2 args))))

(defn section-files [section]
  (sort (fs/glob (fs/path library section) "*.flexiarg")))

(defn pattern-id [section file]
  (str section "/" (str/replace (fs/file-name file) #"\.flexiarg$" "")))

(defn state-paths [section]
  (let [section-dir (fs/path library section)
        run-dir (fs/path section-dir ".spider")]
    {:section-dir (str section-dir) :run-dir (str run-dir)
     :state (str (fs/path run-dir "state.edn"))
     :gates (str (fs/path run-dir "gates"))
     :receipts (str (fs/path run-dir "receipts"))
     :checkpoints (str (fs/path run-dir "checkpoints"))
     :absences (str (fs/path run-dir "absences.edn"))
     :attestations (str (fs/path section-dir "attestations.edn"))
     :proposals (str (fs/path section-dir "proposals.edn"))}))

(defn initial-state [section seat]
  {:schema 1 :section section :seat seat :phase :turn-ready :turn 0 :checkpoint 0
   :patterns (mapv #(pattern-id section %) (section-files section))
   :completed [] :current-pattern nil :failure-fingerprint nil
   :consecutive-same-failures 0 :last-green-gate nil :started-at (now)})

(defn receipt-path [paths turn suffix]
  (str (fs/path (:receipts paths) (format "turn-%03d-%s.edn" turn suffix))))

(defn gate-path [paths turn]
  (str (fs/path (:gates paths) (format "turn-%03d.edn" turn))))

(defn reconcile! [paths state]
  (if (= :turn-running (:phase state))
    (let [turn (:turn state)
          completed (receipt-path paths turn "completion")
          recovery (receipt-path paths turn "kill-recovery")]
      (if (fs/exists? completed)
        ;; A completion is data, not permission to guess that edits landed.
        (let [paused (assoc state :phase :paused :paused-reason :completion-without-gate)]
          (write-edn! (:state paths) paused) paused)
        (let [ready (assoc state :phase :turn-ready :recovered-at (now)
                           :recovery :interrupted-turn-no-completion)]
          (write-edn! recovery {:schema 1 :event :runner-killed-mid-turn
                                :at (now) :turn turn :pattern (:current-pattern state)
                                :disposition :retry-from-turn-ready})
          (write-edn! (:state paths) ready) ready)))
    state))

(defn run-linter! [section paths turn]
  (let [report (gate-path paths turn)
        result (process/shell {:continue true :out :string :err :string}
                              "bb" linter "--library" library "--section" section
                              "--baseline" baseline "--attestations" (:attestations paths)
                              "--report" report)]
    {:exit (:exit result) :report report
     :data (read-edn report {:checks [{:reason :missing-gate-report}]
                            :summary {:pass? false}})
     :stdout (:out result) :stderr (:err result)}))

(defn response-prompt [section pattern file paths state]
  (let [readme (first (filter fs/exists? [(fs/path (:section-dir paths) "README.md")
                                          (fs/path (:section-dir paths) "README-flexiarg.md")]))
        previous-failure (:failure-fingerprint state)
        checkpoint? (zero? (mod (inc (count (:completed state))) 20))]
    (str
     "You are the spider for library/" section ". Work on exactly one existing pattern. "
     "Never create or edit files; return data only. Return ONLY one EDN vector, no markdown.\n\n"
     "Each item is exactly one of:\n"
     "{:edge {:from \"" pattern "\" :to \"existing-dir/existing-name\" :kind :why|:how|:see-also} "
     ":cited \"verbatim sentence from the source pattern warranting this target\" "
     ":evidence [{:id \"real evidence id\" :via :tag|:text :query \"query actually run\" :excerpt \"verbatim evidence excerpt\"}] "
     ":rung 1|2 :read [\"pattern ids actually read\"]}\n"
     "{:proposal {:kind :retire|:specialise|:merge|:split :pattern \"" pattern "\" :reason \"...\" :evidence \"...\"}}\n"
     "{:absence {:pattern \"" pattern "\" :note \"queries tried and why no warranted edge was found\"}}\n"
     (when checkpoint?
       "{:checkpoint-restatement \"one paragraph stating what this section is about\"}\n")
     "\nRules: :from must equal the selected pattern. Targets must already exist as library/<target>.flexiarg. "
     "Do not repeat an existing directive. Prefer a small number of strong edges; [] is invalid, use an absence. "
     "Every edge needs a nonblank citation and real external evidence. Rung 1 means the pattern id was found as a pattern tag/result id; "
     "rung 2 is a bounded GET http://127.0.0.1:7073/api/alpha/evidence/text-search?q=<encoded title or conclusion keywords>&limit=5&hydrate=true. "
     "Never request the unfiltered evidence list. Only use IDs and excerpts returned by a query you actually ran. "
     "The runner verifies every :id by GET /api/alpha/evidence/<id> and requires the normalized :excerpt to occur verbatim in that record; "
     "pattern IDs are not evidence IDs, summaries and ellipses are rejected, and rung 1 also requires the source pattern id to occur in the record. "
     "If tag and text queries find nothing, return Absence naming both queries. @why means what this pattern stands on; "
     "@how means a pattern carrying it out; @see-also is non-dependency relevance.\n"
     (when previous-failure (str "Previous gate/output failure to correct: " (pr-str previous-failure) "\n"))
     "\nAttestation schema:\n" (slurp schema-path) "\n"
     "Section README:\n" (if readme (slurp (str readme)) "(No section README exists.)") "\n"
     "Section pattern ids:\n" (pr-str (:patterns state)) "\n"
     "\nSelected pattern text:\n" (slurp (str file)))))

(defn invoke-seat! [seat prompt]
  (let [result (process/shell {:continue true :out :string :err :string :in prompt}
                              "python3" agency-send "--from" "codex-20"
                              "--to" seat "--kind" "whistle")]
    (when-not (zero? (:exit result))
      (throw (ex-info "agency whistle failed" {:exit (:exit result) :stderr (:err result)})))
    (let [wire (json/parse-string (str/trim (:out result)) true)
          response (:response wire)]
      (when-not (string? response) (throw (ex-info "whistle response missing" {:wire wire})))
      {:wire wire :raw response :value (edn/read-string response)})))

(defn current-edge-keys []
  (let [scan (lint/scan-library library)]
    (set (map #(select-keys % [:from :to :kind]) (:edges scan)))))

(defn string-leaves [x]
  (cond (map? x) (mapcat string-leaves (concat (keys x) (vals x)))
        (sequential? x) (mapcat string-leaves x)
        (string? x) [x]
        :else []))

(defn normalized [s]
  (-> (str s) str/lower-case (str/replace #"\s+" " ") str/trim))

(defn evidence-valid? [pattern rung {:keys [id excerpt]}]
  (when (and (string? id) (re-matches #"e-[A-Za-z0-9-]+" id)
             (string? excerpt) (>= (count (normalized excerpt)) 20))
    (try
      (let [record (edn/read-string
                    (slurp (str "http://127.0.0.1:7073/api/alpha/evidence/" id)))
            haystack (normalized (str/join " " (string-leaves record)))
            needle (normalized excerpt)]
        (and (str/includes? haystack needle)
             (or (= rung 2) (str/includes? haystack (normalized pattern)))))
      (catch Exception _ false))))

(defn validate-output [value pattern checkpoint?]
  (when-not (vector? value) (throw (ex-info "seat output root must be a vector" {})))
  (let [edge-items (filter :edge value)
        proposal-items (filter :proposal value)
        absence-items (filter :absence value)
        checkpoint-items (filter :checkpoint-restatement value)
        existing (current-edge-keys)]
    (when-not (every? #(= 1 (count (filter some? [(:edge %) (:proposal %) (:absence %)
                                                  (:checkpoint-restatement %)]))) value)
      (throw (ex-info "each output item must have exactly one recognized key" {})))
    (when (and (empty? edge-items) (empty? proposal-items) (empty? absence-items))
      (throw (ex-info "seat returned neither edge, proposal, nor absence" {})))
    (doseq [{:keys [edge cited evidence rung read]} edge-items]
      (when-not (and (= pattern (:from edge)) (string? (:to edge))
                     (edge-kinds (:kind edge))
                     (fs/exists? (fs/path library (str (:to edge) ".flexiarg")))
                     (not (contains? existing edge))
                     (string? cited) (>= (count (normalized cited)) 20)
                     (str/includes?
                      (normalized (slurp (str (fs/path library (str pattern ".flexiarg")))))
                      (normalized cited))
                     (vector? evidence) (seq evidence)
                     (every? #(and (string? (:id %)) (#{:tag :text} (:via %))
                                   (not (str/blank? (str (:query %))))
                                   (not (str/blank? (str (:excerpt %))))
                                   (evidence-valid? pattern rung %)) evidence)
                     (#{1 2} rung) (vector? read) (seq read))
        (throw (ex-info "invalid or duplicate edge item" {:item edge}))))
    (doseq [{:keys [proposal]} proposal-items]
      (when-not (and (proposal-kinds (:kind proposal)) (= pattern (:pattern proposal))
                     (not (str/blank? (str (:reason proposal))))
                     (not (str/blank? (str (:evidence proposal)))))
        (throw (ex-info "invalid proposal" {:proposal proposal}))))
    (doseq [{:keys [absence]} absence-items]
      (when-not (and (= pattern (:pattern absence))
                     (not (str/blank? (str (:note absence)))))
        (throw (ex-info "invalid absence" {:absence absence}))))
    (when (and checkpoint? (not= 1 (count checkpoint-items)))
      (throw (ex-info "checkpoint turn requires one restatement" {})))
    {:edges edge-items :proposals (mapv :proposal proposal-items)
     :absences (mapv :absence absence-items)
     :restatement (:checkpoint-restatement (first checkpoint-items))}))

(defn insert-directives [text edges]
  (let [lines (vec (str/split-lines text))
        body-index (or (first (keep-indexed
                               (fn [i line]
                                 (when (and (not (str/blank? line))
                                            (not (str/starts-with? line "@"))
                                            (not (str/starts-with? (str/triml line) ";;"))) i))
                               lines)) (count lines))
        directives (mapv (fn [{:keys [edge]}]
                           (str "@" (name (get-in edge [:kind])) " " (get-in edge [:to]))) edges)]
    (str (str/join "\n" (concat (subvec lines 0 body-index) directives
                                  (subvec lines body-index))) "\n")))

(defn append-records! [path records]
  (let [prior (read-edn path [])]
    (write-edn! path (into (vec prior) records))))

(defn apply-output! [paths seat file parsed]
  (let [attestations (mapv (fn [{:keys [edge cited evidence rung read]}]
                             {:edge edge :by seat :at (date) :read read :cited cited
                              :evidence evidence :rung rung :state :proposed}) (:edges parsed))]
    (when (seq (:edges parsed))
      (atomic-spit! file (insert-directives (slurp (str file)) (:edges parsed)))
      (append-records! (:attestations paths) attestations))
    (when (seq (:proposals parsed)) (append-records! (:proposals paths) (:proposals parsed)))
    (when (seq (:absences parsed)) (append-records! (:absences paths) (:absences parsed)))
    {:attestations attestations :proposals (:proposals parsed) :absences (:absences parsed)}))

(defn checkpoint! [paths state restatement gate-summary]
  (let [n (inc (:checkpoint state))
        previous (when (> n 1) (read-edn (str (fs/path (:checkpoints paths)
                                                     (format "checkpoint-%03d.edn" (dec n)))) nil))
        receipt {:schema 1 :checkpoint n :at (now) :turn (:turn state)
                 :restatement restatement
                 :fraction-organised (get-in gate-summary [:section :fraction-organised])}]
    (write-edn! (str (fs/path (:checkpoints paths) (format "checkpoint-%03d.edn" n))) receipt)
    {:number n :unchanged? (= restatement (:restatement previous))}))

(defn process-turn! [section seat paths state]
  (let [pattern (first (remove (set (:completed state)) (:patterns state)))
        file (fs/path library (str pattern ".flexiarg"))
        turn (inc (:turn state))
        checkpoint? (zero? (mod (inc (count (:completed state))) 20))
        original {:file (slurp (str file))
                  :attestations (when (fs/exists? (:attestations paths)) (slurp (:attestations paths)))
                  :proposals (when (fs/exists? (:proposals paths)) (slurp (:proposals paths)))
                  :absences (when (fs/exists? (:absences paths)) (slurp (:absences paths)))}
        running (assoc state :phase :turn-running :turn turn :current-pattern pattern)]
    (write-edn! (:state paths) running)
    (write-edn! (receipt-path paths turn "intent")
                {:schema 1 :event :dispatch-intent :at (now) :turn turn
                 :pattern pattern :seat seat :transport :agency-whistle})
    (try
        (let [prompt (response-prompt section pattern file paths running)
            response (invoke-seat! seat prompt)
            _ (write-edn! (receipt-path paths turn "completion")
                          {:schema 1 :event :dispatch-completion :at (now) :turn turn
                           :pattern pattern :seat seat :wire (:wire response)
                           :response (:raw response)})
            parsed (validate-output (:value response) pattern checkpoint?)
            applied (apply-output! paths seat file parsed)
            gating (assoc running :phase :gating)]
        (write-edn! (:state paths) gating)
        (let [gate (run-linter! section paths turn)]
          (if-not (zero? (:exit gate))
            (do (atomic-spit! file (:file original))
              (doseq [[path content] [[(:attestations paths) (:attestations original)]
                                      [(:proposals paths) (:proposals original)]
                                      [(:absences paths) (:absences original)]]]
                (if (some? content) (atomic-spit! path content) (when (fs/exists? path) (fs/delete path))))
              (let [fingerprint (mapv :reason (get-in gate [:data :checks]))
                    same? (= fingerprint (:failure-fingerprint state))
                    failures (if same? (inc (:consecutive-same-failures state)) 1)
                    next (assoc gating :phase (if (>= failures 2) :paused :turn-ready)
                                :failure-fingerprint fingerprint
                                :consecutive-same-failures failures
                                :paused-reason (when (>= failures 2) :repeated-gate-failure))]
                (write-edn! (:state paths) next) next))
            (let [checkpoint (when checkpoint?
                               (write-edn! (:state paths) (assoc gating :phase :checkpoint-ready))
                               (checkpoint! paths gating (:restatement parsed)
                                            (:summary (:data gate))))
                  review-state (when checkpoint?
                                 (assoc gating :phase :review-pending
                                        :checkpoint (:number checkpoint)))
                  _ (when review-state (write-edn! (:state paths) review-state))
                  next (assoc (or review-state gating)
                              :phase (if (:unchanged? checkpoint) :paused :turn-ready)
                              :completed (conj (:completed state) pattern)
                              :current-pattern nil :failure-fingerprint nil
                              :consecutive-same-failures 0
                              :last-green-gate (:report gate)
                              :paused-reason (when (:unchanged? checkpoint)
                                               :unchanged-checkpoint-restatement))]
              (write-edn! (:state paths) next)
              (write-edn! (receipt-path paths turn "applied")
                          {:schema 1 :event :turn-applied :at (now) :turn turn
                           :pattern pattern :gate (:report gate) :changes applied})
              next))))
      (catch Exception e
        (let [failure {:type (str (type e)) :message (.getMessage e) :data (ex-data e)}
              failures (if (= pattern (:current-pattern state))
                         (inc (:consecutive-same-failures state)) 1)
              ;; No writes occur before validation; malformed output is safe to retry.
              next (assoc running :phase (if (>= failures 2) :turn-ready :turn-ready)
                          :failure-fingerprint failure :consecutive-same-failures failures)]
          (write-edn! (gate-path paths turn)
                      {:checks [{:file (str file) :line nil :edge nil
                                 :reason :seat-output-invalid :detail failure}]
                       :summary {:pass? false :failures 1}})
          (write-edn! (:state paths) next)
          next)))))

(defn checkpoint-only! [section seat paths state]
  (let [prompt (str "Checkpoint only for completed patterns in library/" section
                    ". Return ONLY one EDN map {:checkpoint-restatement \"one paragraph\"}. "
                    "Restate the section's common discipline, what it detects, and how its patterns compose.")
        response (invoke-seat! seat prompt)
        value (:value response)
        restatement (:checkpoint-restatement value)]
    (when-not (and (map? value) (= #{:checkpoint-restatement} (set (keys value)))
                   (string? restatement) (not (str/blank? restatement)))
      (throw (ex-info "invalid checkpoint-only response" {:response (:raw response)})))
    (write-edn! (receipt-path paths (:turn state) "checkpoint-completion")
                {:schema 1 :event :checkpoint-dispatch-completion :at (now)
                 :seat seat :wire (:wire response) :response (:raw response)})
    (let [gate (run-linter! section paths (:turn state))]
      (when-not (zero? (:exit gate))
        (throw (ex-info "checkpoint linter failed" {:report (:report gate)})))
      (checkpoint! paths state restatement (get-in gate [:data :summary])))))

(defn record-malformed-absence! [section seat paths state]
  (let [pattern (:current-pattern state)
        absence {:pattern pattern :note "seat returned malformed output after two attempts"}
        completed (conj (:completed state) pattern)
        checkpoint? (zero? (mod (count completed) 20))]
    (append-records! (:absences paths) [absence])
    (try
      (let [_ (when checkpoint?
                (write-edn! (:state paths) (assoc state :phase :checkpoint-ready
                                                  :completed completed)))
            checkpoint (when checkpoint? (checkpoint-only! section seat paths state))
            _ (when checkpoint?
                (write-edn! (:state paths) (assoc state :phase :review-pending
                                                  :completed completed
                                                  :checkpoint (:number checkpoint))))
            next (assoc state :phase :turn-ready :completed completed :current-pattern nil
                        :failure-fingerprint nil :consecutive-same-failures 0
                        :checkpoint (or (:number checkpoint) (:checkpoint state)))]
        (write-edn! (:state paths) next) next)
      (catch Exception e
        (let [paused (assoc state :phase :paused :completed completed
                            :paused-reason :checkpoint-after-malformed-output-failed
                            :checkpoint-error (.getMessage e))]
          (write-edn! (:state paths) paused) paused)))))

(defn run-spider! [section seat budget]
  (let [paths (state-paths section)]
    (doseq [k [:run-dir :gates :receipts :checkpoints]] (fs/create-dirs (get paths k)))
    (when-not (fs/exists? (:attestations paths)) (write-edn! (:attestations paths) []))
    (when-not (fs/exists? (:proposals paths)) (write-edn! (:proposals paths) []))
    (when-not (fs/exists? (:absences paths)) (write-edn! (:absences paths) []))
    (let [preflight (run-linter! section paths 0)]
      (when-not (zero? (:exit preflight))
        (throw (ex-info "preflight linter failed" {:report (:report preflight)}))))
    (loop [state (reconcile! paths (read-edn (:state paths) (initial-state section seat)))
           remaining budget]
      (cond
        (= :paused (:phase state)) state
        (= (count (:completed state)) (count (:patterns state)))
        (let [done (assoc state :phase :complete :finished-at (now))]
          (write-edn! (:state paths) done) done)
        (zero? remaining) state
        (>= (:consecutive-same-failures state) 2)
        (recur (record-malformed-absence! section seat paths state) (dec remaining))
        :else (recur (process-turn! section seat paths state) (dec remaining))))))

(defn -main [& args]
  (let [{:keys [section seat budget]} (parse-args args)]
    (when-not (and section seat budget)
      (throw (ex-info "usage: spider_runner.clj --section NAME --seat ZAI-ID --budget N" {})))
    (println (pr-str (run-spider! section seat (parse-long budget))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
