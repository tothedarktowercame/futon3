(ns lab-stream-codex
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.engine :as engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon3.fulab.hud :as hud]
            [futon3.fulab.pattern-competence :as pc]))

(def ^:private pattern-file-path-re
  #"[A-Za-z0-9_./-]+\.(?:flexiarg|multiarg)")
(def ^:private fulab-id-re
  #"(fulab/[a-z0-9_-]+)")
(def ^:private ignore-pattern-ids
  #{"fulab/fulab-patterns"})
(def ^:private pattern-arg-re
  #"(?:^|\s)@(?:flexiarg|arg)\s+([a-z0-9_-]+/[a-z0-9_-]+)")
(def ^:private write-command-re
  #"(apply_patch|\bsed\b\s+-i|perl\s+-pi|\btee\b|\bmv\b|\bcp\b|\btruncate\b)")
(def ^:private pattern-action-window-ms 30000)
(def ^:private off-trail-defaults
  {:free 3
   :ratio 0.5
   :action "off-trail"})

(declare log-pattern-action! handle-pattern-action! maybe-log-aif-event!)


(defn usage []
  (println "Usage: dev/lab_stream_codex.clj [--lab-root PATH] [--patterns CSV] [--chosen ID] [--clock-in CSV] [--session-id ID] [--aif-config PATH] [--aif-select] [--proposal-hook] [--hud-json PATH] [--trail-allow CSV]")
  (println "Reads codex --json stream from stdin and appends session + AIF events.")
  (println "Clock-in entries are consumed in order, one per turn (override --chosen for that turn)."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--patterns" (recur (assoc opts :patterns (second remaining)) (nnext remaining))
        "--chosen" (recur (assoc opts :chosen (second remaining)) (nnext remaining))
        "--clock-in" (recur (assoc opts :clock-in (second remaining)) (nnext remaining))
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--aif-config" (recur (assoc opts :aif-config (second remaining)) (nnext remaining))
        "--aif-select" (recur (assoc opts :aif-select true) (rest remaining))
        "--proposal-hook" (recur (assoc opts :proposal-hook true) (rest remaining))
        "--hud-json" (recur (assoc opts :hud-json (second remaining)) (nnext remaining))
        "--trail-allow" (recur (assoc opts :trail-allow (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn parse-json [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Throwable _ nil)))

(defn parse-csv [value]
  (when (and value (not (str/blank? value)))
    (vec (remove str/blank? (str/split value #",")))))

(defn- coerce-output-text [value]
  (cond
    (string? value) value
    (map? value) (or (coerce-output-text (:aggregated_output value))
                     (coerce-output-text (:aggregated-output value))
                     (coerce-output-text (:text value))
                     (coerce-output-text (:output value))
                     (coerce-output-text (:stdout value)))
    (sequential? value) (->> value
                             (map coerce-output-text)
                             (remove nil?)
                             (str/join ""))
    :else nil))

(defn- pattern-paths-from-command [command]
  (->> (re-seq pattern-file-path-re (or command ""))
       (map (fn [match]
              (if (vector? match)
                (second match)
                match)))
       distinct
       vec))

(defn- command-write? [command]
  (boolean (re-find write-command-re (or command ""))))

(defn- command->action [command]
  (if (command-write? command) "update" "read"))

(defn- patterns-from-output [output]
  (->> (re-seq pattern-arg-re (or output ""))
       (map second)
       distinct
       vec))

(defn- patterns-from-command [command]
  (->> (re-seq fulab-id-re (or command ""))
       (map second)
       (remove ignore-pattern-ids)
       distinct
       vec))

(defn- normalize-path [repo-root path]
  (let [file (io/file path)]
    (str (if (.isAbsolute file)
           file
           (io/file repo-root path)))))

(defn- pattern-file? [path]
  (boolean (re-find pattern-file-path-re (or path ""))))

(defn- patterns-from-file [repo-root path]
  (let [full (normalize-path repo-root path)
        file (io/file full)]
    (when (.exists file)
      (patterns-from-output (slurp file)))))

(defn- patterns-from-change-paths [repo-root changes]
  (->> changes
       (keep :path)
       (filter pattern-file?)
       (mapcat #(patterns-from-file repo-root %))
       distinct
       vec))

(defn- normalize-off-trail-config [config]
  (let [base off-trail-defaults
        nested (when (map? (:off-trail config)) (:off-trail config))]
    (merge base
           nested
           (when (contains? config :off-trail/free) {:free (:off-trail/free config)})
           (when (contains? config :off-trail/ratio) {:ratio (:off-trail/ratio config)})
           (when (contains? config :off-trail/action) {:action (:off-trail/action config)}))))

(defn- parse-trail-allow [value]
  (let [entries (parse-csv value)]
    (reduce (fn [acc entry]
              (let [entry (str/lower-case (str entry))]
                (if (str/includes? entry "/")
                  (update acc :patterns conj entry)
                  (update acc :namespaces conj entry))))
            {:patterns #{} :namespaces #{}}
            entries)))

(defn- off-trail-action? [state action]
  (let [label (get-in @state [:off-trail :action] "off-trail")]
    (or (= action label)
        (= action (keyword label)))))

(defn- allowed-patterns [state]
  (let [{:keys [candidates clock-in-queue clock-in-current explicit-chosen trail-picked]} @state]
    (->> (concat candidates
                 clock-in-queue
                 trail-picked
                 (when clock-in-current [clock-in-current])
                 (when explicit-chosen [explicit-chosen]))
         (remove nil?)
         set)))

(defn- on-trail? [state pattern-id]
  (let [trail-allow (:trail-allow @state)
        namespace (some-> pattern-id (str/split #"/") first)]
    (or (contains? (allowed-patterns state) pattern-id)
        (contains? (:patterns trail-allow) pattern-id)
        (and namespace (contains? (:namespaces trail-allow) namespace)))))

(defn- bump-trail-stats! [state on-trail?]
  (swap! state update :trail-stats
         (fn [stats]
           (let [{:keys [on off]} (or stats {:on 0 :off 0})]
             (if on-trail?
               {:on (inc on) :off off}
               {:on on :off (inc off)})))))

(defn- trail-limit [state]
  (let [{:keys [on off]} (or (:trail-stats @state) {:on 0 :off 0})
        {:keys [free ratio]} (or (:off-trail @state) off-trail-defaults)]
    {:on on
     :off off
     :limit (+ (double free) (* (double ratio) (double on)))}))

(defn- off-trail-excess? [state]
  (let [{:keys [off limit]} (trail-limit state)]
    (> (double off) (double limit))))

(defn- parse-pattern-action-lines [text]
  (->> (str/split-lines (or text ""))
       (keep (fn [line]
               (when-let [payload (parse-json (str/trim line))]
                 (when (= "pattern-action" (:type payload))
                   payload))))))

(defn- post-json! [url payload]
  (try
    (let [conn ^java.net.HttpURLConnection (.openConnection (java.net.URL. url))
          data (.getBytes (json/write-str payload) "UTF-8")]
      (.setRequestMethod conn "POST")
      (.setDoOutput conn true)
      (.setRequestProperty conn "Content-Type" "application/json")
      (with-open [out (.getOutputStream conn)]
        (.write out data))
      (try
        (.getResponseCode conn)
        (catch Exception _ nil)))
    (catch Exception _ nil)))

(defn- record-pattern-action-rpc! [state action pattern-id note]
  (let [session-id (:session-id @state)
        server-url (or (System/getenv "FUTON3_CODEX_SERVER_URL")
                       (System/getenv "HUD_SERVER")
                       "http://localhost:5050")]
    (when (and session-id pattern-id)
      (post-json!
       (str (str/replace server-url #"/$" "") "/codex/pattern-action/" session-id)
       (cond-> {:pattern-id pattern-id
                :action action}
         (and note (not (str/blank? note)))
         (assoc :note note))))))

(defn- record-pattern-selection-rpc! [state psr]
  (let [session-id (:session-id @state)
        server-url (or (System/getenv "FUTON3_CODEX_SERVER_URL")
                       (System/getenv "HUD_SERVER")
                       "http://localhost:5050")]
    (when (and session-id psr)
      (post-json!
       (str (str/replace server-url #"/$" "") "/codex/pattern-selection/" session-id)
       {:psr psr}))))

(defn- record-pattern-use-rpc! [state pur]
  (let [session-id (:session-id @state)
        server-url (or (System/getenv "FUTON3_CODEX_SERVER_URL")
                       (System/getenv "HUD_SERVER")
                       "http://localhost:5050")]
    (when (and session-id pur)
      (post-json!
       (str (str/replace server-url #"/$" "") "/codex/pattern-use/" session-id)
       {:pur pur}))))

(defn read-hud-json [path]
  (when (and path (.exists (io/file path)))
    (try
      (json/read-str (slurp path) :key-fn keyword)
      (catch Throwable _ nil))))

(defn start-heartbeat! []
  (let [state (atom {:seen? false :ticks 0})
        stop? (atom false)
        worker (future
                 (while (not @stop?)
                   (Thread/sleep 2000)
                   (when-not (:seen? @state)
                     (swap! state update :ticks inc)
                     (println "[lab] waiting for codex stream... (no events yet)")
                     (flush))))]
    {:state state :stop? stop? :worker worker}))

(defn stop-heartbeat! [{:keys [stop? worker]}]
  (when stop?
    (reset! stop? true))
  (when worker
    @worker))

(defn now-inst []
  (java.util.Date.))

(defn- now-ms []
  (System/currentTimeMillis))

(defn ensure-session [lab-root thread-id]
  (let [path (io/file lab-root "sessions" (str thread-id ".edn"))]
    (if (.exists path)
      (pc/read-session-file (str path))
      {:session/id thread-id
       :session/agent :fucodex
       :events []})))

(defn write-session! [lab-root session-id session]
  (pc/write-session-file! (str (io/file lab-root "sessions" (str session-id ".edn"))) session))

(defn tau-cache-path [lab-root]
  (str (io/file lab-root "aif" "tau-cache.edn")))

(defn read-tau-cache [lab-root]
  (let [path (tau-cache-path lab-root)]
    (when (.exists (io/file path))
      (read-string (slurp path)))))

(defn write-tau-cache! [lab-root cache]
  (let [path (tau-cache-path lab-root)]
    (io/make-parents path)
    (spit path (pr-str cache))))

(defn append-event! [state event]
  (let [session (pc/append-event (:session @state) event)]
    (swap! state assoc :session session)
    (write-session! (:lab-root @state) (:session-id @state) session)
    (when (#{:aif/summary :aif/tap} (:event/type event))
      (maybe-log-aif-event! event))))

(defn ensure-raw-writer [state]
  (when-not (:raw-path @state)
    (let [path (io/file (:lab-root @state) "raw-stream" (str (:thread-id @state) ".jsonl"))]
      (io/make-parents path)
      (swap! state assoc :raw-path (str path)))))

(defn write-raw-line! [state line]
  (when (:raw-path @state)
    (spit (:raw-path @state) (str line "\n") :append true)))

(defn turn-anchor [turn]
  {:anchor/type :turn/completed
   :anchor/ref {:event/type :turn/completed
                :turn turn}})

(defn build-forecast [turn]
  {:benefits [{:tag :benefit/traceable-choice
               :locus (turn-anchor turn)
               :note "Decision captured at turn boundary."}]
   :risks [{:tag :risk/mismatch
            :locus (turn-anchor turn)
            :note "Pattern may not match actual change."}]
   :success [{:tag :success/traceable-outcome
              :locus (turn-anchor turn)
              :note "Outcome tied to decision."}]
   :failure [{:tag :failure/unresolved
              :locus (turn-anchor turn)
              :note "Decision does not explain outcome."}]})

(defn count-clock-ins [session]
  (count (filter #(= :clock-in/start (:event/type %)) (:events session))))

(defn last-turn [session]
  (let [turns (->> (:events session)
                   (filter #(= :turn/completed (:event/type %)))
                   (map :turn)
                   (remove nil?))]
    (if (seq turns)
      (apply max turns)
      0)))

(defn build-psr [thread-id turn candidates chosen]
  {:psr/id (str "psr-" turn)
   :session/id thread-id
   :decision/id (str thread-id ":turn-" turn)
   :candidates candidates
   :chosen chosen
   :context/anchors [(turn-anchor turn)]
   :forecast (build-forecast turn)
   :rejections (into {}
                     (for [p candidates :when (not= p chosen)]
                       [p {:codes [:reject/fit]
                           :note "Alternate pattern not selected."}]))
   :horizon :immediate})

(defn build-pur [thread-id turn pattern-id]
  (let [uid (str (java.util.UUID/randomUUID))]
    {:pur/id (str "pur-" turn "-" uid)
   :session/id thread-id
   :pattern/id pattern-id
   :instance/id (str "pur-" turn "-" uid)
   :decision/id (str thread-id ":turn-" turn)
   :fields {:context "Captured at turn completion."
            :if "A decision was needed."
            :however "Fit only known after the turn."
            :then "Recorded the observed outcome."
            :because "Need traceability for decisions."
            :next-steps "Compare decision vs outcome."}
   :anchors [(turn-anchor turn)]
   :outcome/tags [:outcome/partial]}))

(defn fmt-num [value]
  (when (number? value)
    (format "%.3f" (double value))))

(defn- aif-event-line [event]
  (let [payload (:payload event)
        kind (:aif/kind payload)
        tap-event (:event payload)
        result (:aif/result payload)
        aif (or (:aif result) (:aif payload))
        tau (or (:tau aif) (:tau-updated aif))
        g-chosen (:G-chosen aif)
        prediction-error (:prediction-error aif)
        evidence-score (:evidence-score aif)
        evidence-delta (:evidence-delta aif)
        key (or kind tap-event)
        parts (cond-> []
                key (conj (str "k=" (name key)))
                (number? tau) (conj (str "tau=" (fmt-num tau)))
                (number? g-chosen) (conj (str "G=" (fmt-num g-chosen)))
                (number? prediction-error) (conj (str "o=" (fmt-num prediction-error)))
                (number? evidence-score) (conj (str "e=" (fmt-num evidence-score)))
                (number? evidence-delta) (conj (str "dE=" (fmt-num evidence-delta))))]
    (when (seq parts)
      (str "[aif] " (str/join " " parts)))))

(defn- maybe-log-aif-event! [event]
  (when-let [line (aif-event-line event)]
    (println line)
    (flush)))

(defn selection-explainer [selection explicit-chosen used-aif]
  (let [aif (:aif selection)
        chosen (:chosen selection)
        g-chosen (fmt-num (:G-chosen aif))
        tau (fmt-num (:tau aif))
        rejected (:G-rejected aif)
        rejected-vals (when (map? rejected) (vals rejected))
        rej-min (fmt-num (when (seq rejected-vals) (apply min rejected-vals)))]
    (cond
      used-aif
      (str "AIF selected " chosen
           (when g-chosen (str " (G=" g-chosen))
           (when rej-min (str ", next-best=" rej-min))
           (when tau (str ", tau=" tau))
           ").")
      explicit-chosen
      (str "Explicit choice " explicit-chosen
           (when g-chosen (str " (G=" g-chosen))
           (when tau (str ", tau=" tau))
           ").")
      :else nil)))

(defn- selection-metrics [selection]
  (let [aif (:aif selection)
        rejected (:G-rejected aif)]
    (cond-> {}
      (number? (:G-chosen aif)) (assoc :G-chosen (:G-chosen aif))
      (number? (:tau aif)) (assoc :tau (:tau aif))
      (and (map? rejected) (seq rejected)) (assoc :G-rejected rejected))))

(defn- selection-anchors [actions]
  (->> actions
       (filter #(= "read" (:action %)))
       (map (fn [action]
              {:anchor/type :pattern/action
               :anchor/ref {:event/type :pattern/action
                            :pattern/id (:pattern-id action)
                            :pattern/action "read"}}))
       vec))

(defn- selection-reason [selection explicit-chosen used-aif actions]
  (let [read-actions (filter #(= "read" (:action %)) actions)
        read-ids (vec (distinct (map :pattern-id read-actions)))
        explainer (selection-explainer selection explicit-chosen used-aif)
        mode (cond
               used-aif :aif
               explicit-chosen :explicit
               :else :heuristic)
        metrics (selection-metrics selection)]
    (cond-> {:mode mode
             :reads read-ids}
      (seq metrics) (assoc :metrics metrics)
      explainer (assoc :explainer explainer))))

(defn- action-with-reason? [action note]
  (and (#{"implement" "update"} action)
       (not (str/blank? (or note "")))))

(def ^:private report-actions
  #{"read" "implement" "update"})

(def ^:private report-implement-re
  #"(?:^|/)(src|test|dev|scripts|apps)/|\\.(?:clj|cljs|cljc|edn|bb|sh)$")

(def ^:private report-update-re
  #"(?:^|/)(docs|library|resources)/|README|\\.md$")

(defn- normalize-report-action [value]
  (let [action (some-> value str str/lower-case str/trim)]
    (when (contains? report-actions action)
      action)))

(defn- infer-report-action [paths]
  (let [paths (map str (or paths []))]
    (cond
      (some #(re-find report-implement-re %) paths) "implement"
      (some #(re-find report-update-re %) paths) "update"
      (seq paths) "update"
      :else "read")))

(defn- report->pattern-action [report paths]
  (let [action (or (normalize-report-action (:action report))
                   (infer-report-action paths))
        note (some-> (:notes report) str)]
    {:pattern-id (:applied report)
     :action action
     :note note}))

(defn- auto-update? [source action]
  (and (= source :auto) (#{"implement" "update"} action)))

(defn- record-warning! [state reason action pattern-id note source]
  (swap! state update :use-warnings
         (fn [warnings]
           (conj (vec (or warnings []))
                 {:reason reason
                  :pattern-id pattern-id
                  :action action
                  :note note
                  :source source
                  :at (now-inst)}))))

(defn- record-explicit-reason! [state pattern-id]
  (swap! state assoc-in [:explicit-reasons pattern-id] (now-ms)))

(defn- recent-explicit-reason? [state pattern-id]
  (let [seen-at (get-in @state [:explicit-reasons pattern-id])]
    (and seen-at (< (- (now-ms) seen-at) pattern-action-window-ms))))

(defn proposal-draft [session-id turn candidates]
  {:event/type :pattern/proposal-draft
   :at (now-inst)
   :payload {:session/id session-id
             :turn turn
             :proposal/reason "Insufficient pattern candidates; consider proposing a new pattern."
             :proposal/candidates candidates}})

(defn tau->uncertainty [tau-scale tau]
  (when (and tau-scale (number? tau) (pos? tau))
    (max 1.0 (/ (double tau-scale) (double tau)))))

(defn summary-event [thread-id kind result]
  {:event/type :aif/summary
   :at (now-inst)
   :payload {:session/id thread-id
             :aif/kind kind
             :aif/result result}})

(defn- format-fulab-summary [state]
  (let [{:keys [session-id turn last-psr last-pur pattern-action-counts trail-stats use-warnings]} @state]
    {:session/id session-id
     :turn turn
     :psr/chosen (:chosen last-psr)
     :pur/pattern (:pattern/id last-pur)
     :actions (or pattern-action-counts {})
     :trail (or trail-stats {:on 0 :off 0})
     :warnings (count (or use-warnings []))}))

(defn- emit-session-summary! [state]
  (when-not (:summary-emitted? @state)
    (let [summary (format-fulab-summary state)
          applied (or (:pur/pattern summary) (:psr/chosen summary))
          notes (format "turn=%s actions=%s trail=%s"
                        (:turn summary)
                        (:actions summary)
                        (:trail summary))]
      (println "[FULAB-SUMMARY]")
      (println (str ":session/id \"" (or (:session/id summary) "") "\""))
      (println (str ":turn " (or (:turn summary) 0)))
      (println (str ":psr/chosen \"" (or (:psr/chosen summary) "") "\""))
      (println (str ":pur/pattern \"" (or (:pur/pattern summary) "") "\""))
      (println (str ":actions " (:actions summary)))
      (println (str ":trail " (:trail summary)))
      (println (str ":warnings " (:warnings summary)))
      (println "[/FULAB-SUMMARY]")
      (when (and applied (not (:agent-report-seen? @state)))
        (println "[FULAB-REPORT]")
        (println (str ":applied \"" applied "\""))
        (println (str ":notes \"" notes "\""))
        (println "[/FULAB-REPORT]"))
      (flush)
      (swap! state assoc :summary-emitted? true))))

(defn- compute-turn-selection [state candidates chosen aif-config engine]
  (let [turn (or (:turn-current @state) (inc (:turn @state)))
        session-id (:session-id @state)
        current (first (:clock-in-queue @state))
        explicit-chosen (or current chosen)
        use-aif (and (:aif-select? @state) (nil? explicit-chosen))
        decision-id (str session-id ":turn-" turn)
        anchors [(turn-anchor turn)]
        forecast (build-forecast turn)
        cached-tau (get (:tau-cache @state) explicit-chosen)
        uncertainty (tau->uncertainty (:tau/scale aif-config) cached-tau)
        candidate-scores (:candidate-scores @state)
        selection-context (cond-> {:decision/id decision-id
                                   :session/id session-id
                                   :candidates candidates
                                   :anchors anchors
                                   :forecast forecast}
                            candidate-scores (assoc :candidate-scores candidate-scores)
                            uncertainty (assoc :uncertainty uncertainty)
                            (not use-aif) (assoc :chosen explicit-chosen))
        selection (engine/select-pattern engine selection-context)
        chosen-final (if use-aif (:chosen selection) explicit-chosen)
        explainer (selection-explainer selection explicit-chosen use-aif)
        psr (cond-> (build-psr session-id turn candidates chosen-final)
              explainer (assoc :aif/explainer explainer))]
    {:turn turn
     :current current
     :selection selection
     :chosen chosen-final
     :explicit-chosen explicit-chosen
     :used-aif use-aif
     :psr psr}))

(defn- ensure-turn-selection! [state candidates chosen aif-config engine]
  (if-let [selection (:turn-selection @state)]
    {:turn (:turn-current @state)
     :current (:turn-clock-in @state)
     :selection selection
     :chosen (:turn-chosen @state)
     :explicit-chosen (:turn-explicit @state)
     :used-aif (:turn-used-aif @state)
     :psr (:turn-psr @state)}
    (let [{:keys [turn current selection chosen psr explicit-chosen used-aif] :as result}
          (compute-turn-selection state candidates chosen aif-config engine)]
      (swap! state assoc :turn-current turn
                       :turn-clock-in current
                       :turn-selection selection
                       :turn-chosen chosen
                       :turn-psr psr
                       :turn-explicit explicit-chosen
                       :turn-used-aif used-aif)
      result)))

(defn- emit-selection-summary! [state selection]
  (when (and selection (not (:turn-selection-summary-emitted? @state)))
    (append-event! state (summary-event (:session-id @state) :psr selection))
    (swap! state assoc :turn-selection-summary-emitted? true)))

(defn- emit-psr! [state psr selection chosen used?]
  (when (and psr (not (:turn-psr-emitted? @state)))
    (append-event! state {:event/type :pattern/selection-claimed
                          :at (now-inst)
                          :payload {:psr psr}})
    (record-pattern-selection-rpc! state psr)
    (let [reason (:selection/reason psr)
          mode (:mode reason)
          reads (seq (:reads reason))
          use-label (if used? "use=recorded" "use=none")]
      (println (format "[pattern-selection] chosen=%s candidates=%d mode=%s%s %s"
                       chosen
                       (count (:candidates psr))
                       (or mode :unknown)
                       (if reads
                         (str " reads=" (str/join "," reads))
                         "")
                       use-label)))
    (flush)
    (swap! state assoc :turn-psr-emitted? true
                     :turn-selection selection
                     :turn-psr psr
                     :turn-chosen chosen
                     :last-psr psr)))

(defn- pattern-actions-for-turn [actions pattern-id]
  (->> actions
       (filter #(= pattern-id (:pattern-id %)))
       (map :action)
       distinct
       vec))

(defn- pattern-files-for-turn [actions pattern-id]
  (->> actions
       (filter #(and (= pattern-id (:pattern-id %))
                     (#{"implement" "update"} (:action %))))
       (mapcat :files)
       (remove nil?)
       distinct
       vec))

(defn- emit-pur! [state pur]
  (when pur
    (append-event! state {:event/type :pattern/use-claimed
                          :at (now-inst)
                          :payload {:pur pur}})
    (record-pattern-use-rpc! state pur)
    (let [actions (pattern-actions-for-turn (:turn-actions @state) (:pattern/id pur))
          turn-files (vec (distinct (or (:turn-files @state) [])))
          files (let [pattern-files (pattern-files-for-turn (:turn-actions @state) (:pattern/id pur))]
                  (if (seq pattern-files)
                    pattern-files
                    turn-files))
          reason (:use/reason pur)
          suffix (when (seq files)
                   (str " \u2192 " (str/join ", " files)))
          action-label (when (seq actions)
                         (str " actions=" (str/join "," actions)))
          reason-label (when (and reason (not (str/blank? reason)))
                         (str " because=" reason))]
      (println (format "[pattern-use] %s%s%s%s"
                       (:pattern/id pur)
                       (or suffix "")
                       (or action-label "")
                       (or reason-label ""))))
    (flush)
    (swap! state assoc :turn-pur-emitted? true
                     :turn-pur-files-logged? (boolean (seq (pattern-files-for-turn (:turn-actions @state)
                                                                                   (:pattern/id pur))))
                     :turn-pur pur
                     :last-pur pur)))

(defn- clear-turn-state! [state]
  (swap! state assoc :turn-open? false
                   :turn-current nil
                   :turn-clock-in nil
                   :turn-selection nil
                   :turn-psr nil
                   :turn-psr-emitted? false
                   :turn-selection-summary-emitted? false
                   :turn-chosen nil
                   :turn-explicit nil
                   :turn-used-aif nil
                   :turn-pur nil
                   :turn-pur-emitted? false
                   :turn-pur-files-logged? false
                   :turn-actions []
                   :turn-files []))

(defn process-turn-complete! [state candidates chosen aif-config engine]
  "Process a completed turn - emit PSR/PUR and AIF events."
  (let [{:keys [turn current selection chosen psr explicit-chosen used-aif]}
        (ensure-turn-selection! state candidates chosen aif-config engine)
        session-id (:session-id @state)
        pur (or (:turn-pur @state)
                (build-pur session-id turn chosen))
        actions (:turn-actions @state)
        reason (selection-reason selection explicit-chosen used-aif actions)
        anchors (selection-anchors actions)
        psr (cond-> (assoc psr :selection/reason reason)
              (seq anchors) (assoc :selection/anchors anchors))
        belief-update (engine/update-beliefs engine {:decision/id (:decision/id pur)
                                                    :session/id session-id
                                                    :outcome (:outcome/tags pur)
                                                    :status :observed})]
    (swap! state assoc :turn turn)
    (when chosen
      (swap! state update :trail-picked (fnil conj #{}) chosen))
    (when current
      (append-event! state {:event/type :clock-in/start
                            :turn turn
                            :at (now-inst)
                            :clock-in/pattern-id current
                            :clock-in/intent "fucodex live run"})
      (swap! state assoc :clock-in-queue (vec (rest (:clock-in-queue @state)))
                   :clock-in-current current))
    (append-event! state {:event/type :turn/completed
                          :turn turn
                          :at (now-inst)})
    (emit-psr! state psr selection chosen (:turn-pur-emitted? @state))
    (when (and (:turn-pur @state) (not (:turn-pur-files-logged? @state)))
      (let [turn-files (vec (distinct (or (:turn-files @state) [])))
            files (let [pattern-files (pattern-files-for-turn actions (:pattern/id (:turn-pur @state)))]
                    (if (seq pattern-files)
                      pattern-files
                      turn-files))]
        (when (seq files)
          (println (format "[pattern-use] %s \u2192 %s"
                           (:pattern/id (:turn-pur @state))
                           (str/join ", " files)))
          (flush)
          (swap! state assoc :turn-pur-files-logged? true))))
    (when-let [report (:last-agent-report @state)]
      (let [turn-files (vec (distinct (or (:turn-files @state) [])))
            {:keys [pattern-id action note]} (report->pattern-action report turn-files)]
        (when (and pattern-id action)
          (handle-pattern-action! state engine {:pattern-id pattern-id
                                                :action action
                                                :note note
                                                :files (when (seq turn-files) turn-files)
                                                :source :report})))
      (let [hud-id (get-in @state [:hud :hud/id])]
        (append-event! state {:event/type :hud/agent-reported
                              :hud/id hud-id
                              :at (now-inst)
                              :payload {:hud/id hud-id
                                        :session/id session-id
                                        :turn turn
                                        :report report}}))
      (swap! state assoc :last-agent-report nil
                   :agent-report-seen? true))
    (when (and (:proposal-hook? @state) (< (count candidates) 2))
      (append-event! state (proposal-draft session-id turn candidates)))
    (append-event! state (summary-event session-id :psr selection))
    (append-event! state (summary-event session-id :pur belief-update))
    (when-let [tau-updated (get-in belief-update [:aif :tau-updated])]
      (let [next-cache (assoc (:tau-cache @state) chosen tau-updated)]
        (swap! state assoc :tau-cache next-cache)
        (write-tau-cache! (:lab-root @state) next-cache)))
    (clear-turn-state! state)))

(defn- mark-pattern-action! [state action pattern-id]
  (swap! state assoc-in [:pattern-action-last [action pattern-id]] (now-ms)))

(defn- recently-seen? [state action pattern-id]
  (let [seen-at (get-in @state [:pattern-action-last [action pattern-id]])]
    (and seen-at (< (- (now-ms) seen-at) pattern-action-window-ms))))

(defn- handle-pattern-action! [state engine payload]
  (let [session-id (:session-id @state)
        pattern-id (:pattern-id payload)
        action (:action payload)
        note (:note payload)
        files (:files payload)
        source (keyword (or (:source payload) "explicit"))
        explicit? (contains? #{:explicit :report} source)
        report? (= source :report)
        has-reason (action-with-reason? action note)]
    (when (and session-id pattern-id action)
      (let [decision-id (str session-id ":action-" (java.util.UUID/randomUUID))
            event {:event/type :pattern/action
                   :at (now-inst)
                   :payload {:session/id session-id
                             :pattern/id pattern-id
                             :pattern/action action
                             :pattern/note note
                             :pattern/files (when (seq files) files)
                             :pattern/source source}}]
        (swap! state update :pattern-action-counts
               (fn [counts]
                 (let [counts (or counts {})]
                   (update counts action (fnil inc 0)))))
        (append-event! state event)
        (when (or explicit? (= action "read"))
          (let [update (engine/update-beliefs engine {:decision/id decision-id
                                                      :session/id session-id
                                                      :pattern/id pattern-id
                                                      :pattern/action action
                                                      :outcome (str "pattern-action " action " " pattern-id)
                                                      :status :observed})]
            (append-event! state (summary-event session-id :pattern-action update))))
        (when (and explicit? (or (false? (:ok payload)) (= "skipped" (:rpc payload))))
          (record-pattern-action-rpc! state action pattern-id note))
        (when report?
          (record-pattern-action-rpc! state action pattern-id note))
        (swap! state update :turn-actions
               (fn [actions]
                 (conj (vec (or actions []))
                       {:pattern-id pattern-id
                        :action action
                        :note note
                        :files files
                        :source source
                        :at (now-inst)})))
        (when (and explicit? (#{"implement" "update"} action) (not has-reason))
          (record-warning! state :missing-reason action pattern-id note source))
        (when (and explicit? has-reason)
          (record-explicit-reason! state pattern-id))
        (when (and (auto-update? source action)
                   (not (recent-explicit-reason? state pattern-id)))
          (record-warning! state :auto-update action pattern-id note source))
        (when (and explicit? has-reason)
          (let [{:keys [turn chosen]}
                (ensure-turn-selection! state
                                        (:candidates @state)
                                        (:explicit-chosen @state)
                                        (:aif-config @state)
                                        engine)
                pur (-> (build-pur session-id turn pattern-id)
                        (assoc :use/reason note)
                        (assoc-in [:fields :because] note)
                        (update :anchors conj {:anchor/type :pattern/action
                                               :anchor/ref {:event/type :pattern/action
                                                            :pattern/id pattern-id
                                                            :pattern/action action}}))]
            (emit-pur! state pur)
            (swap! state assoc :turn-chosen (or chosen pattern-id))))
        (mark-pattern-action! state action pattern-id)))
    (when (and pattern-id action (not (off-trail-action? state action)))
      (let [on-trail (on-trail? state pattern-id)]
        (bump-trail-stats! state on-trail)
        (when (and (not on-trail) (off-trail-excess? state))
          (let [{:keys [off on limit]} (trail-limit state)
                off-action (get-in @state [:off-trail :action] "off-trail")
                note (format "off-trail count=%d on-trail=%d limit=%.1f"
                             off on limit)]
            (log-pattern-action! state engine off-action pattern-id note)))))))

(defn- log-pattern-action! [state engine action pattern-id note & [files]]
  (when-not (recently-seen? state action pattern-id)
    (handle-pattern-action! state engine {:pattern-id pattern-id
                                          :action action
                                          :note note
                                          :files files
                                          :source :auto})
    (println (format "[pattern-action] %s %s%s"
                     action
                     pattern-id
                     (if (and note (not (str/blank? note)))
                       (str " - " note)
                       "")))
    (flush)))

(defn read-aif-config [path]
  (when path
    (try
      (edn/read-string (slurp path))
      (catch Throwable _ nil))))

(defn ensure-candidates [candidates clock-ins]
  (if (seq clock-ins)
    (reduce (fn [acc pid]
              (if (some #(= pid %) acc)
                acc
                (conj acc pid)))
            (vec candidates)
            clock-ins)
    (vec candidates)))

(defn -main [& args]
  (let [{:keys [help unknown lab-root patterns chosen clock-in session-id aif-config aif-select proposal-hook hud-json trail-allow]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        clock-ins (parse-csv clock-in)
        hud-response (read-hud-json hud-json)
        hud-state (:hud hud-response)
        hud-candidates (when (seq (get hud-state :candidates))
                         (mapv :id (get hud-state :candidates)))
        hud-scores (when (seq (get hud-state :candidates))
                     (into {}
                           (keep (fn [entry]
                                   (when (and (:id entry) (number? (:score entry)))
                                     [(:id entry) (:score entry)])))
                           (get hud-state :candidates)))
        base-candidates (cond
                          (and patterns (not (str/blank? patterns)))
                          (vec (remove str/blank? (str/split patterns #",")))
                          (seq hud-candidates) hud-candidates
                          :else ["ants/white-space-scout" "ants/pheromone-trail-tuner"])
        candidates (ensure-candidates base-candidates clock-ins)
        aif-config (read-aif-config aif-config)
        off-trail-config (normalize-off-trail-config (or aif-config {}))
        trail-allow (parse-trail-allow trail-allow)
        engine (engine/new-engine (fulab/new-adapter aif-config) {:beliefs {}})
        state (atom {:lab-root lab-root
                     :thread-id nil
                     :session-id session-id
                     :session nil
                     :turn 0
                     :raw-path nil
                     :clock-in-queue clock-ins
                     :clock-in-current nil
                     :candidates candidates
                     :explicit-chosen chosen
                     :trail-picked #{}
                     :trail-stats {:on 0 :off 0}
                     :off-trail off-trail-config
                     :trail-allow trail-allow
                     :turn-open? false
                     :turn-actions []
                     :turn-files []
                     :aif-config aif-config
                     :aif-config-logged? false
                     :hud hud-state
                     :candidate-scores hud-scores
                     :hud-logged? false
                     :last-agent-report nil
                     :aif-select? (boolean aif-select)
                     :proposal-hook? (boolean proposal-hook)
                     :tau-cache (or (read-tau-cache lab-root) {})
                     :pattern-action-last {}
                     :pattern-action-counts {}
                     :last-psr nil
                     :last-pur nil
                     :turn-explicit nil
                     :turn-used-aif nil
                     :turn-selection-summary-emitted? false
                     :turn-pur-files-logged? false
                     :agent-report-seen? false
                     :summary-emitted? false
                     :use-warnings []
                     :explicit-reasons {}})
        tap-listener (fn [event]
                       (when (= :aif/fulab (:type event))
                         (append-event! state {:event/type :aif/tap
                                               :at (now-inst)
                                               :payload event})))]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      :else
      (do
        (add-tap tap-listener)
        (let [heartbeat (start-heartbeat!)]
          (try
            (doseq [line (line-seq (io/reader *in*))]
              (let [event (parse-json line)]
                (when event
                  (swap! (:state heartbeat) assoc :seen? true)
                  (when-let [thread-id (:thread_id event)]
                    (when-not (:thread-id @state)
                      (let [session-id (or (:session-id @state) thread-id)
                            session (ensure-session lab-root session-id)
                            consumed (count-clock-ins session)
                            remaining (vec (drop consumed (:clock-in-queue @state)))]
                        (swap! state assoc :thread-id thread-id
                               :session-id session-id
                               :session session
                               :turn (last-turn session)
                               :clock-in-queue remaining)
                        (ensure-raw-writer state)
                        (when (and (:hud @state) (not (:hud-logged? @state)))
                          (let [hud-id (get-in @state [:hud :hud/id])]
                            (append-event! state {:event/type :hud/initialized
                                                  :hud/id hud-id
                                                  :at (now-inst)
                                                  :payload (assoc (:hud @state)
                                                                  :session/id session-id)}))
                          (swap! state assoc :hud-logged? true))
                        (when (and (:aif-config @state) (not (:aif-config-logged? @state)))
                          (append-event! state {:event/type :aif/config
                                                :at (now-inst)
                                                :payload {:session/id session-id
                                                          :config (:aif-config @state)}})
                          (swap! state assoc :aif-config-logged? true)))))
                  (when (:thread-id @state)
                    (write-raw-line! state line))
                  (when (and (= (:type event) "item.completed")
                             (= (get-in event [:item :type]) "agent_message"))
                    (let [text (get-in event [:item :text])
                          report (when (string? text)
                                   (hud/parse-agent-report text))]
                      (when report
                        (swap! state assoc :last-agent-report report))))
                  (when (and (= (:type event) "item.completed")
                             (= (get-in event [:item :type]) "command_execution"))
                    (let [command (get-in event [:item :command])
                          output (coerce-output-text (get-in event [:item]))
                          action (command->action command)
                          output-patterns (patterns-from-output output)
                          command-patterns (patterns-from-command command)
                          command-paths (pattern-paths-from-command command)
                          path-patterns (mapcat #(patterns-from-file repo-root %) command-paths)]
                      (doseq [payload (parse-pattern-action-lines output)]
                        (handle-pattern-action! state engine payload))
                      (when (or (seq command-paths) (seq output-patterns))
                        (when-let [pattern-ids (seq (distinct (concat output-patterns
                                                                     command-patterns
                                                                     path-patterns)))]
                          (doseq [pattern-id pattern-ids]
                            (log-pattern-action! state
                                                 engine
                                                 action
                                                 pattern-id
                                                 "auto-detected from command"
                                                 command-paths))))))
                  (when (and (= (:type event) "item.completed")
                             (= (get-in event [:item :type]) "file_change"))
                    (let [changes (get-in event [:item :changes])
                          paths (->> changes (map :path) (remove nil?) vec)]
                      (when (seq paths)
                        (swap! state update :turn-files
                               (fn [existing]
                                 (vec (distinct (concat (or existing []) paths))))))
                      (when-let [pattern-ids (seq (patterns-from-change-paths repo-root changes))]
                        (doseq [pattern-id pattern-ids]
                          (log-pattern-action! state
                                               engine
                                               "update"
                                               pattern-id
                                               "auto-detected from file_change"
                                               paths)))))
                  (when (= (:type event) "turn.started")
                    (swap! state assoc :turn-open? true)
                    (let [{:keys [selection]} (ensure-turn-selection! state candidates chosen aif-config engine)]
                      (emit-selection-summary! state selection)))
                  (when (= (:type event) "turn.completed")
                    (swap! state assoc :turn-open? false)
                    (process-turn-complete! state candidates chosen aif-config engine)))))
              (when (and (:turn-open? @state) (:session-id @state))
                (swap! state assoc :turn-open? false)
                (process-turn-complete! state candidates chosen aif-config engine))
            (finally
              (emit-session-summary! state)
              (stop-heartbeat! heartbeat))))
        (remove-tap tap-listener)))))
(apply -main *command-line-args*)
