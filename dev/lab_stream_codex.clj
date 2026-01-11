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

(declare log-pattern-action! handle-pattern-action!)

(defn usage []
  (println "Usage: dev/lab_stream_codex.clj [--lab-root PATH] [--patterns CSV] [--chosen ID] [--clock-in CSV] [--session-id ID] [--aif-config PATH] [--aif-select] [--proposal-hook] [--hud-json PATH]")
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
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn parse-json [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Throwable _ nil)))

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
  (contains? (allowed-patterns state) pattern-id))

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
    (write-session! (:lab-root @state) (:session-id @state) session)))

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
  {:pur/id (str "pur-" turn)
   :session/id thread-id
   :pattern/id pattern-id
   :instance/id (str "pur-" turn "-a")
   :decision/id (str thread-id ":turn-" turn)
   :fields {:context "Captured at turn completion."
            :if "A decision was needed."
            :however "Fit only known after the turn."
            :then "Recorded the observed outcome."
            :because "Need traceability for decisions."
            :next-steps "Compare decision vs outcome."}
   :anchors [(turn-anchor turn)]
   :outcome/tags [:outcome/partial]})

(defn summary-event [thread-id kind result]
  {:event/type :aif/summary
   :at (now-inst)
   :payload {:session/id thread-id
             :aif/kind kind
             :aif/result result}})

(defn- mark-pattern-action! [state action pattern-id]
  (swap! state assoc-in [:pattern-action-last [action pattern-id]] (now-ms)))

(defn- recently-seen? [state action pattern-id]
  (let [seen-at (get-in @state [:pattern-action-last [action pattern-id]])]
    (and seen-at (< (- (now-ms) seen-at) pattern-action-window-ms))))

(defn- handle-pattern-action! [state engine payload]
  (let [session-id (:session-id @state)
        pattern-id (:pattern-id payload)
        action (:action payload)
        note (:note payload)]
    (when (and session-id pattern-id action)
      (let [decision-id (str session-id ":action-" (java.util.UUID/randomUUID))
            event {:event/type :pattern/action
                   :at (now-inst)
                   :payload {:session/id session-id
                             :pattern/id pattern-id
                             :pattern/action action
                             :pattern/note note}}
            update (engine/update-beliefs engine {:decision/id decision-id
                                                  :session/id session-id
                                                  :pattern/id pattern-id
                                                  :pattern/action action
                                                  :outcome (str "pattern-action " action " " pattern-id)
                                                  :status :observed})]
        (append-event! state event)
        (append-event! state (summary-event session-id :pattern-action update))
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

(defn- log-pattern-action! [state engine action pattern-id note]
  (when-not (recently-seen? state action pattern-id)
    (record-pattern-action-rpc! state action pattern-id note)
    (handle-pattern-action! state engine {:pattern-id pattern-id
                                          :action action
                                          :note note})
    (println (format "[pattern-action] %s %s%s"
                     action
                     pattern-id
                     (if (and note (not (str/blank? note)))
                       (str " - " note)
                       "")))
    (flush)))

(defn parse-csv [value]
  (when (and value (not (str/blank? value)))
    (vec (remove str/blank? (str/split value #",")))))

(defn fmt-num [value]
  (when (number? value)
    (format "%.3f" (double value))))

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
  (let [{:keys [help unknown lab-root patterns chosen clock-in session-id aif-config aif-select proposal-hook hud-json]} (parse-args args)
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
                     :aif-config aif-config
                     :aif-config-logged? false
                     :hud hud-state
                     :candidate-scores hud-scores
                     :hud-logged? false
                     :last-agent-report nil
                     :aif-select? (boolean aif-select)
                     :proposal-hook? (boolean proposal-hook)
                     :tau-cache (or (read-tau-cache lab-root) {})
                     :pattern-action-last {}})
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
                          path-patterns (mapcat #(patterns-from-file repo-root %) command-paths)
                          pattern-ids (seq (distinct (concat output-patterns
                                                             command-patterns
                                                             path-patterns)))]
                      (doseq [payload (parse-pattern-action-lines output)]
                        (handle-pattern-action! state engine payload))
                      (when (and (seq pattern-ids) (or (seq command-paths) (seq output-patterns)))
                        (doseq [pattern-id pattern-ids]
                          (log-pattern-action! state
                                               engine
                                               action
                                               pattern-id
                                               "auto-detected from command")))))
                  (when (and (= (:type event) "item.completed")
                             (= (get-in event [:item :type]) "file_change"))
                    (let [changes (get-in event [:item :changes])
                          pattern-ids (seq (patterns-from-change-paths repo-root changes))]
                      (doseq [pattern-id pattern-ids]
                        (log-pattern-action! state
                                             engine
                                             "update"
                                             pattern-id
                                             "auto-detected from file_change"))))
                  (when (= (:type event) "turn.completed")
                    (let [turn (inc (:turn @state))
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
                                explainer (assoc :aif/explainer explainer))
                          pur (build-pur session-id turn chosen-final)
                          update (engine/update-beliefs engine {:decision/id (:decision/id pur)
                                                                :session/id session-id
                                                                :outcome (:outcome/tags pur)
                                                                :status :observed})]
                      (swap! state assoc :turn turn)
                      (when chosen-final
                        (swap! state update :trail-picked (fnil conj #{}) chosen-final))
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
                      (append-event! state {:event/type :pattern/selection-claimed
                                            :at (now-inst)
                                            :payload {:psr psr}})
                      (append-event! state {:event/type :pattern/use-claimed
                                            :at (now-inst)
                                            :payload {:pur pur}})
                      (record-pattern-selection-rpc! state psr)
                      (println (format "[pattern-selection] chosen=%s candidates=%d"
                                       chosen-final
                                       (count candidates)))
                      (record-pattern-use-rpc! state pur)
                      (println (format "[pattern-use] %s" chosen-final))
                      (flush)
                      (when-let [report (:last-agent-report @state)]
                        (let [hud-id (get-in @state [:hud :hud/id])]
                          (append-event! state {:event/type :hud/agent-reported
                                                :hud/id hud-id
                                                :at (now-inst)
                                                :payload {:hud/id hud-id
                                                          :session/id session-id
                                                          :turn turn
                                                          :report report}}))
                        (swap! state assoc :last-agent-report nil))
                      (when (and (:proposal-hook? @state) (< (count candidates) 2))
                        (append-event! state (proposal-draft session-id turn candidates)))
                      (append-event! state (summary-event session-id :psr selection))
                      (append-event! state (summary-event session-id :pur update))
                      (when-let [tau-updated (get-in update [:aif :tau-updated])]
                        (let [next-cache (assoc (:tau-cache @state) chosen-final tau-updated)]
                          (swap! state assoc :tau-cache next-cache)
                          (write-tau-cache! lab-root next-cache))))))))
            (finally
              (stop-heartbeat! heartbeat))))
        (remove-tap tap-listener)))))
(apply -main *command-line-args*)
