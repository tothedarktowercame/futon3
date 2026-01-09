(ns lab-stream-claude
  "Stream handler for Claude Code's --output-format stream-json output."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.engine :as engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon3.fulab.hud :as hud]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-stream-claude.clj [--lab-root PATH] [--patterns CSV] [--chosen ID] [--clock-in CSV] [--session-id ID] [--aif-config PATH] [--aif-select] [--proposal-hook] [--hud-json PATH]")
  (println "Reads Claude --print --output-format stream-json from stdin and appends session + AIF events.")
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

(defn read-hud-json [path]
  (when (and path (.exists (io/file path)))
    (try
      (json/read-str (slurp path) :key-fn keyword)
      (catch Throwable _ nil))))

(defn now-inst []
  (java.util.Date.))

(defn ensure-session [lab-root session-id]
  (let [path (io/file lab-root "sessions" (str session-id ".edn"))]
    (if (.exists path)
      (pc/read-session-file (str path))
      {:session/id session-id
       :session/agent :fuclaude
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
    (let [path (io/file (:lab-root @state) "raw-stream" (str (:session-id @state) ".jsonl"))]
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

(defn build-psr [session-id turn candidates chosen]
  {:psr/id (str "psr-" turn)
   :session/id session-id
   :decision/id (str session-id ":turn-" turn)
   :candidates candidates
   :chosen chosen
   :context/anchors [(turn-anchor turn)]
   :forecast (build-forecast turn)
   :rejections (into {}
                     (for [p candidates :when (not= p chosen)]
                       [p {:codes [:reject/fit]
                           :note "Alternate pattern not selected."}]))
   :horizon :immediate})

(defn build-pur [session-id turn pattern-id]
  {:pur/id (str "pur-" turn)
   :session/id session-id
   :pattern/id pattern-id
   :instance/id (str "pur-" turn "-a")
   :decision/id (str session-id ":turn-" turn)
   :fields {:context "Captured at turn completion."
            :if "A decision was needed."
            :however "Fit only known after the turn."
            :then "Recorded the observed outcome."
            :because "Need traceability for decisions."
            :next-steps "Compare decision vs outcome."}
   :anchors [(turn-anchor turn)]
   :outcome/tags [:outcome/partial]})

(defn summary-event [session-id kind result]
  {:event/type :aif/summary
   :at (now-inst)
   :payload {:session/id session-id
             :aif/kind kind
             :aif/result result}})

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

;; Claude stream-json event types:
;; - init: session initialization (has session_id)
;; - message_start: start of assistant message
;; - content_block_start: start of content block (text, tool_use, etc.)
;; - content_block_delta: streaming content
;; - content_block_stop: end of content block
;; - message_delta: message metadata updates (stop_reason, usage)
;; - message_stop: end of message (turn complete)
;; - result: final result with session info

(defn extract-session-id [event]
  "Extract session ID from various Claude stream events."
  (or (:session_id event)
      (get-in event [:result :session_id])
      (get-in event [:message :id])))

(defn is-turn-complete? [event]
  "Check if this event signals turn completion."
  (or (= "message_stop" (:type event))
      (= "result" (:type event))
      (and (= "message_delta" (:type event))
           (get-in event [:delta :stop_reason]))))

(defn process-turn-complete! [state candidates chosen aif-config engine]
  "Process a completed turn - emit PSR/PUR and AIF events."
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
        selection-context (cond-> {:decision/id decision-id
                                   :session/id session-id
                                   :candidates candidates
                                   :anchors anchors
                                   :forecast forecast}
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
    (when current
      (append-event! state {:event/type :clock-in/start
                            :turn turn
                            :at (now-inst)
                            :clock-in/pattern-id current
                            :clock-in/intent "fuclaude live run"})
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
    (when (and (:proposal-hook? @state) (< (count candidates) 2))
      (append-event! state (proposal-draft session-id turn candidates)))
    (append-event! state (summary-event session-id :psr selection))
    (append-event! state (summary-event session-id :pur update))
    (when-let [tau-updated (get-in update [:aif :tau-updated])]
      (let [next-cache (assoc (:tau-cache @state) chosen-final tau-updated)]
        (swap! state assoc :tau-cache next-cache)
        (write-tau-cache! (:lab-root @state) next-cache)))))

(defn -main [& args]
  (let [{:keys [help unknown lab-root patterns chosen clock-in session-id aif-config aif-select proposal-hook hud-json]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        clock-ins (parse-csv clock-in)
        base-candidates (if (and patterns (not (str/blank? patterns)))
                          (vec (remove str/blank? (str/split patterns #",")))
                          ["ants/white-space-scout" "ants/pheromone-trail-tuner"])
        candidates (ensure-candidates base-candidates clock-ins)
        aif-cfg (read-aif-config aif-config)
        engine (engine/new-engine (fulab/new-adapter aif-cfg) {:beliefs {}})
        generated-session-id (or session-id (str (java.util.UUID/randomUUID)))
        hud-response (read-hud-json hud-json)
        hud-state (:hud hud-response)
        state (atom {:lab-root lab-root
                     :session-id generated-session-id
                     :session nil
                     :turn 0
                     :raw-path nil
                     :clock-in-queue clock-ins
                     :clock-in-current nil
                     :aif-config aif-cfg
                     :aif-config-logged? false
                     :hud hud-state
                     :hud-logged? false
                     :last-agent-report nil
                     :aif-select? (boolean aif-select)
                     :proposal-hook? (boolean proposal-hook)
                     :tau-cache (or (read-tau-cache lab-root) {})
                     :initialized? false})
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
        (doseq [line (line-seq (io/reader *in*))]
          ;; Echo the line to stdout for user visibility
          (println line)
          (flush)
          (let [event (parse-json line)]
            (when event
              ;; Initialize session on first event with session info
              (when-not (:initialized? @state)
                (when-let [sid (extract-session-id event)]
                  (let [session-id (or (:session-id @state) sid)
                        session (ensure-session lab-root session-id)
                        consumed (count-clock-ins session)
                        remaining (vec (drop consumed (:clock-in-queue @state)))]
                    (swap! state assoc :session-id session-id
                                     :session session
                                     :turn (last-turn session)
                                     :clock-in-queue remaining
                                     :initialized? true)
                    (ensure-raw-writer state)
                    (when (and (:hud @state) (not (:hud-logged? @state)))
                      (let [hud-id (get-in @state [:hud :hud/id])]
                        (append-event! state {:event/type :hud/initialized
                                              :hud/id hud-id
                                              :at (now-inst)
                                              :payload (assoc (:hud @state)
                                                              :session/id session-id)}))
                      (swap! state assoc :hud-logged? true))
                    (when (and aif-cfg (not (:aif-config-logged? @state)))
                      (append-event! state {:event/type :aif/config
                                            :at (now-inst)
                                            :payload {:session/id session-id
                                                      :config aif-cfg}})
                      (swap! state assoc :aif-config-logged? true)))))
              ;; Write raw line if initialized
              (when (:initialized? @state)
                (write-raw-line! state line))
              ;; Capture text content for agent report parsing
              (when (and (= "content_block_delta" (:type event))
                         (= "text_delta" (get-in event [:delta :type])))
                (let [text (get-in event [:delta :text])]
                  (when (string? text)
                    (swap! state update :accumulated-text (fnil str "") text))))
              ;; Parse agent report on message stop
              (when (= "message_stop" (:type event))
                (let [text (:accumulated-text @state)
                      report (when (string? text)
                               (hud/parse-agent-report text))]
                  (when report
                    (swap! state assoc :last-agent-report report))
                  (swap! state dissoc :accumulated-text)))
              ;; Handle turn completion
              (when (and (:initialized? @state) (is-turn-complete? event))
                (process-turn-complete! state candidates chosen aif-cfg engine)
                ;; Log agent report if present
                (when-let [report (:last-agent-report @state)]
                  (let [hud-id (get-in @state [:hud :hud/id])
                        session-id (:session-id @state)
                        turn (:turn @state)]
                    (append-event! state {:event/type :hud/agent-reported
                                          :hud/id hud-id
                                          :at (now-inst)
                                          :payload {:hud/id hud-id
                                                    :session/id session-id
                                                    :turn turn
                                                    :report report}}))
                  (swap! state assoc :last-agent-report nil))))))
        (remove-tap tap-listener)))))

(apply -main *command-line-args*)
