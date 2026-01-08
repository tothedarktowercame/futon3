(ns lab-stream-codex
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.engine :as engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-stream-codex.clj [--lab-root PATH] [--patterns CSV] [--chosen ID] [--clock-in CSV] [--session-id ID] [--aif-config PATH] [--aif-select] [--proposal-hook]")
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
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn parse-json [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Throwable _ nil)))

(defn now-inst []
  (java.util.Date.))

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
  (or (->> (:events session)
           (filter #(= :turn/completed (:event/type %)))
           (map :turn)
           (remove nil?)
           (apply max))
      0))

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
  (let [{:keys [help unknown lab-root patterns chosen clock-in session-id aif-config aif-select proposal-hook]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        clock-ins (parse-csv clock-in)
        base-candidates (if (and patterns (not (str/blank? patterns)))
                          (vec (remove str/blank? (str/split patterns #",")))
                          ["ants/white-space-scout" "ants/pheromone-trail-tuner"])
        candidates (ensure-candidates base-candidates clock-ins)
        aif-config (read-aif-config aif-config)
        engine (engine/new-engine (fulab/new-adapter aif-config) {:beliefs {}})
        state (atom {:lab-root lab-root
                     :thread-id nil
                     :session-id session-id
                     :session nil
                     :turn 0
                     :raw-path nil
                     :clock-in-queue clock-ins
                     :clock-in-current nil
                     :aif-config aif-config
                     :aif-config-logged? false
                     :aif-select? (boolean aif-select)
                     :proposal-hook? (boolean proposal-hook)
                     :tau-cache (or (read-tau-cache lab-root) {})})
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
          (let [event (parse-json line)]
            (when event
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
                    (when (and (:aif-config @state) (not (:aif-config-logged? @state)))
                      (append-event! state {:event/type :aif/config
                                            :at (now-inst)
                                            :payload {:session/id session-id
                                                      :config (:aif-config @state)}})
                      (swap! state assoc :aif-config-logged? true))))))
            (when (:thread-id @state)
              (write-raw-line! state line))
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
                (when (and (:proposal-hook? @state) (< (count candidates) 2))
                  (append-event! state (proposal-draft session-id turn candidates)))
                (append-event! state (summary-event session-id :psr selection))
                (append-event! state (summary-event session-id :pur update))
                (when-let [tau-updated (get-in update [:aif :tau-updated])]
                  (let [next-cache (assoc (:tau-cache @state) chosen-final tau-updated)]
                    (swap! state assoc :tau-cache next-cache)
                    (write-tau-cache! lab-root next-cache)))))))
        (remove-tap tap-listener)))))
(apply -main *command-line-args*)
