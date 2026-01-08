(ns lab-stream-codex
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.engine :as engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-stream-codex.clj [--lab-root PATH] [--patterns CSV] [--chosen ID] [--clock-in CSV] [--session-id ID] [--aif-config PATH]")
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
   :forecast {:benefits [{:tag :benefit/traceable-choice
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
                         :note "Decision does not explain outcome."}]}
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
  (let [{:keys [help unknown lab-root patterns chosen clock-in session-id aif-config]} (parse-args args)
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
                     :aif-config-logged? false})
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
                    chosen (or current chosen (first candidates))
                    psr (build-psr session-id turn candidates chosen)
                    pur (build-pur session-id turn chosen)
                    selection (engine/select-pattern engine {:decision/id (:decision/id psr)
                                                             :session/id session-id
                                                             :candidates (:candidates psr)
                                                             :chosen (:chosen psr)
                                                             :anchors (:context/anchors psr)
                                                             :forecast (:forecast psr)})
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
                (append-event! state (summary-event session-id :psr selection))
                (append-event! state (summary-event session-id :pur update))))))
        (remove-tap tap-listener)))))
(apply -main *command-line-args*)
(defn read-aif-config [path]
  (when path
    (try
      (edn/read-string (slurp path))
      (catch Throwable _ nil))))
