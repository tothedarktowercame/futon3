(ns musn-stream
  "Translate Codex --json stream into MUSN turn lifecycle calls.
   - turn.started/turn.completed -> MUSN turn/start, turn/end
   - plan lines -> MUSN turn/plan
   - pattern-action / pattern-selection events -> MUSN turn/select, turn/action, turn/use, evidence/add
   - command/file fallback for pattern detection
   - pause/halts are printed and exit with code 3 for resume.

   Assumes PATTERN_ACTION_RPC is on so pattern-ids are detectable in events.
   Uses FUTON3_MUSN_URL for the MUSN service."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [futon3.fulab.hud :as hud]))

(def debug-log "/tmp/musn_stream.log")

(defn log! [msg & more]
  (when debug-log
    (spit debug-log (str (java.time.Instant/now) " " msg
                         (when (seq more) (str " " (pr-str more)))
                         "\n")
          :append true)))

(def musn-url (or (System/getenv "FUTON3_MUSN_URL") "http://localhost:6065"))
(def musn-intent (System/getenv "FUTON3_MUSN_INTENT"))
(def require-approval? (not= "0" (or (System/getenv "FUTON3_MUSN_REQUIRE_APPROVAL") "1")))
(def musn-client-id (or (System/getenv "FUTON3_MUSN_CLIENT_ID") "fucodex"))
(def aif-config-path (System/getenv "FUTON3_MUSN_AIF_CONFIG_PATH"))

(defn read-aif-config [path]
  (when path
    (try
      (edn/read-string (slurp path))
      (catch Throwable _ nil))))

(def aif-config (read-aif-config aif-config-path))

(defn parse-json [line]
  (try (json/parse-string line true)
       (catch Throwable _ nil)))

(defn post! [path payload]
  (let [url (str (str/replace musn-url #"/+$" "") path)
        resp (http/post url {:content-type :json
                             :accept :json
                             :throw-exceptions false
                             :body (json/generate-string payload)})
        body (some-> resp :body (json/parse-string true))]
    (if (= 200 (:status resp))
      body
      (throw (ex-info "musn http error" {:status (:status resp)
                                         :body body
                                         :url url
                                         :payload payload})))))

(defn musn-create-session []
  (let [policy (cond-> {}
                 aif-config (assoc :aif-config aif-config))
        payload (cond-> {:client {:id musn-client-id}}
                  (seq policy) (assoc :policy policy))
        resp (post! "/musn/session/create" payload)]
    (log! "[musn] session/create" resp)
    resp))

(defn musn-start [{:keys [session/id]} turn hud]
  (post! "/musn/turn/start" {:session/id id
                             :turn turn
                             :hud (cond-> (or hud {})
                                    musn-intent (assoc :intent musn-intent))}))

(defn musn-plan [{:keys [session/id]} turn plan]
  (post! "/musn/turn/plan" {:session/id id
                            :turn turn
                            :plan plan}))

(defn musn-select [{:keys [session/id]} turn chosen reason reads]
  (post! "/musn/turn/select" {:session/id id
                              :turn turn
                              :candidates (if (seq reads) (vec (distinct (conj reads chosen))) [chosen])
                              :chosen chosen
                              :reason (cond-> reason
                                        (seq reads) (assoc :reads reads))}))

(defn musn-action [{:keys [session/id]} turn pattern-id action note files]
  (post! "/musn/turn/action" (cond-> {:session/id id
                                      :turn turn
                                      :pattern/id pattern-id
                                      :action action}
                               note (assoc :note note)
                               (seq files) (assoc :files files))))

(defn musn-use [{:keys [session/id]} turn pattern-id]
  (post! "/musn/turn/use" {:session/id id :turn turn :pattern/id pattern-id}))

(defn musn-evidence [{:keys [session/id]} turn pattern-id files note]
  (post! "/musn/evidence/add" {:session/id id
                               :turn turn
                               :pattern/id pattern-id
                               :files files
                               :note note}))

(defn musn-end [{:keys [session/id]} turn]
  (post! "/musn/turn/end" {:session/id id :turn turn}))

(defn musn-resume [{:keys [session/id]} turn note]
  (post! "/musn/turn/resume" {:session/id id :turn turn :note note}))

(defn musn-state [{:keys [session/id]}]
  (post! "/musn/session/state" {:session/id id}))

(defn plan-line? [text]
  (and (string? text)
       (re-find #"(?i)^(?:\s*\[plan\]\s+|\s*plan\s*:)" text)))

(defn patterns-from-command [cmd]
  (->> (re-seq #"(?:^|\s)([a-z0-9._-]+/[a-z0-9._-]+)" (or cmd ""))
       (map second)
       distinct
       vec))

(defn parse-pattern-action [payload]
  (when (= "pattern-action" (:type payload))
    {:pattern/id (:pattern-id payload)
     :action (:action payload)
     :note (:note payload)
     :files (:files payload)}))

(defn parse-pattern-action-line [line]
  ;; Fallback for plain text pattern-action lines: "[pattern-action] update fulab/clock-in - note files=path1,path2"
  (when-let [m (re-find #"\[pattern-action\]\s+(\S+)\s+(\S+)(?:\s+-\s+([^\\f\\r\\n]+))?" line)]
    (let [action (nth m 1 nil)
          pid (nth m 2 nil)
          note (nth m 3 nil)
          files (when note
                  (when-let [fm (re-find #"files=([A-Za-z0-9_./,-]+)" note)]
                    (-> (second fm)
                        (str/split #",")
                        (map str/trim)
                        (remove str/blank?)
                        vec)))]
      {:pattern/id pid
       :action action
       :note note
       :files files})))

(defn parse-pattern-selection [payload]
  (when (= "pattern-selection" (:type payload))
    {:chosen (:chosen payload)
     :candidates (:candidates payload)
     :mode (:mode payload)
     :reads (some-> (:reads payload) (str/split #","))}))

(defn files-from-change [changes]
  (->> changes (map :path) (remove nil?) vec))

(defn fmt-num [value]
  (when (number? value)
    (format "%.3f" (double value))))

(defn log-selection! [psr]
  (let [reason (:selection/reason psr)
        mode (:mode reason)
        reads (:reads reason)
        aif (:aif reason)
        g (fmt-num (:G aif))
        tau (fmt-num (:tau aif))
        aif-label (when (or g tau)
                    (str " aif=" (str/join ","
                                          (remove nil?
                                                  [(when g (str "G=" g))
                                                   (when tau (str "tau=" tau))]))))]
    (println (format "[pattern-selection] chosen=%s candidates=%d mode=%s%s%s"
                     (:chosen psr)
                     (count (:candidates psr))
                     (or mode :unknown)
                     (if (seq reads)
                       (str " reads=" (str/join "," reads))
                       "")
                     (or aif-label "")))
    (flush)))

(defn log-use! [pur aif]
  (let [pattern-id (:pattern/id pur)
        tags (:outcome/tags pur)
        tau (fmt-num (:tau-updated aif))
        err (fmt-num (:prediction-error aif))
        aif-label (when (or tau err)
                    (str " aif="
                         (str/join ","
                                   (remove nil?
                                           [(when err (str "err=" err))
                                            (when tau (str "tau=" tau))]))))
        tag-label (when (seq tags)
                    (str " outcome=" (str/join "," (map name tags))))]
    (println (format "[pattern-use] %s%s%s"
                     pattern-id
                     (or tag-label "")
                     (or aif-label "")))
    (flush)))

(defn print-pause [resp]
  (when-let [pause (:pause resp)]
    (println "[MUSN-PAUSE]" (json/generate-string pause))
    (flush)))

(defn handle-event! [state event musn-session]
  (let [{:keys [turn]} @state]
    (cond
      ;; pattern-selection / pattern-action parsed from text payloads (RPC)
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "agent_message"))
      (let [text (get-in event [:item :text])]
        (when (plan-line? text)
          (musn-plan musn-session turn text))
        (doseq [line (str/split-lines (or text ""))]
          (if-let [payload (parse-json line)]
            (do
              (when-let [sel (parse-pattern-selection payload)]
                (let [resp (musn-select musn-session
                                        turn
                                        (:chosen sel)
                                        {:mode (or (:mode sel) :use)
                                         :note "auto-read from selection event"}
                                        (:reads sel))]
                  (when-let [psr (:psr resp)]
                    (log-selection! psr))))
              (when-let [pa (parse-pattern-action payload)]
                (let [act (:action pa)
                      pid (:pattern/id pa)
                      files (:files pa)]
                  (musn-action musn-session turn pid act (:note pa) files)
                  (when (#{"implement" "update"} act)
                    (let [use-resp (musn-use musn-session turn pid)]
                      (when-let [pur (:pur use-resp)]
                        (log-use! pur (:aif use-resp))))
                    (when (seq files)
                      (musn-evidence musn-session turn pid files "auto evidence from pattern-action"))))))
            (when-let [pa (parse-pattern-action-line line)]
              (let [act (:action pa)
                    pid (:pattern/id pa)
                    files (:files pa)]
                (musn-action musn-session turn pid act (:note pa) files)
                (when (#{"implement" "update"} act)
                  (let [use-resp (musn-use musn-session turn pid)]
                    (when-let [pur (:pur use-resp)]
                      (log-use! pur (:aif use-resp))))
                  (when (seq files)
                    (musn-evidence musn-session turn pid files "auto evidence from pattern-action"))))))))

      ;; command execution
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "command_execution"))
      (let [cmd (get-in event [:item :command])
            pats (patterns-from-command cmd)]
        (doseq [p pats]
          (musn-action musn-session turn p "read" nil nil)))

      ;; file changes
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "file_change"))
      (let [changes (get-in event [:item :changes])
            files (files-from-change changes)
            pats (->> changes
                      (map :path)
                      (keep #(re-find #"(?:^|/)([a-z0-9._-]+/[a-z0-9._-]+)\.(flexiarg|multiarg)$" %))
                      (map second)
                      distinct
                      vec)]
        (doseq [p pats]
          (musn-action musn-session turn p "update" "auto file_change" files)
          (when (seq files)
            (musn-evidence musn-session turn p files "auto evidence from change"))))

      :else nil)))

(defn stream-loop []
  (try
    (let [session (musn-create-session)
          state (atom {:turn 0})]
      (log! "[musn] stream-loop start")
      (when-let [sid (:session/id session)]
        (println (format "[musn-session] %s" sid))
        (flush))
      (doseq [line (line-seq (io/reader *in*))]
        (when-let [event (parse-json line)]
          (log! "[musn] event" event)
          (cond
            (= (:type event) "turn.started")
            (let [t (or (:turn event) (inc (:turn @state)) 1)]
              (swap! state assoc :turn t)
              (let [hud-map (try
                              (when musn-intent
                                (hud/build-hud {:intent musn-intent
                                                :pattern-limit 8
                                                :namespaces ["musn" "fulab" "aif" "agent"]}))
                              (catch Throwable e
                                (log! "[musn] hud-build error" (.getMessage e))
                                (println (format "[hud-build-error] %s" (.getMessage e)))
                                (flush)
                                nil))
                    candidates (vec (or (:prototypes hud-map)
                                        (:candidates hud-map)
                                        []))
                    hud-payload (cond-> {:intent musn-intent
                                         :candidates candidates}
                                  (:sigils hud-map) (assoc :sigils (:sigils hud-map))
                                  (:namespaces hud-map) (assoc :namespaces (:namespaces hud-map))
                                  (:aif hud-map) (assoc :aif (:aif hud-map)))]
                (musn-start session t hud-payload)
                (if (seq candidates)
                  (println (format "[hud-candidates] %s" (str/join ", " candidates)))
                  (println "[hud-candidates] none"))
                (when-let [sigils (:sigils hud-map)]
                  (println (format "[hud-sigils] %s" sigils)))
                (flush))
              (when musn-intent
                ;; announce intent into the stream so HUD can pick it up as handshake
                (log! (format "[hud-intent] %s" musn-intent))
                (println (format "[hud-intent] %s" musn-intent))
                (flush)
                (when require-approval?
                  (println "[musn-pause] intent computed; waiting for approval to proceed")
                  (flush)
                  ;; Block until a resume note shows up in session state.
                  (loop []
                    (Thread/sleep 1000)
                    (when-let [st (musn-state session)]
                      (let [note (get-in st [:state :resume-note])]
                        (when note
                          (println (format "[musn-resume] note=%s" note))
                          (flush)
                          (swap! state assoc :resume-note note))
                        (when-not note
                          (recur)))))))))

            (= (:type event) "turn.completed")
            (let [resp (musn-end session (:turn @state))]
              (when (:halt? resp)
                (print-pause resp)))

            :else
            (handle-event! state event session))))
    (System/exit 0)
    (catch Throwable t
      (binding [*out* *err*]
        (println "[musn-stream] ERROR" (.getMessage t))
        (when-let [d (ex-data t)] (prn d)))
      (log! "[musn-stream] ERROR" (.getMessage t) (ex-data t))
      (System/exit 4))))

(defn -main [& _]
  (stream-loop))

;; Invoke main when run as a script (clojure -M dev/musn_stream.clj)
(apply -main *command-line-args*)
