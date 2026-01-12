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
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as http]))

(def musn-url (or (System/getenv "FUTON3_MUSN_URL") "http://localhost:6065"))

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
  (post! "/musn/session/create" {}))

(defn musn-start [{:keys [session/id]} turn hud]
  (post! "/musn/turn/start" {:session/id id
                             :turn turn
                             :hud hud}))

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

(defn parse-pattern-selection [payload]
  (when (= "pattern-selection" (:type payload))
    {:chosen (:chosen payload)
     :candidates (:candidates payload)
     :mode (:mode payload)
     :reads (some-> (:reads payload) (str/split #","))}))

(defn files-from-change [changes]
  (->> changes (map :path) (remove nil?) vec))

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
          (when-let [payload (parse-json line)]
            (when-let [sel (parse-pattern-selection payload)]
              (musn-select musn-session
                           turn
                           (:chosen sel)
                           {:mode (or (:mode sel) :use)
                            :note "auto-read from selection event"}
                           (:reads sel))))
            (when-let [pa (parse-pattern-action payload)]
              (let [act (:action pa)
                    pid (:pattern/id pa)
                    files (:files pa)]
                (musn-action musn-session turn pid act (:note pa) files)
                (when (#{"implement" "update"} act)
                  (musn-use musn-session turn pid)
                  (when (seq files)
                    (musn-evidence musn-session turn pid files "auto evidence from pattern-action")))))))) 

      ;; plan text
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "agent_message"))
      nil

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
            (musn-evidence musn-session turn p files "auto evidence from change")))))
    nil))

(defn stream-loop []
  (let [session (musn-create-session)
        state (atom {:turn 0})]
    (doseq [line (line-seq (io/reader *in*))]
      (let [event (parse-json line)]
        (when event
          (cond
            (= (:type event) "turn.started")
            (do (swap! state assoc :turn (:turn event))
                (musn-start session (:turn event) {:candidates []}))

            (= (:type event) "turn.completed")
            (let [resp (musn-end session (:turn @state))]
              (when (:halt? resp)
                (print-pause resp)
                (System/exit 3)))

            :else
            (handle-event! state event session))))))
  (System/exit 0))

(defn -main [& _] (stream-loop))
