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
            [clojure.java.shell :as shell]
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
(def musn-session-id
  (let [sid (System/getenv "FUTON3_MUSN_SESSION_ID")]
    (when (and sid (not (str/blank? sid)))
      sid)))
(def require-approval? (not= "0" (or (System/getenv "FUTON3_MUSN_REQUIRE_APPROVAL") "1")))
(def musn-client-id (or (System/getenv "FUTON3_MUSN_CLIENT_ID") "fucodex"))
(def aif-config-path (System/getenv "FUTON3_MUSN_AIF_CONFIG_PATH"))

(defn session-id [session]
  (or (:session/id session)
      (:sid session)
      (get-in session [:session :id])))

(defn require-session-id [session]
  (let [sid (session-id session)]
    (when-not sid
      (throw (ex-info "missing musn session id" {:session session})))
    sid))

(defn read-aif-config [path]
  (when path
    (try
      (edn/read-string (slurp path))
      (catch Throwable _ nil))))

(def aif-config (read-aif-config aif-config-path))

(declare candidate-id)

(defn- repo-root []
  (or (some-> (System/getenv "FUTON3_REPO_ROOT") str/trim not-empty)
      (some-> (System/getenv "RUN_CWD") str/trim not-empty)
      (System/getProperty "user.dir")))

(defn- git-head [root]
  (try
    (let [{:keys [exit out]} (shell/sh "git" "-C" root "rev-parse" "HEAD")
          commit (str/trim out)]
      (when (and (zero? exit)
                 (re-matches #"(?i)[0-9a-f]{7,40}" commit))
        commit))
    (catch Throwable _ nil)))

(defn- build-certificates []
  (when-let [root (repo-root)]
    (when-let [commit (git-head root)]
      [{:certificate/type :git/commit
        :certificate/ref commit
        :certificate/repo root}])))

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
  (let [certs (build-certificates)
        policy (cond-> {}
                 aif-config (assoc :aif-config aif-config)
                 (seq certs) (assoc :certificates certs))
        payload (cond-> {:client {:id musn-client-id}}
                  musn-session-id (assoc :session/id musn-session-id)
                  (seq policy) (assoc :policy policy))
        resp (post! "/musn/session/create" payload)
        sid (session-id resp)
        session (cond-> resp sid (assoc :session/id sid))]
    (log! "[musn] session/create" session)
    session))

(defn musn-start [session turn hud]
  (let [sid (require-session-id session)]
    (post! "/musn/turn/start" {:session/id sid
                               :turn turn
                               :hud (cond-> (or hud {})
                                      musn-intent (assoc :intent musn-intent))})))

(defn musn-plan [session turn plan]
  (let [sid (require-session-id session)]
    (post! "/musn/turn/plan" {:session/id sid
                              :turn turn
                              :plan plan})))

(defn musn-select [session turn chosen reason reads]
  (let [sid (require-session-id session)
        chosen-id (candidate-id chosen)
        read-ids (->> reads (map candidate-id) (remove nil?) vec)
        candidates (if (seq read-ids)
                     (vec (distinct (cond-> read-ids chosen-id (conj chosen-id))))
                     (when chosen-id [chosen-id]))]
    (post! "/musn/turn/select" {:session/id sid
                                :turn turn
                                :candidates candidates
                                :chosen chosen-id
                                :reason (cond-> reason
                                          (seq read-ids) (assoc :reads read-ids))})))

(defn musn-action [session turn pattern-id action note files]
  (let [sid (require-session-id session)
        pid (candidate-id pattern-id)]
    (post! "/musn/turn/action" (cond-> {:session/id sid
                                        :turn turn
                                        :pattern/id pid
                                        :action action}
                                 note (assoc :note note)
                                 (seq files) (assoc :files files)))))

(defn musn-use [session turn pattern-id]
  (let [sid (require-session-id session)
        pid (candidate-id pattern-id)]
    (post! "/musn/turn/use" {:session/id sid :turn turn :pattern/id pid})))

(defn musn-evidence [session turn pattern-id files note]
  (let [sid (require-session-id session)
        pid (candidate-id pattern-id)]
    (post! "/musn/evidence/add" {:session/id sid
                                 :turn turn
                                 :pattern/id pid
                                 :files files
                                 :note note})))

(defn musn-end [session turn]
  (let [sid (require-session-id session)]
    (post! "/musn/turn/end" {:session/id sid :turn turn})))

(defn musn-resume [session turn note]
  (let [sid (require-session-id session)]
    (post! "/musn/turn/resume" {:session/id sid :turn turn :note note})))

(defn musn-state [session]
  (let [sid (require-session-id session)]
    (post! "/musn/session/state" {:session/id sid})))

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

(defn candidate-id [candidate]
  (cond
    (map? candidate) (recur (or (:id candidate) (:pattern/id candidate)))
    (keyword? candidate) (subs (str candidate) 1)
    (string? candidate) candidate
    (nil? candidate) nil
    :else (str candidate)))

(defn candidate-ids [candidates]
  (->> candidates (map candidate-id) (remove nil?) vec))

(defn normalize-score-keys [scores]
  (when (map? scores)
    (into {}
          (keep (fn [[k v]]
                  (when-let [id (candidate-id k)]
                    [id v])))
          scores)))

(defn log-aif-line! [label parts]
  (when (seq parts)
    (let [line (str "[aif]"
                    (when label (str " " label))
                    " "
                    (str/join " " parts))]
      (log! line)
      (println line)
      (flush))))

(defn log-warning! [label payload]
  (let [line (str "[musn-warning] " label
                  (when payload (str " " (pr-str payload))))]
    (log! line)
    (println line)
    (flush)))

(defn log-aif-missing! [label payload]
  (log-warning! (str "aif-missing:" label) payload))

(defn log-aif-selection! [aif]
  (let [g (fmt-num (:G aif))
        tau (fmt-num (:tau aif))
        g-scores (when (map? (:G-scores aif)) (count (:G-scores aif)))
        rejected (when (map? (:G-rejected aif)) (count (:G-rejected aif)))]
    (log-aif-line! "select"
                   (remove nil?
                           [(when g (str "G=" g))
                            (when tau (str "tau=" tau))
                            (when g-scores (str "G-scores=" g-scores))
                            (when rejected (str "G-rejected=" rejected))]))))

(defn log-aif-update! [aif]
  (let [err (fmt-num (:prediction-error aif))
        tau (fmt-num (:tau-updated aif))]
    (log-aif-line! "update"
                   (remove nil?
                           [(when err (str "err=" err))
                            (when tau (str "tau=" tau))]))))

(defn log-aif-tap! [event]
  (let [payload (or (:payload event) event)
        aif (:aif payload)
        kind (or (:event payload) (:aif/kind payload) (:type event))
        chosen (:chosen payload)
        tau (fmt-num (or (:tau aif) (:tau-updated aif)))
        g (fmt-num (:G-chosen aif))
        err (fmt-num (:prediction-error aif))]
    (log-aif-line! "tap"
                   (remove nil?
                           [(when kind (str "kind=" (if (keyword? kind) (name kind) kind)))
                            (when chosen (str "chosen=" chosen))
                            (when g (str "G=" g))
                            (when tau (str "tau=" tau))
                            (when err (str "err=" err))]))))

(defn- mana-balance [mana]
  (cond
    (number? (:balance mana)) (double (:balance mana))
    (number? (:budget mana)) (double (:budget mana))
    :else nil))

(defn- mana-label [mana]
  (when-let [balance (mana-balance mana)]
    (format "🔮:%.0f" balance)))

(defn log-mana-response! [state resp]
  (when-let [mana (:mana resp)]
    (swap! state assoc
           :mana mana
           :mana-label (mana-label mana))))

(defn- musn-prefix [state]
  (if-let [label (:mana-label @state)]
    (str "[musn " label "]")
    "[musn]"))

(defn log-musn! [state msg & more]
  (let [prefix (musn-prefix state)
        line (str prefix " " msg)]
    (if (seq more)
      (apply log! line more)
      (log! line))))

(defn- tap-event-id [event]
  (or (:at event)
      (get-in event [:payload :at])
      (get-in event [:payload :aif :belief-id])
      (hash event)))

(defn- log-latest-aif-tap!
  [state session]
  (when (and state session)
    (try
      (when-let [resp (musn-state session)]
        (when-let [tap (:aif/tap resp)]
          (let [tap-id (tap-event-id tap)
                last-id (:last-aif-tap @state)]
            (when (and tap-id (not= tap-id last-id))
              (swap! state assoc :last-aif-tap tap-id)
              (log-aif-tap! tap)))))
      (catch Throwable _ nil))))

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
    (let [line (format "[pattern-selection] chosen=%s candidates=%d mode=%s%s%s"
                       (:chosen psr)
                       (count (:candidates psr))
                       (or mode :unknown)
                       (if (seq reads)
                         (str " reads=" (str/join "," reads))
                         "")
                       (or aif-label ""))]
      (log! line)
      (println line))
    (flush)
    (when (seq aif)
      (log-aif-selection! aif))))

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
                    (str " outcome=" (str/join "," (map name tags))))
        line (format "[pattern-use] %s%s%s"
                     pattern-id
                     (or tag-label "")
                     (or aif-label ""))]
    (log! line)
    (println line)
    (flush))
  (when (seq aif)
    (log-aif-update! aif)))

(defn- note-selected! [state pattern-id]
  (swap! state update :selected-patterns (fnil conj #{}) pattern-id))

(defn- selected? [state pattern-id]
  (contains? (get @state :selected-patterns #{}) pattern-id))

(defn- ensure-selection!
  [state musn-session turn pattern-id reason reads]
  (let [pid (candidate-id pattern-id)]
    (when (and pid (not (selected? state pid)))
      (let [resp (musn-select musn-session turn pid reason reads)]
        (if (and resp (:ok resp))
          (do
            (note-selected! state pid)
            (when-let [psr (:psr resp)]
              (log-selection! psr))
            (if-let [aif (:aif resp)]
              (log-aif-selection! aif)
              (do
                (log-aif-missing! "turn/select"
                                  {:turn turn
                                   :chosen pid})
                (log-latest-aif-tap! state musn-session))))
          (log-warning! "select-failed"
                        {:turn turn
                         :chosen pid
                         :resp resp}))
        resp))))

(defn print-pause [resp]
  (when-let [pause (:pause resp)]
    (let [line (str "[MUSN-PAUSE] " (json/generate-string pause))]
      (log! line)
      (println line))
    (flush)))

(defn handle-event! [state event musn-session]
  (let [{:keys [turn]} @state]
    (cond
      ;; pattern-selection / pattern-action parsed from text payloads (RPC)
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "agent_message"))
      (let [text (get-in event [:item :text])]
        (doseq [line (str/split-lines (or text ""))]
          (when (plan-line? line)
            (musn-plan musn-session turn (str/trim line)))
          (if-let [payload (parse-json line)]
            (do
              (when-let [sel (parse-pattern-selection payload)]
                (let [chosen (:chosen sel)
                      resp (musn-select musn-session
                                        turn
                                        chosen
                                        {:mode (or (:mode sel) :use)
                                         :note "auto-read from selection event"}
                                        (:reads sel))]
                  (if (and resp (:ok resp) chosen)
                    (note-selected! state (candidate-id chosen))
                    (when (and chosen (not (:ok resp)))
                      (log-warning! "select-failed"
                                    {:turn turn
                                     :chosen chosen
                                     :resp resp})))
                  (when-let [psr (:psr resp)]
                    (log-selection! psr))
                  (if-let [aif (:aif resp)]
                    (log-aif-selection! aif)
                    (do
                      (log-aif-missing! "turn/select"
                                        {:turn turn
                                         :chosen chosen})
                      (log-latest-aif-tap! state musn-session))))
              (when-let [pa (parse-pattern-action payload)]
                (let [act (:action pa)
                      pid (:pattern/id pa)
                      files (:files pa)]
                  (when (#{"implement" "update"} act)
                    (ensure-selection! state musn-session turn pid
                                       {:mode :use
                                        :note "auto selection from pattern action"
                                        :source :auto}
                                       nil))
                  (let [action-resp (musn-action musn-session turn pid act (:note pa) files)]
                    (if-let [aif (:aif action-resp)]
                      (log-aif-update! aif)
                      (do
                        (log-aif-missing! "turn/action"
                                          {:turn turn
                                           :pattern/id (candidate-id pid)
                                           :action act})
                        (log-latest-aif-tap! state musn-session)))
                    (log-mana-response! state action-resp))
                  (when (#{"implement" "update"} act)
                    (let [use-resp (musn-use musn-session turn pid)]
                      (if-let [pur (:pur use-resp)]
                        (log-use! pur (:aif use-resp))
                        (do
                          (log-aif-missing! "turn/use"
                                            {:turn turn
                                             :pattern/id (candidate-id pid)
                                             :action act})
                          (log-latest-aif-tap! state musn-session)))
                      (log-mana-response! state use-resp)))
                  (when (seq files)
                    (musn-evidence musn-session turn pid files "auto evidence from pattern-action"))))))
            (when-let [pa (parse-pattern-action-line line)]
              (let [act (:action pa)
                    pid (:pattern/id pa)
                    files (:files pa)]
                (when (#{"implement" "update"} act)
                  (ensure-selection! state musn-session turn pid
                                     {:mode :use
                                      :note "auto selection from pattern action"
                                      :source :auto}
                                     nil))
                (let [action-resp (musn-action musn-session turn pid act (:note pa) files)]
                  (if-let [aif (:aif action-resp)]
                    (log-aif-update! aif)
                    (do
                      (log-aif-missing! "turn/action"
                                        {:turn turn
                                         :pattern/id (candidate-id pid)
                                         :action act})
                      (log-latest-aif-tap! state musn-session)))
                  (log-mana-response! state action-resp))
                (when (#{"implement" "update"} act)
                  (let [use-resp (musn-use musn-session turn pid)]
                    (if-let [pur (:pur use-resp)]
                      (log-use! pur (:aif use-resp))
                      (do
                        (log-aif-missing! "turn/use"
                                          {:turn turn
                                           :pattern/id (candidate-id pid)
                                           :action act})
                        (log-latest-aif-tap! state musn-session)))
                    (log-mana-response! state use-resp)))
                (when (seq files)
                  (musn-evidence musn-session turn pid files "auto evidence from pattern-action"))))))

      ;; command execution
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "command_execution"))
      (let [cmd (get-in event [:item :command])
            pats (patterns-from-command cmd)]
        (doseq [p pats]
          (let [action-resp (musn-action musn-session turn p "read" nil nil)]
            (if-let [aif (:aif action-resp)]
              (log-aif-update! aif)
              (do
                (log-aif-missing! "turn/action"
                                  {:turn turn
                                   :pattern/id (candidate-id p)
                                   :action "read"})
                (log-latest-aif-tap! state musn-session)))
            (log-mana-response! state action-resp))))

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
          (let [pid (candidate-id p)]
            (when (and pid (not (selected? state pid)))
              (log-warning! "pattern-edit-without-selection"
                            {:pattern/id pid
                             :turn turn
                             :files (vec (take 3 files))
                             :file-count (count files)}))
            (ensure-selection! state musn-session turn pid
                               {:mode :use
                                :note "auto selection from pattern file change"
                                :source :auto}
                               nil))
          (let [action-resp (musn-action musn-session turn p "update" "auto file_change" files)]
            (if-let [aif (:aif action-resp)]
              (log-aif-update! aif)
              (do
                (log-aif-missing! "turn/action"
                                  {:turn turn
                                   :pattern/id (candidate-id p)
                                   :action "update"})
                (log-latest-aif-tap! state musn-session)))
            (log-mana-response! state action-resp))
          (when (seq files)
            (musn-evidence musn-session turn p files "auto evidence from change"))))

      :else nil))))

(defn stream-loop []
  (let [tap-listener (fn [event]
                       (when (= :aif/fulab (:type event))
                         (log! "[aif-tap]" event)
                         (log-aif-tap! event)))]
    (add-tap tap-listener)
    (try
      (let [session (musn-create-session)
            state (atom {:turn 0
                         :paused? false
                         :pause-latched? false
                         :selected-patterns #{}
                         :last-aif-tap nil})
            sid (require-session-id session)]
      (log-musn! state "stream-loop start")
        (let [line (format "[musn-session] %s" sid)]
          (log! line)
          (println line))
        (flush)
        (doseq [line (line-seq (io/reader *in*))]
          (when-let [event (parse-json line)]
          (log-musn! state "event" event)
            (cond
            (= (:type event) "turn.started")
            (let [t (or (:turn event) (inc (:turn @state)) 1)]
              (swap! state assoc
                     :turn t
                     :selected-patterns #{})
              (let [hud-map (try
                              (when musn-intent
                                (hud/build-hud {:intent musn-intent
                                                :pattern-limit 8
                                                :namespaces ["musn" "fulab" "aif" "agent"]}))
                              (catch Throwable e
                                (log-musn! state "hud-build error" (.getMessage e))
                                (println (format "[hud-build-error] %s" (.getMessage e)))
                                (flush)
                                nil))
                    full-candidates (vec (or (seq (:candidates hud-map)) []))
                    candidates-present? (contains? hud-map :candidates)
                    raw-candidates (if candidates-present?
                                     (:candidates hud-map)
                                     (:prototypes hud-map))
                    seed-candidates (vec (or raw-candidates []))
                    candidate-ids (candidate-ids seed-candidates)
                    raw-scores (get-in hud-map [:aif :G-scores])
                    scores (normalize-score-keys raw-scores)
                    scores (when (seq candidate-ids)
                             (select-keys scores candidate-ids))
                    hud-payload (cond-> {:intent musn-intent
                                         :candidates candidate-ids}
                                  (map? scores) (assoc :scores scores)
                                  (seq full-candidates) (assoc :candidate-details full-candidates)
                                  (:sigils hud-map) (assoc :sigils (:sigils hud-map))
                                  (:namespaces hud-map) (assoc :namespaces (:namespaces hud-map))
                                  (:aif hud-map) (assoc :aif (:aif hud-map)))]
                (let [start-resp (musn-start session t hud-payload)]
                  (if-let [aif (:aif start-resp)]
                    (log-aif-selection! aif)
                    (when (seq candidate-ids)
                      (log-aif-missing! "turn/start"
                                        {:turn t
                                         :candidates (count candidate-ids)})
                      (log-latest-aif-tap! state session)))
                  (log-mana-response! state start-resp))
                (if (seq candidate-ids)
                  (let [line (format "[hud-candidates] %s" (str/join ", " candidate-ids))]
                    (log! line)
                    (println line))
                  (let [line "[hud-candidates] none"]
                    (log! line)
                    (println line)))
                (when-let [sigils (:sigils hud-map)]
                  (let [line (format "[hud-sigils] %s" sigils)]
                    (log! line)
                    (println line)))
                (flush))
                (when musn-intent
                  ;; announce intent into the stream so HUD can pick it up as handshake
                  (log! (format "[hud-intent] %s" musn-intent))
                  (println (format "[hud-intent] %s" musn-intent))
                  (flush)
                  (when require-approval?
                    (let [line "[musn-pause] intent computed; waiting for approval to proceed"]
                      (log! line)
                      (println line))
                    (flush)
                    ;; Block until a resume note shows up in session state.
                    (loop []
                      (Thread/sleep 1000)
                      (when-let [st (musn-state session)]
                        (let [note (get-in st [:state :resume-note])]
                          (when note
                            (let [line (format "[musn-resume] note=%s" note)]
                              (log! line)
                              (println line))
                            (flush)
                            (swap! state assoc :resume-note note :paused? false :pause-latched? false))
                        (when-not note
                          (recur)))))))))

            (= (:type event) "turn.completed")
            (let [resp (musn-end session (:turn @state))
                  paused? (:halt? resp)]
              (log-mana-response! state resp)
              (if paused?
                (when-not (:pause-latched? @state)
                  (swap! state assoc :paused? true :pause-latched? true)
                  (log! "[pattern] pause-latch" {:state :latched :turn (:turn @state)})
                  (print-pause resp)
                  (remove-tap tap-listener)
                  (System/exit 3))
                (when (or (:paused? @state) (:pause-latched? @state))
                  (swap! state assoc :paused? false :pause-latched? false)
                  (log! "[pattern] pause-latch" {:state :reset :turn (:turn @state)}))))

            :else
            (handle-event! state event session))))
      (remove-tap tap-listener)
      (System/exit 0)
      (catch Throwable t
        (remove-tap tap-listener)
        (binding [*out* *err*]
          (println "[musn-stream] ERROR" (.getMessage t))
          (when-let [d (ex-data t)] (prn d)))
        (log! "[musn-stream] ERROR" (.getMessage t) (ex-data t))
        (System/exit 4)))))

(defn -main [& _]
  (stream-loop))

;; Invoke main when run as a script (clojure -M dev/musn_stream.clj)
(apply -main *command-line-args*)
