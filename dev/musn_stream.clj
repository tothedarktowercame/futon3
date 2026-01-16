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
(def musn-prompt (System/getenv "FUTON3_MUSN_PROMPT"))
(def musn-session-id
  (let [sid (System/getenv "FUTON3_MUSN_SESSION_ID")]
    (when (and sid (not (str/blank? sid)))
      sid)))
(def hud-json-path (some-> (System/getenv "FUTON3_HUD_JSON") str/trim not-empty))
(def require-approval? (not= "0" (or (System/getenv "FUTON3_MUSN_REQUIRE_APPROVAL") "1")))
(def musn-client-id (or (System/getenv "FUTON3_MUSN_CLIENT_ID") "fucodex"))
(def aif-config-path (System/getenv "FUTON3_MUSN_AIF_CONFIG_PATH"))

(defn- strip-hud-block [prompt]
  (when (and prompt (string? prompt))
    (let [lines (str/split-lines prompt)]
      (->> lines
           (reduce (fn [{:keys [acc in-block?]} line]
                     (cond
                       (re-find #"^\\s*\\[FULAB-HUD\\]\\s*$" line)
                       {:acc acc :in-block? true}
                       (re-find #"^\\s*\\[/FULAB-HUD\\]\\s*$" line)
                       {:acc acc :in-block? false}
                       in-block? {:acc acc :in-block? in-block?}
                       :else {:acc (conj acc line) :in-block? in-block?}))
                   {:acc [] :in-block? false})
           :acc
           (str/join "\n")))))

(defn- extract-intent-from-prompt [prompt]
  (when (and prompt (string? prompt))
    (let [prompt (or (strip-hud-block prompt) prompt)
          lines (str/split-lines prompt)
          start (first (keep-indexed (fn [idx line]
                                       (when (re-find #"^\\s*(User task|Task|Intent)\\s*:" line)
                                         idx))
                                     lines))]
      (if (some? start)
        (let [first-line (str/trim (str/replace (nth lines start)
                                                #"^\\s*(User task|Task|Intent)\\s*:\\s*" ""))]
          (loop [idx (inc start)
                 acc (cond-> [] (not (str/blank? first-line)) (conj first-line))]
            (if (and (< idx (count lines))
                     (let [line (nth lines idx)]
                       (not (str/blank? (str/trim line)))))
              (recur (inc idx) (conj acc (str/trim (nth lines idx))))
              (str/join " " acc))))
        (some->> lines
                 (drop-while #(str/blank? (str/trim %)))
                 first
                 str/trim)))))

(defn- summarize-intent [text]
  (when (and text (string? text))
    (let [line (or (first (str/split-lines text)) text)
          cleaned (-> line
                      (str/replace #"[`*_]+" "")
                      (str/replace #"\s+" " ")
                      str/trim)
          cleaned (str/replace cleaned #"(?i)^\s*(please|pls)\s+" "")
          seps ["—" " -- " " - " ". " "? " "! "]
          summary (reduce (fn [s sep]
                            (if-let [idx (str/index-of s sep)]
                              (subs s 0 idx)
                              s))
                          cleaned
                          seps)
          summary (-> summary
                      str/trim
                      (str/replace "/home/joe/code/futon3/" "")
                      (str/replace "/home/joe/code/" ""))]
      (when-not (str/blank? summary)
        summary))))

(defn- compute-intent [prompt]
  (let [candidate (or (extract-intent-from-prompt prompt) prompt)]
    (summarize-intent candidate)))

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

(defn- read-hud-json [path]
  (when (and path (.exists (io/file path)))
    (try
      (let [data (json/parse-string (slurp path) true)
            hud (or (:hud data) data)]
        (when (map? hud)
          hud))
      (catch Throwable _ nil))))

(def aif-config (read-aif-config aif-config-path))

(declare candidate-id)
(declare log-warning!)
(declare log-aif-line!)
(declare fmt-num)
(declare note-selected!)
(declare log-selection!)
(declare candidate-detail)
(declare log-use!)
(declare ensure-selection!)
(declare log-aif-missing!)
(declare log-latest-aif-tap!)
(declare tap-event-id)
(declare log-mana-response!)
(declare wait-for-resume!)

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
                             :headers {"x-musn-client" musn-client-id
                                       "user-agent" (str "musn-stream/" musn-client-id)}
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
                               :hud (or hud {})})))

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
    (if (and chosen-id (seq candidates) (map? reason))
      (post! "/musn/turn/select" {:session/id sid
                                  :turn turn
                                  :candidates candidates
                                  :chosen chosen-id
                                  :reason (cond-> reason
                                            (seq read-ids) (assoc :reads read-ids))})
      (do
        (log-warning! "select-missing-chosen"
                      {:turn turn
                       :chosen chosen
                       :candidates candidates
                       :reason reason})
        nil))))

(defn musn-action [session turn pattern-id action note files]
  (let [sid (require-session-id session)
        pid (candidate-id pattern-id)]
    (post! "/musn/turn/action" (cond-> {:session/id sid
                                        :turn turn
                                        :pattern/id pid
                                        :action action}
                                 note (assoc :note note)
                                 (seq files) (assoc :files files)))))

(defn musn-use
  ([session turn pattern-id]
   (musn-use session turn pattern-id nil))
  ([session turn pattern-id note]
   (let [sid (require-session-id session)
         pid (candidate-id pattern-id)]
     (post! "/musn/turn/use" (cond-> {:session/id sid :turn turn :pattern/id pid}
                               note (assoc :note note))))))

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

(def ^:private intent-line-re
  #"(?i)\b(?:hud-)?intent\s*:\s*([^\n]+)")

(defn- intent-line [text]
  (when (string? text)
    (let [line (str/trim text)
          match (re-find intent-line-re line)]
      (when-let [intent (some-> match second str/trim)]
        (when-not (str/blank? intent)
          intent)))))

(def ^:private plan-proxy-re
  #"(?i)^(?:\*+)?\s*plan(?:ning)?\b")

(def ^:private plan-proxy-max-len 180)

(defn- plan-proxy? [text]
  (when (string? text)
    (let [line (some-> text str/trim (str/split-lines) first)]
      (and (string? line)
           (re-find plan-proxy-re line)))))

(defn- plan-proxy-text [text]
  (let [line (some-> text str/trim (str/split-lines) first)
        line (-> (or line "")
                 (str/replace #"\*+" "")
                 (str/replace #"\s+" " ")
                 str/trim)]
    (cond
      (str/blank? line) nil
      (> (count line) plan-proxy-max-len) (subs line 0 plan-proxy-max-len)
      :else line)))

(def ^:private plan-required-actions
  #{"implement" "update" "off-trail" "wide-scan"})

(defn- normalize-action [action]
  (cond
    (keyword? action) (name action)
    (string? action) action
    (nil? action) nil
    :else (str action)))

(defn- plan-required? [action]
  (when-let [value (normalize-action action)]
    (contains? plan-required-actions value)))

(defn- note-plan-seen! [state text]
  (when (plan-line? text)
    (swap! state assoc :turn-plan? true)))

(defn- note-plan-proxy! [state musn-session turn text]
  (when (and (plan-proxy? text)
             (not (:turn-plan? @state)))
    (swap! state assoc :turn-plan? true :turn-plan-proxy? true)
    (when-let [proxy (plan-proxy-text text)]
      (musn-plan musn-session turn (str "Plan: " proxy)))))

(defn- note-intent-seen! [state musn-session intent]
  (when (and intent (not (:intent-seen? @state)))
    (swap! state assoc :intent-seen? true :intent intent)
    (let [line (format "[hud-intent] %s" intent)]
      (log! line)
      (println line)
      (flush))
    (when require-approval?
      (let [line "[musn-pause] intent received; waiting for approval to proceed"]
        (log! line)
        (println line)
        (flush))
      (wait-for-resume! state musn-session))))

(defn- maybe-warn-missing-plan! [state action]
  (when (and (plan-required? action)
             (not (:turn-plan? @state))
             (not (:turn-plan-warned? @state)))
    (swap! state assoc :turn-plan-warned? true)
    (log-warning! "missing-plan"
                  {:turn (:turn @state)
                   :action (normalize-action action)
                   :note "plan required before tool use; expected 'Plan:' or '[plan]' line"})
    (flush)))

(defn patterns-from-command [cmd]
  (->> (re-seq #"(?:^|\s)([a-z0-9._-]+/[a-z0-9._-]+)" (or cmd ""))
       (map second)
       distinct
       vec))

(defn- strip-wrapper-quotes [token]
  (-> token
      (str/replace #"^['\"]+" "")
      (str/replace #"['\"]+$" "")))

(defn- normalize-helper-token [token]
  (-> token
      strip-wrapper-quotes
      (str/replace #".*/scripts/" "")
      (str/replace #"^\.?/?scripts/" "")
      (str/replace #"^./" "")))

(defn- normalize-pattern-token [token]
  (let [clean (strip-wrapper-quotes token)]
    (if (str/starts-with? clean "library/")
      (subs clean (count "library/"))
      clean)))

(def ^:private note-path-re
  #"(?:^|\\s)([A-Za-z0-9_./-]+\\.[A-Za-z0-9_-]+)(?=$|\\s)")

(defn- files-from-note [note]
  (when (string? note)
    (->> (re-seq note-path-re note)
         (map second)
         (remove str/blank?)
         distinct
         vec)))

(defn- parse-helper-command [cmd]
  (when (string? cmd)
    (let [raw-tokens (->> (str/split (str/trim cmd) #"\s+")
                          (remove str/blank?)
                          vec)
          tokens (mapv normalize-helper-token raw-tokens)
          idx (first (keep-indexed (fn [i tok]
                                     (when (#{"pattern-select" "pattern-use" "musn-help"} tok)
                                       i))
                                   tokens))]
      (when idx
        (let [tool (keyword (nth tokens idx))]
          (case tool
            :musn-help
            (let [topic (some-> (get raw-tokens (inc idx))
                                strip-wrapper-quotes
                                str/trim)]
              {:tool tool
               :topic (or (not-empty topic) "tools")})
            (let [pattern-token (get raw-tokens (inc idx))
                  pattern-id (when-let [raw (some-> pattern-token normalize-pattern-token)]
                               (when (re-matches #"[a-z0-9._-]+/[a-z0-9._-]+" raw)
                                 raw))
                  note (when (> (count raw-tokens) (+ idx 2))
                         (->> (subvec raw-tokens (+ idx 2))
                              (str/join " ")
                              strip-wrapper-quotes
                              str/trim))]
              (when pattern-id
                {:tool tool
                 :pattern-id pattern-id
                 :note (when (seq note) note)}))))))))

(defn- parse-pattern-action-command [cmd]
  (when (string? cmd)
    (let [raw-tokens (->> (str/split (str/trim cmd) #"\s+")
                          (remove str/blank?)
                          vec)
          tokens (mapv normalize-helper-token raw-tokens)
          idx (first (keep-indexed (fn [i tok]
                                     (when (= "pattern-action" tok) i))
                                   tokens))]
      (when idx
        (let [action (get raw-tokens (inc idx))
              pattern-token (get raw-tokens (+ idx 2))
              pattern-id (when-let [raw (some-> pattern-token normalize-pattern-token)]
                           (when (re-matches #"[a-z0-9._-]+/[a-z0-9._-]+" raw)
                             raw))
              note (when (> (count raw-tokens) (+ idx 3))
                     (->> (subvec raw-tokens (+ idx 3))
                          (str/join " ")
                          strip-wrapper-quotes
                          str/trim))]
          (when (and action pattern-id)
            {:action (normalize-action action)
             :pattern-id pattern-id
             :note (when (seq note) note)}))))))

(defn- log-aif-policy! [decision]
  (when (map? decision)
    (log-aif-line! "policy"
                   (remove nil?
                           [(when-let [choice (:chosen decision)]
                              (str "chosen=" choice))
                            (when-let [tau (:tau decision)]
                              (str "tau=" (fmt-num tau)))
                            (when-let [mode (:mode decision)]
                              (str "mode=" (name mode)))
                            (when-let [reason (:reason decision)]
                              (str "reason=" reason))]))))

(def ^:private aif-auto-select-min-tau 0.55)

(defn- aif-policy-choice
  [state candidates]
  (let [policy (or (:aif-policy @state) {})
        tau (:tau policy)
        scores (:G-scores policy)
        suggested (:suggested policy)
        scored (when (seq candidates)
                 (->> candidates
                      (map (fn [pid]
                             {:id pid
                              :G (get scores pid Double/POSITIVE_INFINITY)}))
                      (sort-by :G)))
        best (some-> scored first :id)
        chosen (cond
                 (and suggested (some #{suggested} candidates)) suggested
                 best best
                 :else (first candidates))
        allow? (or (nil? tau) (>= (double tau) aif-auto-select-min-tau))]
    {:chosen (when allow? chosen)
     :tau tau
     :mode (if allow? :auto-select :abstain)
     :reason (if allow? "aif-policy" "aif-low-tau")}))

(defn- handle-helper-command!
  [state musn-session turn cmd]
  (when-let [{:keys [tool pattern-id note topic]} (parse-helper-command cmd)]
    (case tool
      :pattern-select
      (when-let [pid (candidate-id pattern-id)]
        (ensure-selection! state musn-session turn pid
                           (cond-> {:mode :read
                                    :source :command}
                             note (assoc :note note))
                           nil)
        (note-selected! state pid))
      :pattern-use
      (when-let [pid (candidate-id pattern-id)]
        (ensure-selection! state musn-session turn pid
                           (cond-> {:mode :use
                                    :source :command}
                             note (assoc :note note))
                           nil)
        (let [use-resp (musn-use musn-session turn pid note)]
          (if-let [pur (:pur use-resp)]
            (log-use! state pur (:aif use-resp))
            (do
              (log-aif-missing! "turn/use"
                                {:turn turn
                                 :pattern/id pid})
              (log-latest-aif-tap! state musn-session)))
          (when-let [files (seq (files-from-note note))]
            (musn-evidence musn-session turn pid files "auto evidence from pattern-use note"))
          (log-mana-response! state use-resp)))
      :musn-help
      (let [pid "musn/help"
            help-note (str "musn-help " (or topic "tools"))
            action-resp (musn-action musn-session turn pid "read" help-note nil)]
        (let [logged? (log-latest-aif-tap! state musn-session)]
          (when (and (not logged?) (not (:aif action-resp)))
            (log-aif-missing! "turn/action"
                              {:turn turn
                               :pattern/id pid
                               :action "read"})))
        (log-mana-response! state action-resp))
      nil)
    true))

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
                    (->> (str/split (second fm) #",")
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

(def ^:private pattern-id-re
  #"(?:^|/)([a-z0-9._-]+/[a-z0-9._-]+)\.(?:flexiarg|multiarg)$")

(def ^:private arg-line-re
  #"(?m)^\s*@(?:arg|flexiarg)\s+([a-z0-9._-]+/[a-z0-9._-]+)")

(defn- resolve-pattern-file [path]
  (let [file (io/file path)]
    (if (.isAbsolute file)
      file
      (io/file (repo-root) path))))

(defn- pattern-ids-from-file [path]
  (let [file (resolve-pattern-file path)]
    (when (.exists file)
      (try
        (->> (re-seq arg-line-re (slurp file))
             (map second)
             distinct
             vec)
        (catch Throwable _ nil)))))

(defn- pattern-ids-from-path [path]
  (when-let [m (re-find pattern-id-re path)]
    [(second m)]))

(defn- patterns-from-change-path [path]
  (let [ids (pattern-ids-from-file path)]
    (if (seq ids)
      ids
      (pattern-ids-from-path path))))

(defn fmt-num [value]
  (when (number? value)
    (format "%.3f" (double value))))

(def ^:private pattern-id-token-re
  #"[a-z0-9._-]+/[a-z0-9._-]+")

(defn- normalize-pattern-id [value]
  (let [s (str/trim (str value))]
    (when-not (str/blank? s)
      (or (re-find pattern-id-token-re s) s))))

(defn candidate-id [candidate]
  (cond
    (map? candidate) (recur (or (:id candidate) (:pattern/id candidate)))
    (keyword? candidate) (subs (str candidate) 1)
    (string? candidate) (normalize-pattern-id candidate)
    (nil? candidate) nil
    :else (normalize-pattern-id candidate)))

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

(defn- update-aif-policy! [state aif]
  (when (and state (map? aif))
    (swap! state update :aif-policy
           (fn [policy]
             (let [policy (or policy {})
                   tau (or (:tau aif) (:tau-updated aif) (:tau policy))
                   patch (cond-> {}
                           (map? (:G-scores aif)) (assoc :G-scores (:G-scores aif))
                           (contains? aif :suggested) (assoc :suggested (:suggested aif))
                           (contains? aif :white-space?) (assoc :white-space? (:white-space? aif))
                           (contains? aif :rationale) (assoc :rationale (:rationale aif))
                           (number? tau) (assoc :tau (double tau)))]
               (merge policy patch))))))

(defn log-aif-selection!
  ([aif] (log-aif-selection! nil aif))
  ([state aif]
   (update-aif-policy! state aif)
   (let [g-chosen (or (:G aif) (:G-chosen aif))
         g-values (->> (concat (when (number? g-chosen) [g-chosen])
                               (vals (or (:G-rejected aif) {}))
                               (vals (or (:G-scores aif) {})))
                       (filter number?))
         g-min (when (seq g-values) (apply min g-values))
         g-max (when (seq g-values) (apply max g-values))
         g-span (when (and (number? g-min) (number? g-max))
                  (- (double g-max) (double g-min)))
         g (fmt-num g-chosen)
         g-span (fmt-num g-span)
         g-count (when (seq g-values) (count g-values))
         tau (fmt-num (:tau aif))
         white-space? (:white-space? aif)]
     (log-aif-line! "select"
                    (remove nil?
                            [(when g (str "G=" g))
                             (when g-span (str "span=" g-span))
                             (when g-count (str "n=" g-count))
                             (when tau (str "tau=" tau))
                             (when white-space? "white-space")])))))

(defn log-aif-update!
  ([aif] (log-aif-update! nil aif))
  ([state aif]
   (update-aif-policy! state aif)
   (let [delta-map (:belief-delta aif)
         pid (:pattern/id delta-map)
         action (:action delta-map)
         err (fmt-num (:prediction-error aif))
         tau (fmt-num (:tau-updated aif))
         ev (fmt-num (:evidence-score aif))
         delta (fmt-num (:evidence-delta aif))
         counts (:evidence-counts aif)
         counts (when (map? counts)
                  (->> counts
                       (sort-by (comp str key))
                       (map (fn [[k v]] (format "%s:%s" (name k) v)))
                       (take 4)
                       (str/join ",")))]
     (log-aif-line! "update"
                    (remove nil?
                            [(when err (str "err=" err))
                             (when ev (str "ev=" ev))
                             (when delta (str "d=" delta))
                             (when counts (str "counts=" counts))
                             (when pid (str "pattern=" pid))
                             (when action (str "action=" (name action)))
                             (when tau (str "tau=" tau))])))))

(defn log-aif-tap!
  ([event] (log-aif-tap! nil event))
  ([state event]
   (let [payload (or (:payload event) event)
         aif (:aif payload)
         kind (or (:event payload) (:aif/kind payload) (:type event))
         kind (cond
                (keyword? kind) kind
                (string? kind) (keyword kind)
                :else :unknown)
         tap-id (tap-event-id event)]
     (when (and state tap-id)
       (swap! state assoc :last-aif-tap tap-id))
     (when (map? aif)
       (cond
         (= kind :select) (log-aif-selection! state aif)
         (contains? #{:update :pattern-action :action :use} kind) (log-aif-update! state aif)
         :else (log-aif-line! "tap" [(str "kind=" (name kind))]))))))

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
              (log-aif-tap! state tap)
              true))))
      (catch Throwable _ nil))))

(defn- format-confidence [detail]
  (when (map? detail)
    (let [phase (:maturity-phase detail)
          precision (:precision-prior detail)
          evidence (:evidence-count detail)
          next-steps (:next-steps-count detail)
          parts (remove nil?
                        [(when phase (str "confidence=" (name phase)))
                         (when (number? precision) (str "p=" (fmt-num precision)))
                         (when (number? evidence) (str "e=" (long evidence)))
                         (when (number? next-steps) (str "n=" (long next-steps)))])]
      (when (seq parts)
        (str " " (str/join " " parts))))))

(defn log-selection!
  ([psr] (log-selection! nil psr nil))
  ([psr detail] (log-selection! nil psr detail))
  ([_state psr detail]
   (let [reason (:selection/reason psr)
         mode (:mode reason)
         reads (:reads reason)
         aif (:aif reason)
         g (fmt-num (:G aif))
         tau (fmt-num (:tau aif))
         confidence-label (format-confidence detail)
         aif-label (when (or g tau)
                     (str " aif=" (str/join ","
                                            (remove nil?
                                                    [(when g (str "G=" g))
                                                     (when tau (str "tau=" tau))]))))]
     (let [line (format "[pattern-selection] chosen=%s candidates=%d mode=%s%s%s%s"
                        (:chosen psr)
                        (count (:candidates psr))
                        (or mode :unknown)
                        (if (seq reads)
                          (str " reads=" (str/join "," reads))
                          "")
                        (or confidence-label "")
                        (or aif-label ""))]
       (log! line)
       (println line))
    (flush)
    nil)))

(defn- candidate-detail [state pattern-id]
  (get-in @state [:candidate-details (candidate-id pattern-id)]))

(defn log-use!
  ([pur aif] (log-use! nil pur aif))
  ([_state pur aif]
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
    (flush))))

(defn- log-pattern-action! [pattern-id action note files]
  (let [label (case action
                "update" "pattern-update"
                "implement" "pattern-implement"
                "read" "pattern-read"
                "pattern-action")
        parts (remove nil?
                      [(when pattern-id (str "id=" pattern-id))
                       (when action (str "action=" action))
                       (when (seq files) (str "files=" (str/join "," files)))
                       (when (seq note) (str "note=" note))])
        line (str "[" label "] " (str/join " " parts))]
    (log! line)
    (println line)
    (flush)))

(defn format-sigils [sigils]
  (when (seq sigils)
    (->> sigils
         (keep (fn [sigil]
                 (cond
                   (map? sigil)
                   (let [emoji (:emoji sigil)
                         hanzi (:hanzi sigil)]
                     (when (or emoji hanzi)
                       (str (or emoji "") "/" (or hanzi ""))))
                   (string? sigil) sigil
                   :else (str sigil))))
         (str/join " "))))

(defn- note-selected! [state pattern-id]
  (swap! state (fn [st]
                 (-> st
                     (update :selected-patterns (fnil conj #{}) pattern-id)
                     (assoc :last-selected pattern-id)))))

(defn- selected? [state pattern-id]
  (contains? (get @state :selected-patterns #{}) pattern-id))

(defn- resolve-selected-pattern [state pattern-ids]
  (let [last-selected (:last-selected @state)
        selected (get @state :selected-patterns #{})
        matches (vec (filter selected pattern-ids))]
    (cond
      (and last-selected (some #{last-selected} pattern-ids)) last-selected
      (= 1 (count pattern-ids)) (first pattern-ids)
      (= 1 (count matches)) (first matches)
      :else nil)))

(defn- ensure-selection!
  [state musn-session turn pattern-id reason reads]
  (let [pid (candidate-id pattern-id)]
    (when (and pid (not (selected? state pid)))
      (let [resp (musn-select musn-session turn pid reason reads)]
        (if (and resp (:ok resp))
          (do
            (note-selected! state pid)
            (when-let [psr (:psr resp)]
              (log-selection! state psr (candidate-detail state pid)))
            (let [logged? (log-latest-aif-tap! state musn-session)]
              (when (and (not logged?) (not (:aif resp)))
                (log-aif-missing! "turn/select"
                                  {:turn turn
                                   :chosen pid}))))
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

(defn- wait-for-resume!
  [state session]
  (loop []
    (Thread/sleep 1000)
    (when-let [st (musn-state session)]
      (let [note (get-in st [:state :resume-note])]
        (if note
          (do
            (let [line (format "[musn-resume] note=%s" note)]
              (log! line)
              (println line))
            (flush)
            (swap! state assoc :resume-note note :paused? false :pause-latched? false))
          (recur))))))

(defn- handle-pattern-selection!
  [state musn-session turn sel]
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
      (log-selection! state psr (candidate-detail state chosen)))
    (let [logged? (log-latest-aif-tap! state musn-session)]
      (when (and (not logged?) (not (:aif resp)))
        (log-aif-missing! "turn/select"
                          {:turn turn
                           :chosen chosen})))))

(defn- perform-pattern-action!
  [state musn-session turn pid act note files]
  (maybe-warn-missing-plan! state act)
  (when (#{"implement" "update"} act)
    (ensure-selection! state musn-session turn pid
                       {:mode :use
                        :note "auto selection from pattern action"
                        :source :auto}
                       nil))
  (log-pattern-action! (candidate-id pid) act note files)
  (let [action-resp (musn-action musn-session turn pid act note files)]
    (let [logged? (log-latest-aif-tap! state musn-session)]
      (when (and (not logged?) (not (:aif action-resp)))
        (log-aif-missing! "turn/action"
                          {:turn turn
                           :pattern/id (candidate-id pid)
                           :action act})))
    (log-mana-response! state action-resp))
  (when (#{"implement" "update"} act)
    (let [use-resp (musn-use musn-session turn pid)]
      (when-let [pur (:pur use-resp)]
        (log-use! state pur (:aif use-resp)))
      (let [logged? (log-latest-aif-tap! state musn-session)]
        (when (and (not logged?) (not (:aif use-resp)))
          (log-aif-missing! "turn/use"
                            {:turn turn
                             :pattern/id (candidate-id pid)
                             :action act})))
      (log-mana-response! state use-resp)))
  (when (seq files)
    (musn-evidence musn-session turn pid files "auto evidence from pattern-action")))

(defn- handle-pattern-action!
  [state musn-session turn payload]
  (let [act (:action payload)
        pid (:pattern/id payload)
        files (:files payload)
        note (:note payload)]
    (perform-pattern-action! state musn-session turn pid act note files)))

(defn- warn-missing-intent! [state context]
  (when-not (or (:intent-missing-warned? @state)
                (some-> (:intent-seed @state) str/trim not-empty))
    (swap! state assoc :intent-missing-warned? true)
    (log-warning! "missing-intent"
                  {:turn (:turn @state)
                   :context context
                   :note "intent line not seen yet; ignoring actions until hud-intent is provided"})))

(defn- handle-agent-message!
  [state musn-session turn text]
  (let [handle-line (fn [line]
                      (when (plan-line? line)
                        (note-plan-seen! state line)
                        (musn-plan musn-session turn (str/trim line)))
                      (when-let [payload (parse-json line)]
                        (when-let [sel (parse-pattern-selection payload)]
                          (handle-pattern-selection! state musn-session turn sel))
                        (when-let [pa (parse-pattern-action payload)]
                          (handle-pattern-action! state musn-session turn pa)))
                      (when-let [pa (parse-pattern-action-line line)]
                        (handle-pattern-action! state musn-session turn pa)))]
    (if (:intent-seen? @state)
      (doseq [line (str/split-lines (or text ""))]
        (handle-line line))
      (let [lines (str/split-lines (or text ""))
            idx (first (keep-indexed (fn [i line]
                                       (when (intent-line line)
                                         i))
                                     lines))]
        (if (number? idx)
          (let [line (nth lines idx)
                intent (intent-line line)]
            (note-intent-seen! state musn-session intent)
            (doseq [rest-line (drop (inc idx) lines)]
              (handle-line rest-line)))
          (warn-missing-intent! state :agent-message)))))
  (when-let [report (hud/parse-agent-report text)]
    (when-let [pid (:applied report)]
      (let [mode (case (some-> (:action report) str/lower-case)
                   "read" :read
                   "implement" :use
                   "update" :use
                   :select)]
        (ensure-selection! state musn-session turn pid
                           {:mode mode
                            :note (or (:notes report) "agent report selection")
                            :source :report}
                           nil)))))

(defn- handle-reasoning-message!
  [state musn-session turn text]
  (if-not (:intent-seen? @state)
    (warn-missing-intent! state :reasoning)
    (do
      (doseq [line (str/split-lines (or text ""))]
        (when-let [intent (intent-line line)]
          (note-intent-seen! state musn-session intent)))
      (note-plan-proxy! state musn-session turn text))))

(defn- handle-command-execution!
  [state musn-session turn cmd]
  (if-not (:intent-seen? @state)
    (warn-missing-intent! state :command)
    (when-not (handle-helper-command! state musn-session turn cmd)
      (if-let [{:keys [action pattern-id note]} (parse-pattern-action-command cmd)]
        (perform-pattern-action! state musn-session turn pattern-id action note nil)
        (let [pats (patterns-from-command cmd)]
          (if (seq pats)
            (doseq [p pats]
              (log-pattern-action! (candidate-id p) "read" "auto read from command" nil)
              (let [action-resp (musn-action musn-session turn p "read" nil nil)]
                (let [logged? (log-latest-aif-tap! state musn-session)]
                  (when (and (not logged?) (not (:aif action-resp)))
                    (log-aif-missing! "turn/action"
                                      {:turn turn
                                       :pattern/id (candidate-id p)
                                       :action "read"})))
                (log-mana-response! state action-resp)))
            (do
              (maybe-warn-missing-plan! state "wide-scan")
              (let [note (when (string? cmd)
                           (let [trimmed (str/trim cmd)]
                             (when-not (str/blank? trimmed)
                               (str "tool command: " trimmed))))
                    action-resp (musn-action musn-session
                                             turn
                                             "musn/plan-before-tool"
                                             "wide-scan"
                                             note
                                             nil)]
                (let [logged? (log-latest-aif-tap! state musn-session)]
                  (when (and (not logged?) (not (:aif action-resp)))
                    (log-aif-missing! "turn/action"
                                      {:turn turn
                                       :pattern/id "musn/plan-before-tool"
                                       :action "wide-scan"})))
                (log-mana-response! state action-resp)))))))))

(defn- handle-file-change!
  [state musn-session turn changes]
  (if-not (:intent-seen? @state)
    (warn-missing-intent! state :file-change)
    (let [files (files-from-change changes)
          pattern-files (->> changes
                             (map :path)
                             (remove nil?)
                             (filter #(re-find pattern-id-re %)))
          _ (when (seq pattern-files)
              (maybe-warn-missing-plan! state "update"))
          resolved (reduce
                    (fn [acc path]
                      (let [candidates (patterns-from-change-path path)]
                        (if (seq candidates)
                          (if-let [pid (resolve-selected-pattern state candidates)]
                            (update acc pid (fnil conj []) path)
                            (let [decision (aif-policy-choice state candidates)
                                  chosen (:chosen decision)]
                              (if chosen
                                (do
                                  (log-aif-policy! (assoc decision :chosen chosen))
                                  (update acc chosen (fnil conj []) path))
                                (do
                                  (log-aif-policy! decision)
                                  (log-warning! "pattern-edit-without-selection"
                                                {:turn turn
                                                 :file path
                                                 :pattern-count (count candidates)
                                                 :pattern-sample (vec (take 5 candidates))
                                                 :note "multiarg change; select a pattern to disambiguate"})
                                  acc))))
                          acc)))
                    {}
                    pattern-files)]
      (doseq [[p paths] resolved]
        (let [pid (candidate-id p)]
          (when (and pid (not (selected? state pid)))
            (log-warning! "pattern-edit-auto-selected"
                          {:pattern/id pid
                           :turn turn
                           :files (vec (take 3 files))
                           :file-count (count files)}))
          (ensure-selection! state musn-session turn pid
                             {:mode :use
                              :note "auto selection from pattern file change"
                              :source :aif}
                             nil))
        (let [action-resp (musn-action musn-session turn p "update" "auto file_change" paths)]
          (let [logged? (log-latest-aif-tap! state musn-session)]
            (when (and (not logged?) (not (:aif action-resp)))
              (log-aif-missing! "turn/action"
                                {:turn turn
                                 :pattern/id (candidate-id p)
                                 :action "update"})))
          (log-mana-response! state action-resp))
        (when (seq paths)
          (musn-evidence musn-session turn p paths "auto evidence from change"))))))

(defn handle-event! [state event musn-session]
  (let [{:keys [turn]} @state]
    (cond
      ;; pattern-selection / pattern-action parsed from text payloads (RPC)
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "agent_message"))
      (handle-agent-message! state musn-session turn (get-in event [:item :text]))

      ;; command execution
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "command_execution"))
      (handle-command-execution! state musn-session turn (get-in event [:item :command]))

      ;; reasoning text (treat "Planning..." as plan proxy)
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "reasoning"))
      (handle-reasoning-message! state musn-session turn (get-in event [:item :text]))

      ;; file changes
      (and (= (:type event) "item.completed")
           (= (get-in event [:item :type]) "file_change"))
      (handle-file-change! state musn-session turn (get-in event [:item :changes]))

      :else nil)))

(defn- handle-turn-start!
  [state session event]
  (let [t (or (:turn event) (inc (:turn @state)) 1)]
    (swap! state assoc
           :turn t
           :selected-patterns #{}
           :last-selected nil
           :turn-plan? false
           :turn-plan-warned? false
           :turn-ended nil
           :intent-seen? false
           :intent nil
           :intent-seed nil)
    (let [seed-intent (or (some-> musn-intent str/trim not-empty)
                          (compute-intent musn-prompt))
          hud-map (or (read-hud-json hud-json-path)
                      (try
                        (when seed-intent
                          (hud/build-hud {:intent seed-intent
                                          :pattern-limit 8
                                          :namespaces ["musn" "fulab" "aif" "agent"]}))
                        (catch Throwable e
                          (log-musn! state "hud-build error" (.getMessage e))
                          (println (format "[hud-build-error] %s" (.getMessage e)))
                          (flush)
                          nil)))
          hud-map (if (and seed-intent (map? hud-map) (str/blank? (:intent hud-map)))
                    (assoc hud-map :intent seed-intent)
                    hud-map)
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
          hud-payload (cond-> {}
                        (seq candidate-ids) (assoc :candidates candidate-ids)
                        (map? scores) (assoc :scores scores)
                        (seq full-candidates) (assoc :candidate-details full-candidates)
                        (:sigils hud-map) (assoc :sigils (:sigils hud-map))
                        (:namespaces hud-map) (assoc :namespaces (:namespaces hud-map))
                        (:aif hud-map) (assoc :aif (:aif hud-map)))]
      (when seed-intent
        (swap! state assoc :intent-seed seed-intent))
      (when-let [aif-policy (:aif hud-map)]
        (swap! state assoc :aif-policy aif-policy))
      ;; Log task summary at turn start
      (when seed-intent
        (let [line (format "[task] %s" seed-intent)]
          (log! line)
          (println line)
          (flush)))
      (let [start-resp (musn-start session t hud-payload)]
        (when (and seed-intent (not (:intent-seen? @state)))
          (note-intent-seen! state session seed-intent))
        (let [logged? (log-latest-aif-tap! state session)]
          (when (and (seq candidate-ids) (not logged?) (not (:aif start-resp)))
            (log-aif-missing! "turn/start"
                              {:turn t
                               :candidates (count candidate-ids)})))
        (log-mana-response! state start-resp))
      (swap! state assoc
             :candidate-details (into {}
                                      (keep (fn [entry]
                                              (when-let [id (:id entry)]
                                                [id (select-keys entry
                                                                 [:maturity-phase
                                                                  :precision-prior
                                                                  :evidence-count
                                                                  :next-steps-count])])))
                                      full-candidates))
      (if (seq candidate-ids)
        (let [line (format "[hud-candidates] %s" (str/join ", " candidate-ids))]
          (log! line)
          (println line))
        (let [line "[hud-candidates] none"]
          (log! line)
          (println line)))
      (when-let [sigils (:sigils hud-map)]
        (let [line (format "[hud-sigils] %s"
                           (or (format-sigils sigils) sigils))]
          (log! line)
          (println line)))
      (flush))))

(defn- handle-turn-completed!
  [state session event tap-listener]
  (when (= (:type event) "turn.completed")
    (let [turn (:turn @state)]
      (when-not (= turn (:turn-ended @state))
        (swap! state assoc :turn-ended turn)
        (swap! state update :turn-end-count (fnil inc 0))
        (let [resp (musn-end session turn)
              paused? (:halt? resp)
              reason-type (get-in resp [:halt/reason :type])]
          (log-mana-response! state resp)
          (when-let [policy (:aif/policy resp)]
            (log-aif-policy! policy))
          (when-not paused?
            (log! "[musn-end]"
                  {:turn turn
                   :event (:type event)
                   :event/turn (:turn event)
                   :usage (:usage event)
                   :end-count (:turn-end-count @state)}))
          (if (and paused? (= reason-type :mana-depleted))
            (do
              (log! "[musn-end]"
                    {:turn turn
                     :event (:type event)
                     :event/turn (:turn event)
                     :usage (:usage event)
                     :end-count (:turn-end-count @state)
                     :reason :mana-depleted})
              (remove-tap tap-listener)
              (System/exit 0))
            (if paused?
            (when-not (:pause-latched? @state)
              (swap! state assoc :paused? true :pause-latched? true)
              (log! "[pattern] pause-latch" {:state :latched :turn turn})
              (print-pause resp)
              (remove-tap tap-listener)
              (System/exit 3))
            (when (or (:paused? @state) (:pause-latched? @state))
              (swap! state assoc :paused? false :pause-latched? false)
              (log! "[pattern] pause-latch" {:state :reset :turn turn})))))))))

(defn- process-event!
  [state session event tap-listener]
  (let [event-type (:type event)
        log-event? (or (not= event-type "item.completed")
                       (:intent-seen? @state))]
    (when log-event?
      (log-musn! state "event" event)))
  (let [event-type (:type event)]
    (if (= event-type "turn.started")
      (handle-turn-start! state session event)
      (handle-event! state event session))
    (handle-turn-completed! state session event tap-listener)))

(defn stream-loop []
  (let [state (atom {:turn 0
                     :paused? false
                     :pause-latched? false
                     :selected-patterns #{}
                     :last-selected nil
                     :turn-plan? false
                     :turn-plan-warned? false
                     :intent-missing-warned? false
                     :intent-seen? false
                     :intent nil
                     :last-aif-tap nil
                     :candidate-details {}
                     :turn-ended nil})
        tap-listener (fn [event]
                       (when (= :aif/fulab (:type event))
                         (log! "[aif-tap]" event)
                         (log-aif-tap! state event)))]
    (add-tap tap-listener)
    (try
      (let [session (musn-create-session)
            sid (require-session-id session)]
        (log-musn! state "stream-loop start")
        (let [line (format "[musn-session] %s" sid)]
          (log! line)
          (println line))
        (flush)
        (doseq [line (line-seq (io/reader *in*))]
          (when-let [event (parse-json line)]
            (process-event! state session event tap-listener))))
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
