#!/usr/bin/env clojure
(ns scripts.musn-chat-supervisor
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.lang ProcessBuilder]))

(def ^:private log-path
  (or (System/getenv "MUSN_CHAT_SUP_LOG") "/tmp/musn_chat_supervisor.log"))

(def ^:private sessions-path
  (or (System/getenv "MUSN_CHAT_SESSIONS") "/tmp/musn_chat_sessions.edn"))

(defn- musn-log-path [session-id]
  (str "/tmp/musn_stream." session-id ".log"))

(defn- log! [msg & [ctx]]
  (let [line (if ctx
               (str "[musn-chat-supervisor] " msg " " (pr-str ctx))
               (str "[musn-chat-supervisor] " msg))]
    (println line)
    (try
      (spit log-path (str (java.time.Instant/now) " " line "\n") :append true)
      (catch Throwable _))))

(defn- read-session-map []
  (try
    (when (.exists (io/file sessions-path))
      (-> sessions-path slurp read-string))
    (catch Throwable _ nil)))

(defonce !sessions
  (atom (or (read-session-map) {})))

(defn- persist-sessions! []
  (try
    (spit sessions-path (pr-str @!sessions))
    (catch Throwable t
      (log! "failed to persist sessions" {:err (.getMessage t)}))))

(defn- session-key [room token]
  [(or room "lab") (or token "fucodex")])

(defn- session-for [room token]
  (get @!sessions (session-key room token)))

(defn- set-session! [room token session-id]
  (swap! !sessions assoc (session-key room token) session-id)
  (persist-sessions!))

(defn- parse-args [args]
  (loop [args args
         opts {:musn-url "http://localhost:6065"
               :room "lab"
               :poll-interval 2.0
               :bot-name "fucodex"
               :agent "fucodex"
               :mode :auto  ; :auto, :claude, or :codex
               :no-sandbox false
               :approval-policy nil}]
    (if (empty? args)
      opts
      (let [[flag val & rest] args]
        (case flag
          "--musn-url" (recur rest (assoc opts :musn-url val))
          "--room" (recur rest (assoc opts :room val))
          "--poll-interval" (recur rest (assoc opts :poll-interval (Double/parseDouble val)))
          "--bot-name" (recur rest (assoc opts :bot-name val))
          "--fucodex" (recur rest (assoc opts :agent val))  ; legacy alias
          "--agent" (recur rest (assoc opts :agent val))
          "--mode" (recur rest (assoc opts :mode (keyword val)))
          "--no-sandbox" (recur rest (assoc opts :no-sandbox true))
          "--approval-policy" (recur rest (assoc opts :approval-policy val))
          (recur rest opts))))))

(defn- detect-agent-mode
  "Detect whether agent command is claude-style or codex-style."
  [agent-cmd mode]
  (if (= mode :auto)
    (cond
      (str/includes? (str agent-cmd) "fuclaude") :claude
      (str/includes? (str agent-cmd) "claude") :claude
      :else :codex)
    mode))

(defn- strip-fulab-report [text]
  (let [text (or text "")]
    (-> text
        (str/replace #"\[FULAB-REPORT\][\s\S]*?\[/FULAB-REPORT\]" "")
        (str/replace #"[ \t]+\n" "\n")
        (str/replace #"\n{3,}" "\n\n")
        str/trim)))

(defn- musn-post! [musn-url path payload]
  (try
    (let [resp (http/post (str (str/replace musn-url #"/+$" "") path)
                          {:content-type :json
                           :accept :json
                           :throw-exceptions false
                           :body (json/generate-string payload)})
          body (when-let [raw (:body resp)]
                 (try
                   (json/parse-string raw true)
                   (catch Exception _ nil)))]
      {:status (:status resp) :body body})
    (catch Throwable t
      (log! "musn post failed" {:path path :err (.getMessage t)})
      {:status 0 :body nil})))

(defn- musn-chat-message!
  [musn-url room bot-name text]
  (musn-post! musn-url "/musn/chat/message"
              {:room room
               :msg-id (str (java.util.UUID/randomUUID))
               :author {:id bot-name :name bot-name}
               :text text}))

(defn- musn-health! [musn-url]
  (try
    (let [resp (http/get (str (str/replace musn-url #"/+$" "") "/health")
                         {:throw-exceptions false
                          :accept :json})]
      (= 200 (:status resp)))
    (catch Throwable _ false)))

(defn- wait-for-musn! [musn-url]
  (loop [attempt 0]
    (if (musn-health! musn-url)
      (log! "musn healthy")
      (do
        (when (zero? (mod attempt 10))
          (log! "waiting for musn http" {:attempt attempt :url musn-url}))
        (Thread/sleep 1000)
        (recur (inc attempt))))))

(defn- normalize-event-type [etype]
  (cond
    (keyword? etype) (name etype)
    (string? etype) etype
    :else ""))

(defn- parse-chat-message [event]
  (let [etype (normalize-event-type (:event/type event))]
    (when (= "chat/message" etype)
      (let [payload (:payload event)
            author (:author payload)
            name (or (:name author) (:id author) "anon")
            text (strip-fulab-report (:text payload))]
        (when (seq text)
          {:msg-id (:msg-id payload)
           :chat-id (:chat/id payload)
           :name name
           :author-id (:id author)
           :text text
           :at (:at payload)})))))

(defn- parse-instant [value]
  (try
    (when (string? value)
      (Instant/parse value))
    (catch Exception _ nil)))

(defn- same-bot? [bot-name message]
  (let [bot-name (str/lower-case (or bot-name ""))
        author (str/lower-case (or (:name message) ""))]
    (= bot-name author)))

(defn- sanitize-token [value]
  (-> (or value "")
      str/trim
      (str/replace #"[^a-zA-Z0-9._-]+" "-")
      (str/replace #"^-+" "")
      (str/replace #"-+$" "")))

(defn- new-session-id [room token]
  (format "musn-chat-%s-%s-%d"
          (sanitize-token room)
          (sanitize-token token)
          (System/currentTimeMillis)))

(defn- explicit-session-id [token]
  (let [token (or token "")]
    (cond
      (str/starts-with? token "sid:") (subs token 4)
      (re-find #"^(musn|session)-" token) token
      :else nil)))

(defn- parse-task [text]
  (let [text (or text "")
        lines (str/split-lines text)
        line (some #(when (str/starts-with? (str/trim %) "!task") (str/trim %)) lines)]
    (when line
      (let [rest (str/trim (subs line (count "!task")))
            sid-match (re-find #"^sid:([^\s]+)\s+(.+)$" rest)
            token-match (re-find #"^token:([^\s]+)\s+(.+)$" rest)]
        (cond
          sid-match {:session-id (second sid-match)
                     :body (nth sid-match 2)
                     :implicit-token? false}
          token-match {:token (second token-match)
                       :body (nth token-match 2)
                       :implicit-token? false}
          (not (str/blank? rest))
          {:token nil :body rest :implicit-token? true}
          :else nil)))))

(defn- parse-new [text]
  (let [text (or text "")
        lines (str/split-lines text)
        line (some #(when (str/starts-with? (str/trim %) "!new") (str/trim %)) lines)]
    (when line
      (let [rest (str/trim (subs line (count "!new")))
            sid-match (re-find #"^sid:([^\s]+)\s+(.+)$" rest)
            token-match (re-find #"^token:([^\s]+)\s+(.+)$" rest)]
        (cond
          sid-match {:session-id (second sid-match)
                     :body (nth sid-match 2)}
          token-match {:token (second token-match)
                       :body (nth token-match 2)}
          (not (str/blank? rest)) {:body rest})))))

(defn- infer-task-target [room body]
  (let [body (or body "")
        parts (str/split (str/trim body) #"\s+" 2)
        head (first parts)
        tail (second parts)
        explicit-sid (explicit-session-id head)
        token-key (sanitize-token head)
        known-token? (and (seq token-key)
                          (contains? @!sessions (session-key room token-key)))]
    (cond
      explicit-sid {:session-id explicit-sid :body (or tail "") :implicit-token? false}
      known-token? {:token head :body (or tail "") :implicit-token? false}
      :else {:token nil :body body :implicit-token? true})))

(defn- run-agent!
  "Run agent (fuclaude or fucodex) to resume an existing session."
  [agent-cmd mode musn-url room bot-name session-id prompt intent no-sandbox approval-policy]
  (let [agent-cmd (or (when (and agent-cmd (re-find #"/" agent-cmd)) agent-cmd)
                      (let [candidate (io/file (System/getProperty "user.dir") (or agent-cmd "fucodex"))]
                        (when (.exists candidate) (.getAbsolutePath candidate)))
                      agent-cmd)
        mode (detect-agent-mode agent-cmd mode)
        ;; Common args for both modes
        base-args (cond-> [agent-cmd
                           "--live"
                           "--musn"
                           "--musn-url" musn-url
                           "--session-id" session-id
                           "--chat-room" room
                           "--chat-author" bot-name]
                    (and intent (not (str/blank? intent)))
                    (conj "--intent" intent)
                    no-sandbox (conj (if (= mode :claude) "--yolo" "--no-sandbox"))
                    (and approval-policy (not (str/blank? approval-policy)))
                    (conj "--approval-policy" approval-policy))
        ;; Mode-specific suffix
        args (if (= mode :claude)
               ;; fuclaude: --continue -p <prompt>
               (into base-args ["--continue" "-p" prompt])
               ;; fucodex: resume --last <prompt>
               (into base-args ["resume" "--last" prompt]))
        pb (doto (ProcessBuilder. ^java.util.List args)
             (.inheritIO))
        env (.environment pb)
        started-at (Instant/now)]
    (.put env "FUTON3_MUSN_REQUIRE_APPROVAL" "0")
    (.put env "FUTON3_MUSN_CHAT_TRIM" "1")
    (.put env "FUTON3_MUSN_LOG" (musn-log-path session-id))
    (log! "starting agent run" {:room room :bot bot-name :session session-id :cmd agent-cmd :mode mode})
    (try
      (let [proc (.start pb)
            exit-code (.waitFor proc)
            duration-ms (.toMillis (java.time.Duration/between started-at (Instant/now)))]
        (log! "agent run complete" {:exit exit-code :ms duration-ms :session session-id})
        (when (not= 0 exit-code)
          (log! "agent exited non-zero" {:exit exit-code :session session-id})))
      (catch Throwable t
        (log! "agent failed to start" {:err (.getMessage t) :session session-id})))
    {:ok true :started-at started-at}))

;; Legacy alias for backwards compatibility
(def run-fucodex! run-agent!)

(defn- run-agent-new!
  "Run agent (fuclaude or fucodex) to start a new session."
  [agent-cmd mode musn-url room bot-name session-id intent no-sandbox approval-policy]
  (let [agent-cmd (or (when (and agent-cmd (re-find #"/" agent-cmd)) agent-cmd)
                      (let [candidate (io/file (System/getProperty "user.dir") (or agent-cmd "fucodex"))]
                        (when (.exists candidate) (.getAbsolutePath candidate)))
                      agent-cmd)
        mode (detect-agent-mode agent-cmd mode)
        ;; Common args for both modes
        base-args (cond-> [agent-cmd
                           "--live"
                           "--musn"
                           "--musn-url" musn-url
                           "--chat-room" room
                           "--chat-author" bot-name]
                    (and session-id (not (str/blank? session-id)))
                    (conj "--session-id" session-id)
                    no-sandbox (conj (if (= mode :claude) "--yolo" "--no-sandbox"))
                    (and approval-policy (not (str/blank? approval-policy)))
                    (conj "--approval-policy" approval-policy)
                    (and intent (not (str/blank? intent)))
                    (conj "--intent" intent))
        ;; Mode-specific suffix
        args (if (= mode :claude)
               ;; fuclaude: -p <prompt>
               (into base-args ["-p" intent])
               ;; fucodex: exec --prompt <prompt>
               (into base-args ["exec" "--prompt" intent]))
        pb (doto (ProcessBuilder. ^java.util.List args)
             (.inheritIO))
        env (.environment pb)
        started-at (Instant/now)]
    (.put env "FUTON3_MUSN_REQUIRE_APPROVAL" "0")
    (.put env "FUTON3_MUSN_CHAT_TRIM" "1")
    (.put env "FUTON3_MUSN_LOG" (musn-log-path session-id))
    (log! "starting agent run (new)" {:room room :bot bot-name :session session-id :cmd agent-cmd :mode mode})
    (try
      (let [proc (.start pb)
            exit-code (.waitFor proc)
            duration-ms (.toMillis (java.time.Duration/between started-at (Instant/now)))]
        (log! "agent run complete (new)" {:exit exit-code :ms duration-ms :session session-id})
        (when (not= 0 exit-code)
          (log! "agent exited non-zero (new)" {:exit exit-code :session session-id})))
      (catch Throwable t
        (log! "agent failed to start (new)" {:err (.getMessage t) :session session-id})))
    {:ok true :started-at started-at}))

;; Legacy alias for backwards compatibility
(def run-fucodex-new! run-agent-new!)

(def ^:private pattern-line-re
  #"^([0-9T:\.\-Z]+)\s+\[(pattern-[^\]]+)\]\s+(.*)$")

(defn- parse-pattern-id [label payload]
  (cond
    (= label "pattern-use")
    (some-> payload (str/split #"\s+" 2) first)

    :else
    (some-> (re-find #"id=([^\s]+)" payload) second)))

(defn- parse-log-line [line]
  (when-let [[_ ts label payload] (re-find pattern-line-re (or line ""))]
    (when-let [inst (parse-instant ts)]
      {:at inst
       :label label
       :payload payload
       :pattern-id (parse-pattern-id label payload)})))

(defn- read-pattern-trace [log-path started-at]
  (when (and log-path (.exists (io/file log-path)))
    (with-open [r (io/reader log-path)]
      (->> (line-seq r)
           (keep parse-log-line)
           (filter (fn [{:keys [at]}]
                     (and at started-at (not (.isBefore at started-at)))))
           (filter :pattern-id)
           vec))))

(defn- summarize-pattern-trace [entries]
  (if (seq entries)
    (let [bucket (fn [label]
                   (->> entries
                        (filter #(= label (:label %)))
                        (map :pattern-id)
                        distinct))
          selections (bucket "pattern-selection")
          reads (bucket "pattern-read")
          uses (bucket "pattern-use")
          updates (concat (bucket "pattern-update")
                          (bucket "pattern-implement")
                          (bucket "pattern-action"))
          parts (remove nil?
                        [(when (seq selections) (str "select=" (str/join "," selections)))
                         (when (seq reads) (str "read=" (str/join "," reads)))
                         (when (seq uses) (str "use=" (str/join "," uses)))
                         (when (seq updates) (str "action=" (str/join "," updates)))])]
      (if (seq parts)
        (str "Pattern trace: " (str/join " | " parts))
        "Pattern trace: none"))
    "Pattern trace: none"))

(defn- poll-loop!
  [{:keys [musn-url room poll-interval bot-name agent mode no-sandbox approval-policy]}]
  (log! "starting supervisor" {:room room :bot bot-name :agent agent :mode mode :musn musn-url})
  (wait-for-musn! musn-url)
  (let [started-at (Instant/now)
        init-resp (musn-post! musn-url "/musn/chat/state" {:room room})
        init-cursor (if (and (= 200 (:status init-resp)) (get-in init-resp [:body :ok]))
                      (or (get-in init-resp [:body :cursor]) 0)
                      0)
        init-events (get-in init-resp [:body :events])
        init-messages (->> init-events (map parse-chat-message) (remove nil?) vec)
        seen-init (into #{} (map :msg-id init-messages))
        running (atom #{})]
    (loop [cursor init-cursor
           seen seen-init]
      (let [payload (cond-> {:room room}
                      (pos? cursor) (assoc :since cursor))
            {:keys [status body]} (musn-post! musn-url "/musn/chat/state" payload)
            ok? (and (= 200 status) (:ok body))
            next-cursor (if ok? (or (:cursor body) cursor) cursor)
            events (when ok? (:events body))
            messages (->> events (map parse-chat-message) (remove nil?) vec)
            recent (->> messages
                        (filter (fn [msg]
                                  (when-let [at (parse-instant (:at msg))]
                                    (not (.isBefore at started-at)))))
                        (remove #(same-bot? bot-name %))
                        vec)
            new-messages (->> recent
                              (remove #(contains? seen (:msg-id %)))
                              vec)
            seen (into seen (map :msg-id messages))]
        (doseq [message new-messages]
          (when-let [{:keys [token body implicit-token?]} (parse-task (:text message))]
            (let [inferred (when (and implicit-token? (not (str/blank? body)))
                             (infer-task-target room body))
                  token (or (:token inferred) token)
                  session-id (:session-id inferred)
                  body (or (:body inferred) body)
                  implicit-token? (if inferred (:implicit-token? inferred) implicit-token?)
                  raw-token (or token bot-name)
                  token (sanitize-token raw-token)
                  existing-session (session-for room token)
                  session-id (or session-id existing-session)
                  session-id (or session-id (new-session-id room token))]
              (when (and session-id (not= session-id existing-session))
                (set-session! room token session-id))
              (if (contains? @running session-id)
                (log! "task ignored (session running)"
                      {:session session-id :from (:name message)})
                (do
                  (swap! running conj session-id)
                  (musn-chat-message! musn-url room bot-name
                                      (if implicit-token?
                                        (format "Starting %s for !task" session-id)
                                        (format "Starting %s for !task %s" session-id token)))
                  (future
                    (try
                      (let [result (run-agent! agent mode musn-url room bot-name session-id body body no-sandbox approval-policy)
                            started-at (:started-at result)
                            log-path (musn-log-path session-id)
                            trace (read-pattern-trace log-path started-at)
                            summary (summarize-pattern-trace trace)]
                        (when summary
                          (musn-chat-message! musn-url room bot-name summary)))
                      (finally
                        (swap! running disj session-id))))))))
          (when-let [{:keys [token body session-id]} (parse-new (:text message))]
            (let [raw-token (or token bot-name)
                  token (sanitize-token raw-token)
                  session-id (or (and (seq session-id) session-id)
                                 (new-session-id room token))]
              (if (contains? @running session-id)
                (log! "new ignored (session running)"
                      {:session session-id :from (:name message)})
                (do
                  (swap! running conj session-id)
                  (set-session! room token session-id)
                  (musn-chat-message! musn-url room bot-name
                                      (format "Starting %s for !new" session-id))
                  (future
                    (try
                      (let [result (run-agent-new! agent mode musn-url room bot-name session-id body no-sandbox approval-policy)
                            started-at (:started-at result)
                            log-path (musn-log-path session-id)
                            trace (read-pattern-trace log-path started-at)
                            summary (summarize-pattern-trace trace)]
                        (when summary
                          (musn-chat-message! musn-url room bot-name summary)))
                      (finally
                        (swap! running disj session-id)))))))))
        (Thread/sleep (long (* 1000 poll-interval)))
        (recur next-cursor seen)))))

(defn -main [& args]
  (poll-loop! (parse-args args)))

(apply -main *command-line-args*)
