(ns futon3.agency.service
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io BufferedReader InputStreamReader)
           (java.nio.channels FileChannel FileLock)
           (java.nio.file StandardOpenOption)
           (java.time Instant)))

(defn- env-int [key default]
  (try
    (some-> (System/getenv key) str/trim not-empty Long/parseLong)
    (catch Exception _ default)))

(defn- env-float [key default]
  (try
    (some-> (System/getenv key) str/trim not-empty Double/parseDouble)
    (catch Exception _ default)))

(defn- env-bool [key default]
  (let [value (some-> (System/getenv key) str/trim str/lower-case)]
    (cond
      (nil? value) default
      (contains? #{"1" "true" "yes"} value) true
      (contains? #{"0" "false" "no"} value) false
      :else default)))

(def ^:private default-config
  {:lab-root (or (System/getenv "AGENCY_LAB_ROOT") "lab")
   :state-dir (or (System/getenv "AGENCY_STATE_DIR") "lab/agency")
   :context-max-tokens (env-int "AGENCY_CONTEXT_MAX_TOKENS" 12000)
   :context-rollover-ratio (env-float "AGENCY_CONTEXT_ROLLOVER_RATIO" 0.85)
   :summary-words (env-int "AGENCY_SUMMARY_WORDS" 250)
   :recent-messages (env-int "AGENCY_CONTEXT_RECENT_MESSAGES" 12)
   :lock-timeout-ms (env-int "AGENCY_LOCK_TIMEOUT_MS" 15000)
   :codex-bin (or (System/getenv "AGENCY_CODEX_BIN") "codex")
   :futon3a-root (or (System/getenv "AGENCY_FUTON3A_ROOT")
                     (str (io/file (System/getProperty "user.dir") ".." "futon3a")))
   :workdir (or (System/getenv "AGENCY_WORKDIR")
                (System/getProperty "user.dir"))
   :approval-policy (System/getenv "AGENCY_APPROVAL_POLICY")
   :no-sandbox (env-bool "AGENCY_NO_SANDBOX" false)
   :skip-git-check (env-bool "AGENCY_SKIP_GIT_CHECK" true)
   :peripherals-path (System/getenv "AGENCY_PERIPHERALS_PATH")
   :retry-on-overflow (env-bool "AGENCY_RETRY_ON_OVERFLOW" true)})

(defonce ^:private !config (atom default-config))
(defonce ^:private !state (atom {}))

(defn config []
  @!config)

(defn- now-inst []
  (str (Instant/now)))

(defn- state-dir []
  (io/file (:state-dir (config))))

(defn- state-path [agent-id]
  (io/file (state-dir) (str (name agent-id) ".edn")))

(defn- load-agent-state [agent-id]
  (let [path (state-path agent-id)]
    (when (.exists path)
      (try
        (edn/read-string (slurp path))
        (catch Exception _ nil)))))

(defn- default-agent-state [agent-id]
  {:agent/id (name agent-id)
   :agent/current-thread-id nil
   :agent/ancestor-chain []
   :agent/state-capsule {}
   :agent/summary ""
   :agent/recent-messages []
   :agent/token-usage nil
   :agent/last-active (now-inst)})

(defn ensure-agent-state!
  [agent-id]
  (let [agent-id (name agent-id)]
    (or (get @!state agent-id)
        (let [loaded (load-agent-state agent-id)
              state (merge (default-agent-state agent-id) loaded)]
          (swap! !state assoc agent-id state)
          state))))

(defn- save-agent-state! [agent-id state]
  (let [path (state-path agent-id)]
    (io/make-parents path)
    (spit path (pr-str state)))
  state)

(defn- update-agent-state! [agent-id f & args]
  (let [agent-id (name agent-id)]
    (let [next-state (apply f (ensure-agent-state! agent-id) args)]
      (swap! !state assoc agent-id next-state)
      (save-agent-state! agent-id next-state))))

(defn list-agent-states []
  (vals @!state))

(defn- lock-path [agent-id]
  (io/file "/tmp" (str "agency-" (name agent-id) ".lock")))

(defn- acquire-lock!
  [agent-id timeout-ms]
  (let [path (lock-path agent-id)
        deadline (+ (System/currentTimeMillis) timeout-ms)
        opts (into-array java.nio.file.OpenOption
                         [StandardOpenOption/CREATE StandardOpenOption/WRITE])]
    (io/make-parents path)
    (loop []
      (let [channel (FileChannel/open (.toPath path) opts)]
        (let [lock-result (try
                            (.tryLock channel)
                            (catch Exception _ ::error))]
          (cond
            (= lock-result ::error)
            (do
              (try (.close channel) (catch Exception _))
              (if (> (System/currentTimeMillis) deadline)
                nil
                (do (Thread/sleep 150) (recur))))

            (nil? lock-result)
            (do
              (try (.close channel) (catch Exception _))
              (if (> (System/currentTimeMillis) deadline)
                nil
                (do (Thread/sleep 150) (recur))))

            :else
            {:channel channel :lock lock-result}))))))

(defn- release-lock! [{:keys [^FileChannel channel ^FileLock lock]}]
  (when lock
    (try (.release lock) (catch Exception _)))
  (when channel
    (try (.close channel) (catch Exception _))))

(defn- with-agent-lock
  [agent-id f]
  (let [timeout-ms (:lock-timeout-ms (config))
        lock (acquire-lock! agent-id timeout-ms)]
    (if (nil? lock)
      {:ok false :error "lock-timeout"}
      (try
        (f)
        (finally
          (release-lock! lock))))))

(defn- load-peripherals []
  (let [path (:peripherals-path (config))
        data (cond
               (and path (.exists (io/file path)))
               (slurp path)
               :else
               (some-> (io/resource "agency/peripherals.edn") slurp))]
    (when data
      (edn/read-string data))))

(defonce ^:private !peripherals (atom (load-peripherals)))

(defn- get-peripheral [peripheral-id]
  (let [entry (get-in @!peripherals [:peripherals (keyword peripheral-id)])]
    (when-not entry
      (throw (ex-info "unknown peripheral" {:peripheral peripheral-id})))
    entry))

(defn- render-template [template ctx]
  (reduce-kv
   (fn [text k v]
     (str/replace text
                  (re-pattern (str "\\{\\{" (name k) "\\}\\}"))
                  (str v)))
   (or template "")
   ctx))

(defn- format-state-capsule [capsule]
  (when (seq capsule)
    (str "State capsule (EDN):\n" (pr-str capsule))))

(defn- format-summary [summary]
  (when (seq (str/trim (or summary "")))
    (str "Rolling summary:\n" (str/trim summary))))

(defn- format-recent-messages [messages]
  (when (seq messages)
    (->> messages
         (map (fn [{:keys [role author text]}]
                (format "[%s]: %s"
                        (or author (name role))
                        text)))
         (str/join "\n"))))

(defn- build-run-prompt
  [peripheral {:keys [agent/summary agent/state-capsule agent/recent-messages]} {:keys [prompt musn agent-id]}]
  (let [summary-block (format-summary summary)
        capsule-block (format-state-capsule state-capsule)
        recent-block (format-recent-messages recent-messages)
        ctx {:agent-id (name agent-id)
             :prompt prompt
             :summary (or summary "")
             :state (pr-str (or state-capsule {}))
             :musn-session-id (get musn :session-id "")
             :musn-url (get musn :url "")
             :recent (or recent-block "")}
        entry (render-template (:entry peripheral) ctx)
        postlude (render-template (:postlude peripheral) ctx)
        body (render-template (:prompt peripheral) ctx)
        blocks (->> [(when summary-block summary-block)
                     (when capsule-block capsule-block)
                     (when recent-block (str "Recent messages:\n" recent-block))
                     (when (seq entry) entry)
                     (when (seq body) body)
                     (str "User request:\n" prompt)
                     (when (seq postlude) postlude)]
                    (remove nil?))]
    (str/join "\n\n" blocks)))

(defn- codex-command
  [{:keys [prompt resume-id approval-policy no-sandbox skip-git-check]}]
  (let [base (cond-> [(or (:codex-bin (config)) "codex")
                      "exec"
                      "--json"]
               skip-git-check (conj "--skip-git-repo-check")
               no-sandbox (conj "--dangerously-bypass-approvals-and-sandbox")
               approval-policy (conj "-c" (str "approval_policy=" approval-policy)))]
    (cond-> base
      resume-id (conj "resume" resume-id)
      true (conj prompt))))

(defn- parse-json-line [line]
  (try
    (json/parse-string line true)
    (catch Exception _ nil)))

(defn- parse-json [value]
  (try
    (json/parse-string value true)
    (catch Exception _ nil)))

(defn- run-command!
  [{:keys [args cwd env]}]
  (let [pb (doto (ProcessBuilder. ^java.util.List (mapv str args))
             (.directory (io/file (or cwd (:workdir (config)))))
             (.redirectErrorStream true))
        pb-env (.environment pb)]
    (doseq [[k v] (merge env {})]
      (.put pb-env (str k) (str v)))
    (let [process (.start pb)
          reader (BufferedReader. (InputStreamReader. (.getInputStream process)))
          output (StringBuilder.)]
      (loop []
        (when-let [line (.readLine reader)]
          (.append output line)
          (.append output "\n")
          (recur)))
      (.waitFor process)
      (let [exit (.exitValue process)
            out (str output)]
        {:ok (= 0 exit)
         :exit exit
         :out out}))))

(defn- extract-assistant-text [payload]
  (cond
    (string? payload) payload
    (map? payload) (or (extract-assistant-text (:text payload))
                       (extract-assistant-text (:content payload))
                       (extract-assistant-text (:message payload))
                       (extract-assistant-text (:output payload)))
    (sequential? payload)
    (->> payload
         (map extract-assistant-text)
         (remove nil?)
         (str/join ""))
    :else nil))

(defn- tokenize [text]
  (->> (str/split (str/lower-case (or text "")) #"[^a-z0-9._-]+")
       (remove str/blank?)
       distinct
       vec))

(defn- match-count [tokens text]
  (let [haystack (str/lower-case (or text ""))]
    (count (filter #(str/includes? haystack %) tokens))))

(defn- parse-usage [usage]
  (when (map? usage)
    (let [input (or (:input_tokens usage) (:input-tokens usage))
          output (or (:output_tokens usage) (:output-tokens usage))
          cached (or (:cached_input_tokens usage) (:cached-input-tokens usage))
          total (+ (long (or input 0)) (long (or output 0)) (long (or cached 0)))]
      {:input_tokens input
       :output_tokens output
       :cached_input_tokens cached
       :total_tokens total})))

(defn- run-codex!
  [{:keys [prompt resume-id cwd approval-policy no-sandbox skip-git-check env]}]
  (let [cmd (codex-command {:prompt prompt
                            :resume-id resume-id
                            :approval-policy approval-policy
                            :no-sandbox no-sandbox
                            :skip-git-check skip-git-check})
        pb (doto (ProcessBuilder. ^java.util.List cmd)
             (.directory (io/file (or cwd (:workdir (config)))))
             (.redirectErrorStream true))
        pb-env (.environment pb)]
    (doseq [[k v] (merge env {})]
      (.put pb-env (str k) (str v)))
    (let [process (.start pb)
          reader (BufferedReader. (InputStreamReader. (.getInputStream process)))
          response (atom nil)
          thread-id (atom nil)
          usage (atom nil)
          errors (atom [])
          lines (atom [])]
      (loop []
        (when-let [line (.readLine reader)]
          (swap! lines conj line)
          (when-let [parsed (parse-json-line line)]
            (let [etype (:type parsed)
                  payload (:payload parsed)]
              (cond
                (= etype "thread.started")
                (when-let [tid (:thread_id parsed)]
                  (reset! thread-id tid))

                (= etype "turn.completed")
                (when-let [u (:usage parsed)]
                  (reset! usage (parse-usage u)))

                (= etype "turn.failed")
                (swap! errors conj (or (get-in parsed [:error :message]) "turn-failed"))

                (= etype "error")
                (swap! errors conj (:message parsed))

                (and (= etype "item.completed")
                     (= "agent_message" (get-in parsed [:item :type])))
                (when-let [text (get-in parsed [:item :text])]
                  (reset! response text))

                (and (= etype "response_item")
                     (= "message" (get-in parsed [:payload :type]))
                     (= "assistant" (get-in parsed [:payload :role])))
                (when-let [text (extract-assistant-text (get-in parsed [:payload :content]))]
                  (reset! response text))

                (and (= etype "event_msg")
                     (= "agent_message" (get-in parsed [:payload :type])))
                (when-let [text (or (:message payload) (:text payload))]
                  (reset! response text))

                :else nil)))
          (recur)))
      (.waitFor process)
      (let [exit (.exitValue process)
            err (when (or (seq @errors) (not= 0 exit))
                  (or (first @errors) (str "codex-exec-exit-" exit)))]
        {:ok (nil? err)
         :error err
         :response @response
         :thread-id (or @thread-id resume-id)
         :usage @usage
         :lines @lines}))))

(defn- context-error? [err]
  (when err
    (let [text (str/lower-case (str err))]
      (or (str/includes? text "context")
          (str/includes? text "token")
          (str/includes? text "context_length_exceeded")))))

(defn- usage-rollover? [usage]
  (let [{:keys [context-max-tokens context-rollover-ratio]} (config)
        max-tokens (long (or context-max-tokens 0))
        threshold (long (Math/floor (* max-tokens context-rollover-ratio)))
        used (long (or (:input_tokens usage) (:total_tokens usage) 0))]
    (and (pos? max-tokens)
         (>= used threshold))))

(defn- portal-script []
  (io/file (:futon3a-root (config)) "scripts" "portal"))

(defn- musn-select-script []
  (io/file (:futon3a-root (config)) "scripts" "musn-select"))

(defn- musn-action-script []
  (io/file (:futon3a-root (config)) "scripts" "musn-action"))

(defn- portal-suggest
  [{:keys [query limit namespace]}]
  (let [args (cond-> [(str (portal-script)) "suggest" "--json" query]
               (seq namespace) (conj "--namespace" namespace)
               limit (conj "--limit" (str limit)))
        result (run-command! {:args args})]
    (if (:ok result)
      (or (parse-json (:out result)) [])
      (throw (ex-info "portal suggest failed" result)))))

(defn- portal-pattern-get
  [pattern-id]
  (let [args [(str (portal-script)) "patterns" "get" "--json" pattern-id]
        result (run-command! {:args args})]
    (if (:ok result)
      (parse-json (:out result))
      (throw (ex-info "portal patterns get failed" result)))))

(defn- choose-candidate
  [prompt candidates details]
  (let [tokens (tokenize prompt)
        score-entry (fn [cand detail]
                      (let [base (double (or (:score cand) 0))
                            summary (str (:summary detail) " " (:context detail) " " (:name detail))
                            bonus (match-count tokens summary)]
                        (assoc cand
                               :detail detail
                               :score/base base
                               :score/bonus bonus
                               :score/total (+ base bonus))))
        indexed (map (fn [cand]
                       (score-entry cand (get details (:id cand))))
                     candidates)]
    (->> indexed
         (sort-by (juxt (comp - :score/total) (comp - :score/base) :id))
         first)))

(defn- run-pattern-search!
  [{:keys [agent-id prompt musn peripheral]}]
  (let [limit (or (:limit peripheral) 5)
        namespace (:namespace peripheral)
        candidates (portal-suggest {:query prompt :limit limit :namespace namespace})]
    (if (empty? candidates)
      {:response (str "Pattern search for: " prompt "\n\nNo candidates found.")
       :chosen-id nil
       :candidates []}
      (let [candidate-ids (map :id candidates)
            details (into {}
                          (map (fn [pid] [pid (portal-pattern-get pid)]))
                          candidate-ids)
            chosen (choose-candidate prompt candidates details)
            chosen-id (:id chosen)
            chosen-detail (:detail chosen)
            reason (str "Selected " chosen-id
                        " (score=" (:score/total chosen)
                        ", base=" (:score/base chosen)
                        ", bonus=" (:score/bonus chosen) ")")
            musn-session (get musn :session-id)
            select-result (when (and musn-session chosen-id)
                            (run-command! {:args [(str (musn-select-script))
                                                  musn-session
                                                  chosen-id
                                                  reason]}))
            action-result (when (and musn-session chosen-id (:auto-action peripheral))
                            (run-command! {:args [(str (musn-action-script))
                                                  musn-session
                                                  chosen-id
                                                  (or (:action peripheral) "use")
                                                  (or (:action-note peripheral)
                                                      (str "Agency pattern-search recommended: " chosen-id))]}))
            report-lines (->> candidates
                              (map (fn [cand]
                                     (let [detail (get details (:id cand))
                                           name (or (:name detail) (:title detail) (:name cand))
                                           summary (or (:summary detail) (:context detail) "")]
                                       (format "- %s (score=%s): %s %s"
                                               (:id cand)
                                               (:score cand)
                                               (or name "")
                                               (when (seq summary)
                                                 (str "— " (str/trim summary)))))))
                              (str/join "\n"))
            report (str "Pattern search for: " prompt "\n\n"
                        "Top candidates:\n" report-lines "\n\n"
                        "Selected: " chosen-id "\n"
                        reason "\n"
                        (when (seq (or (:summary chosen-detail) (:context chosen-detail)))
                          (str "Summary: " (or (:summary chosen-detail) (:context chosen-detail)) "\n"))
                        (when select-result
                          (str "PSR: " (if (:ok select-result) "recorded" "failed") "\n"))
                        (when action-result
                          (str "PUR: " (if (:ok action-result) "recorded" "failed") "\n")))]
        {:response report
         :chosen-id chosen-id
         :candidates candidates
         :select select-result
         :action action-result}))))

(defn- summarize-prompt
  [summary recent summary-words]
  (str "You are maintaining a rolling summary of an IRC/agent discussion.\n"
       "Update the summary in <= " summary-words " words.\n"
       "Focus on decisions, action items, key claims, and open questions.\n"
       "Preserve names and attributions when possible.\n\n"
       "Existing summary:\n"
       (if (seq (str/trim summary)) summary "(none)")
       "\n\nRecent messages:\n"
       (if (seq recent) recent "(none)")
       "\n\nReturn only the updated summary."))

(defn- update-summary!
  [agent-id state reason]
  (let [summary (:agent/summary state)
        recent (format-recent-messages (:agent/recent-messages state))
        summary-words (:summary-words (config))
        prompt (summarize-prompt summary recent summary-words)
        result (run-codex! {:prompt prompt
                            :approval-policy (:approval-policy (config))
                            :no-sandbox (:no-sandbox (config))
                            :skip-git-check (:skip-git-check (config))})
        updated (str/trim (or (:response result) ""))]
    (if (seq updated)
      (update-agent-state!
       agent-id
       (fn [st]
         (assoc st :agent/summary updated
                   :agent/last-summary {:at (now-inst) :reason reason})))
      (update-agent-state!
       agent-id
       (fn [st]
         (assoc st :agent/summary (or (some-> summary str/trim) "")
                   :agent/last-summary {:at (now-inst) :reason (str reason ":fallback")}))))))

(defn- emit-rollover-event!
  [{:keys [musn-url session-id]} payload]
  (when (and musn-url session-id)
    (try
      (http/post (str (str/replace musn-url #"/+$" "") "/musn/activity/log")
                 {:content-type :json
                  :accept :json
                  :throw-exceptions false
                  :body (json/generate-string
                         {:agent "codex"
                          :source "agency"
                          :session/id session-id
                          :event/type :session/rollover
                          :metadata payload})})
      (catch Exception _ nil))))

(defn roll-over!
  [agent-id reason {:keys [musn]}]
  (with-agent-lock
   agent-id
   (fn []
     (let [state (ensure-agent-state! agent-id)
           _ (when (or (seq (:agent/summary state))
                       (seq (:agent/recent-messages state)))
               (update-summary! agent-id state reason))
           current-thread (:agent/current-thread-id (ensure-agent-state! agent-id))
           next-chain (cond-> (vec (or (:agent/ancestor-chain state) []))
                        current-thread (conj current-thread))]
       (update-agent-state!
        agent-id
        (fn [st]
          (-> st
              (assoc :agent/ancestor-chain next-chain
                     :agent/current-thread-id nil
                     :agent/recent-messages []
                     :agent/last-rollover {:at (now-inst) :reason reason}))))
       (emit-rollover-event!
        {:musn-url (get musn :url) :session-id (get musn :session-id)}
        {:reason reason
         :from-thread current-thread
         :ancestor-chain next-chain})
       {:ok true
        :agent-id (name agent-id)
        :rolled true
        :from-thread current-thread
        :ancestor-chain next-chain}))))

(defn- append-recent-message
  [state message]
  (let [limit (:recent-messages (config))
        messages (conj (vec (or (:agent/recent-messages state) [])) message)
        messages (if (> (count messages) limit)
                   (vec (take-last limit messages))
                   messages)]
    (assoc state :agent/recent-messages messages)))

(defn run-peripheral!
  [{:keys [agent-id peripheral prompt musn thread-id cwd approval-policy no-sandbox]}]
  (with-agent-lock
   agent-id
   (fn []
     (let [state (ensure-agent-state! agent-id)
           state (if thread-id
                   (assoc state :agent/current-thread-id thread-id)
                   state)
           peripheral (get-peripheral peripheral)
           prompt-text (build-run-prompt peripheral state {:prompt prompt
                                                           :musn musn
                                                           :agent-id agent-id})
           run-opts {:prompt prompt-text
                     :resume-id (:agent/current-thread-id state)
                     :cwd (or cwd (:workdir (config)))
                     :approval-policy (or approval-policy (:approval-policy (config)))
                     :no-sandbox (if (nil? no-sandbox) (:no-sandbox (config)) no-sandbox)
                     :skip-git-check (:skip-git-check (config))
                     :env (cond-> {}
                            (get musn :url) (assoc "FUTON3_MUSN_URL" (get musn :url))
                            (get musn :session-id) (assoc "FUTON3_MUSN_SESSION_ID" (get musn :session-id)))}]
       (if (= :pattern-search (:runner peripheral))
         (let [result (run-pattern-search! {:agent-id agent-id
                                            :prompt prompt
                                            :musn musn
                                            :peripheral peripheral})]
           (update-agent-state!
            agent-id
            (fn [st]
              (-> st
                  (assoc :agent/last-active (now-inst))
                  (append-recent-message {:role :user :author "user" :text prompt})
                  (append-recent-message {:role :assistant
                                          :author (name agent-id)
                                          :text (:response result)}))))
           {:ok true
            :agent-id (name agent-id)
            :thread-id (:agent/current-thread-id (ensure-agent-state! agent-id))
            :response (:response result)
            :pattern {:selected (:chosen-id result)}})
         (let [result (run-codex! run-opts)
               context-err? (context-error? (:error result))
               retry? (and context-err? (:retry-on-overflow (config)))]
           (when (and context-err? retry?)
             (roll-over! agent-id "context-overflow" {:musn musn}))
           (if (and context-err? retry?)
             (let [state (ensure-agent-state! agent-id)
                   retry-prompt (build-run-prompt peripheral state {:prompt prompt
                                                                    :musn musn
                                                                    :agent-id agent-id})
                   retry-result (run-codex! (assoc run-opts
                                                  :prompt retry-prompt
                                                  :resume-id (:agent/current-thread-id state)))]
               (if (:ok retry-result)
                 (let [next-thread (or (:thread-id retry-result) (:agent/current-thread-id state))]
                   (update-agent-state!
                    agent-id
                    (fn [st]
                      (-> st
                          (assoc :agent/current-thread-id next-thread
                                 :agent/token-usage (:usage retry-result)
                                 :agent/last-active (now-inst))
                          (append-recent-message {:role :user :author "user" :text prompt})
                          (append-recent-message {:role :assistant :author (name agent-id)
                                                  :text (or (:response retry-result) "")}))))
                   {:ok true
                    :agent-id (name agent-id)
                    :thread-id next-thread
                    :response (:response retry-result)
                    :usage (:usage retry-result)
                    :rolled true})
                 {:ok false
                  :agent-id (name agent-id)
                  :error (:error retry-result)}))
             (if (:ok result)
               (let [next-thread (or (:thread-id result) (:agent/current-thread-id state))
                     updated (update-agent-state!
                              agent-id
                              (fn [st]
                                (-> st
                                    (assoc :agent/current-thread-id next-thread
                                           :agent/token-usage (:usage result)
                                           :agent/last-active (now-inst))
                                    (append-recent-message {:role :user :author "user" :text prompt})
                                    (append-recent-message {:role :assistant
                                                            :author (name agent-id)
                                                            :text (or (:response result) "")}))))
                     rollover? (usage-rollover? (:agent/token-usage updated))]
                 (when rollover?
                   (roll-over! agent-id "usage-threshold" {:musn musn}))
                 {:ok true
                  :agent-id (name agent-id)
                  :thread-id (:agent/current-thread-id (ensure-agent-state! agent-id))
                  :response (:response result)
                  :usage (:usage result)
                  :rolled rollover?})
               {:ok false
                :agent-id (name agent-id)
                :error (:error result)}))))))))
