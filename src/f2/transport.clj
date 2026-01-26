(ns f2.transport
  "HTTP/WebSocket transport exposing a multi-user, capability-aware REPL."
  (:require [cheshire.core :as json]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [f0.clock :as clock]
            [futon3.checks :as checks]
            [futon3.fulab.hud :as hud]
            [futon3.fulab.pattern-competence :as pc]
            [futon3.futon2-bridge :as futon2-bridge]
            [futon3.musn.service :as musn-svc]
            [futon3.tatami :as tatami]
            [futon3.workday :as workday]
            [futon3.futon1-bridge :as f1-bridge]
            [f2.repl :as repl]
            [f2.semantics :as semantics]
            [f2.router :as router]
            [org.httpkit.server :as http])
  (:import (java.time Clock Instant)
           (java.util UUID)))

(def max-frame-bytes (* 8 1024))
(def inbox-size 64)
(def history-limit 2000)
(def repl-timeout-ms 100)


(defn- random-id [prefix]
  (str prefix "-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn- random-client-id []
  (random-id "C"))

(defn- random-run-id []
  (random-id "RUN"))

(defn- config! [state]
  @(:config state))

(defn- fixed-instant [config]
  (when-let [value (or (get-in config [:clock :fixed])
                       (:clock-fixed config))]
    (Instant/parse value)))

(defn- clock-instant [state]
  (let [config (config! state)
        fixed (fixed-instant config)
        cfg-clock (or (:clock config) (get-in config [:clock :clock]))]
    (cond
      fixed fixed
      (instance? Clock cfg-clock) (clock/now cfg-clock)
      :else (clock/now))))

(defn- now-iso [state]
  (clock/->iso-string (clock-instant state)))

(defn- now-stamp [state]
  (clock/stamp (clock-instant state)))

(defn- ensure-run-id-counter! [state]
  (let [config-atom (:config state)
        config @config-atom]
    (or (:run-id-counter config)
        (let [counter (atom 0)]
          (swap! config-atom assoc :run-id-counter counter)
          counter))))

(defn- run-id-fn [state]
  (let [config (config! state)]
    (or (:run-id-fn config)
        (get-in config [:transport :run-id-fn])
        (when-let [seed (or (:run-id-seed config)
                            (get-in config [:transport :run-id-seed]))]
          (let [counter (ensure-run-id-counter! state)]
            (fn []
              (format "%s-%04d" seed (swap! counter inc)))))
        random-run-id)))

(defn- proof-id-fn [state]
  (let [config (config! state)]
    (or (:proof-id-fn config)
        (get-in config [:transport :proof-id-fn])
        (when (or (:run-id-seed config)
                  (get-in config [:transport :run-id-seed]))
          (fn [run-id] (str "PROOF-" run-id))))))

(defn- append-history! [state entry]
  (swap! (:history state)
         (fn [hist]
           (let [next (conj hist entry)]
             (if (> (count next) history-limit)
               (vec (take-last history-limit next))
               next)))))

(defn- router-log-fn [state]
  (fn [entry]
    (append-history! state (merge {:stamp (now-stamp state)} entry))))

(defn- ensure-router! [state]
  (when-not @(:router state)
    (reset! (:router state)
            (router/create {:adapters @(:adapters state)
                            :log! (router-log-fn state)
                            :run-id-fn (run-id-fn state)})))
  @(:router state))

(defn clients-view [state]
  (let [repl-mode (name (get-in @(:config state) [:repl :mode] :off))]
    (->> @(:clients state)
         (map (fn [[id {:keys [name connected? last-stamp caps remote-addr]}]]
                {:id id
                 :name (or name "anonymous")
                 :repl-mode repl-mode
                 :remote remote-addr
                 :connected connected?
                 :caps (sort caps)
                 :last-activity (:at last-stamp)}))
         (sort-by :id)
         vec)))

(defn history-view [state]
  @(:history state))

(defn- repl-config [state]
  (get (config! state) :repl {:mode :off}))

(defn- dsl-bindings [state]
  {'now (fn [] (now-iso state))
   'clients (fn [] (clients-view state))
   'history (fn [] (history-view state))
   'links (fn [] (semantics/suggest-links (history-view state)))
   'check! checks/check!
   'tatami-start tatami/start
   'tatami-log tatami/log
   'tatami-convolve tatami/convolve
   'tatami-close tatami/close
   'tatami-status tatami/status})

(defn- lookup-client-by-name [state target]
  (->> @(:clients state)
       (filter (fn [[_ {:keys [name]}]] (= name target)))
       (map second)
       first))

(defn- send-json! [client payload]
  (when-let [channel (:channel client)]
    (http/send! channel (json/encode payload))))

(defn- deliver-message! [state from payload]
  (let [{:keys [to body]} payload
        targets (if (seq to)
                  (keep #(lookup-client-by-name state %) to)
                  (->> @(:clients state)
                       vals
                       (remove #(= (:id from) (:id %)))))
        sender-name (or (:name from) (:id from) "unknown")
        note {:type "message"
              :from sender-name
              :body body
              :stamp (now-stamp state)}]
    (doseq [client targets]
      (send-json! client note))
    {:reply {:ok true
             :type "message"
             :delivered (mapv #(or (:name %) (:id %)) targets)}}))

(defn- ensure-client! [state client-id]
  (when-let [client (get @(:clients state) client-id)]
    client))

(defn- update-client! [state client-id f]
  (swap! (:clients state)
         (fn [clients]
           (if-let [client (get clients client-id)]
             (assoc clients client-id (f client))
             clients))))

(defn- with-timeout [ms f]
  (let [result (promise)]
    (future
      (try
        (deliver result {:value (f)})
        (catch Exception ex
          (deliver result {:error ex}))))
    (let [res (deref result ms {:timeout true})]
      (cond
        (= res {:timeout true}) {:error (ex-info "timeout" {:ms ms})}
        (:error res) {:error (:error res)}
        :else {:value (:value res)}))))

(defn- handle-eval [state client payload]
  (let [config (repl-config state)
        request {:code (:code payload)
                 :mode (some-> (:mode payload) keyword)
                 :token (:token payload)
                 :dsl (dsl-bindings state)
                 :remote-addr (:remote-addr client)}
        outcome (with-timeout repl-timeout-ms #(repl/execute config request))
        base {:type "eval"}
        run-id ((run-id-fn state))
        mode-name (fn [result]
                    (name (or (:mode result)
                              (:mode config)
                              :off)))]
    (cond
      (:error outcome)
      (let [err ^Exception (:error outcome)
            data (ex-data err)
            code (if (= "timeout" (.getMessage err))
                   "eval-timeout"
                   (or (:err data) "eval-error"))]
        {:reply (assoc base :ok false :err code)})

      :else
      (let [result (:value outcome)
            reply-base (assoc base :run-id run-id :mode (mode-name result))]
        (if (:ok result)
          (do
            (append-history! state {:client (:id client)
                                    :type :eval
                                    :stamp (now-stamp state)
                                    :mode (:mode result)
                                    :code (:code payload)
                                    :run-id run-id
                                    :output (:result result)})
            {:reply (assoc reply-base :ok true :result (:result result))})
          {:reply (cond-> (assoc reply-base :ok false :err (:err result))
                    (:message result) (assoc :message (:message result)))})))))

(defn- record-history! [state entry]
  (append-history! state entry)
  entry)

(defn- handle-workday [state client envelope]
  (let [payload (:payload envelope)
        run-id ((run-id-fn state))
        futon1-config (get @(:config state) :futon1)
        futon2-config (get @(:config state) :futon2)
        futon1-enabled? (and (get futon1-config :enabled?)
                             (seq (get futon1-config :api-base)))
        submission (workday/submit! {:payload payload
                                     :client client
                                     :msg-id (:msg-id envelope)
                                     :run-id run-id
                                     :source :ws
                                     :log? (not futon1-enabled?)})]
    (if (:ok submission)
      (let [entry (:entry submission)
            check-request (workday/check-request payload entry)
            check-outcome (when check-request (checks/check! check-request))
            _ (record-history! state {:client (:id client)
                                      :type :workday
                                      :stamp (now-stamp state)
                                      :run-id run-id
                                      :workday/id (:workday/id entry)
                                      :activity (:workday/activity entry)})
            _ (f1-bridge/record-workday! futon1-config entry)
            _ (when check-outcome
                (f1-bridge/record-check! futon1-config check-outcome))
            _ (when check-outcome
                (futon2-bridge/record-check! futon2-config check-outcome))
            base {:ok true
                  :type "workday"
                  :run-id run-id
                  :workday/id (:workday/id entry)
                  :log (:log-path submission)
                  :blocked-by (:blocked-by submission)
                  :note (:note submission)}]
        {:reply (cond-> base
                  check-outcome (assoc :check check-outcome))})
      {:reply {:ok false
               :type "workday"
               :err (:err submission)
               :details (:details submission)}})))

(defn- prepare-check-request [state client envelope]
  (let [origin-base (cond-> {:source :ws
                             :client-id (:id client)}
                      (:msg-id envelope) (assoc :msg-id (:msg-id envelope)))
        run-id ((run-id-fn state))
        proof-id (when-let [id-fn (proof-id-fn state)]
                   (id-fn run-id))]
    (-> (:payload envelope)
        (assoc :run-id run-id
               :proof/recorded (now-iso state))
        (cond-> proof-id
          (assoc :proof/id proof-id))
        (update :origin #(merge origin-base %)))))

(defn- handle-check [state client envelope]
  (let [request (prepare-check-request state client envelope)
        result (checks/check! request)
        run-id (:run-id request)
        futon1-config (get @(:config state) :futon1)
        futon2-config (get @(:config state) :futon2)]
    (if (:ok result)
      (let [proof (:proof result)]
        (record-history! state {:client (:id client)
                                :type :check
                                :stamp (now-stamp state)
                                :run-id run-id
                                :pattern (:pattern/id proof)
                                :status (:status result)})
        (f1-bridge/record-check! futon1-config result)
        (futon2-bridge/record-check! futon2-config result)
        {:reply {:ok true
                 :type "check"
                 :run-id run-id
                 :status (:status result)
                 :missing (:missing result)
                 :derived (:derived/tasks result)
                 :proof proof
                 :log (:log-path result)}})
      {:reply {:ok false
               :type "check"
               :err (:err result)
               :details (:details result)}})))

(defn- route-via-router [state client-id type envelope]
  (let [client (ensure-client! state client-id)]
    (if-not client
      {:reply {:ok false :err "unknown-client"}}
      (router/dispatch (ensure-router! state) type client envelope))))

(defn- dispatch-message [state client-id envelope]
  (let [type (keyword (or (:type envelope) "unknown"))]
    (case type
      :hello (let [client (ensure-client! state client-id)
                   hello (or (:payload envelope) envelope)]
               (if-not client
                 {:reply {:ok false :type "ack" :err "unknown-client"}}
                 (let [response (router/dispatch (ensure-router! state) :hello client envelope)]
                   (when (get-in response [:reply :ok])
                     (update-client! state client-id
                                     (fn [c]
                                       (assoc c
                                               :name (or (:client hello) "anonymous")
                                               :caps (set (or (:caps hello) []))
                                               :last-stamp (now-stamp state)))))
                   response)))
      :eval (let [client (ensure-client! state client-id)]
              (if-not client
                {:reply {:ok false :type "eval" :err "unknown-client"}}
                (handle-eval state client (:payload envelope))))
      :query {:reply {:ok false :err "query-disabled"}}
      :message (let [client (ensure-client! state client-id)]
                 (if-not client
                   {:reply {:ok false :err "unknown-client"}}
                   (deliver-message! state client (:payload envelope))))
      :bye (do
             (update-client! state client-id #(assoc % :connected? false :last-stamp (now-stamp state)))
             {:reply {:ok true :type "bye"}
              :close? true})
      :event (route-via-router state client-id type envelope)
      :session-close (route-via-router state client-id type envelope)
      :export (route-via-router state client-id type envelope)
      :run (route-via-router state client-id type envelope)
      :status (route-via-router state client-id type envelope)
      :gap-report (route-via-router state client-id type envelope)
      :trail-capture (route-via-router state client-id type envelope)
      :hx/artifact-register (route-via-router state client-id type envelope)
      :hx/anchors-upsert (route-via-router state client-id type envelope)
      :hx/link-suggest (route-via-router state client-id type envelope)
      :hx/link-accept (route-via-router state client-id type envelope)
      :hx/link-reject (route-via-router state client-id type envelope)
      :hx/list-artifacts (route-via-router state client-id type envelope)
      :hx/list-links (route-via-router state client-id type envelope)
      :hx/candidates (route-via-router state client-id type envelope)
      :workday (let [client (ensure-client! state client-id)]
                 (if-not client
                   {:reply {:ok false :type "workday" :err "unknown-client"}}
                   (handle-workday state client envelope)))
      :check (let [client (ensure-client! state client-id)]
               (if-not client
                 {:reply {:ok false :type "check" :err "unknown-client"}}
                 (handle-check state client envelope)))
      {:reply {:ok false :err "unsupported-type"}})))

(defn apply-envelope!
  "Expose the dispatcher for tests and internal tooling."
  [state client-id envelope]
  (dispatch-message state client-id envelope))

(defn- make-client [state channel {:keys [remote-addr]}]
  {:id (random-client-id)
   :channel channel
   :name "anonymous"
   :caps #{"eval" "message"}
   :connected? true
   :last-stamp (now-stamp state)
   :remote-addr remote-addr
   :inbox (async/chan inbox-size)})

(defn- register-client! [state client]
  (swap! (:clients state) assoc (:id client) client)
  client)

(defn- unregister-client! [state client-id]
  (when-let [client (get @(:clients state) client-id)]
    (when-let [inbox (:inbox client)]
      (async/close! inbox))
    (swap! (:clients state) dissoc client-id)
    (append-history! state {:client client-id :type :disconnect :stamp (now-stamp state)})
    client))

(defn- parse-json [raw]
  (json/parse-string raw true))

(defn- process-raw [state client-id raw]
  (try
    (when (> (.length ^String raw) max-frame-bytes)
      (throw (ex-info "payload-too-large" {:len (count raw)})))
    (let [envelope (parse-json raw)
          result (dispatch-message state client-id envelope)]
      result)
    (catch Exception ex
      (let [data (ex-data ex)
            err (or (:err data) (.getMessage ex))
            details (:details data)]
        {:reply (cond-> {:ok false :err err}
                  details (assoc :details details))}))))

(defn ingest-string!
  "Process a raw JSON line as if it were sent via HTTP fallback."
  [state raw]
  (:reply (process-raw state "ingest" raw)))

(defn ingest-lines!
  [state raws]
  (map #(ingest-string! state %) raws))

(defn- start-client-loop! [state client]
  (async/go-loop []
    (when-let [raw (async/<! (:inbox client))]
      (let [{:keys [reply close?]} (process-raw state (:id client) raw)]
        (when reply
          (send-json! client reply))
        (when close?
          (when-let [channel (:channel client)]
            (http/close channel))))
      (recur))))

(defn- websocket-handler [state request]
  (http/with-channel request channel
    (let [remote (some-> (:remote-addr request) str)
          client (register-client! state (make-client state channel {:remote-addr (or remote "unknown")}))]
      (start-client-loop! state client)
      (http/on-close channel (fn [_]
                              (unregister-client! state (:id client))))
      (http/on-receive channel (fn [raw]
                                 (if (async/offer! (:inbox client) raw)
                                   nil
                                   (send-json! client {:ok false :err "over-quota"})))))))

(defn- handle-ingest [state request]
  (let [body (slurp (:body request))
        frames (remove str/blank? (str/split-lines body))
        responses (map (fn [raw]
                         (-> (process-raw state "http-client" raw)
                             :reply))
                       frames)]
    {:status 200
     :headers {"content-type" "application/json"}
     :body (->> responses
                (map json/encode)
                (str/join "\n"))}))

;; Forward declarations for HTTP handlers
(declare json-response)

(defn- ensure-seq [value]
  (cond
    (nil? value) nil
    (sequential? value) value
    :else [value]))

(defn- ensure-text [value]
  (cond
    (nil? value) nil
    (string? value) value
    :else (pr-str value)))

(defn- http-origin [request payload]
  (let [origin (when (map? (:origin payload)) (:origin payload))
        remote (some-> (:remote-addr request) str)]
    (merge {:source :http
            :remote-addr remote}
           origin)))

(defn- normalize-check-payload [payload]
  (let [pattern-id (or (:pattern/id payload)
                       (:pattern-id payload)
                       (:pattern payload))
        context (ensure-text (:context payload))
        evidence (ensure-seq (:evidence payload))
        sigils (ensure-seq (:sigils payload))
        prototypes (ensure-seq (:prototypes payload))]
    (-> payload
        (dissoc :pattern-id :pattern)
        (cond-> pattern-id (assoc :pattern/id pattern-id))
        (cond-> context (assoc :context context))
        (cond-> evidence (assoc :evidence evidence))
        (cond-> sigils (assoc :sigils sigils))
        (cond-> prototypes (assoc :prototypes prototypes)))))

(defn- normalize-workday-payload [payload]
  (let [activity (ensure-text (:activity payload))
        evidence (ensure-seq (:evidence payload))
        sigils (ensure-seq (:sigils payload))
        prototypes (ensure-seq (:prototypes payload))
        tags (ensure-seq (:tags payload))]
    (-> payload
        (cond-> activity (assoc :activity activity))
        (cond-> evidence (assoc :evidence evidence))
        (cond-> sigils (assoc :sigils sigils))
        (cond-> prototypes (assoc :prototypes prototypes))
        (cond-> tags (assoc :tags tags)))))

(defn- parse-json-body [request]
  (let [body (slurp (:body request))]
    (when-not (str/blank? body)
      (json/parse-string body true))))

(defn- handle-check-http [state request]
  (try
    (let [payload (normalize-check-payload (or (parse-json-body request) {}))
          run-id ((run-id-fn state))
          proof-id (when-let [id-fn (proof-id-fn state)]
                     (id-fn run-id))
          request (-> payload
                      (assoc :run-id run-id
                             :proof/recorded (now-iso state)
                             :origin (http-origin request payload))
                      (cond-> proof-id (assoc :proof/id proof-id)))
          result (checks/check! request)
          futon1-config (get @(:config state) :futon1)
          futon2-config (get @(:config state) :futon2)]
      (when (:ok result)
        (record-history! state {:client "http"
                                :type :check
                                :stamp (now-stamp state)
                                :run-id run-id
                                :pattern (get-in result [:proof :pattern/id])
                                :status (:status result)}))
      (f1-bridge/record-check! futon1-config result)
      (futon2-bridge/record-check! futon2-config result)
      (if (:ok result)
        (json-response 200 {:ok true
                            :type "check"
                            :run-id run-id
                            :status (:status result)
                            :missing (:missing result)
                            :derived (:derived/tasks result)
                            :proof (:proof result)
                            :log (:log-path result)})
        (json-response 200 {:ok false
                            :type "check"
                            :err (:err result)
                            :details (:details result)})))
    (catch Exception e
      (json-response 500 {:ok false
                          :type "check"
                          :err "check-failed"
                          :details (.getMessage e)}))))

(defn- handle-workday-submit [state request]
  (try
    (let [payload (normalize-workday-payload (or (parse-json-body request) {}))
          run-id ((run-id-fn state))
          futon1-config (get @(:config state) :futon1)
          futon2-config (get @(:config state) :futon2)
          futon1-enabled? (and (get futon1-config :enabled?)
                               (seq (get futon1-config :api-base)))
          client {:id "http-client"
                  :name "http-client"
                  :remote-addr (some-> (:remote-addr request) str)}
          submission (workday/submit! {:payload payload
                                       :client client
                                       :msg-id (:msg-id payload)
                                       :run-id run-id
                                       :source :http
                                       :log? (not futon1-enabled?)})]
      (if (:ok submission)
        (let [entry (:entry submission)
              check-request (workday/check-request payload entry)
              check-outcome (when check-request (checks/check! check-request))
              _ (record-history! state {:client "http"
                                        :type :workday
                                        :stamp (now-stamp state)
                                        :run-id run-id
                                        :workday/id (:workday/id entry)
                                        :activity (:workday/activity entry)})
              _ (f1-bridge/record-workday! futon1-config entry)
              _ (when check-outcome
                  (f1-bridge/record-check! futon1-config check-outcome))
              _ (when check-outcome
                  (futon2-bridge/record-check! futon2-config check-outcome))
              base {:ok true
                    :type "workday"
                    :run-id run-id
                    :workday/id (:workday/id entry)
                    :log (:log-path submission)
                    :blocked-by (:blocked-by submission)
                    :note (:note submission)}]
          (json-response 200 (cond-> base
                               check-outcome (assoc :check check-outcome))))
        (json-response 200 {:ok false
                            :type "workday"
                            :err (:err submission)
                            :details (:details submission)})))
    (catch Exception e
      (json-response 500 {:ok false
                          :type "workday"
                          :err "workday-submit-failed"
                          :details (.getMessage e)}))))

(defn- plaintext-response [status body]
  {:status status
   :headers {"content-type" "text/plain"}
   :body body})

(defn- json-response [status body]
  {:status status
   :headers {"content-type" "application/json"}
   :body (json/encode body)})

(defn- git-head [repo-root]
  (let [{:keys [exit out err]} (shell/sh "git" "-C" repo-root "rev-parse" "HEAD")]
    (when-not (zero? exit)
      (throw (ex-info "git-head-failed" {:error (str/trim err)})))
    (str/trim out)))

(defn- hud-certificates [state {:keys [certify-commit repo-root]}]
  (when certify-commit
    (let [repo-root (or repo-root (get-in @(:config state) [:repo :root]) (System/getProperty "user.dir"))
          commit (git-head repo-root)]
      [{:certificate/type :git/commit
        :certificate/ref commit
        :certificate/repo repo-root}])))

(defn- session-path [repo-root session-id]
  (io/file repo-root "lab" "sessions" (str session-id ".edn")))

(defn- read-session [repo-root session-id]
  (let [path (session-path repo-root session-id)]
    (when (.exists path)
      (pc/read-session-file (str path)))))

(defn- summarize-aif-event [event]
  (let [payload (:payload event)
        result (:aif/result payload)
        aif (:aif result)]
    {:kind (:aif/kind payload)
     :at (:at event)
     :chosen (:chosen result)
     :tau (or (:tau aif) (:tau-updated aif))
     :g-chosen (:G-chosen aif)
     :evidence-score (:evidence-score aif)
     :evidence-delta (:evidence-delta aif)
     :evidence-counts (:evidence-counts aif)
     :prediction-error (:prediction-error aif)
     :belief-delta (:belief-delta aif)}))

(defn- summarize-aif-tap [event]
  (let [payload (:payload event)
        aif (:aif payload)]
    {:kind (or (:event payload) :tap)
     :at (:at event)
     :chosen (:chosen payload)
     :tau (or (:tau aif) (:tau-updated aif))
     :g-chosen (:G-chosen aif)
     :evidence-score (:evidence-score aif)
     :evidence-delta (:evidence-delta aif)
     :evidence-counts (:evidence-counts aif)
     :prediction-error (:prediction-error aif)
     :belief-delta (:belief-delta aif)}))

(defn- summarize-pattern-action [event]
  (let [payload (:payload event)]
    {:pattern-id (:pattern/id payload)
     :action (:pattern/action payload)
     :note (:pattern/note payload)
     :at (:at event)}))

(defn- summarize-pattern-selection [event]
  (let [psr (get-in event [:payload :psr])]
    {:psr-id (:psr/id psr)
     :chosen (:chosen psr)
     :candidates (:candidates psr)
     :decision-id (:decision/id psr)
     :at (:at event)}))

(defn- summarize-pattern-use [event]
  (let [pur (get-in event [:payload :pur])]
    {:pur-id (:pur/id pur)
     :pattern-id (:pattern/id pur)
     :decision-id (:decision/id pur)
     :at (:at event)}))

(defn- aif-live [session]
  (let [events (or (:events session) [])
        last-summary (last (filter #(= :aif/summary (:event/type %)) events))
        last-tap (last (filter #(= :aif/tap (:event/type %)) events))
        last-action (last (filter #(= :pattern/action (:event/type %)) events))
        last-selection (last (filter #(= :pattern/selection-claimed (:event/type %)) events))
        last-use (last (filter #(= :pattern/use-claimed (:event/type %)) events))]
    (when (or last-summary last-tap last-action last-selection last-use)
      {:session-id (:session/id session)
       :summary (cond
                  last-summary (summarize-aif-event last-summary)
                  last-tap (summarize-aif-tap last-tap))
       :last-action (when last-action (summarize-pattern-action last-action))
       :last-selection (when last-selection (summarize-pattern-selection last-selection))
       :last-use (when last-use (summarize-pattern-use last-use))})))

(defn- handle-hud-format [state request]
  (try
    (let [payload (or (parse-json-body request) {})
          intent (:intent payload)
          prototypes (:prototypes payload)
          sigils (:sigils payload)
          session-id (:session-id payload)
          mana (:mana payload)
          musn-help (:musn-help payload)
          musn-help-force (:musn-help-force payload)
          musn-hud (:musn-hud payload)
          musn-hud-force (:musn-hud-force payload)
          repo-root (or (:repo-root payload)
                        (get-in @(:config state) [:repo :root])
                        (System/getProperty "user.dir"))
          limit (or (:pattern-limit payload) (:limit payload) 4)
          certs (hud-certificates state (assoc payload :repo-root repo-root))
          live (when session-id
                 (some-> (read-session repo-root session-id)
                         aif-live))
          ;; Scribe integration: enrich intent with recent conversation turns
          turn-context (when session-id
                         (musn-svc/turns->context session-id 6))
          enriched-intent (if (and turn-context (seq turn-context))
                            (str intent " [Recent: "
                                 (subs turn-context 0 (min 300 (count turn-context)))
                                 (when (> (count turn-context) 300) "...")
                                 "]")
                            intent)
          musn-help (cond
                      (true? musn-help-force) true
                      (true? musn-help)
                      (let [help-state (:hud-help state)
                            sid (some-> session-id str str/trim)]
                        (when (seq sid)
                          (let [seen? (contains? (:sessions @help-state) sid)]
                            (when-not seen?
                              (swap! help-state update :sessions conj sid))
                            (not seen?)))))
          musn-hud (cond
                     (true? musn-hud-force) true
                     (true? musn-hud) true
                     :else false)
          hud (hud/build-hud {:intent enriched-intent
                              :prototypes prototypes
                              :sigils sigils
                              :pattern-limit limit
                              :mana mana
                              :musn-help musn-help
                              :musn-hud musn-hud
                              :certificates certs})
          hud (if live
                (assoc hud :aif-live live)
                hud)
          hud (if turn-context
                (assoc hud :scribe-context? true)
                hud)
          prompt (hud/hud->prompt-block hud)]
      (json-response 200 {:ok true
                          :hud hud
                          :prompt prompt}))
    (catch Exception e
      (json-response 500 {:ok false
                          :error "hud-format-failed"
                          :detail (.getMessage e)}))))

(defn handler [state request]
  (cond
    (= [:get "/healthz"] [(:request-method request) (:uri request)])
    (plaintext-response 200 "ok")
    (and (= (:uri request) "/fulab/hud/format") (= (:request-method request) :post))
    (handle-hud-format state request)
    (and (= (:uri request) "/musn/ws") (= (:request-method request) :get))
    (websocket-handler state request)
    (and (= (:uri request) "/musn/check") (= (:request-method request) :post))
    (handle-check-http state request)
    (and (= (:uri request) "/musn/workday/submit") (= (:request-method request) :post))
    (handle-workday-submit state request)
    (and (= (:uri request) "/musn/ingest") (= (:request-method request) :post))
    (handle-ingest state request)
    :else
    (plaintext-response 404 "not-found")))

(defn start!
  ([state]
   (start! state {}))
  ([state {:keys [port]}]
   (let [port (or port (get-in state [:config :transport-port] 5050))
         stop-fn (http/run-server (partial handler state) {:port port})]
     (swap! (:config state) assoc :transport-port port)
     stop-fn)))

(defn stop! [stop-fn]
  (when stop-fn
    (stop-fn)))
