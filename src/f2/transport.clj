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
            [futon3.lab.enrichment :as enrichment]
            [futon3.futon2-bridge :as futon2-bridge]
            [futon3.forum.http :as forum-http]
            [futon3.forum.service :as forum-svc]
            [futon3.forum.ws :as forum-ws]
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
          ;; Pull mana from MUSN session if not provided in payload
          session-mana (when (and session-id (not mana))
                         (when-let [entry (musn-svc/get-session session-id)]
                           (when-let [mana-atom (:mana entry)]
                             (let [m @mana-atom]
                               (select-keys m [:budget :balance :earned :spent])))))
          mana (or mana session-mana)
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

;; =============================================================================
;; Live Notebook Viewer - SSE streaming of conversation turns
;; Future: WebSocket support via /musn/ws for bidirectional features
;; =============================================================================

(defn- serve-static-file [path content-type]
  (if-let [resource (io/resource path)]
    {:status 200
     :headers {"content-type" content-type}
     :body (slurp resource)}
    (plaintext-response 404 "not-found")))

(defn- event->sse-data [event]
  (json/encode (-> event
                   (update :event/type #(when % (name %)))
                   (update-in [:payload :role] #(when % (name %))))))

(defn- get-latest-plan-diagram
  "Get the most recent plan diagram from a session."
  [session-id]
  (when-let [events (musn-svc/recent-scribe-events session-id 1000)]
    (->> events
         (filter #(= :turn/plan (:event/type %)))
         last
         :payload
         :diagram)))

(defn- handle-plan-diagram [session-id]
  (if-let [diagram (get-latest-plan-diagram session-id)]
    {:status 200
     :headers {"content-type" "text/html; charset=utf-8"}
     :body (str "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>Plan Diagram - " session-id "</title>
  <style>
    body {
      font-family: 'SF Mono', 'Fira Code', monospace;
      background: #1a1a2e;
      color: #eaeaea;
      margin: 0;
      padding: 20px;
      min-height: 100vh;
      display: flex;
      flex-direction: column;
      align-items: center;
    }
    h1 {
      color: #f9a825;
      font-size: 1.2rem;
      margin-bottom: 20px;
    }
    .mermaid {
      background: #16213e;
      padding: 30px;
      border-radius: 8px;
      border: 1px solid #3a3a5a;
    }
    .back-link {
      margin-top: 20px;
      color: #6ec1e4;
      text-decoration: none;
    }
    .back-link:hover { text-decoration: underline; }
  </style>
</head>
<body>
  <h1>Plan: " session-id "</h1>
  <div class=\"mermaid\">
" diagram "
  </div>
  <a class=\"back-link\" href=\"/fulab/notebook/" session-id "\">← Back to notebook</a>
  <script src=\"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js\"></script>
  <script>
    mermaid.initialize({
      startOnLoad: true,
      theme: 'dark',
      themeVariables: {
        primaryColor: '#2d2d44',
        primaryTextColor: '#eaeaea',
        primaryBorderColor: '#6ec1e4',
        lineColor: '#c792ea',
        secondaryColor: '#1e3a5f',
        tertiaryColor: '#16213e'
      }
    });
  </script>
</body>
</html>")}
    (plaintext-response 404 "no plan diagram found")))

(defn- keyword->str [kw]
  "Convert keyword to string, preserving namespace."
  (when kw
    (if-let [ns (namespace kw)]
      (str ns "/" (name kw))
      (name kw))))

(defn- normalize-event-for-stream
  "Normalize event for stream transmission."
  [event]
  (-> event
      (update :event/type keyword->str)
      (update-in [:payload :role] #(when % (name %)))))

(defn- handle-notebook-stream [state request session-id]
  ;; SSE endpoint for live notebook updates
  (http/with-channel request channel
    (http/send! channel
                {:status 200
                 :headers {"content-type" "text/event-stream"
                           "cache-control" "no-cache"
                           "connection" "keep-alive"}}
                false)
    ;; Send initial state
    (let [events (or (musn-svc/recent-scribe-events session-id 100)
                     [])
          init-data (json/encode {:type "init"
                                  :session-id session-id
                                  :events (mapv normalize-event-for-stream events)})]
      (http/send! channel (str "data: " init-data "\n\n") false))
    ;; Poll for new events (simple approach; could use core.async watch later)
    (let [poll-interval 1000
          last-count (atom (count (or (musn-svc/scribe-events session-id) [])))]
      (future
        (try
          (loop []
            (Thread/sleep poll-interval)
            (when (http/open? channel)
              (let [events (or (musn-svc/scribe-events session-id) [])
                    current-count (count events)]
                (when (> current-count @last-count)
                  (let [new-events (drop @last-count events)]
                    (doseq [event new-events]
                      (let [data (json/encode {:type "turn"
                                               :event (normalize-event-for-stream event)})]
                        (http/send! channel (str "data: " data "\n\n") false))))
                  (reset! last-count current-count)))
              (recur)))
          (catch Exception e
            (println "SSE stream error:" (.getMessage e))))))))

;; =============================================================================
;; WebSocket Session Stream - Real-time notebook events via WebSocket
;; =============================================================================

(defn- handle-session-ws
  "WebSocket endpoint for streaming MUSN session events.
   Similar to SSE notebook stream but uses WebSocket for bidirectional support."
  [state request session-id]
  (http/with-channel request channel
    ;; Send initial state with recent events
    (let [events (or (musn-svc/recent-scribe-events session-id 50) [])
          init-msg (json/encode {:type "init"
                                 :session-id session-id
                                 :event-count (count events)
                                 :events (mapv normalize-event-for-stream events)})]
      (http/send! channel init-msg false))

    ;; Handle client messages (filter commands, ping)
    (http/on-receive channel
      (fn [raw]
        (try
          (let [msg (json/parse-string raw true)
                msg-type (:type msg)]
            (case msg-type
              "ping" (http/send! channel (json/encode {:type "pong"
                                                       :at (now-iso state)}) false)
              "filter" nil ;; Future: filter by event type
              nil))
          (catch Exception _
            nil))))

    ;; Poll for new events and push to client
    (let [poll-interval 500
          last-count (atom (count (or (musn-svc/scribe-events session-id) [])))]
      (future
        (try
          (loop []
            (Thread/sleep poll-interval)
            (when (http/open? channel)
              (let [events (or (musn-svc/scribe-events session-id) [])
                    current-count (count events)]
                (when (> current-count @last-count)
                  (let [new-events (drop @last-count events)]
                    (doseq [event new-events]
                      (let [ws-event {:type "event"
                                      :session-id session-id
                                      :event (normalize-event-for-stream event)}]
                        (http/send! channel (json/encode ws-event) false))))
                  (reset! last-count current-count)))
              (recur)))
          (catch Exception e
            (println "WebSocket stream error:" (.getMessage e))))))

    ;; Handle close
    (http/on-close channel
      (fn [_status]
        nil))))

;; =============================================================================
;; Claude Code JSONL Streaming - Real-time file tailing via WebSocket
;; =============================================================================

(defonce claude-stream-watchers (atom {}))

(defn- parse-claude-jsonl-line
  "Parse a single JSONL line from Claude Code transcript."
  [line]
  (try
    (let [entry (json/parse-string line true)
          msg-type (:type entry)
          message (:message entry)
          timestamp (:timestamp entry)]
      (case msg-type
        "user"
        (let [content (or (:content message) (when (string? message) message) "")]
          {:type "user"
           :timestamp timestamp
           :text (if (string? content)
                   content
                   ;; Handle array content
                   (->> content
                        (filter #(= "text" (:type %)))
                        (map :text)
                        (str/join "\n")))})

        "assistant"
        (let [content (or (:content message) (when (string? message) message) "")]
          {:type "assistant"
           :timestamp timestamp
           :text (if (string? content)
                   content
                   (->> content
                        (filter #(= "text" (:type %)))
                        (map :text)
                        (str/join "\n")))})

        "tool_use"
        {:type "tool_use"
         :timestamp timestamp
         :tool-name (or (:name message) "unknown")
         :input (pr-str (or (:input message) {}))}

        "tool_result"
        {:type "tool_result"
         :timestamp timestamp
         :content (str (or (:content message) ""))}

        "summary"
        {:type "summary"
         :timestamp timestamp
         :text "[Context compacted]"}

        ;; Default - include raw for debugging
        {:type msg-type
         :timestamp timestamp
         :raw entry}))
    (catch Exception _
      nil)))

(defn- parse-query-string
  "Parse query string into map of params."
  [query-string]
  (when query-string
    (into {}
          (for [pair (str/split query-string #"&")
                :let [[k v] (str/split pair #"=" 2)]
                :when k]
            [k (java.net.URLDecoder/decode (or v "") "UTF-8")]))))

(defn- scan-claude-projects
  "Scan ~/.claude/projects/ for JSONL session files.
   Returns list of session maps sorted by modification time (newest first)."
  []
  (let [claude-dir (io/file (System/getProperty "user.home") ".claude" "projects")]
    (when (.exists claude-dir)
      (->> (file-seq claude-dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".jsonl")))
           (map (fn [f]
                  (let [path (.getAbsolutePath f)
                        parent-name (.getName (.getParentFile f))
                        ;; Extract project name from parent dir (e.g., "-home-joe-code-futon3")
                        project (-> parent-name
                                    (str/replace #"^-" "")
                                    (str/replace #"-" "/"))
                        modified (.lastModified f)
                        size (.length f)
                        ;; Consider active if modified in last hour
                        active? (> modified (- (System/currentTimeMillis) 3600000))]
                    {:id (str/replace (.getName f) #"\.jsonl$" "")
                     :project project
                     :path path
                     :modified (str (java.time.Instant/ofEpochMilli modified))
                     :size-kb (quot size 1024)
                     :active active?})))
           (sort-by :modified #(compare %2 %1))))))

(defn- scan-lab-raw
  "Scan lab/raw/ for archived session JSON files.
   Returns list of session maps sorted by modification time (newest first)."
  [state]
  (let [config (config! state)
        lab-root (or (:lab-root config)
                     (io/file "lab" "raw"))]
    (let [lab-dir (if (instance? java.io.File lab-root)
                    lab-root
                    (io/file lab-root))]
      (when (.exists lab-dir)
        (->> (.listFiles lab-dir)
             (filter #(and (.isFile %)
                           (str/ends-with? (.getName %) ".json")))
             (map (fn [f]
                    (let [path (.getAbsolutePath f)
                          modified (.lastModified f)
                          size (.length f)]
                      {:id (str/replace (.getName f) #"\.json$" "")
                       :path path
                       :modified (str (java.time.Instant/ofEpochMilli modified))
                       :size-kb (quot size 1024)
                       :archived true})))
             (sort-by :modified #(compare %2 %1)))))))

(defn- handle-lab-sessions-active
  "List active Claude Code sessions."
  [_state _request]
  (try
    (let [sessions (scan-claude-projects)]
      (json-response 200 {:ok true :sessions (vec sessions)}))
    (catch Exception e
      (json-response 500 {:ok false :err "scan-failed" :detail (.getMessage e)}))))

(defn- handle-lab-sessions-archived
  "List archived lab sessions."
  [state _request]
  (try
    (let [sessions (scan-lab-raw state)]
      (json-response 200 {:ok true :sessions (vec sessions)}))
    (catch Exception e
      (json-response 500 {:ok false :err "scan-failed" :detail (.getMessage e)}))))

(defn- handle-claude-stream-ws
  "WebSocket endpoint for streaming Claude Code JSONL file changes.
   Query params:
   - path: path to JSONL file (required)
   - tail: number of recent lines to send on connect (default 50)"
  [state request]
  (let [params (or (:query-params request)
                   (parse-query-string (:query-string request))
                   {})
        jsonl-path (get params "path")
        tail-lines (or (some-> (get params "tail") Integer/parseInt) 50)]
    (if (or (nil? jsonl-path) (not (.exists (io/file jsonl-path))))
      (json-response 400 {:ok false :err "invalid-path" :path jsonl-path})
      (http/with-channel request channel
        (let [file (io/file jsonl-path)
              watcher-id (str (UUID/randomUUID))
              stop-flag (atom false)]

          ;; Register watcher
          (swap! claude-stream-watchers assoc watcher-id
                 {:path jsonl-path :channel channel :stop-flag stop-flag})

          ;; Send initial tail
          (try
            (with-open [rdr (io/reader file)]
              (let [all-lines (vec (line-seq rdr))
                    recent (take-last tail-lines all-lines)
                    events (->> recent
                                (map parse-claude-jsonl-line)
                                (filter some?)
                                (filter #(seq (:text %))))]
                (http/send! channel
                  (json/encode {:type "init"
                                :path jsonl-path
                                :line-count (count all-lines)
                                :events (vec events)})
                  false)))
            (catch Exception e
              (http/send! channel
                (json/encode {:type "error" :err "read-failed" :detail (.getMessage e)})
                false)))

          ;; Handle client messages
          (http/on-receive channel
            (fn [raw]
              (try
                (let [msg (json/parse-string raw true)]
                  (case (:type msg)
                    "ping" (http/send! channel
                             (json/encode {:type "pong" :at (now-iso state)}) false)
                    nil))
                (catch Exception _ nil))))

          ;; Start file watcher loop
          (future
            (try
              (let [last-size (atom (.length file))
                    last-line-count (atom (with-open [r (io/reader file)]
                                            (count (line-seq r))))]
                (loop []
                  (Thread/sleep 300)
                  (when (and (http/open? channel) (not @stop-flag))
                    (let [current-size (.length file)]
                      (when (> current-size @last-size)
                        ;; File grew - read new lines
                        (try
                          (with-open [rdr (io/reader file)]
                            (let [all-lines (vec (line-seq rdr))
                                  current-count (count all-lines)
                                  new-lines (drop @last-line-count all-lines)]
                              (doseq [line new-lines]
                                (when-let [event (parse-claude-jsonl-line line)]
                                  (when (seq (:text event))
                                    (http/send! channel
                                      (json/encode {:type "event" :event event})
                                      false))))
                              (reset! last-line-count current-count)))
                          (catch Exception e
                            (println "Claude stream read error:" (.getMessage e))))
                        (reset! last-size current-size)))
                    (recur))))
              (catch Exception e
                (println "Claude stream watcher error:" (.getMessage e)))))

          ;; Handle close
          (http/on-close channel
            (fn [_status]
              (reset! stop-flag true)
              (swap! claude-stream-watchers dissoc watcher-id))))))))

(defn- extract-session-from-path [uri]
  (when-let [match (re-find #"/fulab/notebook/([^/]+)" uri)]
    (second match)))

;; =============================================================================
;; Arxana Anchor/Link HTTP Handlers
;; =============================================================================

(defn- handle-anchor-create [state request]
  (try
    (let [payload (or (parse-json-body request) {})
          session-id (:session-id payload)
          turn (:turn payload)
          anchor-type (keyword (or (:type payload) "insight"))
          content (:content payload)
          note (:note payload)
          author (:author payload)]
      (if (or (nil? session-id) (nil? turn) (nil? content))
        (json-response 400 {:ok false :err "missing-required-fields"
                            :required [:session-id :turn :content]})
        (let [result (musn-svc/record-anchor! session-id turn anchor-type content
                                               :note note :author author)]
          (json-response (if (:ok result) 200 400) result))))
    (catch Exception e
      (json-response 500 {:ok false :err "anchor-create-failed" :detail (.getMessage e)}))))

(defn- handle-link-create [state request]
  (try
    (let [payload (or (parse-json-body request) {})
          from-anchor (:from payload)
          to-anchor (:to payload)
          link-type (keyword (or (:type payload) "references"))
          note (:note payload)
          author (:author payload)]
      (if (or (nil? from-anchor) (nil? to-anchor))
        (json-response 400 {:ok false :err "missing-required-fields"
                            :required [:from :to]})
        (let [result (musn-svc/create-link! from-anchor to-anchor link-type
                                             :note note :author author)]
          (json-response (if (:ok result) 200 400) result))))
    (catch Exception e
      (json-response 500 {:ok false :err "link-create-failed" :detail (.getMessage e)}))))

(defn- handle-anchors-get [state session-id turn]
  (try
    (let [turn (when turn
                 (let [trim (str/trim (str turn))]
                   (if (re-matches #"-?\\d+" trim)
                     (Integer/parseInt trim)
                     trim)))
          anchors (if turn
                    (musn-svc/get-anchors session-id :turn turn)
                    (musn-svc/get-anchors session-id))]
      (json-response 200 {:ok true :session-id session-id :anchors (vec anchors)}))
    (catch Exception e
      (json-response 500 {:ok false :err "anchors-get-failed" :detail (.getMessage e)}))))

(defn- handle-links-get [state anchor-id]
  (try
    (let [links (musn-svc/get-links anchor-id)]
      (json-response 200 {:ok true :anchor-id anchor-id :links links}))
    (catch Exception e
      (json-response 500 {:ok false :err "links-get-failed" :detail (.getMessage e)}))))

(defn handler [state request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (= [:get "/healthz"] [method uri])
      (plaintext-response 200 "ok")

      ;; Arxana anchor/link routes
      (and (= method :post) (= uri "/arxana/anchor/create"))
      (handle-anchor-create state request)

      (and (= method :post) (= uri "/arxana/link/create"))
      (handle-link-create state request)

      (and (= method :get) (re-matches #"/arxana/anchors/[^/]+" uri))
      (let [session-id (second (re-find #"/arxana/anchors/([^/]+)" uri))
            turn (get (:query-params request) "turn")]
        (handle-anchors-get state session-id turn))

      (and (= method :get) (re-matches #"/arxana/links/.*" uri))
      (let [anchor-id (second (re-find #"/arxana/links/(.+)" uri))]
        (handle-links-get state anchor-id))

      ;; Semantic anchor linking (POST to handle complex anchor IDs)
      (and (= method :post) (= uri "/arxana/suggest-links"))
      (try
        (let [payload (or (parse-json-body request) {})
              session-id (:session-id payload)
              anchor-id (:anchor-id payload)
              limit (or (:limit payload) 3)]
          (if (and session-id anchor-id)
            (let [suggestions (musn-svc/suggest-links-for-anchor session-id anchor-id :limit limit)]
              (json-response 200 {:ok true :anchor-id anchor-id :suggestions (vec suggestions)}))
            (json-response 400 {:ok false :err "missing-session-id-or-anchor-id"})))
        (catch Exception e
          (json-response 500 {:ok false :err "suggest-links-failed" :detail (.getMessage e)})))

      (and (= method :post) (= uri "/arxana/auto-link"))
      (try
        (let [payload (or (parse-json-body request) {})
              session-id (:session-id payload)
              threshold (or (:threshold payload) 0.3)
              limit (or (:limit payload) 10)]
          (if session-id
            (let [result (musn-svc/auto-link-anchors! session-id :threshold threshold :limit limit)]
              (json-response 200 result))
            (json-response 400 {:ok false :err "missing-session-id"})))
        (catch Exception e
          (json-response 500 {:ok false :err "auto-link-failed" :detail (.getMessage e)})))

      ;; Lab enrichment endpoint - enrich content with patterns + embeddings
      (and (= method :post) (= uri "/lab/enrich"))
      (try
        (let [payload (or (parse-json-body request) {})
              content (:content payload)
              session-id (:session-id payload)
              use-portal (if (contains? payload :use-portal) (:use-portal payload) true)
              pattern-limit (or (:pattern-limit payload) 5)
              namespace (:namespace payload)]
          (if content
            (let [result (enrichment/enrich content
                                            :session-id session-id
                                            :use-portal use-portal
                                            :pattern-limit pattern-limit
                                            :namespace namespace)]
              (json-response 200 {:ok true :enrichment result}))
            (json-response 400 {:ok false :err "missing-content"})))
        (catch Exception e
          (json-response 500 {:ok false :err "enrichment-failed" :detail (.getMessage e)})))

      ;; Lab recurring patterns - patterns that have recurred across sessions
      (and (= method :get) (= uri "/lab/recurring-patterns"))
      (try
        (let [params (or (:query-params request) {})
              min-count (some-> (get params "min-count") Integer/parseInt)
              min-sessions (some-> (get params "min-sessions") Integer/parseInt)]
          (let [result (enrichment/recurring-patterns
                        :min-count (or min-count 3)
                        :min-sessions (or min-sessions 2))]
            (json-response 200 {:ok true :patterns result})))
        (catch Exception e
          (json-response 500 {:ok false :err "recurring-patterns-failed" :detail (.getMessage e)})))

      ;; WebSocket session stream (must be before SSE route)
      (and (= method :get) (re-matches #"/fulab/session/[^/]+/ws" uri))
      (let [session-id (second (re-find #"/fulab/session/([^/]+)/ws" uri))]
        (handle-session-ws state request session-id))

      ;; Claude Code JSONL stream (WebSocket file tailing)
      (and (= method :get) (= uri "/fulab/claude-stream/ws"))
      (handle-claude-stream-ws state request)

      ;; Lab sessions listing
      (and (= method :get) (= uri "/fulab/lab/sessions/active"))
      (handle-lab-sessions-active state request)

      (and (= method :get) (= uri "/fulab/lab/sessions/archived"))
      (handle-lab-sessions-archived state request)

      (and (= method :get) (= uri "/fulab/lab/sessions"))
      ;; Combined: active + archived
      (try
        (let [active (or (scan-claude-projects) [])
              archived (or (scan-lab-raw state) [])
              all (concat (map #(assoc % :source "active") active)
                          (map #(assoc % :source "archived") archived))]
          (json-response 200 {:ok true
                              :sessions (vec (sort-by :modified #(compare %2 %1) all))}))
        (catch Exception e
          (json-response 500 {:ok false :err "scan-failed" :detail (.getMessage e)})))

      ;; Notebook viewer routes
      (and (= method :get) (re-matches #"/fulab/notebook/[^/]+/stream" uri))
      (let [session-id (extract-session-from-path uri)]
        (handle-notebook-stream state request session-id))

      (and (= method :get) (re-matches #"/fulab/plan/[^/]+/diagram" uri))
      (let [session-id (second (re-find #"/fulab/plan/([^/]+)/diagram" uri))]
        (handle-plan-diagram session-id))

      (and (= method :get) (re-matches #"/fulab/notebook/[^/]+" uri))
      (serve-static-file "public/notebook.html" "text/html")

      ;; Static assets
      (and (= method :get) (str/starts-with? uri "/js/"))
      (let [path (str "public" uri)]
        (serve-static-file path "application/javascript"))

      ;; Existing routes
      (and (= uri "/fulab/hud/format") (= method :post))
      (handle-hud-format state request)
      (and (= uri "/musn/ws") (= method :get))
      (websocket-handler state request)
      (and (= uri "/musn/check") (= method :post))
      (handle-check-http state request)
      (and (= uri "/musn/workday/submit") (= method :post))
      (handle-workday-submit state request)
      (and (= uri "/musn/ingest") (= method :post))
      (handle-ingest state request)

      ;; Native planning detection endpoints
      (and (= uri "/musn/scribe/native-planning") (= method :post))
      (try
        (let [payload (or (parse-json-body request) {})
              session-id (:session/id payload)
              tool-name (:tool payload)
              task-id (:task-id payload)
              subject (:subject payload)]
          (if session-id
            (let [result (musn-svc/note-native-planning-detected! session-id tool-name
                                                                  :task-id task-id
                                                                  :subject subject)]
              (json-response 200 (or result {:ok false :err "session-not-found"})))
            (json-response 400 {:ok false :err "missing-session-id"})))
        (catch Exception e
          (json-response 500 {:ok false :err "native-planning-failed" :detail (.getMessage e)})))

      (and (= uri "/musn/scribe/native-plan") (= method :post))
      (try
        (let [payload (or (parse-json-body request) {})
              session-id (:session/id payload)
              tasks (:tasks payload)]
          (if (and session-id (seq tasks))
            (let [result (musn-svc/record-native-plan! session-id tasks)]
              (json-response 200 (or result {:ok false :err "session-not-found"})))
            (json-response 400 {:ok false :err "missing-session-id-or-tasks"})))
        (catch Exception e
          (json-response 500 {:ok false :err "native-plan-failed" :detail (.getMessage e)})))

      ;; Record conversation turn (user or agent message)
      (and (= uri "/musn/scribe/turn") (= method :post))
      (try
        (let [payload (or (parse-json-body request) {})
              session-id (:session/id payload)
              role (keyword (or (:role payload) "user"))
              content (:content payload)]
          (if (and session-id content)
            (let [result (musn-svc/record-turn! session-id role content)]
              (json-response 200 (or result {:ok false :err "session-not-found"})))
            (json-response 400 {:ok false :err "missing-session-id-or-content"})))
        (catch Exception e
          (json-response 500 {:ok false :err "turn-recording-failed" :detail (.getMessage e)})))

      ;; Forum routes (delegate to forum-http)
      (str/starts-with? uri "/forum/")
      (or (forum-http/route request)
          (plaintext-response 404 "not-found"))

      :else
      (plaintext-response 404 "not-found"))))

;; State atom for hot-reloadable handler
(defonce ^:private server-state (atom nil))

(defn- reloadable-handler
  "Wrapper that resolves handler through var for hot reloading via Drawbridge."
  [request]
  (if-let [state @server-state]
    (#'handler state request)
    (plaintext-response 503 "server not initialized")))

(defn start!
  ([state]
   (start! state {}))
  ([state {:keys [port]}]
   (reset! server-state state)
   ;; Initialize forum service
   (forum-svc/init!)
   ;; Start Java-WebSocket server for forum (avoids http-kit compression issues)
   (forum-ws/start!)
   (let [port (or port (get-in state [:config :transport-port] 5050))
         stop-fn (http/run-server #'reloadable-handler {:port port})]
     (swap! (:config state) assoc :transport-port port)
     stop-fn)))

(defn stop! [stop-fn]
  (when stop-fn
    (reset! server-state nil)
    (stop-fn)))
