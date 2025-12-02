(ns f2.transport
  "HTTP/WebSocket transport exposing a multi-user, capability-aware REPL."
  (:require [cheshire.core :as json]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [f0.clock :as clock]
            [futon3.checks :as checks]
            [futon3.tatami :as tatami]
            [futon3.workday :as workday]
            [f2.repl :as repl]
            [f2.semantics :as semantics]
            [f2.router :as router]
            [org.httpkit.server :as http])
  (:import (java.util UUID)))

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

(defn- now-stamp []
  (clock/stamp))

(defn- append-history! [state entry]
  (swap! (:history state)
         (fn [hist]
           (let [next (conj hist entry)]
             (if (> (count next) history-limit)
               (vec (take-last history-limit next))
               next)))))

(defn- router-log-fn [state]
  (fn [entry]
    (append-history! state (merge {:stamp (now-stamp)} entry))))

(defn- ensure-router! [state]
  (when-not @(:router state)
    (reset! (:router state)
            (router/create {:adapters @(:adapters state)
                            :log! (router-log-fn state)
                            :run-id-fn random-run-id})))
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
  (get @(:config state) :repl {:mode :off}))

(defn- dsl-bindings [state]
  {'now (fn [] (clock/->iso-string))
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
              :stamp (now-stamp)}]
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
        run-id (random-run-id)
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
                                    :stamp (now-stamp)
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
        run-id (random-run-id)
        submission (workday/submit! {:payload payload
                                     :client client
                                     :msg-id (:msg-id envelope)
                                     :run-id run-id
                                     :source :ws})]
    (if (:ok submission)
      (let [entry (:entry submission)
            check-request (workday/check-request payload entry)
            check-outcome (when check-request (checks/check! check-request))
            _ (record-history! state {:client (:id client)
                                      :type :workday
                                      :stamp (now-stamp)
                                      :run-id run-id
                                      :workday/id (:workday/id entry)
                                      :activity (:workday/activity entry)})
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

(defn- prepare-check-request [client envelope]
  (-> (:payload envelope)
      (assoc :run-id (random-run-id))
      (update :origin #(merge {:source :ws
                               :client-id (:id client)
                               :msg-id (:msg-id envelope)}
                              %))))

(defn- handle-check [state client envelope]
  (let [request (prepare-check-request client envelope)
        result (checks/check! request)
        run-id (:run-id request)]
    (if (:ok result)
      (let [proof (:proof result)]
        (record-history! state {:client (:id client)
                                :type :check
                                :stamp (now-stamp)
                                :run-id run-id
                                :pattern (:pattern/id proof)
                                :status (:status result)})
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

(defn- dispatch-message [state client-id envelope]
  (let [type (keyword (or (:type envelope) "unknown"))]
    (case type
      :hello (do
               (update-client! state client-id
                               (fn [client]
                                 (assoc client
                                        :name (or (:client envelope) "anonymous")
                                        :caps (set (:caps envelope))
                                        :last-stamp (now-stamp))))
               {:reply {:type "hello" :ok true :client client-id}})
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
             (update-client! state client-id #(assoc % :connected? false :last-stamp (now-stamp)))
             {:reply {:ok true :type "bye"}
              :close? true})
      (:event :session :session-close :export :run :status)
      (let [client (ensure-client! state client-id)]
        (if-not client
          {:reply {:ok false :err "unknown-client"}}
          (router/dispatch (ensure-router! state) type client envelope)))
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

(defn- make-client [channel {:keys [remote-addr]}]
  {:id (random-client-id)
   :channel channel
   :name "anonymous"
   :caps #{"eval" "message"}
   :connected? true
   :last-stamp (now-stamp)
   :remote-addr remote-addr
   :inbox (async/chan inbox-size)})

(defn- register-client! [state client]
  (swap! (:clients state) assoc (:id client) client)
  client)

(defn- unregister-client! [state client-id]
  (when-let [client (get @(:clients state) client-id)]
    (async/close! (:inbox client))
    (swap! (:clients state) dissoc client-id)
    (append-history! state {:client client-id :type :disconnect :stamp (now-stamp)})
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
      {:reply {:ok false :err (.getMessage ex)}})))

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
          client (register-client! state (make-client channel {:remote-addr (or remote "unknown")}))]
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

(defn- plaintext-response [status body]
  {:status status
   :headers {"content-type" "text/plain"}
   :body body})

(defn handler [state request]
  (cond
    (= [:get "/healthz"] [(:request-method request) (:uri request)])
    (plaintext-response 200 "ok")
    (and (= (:uri request) "/musn/ws") (= (:request-method request) :get))
    (websocket-handler state request)
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
