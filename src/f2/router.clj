(ns f2.router
  "Decode + validate frames and dispatch to adapters."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [malli.core :as m]
            [malli.error :as me]))

(def ^:const protocol-revision 1)
(def ^:const status-timeout-ms 200)
(def ^:const run-timeout-ms 5000)

(def ^:private schemas
  (delay (edn/read-string (slurp (io/resource "f2/schemas.edn")))))

(defn- schema-for [type]
  (or (get @schemas type)
      (throw (ex-info (str "No schema for " type) {:type type}))))

(defn- validate! [type payload]
  (let [schema (schema-for type)]
    (if (m/validate schema payload)
      payload
      (throw (ex-info "invalid-payload"
                      {:type type
                       :details (me/humanize (m/explain schema payload))})))))

(defn create
  [{:keys [adapters log! run-id-fn]}]
  {:adapters adapters
   :log! (or log! (fn [_]))
   :run-id-fn (or run-id-fn #(str "RUN-" %))
   :revision protocol-revision
   :msg-cache (atom {})
   :rate (atom {:window-start (System/currentTimeMillis)
                :count 0})})

(defn- new-run-id [{:keys [run-id-fn]}]
  (run-id-fn))

(defn- cache-hit? [router msg-id]
  (when (seq msg-id)
    (get @(:msg-cache router) msg-id)))

(defn- remember! [router msg-id reply]
  (if (seq msg-id)
    (do (swap! (:msg-cache router) assoc msg-id reply)
        reply)
    reply))

(defn- rate-limit! [{:keys [rate]}]
  (let [{:keys [window-start count]} @rate
        now (System/currentTimeMillis)
        window (if (> (- now window-start) 60000)
                 {:window-start now :count 0}
                 {:window-start window-start :count count})
        next-count (inc (:count window))]
    (if (> next-count 1000)
      (throw (ex-info "overload" {:err "overload"}))
      (swap! rate (constantly (assoc window :count next-count))))))

(defn- log! [{:keys [log!]} entry]
  (log! entry))

(defn- call-f1 [router fn-key & args]
  (apply (get-in router [:adapters :f1 fn-key]) args))

(defn- call-f3 [router fn-key & args]
  (apply (get-in router [:adapters :f3 fn-key]) args))

(defn- call-with-timeout [timeout-ms thunk]
  (let [result (promise)
        fut (future
              (try
                (deliver result {:value (thunk)})
                (catch Throwable t
                  (deliver result {:error t}))))
        outcome (deref result timeout-ms ::timeout)]
    (cond
      (= outcome ::timeout)
      (do
        (future-cancel fut)
        (throw (ex-info "timeout" {:err "timeout"
                                    :details {:timeout-ms timeout-ms}})))

      (:error outcome)
      (throw (:error outcome))

      :else (:value outcome))))

(defn handle-hello [router client envelope]
  (let [payload (or (:payload envelope)
                    (select-keys envelope [:rev :client :caps]))
        {:keys [rev]} (validate! :hello payload)
        negotiated (or rev protocol-revision)
        run-id (new-run-id router)]
    (if (= negotiated protocol-revision)
      (do
        (log! router {:client (:id client)
                      :type :hello
                      :run-id run-id})
        {:reply {:ok true
                 :type "ack"
                 :rev protocol-revision
                 :run-id run-id
                 :client (:id client)}})
      {:reply {:ok false
               :type "ack"
               :err "unsupported-rev"
               :expected protocol-revision
               :rev rev}})))

(defn handle-event [router client envelope]
  (let [{:keys [payload]} envelope
        cached-id (:msg-id payload)]
    (if-let [cached (cache-hit? router cached-id)]
      {:reply cached}
      (do
        (rate-limit! router)
        (let [{:keys [msg-id] :as valid} (validate! :event payload)
              run-id (new-run-id router)
              result (call-f1 router :put-event! run-id valid)
              reply {:ok true
                     :type "ack"
                     :run-id run-id
                     :eid (:eid result)}]
          (log! router {:client (:id client)
                        :type :event
                        :run-id run-id
                        :eid (:eid result)})
          {:reply (remember! router msg-id reply)})))))

(defn handle-session-close [router client envelope]
  (let [{:keys [payload]} envelope
        {:keys [msg-id sid]} (validate! :session-close payload)
        run-id (new-run-id router)
        result (call-f1 router :close-session! run-id sid)
        reply {:ok true :type "ack" :sid (:sid result) :run-id run-id}]
    (log! router {:client (:id client)
                  :type :session-close
                  :run-id run-id
                  :sid sid})
    {:reply (remember! router msg-id reply)}))

(defn handle-export [router client envelope]
  (let [{:keys [payload]} envelope
        {:keys [msg-id sid]} (validate! :export payload)
        run-id (new-run-id router)
        result (call-f1 router :export-scenario! run-id sid)
        reply {:ok true
               :type "ack"
               :scenario-path (:scenario-path result)
               :run-id run-id}]
    (log! router {:client (:id client)
                  :type :export
                  :run-id run-id
                  :sid sid})
    {:reply (remember! router msg-id reply)}))

(defn handle-run [router client envelope]
  (let [{:keys [payload]} envelope
        {:keys [msg-id sid scenario-path policy]} (validate! :run payload)
        _ (when (and (nil? sid) (nil? scenario-path))
            (throw (ex-info "invalid-payload"
                            {:type :run
                             :details {:missing [:sid :scenario-path]}})))
        run-id (new-run-id router)
        resolved-path (or scenario-path
                          (:scenario-path (call-f1 router :export-scenario! run-id sid)))
        result (call-with-timeout run-timeout-ms #(call-f3 router :run-scenario! run-id resolved-path policy))
        reply {:ok true :type "ack" :job-id (:job-id result) :run-id run-id :scenario-path resolved-path}]
    (log! router {:client (:id client)
                  :type :run
                  :run-id run-id
                  :scenario-path resolved-path})
    {:reply (remember! router msg-id reply)}))

(defn handle-status [router client envelope]
  (let [{:keys [payload]} envelope
        {:keys [msg-id job-id]} (validate! :status payload)
        run-id (new-run-id router)
        result (call-with-timeout status-timeout-ms #(call-f3 router :job-status run-id job-id))
        reply {:ok true :type "status" :state (:state result) :metrics (:metrics result) :run-id run-id}]
    (log! router {:client (:id client)
                  :type :status
                  :run-id run-id
                  :job-id job-id})
    {:reply (remember! router msg-id reply)}))

(defn dispatch [router type client envelope]
  (case type
    :hello (handle-hello router client envelope)
    :event (handle-event router client envelope)
    :session-close (handle-session-close router client envelope)
    :export (handle-export router client envelope)
    :run (handle-run router client envelope)
    :status (handle-status router client envelope)
    {:reply {:ok false :err "unsupported-type"}}))
