(ns f1.graph
  "In-memory graph built on Datascript with provenance-aware helpers."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [datascript.core :as d]
            [f0.clock :as clock])
  (:import (java.util UUID)))

(declare load-fixture!)

(def schema
  {:event/id {:db/unique :db.unique/identity}
   :event/actor {:db/doc "Actor id"}
   :event/verb {:db/doc "Verb"}
   :event/object {:db/doc "Object of the action"}
   :event/time {:db/index true}
   :session/id {:db/unique :db.unique/identity}
   :session/topic {:db/fulltext true}
   :session/events {:db/cardinality :db.cardinality/many
                    :db/valueType :db.type/ref}
   :musn/type {:db/doc "Entity discriminator"}
   :musn/schema-version {:db/doc "Schema version applied at ingest"}})

(def default-schema-version 1)

(defn- random-id [prefix]
  (str prefix "-" (.substring (.toString (UUID/randomUUID)) 0 8)))

(defn- ensure-id [payload prefix]
  (or (:id payload)
      (get payload :sid)
      (random-id prefix)))

(defn- prepare-event [payload schema-version]
  (let [id (ensure-id payload "E")
        prov (select-keys (:prov payload) [:file :line])
        base {:event/id id
              :event/actor (:actor payload)
              :event/verb (:verb payload)
              :event/object (:object payload)
              :event/time (or (:t payload) (clock/->iso-string))
              :event/prov prov
              :musn/type :musn.type/event
              :musn/schema-version schema-version}]
    [base]))

(defn- prepare-session [payload schema-version]
  (let [sid (ensure-id payload "S")
        refs (mapv (fn [eid] [:event/id eid]) (:events payload []))
        base {:session/id sid
              :session/topic (:topic payload)
              :session/events refs
              :musn/type :musn.type/session
              :musn/schema-version schema-version}]
    [base]))

(defmulti ->tx "Normalize an ingest envelope into Datascript transactions." :type)

(defmethod ->tx :event [{:keys [payload schema-version]}]
  {:id (ensure-id payload "E")
   :tx (prepare-event payload schema-version)})

(defmethod ->tx :session [{:keys [payload schema-version]}]
  {:id (ensure-id payload "S")
   :tx (prepare-session payload schema-version)})

(defmethod ->tx :default [env]
  (throw (ex-info "Unknown entity type" {:env env :graph/error :unknown-type})))

(defn init
  "Create a graph store. Options: :clock, :fixtures (path or data), :schema-version."
  ([] (init {}))
  ([{:keys [clock fixtures schema-version]
     :or {clock (clock/system-clock)
          schema-version default-schema-version}}]
   (let [conn (d/create-conn schema)
         tx-log (atom [])
         store {:conn conn
                :clock clock
                :schema-version schema-version
                :tx-log tx-log}]
     (when fixtures
       (load-fixture! store fixtures))
     store)))

(defn snapshot [{:keys [conn]}]
  (d/db conn))

(defn q
  "Run a Datascript query against the current snapshot."
  [store query & inputs]
  (apply d/q query (snapshot store) inputs))

(defn- append-log! [{:keys [tx-log]} entry]
  (swap! tx-log conj entry))

(defn tx-log
  "Return the transaction log entries."
  [store]
  @(:tx-log store))

(defn put!
  "Transact the given entity envelope {:type :event|:session :payload {...}}."
  [store envelope]
  (let [env (assoc envelope :schema-version (:schema-version store))]
    (try
      (let [{:keys [tx id]} (->tx env)
            report (d/transact! (:conn store) tx)
            entry {:id id
                   :type (:type envelope)
                   :t (clock/->iso-string (clock/now (:clock store)))
                   :tempids (:tempids report)}]
        (append-log! store entry)
        {:ok true :id id :report report})
      (catch Exception ex
        {:ok false :error ex :id nil}))))

(defn load-fixture!
  "Load EDN fixtures into the store. Accepts map data or path string."
  [store fixture]
  (let [data (cond
               (string? fixture) (with-open [r (java.io.PushbackReader. (io/reader fixture))]
                                 (edn/read r))
               (map? fixture) fixture
               :else (throw (ex-info "Unsupported fixture format" {:given fixture})))]
    (doseq [event (:events data)]
      (put! store {:type :event :payload event}))
    (doseq [session (:sessions data)]
      (put! store {:type :session :payload session}))
    store))
