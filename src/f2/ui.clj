(ns f2.ui
  "Minimal HTTP UI exposing MUSN status + export endpoint."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [f0.clock :as clock]
            [f2.semantics :as semantics]
            [f2.transport :as transport]
            [org.httpkit.server :as http]))

(defn- json-response [status body]
  {:status status
   :headers {"content-type" "application/json"}
   :body (json/encode body)})

(defn- sessions-view [state]
  (let [history (transport/history-view state)]
    (->> history
         (group-by :client)
         (map (fn [[client entries]]
                {:client client
                 :events (count entries)
                 :last (:stamp (last entries))
                 :samples (map #(select-keys % [:type :input :output :stamp])
                               (take-last 3 entries))}))
         vec)))

(defn- practices-view [state]
  (let [history (transport/history-view state)]
    {:links (semantics/suggest-links history)
     :patterns (semantics/instantiate-pattern history {:pattern/id "scenario"
                                                       :pattern/name "Conversation"
                                                       :pattern/slots []})
     :consistency (semantics/check-consistency history)}))

(def export-path "dev/scenario.edn")

(defn- export-scenario! [state]
  (let [history (transport/history-view state)
        scenario {:generated-at (clock/->iso-string)
                  :clients (transport/clients-view state)
                  :history history}]
    (io/make-parents export-path)
    (spit export-path (pr-str scenario))
    {:status 201
     :headers {"content-type" "application/edn"}
     :body (pr-str {:ok true :path export-path})}))

(defn handler [state request]
  (case [(:request-method request) (:uri request)]
    [:get "/musn/clients"] (json-response 200 {:clients (transport/clients-view state)})
    [:get "/musn/sessions"] (json-response 200 {:sessions (sessions-view state)})
    [:get "/musn/practices"] (json-response 200 (practices-view state))
    [:post "/musn/export"] (export-scenario! state)
    [:get "/healthz"] {:status 200 :headers {"content-type" "text/plain"} :body "ok"}
    {:status 404 :headers {"content-type" "text/plain"} :body "not-found"}))

(defn start!
  ([state] (start! state {}))
  ([state {:keys [port]}]
   (let [port (or port (get @(:config state) :ui-port) 6060)
         stop-fn (http/run-server (partial handler state) {:port port})]
     (swap! (:config state) assoc :ui-port port)
     stop-fn)))

(defn stop! [stop-fn]
  (when stop-fn (stop-fn)))
