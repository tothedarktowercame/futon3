(ns f2.ui
  "Minimal HTTP UI exposing MUSN status + export endpoint."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [f0.clock :as clock]
            [futon3.cue-embedding :as cue]
            [futon3.trail-bridge :as trail-bridge]
            [futon3.learn-or-act :as learn-act]
            [futon3.tatami :as tatami]
            [f2.semantics :as semantics]
            [f2.transport :as transport]
            [org.httpkit.server :as http]))

(defn- json-response [status body]
  {:status status
   :headers {"content-type" "application/json"}
   :body (json/encode body)})

(defn- read-json [request]
  (when-let [body (:body request)]
    (try
      (json/parse-string (slurp body) true)
      (catch Exception _ {}))))

(defn- safe-handler [f]
  (try
    (f)
    (catch Exception ex
      (json-response 400 {:ok false :error (.getMessage ex)}))))

(defn- normalize-entry [entry]
  (cond
    (map? entry) entry
    (vector? entry) (normalize-entry (first entry))
    (sequential? entry) (apply hash-map entry)
    :else {}))

(defn- parse-cue-entry [payload]
  (let [raw (or (:entry payload)
                (:entry-edn payload))]
    (cond
      (map? raw) (normalize-entry raw)
      (string? raw) (normalize-entry (edn/read-string raw))
      (sequential? raw) (normalize-entry raw)
      :else nil)))

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
  (let [payload (delay (read-json request))]
    (case [(:request-method request) (:uri request)]
      [:get "/musn/clients"] (json-response 200 {:clients (transport/clients-view state)})
      [:get "/musn/sessions"] (json-response 200 {:sessions (sessions-view state)})
      [:get "/musn/practices"] (json-response 200 (practices-view state))
      [:get "/musn/tatami/status"] (json-response 200 (tatami/status {:quiet? true}))
      [:post "/musn/tatami/select"]
      (safe-handler (fn [] (json-response 200 (tatami/select-target! @payload))))
      [:post "/musn/tatami/start"]
      (safe-handler (fn [] (json-response 200 (tatami/start-session @payload))))
      [:post "/musn/tatami/log"]
      (safe-handler (fn [] (json-response 200 (tatami/log-event @payload))))
      [:post "/musn/tatami/close"]
      (safe-handler (fn [] (json-response 200 (tatami/close-session @payload))))
      [:post "/musn/hints"]
      (safe-handler (fn [] (json-response 200 (learn-act/decide @payload))))
      [:post "/musn/cues"]
      (safe-handler
       (fn []
         (let [entry (parse-cue-entry @payload)]
           (if entry
             (let [intent-cues (cue/entry-intent-cues entry)
                   embedding (when-let [tatami (:tatami entry)]
                               (cue/embed-cues tatami))]
                (trail-bridge/publish! (get @(:config state) :futon1)
                                       entry intent-cues embedding)
                (json-response 200 {:ok true
                                    :fruits (:fruits intent-cues)
                                    :paramitas (:paramitas intent-cues)
                                   :intent intent-cues
                                   :embedding embedding}))
             (json-response 400 {:ok false :error "missing-entry"})))))
      [:post "/musn/export"] (export-scenario! state)
      [:get "/healthz"] {:status 200 :headers {"content-type" "text/plain"} :body "ok"}
      {:status 404 :headers {"content-type" "text/plain"} :body "not-found"})))

(defn start!
  ([state] (start! state {}))
  ([state {:keys [port]}]
   (let [port (or port (get @(:config state) :ui-port) 6060)
         stop-fn (http/run-server (partial handler state) {:port port})]
     (swap! (:config state) assoc :ui-port port)
     stop-fn)))

(defn stop! [stop-fn]
  (when stop-fn (stop-fn)))
