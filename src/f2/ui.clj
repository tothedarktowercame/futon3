(ns f2.ui
  "Minimal HTTP UI exposing MUSN status + export endpoint."
  (:require [cheshire.core :as json]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [f0.clock :as clock]
            [f2.claude :as claude]
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

;; --- Claude session streaming ---

(defn- stream-claude-events!
  "Stream events from a Claude session over WebSocket."
  [request session-id]
  (http/with-channel request channel
    (if-let [sub-ch (claude/subscribe! session-id)]
      (do
        (http/send! channel (json/encode {:ok true :subscribed session-id}))
        (async/go-loop []
          (if-let [event (async/<! sub-ch)]
            (do
              (http/send! channel (json/encode event))
              (if (#{:session/finished :session/error} (:event/type event))
                (do
                  (claude/unsubscribe! session-id sub-ch)
                  (http/close channel))
                (recur)))
            (http/close channel)))
        (http/on-close channel
                       (fn [_] (claude/unsubscribe! session-id sub-ch))))
      (do
        (http/send! channel (json/encode {:ok false :error "session-not-found"}))
        (http/close channel)))))

(defn- handle-claude-run [payload]
  (let [result (claude/start-session! payload)]
    (if (:ok result)
      (json-response 201 result)
      (json-response 400 result))))

(defn- handle-claude-cancel [session-id]
  (if-let [result (claude/cancel-session! session-id)]
    (json-response 200 result)
    (json-response 404 {:ok false :error "session-not-found"})))

(defn- handle-claude-approve [session-id call-id]
  (json-response 200 (claude/approve-tool-call! session-id call-id)))

(defn- handle-claude-deny [session-id call-id reason]
  (json-response 200 (claude/deny-tool-call! session-id call-id reason)))

(defn- handle-claude-input [session-id input]
  (json-response 200 (claude/send-input! session-id input)))

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

(defn- extract-path-param
  "Extract a path parameter from URI like /claude/stream/:session-id"
  [uri prefix]
  (when (str/starts-with? uri prefix)
    (subs uri (count prefix))))

(defn handler [state request]
  (let [payload (delay (read-json request))
        uri (:uri request)
        method (:request-method request)]
    ;; Claude endpoints with path params
    (cond
      ;; Claude session management
      (and (= method :post) (= uri "/claude/run"))
      (safe-handler #(handle-claude-run @payload))

      (and (= method :get) (= uri "/claude/sessions"))
      (json-response 200 {:sessions (claude/list-sessions)})

      (and (= method :get) (str/starts-with? uri "/claude/session/"))
      (let [session-id (extract-path-param uri "/claude/session/")]
        (if-let [session (claude/get-session session-id)]
          (json-response 200 session)
          (json-response 404 {:ok false :error "session-not-found"})))

      (and (= method :get) (str/starts-with? uri "/claude/events/"))
      (let [path-and-query (extract-path-param uri "/claude/events/")
            [session-id query] (str/split path-and-query #"\?" 2)
            offset (when query
                     (some->> (re-find #"offset=(\d+)" query)
                              second
                              parse-long))]
        (if-let [events (claude/get-events session-id {:offset (or offset 0)})]
          (json-response 200 events)
          (json-response 404 {:ok false :error "session-not-found"})))

      (and (= method :get) (str/starts-with? uri "/claude/stream/"))
      (let [session-id (extract-path-param uri "/claude/stream/")]
        (stream-claude-events! request session-id))

      (and (= method :post) (str/starts-with? uri "/claude/cancel/"))
      (let [session-id (extract-path-param uri "/claude/cancel/")]
        (handle-claude-cancel session-id))

      (and (= method :post) (str/starts-with? uri "/claude/approve/"))
      (let [path (extract-path-param uri "/claude/approve/")
            [session-id call-id] (str/split path #"/" 2)]
        (handle-claude-approve session-id call-id))

      (and (= method :post) (str/starts-with? uri "/claude/deny/"))
      (let [path (extract-path-param uri "/claude/deny/")
            [session-id call-id] (str/split path #"/" 2)]
        (handle-claude-deny session-id call-id (:reason @payload)))

      (and (= method :post) (str/starts-with? uri "/claude/input/"))
      (let [session-id (extract-path-param uri "/claude/input/")]
        (handle-claude-input session-id (:input @payload)))

      ;; Existing MUSN endpoints
      :else
      (case [method uri]
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
        {:status 404 :headers {"content-type" "text/plain"} :body "not-found"}))))

(defn start!
  ([state] (start! state {}))
  ([state {:keys [port]}]
   (let [port (or port (get @(:config state) :ui-port) 6060)
         stop-fn (http/run-server (partial handler state) {:port port})]
     (swap! (:config state) assoc :ui-port port)
     stop-fn)))

(defn stop! [stop-fn]
  (when stop-fn (stop-fn)))
