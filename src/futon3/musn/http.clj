(ns futon3.musn.http
  "HTTP adapter for MUSN service. Provides JSON endpoints for session + turn lifecycle."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [org.httpkit.server :as http]
            [clojure.string :as str]
            [futon3.aif.viz :as aif-viz]
            [futon3.musn.service :as svc]
            [futon3.fulab.hud :as hud]
            [futon.compass :as compass]))

(def log-path (or (System/getenv "MUSN_LOG") "/tmp/musn_http.log"))

(defn- log!
  "Append a timestamped line to the MUSN log file, ignore failures."
  [msg & more]
  (try
    (spit log-path
          (str (java.time.Instant/now) " " msg
               (when (seq more) (str " " (pr-str more)))
               "\n")
          :append true)
    (catch Throwable _)))

(defn- keywordize-enums
  "Convert certain string enum values to keywords so they pass schema validation."
  [data]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {}
             (map (fn [[k v]]
                    (cond
                      (and (#{:mode :source :cost} k) (string? v))
                      [k (keyword v)]

                      :else [k v])))
             x)
       x))
   data))

(defn- jsonable
  "Transform response payload into JSON-friendly values (e.g., stringifying Instants)."
  [data]
  (walk/postwalk
   (fn [x]
     (cond
       (instance? java.time.Instant x) (str x)
       :else x))
   data))

(defn- parse-json-body [req]
  (let [body (slurp (:body req))]
    (when (seq body)
      (-> (json/parse-string body true)
          (keywordize-enums)))))

(defn- json-response
  ([payload] (json-response 200 payload))
  ([status payload]
   {:status status
    :headers {"content-type" "application/json"}
    :body (json/generate-string (jsonable payload))}))

(defn- compass-request
  [body]
  (let [narrative (or (:narrative body) (:text body) (:query body))
        top-k (some-> (:top-k body) long)
        sim-steps (some-> (:sim-steps body) long)
        seed (some-> (:seed body) long)
        method (let [m (:method body)]
                 (if (string? m) (keyword m) m))]
    (if (str/blank? (str narrative))
      {:ok false :err "missing narrative"}
      (apply compass/compass-report
             narrative
             (cond-> []
               top-k (conj :top-k top-k)
               sim-steps (conj :sim-steps sim-steps)
               seed (conj :seed seed)
               method (conj :method method))))))

(defn- hud-build-request [body]
  (let [intent (:intent body)
        pattern-limit (or (:pattern-limit body) 8)
        namespaces (or (:namespaces body) ["musn" "fulab" "aif" "agent"])]
    (if (str/blank? intent)
      {:ok false :err "missing intent"}
      (try
        {:ok true
         :hud (hud/build-hud {:intent intent
                              :pattern-limit pattern-limit
                              :namespaces namespaces})}
        (catch Throwable t
          {:ok false :err (.getMessage t)})))))

(defn- dispatch [uri body]
  (case uri
    "/musn/session/create" (svc/create-session! body)
    "/musn/turn/start"    (svc/turn-start! body)
    "/musn/turn/plan"     (svc/turn-plan! body)
    "/musn/turn/select"   (svc/turn-select! body)
    "/musn/turn/action"   (svc/turn-action! body)
    "/musn/turn/use"      (svc/turn-use! body)
    "/musn/evidence/add"  (svc/evidence-add! body)
    "/musn/turn/end"      (svc/turn-end! body)
    "/musn/turn/resume"   (svc/turn-resume! body)
    "/musn/session/state" (svc/session-state! body)
    "/musn/chat/message"  (svc/chat-message! body)
    "/musn/chat/bid"      (svc/chat-bid! body)
    "/musn/chat/unlatch"  (svc/chat-unlatch! body)
    "/musn/chat/state"    (svc/chat-state! body)
    "/musn/compass"       (compass-request body)
    "/musn/hud/build"     (hud-build-request body)
    "/musn/scribe/turn"   (let [{:keys [session/id role content]} body]
                            (svc/record-turn! id (keyword role) content))
    "/musn/scribe/plan"   (let [{:keys [session/id note diagram]} body]
                            (svc/record-plan! id note diagram))
    "/musn/scribe/plan-wiring"
                          (let [{:keys [session/id note wiring]} body]
                            (svc/record-plan-wiring! id note wiring))
    "/musn/scribe/native-planning"
                          (let [{:keys [session/id tool task-id subject]} body]
                            (svc/note-native-planning-detected! id tool
                                                                :task-id task-id
                                                                :subject subject))
    "/musn/scribe/native-plan"
                          (let [{:keys [session/id tasks note]} body]
                            (svc/record-native-plan! id tasks :note note))
    "/musn/activity/log"  (svc/activity-log! body)
    nil))

(defn- handle-activity-ws
  "WebSocket endpoint for bidirectional activity streaming.
   - Receives: activity events from clients (logged + broadcast)
   - Sends: all activity events to connected clients"
  [req]
  (http/with-channel req channel
    ;; Register client
    (svc/register-activity-client! channel)
    (log! "[activity-ws] client connected" {:remote (:remote-addr req)})

    ;; Send recent history on connect
    (let [recent (take-last 50 (svc/activity-log-entries))
          init-msg (json/generate-string {:type "init"
                                          :count (count recent)
                                          :events (vec recent)})]
      (http/send! channel init-msg false))

    ;; Handle incoming messages (activity events from clients)
    (http/on-receive channel
      (fn [raw]
        (try
          (let [msg (json/parse-string raw true)]
            (case (:type msg)
              "ping" (http/send! channel (json/generate-string {:type "pong"}) false)
              "activity" (svc/activity-log! (:payload msg))
              ;; Default: treat as activity event directly
              (when (and (:agent msg) (:source msg))
                (svc/activity-log! msg))))
          (catch Exception e
            (log! "[activity-ws] parse error" {:err (.getMessage e)})))))

    ;; Handle disconnect
    (http/on-close channel
      (fn [status]
        (svc/unregister-activity-client! channel)
        (log! "[activity-ws] client disconnected" {:status status})))))

(defn handler [req]
  (try
    (let [uri (:uri req)
          method (:request-method req)]
      (cond
        (= uri "/health") (json-response {:ok true})

        ;; WebSocket endpoint for activity stream
        (and (= uri "/musn/activity/ws") (= method :get))
        (handle-activity-ws req)

        ;; GET endpoint for activity entries
        (and (= uri "/musn/activity/entries") (= method :get))
        (let [params (some-> (:query-string req)
                             (java.net.URLDecoder/decode "UTF-8")
                             (clojure.string/split #"&")
                             (->> (map #(clojure.string/split % #"=" 2))
                                  (into {})))
              limit (some-> (get params "limit") parse-long)
              entries (svc/activity-log-entries {:limit (or limit 20)})]
          (json-response {:ok true :entries (vec entries)}))

        ;; GET endpoint for vitality scan data
        (and (= uri "/musn/vitality") (= method :get))
        (let [scan-path (io/file (or (System/getenv "FUTON3_ROOT") ".")
                                 "resources/vitality/latest_scan.json")]
          (if (.exists scan-path)
            (try
              (let [data (json/parse-string (slurp scan-path) true)]
                (json-response {:ok true :vitality data}))
              (catch Exception e
                (json-response 500 {:ok false :err (.getMessage e)})))
            (json-response 404 {:ok false :err "vitality scan not found"})))

        (not= :post method) (json-response 405 {:ok false :err "method not allowed"})
        :else
        (let [body (parse-json-body req)]
          (when (= uri "/musn/turn/end")
            (log! "[musn-http] turn/end"
                  {:session/id (:session/id body)
                   :turn (:turn body)
                   :remote-addr (:remote-addr req)
                   :user-agent (get-in req [:headers "user-agent"])
                   :client (get-in req [:headers "x-musn-client"])
                   :content-length (get-in req [:headers "content-length"])}))
          (if-let [resp (dispatch uri body)]
            (json-response resp)
            (json-response 404 {:ok false :err "not found"})))))
    (catch Throwable t
      (log! "[musn-http] handler error" {:uri (:uri req)
                                         :err (.getMessage t)})
      (json-response 400 {:ok false
                          :err (.getMessage t)
                          :type (keyword (.. t getClass getSimpleName))}))))

(defonce ^:private server-state (atom nil))

(defn start!
  "Start the MUSN HTTP server. Returns a stop function.
   Options:
     :port - HTTP port (default 6065)
     :ssl-port - HTTPS/WSS port (default nil, disabled)
     :ssl-context - javax.net.ssl.SSLContext for TLS
     :ssl-domain - Let's Encrypt domain (alternative to ssl-context)
     :aif-viz-port - optional AIF visualization port"
  ([] (start! {}))
  ([opts]
   (let [port (or (:port opts)
                  (some-> (System/getenv "MUSN_PORT") Long/parseLong)
                  6065)
         ssl-port (or (:ssl-port opts)
                      (some-> (System/getenv "MUSN_SSL_PORT") Long/parseLong))
         ssl-domain (or (:ssl-domain opts)
                        (System/getenv "MUSN_SSL_DOMAIN"))
         aif-viz-port (or (:aif-viz-port opts)
                          (some-> (System/getenv "MUSN_AIF_VIZ_PORT") str/trim not-empty Long/parseLong))
         stop-fns (atom [])]
     (when aif-viz-port
       (aif-viz/start! {:port aif-viz-port}))

     ;; Start HTTP server
     (log! (format "MUSN HTTP server on %d" port))
     (swap! stop-fns conj (http/run-server #'handler {:port port}))

     ;; Note: For WSS/TLS, use nginx as reverse proxy to this HTTP server

     (let [stop-all (fn [] (doseq [f @stop-fns] (f)))]
       (reset! server-state {:port port :ssl-port ssl-port :stop-fn stop-all})
       stop-all))))

(defn stop!
  "Stop the MUSN HTTP server."
  []
  (when-let [{:keys [stop-fn]} @server-state]
    (stop-fn)
    (reset! server-state nil)))

(defn -main [& _args]
  (let [port (Long/parseLong (or (System/getenv "MUSN_PORT") "6065"))]
    (binding [*out* *err*]
      (println (format "MUSN HTTP server on %d" port))
      (println (format "Logging to %s" log-path)))
    (start! {:port port})
    (binding [*out* *err*]
      (println "Press Ctrl+C to exit."))
    @(promise)))
