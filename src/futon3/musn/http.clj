(ns futon3.musn.http
  "HTTP adapter for MUSN service. Provides JSON endpoints for session + turn lifecycle."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [org.httpkit.server :as http]
            [org.httpkit.client :as http-client]
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

(def ^:private personal-api-base
  (or (System/getenv "PERSONAL_API_URL") "http://127.0.0.1:7778"))

(def ^:private default-priors
  "Default priors for cold start (168h week allocation)."
  {:sleep 56 :maintenance 24 :slack 28
   :q1 8 :q2 15 :q3 12 :q4 25})

(defn- ewma-priors
  "Compute EWMA priors from historical weeks.
   alpha = weight for most recent observation (default 0.3).
   Falls back to default-priors if no history."
  [history & {:keys [alpha] :or {alpha 0.3}}]
  (if (empty? history)
    default-priors
    (let [;; Get clears from each historical week, most recent first
          historical-clears (keep :clears (reverse history))]
      (if (empty? historical-clears)
        default-priors
        ;; Compute EWMA: start from oldest, weight newer observations more
        (reduce
         (fn [prior clears]
           (let [all-keys (set (concat (keys prior) (keys clears)))]
             (into {}
                   (for [k all-keys]
                     (let [p (get prior k 0)
                           c (get clears k 0)
                           pv (if (number? p) p 0)
                           cv (if (number? c) c 0)]
                       ;; EWMA: new = alpha * observation + (1-alpha) * prior
                       [k (+ (* alpha cv) (* (- 1 alpha) pv))])))))
         default-priors
         historical-clears)))))

(defn- weeks-since
  "Compute weeks between two ISO date strings (YYYY-MM-DD).
   Returns nil if either date is invalid."
  [from-date to-date]
  (try
    (let [from (java.time.LocalDate/parse from-date)
          to (java.time.LocalDate/parse to-date)]
      (Math/abs (.between java.time.temporal.ChronoUnit/WEEKS from to)))
    (catch Throwable _ nil)))

(defn- compute-meta-vitality
  "Compute meta-level vitality: engagement and delivery priors.
   - user_engagement: has user bid recently?
   - system_delivery: has system delivered value recently?"
  [current-week history-data activity-entries]
  (let [today (str (java.time.LocalDate/now))
        ;; Find most recent week with non-empty bids
        weeks-with-bids (filter (fn [w]
                                  (let [bids (:bids w)]
                                    (and (map? bids) (seq bids))))
                                history-data)
        last-bid-week (or (:week-id (first weeks-with-bids))
                          (when (seq (:bids current-week)) (:week-id current-week)))
        weeks-since-bid (when last-bid-week
                          (weeks-since last-bid-week today))
        ;; System delivery: check MUSN activity entries
        last-activity (first activity-entries)
        last-delivery-date (when last-activity
                             (some-> (:at last-activity)
                                     (subs 0 10)))  ; Extract YYYY-MM-DD
        weeks-since-delivery (when last-delivery-date
                               (weeks-since last-delivery-date today))
        ;; Compute statuses
        user-status (cond
                      (nil? weeks-since-bid) "no_data"
                      (zero? weeks-since-bid) "on_track"
                      (= 1 weeks-since-bid) "due"
                      (= 2 weeks-since-bid) "overdue"
                      :else "lapsed")
        system-status (cond
                        (nil? weeks-since-delivery) "no_data"
                        (<= weeks-since-delivery 1) "active"
                        (= 2 weeks-since-delivery) "stale"
                        :else "dead")]
    {:user_engagement {:prior "bid_weekly"
                       :last_bid last-bid-week
                       :weeks_since_bid weeks-since-bid
                       :status user-status}
     :system_delivery {:prior "deliver_weekly"
                       :last_delivery last-delivery-date
                       :weeks_since_delivery weeks-since-delivery
                       :status system-status}
     :dead_man_switch {:threshold_weeks 3
                       :week1_action "nudge"
                       :week2_action "alert"
                       :week3_action "escalate"}}))

(defn- fetch-personal-vitality
  "Fetch personal vitality from the futon5a personal API.
   Includes priors (EWMA of historical clears), prediction error, and meta vitality.
   Returns nil if the API is not available."
  []
  (try
    (let [week-resp @(http-client/get (str personal-api-base "/api/personal/week")
                                       {:timeout 2000})
          status-resp @(http-client/get (str personal-api-base "/api/personal/status")
                                         {:timeout 2000})
          history-resp @(http-client/get (str personal-api-base "/api/personal/history?n=8")
                                          {:timeout 2000})]
      (when (and (= 200 (:status week-resp))
                 (= 200 (:status status-resp)))
        (let [week-data (json/parse-string (:body week-resp) true)
              status-data (json/parse-string (:body status-resp) true)
              history-data (when (= 200 (:status history-resp))
                             (json/parse-string (:body history-resp) true))
              bids (or (:bids week-data) {})
              clears (or (:clears week-data) {})
              ;; Compute priors from history (EWMA) or use defaults
              priors (ewma-priors history-data)
              ;; Compute delta per category (clears - bids = intention deviation)
              all-keys (set (concat (keys bids) (keys clears) (keys priors)))
              deltas (into {}
                           (for [k all-keys]
                             (let [bid (get bids k 0)
                                   clear (get clears k 0)
                                   b (if (number? bid) bid 0)
                                   c (if (number? clear) clear 0)]
                               [k (- c b)])))
              ;; Compute prediction error (clears - priors = expectation deviation)
              prediction-error (into {}
                                     (for [k all-keys]
                                       (let [prior (get priors k 0)
                                             clear (get clears k 0)
                                             p (if (number? prior) prior 0)
                                             c (if (number? clear) clear 0)]
                                         [k (- c p)])))
              ;; Generate flags for significant discrepancies
              flags (vec (distinct
                          (keep (fn [[k delta]]
                                  (let [kname (name k)]
                                    (cond
                                      (and (= kname "sleep") (< delta -7)) "sleep-deficit"
                                      (and (str/includes? kname "q4") (< delta -5)) "q4-squeezed"
                                      (and (= kname "creative") (< delta -3)) "creative-squeezed"
                                      :else nil)))
                                (merge deltas prediction-error))))
              ;; Compute meta vitality (engagement + delivery priors)
              activity-entries (try (svc/activity-log-entries {:limit 5}) (catch Throwable _ []))
              meta-vitality (compute-meta-vitality week-data history-data activity-entries)]
          {:week (:week-id week-data)
           :priors priors
           :bids bids
           :clears clears
           :delta_by_category deltas
           :prediction_error prediction-error
           :bid_total (:bid-total status-data)
           :clear_total (:clear-total status-data)
           :unallocated (:unallocated status-data)
           :flags flags
           :meta_vitality meta-vitality})))
    (catch Throwable _
      nil)))

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

        ;; GET endpoint for vitality scan data (futon + personal)
        (and (= uri "/musn/vitality") (= method :get))
        (let [scan-path (io/file (or (System/getenv "FUTON3_ROOT") ".")
                                 "resources/vitality/latest_scan.json")
              futon-data (when (.exists scan-path)
                           (try
                             (json/parse-string (slurp scan-path) true)
                             (catch Exception _ nil)))
              personal-data (fetch-personal-vitality)]
          (json-response {:ok true
                          :vitality (cond-> (or futon-data {})
                                      personal-data (assoc :personal_vitality personal-data))}))

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
