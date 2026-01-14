;;; futon3.aif.viz --- Lightweight AIF viz endpoint -*- lexical-binding: t; -*-

(ns futon3.aif.viz
  "AIF tap visualizer: exposes recent AIF events as JSON plus a tiny HTML stepper."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [org.httpkit.server :as http]))

(def ^:private default-port 6066)
(def ^:private default-limit 200)
(def ^:private default-max-events 2000)

(defonce state*
  (atom {:events []
         :by-session {}
         :seq 0
         :max default-max-events}))

(defonce server* (atom nil))
(defonce tap-listener* (atom nil))

(defn- now []
  (java.time.Instant/now))

(defn- ->int [value fallback]
  (try
    (Integer/parseInt (str value))
    (catch Throwable _ fallback)))

(defn- jsonable [data]
  (walk/postwalk
   (fn [x]
     (cond
       (instance? java.time.Instant x) (str x)
       :else x))
   data))

(defn- json-response
  ([payload] (json-response 200 payload))
  ([status payload]
   {:status status
    :headers {"content-type" "application/json"}
    :body (json/generate-string (jsonable payload))}))

(defn- html-response [body]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body body})

(defn- safe-string [value]
  (cond
    (keyword? value) (name value)
    (string? value) value
    (nil? value) nil
    :else (str value)))

(defn- session-id-of [payload]
  (or (:session/id payload)
      (:session-id payload)
      (:sid payload)))

(defn- normalize-aif [aif]
  (when (map? aif)
    (select-keys aif
                [:G-chosen
                 :G-rejected
                 :tau
                 :tau-updated
                 :prediction-error
                 :evidence-score
                 :evidence-delta
                 :evidence-counts])))

(defn- event-kind-label [event payload]
  (let [event-type (:type event)
        base (or (:event payload) (:aif/kind payload) (:kind payload) event-type)
        base (safe-string base)]
    (cond
      (= event-type :aif/context) (str "context/" (or base "event"))
      (= event-type :aif/policy) "policy"
      :else base)))

(defn- event-context [event payload]
  (let [context (or (:context payload)
                    (when (= :aif/context (:type event))
                      (select-keys payload
                                   [:event
                                    :turn
                                    :candidates
                                    :candidate-scores
                                    :precision-priors
                                    :uncertainty
                                    :mu])))]
    (when (map? context) context)))

(defn- theory-view [event context]
  (let [aif (:aif event)
        uncertainty (:uncertainty event)
        tau (or (:tau aif) (:tau-updated aif))
        g (select-keys aif [:G-chosen :G-rejected])
        observation (cond-> {}
                      (:candidate-scores context) (assoc :candidate-scores (:candidate-scores context))
                      (:candidates context) (assoc :candidates (:candidates context)))
        precision (:precision-priors context)
        action (cond-> {}
                 (:pattern/id event) (assoc :pattern/id (:pattern/id event))
                 (:chosen event) (assoc :chosen (:chosen event))
                 (:pattern/action event) (assoc :pattern/action (:pattern/action event))
                 (:outcome event) (assoc :outcome (:outcome event)))]
    (cond-> {}
      (seq observation) (assoc :o observation)
      (seq precision) (assoc :Pi-o precision)
      (seq action) (assoc :a action)
      (seq (:mu context)) (assoc :mu (:mu context))
      (seq g) (assoc :G g)
      (number? tau) (assoc :tau tau)
      (map? uncertainty) (assoc :uncertainty uncertainty))))

(defn- normalize-event [event]
  (let [payload (or (:payload event) event)
        session-id (session-id-of payload)
        kind (event-kind-label event payload)
        context (event-context event payload)
        uncertainty (or (:uncertainty payload)
                        (get context :uncertainty))
        base {:type (:type event)
              :kind kind
              :session/id session-id
              :decision/id (:decision/id payload)
              :pattern/id (or (:pattern/id payload) (:chosen payload))
              :chosen (:chosen payload)
              :pattern/action (:pattern/action payload)
              :outcome (:outcome payload)
              :aif (normalize-aif (:aif payload))
              :policy (:policy payload)
              :context context
              :uncertainty uncertainty
              :raw (select-keys payload [:event :turn :candidates :candidate-scores :precision-priors :uncertainty :mu])}
        theory (theory-view base context)]
    (cond-> base
      (seq theory) (assoc :theory theory))))

(defn- trim-state [st]
  (let [events (vec (:events st))
        max-events (int (or (:max st) default-max-events))]
    (if (<= (count events) max-events)
      st
      (let [drop-count (- (count events) max-events)
            trimmed (subvec events drop-count)
            min-seq (:seq (first trimmed))
            by-session (->> (:by-session st)
                            (keep (fn [[sid entries]]
                                    (let [filtered (vec (drop-while #(< (:seq %) min-seq) entries))]
                                      (when (seq filtered)
                                        [sid filtered]))))
                            (into {}))]
        (assoc st :events trimmed :by-session by-session)))))

(defn- record-event! [event]
  (let [payload (normalize-event event)]
    (when (map? payload)
      (swap! state*
             (fn [st]
               (let [seq-id (inc (:seq st))
                     entry (assoc payload :seq seq-id :ts (now))
                     sid (:session/id entry)
                     st (-> st
                            (assoc :seq seq-id)
                            (update :events conj entry)
                            (update :by-session
                                    (fn [by-session]
                                      (if sid
                                        (update by-session sid (fnil conj []) entry)
                                        by-session))))]
                 (trim-state st)))))))

(defn- aif-event? [event]
  (let [t (:type event)]
    (and (or (and (keyword? t) (= "aif" (namespace t)))
             (and (string? t) (str/starts-with? t "aif/")))
         (not= t :aif/summary))))

(defn- tap-listener [event]
  (when (and (map? event) (aif-event? event))
    (record-event! event)))

(defn- query-params [req]
  (let [qs (or (:query-string req) "")]
    (->> (str/split qs #"&")
         (map #(str/split % #"=" 2))
         (keep (fn [[k v]]
                 (when (seq k)
                   [(java.net.URLDecoder/decode k "UTF-8")
                    (when v (java.net.URLDecoder/decode v "UTF-8"))])))
         (into {}))))

(defn- events-for-session [session limit]
  (let [{:keys [events by-session]} @state*
        entries (if (seq session)
                  (get by-session session [])
                  events)
        limit (int (or limit default-limit))
        clipped (if (pos? limit)
                  (vec (take-last limit entries))
                  (vec entries))]
    {:ok true
     :session/id session
     :count (count clipped)
     :events clipped}))

(defn- summary []
  (let [{:keys [by-session]} @state*]
    {:ok true
     :sessions (mapv (fn [[sid entries]]
                       (let [last-event (last entries)]
                         {:session/id sid
                          :count (count entries)
                          :last (select-keys last-event [:ts :kind :pattern/id])}))
                     (sort-by first by-session))}))

(defn- latest [session]
  (let [{:keys [events by-session]} @state*
        entries (if (seq session)
                  (get by-session session [])
                  events)
        last-event (last entries)]
    {:ok true
     :session/id session
     :event last-event}))

(defn- html-page []
  (str "<!doctype html>\n"
       "<html lang=\"en\">\n"
       "<head>\n"
       "<meta charset=\"utf-8\"/>\n"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>\n"
       "<title>AIF Viz</title>\n"
       "<style>\n"
       "body { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace; margin: 20px; }\n"
       "h1 { font-size: 18px; margin-bottom: 10px; }\n"
       ".controls { display: flex; gap: 8px; flex-wrap: wrap; align-items: center; margin-bottom: 10px; }\n"
       ".panel { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }\n"
       "pre { background: #f7f7f7; padding: 10px; border-radius: 6px; overflow: auto; }\n"
       "button { padding: 4px 8px; }\n"
       "input { padding: 4px 6px; }\n"
       "</style>\n"
       "</head>\n"
       "<body>\n"
       "<h1>AIF Viz</h1>\n"
       "<div class=\"controls\">\n"
       "<label>Session <input id=\"session\" placeholder=\"musn-...\"/></label>\n"
       "<label>Limit <input id=\"limit\" value=\"200\" size=\"6\"/></label>\n"
       "<button id=\"load\">Load</button>\n"
       "<button id=\"prev\">Prev</button>\n"
       "<button id=\"next\">Next</button>\n"
       "<span id=\"status\"></span>\n"
       "</div>\n"
       "<div class=\"panel\">\n"
       "<pre id=\"list\"></pre>\n"
       "<pre id=\"detail\"></pre>\n"
       "</div>\n"
       "<script>\n"
       "let events = [];\n"
       "let idx = -1;\n"
       "function lineFor(e, i) {\n"
       "  const tau = e.aif && (e.aif.tau || e.aif['tau-updated']);\n"
       "  const g = e.aif && e.aif['G-chosen'];\n"
       "  const pat = e['pattern/id'] || '';\n"
       "  const u = e.uncertainty ? `${e.uncertainty.source || ''}:${(e.uncertainty.value ?? '').toString()}` : '';\n"
       "  return `${i} | ${e.ts || ''} | ${e.kind || ''} | ${pat} | G=${g ?? ''} τ=${tau ?? ''} u=${u}`;\n"
       "}\n"
       "function render() {\n"
       "  const list = document.getElementById('list');\n"
       "  list.textContent = events.map(lineFor).join('\\n');\n"
       "  const detail = document.getElementById('detail');\n"
       "  if (idx >= 0 && idx < events.length) {\n"
       "    detail.textContent = JSON.stringify(events[idx], null, 2);\n"
       "  } else {\n"
       "    detail.textContent = '';\n"
       "  }\n"
       "  const status = document.getElementById('status');\n"
       "  status.textContent = events.length ? `event ${idx + 1} / ${events.length}` : 'no events';\n"
       "}\n"
       "async function loadEvents() {\n"
       "  const session = document.getElementById('session').value.trim();\n"
       "  const limit = document.getElementById('limit').value.trim();\n"
       "  const params = new URLSearchParams();\n"
       "  if (session) params.set('session', session);\n"
       "  if (limit) params.set('limit', limit);\n"
       "  const resp = await fetch(`/aif/session?${params.toString()}`);\n"
       "  const data = await resp.json();\n"
       "  events = data.events || [];\n"
       "  idx = events.length ? 0 : -1;\n"
       "  render();\n"
       "}\n"
       "document.getElementById('load').addEventListener('click', loadEvents);\n"
       "document.getElementById('prev').addEventListener('click', () => { if (idx > 0) { idx--; render(); } });\n"
       "document.getElementById('next').addEventListener('click', () => { if (idx < events.length - 1) { idx++; render(); } });\n"
       "loadEvents();\n"
       "</script>\n"
       "</body>\n"
       "</html>\n"))

(defn handler [req]
  (let [uri (:uri req)
        method (:request-method req)
        params (query-params req)]
    (cond
      (= uri "/health") (json-response {:ok true})
      (= uri "/") (html-response (html-page))
      (and (= uri "/aif/latest") (= method :get))
      (json-response (latest (get params "session")))
      (and (= uri "/aif/session") (= method :get))
      (json-response (events-for-session (get params "session")
                                         (->int (get params "limit") default-limit)))
      (and (= uri "/aif/summary") (= method :get))
      (json-response (summary))
      :else (json-response 404 {:ok false :err "not found"}))))

(defn start!
  ([]
   (start! {}))
  ([{:keys [port max-events]}]
   (when-let [listener @tap-listener*]
     (remove-tap listener)
     (reset! tap-listener* nil))
   (when-let [server @server*]
     (server)
     (reset! server* nil))
   (swap! state* assoc
          :events []
          :by-session {}
          :seq 0
          :max (or max-events default-max-events))
   (let [listener tap-listener
         port (int (or port default-port))]
     (add-tap listener)
     (reset! tap-listener* listener)
     (reset! server* (http/run-server handler {:port port}))
     {:ok true :port port})))

(defn stop! []
  (when-let [listener @tap-listener*]
    (remove-tap listener)
    (reset! tap-listener* nil))
  (when-let [server @server*]
    (server)
    (reset! server* nil))
  {:ok true})

(defn -main [& _]
  (let [port (some-> (System/getenv "MUSN_AIF_VIZ_PORT") str/trim not-empty)]
    (start! {:port (when port (Integer/parseInt port))})
    (binding [*out* *err*]
      (println (format "AIF viz listening on %s"
                       (or port default-port)))))) 
