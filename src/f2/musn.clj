(ns f2.musn
  "Entry point wiring together transport + ui runtimes.

   When running in consolidated mode (default for dev), this namespace
   starts all futon3 services in a single JVM:
   - Transport server (HUD) on port 5050
   - UI server on port 6060
   - MUSN HTTP service on port 6065
   - IRC bridge on port 6667
   - Chat supervisor (polling loop)
   - Futon1 API on port 8080 (pattern registry, graph memory, etc.)

   Each service can be enabled/disabled via config."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [f2.adapters.mock :as mock]
            [f2.transport :as transport]
            [f2.ui :as ui]
            [futon3.agency.http :as agency]
            [futon3.musn.http :as musn-http]
            [futon3.lab.ws :as lab-ws]
            [phoebe.runtime :as phoebe]
            [scripts.musn-irc-bridge :as irc-bridge]
            [scripts.musn-chat-supervisor :as chat-supervisor]
            [api.server :as futon1-api]))

(defonce runtime (atom nil))

(defn- env-trim [k]
  (when-let [raw (System/getenv k)]
    (let [trimmed (str/trim raw)]
      (when (seq trimmed) trimmed))))

(def ^:private futon1-api-base (env-trim "FUTON1_API_BASE"))
(def ^:private futon1-profile (env-trim "FUTON1_PROFILE"))
(def ^:private drawbridge-enabled? (boolean (env-trim "FUTON3_DRAWBRIDGE")))
(def ^:private drawbridge-port
  (some-> (env-trim "FUTON3_DRAWBRIDGE_PORT") Long/parseLong))
(def ^:private drawbridge-bind (env-trim "FUTON3_DRAWBRIDGE_BIND"))

;; Consolidated service env vars (default to enabled for dev)
(def ^:private musn-http-enabled?
  (not= "0" (env-trim "FUTON3_MUSN_HTTP")))
(def ^:private irc-bridge-enabled?
  (not= "0" (env-trim "FUTON3_IRC_BRIDGE")))
(def ^:private chat-supervisor-enabled?
  (not= "0" (env-trim "FUTON3_CHAT_SUPERVISOR")))
(def ^:private futon1-api-enabled?
  (not= "0" (env-trim "FUTON1_API")))
(def ^:private agency-enabled?
  (not= "0" (env-trim "FUTON3_AGENCY")))

;; IRC bridge config (override via env)
(def ^:private irc-bridge-host (or (env-trim "FUTON3_IRC_HOST") "127.0.0.1"))
(def ^:private irc-bridge-port
  (or (some-> (env-trim "FUTON3_IRC_PORT") Long/parseLong) 6667))
(def ^:private irc-bridge-password (env-trim "FUTON3_IRC_PASSWORD"))

(def default-config
  {:transport-port 5050
   :ui-port 6060
   :repl {:mode :off
          :admin-token nil
          :admin-allow #{"127.0.0.1" "::1"}}
   :drawbridge {:enabled? false
                :bind (or drawbridge-bind "127.0.0.1")
                :port (or drawbridge-port 6767)
                :allow ["127.0.0.1" "::1"]
                :token nil}
   :futon1 {:enabled? (boolean futon1-api-base)
            :api-base futon1-api-base
            :profile (or futon1-profile "default")
            :timeout-ms 2000}
   ;; Consolidated services (all enabled by default for dev)
   :musn-http {:enabled? musn-http-enabled?
               :port 6065}
   :irc-bridge {:enabled? irc-bridge-enabled?
                :host irc-bridge-host
                :port irc-bridge-port
                :password irc-bridge-password
                :musn-url "http://localhost:6065"
                :poll-interval 2.0
                :room nil}
   :chat-supervisor {:enabled? chat-supervisor-enabled?
                     :musn-url "http://localhost:6065"
                     :room "lab"
                     :poll-interval 2.0
                     :bot-name "fucodex"
                     :agent nil  ; auto-detect
                     :mode :auto
                     :no-sandbox false
                     :approval-policy nil}
   :futon1-api {:enabled? futon1-api-enabled?
                :port 8080
                :profile (or futon1-profile "default")}
   :lab-ws {:enabled? true
            :port 5056}
   :agency {:enabled? agency-enabled?
            :port 7070}})

(defn- build-state [config]
  {:config (atom config)
   :clients (atom {})
   :history (atom [])
   :hud-help (atom {:sessions #{}})
   :adapters (atom (mock/adapters))
   :router (atom nil)})

(defn- start-drawbridge! [{:keys [drawbridge]}]
  (when (:enabled? drawbridge)
    (try
      (require 'repl.http)
      (let [start-fn (resolve 'repl.http/start!)]
        (if start-fn
          (let [token (or (:token drawbridge) (System/getenv "ADMIN_TOKEN"))]
            (if (seq token)
              (start-fn (-> drawbridge
                            (dissoc :enabled?)
                            (assoc :token token)))
              (do
                (println "Drawbridge enabled but ADMIN_TOKEN (or :token) not provided; skipping.")
                nil)))
          (do
            (println "Drawbridge namespace loaded but start! missing; skipping.")
            nil)))
    (catch Throwable ex
      (println "Drawbridge requested but unavailable:" (.getMessage ex))
      nil))))

(defn- start-musn-http! [{:keys [musn-http]}]
  (when (:enabled? musn-http)
    (println (format "[f2.musn] Starting MUSN HTTP on port %d..." (:port musn-http)))
    (let [stop-fn (musn-http/start! musn-http)]
      (println "[f2.musn] MUSN HTTP started")
      stop-fn)))

(defn- start-irc-bridge! [{:keys [irc-bridge]}]
  (when (:enabled? irc-bridge)
    (println (format "[f2.musn] Starting IRC bridge on %s:%d..." (:host irc-bridge) (:port irc-bridge)))
    (let [stop-fn (irc-bridge/start! (dissoc irc-bridge :enabled?))]
      (println "[f2.musn] IRC bridge started")
      stop-fn)))

(defn- detect-agent-cmd []
  (let [cwd (System/getProperty "user.dir")]
    (cond
      (.exists (io/file cwd "fuclaude")) (str cwd "/fuclaude")
      (.exists (io/file cwd "fucodex")) (str cwd "/fucodex")
      :else "fucodex")))

(defn- start-chat-supervisor! [{:keys [chat-supervisor]}]
  (when (:enabled? chat-supervisor)
    (let [agent-cmd (or (:agent chat-supervisor) (detect-agent-cmd))
          opts (-> chat-supervisor
                   (dissoc :enabled?)
                   (assoc :agent agent-cmd))]
      (println (format "[f2.musn] Starting chat supervisor (room=%s, agent=%s)..."
                       (:room opts) agent-cmd))
      (let [stop-fn (chat-supervisor/start! opts)]
        (println "[f2.musn] Chat supervisor started")
        stop-fn))))

(defn- start-futon1-api! [{:keys [futon1-api]}]
  (when (:enabled? futon1-api)
    (println (format "[f2.musn] Starting Futon1 API on port %d..." (:port futon1-api)))
    (let [result (futon1-api/start! {:port (:port futon1-api)
                                     :default-profile (:profile futon1-api)})]
      (println "[f2.musn] Futon1 API started")
      ;; Return a stop function
      (fn [] (futon1-api/stop!)))))

(defn- start-lab-ws! [{:keys [lab-ws]}]
  (when (:enabled? lab-ws)
    (println (format "[f2.musn] Starting Lab WebSocket on port %d..." (:port lab-ws)))
    (let [server (lab-ws/start! {:port (:port lab-ws)})]
      (println "[f2.musn] Lab WebSocket started")
      ;; Return a stop function
      (fn [] (lab-ws/stop!)))))

(defn- start-agency! [{:keys [agency]}]
  (when (:enabled? agency)
    (println (format "[f2.musn] Starting Agency on port %d..." (:port agency)))
    (let [stop-fn (agency/start! {:port (:port agency)})]
      (println "[f2.musn] Agency started")
      stop-fn)))

(defn start!
  ([] (start! {}))
  ([opts]
  (let [config (merge-with (fn [a b] (if (map? a) (merge a b) b))
                           default-config
                           (when drawbridge-enabled?
                             {:drawbridge (assoc (:drawbridge default-config) :enabled? true)})
                           opts)
        state (build-state config)
        profile (or (get-in config [:futon1-api :profile])
                    (get-in config [:futon1 :profile]))]
     (phoebe/print-banner!
      {:app {:name "f2.musn"}
       :ports {:transport (:transport-port config)
               :ui (:ui-port config)
               :musn-http (get-in config [:musn-http :port])
               :irc-bridge (get-in config [:irc-bridge :port])
               :drawbridge (get-in config [:drawbridge :port])
               :futon1-api (get-in config [:futon1-api :port])}
       :services {:transport true
                  :ui true
                  :drawbridge (get-in config [:drawbridge :enabled?])
                  :musn-http (get-in config [:musn-http :enabled?])
                  :irc-bridge (get-in config [:irc-bridge :enabled?])
                  :chat-supervisor (get-in config [:chat-supervisor :enabled?])
                  :futon1-api (get-in config [:futon1-api :enabled?])}
       :config {:futon1 (:futon1 config)
                :futon1-api (select-keys (:futon1-api config) [:enabled? :profile])
                :drawbridge (select-keys (:drawbridge config) [:enabled? :bind :port])
                :chat-supervisor (select-keys (:chat-supervisor config) [:enabled? :room :agent :mode])}
       :env-overrides (cond-> {"FUTON3_ROOT" (get-in (phoebe/snapshot) [:git :root])}
                        profile (assoc "FUTON1_PROFILE" profile)
                        (get-in config [:futon1-api :port]) (assoc "ALPHA_PORT" (str (get-in config [:futon1-api :port])))
                        (get-in config [:futon1 :api-base]) (assoc "FUTON1_API_BASE" (get-in config [:futon1 :api-base]))
                        (get-in config [:futon1-api :enabled?]) (assoc "FUTON1_API" "1"))})
     (println "[f2.musn] Starting core services...")
     (flush)
     ;; Core services (always started)
     (let [transport-stop (transport/start! state {:port (:transport-port config)})]
       (println (format "[f2.musn] Transport (HUD) started on port %d" (:transport-port config)))
       (flush)
       (let [ui-stop (ui/start! state {:port (:ui-port config)})]
         (println (format "[f2.musn] UI started on port %d" (:ui-port config)))
         (flush)
         (let [drawbridge-stop (start-drawbridge! config)
               ;; Consolidated services (optional)
               musn-http-stop (start-musn-http! config)
               _ (flush)
               irc-bridge-stop (start-irc-bridge! config)
               _ (flush)
               chat-supervisor-stop (start-chat-supervisor! config)
               _ (flush)
               futon1-api-stop (start-futon1-api! config)
               _ (flush)
               lab-ws-stop (start-lab-ws! config)
               _ (flush)
               agency-stop (start-agency! config)
               _ (flush)]
           (reset! runtime {:state state
                            :transport transport-stop
                            :ui ui-stop
                            :drawbridge drawbridge-stop
                            :musn-http musn-http-stop
                            :irc-bridge irc-bridge-stop
                            :chat-supervisor chat-supervisor-stop
                            :futon1-api futon1-api-stop
                            :lab-ws lab-ws-stop
                            :agency agency-stop})
           state))))))

(defn stop! []
  (when-let [{:keys [transport ui drawbridge musn-http irc-bridge chat-supervisor futon1-api lab-ws agency]} @runtime]
    ;; Stop in reverse order of startup
    (when agency
      (println "Stopping Agency...")
      (agency))
    (when lab-ws
      (println "Stopping Lab WebSocket...")
      (lab-ws))
    (when futon1-api
      (println "Stopping Futon1 API...")
      (futon1-api))
    (when chat-supervisor
      (println "Stopping chat supervisor...")
      (chat-supervisor))
    (when irc-bridge
      (println "Stopping IRC bridge...")
      (irc-bridge))
    (when musn-http
      (println "Stopping MUSN HTTP...")
      (musn-http))
    (when drawbridge (drawbridge))
    (ui/stop! ui)
    (transport/stop! transport)
    (reset! runtime nil)))

(defn ingest-demo! [state]
  (let [path "dev/demo_events.ndjson"]
    (when (.exists (io/file path))
      (with-open [r (io/reader path)]
        (doseq [line (line-seq r)]
          (transport/ingest-string! state line))))))

(defn -main [& args]
  (let [mode (first args)
        state (start! {})]
    (when (= mode "demo")
      (ingest-demo! state)
      (println "[f2.musn] Loaded demo events"))
    (println "")
    (println "[f2.musn] All services started. Press Ctrl+C to exit.")
    (flush)))
