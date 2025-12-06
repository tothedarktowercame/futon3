(ns f2.musn
  "Entry point wiring together transport + ui runtimes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [f2.adapters.mock :as mock]
            [f2.transport :as transport]
            [f2.ui :as ui]))

(defonce runtime (atom nil))

(defn- env-trim [k]
  (when-let [raw (System/getenv k)]
    (let [trimmed (str/trim raw)]
      (when (seq trimmed) trimmed))))

(def ^:private futon1-api-base (env-trim "FUTON1_API_BASE"))
(def ^:private futon1-profile (env-trim "FUTON1_PROFILE"))

(def default-config
  {:transport-port 5050
   :ui-port 6060
   :repl {:mode :off
          :admin-token nil
          :admin-allow #{"127.0.0.1" "::1"}}
   :drawbridge {:enabled? false
                :bind "127.0.0.1"
                :port 6767
                :allow ["127.0.0.1" "::1"]
                :token nil}
   :futon1 {:enabled? (boolean futon1-api-base)
            :api-base futon1-api-base
            :profile (or futon1-profile "default")
            :timeout-ms 2000}})

(defn- build-state [config]
  {:config (atom config)
   :clients (atom {})
   :history (atom [])
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

(defn start!
  ([] (start! {}))
  ([opts]
   (let [config (merge default-config opts)
         state (build-state config)
         transport-stop (transport/start! state {:port (:transport-port config)})
         ui-stop (ui/start! state {:port (:ui-port config)})
         drawbridge-stop (start-drawbridge! config)]
      (reset! runtime {:state state
                       :transport transport-stop
                       :ui ui-stop
                       :drawbridge drawbridge-stop})
      state)))

(defn stop! []
  (when-let [{:keys [transport ui drawbridge]} @runtime]
    (transport/stop! transport)
    (ui/stop! ui)
    (when drawbridge (drawbridge))
    (futon3.tatami-store/reset-events!)
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
      (println "Loaded demo events; hit http://localhost:6060/musn/sessions"))
    (println (str "Transport on " (get @(:config state) :transport-port)
                  ", UI on " (get @(:config state) :ui-port)))
    (println "Press Ctrl+C to exit.")))
