(ns scripts.musn-preflight
  "Lightweight MUSN preflight: exercises plan→select→action→use/evidence→end to catch schema/constraint regressions before live runs."
  (:require [futon3.musn.service :as svc]))

(def ^:private pattern-id "musn/plan-before-tool")
(def ^:private pattern-file "library/musn/plan-before-tool.flexiarg")

(defn- ensure-ok! [label resp]
  (when (:halt? resp)
    (throw (ex-info (str "halt during " label) resp)))
  resp)

(defn -main [& _]
  (try
    (let [{sid :session/id} (svc/create-session! {})]
      (println "[preflight] session" sid)
      (svc/turn-start! {:session/id sid
                        :turn 1
                        :hud {:candidates [pattern-id]}})
      (svc/turn-plan! {:session/id sid
                       :turn 1
                       :plan "preflight ping"})
      (svc/turn-select! {:session/id sid
                         :turn 1
                         :candidates [pattern-id]
                         :chosen pattern-id
                         :reason {:mode :use
                                  :reads []
                                  :note "preflight"
                                  :source :system}})
      (ensure-ok! "read"
                  (svc/turn-action! {:session/id sid
                                     :turn 1
                                     :pattern/id pattern-id
                                     :action "read"
                                     :note "preflight read"}))
      (ensure-ok! "implement"
                  (svc/turn-action! {:session/id sid
                                     :turn 1
                                     :pattern/id pattern-id
                                     :action "implement"
                                     :files [pattern-file]
                                     :note "preflight implement placeholder"
                                     :source :system}))
      (svc/evidence-add! {:session/id sid
                          :turn 1
                          :pattern/id pattern-id
                          :files [pattern-file]
                          :note "first test: preflight ping"})
      (let [end-res (svc/turn-end! {:session/id sid
                                    :turn 1})]
        (ensure-ok! "turn/end" end-res)
        (println "[preflight] summary" (:summary end-res))))
    (println "[preflight] ok")
    (catch Throwable t
      (binding [*out* *err*]
        (println "[preflight] FAILED" (.getMessage t))
        (when-let [data (ex-data t)]
          (prn data)))
      (System/exit 1))))
