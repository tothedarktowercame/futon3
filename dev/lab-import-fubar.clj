(ns lab-import-fubar
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-import-fubar.clj --session-id ID [--lab-root PATH] [--events-path PATH]")
  (println "Imports fubar events into lab/sessions/<session-id>.edn"))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--events-path" (recur (assoc opts :events-path (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn kv-list->map [value]
  (cond
    (map? value) value
    (and (sequential? value)
         (even? (count value))
         (keyword? (first value)))
    (apply hash-map value)
    :else nil))

(defn ->inst [value]
  (cond
    (instance? java.util.Date value) value
    (number? value) (java.util.Date. (long value))
    :else (java.util.Date.)))

(defn event->entry [event]
  (let [event-type (:event/type event)
        event-ts (or (:event/ts event) (:clock-in/timestamp event) (:clock-out/timestamp event))
        base {:event/type event-type
              :at (->inst event-ts)}]
    (cond
      (= event-type :pattern/used)
      (merge base {:pattern/id (:pattern/id event)
                   :pattern/reason (:pattern/reason event)})

      (= event-type :clock-in/start)
      (merge base {:clock-in/pattern-id (:clock-in/pattern-id event)
                   :clock-in/intent (:clock-in/intent event)})

      (= event-type :clock-out/complete)
      (merge base {:session/status (:session/status event)
                   :artifacts (:artifacts event)})

      :else
      (merge base {:event/raw event}))))

(defn session-artifacts [events]
  (->> events
       (mapcat #(or (:artifacts %) []))
       (map #(if (map? %) (:path %) %))
       (remove str/blank?)
       distinct
       vec))

(defn select-session-events [events session-id]
  (->> events
       (filter #(= session-id (:event/session %)))
       vec))

(defn find-first [events event-type]
  (first (filter #(= event-type (:event/type %)) events)))

(defn build-session [session-id events]
  (let [clock-in (find-first events :clock-in/start)
        clock-out (find-first events :clock-out/complete)
        entries (mapv event->entry events)
        artifacts (session-artifacts events)]
    {:session/id session-id
     :session/agent :fucodex
     :clock-in (when clock-in
                 {:clock-in/pattern-id (:clock-in/pattern-id clock-in)
                  :clock-in/intent (:clock-in/intent clock-in)
                  :clock-in/timestamp (->inst (:clock-in/timestamp clock-in))})
     :events entries
     :artifacts artifacts
     :clock-out (when clock-out
                  {:session/id session-id
                   :pattern/primary (:clock-in/pattern-id clock-in)
                   :session/status (:session/status clock-out)
                   :artifacts artifacts
                   :clock-out/timestamp (->inst (:clock-out/timestamp clock-out))})}))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root events-path] :as opts}
        (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        events-path (or events-path (str (io/file repo-root "resources" "fubar-events.edn")))]
    (cond
      help (do (usage) (System/exit 0))
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      (not (.exists (io/file events-path))) (do (println "Events file not found:" events-path) (System/exit 1))
      :else
      (let [raw (edn/read-string (slurp events-path))
            events (keep kv-list->map raw)
            session-events (select-session-events events session-id)
            session (build-session session-id session-events)
            out-path (str (io/file lab-root "sessions" (str session-id ".edn")))]
        (when (empty? session-events)
          (println "No events found for session:" session-id)
          (System/exit 1))
        (pc/write-session-file! out-path session)
        (println (format "[lab-import-fucodex] wrote %s (%d events)" out-path (count session-events)))))))
