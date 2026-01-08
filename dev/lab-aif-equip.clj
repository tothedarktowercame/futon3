(ns lab-aif-equip
  (:require [clojure.java.io :as io]
            [futon2.aif.engine :as engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon2.aif.adapters.ants :as ants]
            [futon2.aif.adapters.futon5-mca :as futon5-mca]))

(defn usage []
  (println "Usage: dev/lab-aif-equip.clj --session-id ID [--adapter fulab|ants|futon5-mca] [--lab-root PATH]")
  (println "Writes a local AIF engine marker for the session."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--adapter" (recur (assoc opts :adapter (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn adapter-for [name]
  (case name
    "ants" (ants/new-adapter)
    "futon5-mca" (futon5-mca/new-adapter)
    "fulab" (fulab/new-adapter)
    (fulab/new-adapter)))

(defn -main [& args]
  (let [{:keys [help unknown session-id adapter lab-root]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        adapter-name (or adapter "fulab")]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      :else
      (let [engine-id (str (java.util.UUID/randomUUID))
            adapter (adapter-for adapter-name)
            _ (engine/new-engine adapter {:engine/id engine-id})
            path (io/file lab-root "aif" (str session-id ".edn"))
            payload {:session/id session-id
                     :aif/engine-id engine-id
                     :aif/adapter (keyword adapter-name)
                     :aif/created (java.util.Date.)}]
        (io/make-parents path)
        (spit path (pr-str payload))
        (println (format "[lab-aif-equip] session=%s adapter=%s" session-id adapter-name))
        (println (format "[lab-aif-equip] path=%s" (str path)))))))

(apply -main *command-line-args*)
