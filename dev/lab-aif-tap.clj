(ns lab-aif-tap
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn usage []
  (println "Usage: dev/lab-aif-tap.clj --session-id ID [--lab-root PATH] [--duration-ms N]")
  (println "Attach a tap listener and append AIF events to lab/aif/<session>-tap.edn."))

(defn parse-args [args]
  (loop [opts {:duration-ms 60000}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--duration-ms" (recur (assoc opts :duration-ms (Long/parseLong (second remaining))) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root duration-ms]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        out-path (str (io/file lab-root "aif" (str session-id "-tap.edn")))]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      :else
      (let [listener (fn [event]
                       (let [payload (assoc event :session/id (or (:session/id event) session-id))]
                         (io/make-parents out-path)
                         (spit out-path (str (pr-str payload) "\n") :append true)))]
        (add-tap listener)
        (println (format "[lab-aif-tap] session=%s path=%s duration-ms=%d" session-id out-path duration-ms))
        (Thread/sleep duration-ms)
        (remove-tap listener)
        (println "[lab-aif-tap] stopped")))))

(apply -main *command-line-args*)
