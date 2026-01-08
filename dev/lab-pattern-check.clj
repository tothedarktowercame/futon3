(ns lab-pattern-check
  (:require [clojure.java.io :as io]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-pattern-check.clj --session-id ID --lab-root PATH [--repo-root PATH] [--dry-run]")
  (println "Validates PUR/PSR claims and appends verification events."))

(defn parse-args [args]
  (loop [opts {:dry-run false}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--session-file" (recur (assoc opts :session-file (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--repo-root" (recur (assoc opts :repo-root (second remaining)) (nnext remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn session-path [{:keys [session-id session-file lab-root]}]
  (cond
    session-file session-file
    (and lab-root session-id) (str (io/file lab-root "sessions" (str session-id ".edn")))
    :else nil))

(defn verified-ids [events event-type key-path]
  (->> events
       (filter #(= event-type (:event/type %)))
       (keep #(get-in % key-path))
       set))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root repo-root dry-run] :as opts}
        (parse-args args)
        path (session-path opts)
        repo-root (or repo-root (System/getProperty "user.dir"))]
    (cond
      help (do (usage) (System/exit 0))
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? path) (do (println "--session-id/--session-file and --lab-root are required") (usage) (System/exit 1))
      (not (.exists (io/file path))) (do (println "Session file not found:" path) (System/exit 1))
      :else
      (let [session (pc/read-session-file path)
            pattern-ids (pc/read-pattern-ids repo-root)
            outcome-tags (pc/read-outcome-tags repo-root)
            events (:events session)
            verified-pur-ids (verified-ids events :pattern/use-verified [:payload :pur/id])
            verified-psr-ids (verified-ids events :pattern/selection-verified [:payload :psr/id])
            pur-claims (remove #(contains? verified-pur-ids (get-in % [:payload :pur :pur/id]))
                               (pc/pur-claim-events session))
            psr-claims (remove #(contains? verified-psr-ids (get-in % [:payload :psr :psr/id]))
                               (pc/psr-claim-events session))
            pur-results (for [event pur-claims]
                          (let [pur (get-in event [:payload :pur])
                                result (pc/check-pur pur session pattern-ids outcome-tags)]
                            {:event event :result result}))
            psr-results (for [event psr-claims]
                          (let [psr (get-in event [:payload :psr])
                                result (pc/check-psr psr session pattern-ids)]
                            {:event event :result result}))
            new-events (mapv (fn [{:keys [event result]}]
                               (pc/claim->verify-event event result))
                             (concat pur-results psr-results))
            updated (reduce pc/append-event session new-events)]
        (doseq [{:keys [event result]} (concat pur-results psr-results)]
          (println (format "[lab-pattern-check] %s %s -> %s"
                           (:event/type event)
                           (or (get-in event [:payload :pur :pur/id])
                               (get-in event [:payload :psr :psr/id])
                               "?")
                           (if (:ok? result) "pass" "fail"))))
        (when-not dry-run
          (pc/write-session-file! path updated)
          (println (format "[lab-pattern-check] appended %d verification events" (count new-events))))))))

(apply -main *command-line-args*)
