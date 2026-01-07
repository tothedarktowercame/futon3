(ns lab-pattern-report
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-pattern-report.clj --session-id ID --lab-root PATH [--repo-root PATH]")
  (println "Prints a concise PUR/PSR report for a session."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--session-file" (recur (assoc opts :session-file (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--repo-root" (recur (assoc opts :repo-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn session-path [{:keys [session-id session-file lab-root]}]
  (cond
    session-file session-file
    (and lab-root session-id) (str (io/file lab-root "sessions" (str session-id ".edn")))
    :else nil))

(defn claim-record [event]
  (or (get-in event [:payload :pur])
      (get-in event [:payload :psr])))

(defn verified-index [events]
  {:pur (->> events
             (filter #(= :pattern/use-verified (:event/type %)))
             (map #(get-in % [:payload :pur/id]))
             set)
   :psr (->> events
             (filter #(= :pattern/selection-verified (:event/type %)))
             (map #(get-in % [:payload :psr/id]))
             set)})

(defn anchor-summary [anchors session]
  (let [resolver (pc/anchor-resolver session)
        result (pc/expected-vs-resolved anchors resolver)]
    (format "%d/%d" (:resolved result) (:expected result))))

(defn format-psr [psr session]
  (let [forecast (:forecast psr)
        forecast-summary (pc/forecast-summary forecast session)]
    (str "PSR " (:psr/id psr) " decision=" (:decision/id psr)
         " chosen=" (:chosen psr) " candidates=" (count (:candidates psr))
         " anchors=" (anchor-summary (:context/anchors psr) session)
         " forecast-loci=" (:resolved forecast-summary) "/" (:expected forecast-summary))))

(defn format-pur [pur session]
  (str "PUR " (:pur/id pur) " pattern=" (:pattern/id pur)
       (when-let [decision (:decision/id pur)] (str " decision=" decision))
       " anchors=" (anchor-summary (:anchors pur) session)))

(defn link-index [purs]
  (->> purs
       (keep #(when-let [decision (:decision/id %)] [decision %]))
       (group-by first)))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root repo-root] :as opts}
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
            events (:events session)
            claims (filter #(or (= :pattern/use-claimed (:event/type %))
                                (= :pattern/selection-claimed (:event/type %)))
                           events)
            purs (->> claims (filter #(= :pattern/use-claimed (:event/type %))) (map claim-record))
            psrs (->> claims (filter #(= :pattern/selection-claimed (:event/type %))) (map claim-record))
            verified (verified-index events)
            decision-links (link-index purs)]
        (println (format "Session %s" (or (:session/id session) session-id)))
        (println (format "Claims: PUR=%d PSR=%d" (count purs) (count psrs)))
        (println (format "Verified: PUR=%d PSR=%d" (count (:pur verified)) (count (:psr verified))))
        (when (seq psrs)
          (println "")
          (println "Pattern Selection Records")
          (doseq [psr psrs]
            (println (format "- %s" (format-psr psr session)))
            (let [decision (:decision/id psr)
                  linked (get decision-links decision)]
              (when (seq linked)
                (println (format "  linked PURs: %s"
                                 (str/join ", " (map (comp :pur/id second) linked))))))))
        (when (seq purs)
          (println "")
          (println "Pattern Use Records")
          (doseq [pur purs]
            (println (format "- %s" (format-pur pur session)))))
        (println (format "Repo root: %s" repo-root))))))
