(ns lab-session-report
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn usage []
  (println "Usage: dev/lab-session-report.clj --session-id ID [--lab-root PATH] [--format md]")
  (println "Generates a three-column session report (repo changes | pattern trace | AIF)."))

(defn parse-args [args]
  (loop [opts {:format "md"}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--format" (recur (assoc opts :format (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn read-json [path]
  (when (.exists (io/file path))
    (json/read-str (slurp path) :key-fn identity)))

(defn read-edn [path]
  (when (.exists (io/file path))
    (read-string (slurp path))))

(defn lab-path [lab-root & parts]
  (str (apply io/file lab-root parts)))

(defn bullets [items]
  (if (seq items)
    (str/join "<br>" (map #(str "- " %) items))
    "- (none)"))

(defn- fmt-num [value]
  (when (number? value)
    (format "%.3f" (double value))))

(defn- summarize-psr-aif [result]
  (let [aif (:aif result)
        g-chosen (fmt-num (:G-chosen aif))
        tau (fmt-num (:tau aif))
        rejected (:G-rejected aif)
        rejected-vals (when (map? rejected) (vals rejected))
        rej-min (fmt-num (when (seq rejected-vals) (apply min rejected-vals)))
        rej-max (fmt-num (when (seq rejected-vals) (apply max rejected-vals)))]
    (remove nil?
            [(when g-chosen (str "G-chosen: " g-chosen))
             (when (or rej-min rej-max)
               (str "G-rejected: "
                    (if (and rej-min rej-max)
                      (str rej-min "–" rej-max)
                      (or rej-min rej-max))
                    " (" (count rejected-vals) ")"))
             (when tau (str "tau: " tau))])))

(defn- summarize-pur-aif [result]
  (let [aif (:aif result)
        err (fmt-num (:prediction-error aif))
        tau (fmt-num (:tau-updated aif))
        status (get-in aif [:belief-delta :status])]
    (remove nil?
            [(when err (str "prediction-error: " err))
             (when tau (str "tau-updated: " tau))
             (when status (str "status: " status))])))

(defn- session-claims [events]
  {:psr (->> events
             (filter #(= :pattern/selection-claimed (:event/type %)))
             (map #(get-in % [:payload :psr]))
             first)
   :pur (->> events
             (filter #(= :pattern/use-claimed (:event/type %)))
             (map #(get-in % [:payload :pur]))
             first)})

(defn render-pattern [psr pur]
  (let [psr-line (when psr
                   (str "PSR " (:psr/id psr) " chosen=" (:chosen psr)
                        " candidates=" (count (:candidates psr))))
        pur-line (when pur
                   (str "PUR " (:pur/id pur) " pattern=" (:pattern/id pur)
                        " decision=" (:decision/id pur)))
        anchors (->> (concat (get psr :context/anchors)
                             (get pur :anchors))
                     (keep #(get-in % [:anchor/ref :file]))
                     distinct)]
    (bullets (remove nil? [psr-line pur-line (when (seq anchors)
                                              (str "anchors: " (str/join ", " anchors)))]))))

(defn- aif-summaries [events]
  (->> events
       (filter #(= :aif/summary (:event/type %)))
       (map :payload)))

(defn render-aif [aif events]
  (let [engine (:aif/engine-id aif)
        adapter (:aif/adapter aif)
        summaries (aif-summaries events)
        psr (first (filter #(= :psr (:aif/kind %)) summaries))
        pur (first (filter #(= :pur (:aif/kind %)) summaries))
        psr-lines (when psr (summarize-psr-aif (:aif/result psr)))
        pur-lines (when pur (summarize-pur-aif (:aif/result pur)))
        lines (remove nil?
                      [(when engine (str "engine-id: " engine))
                       (when adapter (str "adapter: " adapter))
                       (when (seq psr-lines)
                         (str "psr " (str/join "; " psr-lines)))
                       (when (seq pur-lines)
                         (str "pur " (str/join "; " pur-lines)))])]
    (bullets lines)))

(defn- aif-stats [aif events]
  (let [summaries (aif-summaries events)
        psr (first (filter #(= :psr (:aif/kind %)) summaries))
        pur (first (filter #(= :pur (:aif/kind %)) summaries))
        psr-aif (:aif (:aif/result psr))
        pur-aif (:aif (:aif/result pur))
        rejected (:G-rejected psr-aif)
        rejected-vals (when (map? rejected) (vals rejected))
        rej-min (when (seq rejected-vals) (apply min rejected-vals))
        rej-max (when (seq rejected-vals) (apply max rejected-vals))]
    {:engine (:aif/engine-id aif)
     :adapter (:aif/adapter aif)
     :psr-g (fmt-num (:G-chosen psr-aif))
     :psr-g-rej-min (fmt-num rej-min)
     :psr-g-rej-max (fmt-num rej-max)
     :psr-tau (fmt-num (:tau psr-aif))
     :pur-error (fmt-num (:prediction-error pur-aif))
     :pur-tau (fmt-num (:tau-updated pur-aif))
     :pur-status (get-in pur-aif [:belief-delta :status])}))

(defn render-repo [raw]
  (let [files (get raw "lab/files-touched")
        start (get raw "lab/timestamp-start")
        end (get raw "lab/timestamp-end")
        lines (remove nil?
                      [(when (seq files) (str "files: " (str/join ", " files)))
                       (when start (str "start: " start))
                       (when end (str "end: " end))])]
    (bullets lines)))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root format]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      :else
      (let [raw (read-json (lab-path lab-root "raw" (str session-id ".json")))
            aif (read-edn (lab-path lab-root "aif" (str session-id ".edn")))
            session (read-edn (lab-path lab-root "sessions" (str session-id ".edn")))
            events (:events session)
            claims (session-claims events)
            psr (or (:psr claims)
                    (read-edn (lab-path lab-root "pattern-drafts" (str session-id "-psr.edn"))))
            pur (or (:pur claims)
                    (read-edn (lab-path lab-root "pattern-drafts" (str session-id "-pur.edn"))))]
        (when-not raw
          (println "Missing lab raw file for session" session-id)
          (System/exit 1))
        (case format
          "md"
          (let [aif-stats (aif-stats aif events)]
            (println (str "# Session Report: " session-id))
            (println "")
            (println "| Repo changes | Pattern trace | Engine | Adapter | PSR G | PSR G(min) | PSR G(max) | PSR tau | PUR error | PUR tau | PUR status |")
            (println "| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |")
            (println (clojure.core/format "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |"
                                          (render-repo raw)
                                          (render-pattern psr pur)
                                          (or (:engine aif-stats) "-")
                                          (or (:adapter aif-stats) "-")
                                          (or (:psr-g aif-stats) "-")
                                          (or (:psr-g-rej-min aif-stats) "-")
                                          (or (:psr-g-rej-max aif-stats) "-")
                                          (or (:psr-tau aif-stats) "-")
                                          (or (:pur-error aif-stats) "-")
                                          (or (:pur-tau aif-stats) "-")
                                          (or (:pur-status aif-stats) "-"))))
          (do
            (println "Unknown format" format)
            (usage)
            (System/exit 1)))))))

(apply -main *command-line-args*)
