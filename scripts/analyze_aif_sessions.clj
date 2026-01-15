(ns scripts.analyze-aif-sessions
  "AIF-LM-5: Analyze session data for calibration.

   Run with: clojure -M -m scripts.analyze-aif-sessions"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(defn read-session [path]
  (try
    (edn/read-string (slurp path))
    (catch Exception e
      (println "Error reading" path ":" (.getMessage e))
      nil)))

(defn extract-aif-events [session]
  (->> (:events session)
       (filter #(#{:aif/tap :aif/policy :aif/deviation} (:event/type %)))))

(defn extract-taus [session]
  (->> (extract-aif-events session)
       (keep #(or (get-in % [:payload :aif :tau])
                  (get-in % [:payload :tau])
                  (get-in % [:aif :tau])))
       (filter number?)))

(defn extract-selections [session]
  (->> (:events session)
       (filter #(= :pattern/selection-claimed (:event/type %)))
       (map #(get-in % [:payload :psr]))))

(defn extract-deviations [session]
  (->> (:events session)
       (filter #(= :aif/deviation (:event/type %)))
       (map :payload)))

(defn analyze-session [session]
  (let [taus (extract-taus session)
        selections (extract-selections session)
        deviations (extract-deviations session)]
    {:session/id (:session/id session)
     :agent (:session/agent session)
     :event-count (count (:events session))
     :tau-values taus
     :tau-mean (when (seq taus) (/ (reduce + taus) (count taus)))
     :tau-min (when (seq taus) (apply min taus))
     :tau-max (when (seq taus) (apply max taus))
     :selection-count (count selections)
     :deviation-count (count deviations)
     :deviations-justified (count (filter :justified? deviations))}))

(defn analyze-all-sessions [sessions-dir]
  (let [files (->> (io/file sessions-dir)
                   (.listFiles)
                   (filter #(.endsWith (.getName %) ".edn")))
        sessions (->> files
                      (map #(.getPath %))
                      (map read-session)
                      (remove nil?))
        analyses (map analyze-session sessions)
        total-sessions (count analyses)
        total-selections (reduce + (map :selection-count analyses))
        total-deviations (reduce + (map :deviation-count analyses))
        justified-deviations (reduce + (map :deviations-justified analyses))
        all-taus (mapcat :tau-values analyses)]
    {:summary {:total-sessions total-sessions
               :total-selections total-selections
               :total-deviations total-deviations
               :justified-deviations justified-deviations
               :tau-samples (count all-taus)
               :tau-mean (when (seq all-taus)
                           (/ (reduce + all-taus) (count all-taus)))
               :tau-distribution (frequencies (map #(int (* 10 %)) all-taus))}
     :sessions analyses}))

(defn print-report [analysis]
  (println "\n=== AIF Session Analysis ===\n")
  (println "Summary:")
  (pp/pprint (:summary analysis))
  (println "\nPer-session details:")
  (doseq [s (:sessions analysis)]
    (when (pos? (:event-count s))
      (println (format "  %s: %d events, %d selections, %d deviations (%.2f avg tau)"
                       (:session/id s)
                       (:event-count s)
                       (:selection-count s)
                       (:deviation-count s)
                       (or (:tau-mean s) 0.0))))))

(defn -main [& args]
  (let [sessions-dir (or (first args) "lab/sessions")]
    (println "Analyzing sessions in:" sessions-dir)
    (let [analysis (analyze-all-sessions sessions-dir)]
      (print-report analysis))))

(comment
  ;; REPL usage
  (-main)
  (analyze-all-sessions "lab/sessions"))
