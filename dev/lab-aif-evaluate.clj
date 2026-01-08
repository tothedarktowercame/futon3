(ns lab-aif-evaluate
  (:require [clojure.java.io :as io]
            [futon2.aif.engine :as engine]
            [futon2.aif.adapters.fulab :as fulab]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-aif-evaluate.clj --session-id ID [--lab-root PATH]")
  (println "Computes AIF summaries for PSR/PUR events and appends them as :aif/summary."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn session-path [lab-root session-id]
  (str (io/file lab-root "sessions" (str session-id ".edn"))))

(defn psr-context [psr]
  {:decision/id (:decision/id psr)
   :session/id (:session/id psr)
   :candidates (:candidates psr)
   :chosen (:chosen psr)
   :anchors (:context/anchors psr)
   :forecast (:forecast psr)})

(defn pur-observation [pur]
  {:decision/id (:decision/id pur)
   :session/id (:session/id pur)
   :outcome (:outcome/tags pur)
   :status :observed})

(defn summary-event [session-id kind result]
  {:event/type :aif/summary
   :at (java.util.Date.)
   :payload {:session/id session-id
             :aif/kind kind
             :aif/result result}})

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        path (session-path lab-root session-id)]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? session-id) (do (println "--session-id is required") (usage) (System/exit 1))
      (not (.exists (io/file path))) (do (println "Session file not found:" path) (System/exit 1))
      :else
      (let [session (pc/read-session-file path)
            engine (engine/new-engine (fulab/new-adapter) {:beliefs {}})
            psrs (map #(get-in % [:payload :psr])
                      (filter #(= :pattern/selection-claimed (:event/type %)) (:events session)))
            purs (map #(get-in % [:payload :pur])
                      (filter #(= :pattern/use-claimed (:event/type %)) (:events session)))
            psr-events (mapv (fn [psr]
                               (summary-event session-id :psr (engine/select-pattern engine (psr-context psr))))
                             psrs)
            pur-events (mapv (fn [pur]
                               (summary-event session-id :pur (engine/update-beliefs engine (pur-observation pur))))
                             purs)
            updated (reduce pc/append-event session (concat psr-events pur-events))]
        (pc/write-session-file! path updated)
        (println (format "[lab-aif-evaluate] appended %d aif summaries" (+ (count psr-events) (count pur-events))))))))

(apply -main *command-line-args*)
