(ns lab-pattern-demo
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-pattern-demo.clj [--session-id ID] [--decision-id ID] [--pattern-id ID] [--alt-pattern-id ID] [--lab-root PATH]")
  (println "Writes PSR/PUR template drafts for a blast-radius micro-mission."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--decision-id" (recur (assoc opts :decision-id (second remaining)) (nnext remaining))
        "--pattern-id" (recur (assoc opts :pattern-id (second remaining)) (nnext remaining))
        "--alt-pattern-id" (recur (assoc opts :alt-pattern-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn now-id []
  (let [ts (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss") (java.util.Date.))]
    (str "fucodex-demo-" ts)))

(defn psr-template [session-id decision-id pattern-id alt-pattern-id]
  {:psr/id "psr-demo-1"
   :session/id session-id
   :decision/id decision-id
   :candidates [pattern-id alt-pattern-id]
   :chosen pattern-id
   :context/anchors [{:anchor/type :code/edit
                      :anchor/ref {:event/type :code/edit
                                   :file ""
                                   :fn ""}}]
   :forecast {:benefits [{:tag :benefit/traceable-choice
                          :locus {:anchor/type :code/edit
                                  :anchor/ref {:event/type :code/edit
                                               :file ""}}
                          :note "Record why this pattern was selected."}]
              :risks [{:tag :risk/mismatch
                       :locus {:anchor/type :code/edit
                               :anchor/ref {:event/type :code/edit
                                            :file ""}}
                       :note "Pattern may not fit the actual change."}]
              :success [{:tag :success/traceable-outcome
                         :locus {:anchor/type :code/edit
                                 :anchor/ref {:event/type :code/edit
                                              :file ""}}
                         :note "Outcome stays traceable to the choice."}]
              :failure [{:tag :failure/unresolved
                         :locus {:anchor/type :code/edit
                                 :anchor/ref {:event/type :code/edit
                                              :file ""}}
                         :note "Decision rationale doesn't match effects."}]}
   :rejections {alt-pattern-id {:codes [:reject/fit]
                                :note "Alternate pattern not selected for this change."}}
   :horizon :immediate})

(defn pur-template [session-id decision-id pattern-id]
  {:pur/id "pur-demo-1"
   :session/id session-id
   :pattern/id pattern-id
   :instance/id "pur-demo-1-a"
   :decision/id decision-id
   :fields {:context "Chose a small change during the session."
            :if "A pattern was selected to guide the change."
            :however "Fit could only be confirmed after the change."
            :then "Applied the selected pattern and logged outcomes."
            :because "We need traceable decisions for small changes."
            :next-steps "Compare the chosen pattern with observed effects."}
   :anchors [{:anchor/type :code/edit
              :anchor/ref {:event/type :code/edit
                           :file ""
                           :fn ""}}]
   :outcome/tags [:outcome/partial]})

(defn -main [& args]
  (let [{:keys [help unknown session-id decision-id lab-root pattern-id alt-pattern-id]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        session-id (or session-id (now-id))
        decision-id (or decision-id "decision-1")
        pattern-id (or pattern-id "fulab/blast-radius")
        alt-pattern-id (or alt-pattern-id "fulab/tradeoff-record")
        psr (psr-template session-id decision-id pattern-id alt-pattern-id)
        pur (pur-template session-id decision-id pattern-id)
        drafts-dir (io/file lab-root "pattern-drafts")
        psr-path (io/file drafts-dir (str session-id "-psr.edn"))
        pur-path (io/file drafts-dir (str session-id "-pur.edn"))]
    (cond
      help (usage)
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      :else
      (do
        (io/make-parents psr-path)
        (spit psr-path (pr-str psr))
        (spit pur-path (pr-str pur))
        (println (format "[lab-pattern-demo] session=%s decision=%s" session-id decision-id))
        (println (format "[lab-pattern-demo] psr=%s" (str psr-path)))
        (println (format "[lab-pattern-demo] pur=%s" (str pur-path)))))))

(apply -main *command-line-args*)
