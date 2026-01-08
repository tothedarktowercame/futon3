(ns lab-pattern-demo
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-pattern-demo.clj [--session-id ID] [--decision-id ID] [--lab-root PATH]")
  (println "Writes PSR/PUR template drafts for a blast-radius micro-mission."))

(defn parse-args [args]
  (loop [opts {}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--decision-id" (recur (assoc opts :decision-id (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn now-id []
  (let [ts (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss") (java.util.Date.))]
    (str "fucodex-demo-" ts)))

(defn psr-template [session-id decision-id]
  {:psr/id "psr-demo-1"
   :session/id session-id
   :decision/id decision-id
   :candidates ["fulab/blast-radius" "fulab/tradeoff-record"]
   :chosen "fulab/blast-radius"
   :context/anchors [{:anchor/type :code/edit
                      :anchor/ref {:event/type :code/edit
                                   :file ""
                                   :fn ""}}]
   :forecast {:benefits [{:tag :benefit/risk-clarity
                          :locus {:anchor/type :code/edit
                                  :anchor/ref {:event/type :code/edit
                                               :file ""}}
                          :note "Explicitly record blast surfaces + rollback scope."}]
              :risks [{:tag :risk/overhead
                       :locus {:anchor/type :code/edit
                               :anchor/ref {:event/type :code/edit
                                            :file ""}}
                       :note "Adds a logging step during small changes."}]
              :success [{:tag :success/traceable
                         :locus {:anchor/type :code/edit
                                 :anchor/ref {:event/type :code/edit
                                              :file ""}}
                         :note "Change remains traceable to the declared blast radius."}]
              :failure [{:tag :failure/under-scoped
                         :locus {:anchor/type :code/edit
                                 :anchor/ref {:event/type :code/edit
                                              :file ""}}
                         :note "Unexpected side-effect outside declared scope."}]}
   :rejections {"fulab/tradeoff-record" {:codes [:reject/fit]
                                        :note "Need blast boundary more than tradeoff detail."}}
   :horizon :immediate})

(defn pur-template [session-id decision-id]
  {:pur/id "pur-demo-1"
   :session/id session-id
   :pattern/id "fulab/blast-radius"
   :instance/id "pur-demo-1-a"
   :decision/id decision-id
   :fields {:context "Chose a small change during the session."
            :if "Potential side-effects existed."
            :however "Scope and rollback were unclear pre-change."
            :then "Declared blast surfaces + rollback before editing."
            :because "Needed traceable risk boundaries for the change."
            :next-steps "Compare declared blast radius with actual effects."}
   :anchors [{:anchor/type :code/edit
              :anchor/ref {:event/type :code/edit
                           :file ""
                           :fn ""}}]
   :outcome/tags [:outcome/partial]})

(defn -main [& args]
  (let [{:keys [help unknown session-id decision-id lab-root]} (parse-args args)
        repo-root (System/getProperty "user.dir")
        lab-root (or lab-root (str (io/file repo-root "lab")))
        session-id (or session-id (now-id))
        decision-id (or decision-id "decision-1")
        psr (psr-template session-id decision-id)
        pur (pur-template session-id decision-id)
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
