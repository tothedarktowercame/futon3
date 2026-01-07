(ns lab-pattern-suggest
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-pattern-suggest.clj --session-id ID --lab-root PATH --decision-id ID [--repo-root PATH] [--print-only]")
  (println "Writes an unverified PSR draft event based on recent session context."))

(defn parse-args [args]
  (loop [opts {:print-only false}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--session-file" (recur (assoc opts :session-file (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--repo-root" (recur (assoc opts :repo-root (second remaining)) (nnext remaining))
        "--decision-id" (recur (assoc opts :decision-id (second remaining)) (nnext remaining))
        "--print-only" (recur (assoc opts :print-only true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn session-path [{:keys [session-id session-file lab-root]}]
  (cond
    session-file session-file
    (and lab-root session-id) (str (io/file lab-root "sessions" (str session-id ".edn")))
    :else nil))

(defn candidate-patterns [session]
  (let [deps (map :pattern/dep (:pattern-deps session))
        clock-in (get-in session [:clock-in :clock-in/pattern-id])]
    (->> (concat deps [clock-in])
         (remove nil?)
         (remove str/blank?)
         distinct
         vec)))

(defn draft-psr [session-id decision-id candidates]
  (let [base (pc/psr-template session-id decision-id)
        chosen (first candidates)
        rejections (into {}
                         (for [p (remove #{chosen} candidates)]
                           [p {:codes [:rejection/unspecified]}]))
        override? (< (count candidates) 2)
        base (assoc base
                    :candidates (if (seq candidates) candidates [""])
                    :chosen (or chosen "")
                    :rejections rejections)]
    (cond-> base
      override? (assoc :override/solo? true
                       :override/note "Only one candidate found in session context."))))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root decision-id print-only] :as opts}
        (parse-args args)
        path (session-path opts)]
    (cond
      help (do (usage) (System/exit 0))
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? path) (do (println "--session-id/--session-file and --lab-root are required") (usage) (System/exit 1))
      (nil? decision-id) (do (println "--decision-id is required") (usage) (System/exit 1))
      (not (.exists (io/file path))) (do (println "Session file not found:" path) (System/exit 1))
      :else
      (let [session (pc/read-session-file path)
            candidates (candidate-patterns session)
            draft (draft-psr session-id decision-id candidates)
            event (pc/wrap-claim-event draft)]
        (if print-only
          (do (prn event)
              (println "[lab-pattern-suggest] print-only"))
          (let [updated (pc/append-event session event)]
            (pc/write-session-file! path updated)
            (println (format "[lab-pattern-suggest] wrote %s" (:event/type event)))))))))
