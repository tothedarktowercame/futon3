(ns lab-pattern-claim
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.fulab.pattern-competence :as pc]))

(defn usage []
  (println "Usage: dev/lab-pattern-claim.clj --session-id ID --lab-root PATH --kind pur|psr [--decision-id ID] [--stdin] [--print-only]")
  (println "Writes a PUR/PSR claim event into lab/sessions/<id>.edn (append-only)."))

(defn parse-args [args]
  (loop [opts {:stdin false :print-only false}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--session-id" (recur (assoc opts :session-id (second remaining)) (nnext remaining))
        "--session-file" (recur (assoc opts :session-file (second remaining)) (nnext remaining))
        "--lab-root" (recur (assoc opts :lab-root (second remaining)) (nnext remaining))
        "--kind" (recur (assoc opts :kind (second remaining)) (nnext remaining))
        "--decision-id" (recur (assoc opts :decision-id (second remaining)) (nnext remaining))
        "--stdin" (recur (assoc opts :stdin true) (rest remaining))
        "--print-only" (recur (assoc opts :print-only true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (update opts :unknown (fnil conj []) (first remaining)) (rest remaining))))))

(defn session-path [{:keys [session-id session-file lab-root]}]
  (cond
    session-file session-file
    (and lab-root session-id) (str (io/file lab-root "sessions" (str session-id ".edn")))
    :else nil))

(defn read-stdin-edn []
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (edn/read-string content))))

(defn -main [& args]
  (let [{:keys [help unknown session-id lab-root kind decision-id stdin print-only] :as opts}
        (parse-args args)
        path (session-path opts)]
    (cond
      help (do (usage) (System/exit 0))
      (seq unknown) (do (println "Unknown args:" unknown) (usage) (System/exit 1))
      (nil? path) (do (println "--session-id/--session-file and --lab-root are required") (usage) (System/exit 1))
      (not (.exists (io/file path))) (do (println "Session file not found:" path) (System/exit 1))
      (and (not stdin) (nil? kind)) (do (println "--kind is required unless --stdin is used") (usage) (System/exit 1))
      (and (= kind "psr") (nil? decision-id)) (do (println "--decision-id is required for psr templates") (usage) (System/exit 1))
      :else
      (let [session (pc/read-session-file path)
            session-id (or session-id (:session/id session))
            record (cond
                     stdin (read-stdin-edn)
                     (= kind "pur") (pc/pur-template session-id)
                     (= kind "psr") (pc/psr-template session-id decision-id)
                     :else nil)
            event (cond
                    (nil? record) nil
                    (:event/type record) record
                    :else (pc/wrap-claim-event record))]
        (when (nil? event)
          (println "No record to write.")
          (System/exit 1))
        (if print-only
          (do (prn event)
              (println "[lab-pattern-claim] print-only"))
          (let [updated (pc/append-event session event)]
            (pc/write-session-file! path updated)
            (println (format "[lab-pattern-claim] wrote %s" (:event/type event)))))))))
