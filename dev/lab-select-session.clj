#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(import '(java.time Instant))

(defn usage []
  (println "Usage: dev/lab-select-session.clj --since ISO --cwd PATH [--sessions-root PATH]")
  (println)
  (println "Prints matching Codex sessions as: session-id<TAB>file"))

(defn parse-args [args]
  (loop [opts {} remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--since" (recur (assoc opts :since (second remaining)) (nnext remaining))
        "--cwd" (recur (assoc opts :cwd (second remaining)) (nnext remaining))
        "--sessions-root" (recur (assoc opts :sessions-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- parse-instant [s]
  (try
    (Instant/parse s)
    (catch Exception _ nil)))

(defn- session-meta [file]
  (with-open [r (io/reader file)]
    (when-let [line (first (line-seq r))]
      (try
        (let [m (json/read-str line :key-fn keyword)]
          (when (= "session_meta" (:type m))
            {:file (.getAbsolutePath (io/file file))
             :session-id (get-in m [:payload :id])
             :timestamp (get-in m [:payload :timestamp])
             :cwd (get-in m [:payload :cwd])}))
        (catch Exception _ nil)))))

(defn- matching-sessions [root since target-cwd]
  (let [since-ts (parse-instant since)]
    (->> (file-seq (io/file root))
         (filter #(.isFile ^java.io.File %))
         (filter #(str/ends-with? (.getName ^java.io.File %) ".jsonl"))
         (keep session-meta)
         (filter (fn [{:keys [timestamp cwd file]}]
                   (and timestamp
                        (= cwd target-cwd)
                        (when-let [ts (parse-instant timestamp)]
                          (if since-ts
                            (not (.isBefore ts since-ts))
                            true)))))
         (sort-by :timestamp))))

(defn -main [& args]
  (let [{:keys [help unknown since cwd sessions-root]} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (or (nil? since) (nil? cwd)) (do (println "--since and --cwd are required") (usage) (System/exit 1))
      :else
      (let [root (or sessions-root
                     (str (io/file (System/getProperty "user.home") ".codex" "sessions")))]
        (doseq [{:keys [session-id file]} (matching-sessions root since cwd)]
          (println (str (or session-id "") "\t" file)))))))

(apply -main *command-line-args*)
