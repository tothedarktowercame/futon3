(ns scripts.nonstarter-propose
  "Babashka helper to submit a nonstarter proposal via futon5 CLI.")

(require '[clojure.edn :as edn]
         '[clojure.java.shell :as shell]
         '[clojure.string :as str])

(defn- usage []
  (str "usage: scripts/nonstarter-propose --title TEXT (--ask N | --estimate EDN) [opts]\n\n"
       "opts:\n"
       "  --db PATH           Optional; defaults to FUTON5_NONSTARTER_DB or ../futon5/data/nonstarter.db\n"
       "  --description TEXT  Optional\n"
       "  --sigil TEXT        Optional\n"
       "  --proposer NAME     Optional\n\n"
       "env:\n"
       "  FUTON5_ROOT         Path to futon5 repo (default ../futon5)\n"
       "  FUTON5_NONSTARTER_DB  Path to nonstarter db (default data/nonstarter.db under FUTON5_ROOT)\n"))

(defn- parse-args [args]
  (loop [opts {:forward []}
         remaining args]
    (if (empty? remaining)
      opts
      (case (first remaining)
        "--db" (recur (assoc opts :db (second remaining)) (nnext remaining))
        "--title" (recur (assoc opts :title (second remaining)
                                 :forward (conj (:forward opts) "--title" (second remaining)))
                         (nnext remaining))
        "--ask" (recur (assoc opts :forward (conj (:forward opts) "--ask" (second remaining)))
                       (nnext remaining))
        "--estimate" (recur (assoc opts :forward (conj (:forward opts) "--estimate" (second remaining)))
                            (nnext remaining))
        "--description" (recur (assoc opts :forward (conj (:forward opts) "--description" (second remaining)))
                               (nnext remaining))
        "--sigil" (recur (assoc opts :forward (conj (:forward opts) "--sigil" (second remaining)))
                         (nnext remaining))
        "--proposer" (recur (assoc opts :forward (conj (:forward opts) "--proposer" (second remaining)))
                            (nnext remaining))
        (recur (update opts :forward conj (first remaining)) (next remaining))))))

(defn- ensure-required [{:keys [title forward]}]
  (when-not title
    (binding [*out* *err*]
      (println (usage)))
    (System/exit 2))
  (when-not (some #{"--ask" "--estimate"} forward)
    (binding [*out* *err*]
      (println (usage)))
    (System/exit 2)))

(defn- default-root []
  (or (some-> (System/getenv "FUTON5_ROOT") str/trim not-empty)
      "../futon5"))

(defn- default-db [root]
  (or (some-> (System/getenv "FUTON5_NONSTARTER_DB") str/trim not-empty)
      (str (str/replace root #"/+$" "") "/data/nonstarter.db")))

(defn- read-edn-safe [s]
  (try
    (edn/read-string s)
    (catch Throwable _ s)))

(defn -main [& args]
  (let [{:keys [db forward] :as opts} (parse-args args)
        _ (ensure-required opts)
        root (default-root)
        db-path (or db (default-db root))
        cmd (into ["clj" "-M" "-m" "scripts.nonstarter-propose" "--db" db-path]
                  forward)
        {:keys [exit out err]} (apply shell/sh (concat cmd [:dir root]))
        result (read-edn-safe (str/trim out))]
    (if (zero? exit)
      (prn {:ok true
            :mode :local
            :root root
            :db db-path
            :result result})
      (do
        (binding [*out* *err*]
          (println err))
        (prn {:ok false
              :mode :local
              :root root
              :db db-path
              :error (str/trim err)
              :result result})))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

