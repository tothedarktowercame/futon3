(ns futon3.portal
  "Bridge to futon3a portal suggestions."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(defn- env-trim [k]
  (when-let [raw (System/getenv k)]
    (let [trimmed (str/trim raw)]
      (when (seq trimmed) trimmed))))

(defn- default-portal-path []
  (or (env-trim "FUTON3A_PORTAL")
      (some-> (env-trim "FUTON3A_ROOT") (io/file "scripts" "portal") str)
      (str (io/file ".." "futon3a" "scripts" "portal"))))

(defn portal-path
  "Return the portal script path."
  []
  (default-portal-path))

(defn available?
  "Whether the portal script exists."
  []
  (-> (io/file (portal-path))
      .exists))

(defn- normalize-pattern-id [pid]
  (cond
    (nil? pid) nil
    (str/starts-with? pid "library/") (subs pid (count "library/"))
    :else pid))

(defn suggest
  "Call futon3a portal to get candidate patterns.
   Returns vector of {:id :score ...} or empty vec when unavailable."
  [{:keys [intent limit namespace]}]
  (if (and (seq intent) (available?))
    (let [portal (portal-path)
          args (cond-> [portal "suggest" intent "--limit" (str (or limit 4)) "--json"]
                 (and namespace (not (str/blank? namespace)))
                 (conj "--namespace" namespace))
          {:keys [exit out err]} (apply sh/sh args)]
      (if (zero? exit)
        (->> (json/parse-string out true)
             (mapv (fn [entry]
                     (if-let [pid (normalize-pattern-id (:id entry))]
                       (assoc entry :id pid)
                       entry))))
        (do
          (println "[portal] suggest failed:" err)
          [])))
    []))

;; === MiniLM semantic search (direct, bypasses futon1) ===

(defn- default-minilm-script []
  (or (env-trim "FUTON3A_MINILM_SCRIPT")
      (some-> (env-trim "FUTON3A_ROOT") (io/file "scripts" "notions_search.py") str)
      (str (io/file ".." "futon3a" "scripts" "notions_search.py"))))

(defn- default-minilm-venv []
  (or (env-trim "FUTON3A_VENV")
      (some-> (env-trim "FUTON3A_ROOT") (io/file ".venv" "bin" "python3") str)
      (str (io/file ".." "futon3a" ".venv" "bin" "python3"))))

(defn- default-minilm-embeddings []
  (or (env-trim "FUTON3A_MINILM_EMBEDDINGS")
      (some-> (env-trim "FUTON3A_ROOT") (io/file "resources" "notions" "minilm_pattern_embeddings.json") str)
      (str (io/file ".." "futon3a" "resources" "notions" "minilm_pattern_embeddings.json"))))

(defn minilm-available?
  "Whether MiniLM search is available (script + venv + embeddings exist)."
  []
  (and (-> (default-minilm-script) io/file .exists)
       (-> (default-minilm-venv) io/file .exists)
       (-> (default-minilm-embeddings) io/file .exists)))

(defn- parse-minilm-line [line]
  ;; Format: " 1. agent/pause-is-not-failure (0.3305) - Pause Is Not Failure"
  (when-let [[_ rank id score title] (re-matches #"\s*(\d+)\.\s+(\S+)\s+\(([0-9.]+)\)(?:\s+-\s+(.*))?" line)]
    {:id (normalize-pattern-id id)
     :score (Double/parseDouble score)
     :title (or title "")
     :rank (Long/parseLong rank)}))

(defn minilm-suggest
  "Call MiniLM semantic search directly via notions_search.py.
   Returns vector of {:id :score :title} or empty vec when unavailable."
  [{:keys [intent limit]}]
  (if (and (seq intent) (minilm-available?))
    (let [python (default-minilm-venv)
          script (default-minilm-script)
          embeddings (default-minilm-embeddings)
          {:keys [exit out err]} (sh/sh python script
                                        "--query" intent
                                        "--top" (str (or limit 8))
                                        "--embeddings" embeddings)]
      (if (zero? exit)
        (->> (str/split-lines out)
             (keep parse-minilm-line)
             vec)
        (do
          (println "[minilm] search failed:" err)
          [])))
    []))
