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
