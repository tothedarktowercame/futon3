(ns scripts.pattern-pull
  "Pull patterns from Futon1/XTDB into the filesystem working copy."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import (java.io File)
           (java.net URLEncoder)))

(def ^:private default-timeout-ms 4000)

(defn- usage []
  (str/join
   "\n"
   ["Usage: clj -M -m scripts.pattern-pull [--root PATH] --pattern NAME [--pattern NAME]"
    "                                     [--all] [--api-base URL] [--profile NAME] [--dry-run]"
    "Environment variables:"
    "  FUTON1_API_BASE   Base URL for Futon1 API (e.g. http://localhost:8080/api/alpha)"
    "  FUTON1_PROFILE    Optional profile header"
    "  FUTON3_ROOT       Default root if --root not provided"])))

(defn- parse-args [args]
  (loop [opts {:root (or (some-> (System/getenv "FUTON3_ROOT") str/trim not-empty) ".")
               :patterns []
               :all? false
               :dry-run? false
               :api-base nil
               :profile nil}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--root" (recur (assoc opts :root (second remaining)) (nnext remaining))
        "--pattern" (recur (update opts :patterns conj (second remaining)) (nnext remaining))
        "--all" (recur (assoc opts :all? true) (rest remaining))
        "--api-base" (recur (assoc opts :api-base (second remaining)) (nnext remaining))
        "--profile" (recur (assoc opts :profile (second remaining)) (nnext remaining))
        "--dry-run" (recur (assoc opts :dry-run? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- normalize-base [base]
  (some-> base str/trim (str/replace #"/+$" "")))

(defn- api-base [opts]
  (or (normalize-base (:api-base opts))
      (normalize-base (System/getenv "FUTON1_API_BASE"))))

(defn- api-root [opts]
  (let [base (api-base opts)]
    (when base
      (if (re-find #"/api/(alpha|%CE%B1|%ce%b1|%CE%91|%ce%91)$" base)
        base
        (str base "/api/alpha")))))

(defn- profile [opts]
  (or (:profile opts)
      (some-> (System/getenv "FUTON1_PROFILE") str/trim not-empty)
      (some-> (System/getenv "FUTON1_API_PROFILE") str/trim not-empty)))

(defn- headers [opts]
  (cond-> {"accept" "application/json"}
    (seq (profile opts)) (assoc "x-profile" (profile opts))))

(defn- encode-segment [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- request-json! [opts method path]
  (when-not (api-root opts)
    (throw (ex-info "FUTON1_API_BASE is not set" {})))
  (let [url (str (api-root opts) path)
        req (case method
              :get #(http/get url {:headers (headers opts)
                                   :timeout default-timeout-ms}))
        {:keys [status error body]} @(req)]
    (when error
      (throw (ex-info "HTTP error" {:url url :error error})))
    {:status status
     :body (when body (json/parse-string body true))}))

(defn- fetch-entity [opts name]
  (request-json! opts :get (str "/entity/" (encode-segment name))))

(defn- fetch-ego [opts name]
  (request-json! opts :get (str "/ego/" (encode-segment name) "?limit=2000")))

(defn- relation-type [row]
  (let [rel (:relation row)]
    (cond
      (keyword? rel) rel
      (string? rel) (keyword rel)
      (map? rel) (keyword (or (:type rel) (:relation/type rel) (:label rel)))
      :else nil)))

(defn- outgoing-links [ego]
  (or (get ego :outgoing)
      (get-in ego [:links :outgoing])
      []))

(defn- component-order [pattern-name component-name]
  (when (and pattern-name component-name)
    (when-let [[_ digits] (re-find (re-pattern (str "^" (java.util.regex.Pattern/quote pattern-name) "/(\\d+)-")) component-name)]
      (try
        (Integer/parseInt digits)
        (catch Exception _ nil)))))

(defn- pattern-path [root pattern-name]
  (let [segments (str/split pattern-name #"/")
        filename (str (last segments) ".flexiarg")
        dir (apply io/file (concat [(io/file root "library")] (butlast segments)))]
    (io/file dir filename)))

(defn- ensure-parent! [^File file]
  (when-let [parent (.getParentFile file)]
    (.mkdirs parent)))

(defn- format-section [idx label text]
  (let [label (or label "section")
        marker (if (and (= idx 0)
                        (or (= "conclusion" (str/lower-case label))
                            (= "claim" (str/lower-case label))))
                 "!"
               (if (= idx 0) "!" "+"))
        header (str marker " " label ":")
        body (if (str/blank? text)
               ""
               (->> (str/split-lines text)
                    (map #(str "  " %))
                    (str/join "\n")))]
    (str header
         (when (seq body) (str "\n" body)))))

(defn- render-pattern [pattern-name title summary components]
  (let [header (str "@flexiarg " pattern-name
                    (when (seq title) (str "\n@title " title))
                    "\n\n")
        base-components (if (seq components)
                          components
                          [{:label "conclusion" :text summary}])
        sections (->> base-components
                      (map-indexed (fn [idx comp]
                                     (format-section idx (:label comp) (:text comp)))))
        body (str/join "\n\n" sections)]
    (str header body "\n")))

(defn- pull-pattern [opts pattern-name]
  (let [{:keys [status body]} (fetch-entity opts pattern-name)]
    (when-not (= 200 status)
      (throw (ex-info "Pattern not found" {:name pattern-name :status status})))
    (let [entity (:entity body)
          title (or (:external-id entity) (:entity/external-id entity))
          summary (or (:source entity) (:entity/source entity) "")
          ego-body (:body (fetch-ego opts pattern-name))
          ego (:ego ego-body)
          links (->> (outgoing-links ego)
                     (filter #(= :pattern/includes (relation-type %))))
          component-names (->> links
                               (map #(or (get-in % [:entity :name])
                                         (get-in % [:entity :entity/name])
                                         (get-in % [:entity :id])
                                         (get-in % [:entity :entity/id])))
                               (remove nil?)
                               distinct)
          components (->> component-names
                          (map (fn [component-name]
                                 (let [{:keys [status body]} (fetch-entity opts component-name)]
                                   (when (= 200 status)
                                     (let [ent (:entity body)
                                           label (or (:external-id ent) (:entity/external-id ent))
                                           text (or (:source ent) (:entity/source ent) "")
                                           order (component-order pattern-name component-name)]
                                       {:name component-name
                                        :label label
                                        :text text
                                        :order order})))))
                          (remove nil?)
                          (sort-by #(or (:order %) Integer/MAX_VALUE)))
          target (pattern-path (:root opts) pattern-name)
          rendered (render-pattern pattern-name title summary components)]
      (if (:dry-run? opts)
        (println (format "Would write %s (%d components)" (.getPath target) (count components)))
        (do
          (ensure-parent! target)
          (spit target rendered)
          (println (format "Wrote %s (%d components)" (.getPath target) (count components))))))))

(defn- list-patterns [opts]
  (let [{:keys [status body]} (request-json! opts :get "/entities/latest?type=pattern/library&limit=2000")]
    (when-not (= 200 status)
      (throw (ex-info "Failed to list patterns" {:status status})))
    (->> (:entities body)
         (map :name)
         (remove str/blank?)
         vec)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (when-not (api-root opts)
      (throw (ex-info "FUTON1_API_BASE is required" {:help (usage)})))
    (let [patterns (cond
                     (seq (:patterns opts)) (:patterns opts)
                     (:all? opts) (list-patterns opts)
                     :else nil)]
      (when-not (seq patterns)
        (throw (ex-info "No patterns requested" {:help (usage)})))
      (doseq [name patterns]
        (pull-pattern opts name)))))
