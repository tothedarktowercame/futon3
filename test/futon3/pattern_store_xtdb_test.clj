(ns futon3.pattern-store-xtdb-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [org.httpkit.client :as http])
  (:import (java.net URLEncoder)))

(defn- futon1-base []
  (some-> (System/getenv "FUTON1_API_BASE") str/trim not-empty))

(defn- futon1-profile []
  (or (some-> (System/getenv "FUTON1_PROFILE") str/trim not-empty)
      (some-> (System/getenv "FUTON1_API_PROFILE") str/trim not-empty)))

(defn- futon1-url [path]
  (let [base (str/replace (futon1-base) #"/+$" "")]
    (if (re-find #"/api/(alpha|%CE%B1|%ce%b1)$" base)
      (str base path)
      (str base "/api/alpha" path))))

(defn- headers []
  (cond-> {"accept" "application/json"}
    (seq (futon1-profile)) (assoc "x-profile" (futon1-profile))))

(defn- get-json [path]
  (let [url (futon1-url path)
        {:keys [status body error]} @(http/get url {:headers (headers) :timeout 4000})]
    (when error
      (throw (ex-info "HTTP error" {:url url :error error})))
    {:status status
     :body (when body (json/parse-string body true))}))

(defn- encode-path [value]
  (URLEncoder/encode value "UTF-8"))

(defn- pattern-index-ids []
  (with-open [r (io/reader (io/file "resources/sigils/patterns-index.tsv"))]
    (->> (line-seq r)
         (remove #(str/starts-with? % "#"))
         (map #(first (str/split % #"\t")))
         (remove str/blank?)
         vec)))

(defn- relation-type [row]
  (let [rel (:relation row)]
    (cond
      (keyword? rel) rel
      (symbol? rel) (keyword (str rel))
      (string? rel) (keyword rel)
      (map? rel) (keyword (or (:type rel) (:relation/type rel)))
      :else nil)))

(defn- entity-type
  "Normalize entity :type field (string/keyword/symbol) into a keyword.

   Some Futon1-compatible APIs return types as strings (e.g. \"pattern/library\")."
  [v]
  (cond
    (keyword? v) v
    (symbol? v) (keyword (str v))
    (string? v) (keyword (str/replace v #"^:" ""))
    :else nil))

(defn- ensure-futon1! []
  (when-not (futon1-base)
    (println "Skipping XTDB tests; FUTON1_API_BASE not set.")
    (is true)))

(deftest pattern-store-loads-and-enumerates
  (if-not (futon1-base)
    (ensure-futon1!)
    (let [expected (pattern-index-ids)
          {:keys [status body]} (get-json "/entities/latest?type=pattern/library&limit=2000")
          names (->> (get body :entities)
                     (map :name)
                     (remove str/blank?)
                     set)]
      (is (= 200 status))
      (doseq [pattern-id expected]
        (is (contains? names pattern-id) (str "missing in XTDB: " pattern-id))))))

(deftest pattern-metadata-and-links-complete
  (if-not (futon1-base)
    (ensure-futon1!)
    (doseq [pattern-id (pattern-index-ids)]
      (let [entity-path (str "/entity/" (encode-path pattern-id))
            ego-path (str "/ego/" (encode-path pattern-id))
            {:keys [status body]} (get-json entity-path)
            entity (get body :entity)
            {:keys [status ego-body]} (let [resp (get-json ego-path)]
                                        {:status (:status resp)
                                         :ego-body (:body resp)})
            outgoing (get-in ego-body [:ego :outgoing])
            types (set (keep relation-type outgoing))
            includes (filter #(= :pattern/includes (relation-type %)) outgoing)
            sigils (filter #(= :pattern/has-sigil (relation-type %)) outgoing)]
        (is (= 200 status))
        (is (= pattern-id (:name entity)))
        (is (= :pattern/library (entity-type (:type entity))))
        (is (seq includes))
        (is (seq sigils))
        (doseq [row includes]
          (is (seq (get-in row [:entity :source]))))))))

(deftest pattern-store-matches-standard-library
  (if-not (futon1-base)
    (ensure-futon1!)
    (let [expected (set (pattern-index-ids))
          {:keys [status body]} (get-json "/entities/latest?type=pattern/library&limit=2000")
          names (->> (get body :entities)
                     (map :name)
                     (remove str/blank?)
                     set)]
      (is (= 200 status))
      (is (= expected names)))))
