(ns futon3.real-client-interfaces-test
  "Integration tests that interact with real Futon1 API data via standard clients.

   These tests are opt-in because they hit a real running API and real storage.
   Enable with:
     FUTON3_REAL_DATA_TESTS=1 FUTON1_API_BASE=http://localhost:8080 make test-real"
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [org.httpkit.client :as http]
            [scripts.pattern-pull :as pattern-pull])
  (:import (java.net URLEncoder)
           (java.nio.file Files)))

(defn- enabled? []
  (= "1" (some-> (System/getenv "FUTON3_REAL_DATA_TESTS") str/trim)))

(defn- futon1-base []
  (some-> (System/getenv "FUTON1_API_BASE") str/trim not-empty))

(defn- futon1-url [path]
  (let [base (str/replace (futon1-base) #"/+$" "")]
    (if (re-find #"/api/(alpha|%CE%B1|%ce%b1)$" base)
      (str base path)
      (str base "/api/alpha" path))))

(defn- encode-path [value]
  (URLEncoder/encode value "UTF-8"))

(defn- get-json [path]
  (let [url (futon1-url path)
        {:keys [status body error]} @(http/get url {:headers {"accept" "application/json"}
                                                   :timeout 8000})]
    (when error
      (throw (ex-info "HTTP error" {:url url :error error})))
    {:status status
     :body (when body (json/parse-string body true))}))

(defn- relation-type [row]
  (let [rel (:relation row)]
    (cond
      (keyword? rel) rel
      (string? rel) (keyword rel)
      (map? rel) (keyword (or (:type rel) (:relation/type rel) (:label rel)))
      :else nil)))

(defn- pattern-index []
  (with-open [r (io/reader (io/file "resources/sigils/patterns-index.tsv"))]
    (->> (line-seq r)
         (remove #(str/starts-with? % "#"))
         (map #(str/split % #"\t" 5))
         (map (fn [[pid ok truth rationale hotwords]]
                {:pattern-id pid
                 :okipona ok
                 :truth truth
                 :rationale rationale
                 :hotwords hotwords}))
         (remove #(str/blank? (:pattern-id %)))
         vec)))

(defn- require-real! []
  (when-not (enabled?)
    (is true)
    ::skip))

(defn- api-reachable? []
  (when-let [base (futon1-base)]
    (try
      ;; Use an /api/alpha endpoint because futon3 clients treat FUTON1_API_BASE
      ;; as either root (http://host:port) or already-under-alpha
      ;; (http://host:port/api/alpha).
      (let [url (futon1-url "/meta/model")
            {:keys [status error]} @(http/get url {:headers {"accept" "application/json"}
                                                  :timeout 1000})]
        (and (nil? error) (= 200 status)))
      (catch Throwable _ false))))

(deftest real-standard-library-enumeration
  (when-not (= ::skip (require-real!))
    (is (futon1-base) "FUTON1_API_BASE must be set")
    (is (api-reachable?) (str "Futon1 API not reachable at " (futon1-base)))
    (let [expected (set (map :pattern-id (pattern-index)))
          {:keys [status body]} (get-json "/entities/latest?type=pattern/library&limit=2000")
          actual (set (keep :name (:entities body)))]
      (is (= 200 status))
      (is (= expected actual)))))

(deftest real-sigil-links-match-patterns-index
  (when-not (= ::skip (require-real!))
    (is (api-reachable?) (str "Futon1 API not reachable at " (futon1-base)))
    ;; Sample a handful for speed: this test is about correctness, not coverage.
    (let [sample (take 40 (pattern-index))]
      (doseq [{:keys [pattern-id okipona truth]} sample]
        (let [{:keys [status body]} (get-json (str "/ego/" (encode-path pattern-id) "?limit=2000"))
              outgoing (get-in body [:ego :outgoing])
              sigils (->> outgoing
                          (filter #(= :pattern/has-sigil (relation-type %)))
                          (map :entity)
                          (remove nil?))
              expected-name (str okipona "/" truth)]
          (is (= 200 status) (str "ego failed for " pattern-id))
          (is (seq sigils) (str "missing sigil links for " pattern-id))
          (is (some #(= expected-name (:name %)) sigils)
              (str "expected sigil " expected-name " for " pattern-id)))))))

(deftest pattern-pull-client-dry-run
  (when-not (= ::skip (require-real!))
    (is (api-reachable?) (str "Futon1 API not reachable at " (futon1-base)))
    (let [tmp (-> (Files/createTempDirectory "futon3-real-pull-"
                                             (make-array java.nio.file.attribute.FileAttribute 0))
                  (.toFile)
                  (.getAbsolutePath))
          base (futon1-base)
          ids (->> (pattern-index) (take 3) (map :pattern-id) vec)]
      ;; This exercises a standard client interface (`scripts.pattern-pull`) against
      ;; the running API, without touching the working copy.
      (is (seq base))
      (let [args (vec (concat (mapcat (fn [pid] ["--pattern" pid]) ids)
                              ["--root" tmp "--api-base" base "--dry-run"]))
            out (with-out-str (apply pattern-pull/-main args))]
        (is (re-find #"Would write" out))))))
