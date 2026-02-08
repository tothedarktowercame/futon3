(ns futon3.agency.invariants.util
  "Test utilities for Agency invariant proof tests."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.agency.registry])
  (:import (java.io ByteArrayInputStream)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(defn temp-dir!
  "Create a fresh temporary directory under /tmp for test state."
  []
  (let [d (io/file "/tmp" (str "futon3-agency-test-" (UUID/randomUUID)))]
    (.mkdirs d)
    (.getAbsolutePath d)))

(defn- ns-var [ns-sym var-sym]
  (or (ns-resolve ns-sym var-sym)
      (throw (ex-info "var not found" {:ns ns-sym :var var-sym}))))

(defn reset-agency-state!
  "Reset in-process Agency state atoms used by futon3.agency.http."
  []
  (doseq [v ['secrets 'acks 'rendezvous-state 'connected-agents 'local-handlers 'pending-whistles]]
    (reset! @(ns-var 'futon3.agency.http v) {})))

(defn reset-registry!
  []
  (reset! futon3.agency.registry/agent-registry {}))

(defn configure-state-dir!
  "Point futon3.agency.service at a temp state dir and clear in-memory state."
  [state-dir]
  (let [cfg-var (ns-var 'futon3.agency.service '!config)
        st-var (ns-var 'futon3.agency.service '!state)]
    (swap! @cfg-var assoc :state-dir state-dir)
    (reset! @st-var {})))

(defn json-body
  ^ByteArrayInputStream
  [m]
  (let [s (json/encode m)]
    (ByteArrayInputStream. (.getBytes s StandardCharsets/UTF_8))))

(defn ring-json-request
  "Build a minimal Ring request for futon3.agency.http/handler."
  [method uri body-map]
  {:request-method method
   :uri uri
   :headers {"content-type" "application/json"}
   :body (when body-map (json-body body-map))})

(defn parse-json-response
  "Parse handler response body to a Clojure map (keyword keys)."
  [{:keys [body]}]
  (when (some? body)
    (json/parse-string (str body) true)))

(defn read-file
  [path]
  (slurp (io/file path)))

(defn clj-files-under
  "Return all .clj files under a directory."
  [dir]
  (let [root (io/file dir)]
    (->> (file-seq root)
         (filter #(.isFile %))
         (filter #(str/ends-with? (.getName %) ".clj")))))

(defn find-re-matches
  "Return [{:file path :line n :text line}] for lines matching re."
  [files re]
  (mapcat
   (fn [^java.io.File f]
     (->> (str/split-lines (slurp f))
          (map-indexed (fn [i line]
                         (when (re-find re line)
                           {:file (.getPath f) :line (inc i) :text line})))
          (remove nil?)))
   files))
