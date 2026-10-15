(ns futon3.test-runner
  "Run the suite in bounded JVM cohorts so namespace-global executors and
  sleeping futures are reaped between cohorts."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def ^:private batch-size 1)

(defn- test-namespaces []
  (->> (file-seq (io/file "test"))
       (filter #(.isFile %))
       (map #(.getPath %))
       (filter #(str/ends-with? % "_test.clj"))
       (map #(-> %
                 (subs (inc (count "test")))
                 (str/replace #"\.clj$" "")
                 (str/replace java.io.File/separator ".")
                 (str/replace "_" "-")
                 symbol))
       sort
       vec))

(defn- run-batch! [index namespaces]
  (println (format "\nFuton3 test batch %d: %d namespaces" index (count namespaces)))
  (let [{:keys [exit out err]}
        (apply shell/sh "clojure" "-M:test-batch" "-m" "cognitect.test-runner"
               (mapcat (fn [ns] ["-n" (str ns)]) namespaces))]
    (print out)
    (binding [*out* *err*] (print err))
    (when-not (zero? exit)
      (throw (ex-info "Futon3 test batch failed"
                      {:batch index :namespaces namespaces :exit exit})))))

(defn test [_]
  (let [namespaces (test-namespaces)]
    (when (empty? namespaces)
      (throw (ex-info "No Futon3 test namespaces discovered" {})))
    (doseq [[index batch] (map-indexed vector (partition-all batch-size namespaces))]
      (run-batch! (inc index) batch))
    {:namespaces (count namespaces)}))
