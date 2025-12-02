(ns test-runner
  (:require [clojure.test :as t]
           [graph-test]
           [learn-or-act-test]
           [semantics-test]
           [transport-test])
  (:gen-class))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-all-tests)]
    (when (pos? (+ fail error))
      (System/exit 1))))
