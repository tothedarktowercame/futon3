(ns test-runner
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [graph-test]
            [learn-or-act-test]
            [pattern-hints-test]
            [futon3.pattern-store-xtdb-test]
            [semantics-test]
            [transport-test]
            [f2.transport-golden-test])
  (:gen-class))

(defn -main [& _]
  (let [targets (some-> (System/getenv "TEST_NS")
                        (str/trim)
                        (not-empty))
        runner (if targets
                 (let [names (map str/trim (str/split targets #","))
                       syms (map symbol names)]
                   (doseq [ns-sym syms]
                     (require ns-sym))
                   #(apply t/run-tests syms))
                 #(t/run-all-tests))
        {:keys [fail error]} (runner)]
    (when (pos? (+ fail error))
      (System/exit 1))))
