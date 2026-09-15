(require '[clojure.test :as t]
         '[buffer-cleaner.classify-test])
(def res (t/run-tests 'buffer-cleaner.classify-test))
(when (pos? (+ (:fail res) (:error res))) (System/exit 1))
