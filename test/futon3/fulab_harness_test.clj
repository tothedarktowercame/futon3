(ns futon3.fulab-harness-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [futon3.fulab.harness :as harness]))

(deftest regime-sweep-smoke
  (let [out-dir (-> (java.nio.file.Files/createTempDirectory "fulab-harness")
                    (.toFile)
                    (.getPath))
        result (harness/run-sweep {:seed 7 :max-steps 12 :out-dir out-dir})
        runs (mapcat (fn [[_ regime]]
                       (mapcat (fn [[_ ctrl]] (:runs ctrl))
                               (:controllers regime)))
                     (:regimes result))]
    (is (= (* 2 2 (count harness/tasks)) (count runs)))
    (is (every? #(<= 0.0 (:success-rate %) 1.0)
                (for [[_ regime] (:regimes result)
                      [_ ctrl] (:controllers regime)]
                  (:metrics ctrl))))
    (is (.exists (io/file (get-in result [:output/paths :edn]))))
    (is (.exists (io/file (get-in result [:output/paths :json]))))
    (is (.exists (io/file (get-in result [:output/paths :md]))))))
