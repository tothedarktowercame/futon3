(ns futon3.checks-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [futon3.checks :as checks]))

(def sample-pattern
  {:pattern/id "library/devmap"
   :pattern/title "Devmap Canon"
   :pattern/rationale "Ensure devmap clauses tie to logged evidence."
   :pattern/hotwords ["devmap" "prototype"]
   :pattern/hanzi "本"})

(defn- temp-log-file []
  (doto (java.io.File/createTempFile "checks" ".edn")
    (.delete)
    (.deleteOnExit)))

(deftest check-writes-proof-log
  (let [tmp (temp-log-file)
        original (checks/current-log-path)]
    (try
      (checks/set-log-path! (.getAbsolutePath tmp))
      (with-redefs [checks/catalog! (fn [] {"library/devmap" sample-pattern})]
        (let [result (checks/check! {:pattern/id "library/devmap"
                                     :context "Prototype devmap obligations discharged"
                                     :evidence ["README.md#state"]
                                     :sigils ["f3" "proto1"]
                                     :prototypes ["prototype-1"]
                                     :origin {:source :workday :workday/id "WD-42"}})
              logged-line (with-open [r (io/reader tmp)]
                            (first (line-seq r)))
              logged-entry (some-> logged-line edn/read-string)]
          (is (:ok result))
          (is (= :applies (:status result)))
          (is (= [] (:missing result)))
          (is (= (.getAbsolutePath tmp) (:log-path result)))
          (is (= (:proof result) logged-entry))
          (is (= "library/devmap" (get-in result [:proof :pattern/id])))
          (is (= :applies (get-in result [:proof :proof/status])))))
      (finally
        (checks/set-log-path! original)))))

(deftest check-invalid-requests-return-structured-errors
  (with-redefs [checks/catalog! (fn [] {"library/devmap" sample-pattern})]
    (is (= "missing-pattern-id"
           (:err (checks/check! {:context "x"}))))
    (is (= "missing-context"
           (:err (checks/check! {:pattern/id "library/devmap"}))))
    (let [resp (checks/check! {:pattern/id "library/devmap" :context 42})]
      (is (= "invalid-request" (:err resp)))
      (is (seq (:details resp))))
    (let [resp (checks/check! {:pattern/id "library/devmap"
                               :context "ok"
                               :unexpected true})]
      (is (= "invalid-request" (:err resp)))))
  (with-redefs [checks/catalog! (fn [] {})]
    (is (= "unknown-pattern"
           (:err (checks/check! {:pattern/id "missing"
                                 :context "ok"}))))))
