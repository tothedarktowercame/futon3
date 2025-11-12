(ns semantics-test
  (:require [clojure.test :refer :all]
            [f2.semantics :as semantics]))

(def sample-history
  [{:client "C-1" :output 42 :stamp {:seq 1}}
   {:client "C-2" :output 42 :stamp {:seq 2}}
   {:client "C-1" :output 100 :stamp {:seq 3}}
   {:client "C-3" :output 7 :stamp {:seq 4}}])

(deftest suggests-links-when-output-shared
  (let [links (semantics/suggest-links sample-history)]
    (is (= 1 (count links)))
    (is (= :shared-result (:type (first links))))))

(deftest instantiate-pattern-fills-slots
  (let [pattern {:pattern/id "demo" :pattern/name "Demo" :pattern/slots [:slot/a :slot/b]}
        inst (semantics/instantiate-pattern sample-history pattern)]
    (is (= 2 (count (:slots (first inst)))))))

(deftest consistency-checks-volume
  (let [history (map (fn [i] {:client "C-9" :output i}) (range 25))
        issues (semantics/check-consistency history)]
    (is (= 1 (count issues)))
    (is (= :high-volume (:issue (first issues))))))
