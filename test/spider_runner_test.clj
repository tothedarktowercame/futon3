#!/usr/bin/env bb
(ns spider-runner-test
  (:require [checks.spider-runner :as runner]
            [clojure.test :refer [deftest is run-tests]]))

(deftest listing-only-rung-one-is-not-a-warrant
  (let [evidence [{:id "e-listing" :via :tag :query "exact" :excerpt "source and target"}]
        listing [{:id "e-listing" :via :tag :listing true
                  :query "exact" :excerpt "source and target in ranking"}]
        stated [{:id "e-listing" :via :tag :listing false
                 :query "exact" :excerpt "source and target in statement"}]]
    (is (false? (runner/rung-one-warrant? evidence listing)))
    (is (true? (runner/rung-one-warrant? evidence stated)))
    (is (true? (runner/context-retrieval-listing?
                {:evidence/body {:event "context-retrieval"}})))
    (is (true? (runner/context-retrieval-listing?
                {:evidence/body "{\"event\" \"context-retrieval\", \"results\" []}"})))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
