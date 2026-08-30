#!/usr/bin/env bb
(ns spider-runner-test
  (:require [checks.library-graph-lint :as lint]
            [checks.spider-runner :as runner]
            [clojure.test :refer [deftest is run-tests]]))

(deftest cursor-paging-follows-an-empty-terminal-page
  (let [requests (atom [])
        pages [{:entries [{:evidence/id "e-1"}]
                :next-cursor {:at "2026-01-03T00:00:00Z" :id "e-1"}}
               {:entries [{:evidence/id "e-2"}]
                :next-cursor {:at "2026-01-02T00:00:00Z" :id "e-2"}}
               {:entries []}]
        fetch (fn [request]
                (let [n (count @requests)]
                  (swap! requests conj request)
                  (nth pages n)))
        result (lint/page-evidence fetch)]
    (is (= ["e-1" "e-2"] (mapv :evidence/id (:entries result))))
    (is (= 3 (count (:pages result))))
    (is (= [nil {:at "2026-01-03T00:00:00Z" :id "e-1"}
            {:at "2026-01-02T00:00:00Z" :id "e-2"}]
           (mapv :cursor @requests)))))

(deftest listing-only-rung-one-is-not-a-warrant
  (let [evidence [{:id "e-listing" :via :tag :query "exact" :excerpt "source and target"}]
        listing [{:id "e-listing" :via :tag :listing true
                  :self-text false :query "exact" :excerpt "source and target in ranking"}]
        stated [{:id "e-listing" :via :tag :listing false
                 :self-text false :query "exact" :excerpt "source and target in statement"}]
        reflected [{:id "e-listing" :via :tag :listing false :self-text true
                    :query "exact" :excerpt "source and target in spider output"}]]
    (is (false? (runner/rung-one-warrant? evidence listing)))
    (is (false? (runner/rung-one-warrant? evidence reflected)))
    (is (true? (runner/rung-one-warrant? evidence stated)))
    (is (true? (runner/context-retrieval-listing?
                {:evidence/body {:event "context-retrieval"}})))
    (is (true? (runner/context-retrieval-listing?
                {:evidence/body "{\"event\" \"context-retrieval\", \"results\" []}"})))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
