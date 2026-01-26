(ns futon3.similarity-test
  (:require [clojure.test :refer :all]
            [futon3.pattern-hints :as hints]))

(deftest glove-neighbors-deterministic
  (let [expected ["f3/p6"
                  "fulab/proof-commit"
                  "fulab/clock-in"
                  "fulab/changelog-trail"
                  "fulab/pattern-propose"]
        actual (mapv :id (hints/nearest-patterns "musn/plan-before-tool"
                                                 {:limit 5 :method :glove}))]
    (is (= expected actual))))

(deftest sigil-neighbors-deterministic
  (let [expected ["musn/tool-name-hygiene"
                  "musn/plan-before-tool"
                  "exotic/immutable-vision-mutable-plan"
                  "agent/state-is-hypothesis"
                  "meta/scaffolding-emergent-clarity"]
        actual (mapv :id (hints/nearest-patterns "musn/plan-before-tool"
                                                 {:limit 5 :method :sigil}))]
    (is (= expected actual))))
