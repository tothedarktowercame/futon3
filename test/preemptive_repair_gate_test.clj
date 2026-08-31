(ns preemptive-repair-gate-test
  (:require [checks.preemptive-repair-suite :as gate]
            [clojure.test :refer [deftest is]]))

(deftest build-gate-consumes-preemptive-repair-lints
  (let [positive (gate/gate-result)
        negative (gate/gate-result true)]
    (is (:pass? positive))
    (is (nat-int? (:absence-count positive))
        "known C81 absence debt is emitted but does not mask extinct classes")
    (is (false? (:pass? negative))
        "an injected lint finding must fail the build-level consumer")))
