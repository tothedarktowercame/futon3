(ns hx-logic-test
  (:require [clojure.test :refer :all]
            [futon3.hx.logic :as logic]))

(def base-state
  {:artifacts {"A" {:artifact/id "A"}
               "B" {:artifact/id "B"}}
   :anchors {"A" [{:anchor/id "a1"}]
             "B" [{:anchor/id "b1"}]}
   :links {"L1" {:link/id "L1"
                 :link/status :accepted
                 :link/from {:artifact/id "A" :anchor/id "a1"}
                 :link/to {:artifact/id "B" :anchor/id "b1"}
                 :link/type :uses}}})

(defn- obligations [result]
  (get-in result [:logic :obligations]))

(defn- issues [result]
  (:errors result))

(deftest link-suggest-missing-anchor
  (let [step {:hx.step/kind :hx/link-suggest
              :hx.step/payload {:link {:link/from {:artifact/id "A" :anchor/id "a2"}
                                       :link/to {:artifact/id "B" :anchor/id "b1"}
                                       :link/type :uses}}}
        result (logic/check-step step {:state base-state})]
    (is (false? (:ok? result)))
    (is (some #(= :needs-anchor (:obligation %)) (obligations result)))))

(deftest link-suggest-unknown-type
  (let [step {:hx.step/kind :hx/link-suggest
              :hx.step/payload {:link {:link/from {:artifact/id "A" :anchor/id "a1"}
                                       :link/to {:artifact/id "B" :anchor/id "b1"}
                                       :link/type :depends-on}}}
        result (logic/check-step step {:state base-state})]
    (is (false? (:ok? result)))
    (is (some #(= :unknown-link-type (:obligation %)) (obligations result)))))

(deftest link-accept-invalid-status
  (let [step {:hx.step/kind :hx/link-accept
              :hx.step/payload {:link/id "L1"
                                :decided-by :joe}}
        result (logic/check-step step {:state base-state})]
    (is (false? (:ok? result)))
    (is (some #(= :invalid-link-status (:obligation %)) (obligations result)))))

(deftest anchors-upsert-duplicate-id
  (let [step {:hx.step/kind :hx/anchors-upsert
              :hx.step/payload {:artifact/id "A"
                                :anchors [{:anchor/id "dup" :anchor/selector {:kind :line :line 1}}
                                          {:anchor/id "dup" :anchor/selector {:kind :line :line 2}}]}}
        result (logic/check-step step {:state base-state})]
    (is (false? (:ok? result)))
    (is (some #(= :duplicate-anchor-id (:issue %)) (issues result)))))

(deftest artifact-register-docs-required
  (let [step {:hx.step/kind :hx/artifact-register
              :hx.step/payload {:artifact {:artifact/id "C"
                                           :artifact/interactive true}}}
        result (logic/check-step step {:state base-state
                                       :policies {:require-docs? true}})]
    (is (false? (:ok? result)))
    (is (some #(= :needs-documentation (:obligation %)) (obligations result)))))

(deftest artifact-register-patterns-required
  (let [step {:hx.step/kind :hx/artifact-register
              :hx.step/payload {:artifact {:artifact/id "C"}}}
        result (logic/check-step step {:state base-state
                                       :policies {:require-patterns? true}})]
    (is (false? (:ok? result)))
    (is (some #(= :needs-pattern-link (:obligation %)) (obligations result)))))
