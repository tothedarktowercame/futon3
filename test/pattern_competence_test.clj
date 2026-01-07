(ns pattern-competence-test
  (:require [clojure.test :refer :all]
            [futon3.hx.logic :as logic]))

(def base-event
  {:event/type :code/edit
   :file "src/f2/claude.clj"
   :fn "start-session!"
   :action :modified
   :description "Added pattern-id/intent params"
   :at #inst "2026-01-01T15:14:00Z"})

(def base-session
  {:session/id "test-session-1"
   :events [base-event]})

(def anchor
  {:anchor/type :code/edit
   :anchor/ref {:event/type :code/edit
                :file "src/f2/claude.clj"
                :fn "start-session!"}})

(def pur
  {:pur/id "pur-1"
   :session/id "test-session-1"
   :pattern/id "p1"
   :instance/id "pur-1-a"
   :fields {:context "context"
            :if "if"
            :however "however"
            :then "then"
            :because "because"
            :next-steps "next"}
   :anchors [anchor]})

(def psr
  {:psr/id "psr-1"
   :session/id "test-session-1"
   :decision/id "decision-1"
   :candidates ["p1" "p2"]
   :chosen "p1"
   :context/anchors [anchor]
   :forecast {:benefits [{:tag :benefit/test
                          :locus anchor
                          :note "benefit"}]
              :risks []
              :success []
              :failure []}
   :rejections {"p2" {:codes [:reject/test] :note "not fit"}}
   :horizon :short})

(deftest pur-pass-minimal
  (let [result (logic/check-step {:hx.step/kind :hx/pattern-use-claimed
                                  :hx.step/payload {:pur pur}}
                                 {:session base-session
                                  :pattern-ids #{"p1"}})]
    (is (:ok? result))))

(deftest psr-pass-minimal
  (let [result (logic/check-step {:hx.step/kind :hx/pattern-selection-claimed
                                  :hx.step/payload {:psr psr}}
                                 {:session base-session
                                  :pattern-ids #{"p1" "p2"}})]
    (is (:ok? result))))

(deftest pur-failing-anchor
  (let [bad-pur (assoc pur :anchors [{:anchor/type :code/edit
                                      :anchor/ref {:event/type :code/edit
                                                   :file "missing.clj"}}])
        step {:hx.step/kind :hx/pattern-use-claimed
              :hx.step/payload {:pur bad-pur}}
        failures (logic/explain-failure step {:session base-session
                                              :pattern-ids #{"p1"}})]
    (is (some #(= :pur/anchors (:validator %)) failures))))

(deftest psr-failing-candidate
  (let [bad-psr (assoc psr :chosen "p3")
        step {:hx.step/kind :hx/pattern-selection-claimed
              :hx.step/payload {:psr bad-psr}}
        failures (logic/explain-failure step {:session base-session
                                              :pattern-ids #{"p1" "p2"}})]
    (is (some #(= :psr/candidates (:validator %)) failures))))
