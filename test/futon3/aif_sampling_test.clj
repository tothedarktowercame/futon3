(ns futon3.aif-sampling-test
  "AIF-LM-4: Trace/QA harness for AIF policy sampling.

   Tests:
   - Tau sensitivity: low tau leads to greedy selection, high tau yields diversity
   - Distribution shape: softmax probabilities are correctly computed
   - Seed reproducibility: seeded runs are reproducible"
  (:require [clojure.test :refer [deftest is testing]]
            [futon2.aif.adapters.fulab :as fulab]
            [futon2.aif.engine :as aif-engine]))

;; Test fixtures

(def test-candidates ["pattern/a" "pattern/b" "pattern/c"])

(def test-scores
  {"pattern/a" 0.1   ;; Best (lowest G)
   "pattern/b" 0.3
   "pattern/c" 0.8}) ;; Worst

(defn make-context [tau-config & {:keys [session-id turn chosen]
                                   :or {session-id "test-session"
                                        turn 1}}]
  {:decision/id (str session-id ":turn-" turn)
   :session/id session-id
   :candidates test-candidates
   :candidate-scores test-scores
   :uncertainty (/ 1.0 (:tau/scale tau-config 1.0))
   :anchors []
   :forecast nil
   :chosen chosen})

;; Tau sensitivity tests

(deftest low-tau-is-greedy
  (testing "With very low tau, selection is nearly deterministic (greedy)"
    (let [config {:tau/scale 0.1  ;; Very low tau
                  :tau/min 0.05
                  :tau/max 2.0
                  :tau/min-sample 0.01}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          ;; Run multiple times with different seeds
          results (for [seed (range 100)]
                    (let [ctx (make-context config :turn seed)
                          result (aif-engine/select-pattern engine ctx)]
                      (:chosen result)))
          ;; Count how often the best candidate is chosen
          best-count (count (filter #(= "pattern/a" %) results))]
      ;; With low tau, the best pattern should be chosen almost always
      (is (> best-count 90)
          (str "Expected >90% best pattern selection with low tau, got "
               best-count "%")))))

(deftest high-tau-yields-diversity
  (testing "With high tau, selection has diversity (exploration)"
    (let [config {:tau/scale 2.0  ;; High tau
                  :tau/min 0.1
                  :tau/max 3.0
                  :tau/min-sample 0.01}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          results (for [seed (range 100)]
                    (let [ctx (make-context config :turn seed)
                          result (aif-engine/select-pattern engine ctx)]
                      (:chosen result)))
          freq (frequencies results)
          distinct-choices (count (keys freq))]
      ;; With high tau, we should see multiple different patterns chosen
      (is (>= distinct-choices 2)
          (str "Expected diversity with high tau, got frequencies: " freq))
      ;; No single pattern should dominate completely
      (is (< (apply max (vals freq)) 85)
          (str "Expected no pattern to dominate with high tau, got: " freq)))))

;; Distribution shape tests

(deftest softmax-probabilities-sum-to-one
  (testing "Softmax probabilities sum to 1.0"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          result (aif-engine/select-pattern engine (make-context config))
          probs (get-in result [:aif :probs])]
      (is (some? probs) "Probabilities should be present")
      (is (= 3 (count probs)) "Should have probability for each candidate")
      (let [sum (reduce + (vals probs))]
        (is (< (Math/abs (- sum 1.0)) 0.001)
            (str "Probabilities should sum to 1.0, got " sum))))))

(deftest softmax-probabilities-order
  (testing "Lower G should have higher probability"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          result (aif-engine/select-pattern engine (make-context config))
          probs (get-in result [:aif :probs])
          prob-a (get probs "pattern/a")
          prob-b (get probs "pattern/b")
          prob-c (get probs "pattern/c")]
      (is (> prob-a prob-b)
          (str "pattern/a (G=0.1) should have higher prob than pattern/b (G=0.3), got "
               prob-a " vs " prob-b))
      (is (> prob-b prob-c)
          (str "pattern/b (G=0.3) should have higher prob than pattern/c (G=0.8), got "
               prob-b " vs " prob-c)))))

;; Seed reproducibility tests

(deftest seeded-runs-are-reproducible
  (testing "Same seed produces same result"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0
                  :tau/min-sample 0.01}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          ctx (make-context config :session-id "fixed-session" :turn 42)
          result1 (aif-engine/select-pattern engine ctx)
          result2 (aif-engine/select-pattern engine ctx)]
      (is (= (:chosen result1) (:chosen result2))
          "Same context should produce same choice")
      (is (= (get-in result1 [:aif :seed]) (get-in result2 [:aif :seed]))
          "Same context should produce same seed"))))

(deftest different-turns-produce-different-seeds
  (testing "Different turns produce different seeds"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          ctx1 (make-context config :session-id "session" :turn 1)
          ctx2 (make-context config :session-id "session" :turn 2)
          result1 (aif-engine/select-pattern engine ctx1)
          result2 (aif-engine/select-pattern engine ctx2)]
      (is (not= (get-in result1 [:aif :seed]) (get-in result2 [:aif :seed]))
          "Different turns should produce different seeds"))))

(deftest different-sessions-produce-different-seeds
  (testing "Different sessions produce different seeds"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          ctx1 (make-context config :session-id "session-a" :turn 1)
          ctx2 (make-context config :session-id "session-b" :turn 1)
          result1 (aif-engine/select-pattern engine ctx1)
          result2 (aif-engine/select-pattern engine ctx2)]
      (is (not= (get-in result1 [:aif :seed]) (get-in result2 [:aif :seed]))
          "Different sessions should produce different seeds"))))

;; Abstain policy tests

(deftest abstain-when-tau-below-threshold
  (testing "Abstain flag is set when tau is below min-sample"
    (let [config {:tau/scale 0.3  ;; Will produce tau below min-sample
                  :tau/min 0.1
                  :tau/max 2.0
                  :tau/min-sample 0.55}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          result (aif-engine/select-pattern engine (make-context config))
          aif (:aif result)]
      (is (< (:tau aif) 0.55) "Tau should be below threshold")
      (is (:abstain? aif) "Abstain flag should be true when tau < min-sample"))))

(deftest no-abstain-when-tau-above-threshold
  (testing "Abstain flag is false when tau is above min-sample"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0
                  :tau/min-sample 0.55}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          result (aif-engine/select-pattern engine (make-context config))
          aif (:aif result)]
      (is (>= (:tau aif) 0.55) "Tau should be at or above threshold")
      (is (not (:abstain? aif)) "Abstain flag should be false when tau >= min-sample"))))

;; Explicit choice override tests

(deftest explicit-choice-overrides-sampling
  (testing "When chosen is provided, it overrides sampling"
    (let [config {:tau/scale 1.0
                  :tau/min 0.1
                  :tau/max 2.0}
          adapter (fulab/new-adapter config)
          engine (aif-engine/new-engine adapter)
          ctx (make-context config :chosen "pattern/c")  ;; Explicitly choose worst
          result (aif-engine/select-pattern engine ctx)]
      (is (= "pattern/c" (:chosen result))
          "Explicit choice should be used")
      (is (not (get-in result [:aif :sampled?]))
          "sampled? should be false when choice is explicit"))))
