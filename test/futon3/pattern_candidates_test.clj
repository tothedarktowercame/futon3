(ns futon3.pattern-candidates-test
  "Tests for the P-2 hybrid candidate builder (mission
  M-pattern-retrieval-calibration acceptance line 1028): a deduplicated
  pool of at most six structured packets, byte-identical for identical
  query and corpus inputs.  The determinism tests probe the claimed
  property itself (serialized bytes), not a proxy such as candidate
  count, and additionally hold under a permuted corpus input order."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3.pattern-candidates :as pc]))

(def packet-a
  {:pattern/id "agent/intent-handshake-is-binding"
   :pattern/title "Intent handshake is binding"
   :pattern/keywords ["handshake" "commitment"]
   :pattern/clauses [{:name-key "because" :text "a handshake aligns expectations before work begins"}]})

(def packet-b
  {:pattern/id "futon-theory/task-as-arrow"
   :pattern/title "Task as arrow"
   :pattern/keywords ["task" "decomposition"]
   :pattern/clauses [{:name-key "because" :text "task decomposition arrows carry the goal forward"}]})

(def packet-c
  {:pattern/id "memory/licence-to-fail"
   :pattern/title "Licence to fail"
   :pattern/keywords ["experiment" "failure"]
   :pattern/clauses [{:name-key "because" :text "safe failure permits exploration"}]})

(def corpus [packet-a packet-b packet-c])

(defn- query
  [& {:as opts}]
  (pc/candidate-set (merge {:query "task handshake commitment arrow decomposition"
                            :packets corpus}
                           opts)))

(deftest dedup-and-sources-test
  (testing "a pattern surfaced by both rankers appears once with both sources"
    (let [embeddings {"agent/intent-handshake-is-binding" [1.0 0.0]
                      "futon-theory/task-as-arrow" [0.0 1.0]
                      "memory/licence-to-fail" [1.0 0.0]}
          cs (query :query-vector [1.0 0.0] :embeddings embeddings)
          ids (map :pattern/id (:candidates cs))]
      (is (= (distinct ids) ids) "no duplicate pattern ids")
      (is (= 1 (count (filter #(= "agent/intent-handshake-is-binding" %) ids))))
      (is (= #{:classical :embedding}
             (:retrieval/sources (some #(when (= "agent/intent-handshake-is-binding" (:pattern/id %)) %)
                                       (:candidates cs))))))))

(deftest pool-cap-test
  (testing "the pool never exceeds six packets"
    (let [many (vec (map (fn [i] {:pattern/id (str "p/p" i)
                                  :pattern/title (str "pattern " i " task")
                                  :pattern/keywords ["task"]
                                  :pattern/clauses []})
                         (range 20)))
          cs (pc/candidate-set {:query "task" :packets many})]
      (is (pos? (count (:candidates cs))))
      (is (<= (count (:candidates cs)) pc/pool-cap)))))

(deftest determinism-test
  (testing "identical query and corpus inputs produce byte-identical output (the claimed property, not a proxy)"
    (let [cs1 (query)
          cs2 (query)]
      (is (= (pc/candidate-set-str cs1) (pc/candidate-set-str cs2))))))

(deftest determinism-under-corpus-permutation-test
  (testing "permuting the corpus input order cannot change the serialized bytes"
    ;; This is the case a proxy check (candidate count) would pass while
    ;; the claimed property (byte-identical) could fail if ordering
    ;; leaked from corpus iteration order.
    (let [s1 (pc/candidate-set-str (query))
          s2 (pc/candidate-set-str (pc/candidate-set {:query "task handshake commitment arrow decomposition"
                                                      :packets (reverse corpus)}))]
      (is (= s1 s2)))))

(deftest embedding-optional-test
  (testing "without a query vector the pool is classical-only but still non-empty and deterministic"
    (let [cs (query)
          ids (map :pattern/id (:candidates cs))]
      (is (pos? (count ids)))
      (is (every? #(= #{:classical} (:retrieval/sources %)) (:candidates cs)))
      (is (= ids (map :pattern/id (:candidates (query))))))))

(deftest tie-break-test
  (testing "equal scores break by pattern id ascending, a total order"
    (let [twin (fn [id] {:pattern/id id
                         :pattern/title "identical title"
                         :pattern/keywords ["shared"]
                         :pattern/clauses []})
          cs (pc/candidate-set {:query "shared" :packets [(twin "p/zzz") (twin "p/aaa")]})
          ids (map :pattern/id (:candidates cs))]
      (is (= 2 (count ids)))
      (is (= ["p/aaa" "p/zzz"] ids) "input order zzz-first must not leak into output"))))
