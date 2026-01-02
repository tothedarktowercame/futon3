(ns f2.codex-test
  (:require [clojure.test :refer :all]
            [f2.codex :as codex]))

(defn- reset-sessions! []
  (reset! @#'f2.codex/!sessions {}))

(use-fixtures :each
  (fn [f]
    (reset-sessions!)
    (f)))

(deftest start-session-emits-clock-in
  (with-redefs [f2.codex/start-process! (fn [_ _] (Object.))
                f2.codex/run-session-loop! (fn [_] nil)]
    (let [{:keys [ok session-id]}
          (codex/start-session! {:prompt "hello"
                                 :pattern-id "pattern/1"
                                 :intent "test"})
          events (-> (codex/get-events session-id) :events)
          types (map :event/type events)
          clock-in (first (filter #(= :clock-in/start (:event/type %)) events))]
      (is ok)
      (is (some #{:session/started} types))
      (is (some #{:clock-in/start} types))
      (is (= "pattern/1" (:clock-in/pattern-id clock-in))))))

(deftest pattern-artifact-doc-tracking
  (with-redefs [f2.codex/start-process! (fn [_ _] (Object.))
                f2.codex/run-session-loop! (fn [_] nil)]
    (let [{:keys [session-id]} (codex/start-session! {:prompt "hi"})]
      (is (:ok (codex/record-pattern-use! session-id "pattern/x" {:reason "dep"})))
      (is (:ok (codex/record-artifact! session-id "docs/demo.md" {:action :created})))
      (is (:ok (codex/record-doc! session-id {:path "docs/demo.md"
                                              :type :howto
                                              :summary "demo"})))
      (let [session (codex/get-session session-id)
            events (-> (codex/get-events session-id) :events)
            types (map :event/type events)]
        (is (= 1 (count (:patterns-used session))))
        (is (= 1 (count (:artifacts session))))
        (is (= 1 (count (:docs-written session))))
        (is (= 1 (get-in session [:stats :patterns-used])))
        (is (= 1 (get-in session [:stats :docs-written])))
        (is (= 1 (get-in session [:stats :artifacts])))
        (is (some #{:pattern/used} types))
        (is (some #{:doc/written} types))))))
