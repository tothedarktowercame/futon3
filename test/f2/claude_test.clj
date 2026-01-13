(ns f2.claude-test
  (:require [clojure.test :refer :all]
            [f2.claude :as claude]))

(defn- reset-sessions! []
  (reset! @#'f2.claude/!sessions {}))

(use-fixtures :each
  (fn [f]
    (reset-sessions!)
    (f)))

(deftest start-session-emits-clock-in
  (with-redefs [f2.claude/start-process! (fn [_] (Object.))
                f2.claude/run-session-loop! (fn [_] nil)]
    (let [{:keys [ok session-id]}
          (claude/start-session! {:prompt "hello"
                                  :pattern-id "pattern/1"
                                  :intent "test"})
          events (-> (claude/get-events session-id) :events)
          types (map :event/type events)
          clock-in (first (filter #(= :clock-in/start (:event/type %)) events))]
      (is ok)
      (is (some #{:session/started} types))
      (is (some #{:clock-in/start} types))
      (is (= "pattern/1" (:clock-in/pattern-id clock-in))))))

(deftest pattern-artifact-doc-tracking
  (with-redefs [f2.claude/start-process! (fn [_] (Object.))
                f2.claude/run-session-loop! (fn [_] nil)]
    (let [{:keys [session-id]} (claude/start-session! {:prompt "hi"})]
      (is (:ok (claude/record-pattern-use! session-id "pattern/x"
                                           {:reason "dep"
                                            :channels [:code :doc]})))
      (is (:ok (claude/record-artifact! session-id "docs/demo.md" {:action :created})))
      (is (:ok (claude/record-doc! session-id {:path "docs/demo.md"
                                               :type :howto
                                               :summary "demo"})))
      (let [session (claude/get-session session-id)
            events (-> (claude/get-events session-id) :events)
            pattern-used (first (filter #(= :pattern/used (:event/type %)) events))
            types (map :event/type events)]
        (is (= 1 (count (:patterns-used session))))
        (is (= 1 (count (:artifacts session))))
        (is (= 1 (count (:docs-written session))))
        (is (= 1 (get-in session [:stats :patterns-used])))
        (is (= 1 (get-in session [:stats :docs-written])))
        (is (= 1 (get-in session [:stats :artifacts])))
        (is (some #{:pattern/used} types))
        (is (= [:code :doc] (:pattern/channels pattern-used)))
        (is (some #{:doc/written} types))))))

(deftest clock-out-complete-event
  (with-redefs [f2.claude/start-process! (fn [_] (Object.))
                f2.claude/run-session-loop!
                (fn [session]
                  (let [emit! @#'f2.claude/emit-event!
                        clock-out @#'f2.claude/clock-out-event
                        pattern-id (get-in session [:opts :pattern-id])]
                    (emit! session (clock-out (:id session)
                                              {:pattern-id pattern-id
                                               :patterns-trail @(:patterns-used session)
                                               :status :success
                                               :artifacts @(:artifacts session)
                                               :docs @(:docs-written session)
                                               :stats @(:stats session)
                                               :society-paper @(:society-paper session)}))))]
    (let [{:keys [session-id]}
          (claude/start-session! {:prompt "hi"
                                  :pattern-id "pattern/clock-out"
                                  :intent "test"})]
      (let [events (-> (claude/get-events session-id) :events)
            types (map :event/type events)]
        (is (some #{:clock-out/complete} types))))))
