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

(deftest resume-session-emits-start
  (with-redefs [f2.codex/start-process! (fn [_ _] (Object.))
                f2.codex/run-session-loop! (fn [_] nil)]
    (let [{:keys [ok session-id]}
          (codex/resume-session! {:codex-cli-session-id "codex-123"
                                  :prompt "follow-up"})]
      (is ok)
      (let [events (-> (codex/get-events session-id) :events)
            types (map :event/type events)]
        (is (some #{:session/started} types))))))

(deftest resume-session-rejects-missing-id
  (let [{:keys [ok error]} (codex/resume-session! {:codex-cli-session-id ""})]
    (is (not ok))
    (is (= "missing-codex-cli-session-id" error))))

(deftest clock-out-complete-event
  (with-redefs [f2.codex/start-process! (fn [_ _] (Object.))
                f2.codex/run-session-loop!
                (fn [session]
                  (let [emit! @#'f2.codex/emit-event!
                        clock-out @#'f2.codex/clock-out-event
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
          (codex/start-session! {:prompt "hi"
                                 :pattern-id "pattern/clock-out"
                                 :intent "test"})]
      (let [events (-> (codex/get-events session-id) :events)
            types (map :event/type events)]
        (is (some #{:clock-out/complete} types))))))

(deftest tool-approval-and-denial
  (with-redefs [f2.codex/start-process! (fn [_ _] (Object.))
                f2.codex/run-session-loop! (fn [_] nil)
                f2.codex/write-to-process! (fn [_ _] nil)]
    (let [{:keys [session-id]} (codex/start-session! {:prompt "hi"})
          call-id "tool-1"]
      (swap! @#'f2.codex/!sessions assoc-in [session-id :pending-approvals call-id]
             {:tool/name "demo"})
      (is (:ok (codex/approve-tool-call! session-id call-id)))
      (swap! @#'f2.codex/!sessions assoc-in [session-id :pending-approvals call-id]
             {:tool/name "demo"})
      (is (:ok (codex/deny-tool-call! session-id call-id "nope")))
      (let [events (-> (codex/get-events session-id) :events)
            types (map :event/type events)]
        (is (some #{:tool/approved} types))
        (is (some #{:tool/denied} types))))))

(deftest event-log-offset
  (with-redefs [f2.codex/start-process! (fn [_ _] (Object.))
                f2.codex/run-session-loop! (fn [_] nil)]
    (let [{:keys [session-id]} (codex/start-session! {:prompt "hi"})]
      (codex/record-pattern-use! session-id "pattern/x" {:reason "dep"})
      (let [{:keys [events]} (codex/get-events session-id {:offset 1})]
        (is (pos? (count events)))))))
