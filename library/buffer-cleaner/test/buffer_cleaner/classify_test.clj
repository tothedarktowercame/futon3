(ns buffer-cleaner.classify-test
  (:require [clojure.test :refer [deftest is testing]]
            [buffer-cleaner.classify :as c]))

(def categories
  {:stream  {:decisiveness :decisive :prior-needed-later 0.0 :revisit-cost 1.0 :source :constructed}
   :http    {:decisiveness :decisive :prior-needed-later 0.0 :revisit-cost 1.0 :source :constructed}
   :temp    {:decisiveness :decisive :prior-needed-later 0.05 :revisit-cost 1.0 :source :constructed}
   :dired   {:decisiveness :uncertain :prior-needed-later 0.25 :revisit-cost 0.5 :source :constructed}
   :file-stale {:decisiveness :uncertain :revisit-cost 5.0 :source :constructed
                :prior-needed-later {:by-age-seconds {21600 0.40 86400 0.05} :default 0.05}}
   :unknown {:decisiveness :unknown :source :constructed}
   :fuel {:per-scan 0.01 :per-kill 0.1}})

(def aggressive {:wiring/id :aggressive
                 :eligible-kinds #{:stream :http :temp :dired :render :invoke :file-stale}
                 :file-stale-age-seconds 21600})
(def conservative {:wiring/id :conservative
                   :eligible-kinds #{:stream :http :file-stale}
                   :file-stale-age-seconds 86400})

(def packet
  {:buffers
   [{:name "*stream:1*"  :kind "stream" :file "false" :modified "false"
     :has-process "false" :visible "false" :display-age-seconds 9999}
    {:name "*HTTP:1*"  :kind "http" :file "false" :modified "false"
     :has-process "false" :visible "false" :display-age-seconds 9999}
    {:name "notes.org"  :kind "file" :file "true" :modified "false"
     :has-process "false" :visible "false" :display-age-seconds 108000}
    {:name "fresh.org"  :kind "file" :file "true" :modified "false"
     :has-process "false" :visible "false" :display-age-seconds 7200}
    {:name "*dired-nav*"  :kind "dired" :file "false" :modified "false"
     :has-process "false" :visible "false" :display-age-seconds 9999}
    {:name "*visible-dired*"  :kind "dired" :file "false" :modified "false"
     :has-process "false" :visible true :display-age-seconds 9999}
    {:name "*mystery*" :kind "unknown" :file "false" :modified "false"
     :has-process "false" :visible "false" :display-age-seconds 9999}]})

(deftest decisiveness-respected
  (let [r (c/classify-packet packet aggressive categories)
        by (fn [n] (some #(when (= n (:name %)) %) (:classified r)))]
    (testing "decisive stream kills certain-safe"
      (is (= :kill (:action (by "*stream:1*"))))
      (is (= :decisive (:decisiveness (by "*stream:1*"))))
      (is (zero? (:expected-recovery (by "*stream:1*")))))
    (testing "hard preservation wins over eligibility"
      (is (= :keep (:action (by "*visible-dired*"))))
      (is (= :visible (:preservation-reason (by "*visible-dired*")))))
    (testing "unknown channel never killed"
      (is (= :keep (:action (by "*mystery*")))))
    (testing "age->prior declared, not threshold-hidden"
      (is (= 0.05 (:prior-needed-later (by "notes.org")))))))

(deftest wirings-differ-by-args-only
  (let [a (c/classify-packet packet aggressive categories)
        b (c/classify-packet packet conservative categories)]
    (testing "aggressive kills more (its dired eligibility)"
      (is (> (:kills-proposed (:meters a)) (:kills-proposed (:meters b)))))
    (testing "conservative's 24h threshold keeps the 30h file's kill decision same"
      (is (= 0.05 (:prior-needed-later
                   (some #(when (= "notes.org" (:name %)) %) (:classified b))))))
    (testing "both wirings: same code, args only"
      (is (distinct? (:wiring a) (:wiring b)))
      (is (= :dry-run (:dry-run (:receipts a))))
      (is (= :refused-until-gated (:execute (:receipts a)))))))

(deftest execute-refused-typed
  (testing "live kill refuses with a typed refusal naming the gate"
    (is (thrown-with-msg? Exception #"gate does not exist"
                          (c/execute! [])))))

(deftest fuel-meters
  (let [r (c/classify-packet packet aggressive categories)
        m (:meters r)]
    (is (= 7 (:scanned m)))
    (is (= (+ (* 0.01 7) (* 0.1 (:kills-proposed m))) (:fuel-charged m)))))
