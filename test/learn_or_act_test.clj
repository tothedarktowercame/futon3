(ns learn-or-act-test
  (:require [clojure.test :refer :all]
            [futon3.gap-store :as gap-store]
            [futon3.learn-or-act :as loa]
            [futon3.pattern-hints :as hints])
  (:import (java.io File)))

(defn- tmp-gap-file []
  (doto (File/createTempFile "gap-store" ".edn")
    (spit "[]\n")))

(use-fixtures :each
  (fn [f]
    (let [tmp (tmp-gap-file)]
      (gap-store/set-path! (.getAbsolutePath tmp))
      (try
        (f)
        (finally
          (when (.exists tmp)
            (.delete tmp)))))))

(defn- stub-search [patterns]
  {:patterns patterns
   :fruits []
   :paramitas []})

(deftest representation-failure-prompts-question
  (let [result (loa/decide {})]
    (is (= :failure/represent (:failure/mode result)))
    (is (some? (:triage/question result)))
    (is (= 0 (:search/attempts result)))))

(deftest high-score-yields-success
  (binding [loa/*pattern-search*
            (fn [_]
              (stub-search [{:id "p1"
                             :title "Demo"
                             :summary "Summary"
                             :sigils [{:emoji "🏡" :hanzi "工"}]
                             :score 0.05}]))]
    (let [result (loa/decide {:sigils [{:emoji "🏡" :hanzi "工"}]})]
      (is (nil? (:failure/mode result)))
      (is (= 1 (:search/attempts result)))
      (is (nil? (:stub/current result))))))

(deftest low-score-creates-gap-and-stub
  (binding [loa/*pattern-search*
            (fn [_]
              (stub-search []))]
    (let [result (loa/decide {:sigils [{:emoji "🍵" :hanzi "心"}]})]
      (is (= :failure/gap (:failure/mode result)))
      (is (some? (:stub/current result)))
      (is (= 1 (:search/attempts result)))
      (is (= 1 (count (gap-store/load-gaps)))))))

(deftest ambiguous-results-trigger-question
  (binding [loa/*pattern-search*
            (fn [_]
              (stub-search [{:id "p1" :title "Option A" :summary "" :sigils [] :score 0.35}
                            {:id "p2" :title "Option B" :summary "" :sigils [] :score 0.37}]))]
    (let [result (loa/decide {:sigils [{:emoji "👫" :hanzi "人"}]})]
      (is (= :failure/retrieve (:failure/mode result)))
      (is (some? (:triage/question result)))
      (is (> (:search/attempts result) 1)))))

(deftest intent-drives-fruit-and-paramita-cues
  (binding [loa/*pattern-search*
            (fn [_]
              {:patterns [{:id :demo/pattern
                           :title "truthful logging plan"
                           :summary "Offer a truthful logging plan"
                           :sigils [{:emoji "🍒" :hanzi "白"}]
                           :score 0.2}]
               :fruits []
               :paramitas []})]
    (let [result (loa/decide {:sigils [{:emoji "🍒" :hanzi "白"}]
                              :intent "Need truthful logging plan now"
                              :fruit-limit 1
                              :paramita-limit 1})]
      (is (= [:doable]
             (map :fruit/id (:fruits result))))
      (is (= [:truth]
             (map :paramita/id (:paramitas result))))
      (is (= :demo/pattern (-> result :cue/intent :matches first :pattern/id)))
      (is (seq (:tokens (:cue/intent result))))))

(deftest intent-seeding-adds-query-sigils
  (let [captured (atom nil)
        pattern [{:id :or/license-laddering
                  :title "License Laddering"
                  :summary "Progressive license story"
                  :sigils [{:emoji "📥" :hanzi "文"}]}]]
    (with-redefs [hints/all-patterns (fn [] pattern)]
      (binding [loa/*pattern-search*
                (fn [req]
                  (reset! captured req)
                  (stub-search []))]
        (loa/decide {:intent "Need a license story"})))
    (is (seq (:sigils @captured)))
    (is (= "📥" (:emoji (first (:sigils @captured)))))
    (is (= "文" (:hanzi (first (:sigils @captured)))))))
