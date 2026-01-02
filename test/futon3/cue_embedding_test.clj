(ns futon3.cue-embedding-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer :all]
            [futon3.cue-embedding :as cue]))

(deftest embed-cues-normalizes-identifiers
  (let [payload {:fruits [{:fruit/id "doable" :score 0.5}
                          {:fruit/id :breakdown :score 0.25}]
                 :paramitas [{:paramita/id "truth" :score 0.1}
                             {:paramita/id :equanimity :score 0.2}]}
        result (cue/embed-cues payload)]
    (is (= 0.5 (get-in result [:fruits :scores :doable])))
    (is (= 0.25 (get-in result [:fruits :scores :breakdown])))
    (is (= 0.1 (get-in result [:paramitas :scores :truth])))
    (is (= 0.2 (get-in result [:paramitas :scores :equanimity])))))

(deftest annotate-entry-injects-embedding
  (let [entry {:timestamp "t" :tatami {:fruits [{:fruit/id :doable :score 0.5}]}}
        annotated (cue/annotate-entry entry)]
    (is (contains? annotated :cue/embedding))
    (is (= 0.5 (get-in annotated [:cue/embedding :fruits :scores :doable])))))

(deftest intent-pattern-cues-derive-fruits-and-paramitas
  (let [patterns [{:id :mock/pattern
                   :title "Truthful logging"
                   :summary "Deliver a truthful logging rundown"
                   :sigils [{:emoji "🍒" :hanzi "白"}]
                   :score 0.2}
                  {:id :mock/other
                   :title "Unrelated"
                   :summary "Irrelevant work"
                   :sigils [{:emoji "🍊" :hanzi "予"}]
                   :score 0.9}]
        cues (cue/intent-pattern-cues {:intent "Need truthful logging plan"
                                       :patterns patterns
                                       :fruit-limit 1
                                       :paramita-limit 1})]
    (is cues)
    (is (= [:doable]
           (map :fruit/id (:fruits cues))))
    (is (= [:truth]
           (map :paramita/id (:paramitas cues))))
    (is (= :mock/pattern (-> cues :matches first :pattern/id)))))

(deftest annotate-entry-adds-intent-cues
  (let [entry {:intent "Need truthful logging plan"
               :tatami {:patterns [{:id :demo/pattern
                                    :title "Truthful logging"
                                    :summary "Offer truthful logging guidance"
                                    :sigils [{:emoji "🍒" :hanzi "白"}]}]}}
        annotated (cue/annotate-entry entry)]
    (is (contains? annotated :cue/intent))
    (is (= :demo/pattern (-> annotated :cue/intent :matches first :pattern/id)))
    (is (some #{:doable} (map :fruit/id (get-in annotated [:cue/intent :fruits]))))))

(deftest keyword-intents-are-normalized
  (let [entry {:intent :demo-focus
               :tatami {:patterns [{:id :demo/pattern
                                    :title "Demo focus"
                                    :summary "Focus demo"
                                    :sigils [{:emoji "🍒" :hanzi "白"}]}]}}
        cues (cue/entry-intent-cues entry)]
    (is (seq (:fruits cues)))
    (is (= :demo/pattern (-> cues :matches first :pattern/id)))))

(deftest keyword-pattern-titles-are-safe
  (let [patterns [{:id :demo/pattern
                   :title :null
                   :summary "Demo focus summary"
                   :sigils [{:emoji "🍒" :hanzi "白"}]}]
        cues (cue/intent-pattern-cues {:intent :demo-focus
                                       :patterns patterns})]
    (is cues)
    (is (= :demo/pattern (-> cues :matches first :pattern/id)))))

(def ^:private fruits-data (edn/read-string (slurp "resources/sigils/fruits.edn")))
(def ^:private paramita-data (edn/read-string (slurp "resources/sigils/paramitas.edn")))

(defn- fruit-emoji [id]
  (or (:emoji (some #(when (= (:fruit/id %) id) %) fruits-data))
      (throw (ex-info "missing-fruit" {:fruit/id id}))))

(defn- paramita-hanzi [id]
  (or (:zh (some #(when (= (:paramita/id %) id) %) paramita-data))
      (throw (ex-info "missing-paramita" {:paramita/id id}))))

(def ^:private english-intent-cases
  [{:intent "This is a straightforward, honest plan so let's just do it."
    :fruit :doable :paramita :truth
    :pattern {:id :grid/doable
              :title "Straightforward execution"
              :summary "Deliver a straightforward plan with honest doable steps and truth."
              :sigils [{:emoji (fruit-emoji :doable) :hanzi (paramita-hanzi :truth)}]
              :score 0.2}}
   {:intent "We need to break this project into smaller slices and keep persistent focus."
    :fruit :breakdown :paramita :persistence
    :pattern {:id :grid/breakdown
              :title "Break constraints"
              :summary "Break down tangled work into smaller slices with persistent focus."
              :sigils [{:emoji (fruit-emoji :breakdown) :hanzi (paramita-hanzi :persistence)}]
              :score 0.2}}
   {:intent "I absolutely need leadership buyin and genuine goodwill to fund this."
    :fruit :buy-in :paramita :good-will
    :pattern {:id :grid/buy-in
              :title "Leadership goodwill"
              :summary "Invite leadership buyin and cultivate goodwill for sponsorship."
              :sigils [{:emoji (fruit-emoji :buy-in) :hanzi (paramita-hanzi :good-will)}]
              :score 0.2}}
   {:intent "Let's approach this collaboration with calm equanimity and share the load."
    :fruit :collaboration :paramita :equanimity
    :pattern {:id :grid/collaboration
              :title "Calm collaboration"
              :summary "Guide collaboration with calm equanimity and steady rhythm."
              :sigils [{:emoji (fruit-emoji :collaboration) :hanzi (paramita-hanzi :equanimity)}]
              :score 0.2}}
   {:intent "We are chasing an ambitious arc and must bring determination to the table."
    :fruit :ambition :paramita :determination
    :pattern {:id :grid/ambition
              :title "Ambitious resolve"
              :summary "Aim for ambitious outcomes with determined resolve and strength."
              :sigils [{:emoji (fruit-emoji :ambition) :hanzi (paramita-hanzi :determination)}]
              :score 0.2}}])

(deftest english-sentences-map-to-fruit-space
  (doseq [{:keys [intent fruit paramita pattern]} english-intent-cases]
    (let [result (cue/intent-pattern-cues {:intent intent
                                           :patterns [pattern]
                                           :fruit-limit 2
                                           :paramita-limit 2})
          fruit-ids (set (map :fruit/id (:fruits result)))
          paramita-ids (set (map :paramita/id (:paramitas result)))]
      (is (contains? fruit-ids fruit)
          (format "Intent '%s' should surface fruit %s" intent fruit))
      (is (contains? paramita-ids paramita)
          (format "Intent '%s' should surface paramita %s" intent paramita)))))

(deftest fallback-corpus-supports-natural-language
  (let [entry {:intent "Boundless friendliness toward all beings." :tatami {} }
        cues (cue/entry-intent-cues entry)
        paramitas (set (map :paramita/id (:paramitas cues)))]
    (is (contains? paramitas :good-will))
    (is (contains? paramitas :equanimity))))
