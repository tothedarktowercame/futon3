(ns futon3.fulab.pattern-competence
  "Helpers for Pattern Use/Selection records (PUR/PSR) in lab sessions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [futon3.hx.logic :as logic]))

(def pur-fields
  [:context :if :however :then :because :next-steps])

(def forecast-fields
  [:benefits :risks :success :failure])

(def ^:private aif-term-provenance-keys
  [:term-id :observation-keys :precision-channels :intermediate-values :final-contribution])

(def ^:private dominant-channel-threshold 0.6)

(defn now-inst []
  (java.util.Date.))

(defn read-session-file [path]
  (let [content (slurp path)]
    (edn/read-string {:readers *data-readers*} content)))

(defn write-session-file! [path session]
  (with-open [w (io/writer path)]
    (binding [*print-namespace-maps* false]
      (pprint/pprint session w))))

(defn append-event [session event]
  (update session :events (fnil conj []) event))

(defn build-event [event-type payload]
  {:event/type event-type
   :at (now-inst)
   :payload payload})

(defn outcome-tags-path [repo-root]
  (str (io/file repo-root "resources" "fulab-outcome-tags.edn")))

(defn read-outcome-tags [repo-root]
  (let [path (outcome-tags-path repo-root)]
    (if (.exists (io/file path))
      (set (edn/read-string (slurp path)))
      #{})))

(defn pattern-index-path [repo-root]
  (str (io/file repo-root "resources" "sigils" "patterns-index.tsv")))

(defn read-pattern-ids [repo-root]
  (let [path (pattern-index-path repo-root)]
    (if (.exists (io/file path))
      (with-open [r (io/reader path)]
        (->> (line-seq r)
             (map #(str/split % #"\t"))
             (map first)
             (remove str/blank?)
             set))
      #{})))

(defn anchor [type ref]
  {:anchor/type type
   :anchor/ref ref})

(defn pur-template [session-id]
  {:pur/id (str (java.util.UUID/randomUUID))
   :session/id session-id
   :pattern/id ""
   :instance/id ""
   :certificates [{:certificate/type :git/commit
                   :certificate/ref ""
                   :certificate/repo ""}]
   :fields {:context ""
            :if ""
            :however ""
            :then ""
            :because ""
            :next-steps ""}
   :anchors [(anchor :code/edit {:event/type :code/edit
                                 :file ""
                                 :fn ""})]})

(defn psr-template [session-id decision-id]
  {:psr/id (str (java.util.UUID/randomUUID))
   :session/id session-id
   :decision/id decision-id
   :candidates ["" ""]
   :chosen ""
   :certificates [{:certificate/type :git/commit
                   :certificate/ref ""
                   :certificate/repo ""}]
   :context/anchors [(anchor :code/edit {:event/type :code/edit
                                         :file ""
                                         :fn ""})]
   :forecast {:benefits [{:tag :benefit/unknown
                          :locus (anchor :code/edit {:event/type :code/edit
                                                     :file ""
                                                     :fn ""})
                          :note ""}]
              :risks []
              :success []
              :failure []}
   :aif/g-terms []
   :rejections {}
   :horizon :short})

(defn pattern-proposal-template [session-id]
  {:proposal/id (str (java.util.UUID/randomUUID))
   :session/id session-id
   :pattern/draft {:context ""
                   :if ""
                   :however ""
                   :then ""
                   :because ""
                   :next-steps ""}
   :proposal/supporting-trail []
   :proposal/status :draft
   :proposal/reviewer-notes []})

(defn proof-commit-template [session-id]
  {:commit/changes nil
   :commit/session-id session-id
   :commit/checked-at (now-inst)
   :commit/pattern-trail []
   :commit/validation {:ok? false :errors []}})

(defn notebook-cell-template [session-id]
  {:session/id session-id
   :cell/id ""
   :cell/sequence 0
   :cell/input ""
   :cell/output ""
   :cell/metadata {:model ""
                   :cost {}
                   :latency-ms 0}
   :cell/side-effects []
   :cell/timestamp (now-inst)})

(defn changelog-entry-template [pattern-id]
  {:changelog/file ""
   :entry/date ""
   :entry/session-id ""
   :entry/pattern-id pattern-id
   :entry/deps []
   :entry/artifacts []
   :entry/summary ""})

(defn devmap-xref-template [pattern-id]
  {:fulab-pattern pattern-id
   :devmap-prototype ""
   :devmap-file ""
   :devmap-clause ""})

(defn blast-radius-template [session-id]
  {:session/id session-id
   :blast/surfaces []
   :blast/rollback []
   :blast/detectors []
   :blast/risk :unknown
   :blast/notes ""})

(defn tradeoff-record-template [session-id]
  {:session/id session-id
   :decision/id ""
   :decision/recorded-at (now-inst)
   :decision/options []
   :decision/assumptions []
   :decision/rationale ""
   :decision/reversal-triggers []})

(defn proposal-claim-event [proposal]
  (build-event :pattern/proposal-claimed
               {:proposal proposal
                :proposal/claimed-at (now-inst)}))

(defn wrap-claim-event [record]
  (cond
    (:pur/id record)
    (build-event :pattern/use-claimed {:pur record})

    (:psr/id record)
    (build-event :pattern/selection-claimed {:psr record})

    :else
    (build-event :pattern/record-claimed {:record record})))

(defn claim->verify-event [claim-event result]
  (let [payload (:payload claim-event)
        pur (:pur payload)
        psr (:psr payload)
        base {:check/result result
              :check/status (if (:ok? result) :pass :fail)}]
    (cond
      pur {:event/type :pattern/use-verified
           :at (now-inst)
           :payload (assoc base :pur/id (:pur/id pur))}

      psr {:event/type :pattern/selection-verified
           :at (now-inst)
           :payload (assoc base :psr/id (:psr/id psr))}

      :else {:event/type :pattern/record-verified
             :at (now-inst)
             :payload base})))

(defn pur-claim-events [session]
  (->> (:events session)
       (filter #(= :pattern/use-claimed (:event/type %)))))

(defn psr-claim-events [session]
  (->> (:events session)
       (filter #(= :pattern/selection-claimed (:event/type %)))))

(defn check-pur [pur session pattern-ids outcome-tags]
  (logic/check-step {:hx.step/kind :hx/pattern-use-claimed
                     :hx.step/payload {:pur pur}}
                    {:session session
                     :pattern-ids pattern-ids
                     :outcome-tags outcome-tags}))

(defn check-psr [psr session pattern-ids]
  (logic/check-step {:hx.step/kind :hx/pattern-selection-claimed
                     :hx.step/payload {:psr psr}}
                    {:session session
                     :pattern-ids pattern-ids}))

(defn expected-vs-resolved [entries resolver]
  (let [resolved (filter resolver entries)]
    {:expected (count entries)
     :resolved (count resolved)}))

(defn- g-terms->provenance [g-terms]
  (cond
    (map? g-terms)
    (map (fn [[term-id term]]
           (cond
             (map? term) (assoc term :term-id (or (:term-id term) term-id))
             :else {:term-id term-id
                    :final-contribution term}))
         g-terms)

    (coll? g-terms)
    g-terms

    :else nil))

(defn term-provenance-trace [psr]
  (let [terms (g-terms->provenance (:aif/g-terms psr))]
    (when (coll? terms)
      (mapv (fn [term]
              (if (map? term)
                (let [missing (vec (remove #(contains? term %) aif-term-provenance-keys))
                      trace (select-keys term aif-term-provenance-keys)]
                  (cond-> trace
                    (seq missing)
                    (assoc :missing-keys missing)))
                {:term term
                 :missing-keys (vec aif-term-provenance-keys)}))
            terms))))

(defn term-provenance-summary [psr]
  (let [g-terms (:aif/g-terms psr)]
    (if (nil? g-terms)
      {:present? false
       :expected 0
       :resolved 0
       :missing []}
      (let [trace (or (term-provenance-trace psr) [])
            summary (expected-vs-resolved trace #(empty? (:missing-keys %)))
            missing (->> trace
                         (filter #(seq (:missing-keys %)))
                         (map #(or (:term-id %) (:term %)))
                         vec)]
        (assoc summary
               :present? true
               :missing missing)))))

(defn anchor-resolver [session]
  (fn [anchor]
    (let [result (logic/check-step {:hx.step/kind :hx/pattern-use-claimed
                                    :hx.step/payload {:pur {:pur/id "_"
                                                             :session/id (:session/id session)
                                                             :pattern/id "_"
                                                             :instance/id "_"
                                                             :fields {:context "" :if "" :however "" :then "" :because "" :next-steps ""}
                                                             :anchors [anchor]}}}
                                   {:session session
                                    :pattern-ids #{"_"}})]
      (:ok? result))))

(defn forecast-summary [forecast session]
  (let [entries (mapcat #(get forecast % []) forecast-fields)
        resolver (fn [entry]
                   (let [locus (:locus entry)
                         anchor-res (anchor-resolver session)]
                     (if (and (map? locus) (:anchor/type locus))
                       (anchor-res locus)
                       false)))]
    (expected-vs-resolved entries resolver)))

(defn- precision-channel-weights [precision-channels]
  (cond
    (map? precision-channels)
    (keep (fn [[channel weight]]
            (when (number? weight)
              [channel (Math/abs (double weight))]))
          precision-channels)

    (coll? precision-channels)
    (keep (fn [entry]
            (cond
              (and (vector? entry) (= 2 (count entry)))
              (when (number? (second entry))
                [(first entry) (Math/abs (double (second entry)))])

              (map? entry)
              (let [channel (or (:channel/id entry) (:channel entry) (:id entry))
                    weight (or (:weight entry) (:value entry) (:contribution entry))]
                (when (and channel (number? weight))
                  [channel (Math/abs (double weight))]))

              :else nil))
          precision-channels)

    :else nil))

(defn- dominant-channels [psr]
  (let [terms (g-terms->provenance (:aif/g-terms psr))]
    (when (coll? terms)
      (->> terms
           (keep (fn [term]
                   (let [term-id (:term-id term)
                         weights (precision-channel-weights (:precision-channels term))
                         total (when (seq weights)
                                 (reduce + (map second weights)))]
                     (when (and term-id (number? total) (pos? total))
                       (let [[channel share] (->> weights
                                                  (map (fn [[ch weight]]
                                                         [ch (/ weight total)]))
                                                  (apply max-key second))]
                         (when (> share dominant-channel-threshold)
                           {:term-id term-id
                            :channel channel
                            :share share}))))))))))

(defn dominant-channel-summary [psr]
  (let [terms (g-terms->provenance (:aif/g-terms psr))]
    (when (coll? terms)
      (let [dominant (dominant-channels psr)]
        {:total (count terms)
         :dominant (count dominant)
         :terms (vec dominant)}))))
