(ns futon3.hx.logic
  "core.logic-based admissibility checks for hypertext steps."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.string :as str]
            [futon3.hx.link-types :as link-types]
            [futon3.hx.store :as store]))

(pldb/db-rel artifacto id)
(pldb/db-rel anchoro artifact-id anchor-id)
(pldb/db-rel linko id status from-artifact from-anchor to-artifact to-anchor type)
(pldb/db-rel allowed-typeo type)

(def ^:private default-policies
  {:require-docs? false
   :require-patterns? false
   :reject-duplicates? true})

(defn- blankish? [s]
  (or (nil? s)
      (and (string? s)
           (str/blank? s))))

(defn- state->db [state]
  (let [base (pldb/db)
        artifact-facts (for [[artifact-id _] (:artifacts state)]
                         [artifacto artifact-id])
        anchor-facts (for [[artifact-id anchors] (:anchors state)
                           anchor anchors
                           :let [anchor-id (:anchor/id anchor)]
                           :when anchor-id]
                       [anchoro artifact-id anchor-id])
        link-facts (for [[link-id link] (:links state)]
                     [linko link-id
                      (:link/status link)
                      (get-in link [:link/from :artifact/id])
                      (get-in link [:link/from :anchor/id])
                      (get-in link [:link/to :artifact/id])
                      (get-in link [:link/to :anchor/id])
                      (:link/type link)])
        type-facts (for [link-type (link-types/allowed-types)]
                     [allowed-typeo link-type])
        facts (concat artifact-facts anchor-facts link-facts type-facts)]
    (if (seq facts)
      (apply pldb/db facts)
      base)))

(defn- exists? [db goal]
  (not (empty? (pldb/with-db db
                 (l/run 1 [q]
                   goal
                   (l/== q true))))))

(defn- artifact-exists? [db artifact-id]
  (and (not (blankish? artifact-id))
       (exists? db (artifacto artifact-id))))

(defn- anchor-exists? [db artifact-id anchor-id]
  (and (not (blankish? artifact-id))
       (not (blankish? anchor-id))
       (exists? db (anchoro artifact-id anchor-id))))

(defn- link-exists? [db link-id]
  (and (not (blankish? link-id))
       (exists? db
                (l/fresh [status from-art from-anchor to-art to-anchor link-type]
                  (linko link-id status from-art from-anchor to-art to-anchor link-type)))))

(defn- link-status? [db link-id status]
  (exists? db
           (l/fresh [from-art from-anchor to-art to-anchor link-type]
             (linko link-id status from-art from-anchor to-art to-anchor link-type))))

(defn- allowed-type? [db link-type]
  (and (keyword? link-type)
       (exists? db (allowed-typeo link-type))))

(defn- duplicate-link? [db from to link-type]
  (let [from-art (get-in from [:artifact/id])
        from-anchor (get-in from [:anchor/id])
        to-art (get-in to [:artifact/id])
        to-anchor (get-in to [:anchor/id])]
    (exists? db
             (l/fresh [link-id status]
               (linko link-id status from-art from-anchor to-art to-anchor link-type)))))

(defn- interactive? [artifact]
  (or (:artifact/interactive artifact)
      (:artifact/interactive? artifact)))

(defn- has-docs? [artifact]
  (or (seq (:artifact/documents artifact))
      (seq (:artifact/docbook artifact))
      (seq (:artifact/docbook-id artifact))))

(defn- has-patterns? [artifact]
  (or (seq (:artifact/patterns artifact))
      (seq (:artifact/pattern-ids artifact))))

(defn- selector-valid? [selector]
  (cond
    (nil? selector) true
    (and (map? selector) (= :line (:kind selector)))
    (integer? (:line selector))
    (and (map? selector) (= :regex (:kind selector)))
    (string? (:pattern selector))
    :else false))

(defn- duplicate-anchor-ids [anchors]
  (->> anchors
       (keep :anchor/id)
       frequencies
       (filter (fn [[_ count]] (> count 1)))
       (map first)
       vec))

(defn- step-policies [step opts]
  (merge default-policies
         (:policies opts)
         (:hx.step/policy step)))

(defn- finalize-result [rule facts obligations errors]
  (let [ok? (and (empty? obligations) (empty? errors))]
    {:ok? ok?
     :errors errors
     :logic {:kernel :hx.logic/v1
             :judgement (if ok? :admissible :inadmissible)
             :witness {:rule rule
                       :facts facts}
             :obligations obligations}}))

(def ^:private pur-fields
  [:context :if :however :then :because :next-steps])

(def ^:private horizon-values
  #{:immediate :short :medium :long})

(defn- anchor? [value]
  (and (map? value)
       (keyword? (:anchor/type value))
       (map? (:anchor/ref value))))

(defn- anchor-match-value [event key]
  (if (vector? key)
    (get-in event key)
    (get event key)))

(defn- anchor-ref-matches? [event ref]
  (let [ref-keys (remove #{:lines} (keys ref))]
    (every? (fn [k]
              (= (anchor-match-value event k)
                 (get ref k)))
            ref-keys)))

(defn- anchor-resolves? [anchor session-events extra-anchors]
  (and (anchor? anchor)
       (or (some #(= anchor %) extra-anchors)
           (some #(anchor-ref-matches? % (:anchor/ref anchor)) session-events))))

(defn- locus-resolves? [locus session-events extra-anchors]
  (cond
    (anchor? locus)
    (anchor-resolves? locus session-events extra-anchors)

    (and (map? locus)
         (contains? locus :locus/type)
         (#{:stream :log} (:locus/type locus))
         (or (:stream/id locus) (:stream/name locus) (:stream/path locus)))
    true

    :else false))

(defn- pattern-known? [pattern-id pattern-ids]
  (and (string? pattern-id)
       (contains? pattern-ids pattern-id)))

(defn- fields-valid? [fields]
  (and (map? fields)
       (= (set pur-fields) (set (keys fields)))
       (every? (fn [[_ v]]
                 (or (string? v) (anchor? v)))
               fields)))

(def ^:private certificate-types
  #{:git/commit})

(defn- normalize-certificate-type [value]
  (cond
    (keyword? value) value
    (string? value) (keyword value)
    :else nil))

(defn- git-commit-ref? [value]
  (and (string? value)
       (re-matches #"(?i)[0-9a-f]{7,40}" value)))

(defn- certificate-valid? [cert]
  (and (map? cert)
       (contains? certificate-types (normalize-certificate-type (:certificate/type cert)))
       (git-commit-ref? (:certificate/ref cert))
       (or (blankish? (:certificate/repo cert))
           (string? (:certificate/repo cert)))))

(defn- invalid-certificates [certs]
  (->> certs
       (keep-indexed (fn [idx cert]
                       (when-not (certificate-valid? cert)
                         {:index idx :certificate cert})))
       vec))

(def ^:private aif-term-provenance-keys
  [:term-id :observation-keys :precision-channels :intermediate-values :final-contribution])

(defn- term-id-valid? [term-id]
  (or (keyword? term-id)
      (and (string? term-id)
           (not (str/blank? term-id)))))

(defn- observation-key-valid? [observation-key]
  (or (keyword? observation-key)
      (and (string? observation-key)
           (not (str/blank? observation-key)))
      (vector? observation-key)))

(defn- observation-keys-valid? [observation-keys]
  (and (coll? observation-keys)
       (every? observation-key-valid? observation-keys)))

(defn- precision-channel-entry->pair [entry]
  (cond
    (and (vector? entry) (= 2 (count entry)))
    (let [[channel weight] entry]
      (when (and channel (number? weight))
        [channel (Math/abs (double weight))]))

    (map? entry)
    (let [channel (or (:channel/id entry) (:channel entry) (:id entry))
          weight (or (:weight entry) (:value entry) (:contribution entry))]
      (when (and channel (number? weight))
        [channel (Math/abs (double weight))]))

    :else nil))

(defn- precision-channel-entry-valid? [entry]
  (some? (precision-channel-entry->pair entry)))

(defn- precision-channels-valid? [precision-channels]
  (cond
    (map? precision-channels)
    (every? precision-channel-entry-valid? precision-channels)

    (coll? precision-channels)
    (every? precision-channel-entry-valid? precision-channels)

    :else false))

(defn- intermediate-values-valid? [intermediate-values]
  (or (map? intermediate-values)
      (coll? intermediate-values)))

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

(defn- term-provenance-issues [term]
  (let [missing (remove #(contains? term %) aif-term-provenance-keys)]
    (cond-> []
      (seq missing)
      (conj {:issue :missing-keys :keys (vec missing)})
      (and (contains? term :term-id)
           (not (term-id-valid? (:term-id term))))
      (conj {:issue :invalid-term-id :term-id (:term-id term)})
      (and (contains? term :observation-keys)
           (not (observation-keys-valid? (:observation-keys term))))
      (conj {:issue :invalid-observation-keys})
      (and (contains? term :precision-channels)
           (not (precision-channels-valid? (:precision-channels term))))
      (conj {:issue :invalid-precision-channels})
      (and (contains? term :intermediate-values)
           (not (intermediate-values-valid? (:intermediate-values term))))
      (conj {:issue :invalid-intermediate-values})
      (and (contains? term :final-contribution)
           (not (number? (:final-contribution term))))
      (conj {:issue :invalid-final-contribution}))))

(defn- invalid-term-provenance [terms]
  (->> terms
       (keep-indexed (fn [idx term]
                       (let [issues (if (map? term)
                                      (term-provenance-issues term)
                                      [{:issue :invalid-term
                                        :value term}])]
                         (when (seq issues)
                           {:index idx
                            :term-id (:term-id term)
                            :issues issues}))))
       vec))

(def ^:private dominant-channel-threshold 0.6)

(defn- precision-channel-weights [precision-channels]
  (cond
    (map? precision-channels)
    (keep precision-channel-entry->pair precision-channels)

    (coll? precision-channels)
    (keep precision-channel-entry->pair precision-channels)

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
                            :share share}))))))
           vec))))

(defn- validator-result [validator status reasons]
  {:validator validator
   :status status
   :reasons reasons})

(defn- validators->result [rule facts errors validators]
  (let [statuses (set (map :status validators))
        ok? (= statuses #{:pass})
        obligations (vec (for [{:keys [validator status reasons]} validators
                               :when (not= status :pass)]
                           {:obligation :validator-failed
                            :validator validator
                            :status status
                            :reasons reasons}))]
    {:ok? ok?
     :errors errors
     :logic {:kernel :hx.logic/v1
             :judgement (if ok? :admissible :inadmissible)
             :witness {:rule rule
                       :facts facts}
             :checks validators
             :obligations obligations}}))

(defn- check-artifact-register [db step policies]
  (let [payload (:hx.step/payload step)
        artifact (or (:artifact payload) payload)
        artifact-id (:artifact/id artifact)
        errors (cond-> []
                 (blankish? artifact-id)
                 (conj {:issue :missing-artifact-id}))
        facts (cond-> []
                (not (blankish? artifact-id))
                (conj {:fact :artifact/id-present :id artifact-id}))
        obligations (cond-> []
                      (and (:require-docs? policies)
                           (interactive? artifact)
                           (not (has-docs? artifact)))
                      (conj {:obligation :needs-documentation
                             :artifact/id artifact-id})
                      (and (:require-patterns? policies)
                           (not (has-patterns? artifact)))
                      (conj {:obligation :needs-pattern-link
                             :artifact/id artifact-id}))]
    (finalize-result :hx.artifact/register-admissible facts obligations errors)))

(defn- check-anchors-upsert [db step]
  (let [payload (:hx.step/payload step)
        artifact-id (:artifact/id payload)
        anchors (:anchors payload)
        dupes (duplicate-anchor-ids anchors)
        errors (cond-> []
                 (blankish? artifact-id)
                 (conj {:issue :missing-artifact-id})
                 (some #(blankish? (:anchor/id %)) anchors)
                 (conj {:issue :missing-anchor-id})
                 (seq dupes)
                 (conj {:issue :duplicate-anchor-id :anchor/ids dupes})
                 (some #(not (selector-valid? (:anchor/selector %))) anchors)
                 (conj {:issue :invalid-selector}))
        facts (cond-> []
                (artifact-exists? db artifact-id)
                (conj {:fact :artifact/exists :id artifact-id}))
        obligations (cond-> []
                      (and (not (blankish? artifact-id))
                           (not (artifact-exists? db artifact-id)))
                      (conj {:obligation :missing-artifact
                             :artifact/id artifact-id}))]
    (finalize-result :hx.anchors/upsert-admissible facts obligations errors)))

(defn- check-link-suggest [db step policies]
  (let [payload (:hx.step/payload step)
        link (:link payload)
        from (:link/from link)
        to (:link/to link)
        link-type (:link/type link)
        errors (cond-> []
                 (blankish? (get-in from [:artifact/id]))
                 (conj {:issue :missing-from-artifact})
                 (blankish? (get-in to [:artifact/id]))
                 (conj {:issue :missing-to-artifact})
                 (blankish? link-type)
                 (conj {:issue :missing-link-type}))
        facts (cond-> []
                (artifact-exists? db (get-in from [:artifact/id]))
                (conj {:fact :artifact/exists :id (get-in from [:artifact/id])})
                (anchor-exists? db (get-in from [:artifact/id]) (get-in from [:anchor/id]))
                (conj {:fact :anchor/exists
                       :artifact/id (get-in from [:artifact/id])
                       :anchor/id (get-in from [:anchor/id])})
                (artifact-exists? db (get-in to [:artifact/id]))
                (conj {:fact :artifact/exists :id (get-in to [:artifact/id])})
                (anchor-exists? db (get-in to [:artifact/id]) (get-in to [:anchor/id]))
                (conj {:fact :anchor/exists
                       :artifact/id (get-in to [:artifact/id])
                       :anchor/id (get-in to [:anchor/id])})
                (allowed-type? db link-type)
                (conj {:fact :link-type/allowed :type link-type}))
        obligations (cond-> []
                      (and (not (blankish? (get-in from [:artifact/id])))
                           (not (artifact-exists? db (get-in from [:artifact/id]))))
                      (conj {:obligation :missing-artifact
                             :artifact/id (get-in from [:artifact/id])})
                      (and (not (blankish? (get-in from [:anchor/id])))
                           (not (anchor-exists? db (get-in from [:artifact/id])
                                                (get-in from [:anchor/id]))))
                      (conj {:obligation :needs-anchor
                             :artifact/id (get-in from [:artifact/id])
                             :anchor/id (get-in from [:anchor/id])})
                      (and (not (blankish? (get-in to [:artifact/id])))
                           (not (artifact-exists? db (get-in to [:artifact/id]))))
                      (conj {:obligation :missing-artifact
                             :artifact/id (get-in to [:artifact/id])})
                      (and (not (blankish? (get-in to [:anchor/id])))
                           (not (anchor-exists? db (get-in to [:artifact/id])
                                                (get-in to [:anchor/id]))))
                      (conj {:obligation :needs-anchor
                             :artifact/id (get-in to [:artifact/id])
                             :anchor/id (get-in to [:anchor/id])})
                      (and (keyword? link-type)
                           (not (allowed-type? db link-type)))
                      (conj {:obligation :unknown-link-type
                             :type link-type})
                      (and (:reject-duplicates? policies)
                           (duplicate-link? db from to link-type))
                      (conj {:obligation :duplicate-link
                             :from from
                             :to to
                             :type link-type}))]
    (finalize-result :hx.link/suggest-admissible facts obligations errors)))

(defn- check-link-decision [db step rule allowed-statuses]
  (let [payload (:hx.step/payload step)
        link-id (:link/id payload)
        decided-by (:decided-by payload)
        errors (cond-> []
                 (blankish? link-id)
                 (conj {:issue :missing-link-id})
                 (not (keyword? decided-by))
                 (conj {:issue :missing-decided-by}))
        facts (cond-> []
                (link-exists? db link-id)
                (conj {:fact :link/exists :link/id link-id}))
        obligations (cond-> []
                      (and (not (blankish? link-id))
                           (not (link-exists? db link-id)))
                      (conj {:obligation :missing-link
                             :link/id link-id})
                      (and (link-exists? db link-id)
                           (not (some #(link-status? db link-id %) allowed-statuses)))
                      (conj {:obligation :invalid-link-status
                             :link/id link-id
                             :allowed allowed-statuses}))]
    (finalize-result rule facts obligations errors)))

(defn- check-pattern-use-claimed [step opts]
  (let [payload (:hx.step/payload step)
        pur (or (:pur payload) payload)
        session (:session opts)
        session-events (or (:events session) (:session-events opts) [])
        pattern-ids (or (:pattern-ids opts) #{})
        outcome-tags (or (:outcome-tags opts) #{})
        extra-anchors (or (:extra-anchors opts) [])
        required-keys [:pur/id :session/id :pattern/id :instance/id :fields :anchors :certificates]
        missing-keys (filter #(not (contains? pur %)) required-keys)
        fields (:fields pur)
        anchors (:anchors pur)
        certificates (:certificates pur)
        v1-reasons (cond-> []
                     (seq missing-keys)
                     (conj {:issue :missing-keys :keys (vec missing-keys)})
                     (not (pattern-known? (:pattern/id pur) pattern-ids))
                     (conj {:issue :unknown-pattern :pattern-id (:pattern/id pur)})
                     (not (fields-valid? fields))
                     (conj {:issue :invalid-fields}))
        v1 (validator-result :pur/traceability
                             (if (empty? v1-reasons) :pass :fail)
                             v1-reasons)
        resolved-anchors (filter #(anchor-resolves? % session-events extra-anchors) anchors)
        v2-status (cond
                    (empty? anchors) :fail
                    (empty? resolved-anchors) :fail
                    (= (count resolved-anchors) (count anchors)) :pass
                    :else :partial)
        v2-reasons (cond-> []
                     (empty? anchors) (conj {:issue :missing-anchors})
                     (and (seq anchors) (empty? resolved-anchors))
                     (conj {:issue :anchors-unresolved})
                     (and (seq anchors) (not= (count resolved-anchors) (count anchors)))
                     (conj {:issue :anchors-partial
                            :resolved (count resolved-anchors)
                            :total (count anchors)}))
        v2 (validator-result :pur/anchors v2-status v2-reasons)
        v3 (if (seq (:outcome/tags pur))
             (let [unknown (->> (:outcome/tags pur)
                                (remove outcome-tags)
                                vec)]
               (validator-result :pur/outcome-tags
                                 (if (empty? unknown) :pass :fail)
                                 (when (seq unknown)
                                   [{:issue :unknown-tags :tags unknown}])))
             (validator-result :pur/outcome-tags :pass []))
        v4 (if-let [delta (:delta pur)]
             (let [before (:before delta)
                   after (:after delta)
                   before-ok (anchor-resolves? before session-events extra-anchors)
                   after-ok (anchor-resolves? after session-events extra-anchors)
                   identical? (= before after)
                   status (cond
                            (and before-ok after-ok (not identical?)) :pass
                            (or (nil? before) (nil? after)) :fail
                            :else :fail)
                   reasons (cond-> []
                             (nil? before) (conj {:issue :missing-before})
                             (nil? after) (conj {:issue :missing-after})
                             (and before (not before-ok)) (conj {:issue :before-unresolved})
                             (and after (not after-ok)) (conj {:issue :after-unresolved})
                             identical? (conj {:issue :anchors-identical}))]
               (validator-result :pur/delta status reasons))
             (validator-result :pur/delta :pass []))
        v5 (if-let [tension (:unresolved-tension pur)]
             (let [status (cond
                            (string? tension) :pass
                            (and (map? tension)
                                 (#{:if :however} (:ref tension))
                                 (string? (:note tension))) :pass
                            :else :fail)
                   reasons (when (= status :fail)
                             [{:issue :invalid-tension}])]
               (validator-result :pur/tension status reasons))
             (validator-result :pur/tension :pass []))
        v6 (if-let [counterevidence (:counterevidence pur)]
             (let [loci (keep :locus counterevidence)
                   resolved (filter #(locus-resolves? % session-events extra-anchors) loci)
                   status (cond
                            (empty? loci) :fail
                            (seq resolved) :pass
                            :else :fail)
                   reasons (cond-> []
                             (empty? loci) (conj {:issue :missing-locus})
                             (and (seq loci) (empty? resolved))
                             (conj {:issue :locus-unresolved}))]
               (validator-result :pur/counterevidence status reasons))
             (validator-result :pur/counterevidence :pass []))
        v7 (if-let [proposal (:revision-proposal pur)]
             (let [field (:field proposal)
                   edit (:edit proposal)
                   support (:support proposal)
                   support-resolved (filter #(anchor-resolves? % session-events extra-anchors) support)
                   status (cond
                            (and (contains? (set pur-fields) field)
                                 (string? edit)
                                 (seq support)
                                 (= (count support) (count support-resolved))) :pass
                            (and (contains? (set pur-fields) field)
                                 (string? edit)
                                 (seq support)
                                 (seq support-resolved)) :partial
                            :else :fail)
                   reasons (cond-> []
                             (not (contains? (set pur-fields) field))
                             (conj {:issue :invalid-field})
                             (not (string? edit))
                             (conj {:issue :missing-edit})
                             (empty? support)
                             (conj {:issue :missing-support})
                             (and (seq support) (not= (count support) (count support-resolved)))
                             (conj {:issue :support-unresolved
                                    :resolved (count support-resolved)
                                    :total (count support)}))]
               (validator-result :pur/revision status reasons))
             (validator-result :pur/revision :pass []))
        invalid-certs (when (coll? certificates)
                        (invalid-certificates certificates))
        v8-status (cond
                    (not (coll? certificates)) :fail
                    (empty? certificates) :fail
                    (seq invalid-certs) :fail
                    :else :pass)
        v8-reasons (cond-> []
                     (not (coll? certificates)) (conj {:issue :invalid-certificates})
                     (empty? certificates) (conj {:issue :missing-certificates})
                     (seq invalid-certs) (conj {:issue :invalid-certificates
                                                :certificates invalid-certs}))
        v8 (validator-result :pur/certificates v8-status v8-reasons)
        validators [v1 v2 v3 v4 v5 v6 v7 v8]]
    (validators->result :hx.pattern/use-claimed [] [] validators)))

(defn- check-psr [psr]
  (let [g-terms (:aif/g-terms psr)
        terms (g-terms->provenance g-terms)
        invalid-terms (when (coll? terms)
                        (invalid-term-provenance terms))
        status (cond
                 (nil? g-terms) :fail
                 (not (coll? g-terms)) :fail
                 (empty? terms) :fail
                 (seq invalid-terms) :fail
                 :else :pass)
        reasons (cond-> []
                  (nil? g-terms) (conj {:issue :missing-g-terms})
                  (and (some? g-terms) (not (coll? g-terms)))
                  (conj {:issue :invalid-g-terms})
                  (and (coll? terms) (empty? terms))
                  (conj {:issue :empty-g-terms})
                  (seq invalid-terms)
                  (conj {:issue :invalid-term-provenance
                         :terms invalid-terms}))
        dominant (when (coll? terms)
                   (dominant-channels psr))
        facts (cond-> []
                (seq dominant)
                (conj {:fact :psr/dominant-channels
                       :terms dominant}))]
    {:validators [(validator-result :psr/aif-g-terms status reasons)]
     :facts facts}))

(defn- check-pattern-selection-claimed [step opts]
  (let [payload (:hx.step/payload step)
        psr (or (:psr payload) payload)
        session (:session opts)
        session-events (or (:events session) (:session-events opts) [])
        pattern-ids (or (:pattern-ids opts) #{})
        extra-anchors (or (:extra-anchors opts) [])
        required-keys [:psr/id :session/id :decision/id :candidates :chosen :context/anchors
                       :forecast :rejections :horizon :certificates]
        missing-keys (filter #(not (contains? psr %)) required-keys)
        candidates (:candidates psr)
        chosen (:chosen psr)
        forecast (:forecast psr)
        rejections (:rejections psr)
        certificates (:certificates psr)
        v1-reasons (cond-> []
                     (seq missing-keys)
                     (conj {:issue :missing-keys :keys (vec missing-keys)})
                     (not (and (coll? candidates) (some #{chosen} candidates)))
                     (conj {:issue :chosen-not-in-candidates})
                     (and (coll? candidates)
                          (seq candidates)
                          (not (every? #(pattern-known? % pattern-ids) candidates)))
                     (conj {:issue :unknown-patterns
                            :patterns (vec (remove #(pattern-known? % pattern-ids) candidates))})
                     (not (horizon-values (:horizon psr)))
                     (conj {:issue :invalid-horizon}))
        v1-status (cond
                    (seq missing-keys) :fail
                    (some #{:chosen-not-in-candidates} (map :issue v1-reasons)) :fail
                    (some #{:unknown-patterns :invalid-horizon} (map :issue v1-reasons)) :partial
                    :else :pass)
        v1 (validator-result :psr/candidates v1-status v1-reasons)
        override? (:override/solo? psr)
        override-note (:override/note psr)
        v2-status (cond
                    override?
                    (if (string? override-note) :pass :fail)
                    (and (coll? candidates) (>= (count candidates) 2)) :pass
                    :else :fail)
        v2-reasons (cond-> []
                     (and override? (not (string? override-note)))
                     (conj {:issue :missing-override-note})
                     (and (not override?) (or (nil? candidates) (< (count candidates) 2)))
                     (conj {:issue :insufficient-candidates
                            :count (count candidates)}))
        v2 (validator-result :psr/two-alternative v2-status v2-reasons)
        context-anchors (:context/anchors psr)
        resolved-context (filter #(anchor-resolves? % session-events extra-anchors) context-anchors)
        v3-status (cond
                    (empty? context-anchors) :fail
                    (empty? resolved-context) :fail
                    (= (count resolved-context) (count context-anchors)) :pass
                    :else :partial)
        v3-reasons (cond-> []
                     (empty? context-anchors) (conj {:issue :missing-anchors})
                     (and (seq context-anchors) (empty? resolved-context))
                     (conj {:issue :anchors-unresolved})
                     (and (seq context-anchors)
                          (not= (count resolved-context) (count context-anchors)))
                     (conj {:issue :anchors-partial
                            :resolved (count resolved-context)
                            :total (count context-anchors)}))
        v3 (validator-result :psr/anchors v3-status v3-reasons)
        forecast-keys [:benefits :risks :success :failure]
        forecast-missing (filter #(not (contains? forecast %)) forecast-keys)
        forecast-entries (mapcat #(get forecast % []) forecast-keys)
        forecast-resolved (filter #(locus-resolves? (:locus %) session-events extra-anchors)
                                  forecast-entries)
        v4-status (cond
                    (seq forecast-missing) :fail
                    (empty? forecast-entries) :partial
                    (= (count forecast-entries) (count forecast-resolved)) :pass
                    (seq forecast-resolved) :partial
                    :else :fail)
        v4-reasons (cond-> []
                     (seq forecast-missing) (conj {:issue :missing-forecast
                                                   :keys (vec forecast-missing)})
                     (empty? forecast-entries) (conj {:issue :empty-forecast})
                     (and (seq forecast-entries)
                          (not= (count forecast-entries) (count forecast-resolved)))
                     (conj {:issue :forecast-unresolved
                            :resolved (count forecast-resolved)
                            :total (count forecast-entries)}))
        v4 (validator-result :psr/forecast v4-status v4-reasons)
        expected-rejections (set (remove #{chosen} candidates))
        rejection-codes (fn [m]
                          (let [codes (:codes m)]
                            (and (coll? codes) (seq codes))))
        missing-rejections (filter #(not (contains? rejections %)) expected-rejections)
        empty-rejections (filter #(not (rejection-codes (get rejections %))) expected-rejections)
        v5-status (cond
                    (seq missing-rejections) :fail
                    (seq empty-rejections) :fail
                    :else :pass)
        v5-reasons (cond-> []
                     (seq missing-rejections) (conj {:issue :missing-rejections
                                                     :patterns (vec missing-rejections)})
                     (seq empty-rejections) (conj {:issue :missing-rejection-codes
                                                   :patterns (vec empty-rejections)}))
        v5 (validator-result :psr/rejections v5-status v5-reasons)
        invalid-certs (when (coll? certificates)
                        (invalid-certificates certificates))
        v6-status (cond
                    (not (coll? certificates)) :fail
                    (empty? certificates) :fail
                    (seq invalid-certs) :fail
                    :else :pass)
        v6-reasons (cond-> []
                     (not (coll? certificates)) (conj {:issue :invalid-certificates})
                     (empty? certificates) (conj {:issue :missing-certificates})
                     (seq invalid-certs) (conj {:issue :invalid-certificates
                                                :certificates invalid-certs}))
        v6 (validator-result :psr/certificates v6-status v6-reasons)
        psr-check (check-psr psr)
        validators (into [v1 v2 v3 v4 v5 v6] (:validators psr-check))
        facts (or (:facts psr-check) [])]
    (validators->result :hx.pattern/selection-claimed facts [] validators)))

(defn check-step
  "Validate a canonical hx step and return structural admissibility with witness."
  ([step] (check-step step {}))
  ([step opts]
   (let [state (or (:state opts) (store/state))
         db (state->db state)
         policies (step-policies step opts)
         kind (:hx.step/kind step)]
     (case kind
       :hx/artifact-register
       (check-artifact-register db step policies)

       :hx/anchors-upsert
       (check-anchors-upsert db step)

       :hx/link-suggest
       (check-link-suggest db step policies)

       :hx/link-accept
       (check-link-decision db step :hx.link/accept-admissible [:suggested :needs-review])

       :hx/link-reject
       (check-link-decision db step :hx.link/reject-admissible [:suggested :needs-review])

       :hx/pattern-use-claimed
       (check-pattern-use-claimed step opts)

       :hx/pattern-use-verified
       (check-pattern-use-claimed step opts)

       :hx/pattern-selection-claimed
       (check-pattern-selection-claimed step opts)

       :hx/pattern-selection-verified
       (check-pattern-selection-claimed step opts)

       (finalize-result :hx.step/unknown [] [] [{:issue :unknown-step :kind kind}])))))

(defn explain-failure
  "Bounded diagnostic helper for inadmissible steps."
  ([step] (explain-failure step {}))
  ([step opts]
   (let [result (check-step step opts)]
     (when-not (:ok? result)
       (take 3 (get-in result [:logic :obligations]))))))
