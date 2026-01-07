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

       (finalize-result :hx.step/unknown [] [] [{:issue :unknown-step :kind kind}])))))

(defn explain-failure
  "Bounded diagnostic helper for inadmissible steps."
  ([step] (explain-failure step {}))
  ([step opts]
   (let [result (check-step step opts)]
     (when-not (:ok? result)
       (take 3 (get-in result [:logic :obligations]))))))
