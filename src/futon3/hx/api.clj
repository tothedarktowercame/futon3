(ns futon3.hx.api
  "CRUD helpers for hypertext artifacts, anchors, and links."
  (:require [clojure.string :as str]
            [f0.clock :as clock]
            [futon3.hx.link-types :as link-types]
            [futon3.hx.store :as store]
            [futon3.hx.validate :as validate])
  (:import (java.util UUID)))

(defn- blankish? [s]
  (or (nil? s)
      (and (string? s)
           (str/blank? s))))

(defn- now []
  (clock/->iso-string))

(defn- random-id []
  (str "HX-" (.substring (str (UUID/randomUUID)) 0 8)))

(defn register-artifact!
  "Register an artifact and persist it to the log.
   Returns {:ok true :artifact ...} or {:ok false :err ...}."
  [artifact]
  (if (blankish? (:artifact/id artifact))
    {:ok false :err "missing-artifact-id"}
    (let [step {:hx.step/kind :hx/artifact-register
                :hx.step/payload {:artifact artifact}}
          structural (validate/structural-step step)]
      (if-not (:ok? structural)
        {:ok false :err "inadmissible-artifact" :details structural}
        (let [entry (assoc artifact
                           :artifact/registered (or (:artifact/registered artifact)
                                                    (now)))
              persisted (store/append-entry! {:op :artifact/register
                                              :artifact entry})]
          {:ok true :artifact (:artifact persisted)})))))

(defn get-artifact [artifact-id]
  (get-in (store/state) [:artifacts artifact-id]))

(defn list-artifacts []
  (-> (store/state) :artifacts vals vec))

(defn upsert-anchors!
  "Replace anchors for ARTIFACT-ID and persist the change."
  [artifact-id anchors]
  (if (blankish? artifact-id)
    {:ok false :err "missing-artifact-id"}
    (let [step {:hx.step/kind :hx/anchors-upsert
                :hx.step/payload {:artifact/id artifact-id
                                  :anchors (vec anchors)}}
          structural (validate/structural-step step)]
      (if-not (:ok? structural)
        {:ok false :err "inadmissible-anchors" :details structural}
        (let [entry {:op :anchors/replace
                     :artifact/id artifact-id
                     :anchors (vec anchors)}]
          (store/append-entry! entry)
          {:ok true :artifact/id artifact-id :anchors (vec anchors)})))))

(defn get-anchors [artifact-id]
  (get-in (store/state) [:anchors artifact-id]))

(defn- normalize-link [link]
  (let [link-id (or (:link/id link) (random-id))]
    (-> link
        (assoc :link/id link-id)
        (assoc :link/status (or (:link/status link) :suggested))
        (assoc :link/created (or (:link/created link) (now))))))

(defn suggest-link!
  "Persist a suggested link. Returns {:ok true :link ...} or error map."
  [link]
  (if (blankish? (get-in link [:link/from :artifact/id]))
    {:ok false :err "missing-link-from"}
    (if (blankish? (get-in link [:link/to :artifact/id]))
      {:ok false :err "missing-link-to"}
      (if (blankish? (:link/type link))
        {:ok false :err "missing-link-type"}
        (if-not (link-types/allowed-type? (:link/type link))
          {:ok false :err "unknown-link-type" :details {:link/type (:link/type link)}}
          (let [step {:hx.step/kind :hx/link-suggest
                      :hx.step/payload {:link link}}
                structural (validate/structural-step step)]
            (if-not (:ok? structural)
              {:ok false :err "inadmissible-link" :details structural}
              (let [normalized (normalize-link link)
                    persisted (store/append-entry! {:op :link/suggest
                                                    :link normalized})]
                {:ok true :link (:link persisted)}))))))))

(defn accept-link!
  "Mark a link as accepted and persist the decision."
  ([link-id decided-by] (accept-link! link-id decided-by nil))
  ([link-id decided-by validation]
   (if (blankish? link-id)
     {:ok false :err "missing-link-id"}
     (let [step {:hx.step/kind :hx/link-accept
                 :hx.step/payload {:link/id link-id
                                   :decided-by decided-by}}
           structural (validate/structural-step step)]
       (if-not (:ok? structural)
         {:ok false :err "inadmissible-link-accept" :details structural}
         (let [entry (cond-> {:op :link/accept
                              :link/id link-id
                              :link/decided (now)
                              :link/decided-by decided-by}
                       validation (assoc :link/validation validation))
               persisted (store/append-entry! entry)]
           {:ok true :link-id (:link/id persisted)}))))))

(defn reject-link!
  "Mark a link as rejected and persist the decision."
  ([link-id decided-by] (reject-link! link-id decided-by nil nil))
  ([link-id decided-by reason] (reject-link! link-id decided-by reason nil))
  ([link-id decided-by reason validation]
   (if (blankish? link-id)
     {:ok false :err "missing-link-id"}
     (let [step {:hx.step/kind :hx/link-reject
                 :hx.step/payload {:link/id link-id
                                   :decided-by decided-by}}
           structural (validate/structural-step step)]
       (if-not (:ok? structural)
         {:ok false :err "inadmissible-link-reject" :details structural}
         (let [entry (cond-> {:op :link/reject
                              :link/id link-id
                              :link/decided (now)
                              :link/decided-by decided-by
                              :link/reason reason}
                       validation (assoc :link/validation validation))
               persisted (store/append-entry! entry)]
           {:ok true :link-id (:link/id persisted)}))))))

(defn get-link [link-id]
  (get-in (store/state) [:links link-id]))

(defn list-links
  ([] (list-links {}))
  ([{:keys [status artifact-id]}]
   (let [links (-> (store/state) :links vals)]
     (->> links
          (filter (fn [link]
                    (and (if status
                           (= status (:link/status link))
                           true)
                         (if artifact-id
                           (or (= artifact-id (get-in link [:link/from :artifact/id]))
                               (= artifact-id (get-in link [:link/to :artifact/id])))
                           true))))
          vec))))

(defn candidates []
  (list-links {:status :suggested}))
