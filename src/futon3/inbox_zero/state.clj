(ns futon3.inbox-zero.state
  "Durable v0 records for inbox-zero watcher observations and claims.

  The store is deliberately independent of watcher policy. It validates and
  persists witnessed facts; thresholding and delivery consume these facts in
  later layers. The watcher is the single snapshot writer; producers write the
  separate witness intake. Snapshot replacement is atomic."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(def schema-version 0)

;; One watcher process owns a state path. The monitor protects accidental
;; concurrent calls inside that process; external producers never write state.
(def ^:private write-monitor (Object.))

(def record-id-key
  {:inbox-zero/session-seat :seat/id
   :inbox-zero/file-observation :observation/id
   :inbox-zero/session-file-claim :claim/id})

(def required-keys
  {:inbox-zero/session-seat
   [:seat/id :agent/id :session/id :surface :host/id :workspace/root
    :observed-at :registry-witness]

   :inbox-zero/file-observation
   [:observation/id :repo/id :repo/root :worktree/id :path :git/status
    :head/sha :observed-at :source]

   :inbox-zero/session-file-claim
   [:claim/id :seat/id :repo/id :worktree/id :path :relation :witness/type
    :witness/id :first-observed-at :last-observed-at :state]})

(def git-statuses #{:untracked :modified :deleted :renamed :clean :ignored})
(def claim-states #{:active :superseded :released})

(defn empty-state []
  {:schema/version schema-version
   :records {}})

(defn- non-blank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn- fail! [message data]
  (throw (ex-info message (assoc data :error/type :inbox-zero/invalid-record))))

(defn validate-record
  "Return RECORD when it satisfies the v0 storage contract; otherwise throw.

  Referential checks that need accumulated state are performed by
  `apply-record`."
  [record]
  (when-not (map? record)
    (fail! "Inbox-zero record must be a map" {:record record}))
  (let [record-type (:record/type record)
        id-key (get record-id-key record-type)
        missing (vec (remove #(contains? record %) (get required-keys record-type [])))]
    (when-not id-key
      (fail! "Unknown inbox-zero record type" {:record/type record-type}))
    (when (seq missing)
      (fail! "Inbox-zero record is missing required keys"
             {:record/type record-type :missing missing}))
    (when-not (non-blank-string? (get record id-key))
      (fail! "Inbox-zero record id must be a non-blank string"
             {:record/type record-type :id-key id-key :value (get record id-key)}))
    (case record-type
      :inbox-zero/session-seat
      (do
        (doseq [k [:agent/id :session/id :host/id :workspace/root]]
          (when-not (non-blank-string? (get record k))
            (fail! "Session-seat identity fields must be non-blank strings"
                   {:key k :value (get record k)})))
        (when-not (= (:session/id record)
                     (get-in record [:registry-witness :session/id]))
          (fail! "Registry witness must match the seat session"
                 {:seat/session (:session/id record)
                  :witness/session (get-in record [:registry-witness :session/id])})))

      :inbox-zero/file-observation
      (do
        (doseq [k [:repo/id :repo/root :worktree/id :path :head/sha]]
          (when-not (non-blank-string? (get record k))
            (fail! "File-observation identity fields must be non-blank strings"
                   {:key k :value (get record k)})))
        (when-not (git-statuses (:git/status record))
          (fail! "Unknown git status" {:git/status (:git/status record)}))
        (when (and (= :deleted (:git/status record)) (:content/hash record))
          (fail! "Deleted observations cannot have a content hash"
                 {:content/hash (:content/hash record)})))

      :inbox-zero/session-file-claim
      (do
        (doseq [k [:seat/id :repo/id :worktree/id :path :witness/id]]
          (when-not (non-blank-string? (get record k))
            (fail! "Session-file claim identity fields must be non-blank strings"
                   {:key k :value (get record k)})))
        (when-not (claim-states (:state record))
          (fail! "Unknown claim state" {:state (:state record)})))
      nil)
    record))

(defn record-id [record]
  (get record (get record-id-key (:record/type record))))

(defn apply-record
  "Apply one immutable record to STATE.

  Replaying an identical id is idempotent. Reusing an id for different content
  is a provenance conflict and fails. Claims must reference a stored seat."
  [state record]
  (validate-record record)
  (when-not (= schema-version (:schema/version state))
    (throw (ex-info "Unsupported inbox-zero state schema"
                    {:error/type :inbox-zero/unsupported-schema
                     :expected schema-version
                     :actual (:schema/version state)})))
  (let [id (record-id record)
        existing (get-in state [:records id])]
    (cond
      (= existing record) state
      existing
      (throw (ex-info "Inbox-zero record id already has different content"
                      {:error/type :inbox-zero/id-conflict :record/id id}))

      (and (= :inbox-zero/session-file-claim (:record/type record))
           (not= :inbox-zero/session-seat
                 (get-in state [:records (:seat/id record) :record/type])))
      (throw (ex-info "Session-file claim references an unknown seat"
                      {:error/type :inbox-zero/unknown-seat
                       :claim/id id :seat/id (:seat/id record)}))

      :else (assoc-in state [:records id] record))))

(defn replay [records]
  (reduce apply-record (empty-state) records))

(defn records-of-type [state record-type]
  (->> (:records state)
       vals
       (filter #(= record-type (:record/type %)))
       (sort-by record-id)
       vec))

(defn load-state
  "Read PATH, returning an empty state only when the file does not exist.
  Invalid EDN and invalid schemas propagate: corruption must never look empty."
  [path]
  (let [file (io/file path)]
    (if-not (.exists file)
      (empty-state)
      (let [state (edn/read-string (slurp file))]
        (when-not (and (map? state) (map? (:records state)))
          (throw (ex-info "Malformed inbox-zero state"
                          {:error/type :inbox-zero/malformed-state :path (str path)})))
        (when-not (= schema-version (:schema/version state))
          (throw (ex-info "Unsupported inbox-zero state schema"
                          {:error/type :inbox-zero/unsupported-schema
                           :path (str path) :expected schema-version
                           :actual (:schema/version state)})))
        ;; Validate the snapshot as a replay so references and id keys cannot
        ;; become inconsistent through manual edits.
        (let [type-order {:inbox-zero/session-seat 0
                          :inbox-zero/file-observation 1
                          :inbox-zero/session-file-claim 2}
              replayed (replay (sort-by (juxt #(get type-order (:record/type %) 99)
                                              record-id)
                                        (vals (:records state))))]
          (when-not (= state replayed)
            (throw (ex-info "Inbox-zero state is not canonical"
                            {:error/type :inbox-zero/noncanonical-state
                             :path (str path)})))
          state)))))

(defn- atomic-write! [path value]
  (let [target (.toPath (io/file path))
        parent (or (.getParent target) (.toPath (io/file ".")))
        attrs (make-array FileAttribute 0)]
    (Files/createDirectories parent attrs)
    (let [tmp (Files/createTempFile parent ".inbox-zero-" ".edn" attrs)]
      (try
        (spit (.toFile tmp) (str (pr-str value) "\n"))
        (Files/move tmp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (finally
          (Files/deleteIfExists tmp))))))

(defn append-record!
  "Serialize, validate, and durably apply RECORD to the snapshot at PATH.
  Returns the resulting state."
  [path record]
  (locking write-monitor
    (let [current-state (load-state path)
          next-state (apply-record current-state record)]
      (when-not (= current-state next-state)
        (atomic-write! path next-state))
      next-state)))

(defn append-records!
  "Atomically validate and apply RECORDS as one state transition."
  [path records]
  (locking write-monitor
    (let [current-state (load-state path)
          next-state (reduce apply-record current-state records)]
      (when-not (= current-state next-state)
        (atomic-write! path next-state))
      next-state)))
