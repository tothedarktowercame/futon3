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
   :inbox-zero/session-file-claim :claim/id
   :inbox-zero/commit-scan-cursor :cursor/id
   :inbox-zero/commit-observation :commit-observation/id
   :inbox-zero/session-commit-link :link/id})

(def required-keys
  {:inbox-zero/session-seat
   [:seat/id :agent/id :session/id :surface :host/id :workspace/root
    :observed-at :registry-witness]

   :inbox-zero/file-observation
   [:observation/id :repo/id :repo/root :worktree/id :path :git/status
    :head/sha :observed-at :source]

   :inbox-zero/session-file-claim
   [:claim/id :seat/id :repo/id :worktree/id :path :relation :witness/type
    :witness/id :first-observed-at :last-observed-at :state]

   :inbox-zero/commit-scan-cursor
   [:cursor/id :worktree/id :cursor/sha :cursor/reason :prior/cursor-id
    :observed-at]

   :inbox-zero/commit-observation
   [:commit-observation/id :repo/id :worktree/id :commit/sha :change/id
    :parents :paths :authored-at :observed-at :source]

   :inbox-zero/session-commit-link
   [:link/id :seat/id :commit-observation/id :paths :coverage :basis
    :linked-at]})

(def git-statuses #{:untracked :modified :deleted :renamed :clean :ignored})
(def claim-states #{:active :superseded :released})
(def cursor-reasons #{:baseline :advance :rebaseline-rewrite})
(def link-coverages #{:partial :complete})

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

      :inbox-zero/commit-scan-cursor
      (do
        (doseq [k [:worktree/id :cursor/sha]]
          (when-not (non-blank-string? (get record k))
            (fail! "Commit cursor identity fields must be non-blank strings"
                   {:key k :value (get record k)})))
        (when-not (cursor-reasons (:cursor/reason record))
          (fail! "Unknown commit cursor reason" {:cursor/reason (:cursor/reason record)}))
        (if (= :baseline (:cursor/reason record))
          (when (some? (:prior/cursor-id record))
            (fail! "A baseline cursor cannot name a prior cursor"
                   {:prior/cursor-id (:prior/cursor-id record)}))
          (when-not (non-blank-string? (:prior/cursor-id record))
            (fail! "A moved cursor must name its prior cursor"
                   {:cursor/reason (:cursor/reason record)}))))

      :inbox-zero/commit-observation
      (do
        (doseq [k [:repo/id :worktree/id :commit/sha]]
          (when-not (non-blank-string? (get record k))
            (fail! "Commit-observation identity fields must be non-blank strings"
                   {:key k :value (get record k)})))
        (when-not (and (vector? (:parents record))
                       (every? non-blank-string? (:parents record))
                       (vector? (:paths record))
                       (every? non-blank-string? (:paths record)))
          (fail! "Commit parents and paths must be vectors of non-blank strings"
                 {:parents (:parents record) :paths (:paths record)}))
        (when-not (= :git (:source record))
          (fail! "Commit observation source must be :git" {:source (:source record)})))

      :inbox-zero/session-commit-link
      (do
        (doseq [k [:seat/id :commit-observation/id]]
          (when-not (non-blank-string? (get record k))
            (fail! "Session-commit link identity fields must be non-blank strings"
                   {:key k :value (get record k)})))
        (when-not (and (vector? (:paths record))
                       (every? #(and (non-blank-string? (:path %))
                                     (non-blank-string? (:claim/id %)))
                               (:paths record)))
          (fail! "Session-commit link paths must name paths and claims"
                 {:paths (:paths record)}))
        (when-not (link-coverages (:coverage record))
          (fail! "Unknown session-commit coverage" {:coverage (:coverage record)}))
        (when-not (= :path-claim-intersection (:basis record))
          (fail! "Session-commit links require path-claim-intersection evidence"
                 {:basis (:basis record)})))
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

      (and (= :inbox-zero/commit-scan-cursor (:record/type record))
           (:prior/cursor-id record)
           (not= :inbox-zero/commit-scan-cursor
                 (get-in state [:records (:prior/cursor-id record) :record/type])))
      (throw (ex-info "Commit cursor references an unknown prior cursor"
                      {:error/type :inbox-zero/unknown-prior-cursor
                       :cursor/id id :prior/cursor-id (:prior/cursor-id record)}))

      (and (= :inbox-zero/commit-scan-cursor (:record/type record))
           (:prior/cursor-id record)
           (not= (:worktree/id record)
                 (get-in state [:records (:prior/cursor-id record) :worktree/id])))
      (throw (ex-info "Commit cursor crosses worktrees"
                      {:error/type :inbox-zero/cursor-worktree-mismatch
                       :cursor/id id :prior/cursor-id (:prior/cursor-id record)}))

      (and (= :inbox-zero/commit-scan-cursor (:record/type record))
           (or (and (nil? (:prior/cursor-id record))
                    (some #(and (= :inbox-zero/commit-scan-cursor (:record/type %))
                                (= (:worktree/id record) (:worktree/id %)))
                          (vals (:records state))))
               (and (:prior/cursor-id record)
                    (some #(= (:prior/cursor-id record) (:prior/cursor-id %))
                          (filter (comp #{:inbox-zero/commit-scan-cursor} :record/type)
                                  (vals (:records state)))))))
      (throw (ex-info "Commit cursor chain would fork"
                      {:error/type :inbox-zero/cursor-chain-corrupt
                       :cursor/id id :prior/cursor-id (:prior/cursor-id record)}))

      (and (= :inbox-zero/session-commit-link (:record/type record))
           (not= :inbox-zero/session-seat
                 (get-in state [:records (:seat/id record) :record/type])))
      (throw (ex-info "Session-commit link references an unknown seat"
                      {:error/type :inbox-zero/unknown-seat
                       :link/id id :seat/id (:seat/id record)}))

      (and (= :inbox-zero/session-commit-link (:record/type record))
           (not= :inbox-zero/commit-observation
                 (get-in state [:records (:commit-observation/id record) :record/type])))
      (throw (ex-info "Session-commit link references an unknown observation"
                      {:error/type :inbox-zero/unknown-commit-observation
                       :link/id id
                       :commit-observation/id (:commit-observation/id record)}))

      (and (= :inbox-zero/session-commit-link (:record/type record))
           (some (fn [path-claim]
                   (let [path (:path path-claim)
                         claim (get-in state [:records (:claim/id path-claim)])
                         observation (get-in state
                                             [:records (:commit-observation/id record)])]
                     (not (and (= :inbox-zero/session-file-claim (:record/type claim))
                               (= (:seat/id record) (:seat/id claim))
                               (= (:worktree/id observation) (:worktree/id claim))
                               (= path (:path claim))
                               (some #{path} (:paths observation))))))
                 (:paths record)))
      (throw (ex-info "Session-commit link path references an inconsistent claim"
                      {:error/type :inbox-zero/invalid-link-claim :link/id id}))

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
                          :inbox-zero/session-file-claim 2
                          :inbox-zero/commit-scan-cursor 3
                          :inbox-zero/commit-observation 4
                          :inbox-zero/session-commit-link 5}
              records (:records state)
              cursor-depth
              (fn cursor-depth [cursor seen]
                (let [id (:cursor/id cursor)]
                  (when (seen id)
                    (throw (ex-info "Commit cursor chain contains a cycle"
                                    {:error/type :inbox-zero/cursor-chain-corrupt
                                     :cursor/id id})))
                  (if-let [prior-id (:prior/cursor-id cursor)]
                    (let [prior (get records prior-id)]
                      (when-not (= :inbox-zero/commit-scan-cursor
                                   (:record/type prior))
                        (throw (ex-info "Commit cursor references an unknown prior cursor"
                                        {:error/type :inbox-zero/unknown-prior-cursor
                                         :cursor/id id :prior/cursor-id prior-id})))
                      (inc (cursor-depth prior (conj seen id))))
                    0)))
              order-key (fn [record]
                          [(get type-order (:record/type record) 99)
                           (if (= :inbox-zero/commit-scan-cursor (:record/type record))
                             (cursor-depth record #{})
                             0)
                           (record-id record)])
              replayed (replay (sort-by order-key (vals records)))]
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
