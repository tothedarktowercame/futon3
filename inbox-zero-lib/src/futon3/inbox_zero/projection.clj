(ns futon3.inbox-zero.projection
  "Pure inbox-zero association, dirty-set, and notification eligibility views."
  (:require [futon3.inbox-zero.state :as state])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Date]))

(def default-policy
  {:dirty-count-threshold 5
   :dirty-age-threshold-ms (* 24 60 60 1000)})

(def dirty-statuses #{:untracked :modified :deleted :renamed})

(defn- epoch-ms [x]
  (cond
    (instance? Date x) (.getTime ^Date x)
    (integer? x) x
    :else (throw (ex-info "Expected an instant or epoch milliseconds"
                          {:error/type :inbox-zero/invalid-time :value x}))))

(defn- latest-by [time-key records]
  (last (sort-by (juxt #(epoch-ms (get % time-key)) state/record-id) records)))

(defn current-observations
  "Return the latest file observation keyed by [worktree-id path]."
  [store]
  (->> (state/records-of-type store :inbox-zero/file-observation)
       (group-by (juxt :worktree/id :path))
       (reduce-kv (fn [result key observations]
                    (assoc result key (latest-by :observed-at observations)))
                  {})))

(defn current-claims
  "Return the latest claim per [worktree-id path seat-id].

  Claim history is immutable. A later :released or :superseded record for the
  same tuple makes the earlier active claim historical."
  [store]
  (->> (state/records-of-type store :inbox-zero/session-file-claim)
       (group-by (juxt :worktree/id :path :seat/id))
       (reduce-kv (fn [result key claims]
                    (assoc result key (latest-by :last-observed-at claims)))
                  {})))

(defn current-commit-cursors
  "Return the unique immutable cursor-chain head keyed by worktree id.

  Timestamps are not ordering evidence. Two unreferenced heads for one
  worktree are a corrupt fork and fail closed."
  [store]
  (->> (state/records-of-type store :inbox-zero/commit-scan-cursor)
       (group-by :worktree/id)
       (reduce-kv
        (fn [result worktree cursors]
          (let [referenced (into #{} (keep :prior/cursor-id) cursors)
                heads (vec (remove #(referenced (:cursor/id %)) cursors))]
            (when-not (= 1 (count heads))
              (throw (ex-info "Commit cursor chain has multiple or no heads"
                              {:error/type :inbox-zero/cursor-chain-corrupt
                               :worktree/id worktree
                               :head/ids (mapv :cursor/id heads)})))
            (assoc result worktree (first heads))))
        {})))

(declare sha-256)

(defn derive-session-commit-links
  "Derive immutable links for COMMIT-OBSERVATIONS from current active claims.

  Attribution is exclusively path-claim intersection. A link is complete only
  when every changed path has exactly one active claim and all those claims
  belong to the same seat; a multi-seat commit therefore yields partial links."
  [store commit-observations linked-at]
  (let [active-by-path (->> (current-claims store)
                            vals
                            (filter #(= :active (:state %)))
                            (group-by (juxt :worktree/id :path)))]
    (->> commit-observations
         (mapcat
          (fn [observation]
            (let [claims-per-path
                  (mapv (fn [path]
                          [path (vec (get active-by-path
                                          [(:worktree/id observation) path] []))])
                        (:paths observation))
                  unambiguous (for [[path claims] claims-per-path
                                    :when (= 1 (count claims))]
                                [path (first claims)])
                  by-seat (group-by (comp :seat/id second) unambiguous)
                  complete? (and (= (count (:paths observation))
                                    (count unambiguous))
                                 (= 1 (count by-seat)))]
              (for [[seat-id path-claims] by-seat
                    :let [paths (->> path-claims
                                     (map (fn [[path claim]]
                                            {:path path :claim/id (:claim/id claim)}))
                                     (sort-by :path)
                                     vec)
                          identity [(:commit-observation/id observation)
                                    seat-id paths]]]
                {:record/type :inbox-zero/session-commit-link
                 :link/id (str "session-commit-link:" (sha-256 identity))
                 :seat/id seat-id
                 :commit-observation/id (:commit-observation/id observation)
                 :paths paths
                 :coverage (if complete? :complete :partial)
                 :basis :path-claim-intersection
                 :linked-at linked-at}))))
         (sort-by (juxt :commit-observation/id :seat/id))
         vec)))

(defn- dirty-since
  [observations current]
  (let [current-ms (epoch-ms (:observed-at current))
        same-path (->> observations
                       (filter #(and (= (:worktree/id current) (:worktree/id %))
                                     (= (:path current) (:path %))
                                     (<= (epoch-ms (:observed-at %)) current-ms)))
                       (sort-by (juxt #(epoch-ms (:observed-at %)) state/record-id)))
        after-last-clean (->> same-path
                              reverse
                              (take-while #(dirty-statuses (:git/status %)))
                              reverse)]
    (:observed-at (first after-last-clean))))

(defn- cleaned-after?
  "True when OBSERVATIONS (all for the claim's worktree/path) include a clean
  observation later than the claim: the dirt the claim witnessed has since
  been committed, so the claim says nothing about dirt that appears after.

  Live case, 2026-08-26: a seat's tool-edit claim from 08-24 survived the
  08-25 commit that cleaned the file, and a different agent's shell edit on
  08-26 was then attributed to the first seat."
  [observations claim]
  (let [claimed-ms (epoch-ms (:last-observed-at claim))]
    (boolean (some #(and (not (dirty-statuses (:git/status %)))
                         (> (epoch-ms (:observed-at %)) claimed-ms))
                   observations))))

(defn project-dirty-sets
  "Project unambiguous per-seat/per-repo dirty sets from STORE.

  Returns `:dirty-sets`, plus explicit `:ambiguous` and `:unattributed` paths.
  A path is attributed only when exactly one seat has a current active claim
  that has not been cleaned since (see `cleaned-after?`)."
  [store computed-at]
  (let [observations (state/records-of-type store :inbox-zero/file-observation)
        observations-by-path (group-by (juxt :worktree/id :path) observations)
        current-observations* (current-observations store)
        active-claims-by-path
        (->> (current-claims store)
             vals
             (filter #(= :active (:state %)))
             (remove #(cleaned-after? (get observations-by-path
                                           [(:worktree/id %) (:path %)])
                                      %))
             (group-by (juxt :worktree/id :path)))
        result
        (reduce-kv
         (fn [acc path-key observation]
           (if-not (dirty-statuses (:git/status observation))
             acc
             (let [claims (vec (get active-claims-by-path path-key []))
                   base {:repo/id (:repo/id observation)
                         :worktree/id (:worktree/id observation)
                         :path (:path observation)
                         :observation/id (:observation/id observation)}]
               (case (count claims)
                 0 (update acc :unattributed conj base)
                 1 (let [claim (first claims)
                         member (assoc base
                                       :claim/id (:claim/id claim)
                                       :dirty-since (dirty-since observations observation))]
                     (update-in acc [:members [(:seat/id claim)
                                               (:repo/id observation)
                                               (:worktree/id observation)]]
                                (fnil conj []) member))
                 (update acc :ambiguous conj
                         (assoc base :seat/ids (->> claims (map :seat/id) sort vec)))))))
         {:members {} :ambiguous [] :unattributed []}
         current-observations*)
        dirty-sets
        (->> (:members result)
             (map (fn [[[seat-id repo-id worktree-id] members]]
                    (let [members (vec (sort-by :path members))]
                      {:record/type :inbox-zero/dirty-set
                       :seat/id seat-id
                       :repo/id repo-id
                       :worktree/id worktree-id
                       :members members
                       :count (count members)
                       :oldest-dirty-at (first (sort (map :dirty-since members)))
                       :computed-at computed-at})))
             (sort-by (juxt :seat/id :repo/id :worktree/id))
             vec)]
    {:dirty-sets dirty-sets
     :ambiguous (vec (sort-by (juxt :worktree/id :path) (:ambiguous result)))
     :unattributed (vec (sort-by (juxt :worktree/id :path) (:unattributed result)))}))

(defn- sha-256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn member-state-hash
  "Stable hash of the evidence-bearing dirty members, independent of scan time."
  [dirty-set]
  (->> (:members dirty-set)
       (map #(select-keys % [:path :observation/id :claim/id :dirty-since]))
       (sort-by :path)
       vec
       sha-256))

(defn eligibility
  "Return notification trigger and dedupe input, or nil when not yet eligible."
  ([dirty-set now]
   (eligibility dirty-set now default-policy))
  ([dirty-set now policy]
   (let [{:keys [dirty-count-threshold dirty-age-threshold-ms]}
         (merge default-policy policy)
         now-ms (epoch-ms now)
         oldest-ms (some-> (:oldest-dirty-at dirty-set) epoch-ms)
         count-trigger? (>= (:count dirty-set) dirty-count-threshold)
         age-trigger? (and oldest-ms
                           (>= (- now-ms oldest-ms) dirty-age-threshold-ms))
         trigger (cond
                   count-trigger? {:type :count :threshold dirty-count-threshold}
                   age-trigger? {:type :age :threshold-ms dirty-age-threshold-ms})]
     (when trigger
       {:trigger trigger
        :dedupe/key [(:seat/id dirty-set)
                     (:repo/id dirty-set)
                     (member-state-hash dirty-set)]}))))
