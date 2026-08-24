(ns futon3.inbox-zero.promotion
  "Pure turn-end promotion planning over inbox-zero projections.

  One plan is emitted for every repo/worktree with current dirt, including a
  loud held plan when all of that dirt is ambiguous, unattributed, or belongs
  to other seats. Silent holds are how long-lived dirty slices disappear from
  view. A globally clean state yields no plans because no worktree tuple exists
  to plan. This namespace performs no Git calls, IO, gating, or execution."
  (:require [futon3.inbox-zero.projection :as projection]))

(defn plan-promotion
  "Return deterministic promotion plans for SEAT-ID from inbox-zero STATE.

  Current dirty observations are partitioned per repo/worktree. A path is
  includable only when exactly one current active claim names SEAT-ID; every
  other dirty path remains visible with a fail-closed exclusion reason."
  [state seat-id now]
  (let [current-observations (projection/current-observations state)
        active-claims-by-path
        (->> (projection/current-claims state)
             vals
             (filter #(= :active (:state %)))
             (group-by (juxt :worktree/id :path)))
        dirty-by-scope
        (->> current-observations
             vals
             (filter #(projection/dirty-statuses (:git/status %)))
             (group-by (juxt :repo/id :worktree/id)))]
    (->> dirty-by-scope
         (map
          (fn [[[repo-id worktree-id] observations]]
            (let [members
                  (mapv
                   (fn [observation]
                     (let [path (:path observation)
                           claims (vec (get active-claims-by-path
                                            [worktree-id path] []))]
                       (cond
                         (empty? claims)
                         [:exclude {:path path :reason :unattributed}]

                         (> (count claims) 1)
                         [:exclude {:path path :reason :ambiguous}]

                         (= seat-id (:seat/id (first claims)))
                         [:include {:path path
                                    :git/status (:git/status observation)
                                    :claim/id (:claim/id (first claims))}]

                         :else
                         [:exclude {:path path :reason :other-seat}])))
                   (sort-by :path observations))
                  include (mapv second (filter #(= :include (first %)) members))
                  exclude (mapv second (filter #(= :exclude (first %)) members))
                  proposed? (seq include)]
              {:record/type :inbox-zero/promotion-plan
               :seat/id seat-id
               :repo/id repo-id
               :worktree/id worktree-id
               :computed-at now
               :include include
               :exclude exclude
               :verdict (if proposed? :proposed :held)
               :held/reason (when-not proposed? :nothing-promotable)})))
         (sort-by (juxt :repo/id :worktree/id))
         vec)))
