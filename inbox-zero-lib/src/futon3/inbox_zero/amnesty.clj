(ns futon3.inbox-zero.amnesty
  "Pure epoch-amnesty planning for dirty paths that predate attribution.

  This namespace only classifies projected evidence. It performs no file,
  repository, commit, or delivery operations."
  (:require [futon3.inbox-zero.escalation :as escalation]
            [futon3.inbox-zero.projection :as projection]))

(defn- exemption-key
  [entry]
  (select-keys entry [:repo/id :path]))

(defn- sensitivity-hit
  [entry]
  (-> {:include [entry]}
      (escalation/screen-sensitivity escalation/default-rules)
      :sensitive/hits
      first))

(defn- plan-group
  [now exempt-keys entries]
  (let [partitioned
        (reduce
         (fn [result entry]
           (cond
             (contains? exempt-keys (exemption-key entry))
             (update result :exempt conj (:path entry))

             :else
             (if-let [hit (sensitivity-hit entry)]
               (update result :sensitive conj hit)
               (update result :baseline conj (:path entry)))))
         {:exempt [] :sensitive [] :baseline []}
         entries)
        exempt (vec (sort (:exempt partitioned)))
        sensitive (vec (sort-by (juxt :path :rule/kind)
                                (:sensitive partitioned)))
        baseline (vec (sort (:baseline partitioned)))
        first-entry (first entries)]
    {:record/type :inbox-zero/amnesty-plan
     :repo/id (:repo/id first-entry)
     :worktree/id (:worktree/id first-entry)
     :computed-at now
     :exempt exempt
     :sensitive sensitive
     :baseline baseline
     :counts {:exempt (count exempt)
              :sensitive (count sensitive)
              :baseline (count baseline)}}))

(defn plan-amnesty
  "Partition currently unattributed dirty paths into exemption, sensitivity,
  and future-baseline groups.

  EXEMPT contains maps naming `:repo/id` and `:path`. Exemption takes
  precedence over sensitivity so a deliberate tracer remains dirty even if a
  future general sensitivity rule would otherwise match it."
  [state {:keys [exempt now] :or {exempt #{}}}]
  (let [exempt-keys (set (map exemption-key exempt))
        unattributed (:unattributed
                      (projection/project-dirty-sets state now))]
    (->> unattributed
         (group-by (juxt :repo/id :worktree/id))
         (map (fn [[_ entries]] (plan-group now exempt-keys entries)))
         (sort-by (juxt :repo/id :worktree/id))
         vec)))
