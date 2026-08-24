(ns futon3.inbox-zero.escalation
  "Pure sensitivity screening and WHO-CAN-ACT escalation routing.

  Joe's 2026-08-24 policy orders tiers by who can act: ordinary work is
  auto-pushed upstream; held work goes first to its responsible seat, then to
  the street sweeper when that seat is unavailable, and reaches the operator
  only for judgement holds ('holding on your word'). Volume alone never routes
  to the operator. This namespace classifies data; it performs no delivery or
  repository operations."
  (:require [clojure.string :as str]))

(def default-rules
  "Kind-level sensitivity rules. Patterns describe common shapes rather than
  enumerating filenames from any particular project."
  [{:rule/kind :key-material
    :match {:path-patterns
            ["(?i)(^|/)(id_(rsa|dsa|ecdsa|ed25519)(\\.pub)?|[^/]+\\.(pem|key|p12|pfx))$"]}}
   {:rule/kind :credential-file
    :match {:path-patterns
            ["(?i)(^|/)(\\.env([.-][^/]*)?|[^/]*(credentials?|secrets?|tokens?)[^/]*)$"]}}
   {:rule/kind :personal-data
    :match {:path-patterns
            ["(?i)(^|/)[^/]*(contacts?|address[-_]?book|personal[-_]?data)[^/]*\\.(csv|json|vcf|xlsx?)$"]}}
   {:rule/kind :large-binary
    :match {:path-patterns
            ["(?i)\\.(zip|tar|tgz|gz|7z|pdf|png|jpe?g|gif|mp[34]|mov|avi|sqlite3?|bin)$"]
            :max-bytes 10485760}}])

(defn- path-matches? [path patterns]
  (or (empty? patterns)
      (some #(re-find (re-pattern %) path) patterns)))

(defn- rule-hit? [entry rule]
  (let [{:keys [path-patterns max-bytes]} (:match rule)]
    (and (path-matches? (:path entry) path-patterns)
         (if (some? max-bytes)
           (and (number? (:size entry)) (> (:size entry) max-bytes))
           true))))

(defn screen-sensitivity
  "Return PLAN unchanged, or a held plan carrying every sensitivity hit.

  RULES are data maps with a :rule/kind and :match containing regex strings in
  :path-patterns and optionally :max-bytes. Size rules never trigger when an
  include entry lacks :size."
  [plan rules]
  (let [hits (->> (:include plan)
                  (mapcat (fn [entry]
                            (for [rule rules :when (rule-hit? entry rule)]
                              {:path (:path entry) :rule/kind (:rule/kind rule)})))
                  vec)]
    (if (seq hits)
      (assoc plan
             :verdict :held
             :held/reason :sensitive-content
             :sensitive/hits hits)
      plan)))

(defn- item-reason [item]
  (or (:held/reason item) (:escalate/reason item)))

(defn- responsible-seat [item]
  (or (:seat/id item) (get-in item [:plan :seat/id])))

(defn- repo-label [item]
  (or (:repo/id item)
      (get-in item [:plan :repo/id])
      (some-> (:repo-root item) (str/split #"/") last)
      "repository"))

(defn- route-message [item reason]
  (cond
    (= :sensitive-content reason)
    (let [{:keys [path rule/kind]} (first (:sensitive/hits item))]
      (format "%s matched %s — publish to public remote? Holding on your word."
              path kind))

    (true? (:needs-operator item))
    (format "%s: judgement required (%s) — Holding on your word."
            (repo-label item) reason)

    (= :ahead-outlier reason)
    (format "%s: %s ahead (threshold %s) — review and push or explain"
            (repo-label item) (:ahead-count item) (:ahead-threshold item))

    (= :push-failed reason)
    (format "%s: push failed — review the remote state and resolve"
            (repo-label item))

    :else
    (format "%s: promotion held (%s) — review and resolve"
            (repo-label item) reason)))

(defn route
  "Return one deterministic routing decision per item, preserving order.

  Sensitive content and explicit :needs-operator items always route to tier 3.
  All other items use the responsible live seat at tier 1, falling back to the
  configured street sweeper at tier 2. Volume and ahead count never select the
  operator."
  [items {:keys [live-seats sweeper-recipient operator-recipient]
          :or {live-seats #{}
               sweeper-recipient "street-sweeper"
               operator-recipient "joe"}}]
  (mapv
   (fn [item]
     (let [reason (item-reason item)
           seat (responsible-seat item)
           operator? (or (= :sensitive-content reason)
                         (true? (:needs-operator item)))
           [tier recipient]
           (cond
             operator? [3 operator-recipient]
             (and seat (contains? live-seats seat)) [1 seat]
             :else [2 sweeper-recipient])]
       {:route/tier tier
        :route/recipient recipient
        :route/item item
        :route/reason reason
        :route/message (route-message item reason)}))
   items))
