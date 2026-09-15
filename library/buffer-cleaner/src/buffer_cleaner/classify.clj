;; classify.clj — PURE core of the buffer-cleaner adapter.
;; Input: a state packet (from adapter/dump-state.el via emacsclient,
;; parsed to EDN) + a wiring args map + the category parameter table.
;; Output: per-candidate classification, dry-run receipts, meters.
;; NO I/O here; the bb wrapper does emacsclient/JSON->EDN/file writes.
;; :execute is REFUSED with a typed refusal — live kill needs a gate
;; that does not exist (workarounds-forbidden).
(ns buffer-cleaner.classify
  (:require [clojure.edn :as edn]))

(defn- truthy? [x] (contains? #{true "true"} x))

(def hard-preservation
  "Rules that are hard, never parameterized (candidate-kind semantics)."
  #{:visible :has-process :modified :server-clients :active-agent
    :minibuf :scratch :arxana-browser})

(defn- prior-by-age
  [age-sec prior-map]
  (let [{:keys [by-age-seconds default]} (if (map? prior-map) prior-map
                                             {:default prior-map})
        by-age (or by-age-seconds {})
        applicable (->> by-age
                        (filter (fn [[k _]] (and (number? k) (>= age-sec k))))
                        (sort-by key)
                        last)]
    (if applicable (val applicable) (or default 0.05))))

(defn classify-buffer
  "One buffer row -> classification map. Pure."
  [{:keys [name kind file modified has-process visible display-age-seconds] :as row}
   wiring categories]
  (let [kind-k (if (and (= "file" kind)
                         (truthy? file)
                         (not (truthy? modified))
                         (number? display-age-seconds)
                         (>= display-age-seconds (:file-stale-age-seconds wiring 0)))
                   :file-stale (keyword kind))
        params (or (get categories kind-k) {:decisiveness :unknown})
        preserved (cond
                    (truthy? visible) :visible
                    (truthy? has-process) :has-process
                    (truthy? modified) :modified
                    (and (truthy? file) (not (contains? (:eligible-kinds wiring #{}) :file)))
                    :kind-not-eligible
                    :else nil)
        eligible (and (nil? preserved)
                      (contains? (:eligible-kinds wiring #{}) kind-k)
                      (not= :unknown (:decisiveness params)))
        prior (if (map? (:prior-needed-later params))
                (prior-by-age (or display-age-seconds -1)
                              (:prior-needed-later params))
                (:prior-needed-later params))]
    {:name name
     :kind kind-k
     :decisiveness (:decisiveness params)
     :prior-needed-later prior
     :revisit-cost (:revisit-cost params)
     :source (:source params)
     :preservation-reason preserved
     :action (if eligible :kill :keep)
     :expected-recovery (if eligible (* (or prior 0.0) (or (:revisit-cost params) 0.0)) 0.0)
     :row row}))

(defn classify-packet
  "Whole packet under one wiring. Pure."
  [packet wiring categories]
  (let [rows (:buffers packet)
        classified (mapv #(classify-buffer % wiring categories) rows)
        kills (filterv #(= :kill (:action %)) classified)
        scans (count rows)
        {:keys [per-scan per-kill]} (:fuel categories)]
    {:wiring (:wiring/id wiring)
     :classified classified
     :receipts {:kills (mapv #(select-keys % [:name :kind :decisiveness
                                              :prior-needed-later :revisit-cost
                                              :source :expected-recovery])
                             kills)
                :dry-run :dry-run
                :execute :refused-until-gated}
     :meters {:scanned scans
              :kills-proposed (count kills)
              :remaining (- scans (count kills))
              :expected-recovery (reduce + (map :expected-recovery kills))
              :fuel-charged (+ (* (or per-scan 0.01) scans)
                               (* (or per-kill 0.1) (count kills)))}
     :sources (->> classified (map :source) distinct vec)}))

(defn execute!
  "Typed refusal: live kill is not gated. Never performs anything."
  [_receipts]
  (throw (ex-info "live kill refused: gate does not exist"
                  {:record/type :buffer-cleaner/refusal
                   :reason :execute-not-gated
                   :gate-required "reviewed consumer wiring receipts to live kills"})))

(defn load-edn [path] (edn/read-string (slurp path)))
