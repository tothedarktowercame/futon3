(ns find-snatch
  "A structured-state `find` over the authored Snatch pattern repository."
  (:refer-clojure :exclude [find])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [playout-snatch :as snatch]))

(def library-dir "library/snatch")
(def cascade-path "checks/snatch-cascade.edn")
(def output-path "checks/find-snatch.edn")

(def scenario-order
  [[:g1 :snatcher] [:g1 :sharer] [:g1 :cautious]
   [:g4 :snatcher] [:g2 :snatcher] [:g5 :sharer]])

(def zero-mass-patterns
  {[:g1 :snatcher] :consult-the-remedy-before-exiting
   [:g1 :sharer] :consult-the-remedy-before-exiting
   [:g1 :cautious] :consult-the-remedy-before-exiting
   [:g2 :snatcher] :consult-the-remedy-before-exiting
   [:g4 :snatcher] :forced-play-needs-a-loss-floor
   [:g5 :sharer] :re-enter-after-observed-repair})

(defn- normalise [s]
  (some-> s str/trim (str/replace #"\s+" " ")))

(defn- clause-block [lines label]
  (when-let [marker (first (keep-indexed
                            (fn [i line]
                              (when (re-matches
                                     (re-pattern (str "^\\s+\\+ " label ":\\s*$"))
                                     line)
                                i))
                            lines))]
    (let [content (->> (subvec lines (inc marker))
                       (take-while #(and (not (str/blank? %))
                                         (not (re-matches #"^\s+\+ \S.*" %))))
                       vec)]
      (when (seq content)
        {:lines [(+ marker 2) (+ marker 1 (count content))]
         :text (normalise (str/join " " content))}))))

(defn- parse-pattern-file [file]
  (let [lines (vec (str/split-lines (slurp file)))
        id (some #(some-> (re-matches #"@flexiarg snatch/(\S+)" %) second keyword)
                 lines)
        if-clause (clause-block lines "IF")
        however-clause (clause-block lines "HOWEVER")]
    (when-not id
      (throw (ex-info "Snatch pattern has no @flexiarg id" {:file (str file)})))
    [id {:file (str "library/snatch/" (.getName ^java.io.File file))
         :if-lines (:lines if-clause)
         :if-text (:text if-clause)
         :however-lines (:lines however-clause)
         :however-text (:text however-clause)}]))

(def authored-patterns
  (into (sorted-map)
        (map parse-pattern-file)
        (->> (file-seq (io/file library-dir))
             (filter #(.isFile ^java.io.File %))
             (filter #(str/ends-with? (.getName ^java.io.File %) ".flexiarg"))
             (sort-by #(.getName ^java.io.File %)))))

(def repository (set (keys authored-patterns)))

(defn- warrant [id]
  (let [{:keys [file if-lines if-text however-lines however-text]}
        (get authored-patterns id)]
    (cond-> (sorted-map :file file :if-lines if-lines :if-text if-text)
      however-text (assoc :however-lines however-lines
                          :however-text however-text))))

(defn find
  "Evaluate authored P1 antecedents against a structured runner state."
  [state]
  (let [firing (->> snatch/collection
                    (filter #(snatch/fires? % state))
                    (map :id)
                    sort
                    vec)
        outside (set/difference (set firing) repository)]
    (when (or (seq outside) (some #{:no-pattern} firing))
      (throw (ex-info "F1 violated: find selected outside the repository"
                      {:selected firing :outside (sort outside)})))
    (sorted-map
     :absence (when (empty? firing) :no-pattern-addresses-this-tension)
     :receipts (into (sorted-map)
                     (map (fn [id]
                            (let [pat (some #(when (= id (:id %)) %) snatch/collection)]
                              [id (sorted-map
                                   :however (if (:however pat) true :none)
                                   :if true
                                   :route :structured-antecedent
                                   :state-fields :not-instrumented
                                   :warrant (warrant id))])))
                     firing)
     :selected firing)))

(defn- representation-mismatches
  "Reject a second maintained antecedent representation.  The authored file is
   the sole text; runner entries carry only executable interpretations keyed by
   the same pattern id."
  ([] (representation-mismatches snatch/collection))
  ([patterns]
  (->> patterns
       (mapcat (fn [{:keys [id] :as pattern}]
                 (keep identity
                       [(when-not (contains? authored-patterns id)
                          (sorted-map :finding :runner-id-not-authored :pattern id))
                        (when (or (contains? pattern :if-text)
                                  (contains? pattern :however-text))
                          (sorted-map :finding :duplicate-antecedent-text
                                      :pattern id))])))
       (sort-by (juxt :pattern :finding))
       vec)))

(defn- observe-scenario [{:keys [treatment disposition rounds acting]}]
  (let [observations (atom [])
        policy (fn [state patterns]
                 (swap! observations conj
                        (sorted-map :find (find state) :round (:round state)))
                 (snatch/pi-patterns state patterns))
        trace (snatch/play policy treatment disposition rounds)
        selected-union (into #{} (mapcat #(get-in % [:find :selected])) @observations)
        acting-set (disj (set acting) :no-pattern)
        zero-mass (get zero-mass-patterns [treatment disposition])
        violations (filter #(some #{zero-mass} (get-in % [:find :selected]))
                           @observations)
        numerator (count (set/intersection acting-set selected-union))
        denominator (count acting-set)]
    (when (seq violations)
      (throw (ex-info "F4 violated: declared zero-mass pattern was selected"
                      {:scenario [treatment disposition]
                       :pattern zero-mass
                       :rounds (mapv :round violations)})))
    (sorted-map
     :acting (vec (sort acting-set))
     :disposition disposition
     :f4 (sorted-map :holds true :zero-mass-pattern zero-mass)
     :recall (sorted-map :denominator denominator
                         :fraction (str numerator "/" denominator)
                         :numerator numerator)
     :round-results (vec @observations)
     :rounds-run (count trace)
     :selected-union (vec (sort selected-union))
     :treatment treatment)))

(defn- library-sha
  "The last commit that touched library/snatch — the repository the receipts
   cite. (HEAD would change with the commit that contains this report, so a
   re-run could never match the committed artefact.)"
  []
  (let [{:keys [exit out err]} (shell/sh "git" "log" "-1" "--format=%H" "--" library-dir)]
    (when-not (zero? exit)
      (throw (ex-info "Cannot determine library/snatch commit" {:stderr err})))
    (str/trim out)))

(defn- report []
  (let [rows (:scenarios (edn/read-string (slurp cascade-path)))
        rows (filter #(= :patterns (:policy %)) rows)
        by-key (into {} (map (juxt (juxt :treatment :disposition) identity)) rows)
        scenarios (mapv #(observe-scenario (get by-key %)) scenario-order)
        mismatches (representation-mismatches)]
    (sorted-map
     :as-of (library-sha)
     :drift (sorted-map :basis :authored-text-with-executable-interpretation
                        :mismatch-count (count mismatches)
                        :mismatches mismatches)
     :laws (sorted-map :F1 :asserted-selected-subset-of-repository
                       :F4 :asserted-declared-zero-mass-per-scenario)
     :repository (vec (sort repository))
     :scenarios scenarios)))

(defn require-zero-drift! [drift]
  (when (pos? (:mismatch-count drift 0))
    (throw (ex-info "find-snatch antecedent drift"
                    {:finding :antecedent-drift
                     :mismatch-count (:mismatch-count drift)
                     :mismatches (:mismatches drift)})))
  drift)

(defn require-find-laws! [result]
  (doseq [round (mapcat :round-results (:scenarios result))]
    (let [{:keys [selected receipts absence]} (:find round)
          outside (set/difference (set selected) repository)]
      (when (or (seq outside)
                (and (empty? selected)
                     (not= :no-pattern-addresses-this-tension absence)))
        (throw (ex-info "F1 containment/absence failed" {:finding :f1})))
      (when-not (every? #(contains? receipts %) selected)
        (throw (ex-info "F2 selected pattern lacks receipt" {:finding :f2})))
      (when-not (every? (fn [id]
                          (let [receipt (get receipts id)]
                            (and (not= :score-alone (:route receipt))
                                 (string? (get-in receipt [:warrant :file])))))
                        selected)
        (throw (ex-info "F3 receipt is self-certifying" {:finding :f3})))))
  result)

(defn mutate-law [result kind]
  (let [path [:scenarios 0 :round-results 0 :find]
        selected (get-in result (conj path :selected))
        id (first selected)]
    (case kind
      :f1 (update-in result (conj path :selected) conj :outside-repository)
      :f2 (update-in result (conj path :receipts) dissoc id)
      :f3 (assoc-in result (conj path :receipts id)
                    {:route :score-alone :score 1.0})
      result)))

(defn -main [& args]
  (let [negative-kind (cond (some #{"--negative-f1"} args) :f1
                            (some #{"--negative-f2"} args) :f2
                            (some #{"--negative-f3"} args) :f3)
        negative? (some #{"--negative"} args)]
    (if negative?
      ;; Inject the forbidden second textual representation.  This is the
      ;; exact defect the positive path now excludes, not malformed EDN.
      (try
        (require-zero-drift!
         (let [mutated (assoc (first snatch/collection)
                              :if-text "a second hand-maintained antecedent")
               mismatches (representation-mismatches
                           (assoc (vec snatch/collection) 0 mutated))]
           {:basis :authored-text-with-executable-interpretation
            :mismatch-count (count mismatches)
            :mismatches mismatches}))
        (println "find-snatch: FAIL negative drift mutation slipped exit-convention=0-pass/1-fail")
        (shutdown-agents)
        (System/exit 2)
        (catch clojure.lang.ExceptionInfo e
          (println (str "find-snatch: PASS negative drift mutation rejected"
                        " finding=" (name (:finding (ex-data e)))
                        " exit-convention=0-pass/1-fail"))
          (shutdown-agents)
          (System/exit 0)))
      (let [base (report)
            result (if negative-kind (mutate-law base negative-kind) base)]
        (try
          (require-zero-drift! (:drift result))
          (require-find-laws! result)
          (when negative-kind
            (println "find-snatch: FAIL law mutation slipped exit-convention=0-pass/1-fail/2-mutation-slipped")
            (shutdown-agents)
            (System/exit 2))
        (spit output-path (with-out-str (pprint/pprint result)))
        (doseq [{:keys [treatment disposition recall acting selected-union]}
                (:scenarios result)]
          (println (format "%s/%s recall %s acting=%s selected=%s"
                           (name treatment) (name disposition) (:fraction recall)
                           (pr-str acting) (pr-str selected-union))))
        (println (format "F4 %d/%d; drift mismatches %d; wrote %s"
                         (count (:scenarios result)) (count (:scenarios result))
                         (get-in result [:drift :mismatch-count]) output-path))
          (println "find-snatch: PASS exit-convention=0-pass/1-fail")
          (shutdown-agents)
          (System/exit 0)
          (catch clojure.lang.ExceptionInfo e
            (println (str "find-snatch: " (if negative-kind "PASS negative-control rejected " "FAIL finding=")
                          (name (:finding (ex-data e)))
                          " mismatches=" (:mismatch-count (ex-data e))
                          " exit-convention=0-pass/1-fail"))
            (shutdown-agents)
            (System/exit (if negative-kind 0 1))))))))
