(ns f12-preparation
  "Contextual instance preparation adapter; diagnostic build, no primary CLI."
  (:require [find-organise :as fo]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]))

(def ids
  [["apparatus/one-authority-per-question" "find-source-pin"]
   ["apparatus/one-authority-per-question" "risk-binding-pin"]
   ["apparatus/evidence-to-disposition-once" "preparation-integrity"]
   ["apparatus/pin-moves-with-the-population" "supporting"]
   ["apparatus/replayable-not-precious" "supporting"]
   ["apparatus/done-is-observed-running" "supporting"]])
(def repository {:patterns (set ids) :stands-on {} :acyclic? true})
(def enum #{:both-match :find-mismatch :risk-mismatch :both-mismatch :check-failed :refused-basis})
(def summaries
  {:both-match "Both byte identities match; broader readiness unknown."
   :find-mismatch "Find source differs; dependent acceptance blocked."
   :risk-mismatch "Risk binding differs; dependent acceptance blocked."
   :both-mismatch "Both identities differ; dependent acceptance blocked."
   :check-failed "Check failed; no acceptance evidence."
   :refused-basis "Basis refused; no primary comparison."})
(def scores {:both-match 1 :find-mismatch 1 :risk-mismatch 1 :both-mismatch 1
             :check-failed 0 :refused-basis nil})
(defn exhaustive! [values consumers]
  (assert (every? #(= values (set (keys %))) consumers) "non-total consumer"))
(defn disposition [f s]
  (case [(:result f) (:result s)]
    ["match" "match"] :both-match
    ["mismatch" "match"] :find-mismatch
    ["match" "mismatch"] :risk-mismatch
    ["mismatch" "mismatch"] :both-mismatch
    :check-failed))

(defn authority-bindings! [manifest]
  (let [sources (:sources manifest)
        f (edn/read-string (slurp (str "checks/" (:copy (nth sources 1)))))
        s (edn/read-string (slurp (str "checks/" (:copy (nth sources 3)))))
        expected-f (:sha256 (first (filter #(= "checks/find_organise.clj" (:file %))
                                         (get-in f [:source-basis :pins]))))]
    (assert (= expected-f (get-in manifest [:actions :F :expected])))
    (assert (= (:binding-proof-sha256 s) (get-in manifest [:actions :S :expected])))))

(defn cascade [ranks]
  (let [out (fo/organise {:id :preparation :closure :selected-only :precedence ranks}
                         (set ids) repository)
        row (assoc out :stands-on (:stands-on repository))]
    (assert (= (:selected row) (set ids)))
    (assert (= (:stands-on row) (:stands-on repository)))
    (assert (= #{} (:admitted-by row)))
    (assert (every? #(% row) [fo/o1-nodes-recorded fo/o2-authored-reachability fo/o3-fast-forward]))
    row))

(defn command! [manifest name]
  (let [r (p/shell {:out :string :err :string :continue true}
                   "/usr/bin/python3" "-I" "-S" "-B" "checks/f12_preparation_io.py" "diagnostic" manifest name)]
    (when-not (zero? (:exit r)) (throw (ex-info "actuator refused" {:process r})))
    (let [receipt (json/parse-string (:out r) true)]
      (when-not (:basis_stable receipt)
        (throw (ex-info "basis drift after command" {:receipt receipt})))
      receipt)))

(defn diagnostic-arm [manifest-path arm actuator]
  (exhaustive! enum [summaries scores])
  (let [manifest (json/parse-string (slurp manifest-path) true)
        _ (authority-bindings! manifest)
        _ (assert (= ids (:instances manifest)))
        _ (assert (= [] (:authored_stands_on manifest) (:admissions manifest)))
        ranks (get manifest arm)
        _ (assert (contains? #{:baseline :intervention} arm))
        c (cascade ranks)
        priorities (zipmap ids ranks)
        rules [{:id (ids 0) :if (constantly true) :however #(not (contains? % :F))
                :then (fn [_] {:key :F :receipt (actuator manifest-path "F")})}
               {:id (ids 1) :if (constantly true) :however #(not (contains? % :S))
                :then (fn [_] {:key :S :receipt (actuator manifest-path "S")})}
               {:id (ids 2) :if #(and (contains? % :F) (contains? % :S))
                :however #(not (contains? % :disposition))
                :then (fn [s] {:key :disposition :receipt (disposition (:F s) (:S s))})}]]
    (loop [state {} transcript []]
      (cond
        (contains? state :disposition)
        {:kind :diagnostic :primary? false :primary-score nil :cascade c
         :state state :transcript transcript :acting-order (mapv :rule transcript)
         :parent-order (mapv (comp first :rule) transcript)
         :summary (summaries (:disposition state))}
        (>= (count transcript) 3) (throw (ex-info "firing budget exceeded" {}))
        :else
        (let [[rule emission] (try (fo/fire rules #(priorities (:id %)) state)
                                  (catch Exception e
                                    (throw (ex-info "attempt refused; partial evidence retained"
                                                    {:state state :transcript transcript
                                                     :cause (ex-data e)} e))))]
          (when-not rule (throw (ex-info "no enabled rule" {})))
          (recur (assoc state (:key emission) (:receipt emission))
                 (conj transcript {:rule (:id rule) :emission emission})))))))

(defn sha256-file [path]
  (let [d (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1
                      (.digest d (java.nio.file.Files/readAllBytes
                                  (.toPath (java.io.File. path))))))))

(defn reviewed-pair
  "No CLI auto-run. Requires a separately authored primary approval bound to bytes."
  [manifest-path review-path]
  (let [review (edn/read-string (slurp review-path))
        _ (assert (= "codex-17" (:reviewer review)))
        _ (assert (true? (:approved-primary? review)))
        _ (assert (= (sha256-file manifest-path) (:manifest-sha256 review)))
        ;; The same execution path as diagnostic tests; episode classification
        ;; is supplied by this independently approved caller, not the actuator.
        b (diagnostic-arm manifest-path :baseline command!)
        i (diagnostic-arm manifest-path :intervention command!)
        _ (assert (= (sha256-file manifest-path) (:manifest-sha256 review)))
        primary (fn [episode]
                  (assert (= 3 (count (:transcript episode))))
                  (assert (not= :check-failed (get-in episode [:state :disposition])))
                  (assoc episode :kind :primary :primary? true
                         :primary-score (scores (get-in episode [:state :disposition]))))
        before (primary b)
        after (primary i)
        row {:precedence-before (get-in b [:cascade :precedence])
             :precedence-after (get-in i [:cascade :precedence])
             :acting-order-before (:acting-order b) :acting-order-after (:acting-order i)
             :score-before (:primary-score before) :score-after (:primary-score after)}]
    {:review review :before before :after after :row row
     :instance-o4 (fo/o4-precedence-governance row)
     :parent-o4 (fo/o4-precedence-governance
                  (assoc row :acting-order-before (:parent-order b)
                             :acting-order-after (:parent-order i)))
     :closure-claimed? false}))
