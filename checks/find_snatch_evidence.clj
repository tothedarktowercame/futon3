(ns find-snatch-evidence
  "Experimental, process-local evidence consumer. Never hot-load into a shared JVM."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [find-snatch :as finder]
            [playout-snatch :as snatch])
  (:import [java.security MessageDigest]))

(def manifest-path "checks/F11-find-comparison-manifest.edn")
(def evidence-path "checks/find-snatch-evidence.edn")
(def choices-path "checks/find-snatch-choice-evidence.edn")

(defn refuse! [finding data]
  (throw (ex-info (name finding) (assoc data :finding finding))))

(defn digest [algorithm bytes]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (MessageDigest/getInstance algorithm) bytes))))

(defn canonical-state [x]
  (cond
    (map? x) [:map (mapv (fn [[k v]] [(canonical-state k) (canonical-state v)])
                         (sort-by (comp pr-str key) x))]
    (set? x) [:set (vec (sort-by pr-str (map canonical-state x)))]
    (vector? x) [:vector (mapv canonical-state x)]
    (list? x) [:list (mapv canonical-state x)]
    (or (nil? x) (boolean? x) (string? x) (keyword? x) (symbol? x)
        (char? x) (number? x)) x
    :else (refuse! :unsupported-state-value {:class (str (class x))})))

(defn state-digest [state]
  (digest "SHA-256" (.getBytes (pr-str (canonical-state state)) "UTF-8")))

(defn read-manifest []
  ;; The run reads committed author/reviewer data, not an uncommitted edit
  ;; carrying an asserted :reviewed-and-pinned flag.
  (let [{:keys [exit out err]} (shell/sh "git" "show" (str "HEAD:" manifest-path))]
    (when-not (zero? exit) (refuse! :manifest-not-committed {:stderr err}))
    (when-not (= out (slurp manifest-path))
      (refuse! :manifest-working-copy-drift {}))
    (edn/read-string out)))

(defn validate-basis!
  "Check status before reading sources; return the exact verified source snapshot.
   Filesystem root is explicit for isolated refusal tests. No source pin is updated."
  [manifest root]
  (when-not (and (= :experimental/f11-find-comparison-manifest-v1 (:schema manifest))
                 (= :reviewed-and-pinned (:status manifest))
                 (get-in manifest [:F4-C-external-designation :review-pin]))
    (refuse! :manifest-not-reviewed {:status (:status manifest)}))
  (let [pins (get-in manifest [:source-basis :pins])
        files (mapv :file pins)
        pattern-files (set (filter #(str/starts-with? % "library/snatch/") files))
        actual-files (into #{} (map #(str "library/snatch/" (.getName %)))
                           (filter #(str/ends-with? (.getName %) ".flexiarg")
                                   (or (.listFiles (io/file root "library/snatch")) [])))]
    (when-not (and (= 24 (count pattern-files)) (= pattern-files actual-files)
                   (= (count files) (count (set files))))
      (refuse! :source-population-drift {:expected pattern-files :actual actual-files}))
    (let [sources
          (into {}
          (for [{:keys [file sha256 git-blob]} pins]
            (let [path (io/file root file)]
              (when-not (.isFile path)
                (refuse! :reviewed-successor-pin-required {:file file :reason :missing}))
              (let [bytes (java.nio.file.Files/readAllBytes (.toPath path))
                    actual (digest "SHA-256" bytes)
                    prefix (.getBytes (str "blob " (alength bytes) "\u0000") "UTF-8")
                    blob (digest "SHA-1" (byte-array (concat prefix bytes)))]
                (when-not (and (= sha256 actual) (= git-blob blob))
                  (refuse! :reviewed-successor-pin-required
                           {:file file :expected-sha256 sha256 :actual-sha256 actual
                            :expected-blob git-blob :actual-blob blob}))
                [file (String. bytes "UTF-8")]))))]
      (doseq [file ["checks/find_snatch_evidence.clj" "checks/find_snatch.clj"
                    "checks/find_organise.clj" "checks/playout_snatch.clj"]]
        (when-not (contains? sources file)
          (refuse! :reviewed-successor-pin-required {:file file :reason :missing-executable-pin})))
      sources)))

(defn query-as-of [manifest scenario state]
  {:scenario scenario :round (:round state) :state-digest (state-digest state)
   :source-pins (get-in manifest [:source-basis :pins])
   :manifest-digest (state-digest manifest)})

(defn interpretation [manifest id]
  (or (first (filter #(= id (:pattern %)) (:interpretation-table manifest)))
      (refuse! :missing-interpretation-row {:pattern id})))

(defn citation [manifest sources row kind]
  (let [{:keys [file lines text]} (get-in row [:authored-spans kind])
        [start end] lines
        source (get sources file)
        pin (first (filter #(= file (:file %)) (get-in manifest [:source-basis :pins])))]
    (when-not (and source pin (integer? start) (integer? end)
                   (<= 1 start end (count (str/split-lines source)))
                   (= text (str/join "\n" (subvec (vec (str/split-lines source))
                                                   (dec start) end))))
      (refuse! :citation-source-mismatch {:pattern (:pattern row) :kind kind}))
    {:constructor :patternText :pattern (:pattern row) :kind kind
     :file file :lines lines :text text :git-blob (:git-blob pin) :sha256 (:sha256 pin)}))

(defn receipt-extension [manifest sources as-of id evaluation]
  (let [row (interpretation manifest id)]
    {:evidence
     {:schema :experimental/f11-find-receipt-v1
      :primary-kind :if :route :structured-antecedent :as-of as-of
      :clauses (mapv (fn [{:keys [kind clause]}]
                       {:kind kind :clause clause
                        :evaluation (get evaluation kind)
                        :citation (citation manifest sources row kind)})
                     (:query-mappings row))}}))

(defn receipt-checks
  "Independent checks: attribution ignores citations, citation ignores clause labels.
   Query binding is separate from both. Controls will exercise these in packet 3."
  [manifest sources as-of id receipt]
  (let [row (interpretation manifest id)
        evidence (:evidence receipt)
        clauses (:clauses evidence)
        attribution (mapv #(select-keys % [:kind :clause]) clauses)
        expected (mapv #(select-keys % [:kind :clause]) (:query-mappings row))]
    {:presence (some? receipt)
     :clause-attribution (and (seq clauses) (= attribution expected))
     :citation (and (seq clauses)
                    (every? (fn [{:keys [kind] :as clause}]
                              (and (contains? (:authored-spans row) kind)
                                   (= (:citation clause) (citation manifest sources row kind)))) clauses))
     :query-binding (= as-of (:as-of evidence))
     :encoding (and (= :experimental/f11-find-receipt-v1 (:schema evidence))
                    (= :if (:primary-kind evidence))
                    (= :structured-antecedent (:route evidence) (:route receipt)))}))

(def candidate-statuses
  #{:selected :evaluated-excluded :other-grain :p2-out-of-domain :no-executable-interpretation})

(defn require-accounting! [manifest result]
  (let [population (set (map :pattern (:interpretation-table manifest)))
        declared (into {} (map (juxt :pattern :status)) (:interpretation-table manifest))
        evaluations (:evaluations result)
        selected (into #{} (map #(keyword "snatch" (name %))) (get-in result [:find :selected]))
        marked (into #{} (keep (fn [[id e]] (when (= :selected (:status e)) id))) evaluations)]
    (when-not (and (= 24 (count population)) (= population (set (keys evaluations)))
                   (= selected marked)
                   (every? (fn [[_ e]] (and (candidate-statuses (:status e))
                                            (= (:selected? e) (= :selected (:status e))))) evaluations)
                   (every? (fn [[id e]]
                             (if (= :p1-executable (get declared id))
                               (#{:selected :evaluated-excluded} (:status e))
                               (= (get declared id) (:status e)))) evaluations))
      (refuse! :candidate-accounting {:population population :selected selected :marked marked}))
    result))

(defn require-selection-equality! [observed legacy]
  (when-not (= (pr-str (:selected observed)) (pr-str (:selected legacy)))
    (refuse! :selection-mismatch {:observed (:selected observed) :legacy (:selected legacy)}))
  observed)

(defn require-expectations! [manifest scenario state found]
  (when-let [expected (first (filter #(and (= scenario (:scenario %))
                                          (= (:round state) (:round %)))
                                    (:g4-independent-expectations manifest)))]
    (when-not (and (= (:state-assertions expected)
                     (select-keys state (keys (:state-assertions expected))))
                   (not-any? #(contains? state %) (:absent-state-keys expected))
                   (= (set (map :pattern (:expected-selected-receipts expected)))
                      (set (map #(keyword "snatch" (name %)) (:selected found))))
                   (every? (fn [{:keys [pattern acknowledgments]}]
                             (= (mapv #(select-keys % [:kind :clause]) acknowledgments)
                                (mapv #(select-keys % [:kind :clause])
                                      (get-in found [:receipts (keyword (name pattern))
                                                     :evidence :clauses]))))
                           (:expected-selected-receipts expected)))
      (refuse! :independent-query-expectation {:scenario scenario :round (:round state)})))
  found)

(defn observe-query [manifest sources scenario state]
  (when-not (= :play (:grain state)) (refuse! :not-a-p1-play-query {}))
  (let [as-of (query-as-of manifest scenario state)
        result (finder/find-with-evidence
                state (partial receipt-extension manifest sources as-of))
        found (:find result)]
    (require-expectations! manifest scenario state found)
    (require-accounting! manifest result)
    ;; This is a compatibility assertion, not the source of expectations or receipts.
    (require-selection-equality! found (finder/find state))
    (doseq [local-id (:selected found)]
      (let [id (keyword "snatch" (name local-id))
            receipt (get-in found [:receipts local-id])
            checks (assoc (receipt-checks manifest sources as-of id receipt)
                          :evaluation
                          (every? (fn [{:keys [kind evaluation]}]
                                    (and (true? (:boolean evaluation))
                                         (= evaluation (get-in result [:evaluations id kind]))))
                                  (get-in receipt [:evidence :clauses])))]
        (when-not (every? true? (vals checks))
          (refuse! :receipt-content {:pattern id :checks checks}))))
    (assoc result :query as-of :state state
           :evaluations
           (into (sorted-map)
                 (map (fn [[id evaluation]]
                        (let [row (interpretation manifest id)]
                          [id (assoc evaluation :as-of as-of
                                     :interpretation-source (:predicate-source row)
                                     :authored-spans (:authored-spans row))])))
                 (:evaluations result)))))

(defn observing-policy [manifest sources scenario observations]
  (fn [state patterns]
    (swap! observations conj (observe-query manifest sources scenario state))
    (snatch/pi-patterns state patterns)))

(defn build-evidence
  "Fresh computation only after verified source pins. No writes in this function."
  [manifest root]
  (let [sources (validate-basis! manifest root)
        scenarios
        (mapv (fn [{:keys [scenario requested-rounds]}]
                (let [[treatment disposition] scenario
                      observations (atom [])
                      trace (snatch/play (observing-policy manifest sources scenario observations)
                                         treatment disposition requested-rounds)]
                  {:scenario scenario :requested-rounds requested-rounds
                   :queries @observations :trace trace}))
              (:scenarios manifest))]
    (validate-basis! manifest root)
    {:schema :experimental/f11-find-evidence-v1
     :status :pending-comparison-controls
     :manifest-digest (state-digest manifest) :scenarios scenarios}))

(defn -main [& _]
  (let [manifest (read-manifest)
        evidence (build-evidence manifest ".")]
    (spit evidence-path (with-out-str (pprint/pprint evidence)))
    (spit choices-path (with-out-str
                         (pprint/pprint
                          {:schema :experimental/f11-find-choice-evidence-v1
                           :status :pending-packet-3-controls
                           :choice-status (:choice-status manifest)})))
    (shutdown-agents)))
