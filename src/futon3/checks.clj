(ns futon3.checks
  "Lightweight check DSL to keep pattern canon + proof trails honest until full adapters ship."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [f0.clock :as clock]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.util UUID)))

(def default-log-path "futon3/logs/checks.edn")

(defonce ^:private log-path (atom default-log-path))
(def ^:private lock (Object.))
(def ^:private catalog (atom nil))

(def ^:private aif-observation-vector-schema
  [:map {:closed true}
   [:test-status {:optional true} [:or :string :keyword]]
   [:compile-status {:optional true} [:or :string :keyword]]
   [:diff-size {:optional true} :int]
   [:failing-spec-count {:optional true} :int]
   [:user-constraints {:optional true} [:vector :string]]
   [:time-since-anchor {:optional true} :double]
   [:contradiction-flags {:optional true} [:vector [:or :string :keyword]]]])

(def ^:private aif-precision-registry-schema
  [:map {:closed true}
   [:tests {:optional true} :double]
   [:typecheck {:optional true} :double]
   [:static-analysis {:optional true} :double]
   [:tool-output {:optional true} :double]
   [:user-constraints {:optional true} :double]
   [:model-inference {:optional true} :double]])

(def ^:private aif-term-channel-schema
  [:map {:closed true}
   [:observation-keys [:vector [:or :keyword :string]]]
   [:precision-channels [:vector [:or :keyword :string]]]])

(def ^:private aif-observation-coverage-schema
  [:map {:closed true}
   [:observed :int]
   [:total :int]
   [:coverage :double]])

(def ^:private aif-term-traceability-schema
  [:map {:closed true}
   [:terms [:vector [:or :keyword :string]]]
   [:with-provenance [:vector [:or :keyword :string]]]
   [:missing-provenance [:vector [:or :keyword :string]]]])

(def ^:private aif-evidence-schema
  [:map {:closed true}
   [:g-mean :double]
   [:tau-range [:tuple :double :double]]
   [:action-counts [:map-of :keyword :int]]
   [:observation-vector {:optional true} aif-observation-vector-schema]
   [:observation-coverage {:optional true} aif-observation-coverage-schema]
   [:precision-registry {:optional true} aif-precision-registry-schema]
   [:g-terms {:optional true} [:map-of :keyword :double]]
   [:g-term-channels {:optional true} [:map-of :keyword aif-term-channel-schema]]
   [:g-term-traceability {:optional true} aif-term-traceability-schema]
   [:constraint-violations {:optional true} [:vector :string]]])

(def ^:private request-schema
  [:map {:closed true}
   [:pattern/id [:string {:min 1}]]
   [:context [:string {:min 1}]]
   [:evidence {:optional true}
    [:maybe [:sequential any?]]]
   [:aif-trace {:optional true}
    [:maybe aif-evidence-schema]]
   [:sigils {:optional true}
    [:maybe [:sequential [:string {:min 1}]]]]
   [:prototypes {:optional true}
    [:maybe [:sequential [:string {:min 1}]]]]
   [:origin {:optional true}
    [:map {:closed false}
     [:source keyword?]
     [:client-id {:optional true} [:string {:min 1}]]
     [:workday/id {:optional true} [:string {:min 1}]]
     [:msg-id {:optional true} [:string {:min 1}]]
     [:sid {:optional true} [:string {:min 1}]]]]
   [:run-id {:optional true} [:string {:min 1}]]
   [:proof/id {:optional true} [:string {:min 1}]]
   [:proof/recorded {:optional true} [:string {:min 1}]]])

(defn set-log-path!
  "Override check log destination (tests)."
  [path]
  (reset! log-path path))

(defn current-log-path []
  @log-path)

(defn- ensure-log-file! []
  (let [file (io/file @log-path)
        parent (.getParentFile file)]
    (when parent (.mkdirs parent))
    file))

(defn- append-log! [entry]
  (locking lock
    (let [file (ensure-log-file!)]
      (spit file (str (pr-str entry) "\n") :append true)
      entry)))

(defn- random-proof-id []
  (str "PROOF-" (.substring (str (UUID/randomUUID)) 0 10)))

(defn- parse-hotwords [field]
  (->> (str/split (or field "") #"[,;]\s*")
       (map #(-> % str/lower-case str/trim))
       (remove str/blank?)
       vec))

(defn- parse-line [line]
  (when (and (not (str/blank? line))
             (not (str/starts-with? line "#")))
    (let [[pattern tokipona truth rationale hotwords] (str/split line #"\t")]
      (when pattern
        {:pattern/id pattern
         :pattern/title (or tokipona pattern)
         :pattern/hanzi truth
         :pattern/rationale rationale
         :pattern/hotwords (parse-hotwords hotwords)}))))

(defn- load-index []
  (let [file (io/file "resources/sigils/patterns-index.tsv")]
    (when (.exists file)
      (with-open [r (io/reader file)]
        (->> (line-seq r)
             (keep parse-line)
             (reduce (fn [acc entry]
                       (assoc acc (:pattern/id entry) entry))
                     {}))))))

(defn catalog!
  "Return (and cache) the parsed pattern catalog."
  []
  (or @catalog
      (reset! catalog (or (load-index) {}))))

(defn reload-catalog!
  []
  (reset! catalog (or (load-index) {})))

(defn- validate-request!
  [request]
  (if (m/validate request-schema request)
    request
    (let [details (me/humanize (m/explain request-schema request))]
      (throw (ex-info "invalid-request"
                      {:type :invalid-request
                       :details details})))))

(defn- blankish? [s]
  (or (nil? s)
      (and (string? s) (str/blank? s))))

(defn- normalized-text [context evidence]
  (->> (concat [(or context "")] (or evidence []))
       (map #(if (string? %) % (pr-str %)))
       (str/join " \n ")
       str/lower-case))

(defn- hit-set [hotwords text]
  (->> hotwords
       (keep (fn [hw]
               (when (and (seq hw)
                          (str/includes? text hw))
                 hw)))
       set))

(defn- similarity [hits total]
  (if (pos? total)
    (/ (double (count hits)) total)
    0.0))

(defn- determine-status [{:keys [hits evidence]}]
  (cond
    (and (seq hits) (seq evidence)) :applies
    (seq hits) :needs-evidence
    (seq evidence) :needs-alignment
    :else :needs-context))

(defn- derived-actions [status pattern]
  (case status
    :needs-evidence ["Attach at least one evidence link/EDN snippet that proves the pattern advanced."]
    :needs-alignment [(str "Rephrase the context so it explicitly references " (:pattern/title pattern)
                           " hotwords: " (str/join ", " (:pattern/hotwords pattern)))]
    :needs-context ["Describe the workday context (actors, obligation, prototype) before re-checking."]
    []))

(defn check!
  "Evaluate REQUEST {:pattern/id .. :context .. :evidence [...]}.
   Returns {:ok bool :status kw :proof map ...}."
  [{:keys [context evidence sigils prototypes origin run-id aif-trace]
    :proof/keys [id recorded]
    :as request
    pattern-id :pattern/id}]
  (cond
    (blankish? pattern-id)
    {:ok false :err "missing-pattern-id" :details {:request request}}

    (blankish? context)
    {:ok false :err "missing-context" :details {:pattern/id pattern-id}}

    :else
    (try
      (validate-request! request)
      (let [catalog (catalog!)
            pattern (get catalog pattern-id)]
        (if (nil? pattern)
          {:ok false :err "unknown-pattern" :details {:pattern/id pattern-id}}
          (let [text (normalized-text context evidence)
                hits (hit-set (:pattern/hotwords pattern) text)
                status (determine-status {:hits hits :evidence (seq evidence)})
                proof (cond-> {:proof/id (or id (random-proof-id))
                               :proof/run-id (or run-id (random-proof-id))
                               :proof/recorded (or recorded (clock/->iso-string))
                               :pattern/id pattern-id
                               :pattern/title (:pattern/title pattern)
                               :pattern/rationale (:pattern/rationale pattern)
                               :pattern/hotwords (:pattern/hotwords pattern)
                               :pattern/hanzi (:pattern/hanzi pattern)
                               :check/context context
                               :check/evidence (vec evidence)
                               :check/sigils (vec sigils)
                               :check/prototypes (vec prototypes)
                               :check/origin origin
                               :proof/status status
                               :proof/hits (vec (sort hits))
                               :proof/similarity (similarity hits (count (:pattern/hotwords pattern)))}
                        aif-trace (assoc :check/aif-trace aif-trace))
                missing (cond-> []
                          (= status :needs-evidence) (conj :evidence)
                          (= status :needs-context) (conj :context))
                derived (derived-actions status pattern)
                logged (append-log! proof)]
            {:ok true
             :status status
             :missing missing
             :derived/tasks derived
             :proof logged
             :log-path (current-log-path)})))
      (catch clojure.lang.ExceptionInfo ex
        (let [{:keys [type details]} (ex-data ex)]
          (if (= :invalid-request type)
            {:ok false :err "invalid-request" :details details}
            (throw ex)))))))
