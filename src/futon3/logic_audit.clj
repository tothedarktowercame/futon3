(ns futon3.logic-audit
  "Log and summarize logic checks for debugging usage across subsystems."
  (:require [clojure.java.io :as io]
            [f0.clock :as clock]))

(def default-log-path "futon3/logs/logic_audit.edn")

(defonce ^:private log-path (atom default-log-path))
(defonce ^:private summary (atom {:overall {:attempted 0 :passed 0}
                                  :runs {}}))
(def ^:private lock (Object.))

(defn set-log-path!
  "Override logic audit log destination (tests)."
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

(defn- normalize-run-id [run-id session-id]
  (cond
    (string? run-id) run-id
    (string? session-id) session-id
    (keyword? run-id) (name run-id)
    (nil? run-id) "unknown"
    :else (str run-id)))

(defn- obligation-kinds [logic]
  (->> (:obligations logic)
       (keep :obligation)
       vec))

(defn- extract-result [result]
  (let [logic (or (:logic result) result)
        judgement (:judgement logic)
        ok? (if (contains? result :ok?)
              (:ok? result)
              (= judgement :admissible))]
    {:ok? (boolean ok?)
     :kernel (:kernel logic)
     :rule (get-in logic [:witness :rule])
     :judgement judgement
     :obligation-count (count (:obligations logic))
     :obligation-kinds (obligation-kinds logic)
     :error-count (count (:errors result))}))

(defn- update-counts [counts ok?]
  (let [counts (or counts {:attempted 0 :passed 0})
        pass-inc (if ok? 1 0)]
    (-> counts
        (update :attempted inc)
        (update :passed + pass-inc))))

(defn- update-summary [summary entry]
  (let [run-id (:run/id entry)
        scope (:scope entry)
        kernel (:check/kernel entry)
        rule (:check/rule entry)
        ok? (:check/ok? entry)
        at (:at entry)]
    (-> summary
        (update :overall update-counts ok?)
        (update-in [:overall :by-scope scope] update-counts ok?)
        (update-in [:overall :by-kernel kernel] update-counts ok?)
        (update-in [:runs run-id :attempted] (fnil inc 0))
        (update-in [:runs run-id :passed] (fnil + 0) (if ok? 1 0))
        (update-in [:runs run-id :by-scope scope] update-counts ok?)
        (update-in [:runs run-id :by-kernel kernel] update-counts ok?)
        (update-in [:runs run-id :by-rule [scope rule]] update-counts ok?)
        (assoc-in [:runs run-id :last-at] at))))

(defn record!
  "Record a logic check attempt. Accepts {:result ...} or {:logic ...}."
  [{:keys [scope op result] :as entry}]
  (let [{:keys [ok? kernel rule judgement obligation-count obligation-kinds error-count]}
        (extract-result (or result entry))
        session-id (:session/id entry)
        run-id (normalize-run-id (:run/id entry) session-id)
        payload {:at (clock/->iso-string)
                 :scope (or scope :unknown)
                 :run/id run-id
                 :session/id session-id
                 :check/op op
                 :check/kernel (or kernel :unknown)
                 :check/rule (or rule :unknown)
                 :check/judgement judgement
                 :check/ok? ok?
                 :check/obligation-count obligation-count
                 :check/obligation-kinds obligation-kinds
                 :check/error-count error-count}]
    (swap! summary update-summary payload)
    (append-log! payload)))

(defn summary
  "Return accumulated summary of logic checks."
  []
  @summary)

(defn summary-for
  "Return per-run summary for RUN-ID (or session-id)."
  [run-id]
  (get-in @summary [:runs (normalize-run-id run-id nil)]))
