(ns futon3.tatami
  "Tatami-side CLI helpers and future MUSN REPL bindings."
  (:require [clojure.string :as str]
            [futon3.tatami-schema :as schema]
            [futon3.tatami-store :as store])
  (:import (java.time Instant Duration)
           (java.util Date UUID)))

(defonce ^:private !session (atom nil))
(defonce ^:private !selection (atom {:prototypes [:f0/p0]
                                     :intent "general session"}))

(def ^:private activity-order
  [:tai-chi :coding :band :job-search :pattern-work :agent-work :other])

(def ^:private felt-states [:tired :bored :ok :stressed :calm])

(defn- prompt [label]
  (print label)
  (flush)
  (some-> (read-line) str/trim))

(defn- ensure-session! []
  (or @!session
      (throw (ex-info "no-active-session" {:hint "Start a tatami session."}))))

(defn- set-session! [session]
  (reset! !session session)
  session)

(defn- current-session-id []
  (some-> @!session :session-id))

(defn- normalize-session-id [value]
  (cond
    (instance? UUID value) value
    (string? value) (let [trim (str/trim value)]
                      (when (seq trim)
                        (try
                          (UUID/fromString trim)
                          (catch IllegalArgumentException _ nil))))
    :else nil))

(defn- require-session-id [value]
  (when value
    (or (normalize-session-id value)
        (throw (ex-info "invalid-session-id" {:session-id value})))))

(defn- parse-prototypes [text]
  (->> (str/split (or text "") #",+")
       (map str/trim)
       (remove empty?)
       (map keyword)
       vec))

(defn- normalize-prototypes [items]
  (->> items
       (map (fn [item]
              (cond
                (keyword? item) item
                (string? item) (keyword item)
                :else nil)))
       (remove nil?)
       vec))

(defn current-selection []
  @!selection)

(defn select-target!
  "Set the default prototype/intent selection for future sessions/logs."
  [{:keys [prototypes intent] :as opts}]
  (let [normalized (normalize-prototypes prototypes)
        updated {:prototypes (or (seq normalized)
                                 (:prototypes @!selection)
                                 [:f0/p0])
                 :intent (or intent (:intent @!selection) "general session")}]
    (reset! !selection updated)
    updated))

(defn- classify-fruit [activity performed?]
  (cond
    (not performed?) :indicator
    (#{:job-search :agent-work} activity) :obligation
    (#{:tai-chi :band} activity) :joy
    (#{:coding :pattern-work} activity) :insight
    :else :indicator))

(defn- classify-paramita [felt]
  (case felt
    :tired :energy
    :stressed :equanimity
    :bored :wisdom
    :calm :equanimity
    :ok :equanimity
    :energy))

(defn start-session
  "Programmatic session start. Requires :prototypes and :intent.
   Optionally pass :session-id to reuse an existing identifier."
  [{:keys [prototypes intent session-id] :as opts}]
  (let [selection (current-selection)
        chosen-protos (or (seq (normalize-prototypes prototypes))
                          (:prototypes selection)
                          (throw (ex-info "prototypes-required" opts)))
        chosen-intent (or intent (:intent selection) "session")
        sid (or (require-session-id session-id) (UUID/randomUUID))
        event (schema/new-event :session-start {:session-id sid
                                                :prototypes chosen-protos
                                                :intent chosen-intent})]
    (store/append-event! event)
    (set-session! {:session-id sid
                   :prototypes (:prototypes event)
                   :futon-tags (:futon-tags event)
                   :intent (:intent event)})
    event))

(defn start
  "Interactive tatami session start."
  ([] (start {}))
  ([{:keys [prototypes intent] :as opts}]
   (let [derived-protos (or (seq prototypes)
                            (parse-prototypes (prompt "Prototypes? (e.g. f0/p5,f3/p0): ")))
         derived-intent (or intent (prompt "Intention for this block?: "))
         event (start-session {:prototypes derived-protos
                               :intent derived-intent})]
     (println "Session started" (:session-id event))
     event)))

(defn- session-or-default [session-id]
  (let [normalized (normalize-session-id session-id)
        current (current-session-id)]
    (cond
      (nil? session-id) (ensure-session!)
      (and normalized current (= normalized current)) @!session
      :else (throw (ex-info "unknown-session" {:session-id session-id})))))

(defn log-event
  "Programmatic logging helper. Requires :session-id (or active session)."
  [{:keys [session-id activity felt-state] :as opts}]
  (let [session (session-or-default session-id)
        activity (cond
                    (keyword? activity) activity
                    (string? activity) (keyword activity)
                    :else :other)
        performed? (if (contains? opts :performed?) (:performed? opts) true)
        felt (cond
               (keyword? felt-state) felt-state
               (string? felt-state) (keyword felt-state)
               :else :ok)
        event (schema/new-event (if performed? :activity :skip)
                                (-> opts
                                    (assoc :session-id (:session-id session)
                                           :prototypes (:prototypes session)
                                           :futon-tags (:futon-tags session)
                                           :activity activity
                                           :felt-state felt)
                                    (update :fruit #(or % (classify-fruit activity performed?)))
                                    (update :paramita #(or % (classify-paramita felt)))))]
    (store/append-event! event)
    event))

(defn log
  "Log an activity or skip inside the active session."
  ([]
   (let [activity-input (prompt (str "Activity? " (str/join "/" (map name activity-order)) ": "))
         activity (some-> activity-input str/trim keyword)
         did-response (str/lower-case (or (prompt "Did you do it? (y/n): ") ""))
         did-it? (= did-response "y")
         felt (some-> (prompt (str "Felt state? " (str/join "/" (map name felt-states)) ": "))
                      str/trim
                      keyword)
         notes (prompt "Notes?: ")]
     (log {:activity (or activity :other)
           :performed? did-it?
           :felt-state (or felt :ok)
           :notes notes})))
  ([opts]
   (let [event (log-event opts)]
     (println (str "Logged " (name (:kind event)) " " (or (some-> (:activity event) name) "")))
     event)))

(defn convolve
  "Very rough clause heuristics for the current session."
  []
  (let [session (ensure-session!)
        events (->> (store/load-events)
                    (filter #(= (:session-id session) (:session-id %))))
        tai-chi? (some #(and (= :activity (:kind %)) (= :tai-chi (:activity %))) events)
        coding-stack? (some #(and (= :activity (:kind %))
                                  (= :coding (:activity %))
                                  (some #{:f1 :f3} (:futon-tags %)))
                            events)
        agent-work? (some #(and (= :activity (:kind %))
                                  (= :agent-work (:activity %))
                                  (some #{:f3} (:futon-tags %)))
                             events)]
    (println "Session" (:session-id session) "→" (count events) "events")
    (when tai-chi?
      (println "  ✓ Candidate F0/P0 evidence (tai chi logged)."))
    (when coding-stack?
      (println "  ✓ Candidate F0/P5 stack stewardship evidence."))
    (when agent-work?
      (println "  ✓ Candidate F3/P0 agent-work evidence (ChatGPT session logged)."))
    (when-not (or tai-chi? coding-stack? agent-work?)
      (println "  (No heuristic matches yet; keep logging.)"))
    {:session-id (:session-id session)
     :event-count (count events)
     :p0 tai-chi?
     :p5 coding-stack?
     :f3 agent-work?}))

(defn close-session
  [{:keys [session-id summary notes]}]
  (let [session (session-or-default session-id)
        event (schema/new-event :session-proof {:session-id (:session-id session)
                                                :prototypes (:prototypes session)
                                                :notes (or summary notes)
                                                :futon-tags (:futon-tags session)})]
    (store/append-event! event)
    (when (= (:session-id session) (current-session-id))
      (reset! !session nil))
    event))

(defn close
  "Close the current session with a proof summary."
  ([]
   (let [summary (prompt "Short proof summary for this block?: ")]
     (close {:summary summary})))
  ([opts]
   (close-session opts)
   (println "Session closed; proof summary recorded.")))

(defn status
  "Summarise the last 24h of events. Optional opts {:quiet? bool}."
  ([] (status {}))
  ([{:keys [quiet?]}]
   (let [cutoff (Date/from (.minus (Instant/now) (Duration/ofHours 24)))
         events (store/load-events-since cutoff)
         fruit-counts (frequencies (map :fruit events))
         activity-counts (frequencies (map :activity events))
         proofs-today? (boolean (some #(= :session-proof (:kind %)) events))
         summary {:events (count events)
                  :fruits fruit-counts
                  :activities activity-counts
                  :proofs? proofs-today?}]
     (when-not quiet?
       (println "Tatami status (last 24h):")
       (println "  Events:" (:events summary))
       (println "  Fruits:" fruit-counts)
       (println "  Activities:" activity-counts)
       (println "  Proof summaries today?" (if proofs-today? "yes" "no")))
     (assoc summary :selection (-> (current-selection)
                                   (update :prototypes
                                           (fn [prot]
                                             (when prot
                                               (mapv name prot)))))))))

;; TODO: Emacs / sidebuffer integration — display tatami status HUD.
;; TODO: Proof chain — aggregate session-proof events into proof-chain.edn.
