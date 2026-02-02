(ns futon3.musn.service
  "Session registry and thin wrappers around the MUSN router. Intended to be wrapped by HTTP/Drawbridge handlers.

   This module also handles light persistence (append-only log + snapshot) so MUSN sessions can survive crashes
   and be replayed or resumed. Logs are EDN lines under lab/musn/, keyed by session id."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.edn :as edn]
            [org.httpkit.server :as http]
            [futon2.aif.engine :as aif-engine]
            [futon3.fulab.pattern-competence :as pc]
            [futon3.logic-audit :as logic-audit]
            [futon3.musn.fulab :as fulab-musn]
            [futon3.musn.router :as router]
            [futon3.musn.schema :as schema]
            [futon3.pattern-hints :as hints]))

(def default-policy
  {:off-trail {:free 3 :ratio 0.5 :action "off-trail"}
   :trail-allow {:patterns [] :namespaces []}
   :aif-config {}
   :mana {:start 100
          :action-cost 1
          :restore 10}
   :certificates []})

(def default-lab-root
  (or (System/getenv "MUSN_LAB_ROOT") "lab"))

(defonce sessions (atom {}))
(defonce rooms (atom {}))
(declare get-session append-lab-event! restored? note-restore! apply-mana! mana-config turn-event plan-eval-config link-psr-to-pattern! link-pur-to-pattern!)
(defonce aif-tap-installed? (atom false))

(defonce fulab-adapter
  (delay
    (try
      (require 'futon2.aif.adapters.fulab)
      (resolve 'futon2.aif.adapters.fulab/new-adapter)
      (catch Throwable _ nil))))

(defn- warn-aif-unavailable! [note]
  (binding [*out* *err*]
    (println (format "[musn] AIF adapter unavailable (%s)" note))))

(defn- new-aif-engine [config]
  (if-let [new-adapter @fulab-adapter]
    (try
      (aif-engine/new-engine (new-adapter config))
      (catch Throwable t
        (warn-aif-unavailable! (.getMessage t))
        nil))
    (do
      (when (seq config)
        (warn-aif-unavailable! "adapter missing"))
      nil)))

(def ^:private room-event-limit 200)
(def ^:private room-cache-limit 200)
(def ^:private room-bid-limit 50)

(defn- validate! [spec req]
  (let [validator (schema/validator spec)]
    (when-not (validator req)
      (throw (ex-info "invalid request" {:spec spec :value req}))))
  req)

(defn- now-inst []
  (java.time.Instant/now))

(defn- room-log-file [lab-root room-id]
  (io/file lab-root "musn" "chat" (str room-id ".ndjson")))

(defn- new-room [room-id lab-root]
  {:room/id room-id
   :lab/root lab-root
   :log-file (room-log-file lab-root room-id)
   :paused? false
   :pause nil
   :bids []
   :events []
   :seq 0
   :msg-cache {}
   :msg-cache-order []
   :last-activity (now-inst)})

(defn- ensure-room [rooms room-id lab-root]
  (or (get rooms room-id)
      (new-room room-id lab-root)))

(defn- append-chat-log! [room event]
  (let [path (:log-file room)]
    (try
      (io/make-parents path)
      (spit path (str (pr-str event) "\n") :append true)
      (catch Throwable _))))

(defn- cache-key [type msg-id]
  [type msg-id])

(defn- cached-response [room type msg-id]
  (when msg-id
    (get (:msg-cache room) (cache-key type msg-id))))

(defn- remember-response [room type msg-id resp]
  (if-not msg-id
    room
    (let [key (cache-key type msg-id)
          order (if (some #(= key %) (:msg-cache-order room))
                  (:msg-cache-order room)
                  (conj (vec (:msg-cache-order room)) key))
          cache (assoc (:msg-cache room) key resp)
          excess (- (count order) room-cache-limit)
          drop-keys (when (pos? excess) (take excess order))
          order (if (seq drop-keys) (vec (drop excess order)) order)
          cache (if (seq drop-keys) (apply dissoc cache drop-keys) cache)]
      (assoc room :msg-cache cache :msg-cache-order order))))

(defn- append-room-event [room event]
  (let [event (cond-> event
                (nil? (:at event)) (assoc :at (now-inst)))
        seq-id (inc (or (:seq room) 0))
        event (assoc event :seq seq-id)
        events (conj (vec (:events room)) event)
        events (if (> (count events) room-event-limit)
                 (vec (take-last room-event-limit events))
                 events)]
    [(assoc room :seq seq-id :events events :last-activity (:at event)) event]))

(defn- update-room! [room-id lab-root f]
  (let [result (atom nil)]
    (swap! rooms
           (fn [rs]
             (let [room (ensure-room rs room-id lab-root)
                   {:keys [room event resp]} (f room)]
               (reset! result {:room room :event event :resp resp})
               (assoc rs room-id room))))
    (let [{:keys [room event resp]} @result]
      (when event
        (append-chat-log! room event))
      resp)))

(defn- trim-bids [bids]
  (if (> (count bids) room-bid-limit)
    (vec (take-last room-bid-limit bids))
    (vec bids)))

(defn- tap->event [tap]
  {:event/type :aif/tap
   :at (fulab-musn/now-inst)
   :payload tap})

(defn- handle-aif-tap! [tap]
  (when (= :aif/fulab (:type tap))
    (when-let [sid (:session/id tap)]
      (when-let [entry (get-session sid)]
        (let [event (tap->event tap)]
          (reset! (:aif-tap entry) event)
          (append-lab-event! entry event))))))

(defn- ensure-aif-tap-listener! []
  (when (compare-and-set! aif-tap-installed? false true)
    (add-tap #'handle-aif-tap!)))

(defn- aif-engine-state [entry]
  (when-let [engine (:aif-engine entry)]
    (when-let [state (:state engine)]
      @state)))

(defn- aif-mu-summary [entry pattern-id]
  (let [state (aif-engine-state entry)
        evidence (:pattern-evidence state)
        pattern-evidence (when (and pattern-id (map? evidence))
                           (get evidence pattern-id))]
    (cond-> {:pattern-count (count (or evidence {}))}
      (seq pattern-evidence) (assoc :pattern-evidence pattern-evidence))))

(defn- candidate-precision-summary [candidate-details candidate-ids]
  (into {}
        (keep (fn [pid]
                (let [details (get candidate-details pid)
                      fallback (hints/pattern-entry pid)
                      payload (merge (when (map? details)
                                       (select-keys details
                                                    [:precision-prior
                                                     :evidence-count
                                                     :next-steps-count]))
                                     (when (map? fallback)
                                       {:precision-prior (:precision/prior fallback)
                                        :evidence-count (:evidence/count fallback)
                                        :next-steps-count (:next-steps/count fallback)}))]
                  (when (seq payload)
                    [pid payload]))))
        candidate-ids))

(defn- resolve-pattern-files [pattern-id files]
  (let [fallback (some-> (hints/pattern-entry pattern-id) :source/path)
        candidates (->> (concat (or files [])
                                (when fallback [fallback]))
                        (remove nil?)
                        distinct)]
    (->> candidates
         (map io/file)
         (filter #(.exists %))
         vec)))

(defn- current-pattern-maturity [pattern-id files]
  (let [targets (resolve-pattern-files pattern-id files)]
    (some #(hints/maturity-for-pattern-file % pattern-id) targets)))

(defn- previous-pattern-maturity [entry pattern-id]
  (or (get @(:pattern-maturity entry) pattern-id)
      (some-> (hints/pattern-entry pattern-id) :maturity/phase)))

(defn- note-pattern-maturity! [entry pattern-id phase]
  (swap! (:pattern-maturity entry) assoc pattern-id phase))

(defn- maybe-award-maturity-bonus! [entry pattern-id files action]
  (when (and (:mana entry)
             (contains? #{"update" "implement"} (str action))
             (string? pattern-id))
    (when-let [current (current-pattern-maturity pattern-id files)]
      (let [current-phase (:maturity/phase current)
            prev-phase (previous-pattern-maturity entry pattern-id)
            mana @(:mana entry)
            awarded? (restored? mana :maturity-upgrade pattern-id)]
        (note-pattern-maturity! entry pattern-id current-phase)
        (when (and (not awarded?)
                   (= prev-phase :stub)
                   (= current-phase :active))
          (let [{:keys [maturity-bonus]} (mana-config (:policy entry))
                bonus (double (or maturity-bonus 10))]
            (apply-mana! entry bonus :maturity-upgrade
                         {:pattern/id pattern-id
                          :from prev-phase
                          :to current-phase})
            (note-restore! entry :maturity-upgrade pattern-id)
            (append-lab-event! entry {:event/type :mana/bonus
                                      :at (fulab-musn/now-inst)
                                      :payload {:pattern/id pattern-id
                                                :from prev-phase
                                                :to current-phase
                                                :delta bonus}})))))))

(defn- tap-aif-context! [payload]
  (tap> (merge {:type :aif/context} payload)))

(defn- tap-aif-policy! [payload]
  (tap> (merge {:type :aif/policy} payload)))

(defn- session-agent [client-id]
  (keyword (or (some-> client-id str str/trim) "musn")))

(defn- session-file [lab-root sid]
  (io/file lab-root "sessions" (str sid ".edn")))

(defn- read-lab-session [lab-root sid client-id]
  (let [path (session-file lab-root sid)]
    (if (.exists path)
      (pc/read-session-file (str path))
      {:session/id sid
       :session/agent (session-agent client-id)
       :events []})))

(defn- write-lab-session! [entry session]
  (let [path (str (:session-file entry))]
    (io/make-parents path)
    (pc/write-session-file! path session)))

(defn- append-lab-event! [entry event]
  (let [session (pc/append-event @(:lab-session entry) event)]
    (reset! (:lab-session entry) session)
    (write-lab-session! entry session)
    session))

(defn- new-fulab-state []
  {:turn nil
   :candidates []
   :candidate-scores {}
   :candidate-details {}
   :psr nil
   :pur nil
   :psr-emitted? false
   :pur-emitted? false
   :deviation nil              ;; AIF-LM-3: track deviation from AIF suggestion
   :turn-ended nil
   :turn-ended-resp nil
   :aif-config-logged? false})

(defn- tau-cache-path [lab-root]
  (io/file lab-root "aif" "tau-cache.edn"))

(defn- read-tau-cache [lab-root]
  (let [path (tau-cache-path lab-root)]
    (when (.exists path)
      (try
        (edn/read-string (slurp path))
        (catch Throwable _ nil)))))

(defn- write-tau-cache! [lab-root cache]
  (let [path (tau-cache-path lab-root)]
    (io/make-parents path)
    (spit path (pr-str cache))))

(defn- mean [values]
  (/ (reduce + values) (double (count values))))

(defn- scores->tau [scores]
  (let [scores (->> scores
                    (keep #(when (number? %) (double %)))
                    sort
                    vec)]
    (cond
      (< (count scores) 2) 0.5
      :else (let [best (first scores)
                  second (second scores)
                  worst (last scores)
                  range (max 1.0e-6 (- (double worst) (double best)))
                  spread (/ (- (double second) (double best)) range)
                  scaled (+ 0.35 (* 0.6 (min 1.0 spread)))]
              (-> scaled (max 0.35) (min 0.95) double)))))

(defn- donation-summary [entry turn]
  (let [events (or (:events @(:lab-session entry)) [])
        turn (or turn (:turn @(:state entry)))
        plan-award (get-in @(:state entry) [:plan/eval :musn.plan/mana :award])
        donate? (true? (:donate? (plan-eval-config (:policy entry))))]
    (let [turn-events (filter #(= turn (:turn %)) events)
          donations (filter #(= :mana/donation (:event/type %)) turn-events)
          failed (filter #(= :mana/donation-failed (:event/type %)) turn-events)
          total (reduce + 0.0
                        (keep (fn [event]
                                (let [amount (get-in event [:payload :amount])]
                                  (when (number? amount)
                                    (double amount))))
                              donations))
          donation-count (count donations)
          failed-count (count failed)
          pending (when (and donate?
                             (number? plan-award)
                             (pos? (double plan-award))
                             (zero? (+ donation-count failed-count)))
                    (double plan-award))]
      (when (or (pos? donation-count) (pos? failed-count) (number? pending))
        (cond-> {:count donation-count
                 :total total
                 :failed failed-count}
          (number? pending) (assoc :pending pending))))))

(defn- with-donation-summary [resp entry turn]
  (if-let [summary (:summary resp)]
    (if-let [donations (donation-summary entry turn)]
      (assoc resp :summary (assoc summary :mana/donations donations))
      resp)
    resp))

(defn- tau-scale [entry]
  (double (or (:tau/scale (:aif-config entry)) 1.0)))

(defn- tau->uncertainty [entry tau]
  (when (and (number? tau) (pos? tau))
    (/ (tau-scale entry) (double tau))))

(defn- scores->uncertainty [entry scores]
  (when (seq scores)
    (tau->uncertainty entry (scores->tau scores))))

(defn- uncertainty-detail [value source detail]
  (when (number? value)
    (merge {:value (double value)
            :source source}
           detail)))

(defn- precision->uncertainty [precision]
  (when (number? precision)
    (/ 1.0 (double (max 0.05 precision)))))

(declare candidate-id)

(defn- candidate-details-map [details]
  (->> details
       (keep (fn [entry]
               (when-let [id (candidate-id entry)]
                 [id entry])))
       (into {})))

(defn- candidate-precision [entry pattern-id]
  (or (get-in @(:fulab entry) [:candidate-details pattern-id :precision-prior])
      (some-> (hints/pattern-entry pattern-id) :precision/prior)
      0.5))

(defn- candidates-uncertainty-details
  ([entry candidate-ids]
   (candidates-uncertainty-details entry candidate-ids nil nil))
  ([entry candidate-ids candidate-details]
   (candidates-uncertainty-details entry candidate-ids candidate-details nil))
  ([entry candidate-ids candidate-details scores]
   (when (seq candidate-ids)
     (let [cache @(:tau-cache entry)
           taus (keep cache candidate-ids)
           cached (when (seq taus) (mean taus))
           precision-lookup (if (map? candidate-details)
                              (fn [pid] (get-in candidate-details [pid :precision-prior]))
                              (fn [pid] (candidate-precision entry pid)))
           precisions (keep precision-lookup candidate-ids)
           precision (when (seq precisions) (mean precisions))
           score-values (cond
                          (map? scores) (vals scores)
                          (sequential? scores) scores
                          :else nil)
           score-uncertainty (scores->uncertainty entry score-values)]
       (or (uncertainty-detail (tau->uncertainty entry cached)
                               :tau-cache
                               {:tau cached})
           (uncertainty-detail (precision->uncertainty precision)
                               :precision-prior
                               {:precision precision})
           (uncertainty-detail score-uncertainty
                               :score-spread
                               {}))))))

(defn- pattern-uncertainty-details [entry pattern-id]
  (when pattern-id
    (let [cached (get @(:tau-cache entry) pattern-id)
          precision (candidate-precision entry pattern-id)
          score (get-in @(:fulab entry) [:candidate-scores pattern-id])]
      (or (uncertainty-detail (tau->uncertainty entry cached)
                              :tau-cache
                              {:tau cached})
          (uncertainty-detail (precision->uncertainty precision)
                              :precision-prior
                              {:precision precision})
          (uncertainty-detail (scores->uncertainty entry [score])
                              :score-spread
                              {})))))

(defn- update-tau-cache! [entry pattern-id tau]
  (when (and pattern-id (number? tau))
    (let [next (assoc (or @(:tau-cache entry) {}) pattern-id (double tau))]
      (reset! (:tau-cache entry) next)
      (write-tau-cache! (:lab-root entry) next))))

(defn- mana-config [policy]
  (merge {:start 100
          :action-cost 1
          :restore 10
          :maturity-bonus 10}
         (:mana policy)))

(def ^:private default-plan-eval-config
  {:award? true
   :donate? false
   :donate-min 0.0
   :donate-max nil
   :donate-cmd nil
   :donate-donor "musn-plan"})

(defn- env-truthy? [value]
  (contains? #{"1" "true" "yes" "on"} (some-> value str str/lower-case)))

(defn- plan-eval-config [policy]
  (let [cfg (merge default-plan-eval-config
                   (get-in policy [:mana :plan-eval]))]
    (if-let [flag (System/getenv "MUSN_DANA_ENABLED")]
      (assoc cfg :donate? (env-truthy? flag))
      cfg)))

(defn- plan-eval-award [plan-eval]
  (when (map? plan-eval)
    (let [award (get-in plan-eval [:musn.plan/mana :award])]
      (when (number? award)
        (double award)))))

(defn- donation-cmd [config]
  (or (:donate-cmd config)
      (System/getenv "MUSN_DANA_CMD")
      (str (System/getProperty "user.dir") "/scripts/give-mana")))

(defn- plan-eval-note [entry turn plan-eval]
  (let [confidence (double (or (:musn.plan/confidence plan-eval) 0.0))
        risk (double (or (:musn.plan/risk plan-eval) 0.0))]
    (format "plan-eval %s turn %s (confidence %.2f risk %.2f)"
            (:id entry) turn confidence risk)))

(defn- donate-plan-eval! [entry turn amount plan-eval]
  (let [config (plan-eval-config (:policy entry))
        donate? (true? (:donate? config))
        minv (double (or (:donate-min config) 0.0))
        maxv (:donate-max config)
        amount (double amount)
        amount (cond-> amount
                 (number? maxv) (min (double maxv))
                 :always (max minv))]
    (when (and donate? (pos? amount))
      (let [note (plan-eval-note entry turn plan-eval)
            cmd (donation-cmd config)]
        (future
          (try
            (let [args (cond-> [cmd (format "%.4f" amount)]
                         (seq note) (into ["--note" note]))
                  result (apply shell/sh args)
                  payload {:session/id (:id entry)
                           :turn turn
                           :amount amount
                           :note note
                           :plan/eval plan-eval
                           :exit (:exit result)}]
              (if (zero? (:exit result))
                (append-lab-event! entry (turn-event :mana/donation (:id entry) turn payload))
                (append-lab-event! entry (turn-event :mana/donation-failed (:id entry) turn
                                                     (assoc payload
                                                            :stderr (:err result)
                                                            :stdout (:out result))))))
            (catch Throwable t
              (append-lab-event! entry (turn-event :mana/donation-failed (:id entry) turn
                                                   {:amount amount
                                                    :note note
                                                    :error (.getMessage t)})))))))))

(defn- new-mana-state [policy]
  (let [{:keys [start]} (mana-config policy)
        start (or start 100)]
    {:budget start
     :balance start
     :earned 0
     :spent 0
     :turn nil
     :restores {:psr->action #{}
                :action->pur #{}
                :maturity-upgrade #{}
                :help-read #{}}
     :last-change nil}))

(defn- reset-mana-turn! [entry turn]
  (swap! (:mana entry)
         (fn [mana]
           (-> mana
               (assoc :turn turn)
               (assoc :restores {:psr->action #{}
                                 :action->pur #{}
                                 :maturity-upgrade #{}
                                 :help-read #{}})))))

(defn- mana-view [mana]
  (dissoc mana :restores))

(defn- apply-mana! [entry delta reason meta]
  (let [delta (double delta)
        amount (Math/abs delta)]
    (swap! (:mana entry)
           (fn [mana]
             (-> mana
                 (update :balance + delta)
                 (update (if (pos? delta) :earned :spent) + amount)
                 (assoc :last-change (merge {:delta delta
                                             :reason reason
                                             :at (fulab-musn/now-inst)}
                                            meta)))))))

(defn- mana-balance [entry]
  (let [mana @(:mana entry)]
    (cond
      (number? (:balance mana)) (double (:balance mana))
      (number? (:budget mana)) (double (:budget mana))
      :else nil)))

(defn- mark-mana-depleted! [entry turn]
  (when-let [balance (mana-balance entry)]
    (when (neg? balance)
      (swap! (:state entry)
             (fn [st]
               (if (= :mana-depleted (get-in st [:halt :type]))
                 st
                 (assoc st :halt {:type :mana-depleted
                                  :note "mana below zero; turn ends"}))))
      (append-lab-event! entry {:event/type :mana/depleted
                                :at (fulab-musn/now-inst)
                                :payload {:turn turn
                                          :balance balance}}))))

(defn- restored? [mana kind pattern-id]
  (contains? (get-in mana [:restores kind] #{}) pattern-id))

(defn- note-restore! [entry kind pattern-id]
  (swap! (:mana entry)
         (fn [mana]
           (update-in mana [:restores kind] (fnil conj #{}) pattern-id))))

(defn- reset-fulab-state! [entry turn candidates scores candidate-details]
  (reset! (:fulab entry)
          (assoc (new-fulab-state)
                 :turn turn
                 :candidates (vec (or candidates []))
                 :candidate-scores (or scores {})
                 :candidate-details (or candidate-details {}))))

(defn- event-base [session-id turn]
  {:event/type :turn/event
   :session/id session-id
   :turn turn
   :at (fulab-musn/now-inst)})

(defn- turn-event [event-type session-id turn payload]
  (cond-> (merge (event-base session-id turn)
                 {:event/type event-type})
    (map? payload) (assoc :payload payload)))

(defn- pattern-action-event [session-id turn action]
  (let [payload {:session/id session-id
                 :pattern/id (:pattern/id action)
                 :pattern/action (:action action)
                 :pattern/note (:note action)
                 :pattern/files (:files action)
                 :pattern/source (:source action)}]
    (merge (event-base session-id turn)
           {:event/type :pattern/action
            :pattern/id (:pattern/id action)
            :pattern/action (:action action)}
           payload
           {:payload payload})))

(defn- action-anchors [actions pattern-id]
  (->> actions
       (filter #(= pattern-id (:pattern/id %)))
       (map (fn [action]
              (fulab-musn/pattern-action-anchor pattern-id (:action action))))
       distinct
       vec))

(defn- selection-metrics [selection]
  (let [aif (:aif selection)
        rejected (:G-rejected aif)]
    (cond-> {}
      (number? (:G-chosen aif)) (assoc :G (:G-chosen aif))
      (number? (:tau aif)) (assoc :tau (:tau aif))
      (and (map? rejected) (seq rejected)) (assoc :G-rejected rejected))))

(defn- selection-g-terms [selection]
  (let [aif (:aif selection)
        g (if (number? (:G-chosen aif))
            (double (:G-chosen aif))
            0.0)
        rejected (:G-rejected aif)
        rejected-count (if (map? rejected) (count rejected) 0)]
    [{:term-id :base
      :observation-keys [:candidates :anchors :forecast]
      :precision-channels [[:heuristic 1.0]]
      :intermediate-values {:g-chosen g
                            :g-rejected-count rejected-count}
      :final-contribution g}]))

(defn- enrich-reason [reason selection]
  (let [metrics (selection-metrics selection)]
    (if (seq metrics)
      (assoc reason :aif metrics)
      reason)))

(defn- psr-eligible? [candidates chosen]
  (and (seq candidates)
       (string? chosen)
       (some #{chosen} candidates)))
(def ^:private auto-select-sentinels #{"auto" "aif"})

(defn- auto-select-requested? [chosen]
  (cond
    (nil? chosen) true
    (keyword? chosen) (contains? auto-select-sentinels (name chosen))
    (string? chosen) (contains? auto-select-sentinels (str/lower-case chosen))
    :else false))

(defn- detect-deviation
  "Detect when agent's explicit choice differs from AIF-sampled suggestion.
   Returns deviation info or nil."
  [aif-suggested agent-chosen reason]
  (when (and (string? aif-suggested)
             (string? agent-chosen)
             (not= aif-suggested agent-chosen))
    (let [deviation-reason (get-in reason [:deviation :reason])]
      {:aif-suggested aif-suggested
       :agent-chosen agent-chosen
       :justified? (boolean (and (string? deviation-reason)
                                 (> (count deviation-reason) 0)))
       :reason deviation-reason})))

(defn- auto-selection-policy [selection chosen candidates]
  (let [aif (:aif selection)
        abstain? (boolean (:abstain? aif))
        mode (cond
               (and abstain? (seq candidates)) :explore
               (and chosen (not abstain?)) :auto-select
               :else :abstain)
        reason (cond
                 abstain? "aif-low-tau-explore"
                 (empty? candidates) "no-candidates"
                 (nil? chosen) "no-choice"
                 :else "aif-sampled")]
    {:chosen chosen
     :tau (:tau aif)
     :min-sample (:min-sample aif)
     :mode mode
     :reason reason
     :sampled? (:sampled? aif)
     :abstain? abstain?}))
(defn- gen-session-id []
  (str "musn-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- compact [m]
  (into {} (remove (comp nil? val)) m))

(defn- normalize-tags
  [tags]
  (->> (or tags [])
       (map (fn [tag]
              (cond
                (keyword? tag) tag
                (string? tag) (keyword tag)
                (nil? tag) nil
                :else (keyword (str tag)))))
       (remove nil?)
       vec))

(defn- candidate-id [candidate]
  (cond
    (map? candidate) (recur (or (:id candidate) (:pattern/id candidate)))
    (keyword? candidate) (subs (str candidate) 1)
    (string? candidate) candidate
    (nil? candidate) nil
    :else (str candidate)))

(defn- candidate-ids [candidates]
  (->> candidates (map candidate-id) (remove nil?) vec))

(defn- normalize-score-keys [scores]
  (when (map? scores)
    (into {}
          (keep (fn [[k v]]
                  (let [id (cond
                             (string? k) k
                             (keyword? k) (subs (str k) 1)
                             (nil? k) nil
                             :else (str k))]
                    (when id [id v]))))
          scores)))

(defn- ensure-dir [f]
  (when f
    (.mkdirs (.getParentFile f))))

(defn- log-path [lab-root sid]
  (io/file lab-root "musn" (str sid ".edn")))

(defn- snapshot-path [lab-root sid]
  (io/file lab-root "musn" (str sid ".snapshot.edn")))

(defn- read-snapshot [f]
  (when (.exists f)
    (edn/read-string (slurp f))))

(defn- write-snapshot! [f state]
  (ensure-dir f)
  (spit f (pr-str state)))

(defn- append-log! [f payload]
  (ensure-dir f)
  (spit f (str (pr-str payload) "\n") :append true))

(defn session-ids []
  (keys @sessions))

(defn get-session [sid]
  (get @sessions sid))

(defn current-state [sid]
  (when-let [entry (get-session sid)]
    @(:state entry)))

(defn ensure-session!
  [sid policy lab-root client-id]
  (if-let [existing (get @sessions sid)]
    existing
    (let [p (merge default-policy (or policy {}))
          lab-root (or lab-root default-lab-root)
          snap (read-snapshot (snapshot-path lab-root sid))
          st (atom (or snap (router/new-session p)))
          engine (new-aif-engine (:aif-config p))
          session-file (session-file lab-root sid)
          lab-session (atom (read-lab-session lab-root sid client-id))
          entry {:state st
                 :policy p
                 :lab-root lab-root
                 :log-file (log-path lab-root sid)
                 :snapshot-file (snapshot-path lab-root sid)
                 :id sid
                 :session-file session-file
	         :lab-session lab-session
	         :fulab (atom (new-fulab-state))
                 :aif-tap (atom nil)
	         :tau-cache (atom (or (read-tau-cache lab-root) {}))
                 :pattern-maturity (atom {})
	         :mana (atom (new-mana-state p))
                 :aif-engine engine
                 :aif-config (:aif-config p)
                 :certificates (:certificates p)
                 :client-id client-id}]
      (swap! sessions assoc sid entry)
      (ensure-aif-tap-listener!)
      entry)))

(defn- persist! [entry op req resp]
  (let [state @(:state entry)
        event {:ts (java.time.Instant/now)
               :session/id (:id entry)
               :op op
               :req req
               :resp resp
               :state (select-keys state [:turn :plan? :selection :actions :trail :warnings :halt :write?])}]
    (append-log! (:log-file entry) event)
    (write-snapshot! (:snapshot-file entry) state)))

(defn create-session!
  [req]
  (let [sid (or (:session/id req) (gen-session-id))
        client-id (get-in req [:client :id])
        entry (ensure-session! sid (:policy req) (:lab/root req) client-id)]
    (when (and (:aif-config entry)
               (not (:aif-config-logged? @(:fulab entry))))
      (append-lab-event! entry {:event/type :aif/config
                                :at (fulab-musn/now-inst)
                                :payload {:session/id sid
                                          :config (:aif-config entry)}})
      (swap! (:fulab entry) assoc :aif-config-logged? true))
    (persist! entry :session/create req {:ok true :session/id sid :policy (:policy entry)})
    {:ok true
     :session/id sid
     :policy (:policy entry)}))

(defn turn-start!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        candidates (get-in req [:hud :candidates])
        candidate-ids (candidate-ids candidates)
        candidate-details (candidate-details-map (get-in req [:hud :candidate-details]))
        scores (normalize-score-keys (get-in req [:hud :scores]))
        turn (:turn req)
        scores (when (seq candidate-ids)
                 (if (map? scores)
                   (select-keys scores candidate-ids)
                   scores))
        uncertainty-detail (candidates-uncertainty-details entry candidate-ids candidate-details scores)
        uncertainty (:value uncertainty-detail)
        selection (when (and (seq candidate-ids) (:aif-engine entry))
                    (aif-engine/select-pattern
                     (:aif-engine entry)
                     {:decision/id (str sid ":turn-" turn)
                      :session/id sid
                      :candidates candidate-ids
                      :candidate-scores scores
                      :uncertainty uncertainty
                      :anchors [(fulab-musn/turn-anchor turn)]
                      :forecast (fulab-musn/build-forecast turn)}))
        resp (router/handle-turn-start! (:state entry)
                                        (cond-> {:session/id sid
                                                 :turn (:turn req)}
                                          (:hud req)
                                          (assoc :hud (:hud req))))]
    (reset-fulab-state! entry turn candidate-ids scores candidate-details)
    (when selection
      (tap-aif-context! {:event :select
                         :session/id sid
                         :turn turn
                         :decision/id (get-in selection [:decision/id])
                         :candidates candidate-ids
                         :candidate-scores scores
                         :precision-priors (candidate-precision-summary candidate-details candidate-ids)
                         :uncertainty uncertainty-detail
                         :aif (:aif selection)
                         :mu (aif-mu-summary entry (:chosen selection))}))
    (when (:mana entry)
      (reset-mana-turn! entry turn))
    (append-lab-event! entry (turn-event :turn/started sid turn {:session/id sid}))
    (persist! entry :turn/start req resp)
    (cond-> resp
      selection (assoc :aif (:aif selection))
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn turn-plan!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        plan-type (let [t (:plan/type req)]
                    (cond
                      (keyword? t) t
                      (string? t) (keyword t)
                      :else nil))
        payload (cond-> {:session/id sid
                         :turn (:turn req)}
                  (some? (:plan req)) (assoc :plan (:plan req))
                  plan-type (assoc :plan/type plan-type)
                  (:plan/diagram req) (assoc :plan/diagram (:plan/diagram req))
                  (:plan/components-path req) (assoc :plan/components-path (:plan/components-path req))
                  (:plan/context req) (assoc :plan/context (:plan/context req))
                  (:plan/eval-config req) (assoc :plan/eval-config (:plan/eval-config req)))
        resp (router/handle-turn-plan! (:state entry) payload)
        plan-eval (:plan/eval resp)
        award (plan-eval-award plan-eval)
        plan-config (plan-eval-config (:policy entry))]
    (when (and (:mana entry)
               (true? (:award? plan-config))
               (number? award)
               (pos? award))
      (apply-mana! entry award :plan-eval {:plan/eval plan-eval})
      (append-lab-event! entry
                         (turn-event :mana/credit sid turn
                                     {:amount award
                                      :reason :plan-eval
                                      :plan/eval plan-eval}))
      (donate-plan-eval! entry turn award plan-eval))
    (persist! entry :turn/plan req resp)
    (cond-> resp
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn turn-select!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        candidates (candidate-ids (:candidates req))
        raw-chosen (:chosen req)
        auto? (auto-select-requested? raw-chosen)
        chosen (when-not auto? (candidate-id raw-chosen))
        fulab-state @(:fulab entry)
        candidate-details (:candidate-details fulab-state)
        score-map (:candidate-scores fulab-state)
        score-map (when (map? score-map)
                    (select-keys score-map candidates))
        uncertainty-detail (or (pattern-uncertainty-details entry chosen)
                                (candidates-uncertainty-details entry candidates candidate-details score-map))
        uncertainty (:value uncertainty-detail)
        selection (when (:aif-engine entry)
                    (aif-engine/select-pattern
                     (:aif-engine entry)
                     (cond-> {:decision/id (str sid ":turn-" turn)
                              :session/id sid
                              :candidates candidates
                              :candidate-scores (get-in @(:fulab entry) [:candidate-scores])
                              :uncertainty uncertainty
                              :anchors [(fulab-musn/turn-anchor turn)]
                              :forecast (fulab-musn/build-forecast turn)}
                       (not auto?) (assoc :chosen chosen))))
        aif (:aif selection)
        aif-suggested (:chosen selection)
        sampled (when auto? aif-suggested)
        abstain? (boolean (and auto? (:abstain? aif)))
        chosen (cond
                 (and auto? (not abstain?)) sampled
                 auto? nil
                 :else chosen)
        ;; AIF-LM-3: Detect deviation from AIF suggestion
        deviation (when-not auto?
                    (detect-deviation aif-suggested chosen (:reason req)))
        policy (when auto?
                 (auto-selection-policy selection chosen candidates))
        reason (enrich-reason (or (:reason req) {:mode :read}) selection)
        reason (cond-> reason
                 auto? (assoc :source (or (:source reason) :auto))
                 abstain? (assoc :mode :explore
                                 :note (or (:note reason) "aif-low-tau -> explore")))
        req* (compact (assoc req
                             :session/id sid
                             :turn turn
                             :candidates candidates
                             :chosen chosen
                             :reason reason))
        resp (router/handle-turn-select! (:state entry) req*)
        aif-summary (selection-metrics selection)
        g-terms (when selection (selection-g-terms selection))
        psr (when (psr-eligible? candidates chosen)
              (cond-> (fulab-musn/build-psr {:session-id sid
                                             :turn turn
                                             :candidates candidates
                                             :chosen chosen
                                             :reason reason
                                             :anchors (:anchors req)
                                             :certificates (:certificates entry)})
                (seq g-terms) (assoc :aif/g-terms g-terms)
                deviation (assoc :deviation deviation)))]
    (when policy
      (append-lab-event! entry {:event/type :aif/policy
                                :at (fulab-musn/now-inst)
                                :payload (select-keys policy [:chosen :tau :min-sample :mode :reason :sampled? :abstain?])})
      (tap-aif-policy! {:session/id sid
                        :turn turn
                        :policy (select-keys policy [:chosen :tau :min-sample :mode :reason :sampled? :abstain?])}))
    ;; AIF-LM-3: Log deviation event
    (when deviation
      (append-lab-event! entry {:event/type :aif/deviation
                                :at (fulab-musn/now-inst)
                                :payload {:session/id sid
                                          :turn turn
                                          :aif-suggested (:aif-suggested deviation)
                                          :agent-chosen (:agent-chosen deviation)
                                          :justified? (:justified? deviation)
                                          :reason (:reason deviation)}})
      (swap! (:fulab entry) assoc :deviation deviation))
    (when psr
      (append-lab-event! entry {:event/type :pattern/selection-claimed
                                :at (fulab-musn/now-inst)
                                :payload {:psr psr}})
      (swap! (:fulab entry) assoc
             :psr psr
             :psr-emitted? true)
      ;; Arxana: Link PSR to selected pattern
      (link-psr-to-pattern! sid turn psr))
    (when selection
      (tap-aif-context! {:event :select
                         :session/id sid
                         :turn turn
                         :decision/id (get selection :decision/id)
                         :candidates candidates
                         :candidate-scores score-map
                         :precision-priors (candidate-precision-summary candidate-details candidates)
                         :uncertainty uncertainty-detail
                         :chosen chosen
                         :aif (:aif selection)
                         :deviation deviation
                         :mu (aif-mu-summary entry chosen)})
      (append-lab-event! entry (fulab-musn/summary-event sid :psr selection)))
    (when-let [tau (get-in selection [:aif :tau])]
      (update-tau-cache! entry chosen tau))
    (persist! entry :turn/select req* resp)
    (cond-> resp
      psr (assoc :psr psr)
      (seq aif-summary) (assoc :aif aif-summary)
      deviation (assoc :deviation deviation)
      policy (assoc :aif/policy (select-keys policy [:chosen :tau :min-sample :mode :reason :sampled? :abstain?])))))

(defn turn-action!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        action {:pattern/id (:pattern/id req)
                :action (:action req)
                :note (:note req)
                :files (:files req)
                :source (or (:source req) :explicit)}
        resp (router/handle-turn-action! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :pattern/id (:pattern/id req)
                                                   :action (:action req)
                                                   :note (:note req)
                                                   :files (:files req)
                                                   :source (:source req)
                                                   :cost (:cost req)}))
        uncertainty-detail (pattern-uncertainty-details entry (:pattern/id req))
        belief-update (when (:aif-engine entry)
                        (aif-engine/update-beliefs (:aif-engine entry)
                                                   {:decision/id (str sid ":action-" (java.util.UUID/randomUUID))
                                                    :session/id sid
                                                    :pattern/id (:pattern/id req)
                                                    :pattern/action (:action req)
                                                    :uncertainty (:value uncertainty-detail)
                                                    :status :observed}))
        aif (:aif belief-update)]
    (when belief-update
      (tap-aif-context! {:event :action
                         :session/id sid
                         :turn (:turn req)
                         :pattern/id (:pattern/id req)
                         :pattern/action (:action req)
                         :uncertainty uncertainty-detail
                         :aif aif
                         :mu (aif-mu-summary entry (:pattern/id req))}))
    (maybe-award-maturity-bonus! entry (:pattern/id req) (:files req) (:action req))
    (mark-mana-depleted! entry (:turn req))
    (when-let [tau (get-in belief-update [:aif :tau-updated])]
      (update-tau-cache! entry (:pattern/id req) tau))
    (when (:mana entry)
      (let [{:keys [action-cost restore]} (mana-config (:policy entry))
            base-cost (double (or action-cost 1))
            cost (case (:cost req)
                   :human-contact 5.0
                   :expensive (* 2.0 base-cost)
                   :cheap (* 0.5 base-cost)
                   base-cost)
            restore (double (or restore 10))
            pid (:pattern/id req)
            help-action? (and (= "musn/help" (candidate-id pid))
                              (= "read" (:action req)))
            selection-id (get-in @(:state entry) [:selection :pattern/id])
            mana @(:mana entry)
            restore? (and pid
                          (= pid selection-id)
                          (not (restored? mana :psr->action pid)))]
        (when (and help-action? (not (restored? mana :help-read "musn/help")))
          (apply-mana! entry 1.0 :help-read {:pattern/id "musn/help"
                                             :action (:action req)
                                             :turn (:turn req)})
          (note-restore! entry :help-read "musn/help"))
        (when-not help-action?
          (apply-mana! entry (- cost) :action {:pattern/id pid
                                               :action (:action req)
                                               :turn (:turn req)}))
        (when (and (not help-action?) (= "wide-scan" (:action req)))
          (apply-mana! entry -1.0 :wide-scan {:pattern/id pid
                                              :action (:action req)
                                              :turn (:turn req)}))
        (when restore?
          (apply-mana! entry restore :psr->action {:pattern/id pid
                                                   :action (:action req)
                                                   :turn (:turn req)})
          (note-restore! entry :psr->action pid))))
    (append-lab-event! entry (pattern-action-event sid (:turn req) action))
    (persist! entry :turn/action req resp)
    (cond-> resp
      aif (assoc :aif aif)
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn turn-use!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        resp (router/handle-turn-use! (:state entry)
                                      (compact {:session/id sid
                                                :turn (:turn req)
                                                :pattern/id (:pattern/id req)
                                                :anchors (:anchors req)
                                                :note (:note req)
                                                :inferred? (:inferred? req)}))
        actions (:actions @(:state entry))
        anchors (vec (distinct (concat (:anchors req)
                                       (action-anchors actions (:pattern/id req)))))
        pur (fulab-musn/build-pur {:session-id sid
                                   :turn turn
                                   :pattern-id (:pattern/id req)
                                   :reason (:note req)
                                   :anchors anchors
                                   :inferred? (:inferred? req)
                                   :certificates (:certificates entry)})
        uncertainty-detail (pattern-uncertainty-details entry (:pattern/id req))
        belief-update (when (:aif-engine entry)
                        (aif-engine/update-beliefs (:aif-engine entry)
                                                   {:decision/id (:decision/id pur)
                                                    :session/id sid
                                                    :pattern/id (:pattern/id req)
                                                    :outcome (:outcome/tags pur)
                                                    :uncertainty (:value uncertainty-detail)
                                                    :status :observed}))]
    (when belief-update
      (tap-aif-context! {:event :use
                         :session/id sid
                         :turn turn
                         :pattern/id (:pattern/id req)
                         :outcome (:outcome/tags pur)
                         :uncertainty uncertainty-detail
                         :aif (:aif belief-update)
                         :mu (aif-mu-summary entry (:pattern/id req))}))
    (mark-mana-depleted! entry turn)
    (when-let [tau (get-in belief-update [:aif :tau-updated])]
      (update-tau-cache! entry (:pattern/id req) tau))
    (when (:mana entry)
      (let [{:keys [restore]} (mana-config (:policy entry))
            restore (double (or restore 10))
            pid (:pattern/id req)
            mana @(:mana entry)
            action? (some #(= pid (:pattern/id %)) actions)
            restore? (and pid
                          action?
                          (not (restored? mana :action->pur pid)))]
        (when restore?
          (apply-mana! entry restore :action->pur {:pattern/id pid
                                                   :turn turn})
          (note-restore! entry :action->pur pid))))
    (append-lab-event! entry {:event/type :pattern/use-claimed
                              :at (fulab-musn/now-inst)
                              :payload {:pur pur}})
    ;; Arxana: Link PUR to used pattern
    (link-pur-to-pattern! sid turn pur)
    (when belief-update
      (append-lab-event! entry (fulab-musn/summary-event sid :pur belief-update)))
    (swap! (:fulab entry) assoc
           :pur pur
           :pur-emitted? true)
    (persist! entry :turn/use req resp)
    (cond-> (assoc resp :pur pur
                   :aif (:aif belief-update))
      (:mana entry) (assoc :mana (mana-view @(:mana entry))))))

(defn evidence-add!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        resp (router/handle-evidence-add! (:state entry)
                                          (compact {:session/id sid
                                                    :turn (:turn req)
                                                    :pattern/id (:pattern/id req)
                                                    :files (:files req)
                                                    :note (:note req)}))]
    (persist! entry :evidence/add req resp)
    resp))

(defn- turn-end-ready?
  [state]
  (or (:plan? state)
      (:selection state)
      (seq (:actions state))))

(defn- turn-summary
  [state]
  {:actions (frequencies (map :action (:actions state)))
   :trail (:trail state)
   :psr (get-in state [:selection :pattern/id])
   :pur (get-in state [:pur :pattern/id])
   :warnings (count (:warnings state))})

(def ^:private aif-auto-use-min-tau 0.55)

(defn- auto-use-policy [state fulab-state]
  (let [tau (or (get-in fulab-state [:psr :selection/reason :aif :tau])
                (get-in state [:hud :aif :tau]))
        deviation (:deviation fulab-state)
        unjustified-deviation? (and deviation (not (:justified? deviation)))
        tau-ok? (or (nil? tau) (>= (double tau) aif-auto-use-min-tau))
        allow? (and tau-ok? (not unjustified-deviation?))]
    (cond-> {:tau tau
             :mode (if allow? :auto-use :abstain)
             :reason (cond
                       unjustified-deviation? "unjustified-deviation"
                       (not tau-ok?) "aif-low-tau"
                       :else "aif-policy")}
      deviation (assoc :deviation deviation))))

(defn- auto-use-allowed? [policy]
  (and (map? policy)
       (not= :abstain (:mode policy))))

(defn turn-end!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        turn (:turn req)
        state @(:state entry)
        fulab-state @(:fulab entry)
        ended-turn (:turn-ended fulab-state)]
    (cond
      (= turn ended-turn)
      (let [resp (assoc (or (:turn-ended-resp fulab-state) {:ok true})
                        :duplicate? true)
            resp (with-donation-summary resp entry turn)]
        (persist! entry :turn/end req resp)
        (cond-> resp
          (:mana entry) (assoc :mana (mana-view @(:mana entry)))))

      (not (turn-end-ready? state))
      (let [resp {:ok true
                  :ignored? true
                  :reason :turn/end-before-ready
                  :summary (turn-summary state)
                  :halt? false
                  :halt/reason nil}
            resp (with-donation-summary resp entry turn)]
        (persist! entry :turn/end req resp)
        (cond-> resp
          (:mana entry) (assoc :mana (mana-view @(:mana entry)))))

      :else
      (let [resp (router/handle-turn-end! (:state entry)
                                          {:session/id sid
                                           :turn (:turn req)})
            fulab-state @(:fulab entry)
            psr (:psr fulab-state)
            pur (:pur fulab-state)
            actions (:actions @(:state entry))
            inferred-action (first (filter #(contains? #{"implement" "update"} (:action %)) actions))
            chosen (or (:chosen psr)
                       (:pattern/id inferred-action))
            inferred? (boolean (and chosen (not pur) inferred-action))
            policy (when inferred?
                     (assoc (auto-use-policy state fulab-state) :chosen chosen))
            allow-auto-use? (or (nil? policy) (auto-use-allowed? policy))
            resp (cond-> resp policy (assoc :aif/policy policy))
            resp (with-donation-summary resp entry turn)]
        (append-lab-event! entry (turn-event :turn/completed sid turn {:session/id sid}))
        (when policy
          (append-lab-event! entry {:event/type :aif/policy
                                    :at (fulab-musn/now-inst)
                                    :payload (select-keys policy [:chosen :tau :mode :reason])}))
        (when policy
          (tap-aif-policy! {:session/id sid
                            :turn turn
                            :policy (select-keys policy [:chosen :tau :mode :reason])}))
        (when (and chosen (not pur) inferred? allow-auto-use?)
          (let [anchors (vec (distinct (concat (action-anchors actions chosen)
                                               [(fulab-musn/turn-anchor turn)])))
                pur (fulab-musn/build-pur {:session-id sid
                                           :turn turn
                                           :pattern-id chosen
                                           :reason "inferred from pattern action"
                                           :anchors anchors
                                           :inferred? true
                                           :certificates (:certificates entry)})
                belief-update (when (:aif-engine entry)
                                (aif-engine/update-beliefs (:aif-engine entry)
                                                           {:decision/id (:decision/id pur)
                                                            :session/id sid
                                                            :outcome (:outcome/tags pur)
                                                            :status :observed}))]
            (when (:mana entry)
              (let [{:keys [restore]} (mana-config (:policy entry))
                    restore (double (or restore 10))
                    mana @(:mana entry)
                    action? (some #(= chosen (:pattern/id %)) actions)
                    restore? (and action?
                                  (not (restored? mana :action->pur chosen)))]
                (when restore?
                  (apply-mana! entry restore :action->pur {:pattern/id chosen
                                                           :turn turn
                                                           :inferred? true})
                  (note-restore! entry :action->pur chosen))))
            (append-lab-event! entry {:event/type :pattern/use-claimed
                                      :at (fulab-musn/now-inst)
                                      :payload {:pur pur}})
            (when belief-update
              (append-lab-event! entry (fulab-musn/summary-event sid :pur belief-update)))
            (swap! (:fulab entry) assoc
                   :pur pur
                   :pur-emitted? true)))
        (when (and chosen (not pur) inferred? (not allow-auto-use?))
          (append-lab-event! entry {:event/type :pattern/use-skipped
                                    :at (fulab-musn/now-inst)
                                    :payload {:session/id sid
                                              :turn turn
                                              :pattern/id chosen
                                              :reason "auto-use skipped (low tau)"}}))
        (when (and psr (not (:pur-emitted? fulab-state)) (not inferred?))
          (append-lab-event! entry {:event/type :pattern/selection-unapplied
                                    :at (fulab-musn/now-inst)
                                    :payload {:session/id sid
                                              :turn turn
                                              :decision/id (:decision/id psr)
                                              :pattern/id (:chosen psr)
                                              :note "No application recorded for selection."}}))
        (swap! (:fulab entry) assoc
               :turn nil
               :psr nil
               :pur nil
               :psr-emitted? false
               :pur-emitted? false
               :deviation nil
               :turn-ended turn
               :turn-ended-resp resp)
        (persist! entry :turn/end req resp)
        (cond-> resp
          (:mana entry) (assoc :mana (mana-view @(:mana entry))))))))

(defn turn-resume!
  [req]
  (let [sid (:session/id req)
        entry (ensure-session! sid nil (:lab/root req) nil)
        resp (router/handle-turn-resume! (:state entry)
                                         (compact {:session/id sid
                                                   :turn (:turn req)
                                                   :note (:note req)}))]
    (persist! entry :turn/resume req resp)
    resp))

(defn session-state!
  "Return a snapshot of the current session state (no mutation)."
  [req]
  (let [sid (:session/id req)]
    (if-let [entry (get-session sid)]
      (let [st @(:state entry)]
        {:ok true
         :session/id sid
         :state (select-keys st [:turn :hud :selection :actions :trail :warnings :halt :plan? :resume-note])
         :logic/summary (logic-audit/summary-for sid)
         :aif/tap (when-let [tap (:aif-tap entry)]
                    @tap)
         :mana (when (:mana entry)
                 (mana-view @(:mana entry)))} )
      {:ok false :err "not-found"})))

(defn chat-message!
  [req]
  (validate! :chat/message-req req)
  (let [room-id (:room req)
        lab-root (or (:lab/root req) default-lab-root)]
    (update-room! room-id lab-root
                  (fn [room]
                    (if-let [cached (cached-response room :chat/message (:msg-id req))]
                      {:room room :resp cached}
                      (let [chat-id (str "chat-" (java.util.UUID/randomUUID))
                            msg {:chat/id chat-id
                                 :room room-id
                                 :msg-id (:msg-id req)
                                 :author (:author req)
                                 :text (:text req)
                                 :at (now-inst)}
                            pause {:reason :chat/message
                                   :by (:author req)
                                   :chat/id chat-id
                                   :msg-id (:msg-id req)
                                   :at (:at msg)}
                            [room event] (append-room-event room
                                                            {:event/type :chat/message
                                                             :payload (assoc msg :paused? true :pause pause)})
                            room (assoc room :paused? true :pause pause)
                            resp {:ok true
                                  :room room-id
                                  :chat msg
                                  :paused? true
                                  :pause pause
                                  :seq (:seq room)}
                            room (remember-response room :chat/message (:msg-id req) resp)]
                        {:room room :event event :resp resp}))))))

(defn chat-bid!
  [req]
  (validate! :chat/bid-req req)
  (let [room-id (:room req)
        lab-root (or (:lab/root req) default-lab-root)]
    (update-room! room-id lab-root
                  (fn [room]
                    (if-let [cached (cached-response room :chat/bid (:msg-id req))]
                      {:room room :resp cached}
                      (let [bid-id (str "bid-" (java.util.UUID/randomUUID))
                            bid {:bid/id bid-id
                                 :room room-id
                                 :msg-id (:msg-id req)
                                 :bidder (:bidder req)
                                 :note (:note req)
                                 :status :pending
                                 :at (now-inst)}
                            bids (trim-bids (conj (vec (:bids room)) bid))
                            [room event] (append-room-event room
                                                            {:event/type :chat/bid
                                                             :payload bid})
                            room (assoc room :bids bids)
                            resp {:ok true
                                  :room room-id
                                  :bid bid
                                  :paused? (:paused? room)
                                  :pause (:pause room)
                                  :seq (:seq room)}
                            room (remember-response room :chat/bid (:msg-id req) resp)]
                        {:room room :event event :resp resp}))))))

(defn chat-unlatch!
  [req]
  (validate! :chat/unlatch-req req)
  (let [room-id (:room req)
        lab-root (or (:lab/root req) default-lab-root)]
    (update-room! room-id lab-root
                  (fn [room]
                    (if-let [cached (cached-response room :chat/unlatch (:msg-id req))]
                      {:room room :resp cached}
                      (let [bid-id (:bid/id req)
                            pending (filter #(= :pending (:status %)) (:bids room))
                            selected (if (seq bid-id)
                                       (some #(when (= bid-id (:bid/id %)) %) pending)
                                       (first pending))]
                        (if-not selected
                          {:room room
                           :resp {:ok false :err "no-pending-bid"}}
                          (let [now (now-inst)
                                accepted (assoc selected
                                                :status :accepted
                                                :accepted/by (:by req)
                                                :accepted/at now)
                                bids (->> (:bids room)
                                          (mapv (fn [b]
                                                  (if (= (:bid/id b) (:bid/id accepted))
                                                    accepted
                                                    b)))
                                          trim-bids)
                                [room event] (append-room-event room
                                                                {:event/type :chat/unlatch
                                                                 :payload {:accepted accepted
                                                                           :by (:by req)
                                                                           :at now}})
                                room (assoc room :paused? false :pause nil :bids bids)
                                resp {:ok true
                                      :room room-id
                                      :accepted accepted
                                      :paused? false
                                      :pause nil
                                      :seq (:seq room)}
                                room (remember-response room :chat/unlatch (:msg-id req) resp)]
                            {:room room :event event :resp resp}))))))))

(defn chat-state!
  [req]
  (validate! :chat/state-req req)
  (let [room-id (:room req)
        lab-root (or (:lab/root req) default-lab-root)
        room (ensure-room @rooms room-id lab-root)
        since (or (:since req) 0)
        events (->> (:events room)
                    (filter #(> (or (:seq %) 0) since))
                    vec)
        state (select-keys room [:paused? :pause :bids :last-activity])]
    (swap! rooms assoc room-id room)
    {:ok true
     :room room-id
     :state state
     :events events
     :cursor (:seq room)}))

;; =============================================================================
;; Scribe Functions - Record and retrieve conversation turns for HUD context
;; =============================================================================

(defn record-turn!
  "Record a conversation turn (user or agent message) to session.
   Role should be :user or :agent. Content is truncated to 500 chars for storage."
  [session-id role content]
  (when-let [entry (get-session session-id)]
    (let [truncated (if (> (count content) 500)
                      (str (subs content 0 500) "...")
                      content)
          event {:event/type (case role
                               :user :turn/user
                               :agent :turn/agent
                               :turn/unknown)
                 :at (fulab-musn/now-inst)
                 :payload {:role role :content truncated}}]
      (append-lab-event! entry event)
      {:ok true :event/type (:event/type event)})))

(defn record-plan!
  "Record a plan event with optional Mermaid diagram to session."
  [session-id note diagram]
  (when-let [entry (get-session session-id)]
    (let [event {:event/type :turn/plan
                 :at (fulab-musn/now-inst)
                 :payload (cond-> {}
                            note (assoc :note note)
                            diagram (assoc :diagram diagram))}]
      (append-lab-event! entry event)
      {:ok true :event/type :turn/plan})))

(defn record-plan-wiring!
  "Record a plan event from EDN wiring diagram, auto-generating Mermaid.
   The wiring should have :nodes, :edges, and :output keys."
  [session-id note wiring]
  (when-let [entry (get-session session-id)]
    (let [mermaid (try
                    (require 'futon3.musn.plan-wiring)
                    ((resolve 'futon3.musn.plan-wiring/plan->mermaid) wiring)
                    (catch Exception e
                      (str "flowchart LR\n    error[\"Failed to generate: " (.getMessage e) "\"]")))
          event {:event/type :turn/plan
                 :at (fulab-musn/now-inst)
                 :payload (cond-> {:wiring wiring}
                            note (assoc :note note)
                            mermaid (assoc :diagram mermaid))}]
      (append-lab-event! entry event)
      {:ok true :event/type :turn/plan :mermaid? (some? mermaid)})))

(defn recent-turns
  "Get recent conversation turns from session for HUD context.
   Returns up to n most recent :turn/user and :turn/agent events."
  [session-id n]
  (when-let [entry (get-session session-id)]
    (->> @(:lab-session entry)
         :events
         (filter #(#{:turn/user :turn/agent} (:event/type %)))
         (take-last n)
         vec)))

(defn scribe-events
  "Get scribe events (turns + plans + arxana) from session.
   Returns all matching events in session order."
  [session-id]
  (when-let [entry (get-session session-id)]
    (->> @(:lab-session entry)
         :events
         (filter #(#{:turn/user :turn/agent :turn/plan :planning/native-detected
                     :arxana/anchor-created :arxana/link-created :session/par} (:event/type %)))
         vec)))

(defn recent-scribe-events
  "Get recent scribe events (turns + plans + arxana) from session for notebook display.
   Returns up to n most recent turn, plan, planning-hint, and anchor/link events."
  [session-id n]
  (some-> (scribe-events session-id)
          (take-last n)
          vec))

(defn turns->context
  "Convert recent turns to a context string for HUD sigil computation."
  [session-id n]
  (when-let [turns (recent-turns session-id n)]
    (->> turns
         (map #(get-in % [:payload :content]))
         (str/join " "))))

;; =============================================================================
;; Native Plan Detection - Convert Claude's TaskCreate/TaskUpdate to EDN plans
;; =============================================================================

(defn- task->node
  "Convert a native task to a plan wiring node."
  [task idx]
  (let [task-id (or (:id task) (str "task-" idx))
        subject (or (:subject task) "unnamed task")]
    {:id (keyword task-id)
     :component :musn.plan/action-reasoning
     :params {:note subject
              :desc (:description task)}}))

(defn- tasks->edges
  "Convert task dependencies (blockedBy) to plan wiring edges."
  [tasks]
  (->> tasks
       (mapcat (fn [task]
                 (when-let [blocked-by (:blockedBy task)]
                   (map (fn [dep]
                          {:from (keyword dep)
                           :to (keyword (:id task))})
                        blocked-by))))
       (remove nil?)
       vec))

(defn- tasks->plan-wiring
  "Convert a list of native tasks to plan wiring format.
   Returns an EDN plan structure suitable for record-plan-wiring!"
  [tasks]
  (let [tasks (vec tasks)
        nodes (map-indexed (fn [idx task] (task->node task idx)) tasks)
        edges (tasks->edges tasks)
        ;; Output is the last task without any blockers of its own
        blocking-ids (set (mapcat :blockedBy tasks))
        terminal-tasks (->> tasks
                            (remove #(contains? blocking-ids (:id %)))
                            (filter #(or (nil? (:status %))
                                         (= "pending" (:status %))
                                         (= "in_progress" (:status %)))))
        output-id (or (some-> (first terminal-tasks) :id keyword)
                      (some-> (last tasks) :id keyword))]
    {:nodes (vec nodes)
     :edges edges
     :output output-id
     :source :native-task-list}))

(defn- tasks->mermaid
  "Convert native tasks to a Mermaid flowchart."
  [tasks]
  (try
    (require 'futon3.musn.plan-wiring)
    (let [wiring (tasks->plan-wiring tasks)]
      ((resolve 'futon3.musn.plan-wiring/plan->mermaid) wiring {:direction :TD}))
    (catch Exception e
      (str "flowchart TD\n    error[\"Failed to generate: " (.getMessage e) "\"]"))))

(defn record-native-plan!
  "Record a plan event derived from Claude's native TaskCreate/TaskUpdate tools.
   Converts the task list to EDN wiring format and generates a Mermaid diagram.

   Tasks should be a sequence of maps with :id, :subject, :description, :status,
   and optionally :blockedBy (list of task IDs this task depends on)."
  [session-id tasks & {:keys [note]}]
  (when-let [entry (get-session session-id)]
    (let [wiring (tasks->plan-wiring tasks)
          mermaid (tasks->mermaid tasks)
          note (or note "Plan derived from native task list")
          event {:event/type :turn/plan
                 :at (fulab-musn/now-inst)
                 :payload {:source :native-task-list
                           :task-count (count tasks)
                           :note note
                           :wiring wiring
                           :diagram mermaid}}]
      (append-lab-event! entry event)
      {:ok true
       :event/type :turn/plan
       :source :native-task-list
       :task-count (count tasks)})))

(defn note-native-planning-detected!
  "Record that native planning tools (TaskCreate/TaskUpdate) were detected.
   This is a hint to the agent to generate an EDN plan sketch after the fact."
  [session-id tool-name & {:keys [task-id subject]}]
  (when-let [entry (get-session session-id)]
    (let [event {:event/type :planning/native-detected
                 :at (fulab-musn/now-inst)
                 :payload {:tool tool-name
                           :task-id task-id
                           :subject subject
                           :hint "Consider generating an EDN plan sketch for the plan viewer"}}]
      (append-lab-event! entry event)
      {:ok true :event/type :planning/native-detected})))

;; =============================================================================
;; Activity Logging - Unified agent activity tracking for vitality monitoring
;; =============================================================================

(def ^:private activity-log-path
  (io/file (or (System/getenv "MUSN_LAB_ROOT") "lab") "activity" "log.ndjson"))

;; WebSocket clients subscribed to activity stream
(defonce activity-ws-clients (atom #{}))

(defn register-activity-client! [channel]
  (swap! activity-ws-clients conj channel))

(defn unregister-activity-client! [channel]
  (swap! activity-ws-clients disj channel))

(defn broadcast-activity! [record]
  "Broadcast an activity record to all connected WebSocket clients."
  (let [msg (json/generate-string record)]
    (doseq [ch @activity-ws-clients]
      (try
        (when (http/open? ch)
          (http/send! ch msg false))
        (catch Exception _ nil)))))

(defn activity-log!
  "Log agent activity for vitality monitoring. All agents (Claude, Codex, etc.)
   should POST here to create a unified activity stream.

   Required fields:
     :agent    - keyword, e.g. :claude, :codex, :fucodex
     :source   - keyword, e.g. :claude-code, :aob-chatgpt, :codex-cli

   Optional fields:
     :session/id - session identifier if within a session
     :at         - ISO timestamp (defaults to now)
     :event/type - defaults to :agent/interaction
     :metadata   - arbitrary map of additional data

   Example:
     POST /musn/activity/log
     {\"agent\": \"claude\", \"source\": \"claude-code\",
      \"session/id\": \"abc123\", \"metadata\": {\"cwd\": \"/home/joe/code\"}}
  "
  [{:keys [agent source session/id at metadata] :as body}]
  (if (or (nil? agent) (nil? source))
    {:ok false :err "missing required field: agent or source"}
    (let [timestamp (or at (str (fulab-musn/now-inst)))
          event-type (or (:event/type body) :agent/interaction)
          record {:event/type event-type
                  :agent (keyword agent)
                  :source (keyword source)
                  :at timestamp
                  :session/id id
                  :metadata metadata}]
      (try
        (io/make-parents activity-log-path)
        (spit activity-log-path
              (str (pr-str record) "\n")
              :append true)
        ;; Broadcast to WebSocket clients
        (broadcast-activity! record)
        ;; Process for affect signals (async, fire-and-forget)
        (future
          (try
            (binding [*out* *err*]  ;; ensure output goes to stderr
              (println "[affect] Processing activity:" (:agent record))
              (try
                (require 'futon3a.affect)
                (catch java.io.FileNotFoundException _
                  (println "[affect] Skipping (futon3a not on classpath)")
                  (throw (ex-info "affect-unavailable" {}))))
              (let [proc-fn (resolve 'futon3a.affect/process!)]
                (when proc-fn
                  (proc-fn record)
                  (println "[affect] Done processing"))))
            (catch clojure.lang.ExceptionInfo e
              (when-not (= "affect-unavailable" (ex-message e))
                (binding [*out* *err*]
                  (println "[affect] Error:" (.getMessage e))
                  (.printStackTrace e))))
            (catch Throwable t
              (binding [*out* *err*]
                (println "[affect] Error:" (.getMessage t))
                (.printStackTrace t)))))
        {:ok true :logged record}
        (catch Throwable t
          {:ok false :err (.getMessage t)})))))

(defn activity-log-entries
  "Read activity log entries. Returns a lazy seq of records."
  ([] (activity-log-entries nil))
  ([{:keys [since agent source limit]}]
   (when (.exists activity-log-path)
     (let [entries (->> (slurp activity-log-path)
                        str/split-lines
                        (remove str/blank?)
                        (map edn/read-string))]
       (cond->> entries
         agent (filter #(= (keyword agent) (:agent %)))
         source (filter #(= (keyword source) (:source %)))
         since (filter #(pos? (compare (:at %) since)))
         limit (take limit))))))

;; =============================================================================
;; Arxana Graph Persistence - Anchors and Links for Lab Notebooks
;; =============================================================================

(defn- anchors-index-path [lab-root]
  (io/file lab-root "anchors" "index.edn"))

(defn- links-graph-path [lab-root]
  (io/file lab-root "links" "graph.edn"))

(defn- generate-anchor-id [session-id turn anchor-type idx]
  (format "%s:turn-%s:%s-%s" session-id turn (name anchor-type) idx))

(defn- read-anchors-index [lab-root]
  (let [path (anchors-index-path lab-root)]
    (when (.exists path)
      (try
        (->> (slurp path)
             str/split-lines
             (remove str/blank?)
             (map edn/read-string)
             vec)
        (catch Throwable _ [])))))

(defn- append-anchor! [lab-root anchor]
  (let [path (anchors-index-path lab-root)]
    (io/make-parents path)
    (spit path (str (pr-str anchor) "\n") :append true)))

(defn- read-links-graph [lab-root]
  (let [path (links-graph-path lab-root)]
    (when (.exists path)
      (try
        (->> (slurp path)
             str/split-lines
             (remove str/blank?)
             (map edn/read-string)
             vec)
        (catch Throwable _ [])))))

(defn- append-link! [lab-root link]
  (let [path (links-graph-path lab-root)]
    (io/make-parents path)
    (spit path (str (pr-str link) "\n") :append true)))

(defn- normalize-turn
  "Normalize turn values for anchor storage/filtering."
  [turn]
  (cond
    (integer? turn) turn
    (string? turn) (try
                     (Integer/parseInt (str/trim turn))
                     (catch Exception _ turn))
    :else turn))

(defn record-anchor!
  "Record an anchor (named point) within a session turn.
   Anchor types: :insight, :decision, :question, :artifact
   Returns the anchor record with generated ID."
  [session-id turn anchor-type content & {:keys [note author]}]
  (when-let [entry (get-session session-id)]
    (let [turn (normalize-turn turn)
          lab-root (:lab-root entry)
          existing (or (read-anchors-index lab-root) [])
          session-anchors (filter #(= session-id (:anchor/session %)) existing)
          turn-anchors (filter #(= turn (:anchor/turn %)) session-anchors)
          type-anchors (filter #(= anchor-type (:anchor/type %)) turn-anchors)
          idx (inc (count type-anchors))
          base-id (generate-anchor-id session-id turn anchor-type idx)
          existing-ids (set (map :anchor/id existing))
          anchor-id (if (contains? existing-ids base-id)
                      (str base-id "-" (subs (str (java.util.UUID/randomUUID)) 0 6))
                      base-id)
          truncated-content (if (> (count content) 500)
                              (str (subs content 0 500) "...")
                              content)
          anchor {:anchor/id anchor-id
                  :anchor/session session-id
                  :anchor/turn turn
                  :anchor/type anchor-type
                  :anchor/content truncated-content
                  :anchor/created (fulab-musn/now-inst)
                  :anchor/author (or author "agent")}
          anchor (if note (assoc anchor :anchor/note note) anchor)
          event {:event/type :arxana/anchor-created
                 :at (fulab-musn/now-inst)
                 :payload {:anchor/id anchor-id
                           :anchor/type anchor-type
                           :anchor/turn turn}}]
      (append-anchor! lab-root anchor)
      (append-lab-event! entry event)
      {:ok true :anchor anchor})))

(defn create-link!
  "Create a typed link between two anchors.
   Link types: :supports, :contradicts, :extends, :implements, :references
   Returns the link record."
  [from-anchor to-anchor link-type & {:keys [note author]}]
  ;; Find session for from-anchor to get lab-root
  (let [from-session (first (str/split from-anchor #":"))
        entry (get-session from-session)]
    (if-not entry
      {:ok false :err "session-not-found" :session from-session}
      (let [lab-root (:lab-root entry)
            existing-anchors (or (read-anchors-index lab-root) [])
            from-exists? (some #(= from-anchor (:anchor/id %)) existing-anchors)
            to-exists? (some #(= to-anchor (:anchor/id %)) existing-anchors)
            link-id (str "link-" (subs (str (java.util.UUID/randomUUID)) 0 8))
            link {:link/id link-id
                  :link/from from-anchor
                  :link/to to-anchor
                  :link/type link-type
                  :link/created (fulab-musn/now-inst)
                  :link/author (or author "agent")}
            link (if note (assoc link :link/note note) link)]
        (when (or from-exists? to-exists?) ;; Allow forward references
          (append-link! lab-root link)
          (append-lab-event! entry {:event/type :arxana/link-created
                                    :at (fulab-musn/now-inst)
                                    :payload {:link/id link-id
                                              :link/from from-anchor
                                              :link/to to-anchor
                                              :link/type link-type}}))
        (if (or from-exists? to-exists?)
          {:ok true :link link}
          {:ok false :err "no-anchor-exists" :from from-anchor :to to-anchor})))))

;; =============================================================================
;; PSR/PUR → Pattern Arxana Linking
;; =============================================================================

(defn link-psr-to-pattern!
  "Create an Arxana anchor for a PSR and link it to the selected pattern.
   Returns {:ok true :anchor ... :link ...} or nil if linking fails."
  [session-id turn psr]
  (when-let [pattern-id (:chosen psr)]
    (let [content (format "PSR: Selected pattern '%s' from candidates %s"
                          pattern-id
                          (pr-str (:candidates psr)))
          anchor-result (record-anchor! session-id turn :decision content
                                        :note (str "PSR " (:psr/id psr))
                                        :author "musn")]
      (when (:ok anchor-result)
        (let [anchor-id (get-in anchor-result [:anchor :anchor/id])
              ;; Create a pattern anchor ID (virtual - patterns don't have sessions)
              pattern-anchor-id (str "pattern:" pattern-id)
              link-result (create-link! anchor-id pattern-anchor-id :applies-pattern
                                        :note (format "PSR selected %s" pattern-id)
                                        :author "musn")]
          {:ok true
           :anchor (:anchor anchor-result)
           :link (:link link-result)
           :pattern-id pattern-id})))))

(defn link-pur-to-pattern!
  "Create an Arxana anchor for a PUR and link it to the used pattern.
   Returns {:ok true :anchor ... :link ...} or nil if linking fails."
  [session-id turn pur]
  (when-let [pattern-id (:pattern/id pur)]
    (let [content (format "PUR: Used pattern '%s' - %s"
                          pattern-id
                          (or (:use/reason pur) ""))
          anchor-result (record-anchor! session-id turn :artifact content
                                        :note (str "PUR " (:pur/id pur))
                                        :author "musn")]
      (when (:ok anchor-result)
        (let [anchor-id (get-in anchor-result [:anchor :anchor/id])
              pattern-anchor-id (str "pattern:" pattern-id)
              link-result (create-link! anchor-id pattern-anchor-id :implements
                                        :note (format "PUR used %s" pattern-id)
                                        :author "musn")]
          {:ok true
           :anchor (:anchor anchor-result)
           :link (:link link-result)
           :pattern-id pattern-id})))))

(defn get-anchors
  "Get anchors for a session, optionally filtered by turn.
   Reads from global anchor index - does not require active session."
  [session-id & {:keys [turn]}]
  (let [lab-root (if-let [entry (get-session session-id)]
                   (:lab-root entry)
                   default-lab-root)
        anchors (or (read-anchors-index lab-root) [])
        session-anchors (filter #(= session-id (:anchor/session %)) anchors)]
    (if turn
      (let [turn (normalize-turn turn)]
        (filter #(= turn (:anchor/turn %)) session-anchors))
      session-anchors)))

(defn get-links
  "Get links for an anchor (outgoing and incoming).
   Reads from global link graph - does not require active session."
  [anchor-id]
  (let [session-id (first (str/split anchor-id #":"))
        lab-root (if-let [entry (get-session session-id)]
                   (:lab-root entry)
                   default-lab-root)
        links (or (read-links-graph lab-root) [])]
    {:outgoing (filter #(= anchor-id (:link/from %)) links)
     :incoming (filter #(= anchor-id (:link/to %)) links)}))

(defn get-backlinks
  "Get all anchors that link TO the given anchor."
  [anchor-id]
  (let [links (get-links anchor-id)]
    (when links
      (->> (:incoming links)
           (map :link/from)
           vec))))

(defn get-pattern-backlinks
  "Get all links pointing TO a pattern (pattern:namespace/name).
   Scans all known lab roots for links.
   Returns {:pattern-id :links [{:link/from :link/type :session :anchor-content}...]}"
  [pattern-id]
  (let [pattern-anchor (str "pattern:" pattern-id)
        lab-root (io/file default-lab-root)
        links-file (io/file lab-root "links" "graph.edn")
        anchors-file (io/file lab-root "anchors" "index.edn")]
    (when (.exists links-file)
      (let [all-links (->> (slurp links-file)
                           str/split-lines
                           (remove str/blank?)
                           (map edn/read-string))
            matching-links (filter #(= pattern-anchor (:link/to %)) all-links)
            ;; Enrich with anchor content
            anchors (when (.exists anchors-file)
                      (->> (slurp anchors-file)
                           str/split-lines
                           (remove str/blank?)
                           (map edn/read-string)))
            anchor-map (into {} (map (juxt :anchor/id identity) anchors))]
        {:pattern-id pattern-id
         :pattern-anchor pattern-anchor
         :count (count matching-links)
         :links (mapv (fn [link]
                        (let [from-anchor (get anchor-map (:link/from link))]
                          {:link/id (:link/id link)
                           :link/from (:link/from link)
                           :link/type (:link/type link)
                           :link/created (:link/created link)
                           :link/note (:link/note link)
                           :anchor/session (:anchor/session from-anchor)
                           :anchor/turn (:anchor/turn from-anchor)
                           :anchor/type (:anchor/type from-anchor)
                           :anchor/content (:anchor/content from-anchor)}))
                      matching-links)}))))

;; =============================================================================
;; Semantic Anchor Linking (futon3a integration)
;; =============================================================================

(defn- anchor-text
  "Extract searchable text from an anchor."
  [anchor]
  (str (:anchor/content anchor)
       (when-let [note (:anchor/note anchor)]
         (str " " note))))

(defn suggest-similar-anchors
  "Find anchors semantically similar to the given anchor content.
   Uses futon3a portal if available, falls back to simple text matching.
   Returns vector of {:anchor/id :similarity} sorted by similarity."
  [session-id anchor-id & {:keys [limit] :or {limit 5}}]
  (when-let [entry (get-session session-id)]
    (let [lab-root (:lab-root entry)
          all-anchors (or (read-anchors-index lab-root) [])
          target-anchor (first (filter #(= anchor-id (:anchor/id %)) all-anchors))
          other-anchors (remove #(= anchor-id (:anchor/id %)) all-anchors)]
      (when target-anchor
        (let [target-text (anchor-text target-anchor)
              target-words (set (str/split (str/lower-case target-text) #"\s+"))
              ;; Simple word overlap similarity (fallback when futon3a unavailable)
              scored (->> other-anchors
                          (map (fn [anchor]
                                 (let [anchor-words (set (str/split (str/lower-case (anchor-text anchor)) #"\s+"))
                                       intersection (set/intersection target-words anchor-words)
                                       union (set/union target-words anchor-words)
                                       jaccard (if (empty? union) 0.0
                                                   (/ (count intersection) (double (count union))))]
                                   {:anchor/id (:anchor/id anchor)
                                    :anchor/type (:anchor/type anchor)
                                    :anchor/session (:anchor/session anchor)
                                    :similarity jaccard})))
                          (filter #(> (:similarity %) 0.1))
                          (sort-by :similarity >)
                          (take limit)
                          vec)]
          scored)))))

(defn suggest-links-for-anchor
  "Suggest potential links for an anchor based on semantic similarity.
   Returns candidate links with suggested types based on anchor types."
  [session-id anchor-id & {:keys [limit] :or {limit 3}}]
  (when-let [similar (suggest-similar-anchors session-id anchor-id :limit limit)]
    (let [anchors (or (read-anchors-index (:lab-root (get-session session-id))) [])
          source-anchor (first (filter #(= anchor-id (:anchor/id %)) anchors))
          source-type (:anchor/type source-anchor)]
      (->> similar
           (map (fn [entry]
                  (let [target-id (:anchor/id entry)
                        target-type (:anchor/type entry)
                        similarity (:similarity entry)
                        ;; Suggest link type based on anchor types
                        suggested-type (cond
                                         (and (= source-type :insight) (= target-type :decision)) :supports
                                         (and (= source-type :decision) (= target-type :insight)) :implements
                                         (and (= source-type :question) (= target-type :insight)) :extends
                                         (= source-type target-type) :references
                                         :else :references)]
                    {:from anchor-id
                     :to target-id
                     :suggested-type suggested-type
                     :similarity similarity})))
           vec))))

(defn auto-link-anchors!
  "Automatically create links between semantically similar anchors.
   Only creates links above the similarity threshold."
  [session-id & {:keys [threshold limit] :or {threshold 0.3 limit 10}}]
  (when-let [entry (get-session session-id)]
    (let [lab-root (:lab-root entry)
          all-anchors (or (read-anchors-index lab-root) [])
          session-anchors (filter #(= session-id (:anchor/session %)) all-anchors)
          existing-links (or (read-links-graph lab-root) [])
          existing-pairs (set (map (fn [link] [(:link/from link) (:link/to link)]) existing-links))
          created (atom [])]
      (doseq [anchor session-anchors]
        (let [suggestions (suggest-links-for-anchor session-id (:anchor/id anchor) :limit limit)]
          (doseq [{:keys [from to suggested-type similarity]} suggestions]
            (when (and (>= similarity threshold)
                       (not (contains? existing-pairs [from to]))
                       (not (contains? existing-pairs [to from])))
              (let [result (create-link! from to suggested-type
                                         :note (format "Auto-linked (similarity: %.2f)" similarity)
                                         :author "arxana-auto")]
                (when (:ok result)
                  (swap! created conj (:link result))))))))
      {:ok true :links-created (count @created) :links @created})))

;; =============================================================================
;; PAR (Post-Action Review) events
;; =============================================================================

(defn- par-question-line [label value]
  (when (and (string? value) (not (str/blank? value)))
    (format "%s: %s" label value)))

(defn- par->forum-body [par-event]
  (let [tags (->> (:par/tags par-event) (map name) (remove str/blank?) vec)
        span (:par/span par-event)
        span-line (when (or (:from-eid span) (:to-eid span))
                    (format "Span: %s → %s"
                            (or (:from-eid span) "?")
                            (or (:to-eid span) "?")))
        questions (:par/questions par-event)
        lines (->> [(format "PAR #%s" (:par/sequence par-event))
                    (format "Session: %s" (:session/id par-event))
                    (when (seq tags) (format "Tags: %s" (str/join ", " tags)))
                    span-line
                    ""
                    (par-question-line "Intention" (get questions :intention))
                    (par-question-line "Happening" (get questions :happening))
                    (par-question-line "Perspectives" (get questions :perspectives))
                    (par-question-line "Learned" (get questions :learned))
                    (par-question-line "Forward" (get questions :forward))]
                   (remove nil?))]
    (str/join "\n" lines)))

(defn- ensure-forum-thread!
  [entry forum par-event]
  (let [existing (get entry :forum/thread-id)
        thread-id (or (:thread-id forum) existing)
        forum (or forum {})
        author (or (:author forum) (some-> (:client-id entry) str) "musn")
        title (or (:title forum)
                  (format "Session %s PARs" (:id entry)))
        goal (:goal forum)
        tags (normalize-tags (concat (:tags forum) [:session/par]))]
    (if thread-id
      {:thread-id thread-id :author author}
      (try
        (require 'futon3.forum.service)
        (when-let [create-thread (resolve 'futon3.forum.service/create-thread!)]
          (let [body (format "Auto-created PAR thread for session %s." (:id entry))
                result (create-thread {:title title
                                       :author author
                                       :body body
                                       :goal goal
                                       :tags tags})
                new-thread-id (get-in result [:thread :thread/id])]
            (when new-thread-id
              (swap! sessions assoc (:id entry) (assoc entry :forum/thread-id new-thread-id))
              {:thread-id new-thread-id :author author})))
        (catch Throwable t
          (binding [*out* *err*]
            (println "[musn] forum thread creation failed:" (.getMessage t)))
          nil)))))

(defn- relay-par-to-forum!
  [entry forum par-event]
  (when (or forum (:forum/thread-id entry))
    (try
      (require 'futon3.forum.service)
      (when-let [create-post (resolve 'futon3.forum.service/create-post!)]
        (when-let [{:keys [thread-id author]} (ensure-forum-thread! entry forum par-event)]
          (create-post {:thread-id thread-id
                        :author author
                        :body (par->forum-body par-event)
                        :claim-type :step
                        :pattern-applied "session/par"
                        :tags (normalize-tags (concat (:par/tags par-event) [:par]))})
          {:thread-id thread-id}))
      (catch Throwable t
        (binding [*out* *err*]
          (println "[musn] forum relay failed:" (.getMessage t)))))
    nil))

(defn create-par!
  "Create a Post-Action Review event for session punctuation.

   Tags can include :checkpoint, :detach, :reattach.
   Span references the event range covered by this PAR.
   Questions follow the 5-question PAR template:
   - intention: what we expected to learn/make
   - happening: what and how we're learning
   - perspectives: different views on what's happening
   - learned: what we learned or changed
   - forward: what else should we change

   Returns the PAR event."
  [session-id & {:keys [span-from span-to questions tags forum]
                 :or {tags [:checkpoint]}}]
  (when-let [entry (get-session session-id)]
    (let [par-id (str "par-" (subs (str (java.util.UUID/randomUUID)) 0 8))
          events (:events entry)
          sequence (inc (count (filter #(= :session/par (:event/type %)) events)))
          now (fulab-musn/now-inst)
          from-ts (when span-from
                    (:at (first (filter #(= span-from (:event/id %)) events))))
          par-event {:event/type :session/par
                     :at now
                     :par/id par-id
                     :par/sequence sequence
                     :par/span (compact {:from-eid span-from
                                         :to-eid span-to
                                         :from-ts from-ts
                                         :to-ts now})
                     :par/questions (or questions {})
                     :par/tags (normalize-tags tags)
                     :session/id session-id}]
      (append-lab-event! entry par-event)
      (let [forum-result (relay-par-to-forum! entry forum par-event)]
        {:ok true :par par-event :forum forum-result}))))

(defn par!
  "HTTP entrypoint for PAR creation."
  [req]
  (validate! :par/create-req req)
  (let [span (:par/span req)]
    (create-par! (:session/id req)
                 :span-from (:from-eid span)
                 :span-to (:to-eid span)
                 :questions (:par/questions req)
                 :tags (:par/tags req)
                 :forum (:forum req))))

(defn get-pars
  "Get all PAR events for a session."
  [session-id]
  (when-let [entry (get-session session-id)]
    (->> (:events entry)
         (filter #(= :session/par (:event/type %)))
         (sort-by :par/sequence)
         vec)))

(defn latest-par
  "Get the most recent PAR for a session."
  [session-id]
  (last (get-pars session-id)))
