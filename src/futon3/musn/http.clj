(ns futon3.musn.http
  "HTTP adapter for MUSN service. Provides JSON endpoints for session + turn lifecycle."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.edn :as edn]
            [org.httpkit.server :as http]
            [org.httpkit.client :as http-client]
            [clojure.string :as str]
            [futon3.aif.viz :as aif-viz]
            [futon3.musn.service :as svc]
            [futon3.fulab.hud :as hud]
            [futon3.pattern-hints :as hints]
            [futon3.portal :as portal]
            [futon.compass :as compass]))

(def log-path (or (System/getenv "MUSN_LOG") "/tmp/musn_http.log"))

(defn- log!
  "Append a timestamped line to the MUSN log file, ignore failures."
  [msg & more]
  (try
    (spit log-path
          (str (java.time.Instant/now) " " msg
               (when (seq more) (str " " (pr-str more)))
               "\n")
          :append true)
    (catch Throwable _)))

(defn- keywordize-enums
  "Convert certain string enum values to keywords so they pass schema validation."
  [data]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {}
             (map (fn [[k v]]
                    (cond
                      (and (#{:mode :source :cost} k) (string? v))
                      [k (keyword v)]

                      :else [k v])))
             x)
       x))
   data))

(defn- jsonable
  "Transform response payload into JSON-friendly values (e.g., stringifying Instants)."
  [data]
  (walk/postwalk
   (fn [x]
     (cond
       (instance? java.time.Instant x) (str x)
       :else x))
   data))

(defn- parse-json-body [req]
  (let [body (slurp (:body req))]
    (when (seq body)
      (-> (json/parse-string body true)
          (keywordize-enums)))))

(defn- json-response
  ([payload] (json-response 200 payload))
  ([status payload]
   {:status status
    :headers {"content-type" "application/json"}
    :body (json/generate-string (jsonable payload))}))

(defn- compass-request
  [body]
  (let [narrative (or (:narrative body) (:text body) (:query body))
        top-k (some-> (:top-k body) long)
        sim-steps (some-> (:sim-steps body) long)
        seed (some-> (:seed body) long)
        method (let [m (:method body)]
                 (if (string? m) (keyword m) m))]
    (if (str/blank? (str narrative))
      {:ok false :err "missing narrative"}
      (apply compass/compass-report
             narrative
             (cond-> []
               top-k (conj :top-k top-k)
               sim-steps (conj :sim-steps sim-steps)
               seed (conj :seed seed)
               method (conj :method method))))))

(defn- hud-build-request [body]
  (let [intent (:intent body)
        pattern-limit (or (:pattern-limit body) 8)
        namespaces (or (:namespaces body) ["musn" "fulab" "aif" "agent"])]
    (if (str/blank? intent)
      {:ok false :err "missing intent"}
      (try
        {:ok true
         :hud (hud/build-hud {:intent intent
                              :pattern-limit pattern-limit
                              :namespaces namespaces})}
        (catch Throwable t
          {:ok false :err (.getMessage t)})))))

(defn- pattern-search-request
  "Search patterns by intent text. Returns ranked candidates for PSR selection.
   Tries MiniLM semantic search first, falls back to GloVe + sigil matching."
  [body]
  (let [intent (or (:intent body) (:query body) (:text body))
        limit (or (:limit body) 8)
        namespace (:namespace body)
        sigils (:sigils body)]
    (if (and (str/blank? intent) (empty? sigils))
      {:ok false :err "missing intent or sigils"}
      (try
        (let [;; Try MiniLM semantic search first (best quality)
              minilm-results (when (seq intent)
                               (portal/minilm-suggest {:intent intent :limit limit}))
              ;; Also get hints (GloVe + sigil) for comparison/fallback
              hint-result (hints/hints {:intent intent
                                        :sigils sigils
                                        :pattern-limit limit
                                        :portal-limit limit
                                        :portal-namespace namespace})
              glove-patterns (:glove-patterns hint-result)
              ;; Enrich MiniLM results with pattern metadata
              minilm-enriched (mapv (fn [entry]
                                      (let [meta (hints/pattern-entry (:id entry))]
                                        (merge entry
                                               (select-keys meta [:summary :sigils
                                                                  :maturity/phase :precision/prior
                                                                  :source/path]))))
                                    minilm-results)
              ;; Use MiniLM if available, else fall back to hints
              patterns (if (seq minilm-enriched)
                         minilm-enriched
                         (:patterns hint-result))
              method (cond
                       (seq minilm-enriched) :minilm
                       (seq (:patterns hint-result)) (or (some-> (first (:patterns hint-result)) :score-source) :combined)
                       (seq glove-patterns) :glove
                       :else :none)]
          {:ok true
           :method method
           :minilm-available (portal/minilm-available?)
           :portal-available (portal/available?)
           :patterns (vec patterns)
           :minilm-patterns (vec minilm-enriched)
           :glove-patterns (vec glove-patterns)
           :fruits (:fruits hint-result)
           :paramitas (:paramitas hint-result)})
        (catch Throwable t
          {:ok false
           :err (.getMessage t)
           :type (.getSimpleName (class t))})))))

(def ^:private personal-api-base
  (or (System/getenv "PERSONAL_API_URL") "http://127.0.0.1:7778"))

(def ^:private default-priors
  "Default priors for cold start (168h week allocation)."
  {:sleep 56 :maintenance 24 :slack 28
   :q1 8 :q2 15 :q3 12 :q4 25})

(defn- ewma-priors
  "Compute EWMA priors from historical weeks.
   alpha = weight for most recent observation (default 0.3).
   Falls back to default-priors if no history."
  [history & {:keys [alpha] :or {alpha 0.3}}]
  (if (empty? history)
    default-priors
    (let [;; Get clears from each historical week, most recent first
          historical-clears (keep :clears (reverse history))]
      (if (empty? historical-clears)
        default-priors
        ;; Compute EWMA: start from oldest, weight newer observations more
        (reduce
         (fn [prior clears]
           (let [all-keys (set (concat (keys prior) (keys clears)))]
             (into {}
                   (for [k all-keys]
                     (let [p (get prior k 0)
                           c (get clears k 0)
                           pv (if (number? p) p 0)
                           cv (if (number? c) c 0)]
                       ;; EWMA: new = alpha * observation + (1-alpha) * prior
                       [k (+ (* alpha cv) (* (- 1 alpha) pv))])))))
         default-priors
         historical-clears)))))

(defn- weeks-since
  "Compute weeks between two ISO date strings (YYYY-MM-DD).
   Returns nil if either date is invalid."
  [from-date to-date]
  (try
    (let [from (java.time.LocalDate/parse from-date)
          to (java.time.LocalDate/parse to-date)]
      (Math/abs (.between java.time.temporal.ChronoUnit/WEEKS from to)))
    (catch Throwable _ nil)))

(defn- compute-meta-vitality
  "Compute meta-level vitality: engagement and delivery priors.
   - user_engagement: has user bid recently?
   - system_delivery: has system delivered value recently?"
  [current-week history-data activity-entries]
  (let [today (str (java.time.LocalDate/now))
        ;; Find most recent week with non-empty bids
        weeks-with-bids (filter (fn [w]
                                  (let [bids (:bids w)]
                                    (and (map? bids) (seq bids))))
                                history-data)
        last-bid-week (or (:week-id (first weeks-with-bids))
                          (when (seq (:bids current-week)) (:week-id current-week)))
        weeks-since-bid (when last-bid-week
                          (weeks-since last-bid-week today))
        ;; System delivery: check MUSN activity entries
        last-activity (first activity-entries)
        last-delivery-date (when last-activity
                             (some-> (:at last-activity)
                                     (subs 0 10)))  ; Extract YYYY-MM-DD
        weeks-since-delivery (when last-delivery-date
                               (weeks-since last-delivery-date today))
        ;; Compute statuses
        user-status (cond
                      (nil? weeks-since-bid) "no_data"
                      (zero? weeks-since-bid) "on_track"
                      (= 1 weeks-since-bid) "due"
                      (= 2 weeks-since-bid) "overdue"
                      :else "lapsed")
        system-status (cond
                        (nil? weeks-since-delivery) "no_data"
                        (<= weeks-since-delivery 1) "active"
                        (= 2 weeks-since-delivery) "stale"
                        :else "dead")]
    {:user_engagement {:prior "bid_weekly"
                       :last_bid last-bid-week
                       :weeks_since_bid weeks-since-bid
                       :status user-status}
     :system_delivery {:prior "deliver_weekly"
                       :last_delivery last-delivery-date
                       :weeks_since_delivery weeks-since-delivery
                       :status system-status}
     :dead_man_switch {:threshold_weeks 3
                       :week1_action "nudge"
                       :week2_action "alert"
                       :week3_action "escalate"}}))

;; =============================================================================
;; Interface Coverage (Epistemic Rhythm Level 2)
;; =============================================================================

(def ^:private maturity-order
  "Maturity progression for devmap items."
  [:greenfield :stub :sketch :active :qa :stable])

(def ^:private maturity-scores
  {:greenfield 0
   :stub 0.1
   :sketch 0.25
   :active 0.5
   :qa 0.75
   :stable 1.0})

(defn- parse-devmap-items
  "Parse a devmap file and extract prototype items with maturity levels.
   Returns a sequence of {:id, :title, :maturity, :next-steps-count}."
  [content]
  (let [lines (str/split-lines content)]
    (loop [remaining lines
           current-item nil
           items []]
      (if (empty? remaining)
        (if current-item (conj items current-item) items)
        (let [line (first remaining)
              rest-lines (rest remaining)]
          (cond
            ;; New prototype item
            (str/starts-with? line "! instantiated-by:")
            (let [title (str/trim (subs line 18))
                  ;; Extract short ID from title (e.g., "Prototype 0" -> "P0")
                  id-match (re-find #"Prototype\s+(\d+)" title)
                  id (when id-match (str "P" (second id-match)))]
              (recur rest-lines
                     {:id id :title title :maturity :stub :next-steps-count 0}
                     (if current-item (conj items current-item) items)))

            ;; Maturity line
            (and current-item (re-find #":maturity\s+:(\w+)" line))
            (let [[_ mat] (re-find #":maturity\s+:(\w+)" line)]
              (recur rest-lines
                     (assoc current-item :maturity (keyword mat))
                     items))

            ;; Next step line
            (and current-item (str/includes? line "next["))
            (recur rest-lines
                   (update current-item :next-steps-count inc)
                   items)

            :else
            (recur rest-lines current-item items)))))))

(defn- compute-interface-coverage
  "Compute interface coverage from devmaps in the holes directory.
   Returns coverage metrics for epistemic rhythm."
  []
  (let [holes-dir (io/file (or (System/getenv "FUTON3_ROOT") ".") "holes")
        devmap-files (when (.exists holes-dir)
                       (->> (.listFiles holes-dir)
                            (filter #(str/ends-with? (.getName %) ".devmap"))))
        all-items (when (seq devmap-files)
                    (->> devmap-files
                         (mapcat (fn [f]
                                   (let [prefix (-> (.getName f)
                                                    (str/replace ".devmap" ""))]
                                     (map #(assoc % :devmap prefix)
                                          (parse-devmap-items (slurp f))))))
                         (filter :id)))
        by-maturity (group-by :maturity all-items)
        total (count all-items)
        ;; Items with maturity beyond :stub are "addressed"
        addressed (count (filter #(not= :stub (:maturity %)) all-items))
        ;; Compute weighted coverage score
        coverage-score (when (pos? total)
                         (/ (reduce + (map #(get maturity-scores (:maturity %) 0) all-items))
                            total))
        ;; Items in :qa or :stable are "recent progress"
        recent-progress (->> all-items
                             (filter #(#{:qa :stable} (:maturity %)))
                             (map #(str (:devmap %) "/" (:id %) ":" (name (:maturity %))))
                             vec)
        ;; Compute status
        status (cond
                 (zero? total) "no_data"
                 (< coverage-score 0.2) "nascent"
                 (< coverage-score 0.5) "expanding"
                 (< coverage-score 0.8) "maturing"
                 :else "stable")]
    {:stubs_total total
     :stubs_addressed addressed
     :stubs_by_maturity (into {} (map (fn [[k v]] [(name k) (count v)]) by-maturity))
     :coverage_rate (when coverage-score (double coverage-score))
     :recent_progress recent-progress
     :items (mapv (fn [item]
                    {:id (str (:devmap item) "/" (:id item))
                     :maturity (name (:maturity item))
                     :next_steps (:next-steps-count item)})
                  all-items)
     :status status}))

;; =============================================================================
;; Stack Learning (Epistemic Rhythm Level 3)
;; =============================================================================

(defn- compute-stack-learning
  "Compute stack learning metrics from MUSN activity entries.
   Looks for PSR, PUR, and PAR events to measure pattern usage and learning."
  [activity-entries]
  (let [;; Filter for learning-related events
        psr-events (filter #(or (= "psr" (:event/type %))
                                (= "session/psr" (:event/type %))
                                (str/includes? (str (:event/type %)) "psr"))
                           activity-entries)
        pur-events (filter #(or (= "pur" (:event/type %))
                                (= "session/pur" (:event/type %))
                                (str/includes? (str (:event/type %)) "pur"))
                           activity-entries)
        par-events (filter #(or (= "session/par" (:event/type %))
                                (= "par" (:event/type %)))
                           activity-entries)
        ;; Extract patterns from PSR events
        patterns-from-psr (->> psr-events
                               (keep #(or (:pattern/id %) (:pattern %)))
                               (filter some?))
        ;; Extract patterns from PAR events (nested in patterns_used)
        patterns-from-par (->> par-events
                               (mapcat #(or (:patterns_used %) []))
                               (keep :pattern))
        all-patterns (concat patterns-from-psr patterns-from-par)
        unique-patterns (distinct all-patterns)
        ;; Extract prediction errors from PUR events
        pur-prediction-errors (->> pur-events
                                   (keep #(or (:prediction-error %) (:prediction_error %)))
                                   (filter number?))
        ;; Extract prediction errors from PAR events (nested)
        par-prediction-errors (->> par-events
                                   (mapcat #(or (:prediction_errors %) []))
                                   (keep :magnitude)
                                   (filter number?))
        all-prediction-errors (concat pur-prediction-errors par-prediction-errors)
        ;; Count suggestions (new pattern proposals)
        suggestions-count (->> par-events
                               (mapcat #(or (:suggestions %) []))
                               count)
        ;; Compute metrics
        sessions-total (count (distinct (keep :session/id activity-entries)))
        sessions-with-pars (count par-events)
        unique-count (count unique-patterns)
        pattern-reuse-rate (when (pos? unique-count)
                            (double (/ (count all-patterns) unique-count)))
        avg-prediction-error (when (seq all-prediction-errors)
                               (double (/ (reduce + all-prediction-errors)
                                          (count all-prediction-errors))))
        ;; Compute status
        status (cond
                 (zero? sessions-with-pars) "cold"
                 (< sessions-with-pars 5) "warming"
                 (pos? suggestions-count) "growing"
                 :else "mature")]
    {:sessions_total sessions-total
     :sessions_with_pars sessions-with-pars
     :psr_count (count psr-events)
     :pur_count (count pur-events)
     :unique_patterns_used unique-count
     :pattern_reuse_rate pattern-reuse-rate
     :avg_prediction_error avg-prediction-error
     :new_patterns_proposed suggestions-count
     :status status}))

(defn- fetch-personal-vitality
  "Fetch personal vitality from the futon5a personal API.
   Includes priors (EWMA of historical clears), prediction error, and meta vitality.
   Returns nil if the API is not available."
  []
  (try
    (let [week-resp @(http-client/get (str personal-api-base "/api/personal/week")
                                       {:timeout 2000})
          status-resp @(http-client/get (str personal-api-base "/api/personal/status")
                                         {:timeout 2000})
          history-resp @(http-client/get (str personal-api-base "/api/personal/history?n=8")
                                          {:timeout 2000})]
      (when (and (= 200 (:status week-resp))
                 (= 200 (:status status-resp)))
        (let [week-data (json/parse-string (:body week-resp) true)
              status-data (json/parse-string (:body status-resp) true)
              history-data (when (= 200 (:status history-resp))
                             (json/parse-string (:body history-resp) true))
              bids (or (:bids week-data) {})
              clears (or (:clears week-data) {})
              ;; Compute priors from history (EWMA) or use defaults
              priors (ewma-priors history-data)
              ;; Compute delta per category (clears - bids = intention deviation)
              all-keys (set (concat (keys bids) (keys clears) (keys priors)))
              deltas (into {}
                           (for [k all-keys]
                             (let [bid (get bids k 0)
                                   clear (get clears k 0)
                                   b (if (number? bid) bid 0)
                                   c (if (number? clear) clear 0)]
                               [k (- c b)])))
              ;; Compute prediction error (clears - priors = expectation deviation)
              prediction-error (into {}
                                     (for [k all-keys]
                                       (let [prior (get priors k 0)
                                             clear (get clears k 0)
                                             p (if (number? prior) prior 0)
                                             c (if (number? clear) clear 0)]
                                         [k (- c p)])))
              ;; Generate flags for significant discrepancies
              flags (vec (distinct
                          (keep (fn [[k delta]]
                                  (let [kname (name k)]
                                    (cond
                                      (and (= kname "sleep") (< delta -7)) "sleep-deficit"
                                      (and (str/includes? kname "q4") (< delta -5)) "q4-squeezed"
                                      (and (= kname "creative") (< delta -3)) "creative-squeezed"
                                      :else nil)))
                                (merge deltas prediction-error))))
              ;; Compute meta vitality (engagement + delivery priors)
              activity-entries (try (svc/activity-log-entries {:limit 5}) (catch Throwable _ []))
              meta-vitality (compute-meta-vitality week-data history-data activity-entries)]
          {:week (:week-id week-data)
           :priors priors
           :bids bids
           :clears clears
           :delta_by_category deltas
           :prediction_error prediction-error
           :bid_total (:bid-total status-data)
           :clear_total (:clear-total status-data)
           :unallocated (:unallocated status-data)
           :flags flags
           :meta_vitality meta-vitality})))
    (catch Throwable _
      nil)))

;; =============================================================================
;; RAP: Retrieve All PARs (distilled learning from past sessions)
;; =============================================================================

(defn- scan-par-sidecars
  "Scan ~/.claude/projects/ for all .par.edn sidecar files.
   Returns a sequence of {:path :session-id :pars}."
  []
  (let [claude-dir (io/file (System/getProperty "user.home") ".claude" "projects")]
    (when (.exists claude-dir)
      (->> (file-seq claude-dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".par.edn")))
           (map (fn [f]
                  (try
                    (let [content (slurp f)
                          pars (edn/read-string content)
                          session-id (-> (.getName f)
                                         (str/replace ".par.edn" ""))]
                      {:path (.getAbsolutePath f)
                       :session-id session-id
                       :pars (if (vector? pars) pars [pars])})
                    (catch Exception _ nil))))
           (filter some?)))))

(defn- format-par-for-context
  "Format a single PAR for injection into session context."
  [par session-id]
  (let [q (:questions par)]
    (str "## " (or (:title par) "Session Review") "\n"
         "*Session: " session-id " | " (:timestamp par) "*\n"
         (when (:tags par)
           (str "*Tags: " (str/join ", " (map name (:tags par))) "*\n"))
         "\n"
         "**Intention:** " (:intention q) "\n\n"
         "**What happened:** " (:happening q) "\n\n"
         "**Perspectives:** " (:perspectives q) "\n\n"
         "**Learned:** " (:learned q) "\n\n"
         "**Forward:** " (:forward q) "\n")))

(defn- rap-request
  "Handle /rap endpoint - retrieve PARs for context injection.
   Options (via query params):
     limit - max number of PARs (default 10)
     tags - comma-separated tag filter
     since - ISO date filter (YYYY-MM-DD)"
  [params]
  (let [limit (or (some-> (get params "limit") parse-long) 10)
        tag-filter (when-let [t (get params "tags")]
                     (set (map keyword (str/split t #","))))
        since-filter (get params "since")
        all-sidecars (scan-par-sidecars)
        all-pars (->> all-sidecars
                      (mapcat (fn [{:keys [session-id pars]}]
                                (map #(assoc % :session-id session-id) pars)))
                      ;; Filter by tags if specified
                      (filter (fn [par]
                                (if tag-filter
                                  (some tag-filter (:tags par))
                                  true)))
                      ;; Filter by date if specified
                      (filter (fn [par]
                                (if since-filter
                                  (and (:timestamp par)
                                       (>= (compare (:timestamp par) since-filter) 0))
                                  true)))
                      ;; Sort by timestamp descending (most recent first)
                      (sort-by :timestamp #(compare %2 %1))
                      ;; Apply limit
                      (take limit)
                      vec)
        ;; Format for context
        context-block (when (seq all-pars)
                        (str "# Prior Learning (from " (count all-pars) " PARs)\n\n"
                             (str/join "\n---\n\n"
                                       (map #(format-par-for-context % (:session-id %))
                                            all-pars))))]
    {:ok true
     :count (count all-pars)
     :pars (mapv (fn [par]
                   {:id (:id par)
                    :session-id (:session-id par)
                    :title (:title par)
                    :timestamp (:timestamp par)
                    :tags (:tags par)
                    :learned (get-in par [:questions :learned])})
                 all-pars)
     :context context-block}))

;; =============================================================================
;; Multi-Session PAR (Nexus Point)
;; =============================================================================

(defn- session-id->sidecar-path
  "Convert a session ID to its .par.edn sidecar path.
   Searches ~/.claude/projects/ for matching .jsonl file."
  [session-id]
  (let [claude-dir (io/file (System/getProperty "user.home") ".claude" "projects")]
    (when (.exists claude-dir)
      (->> (file-seq claude-dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".jsonl")
                         (str/includes? (.getName %) session-id)))
           first
           (#(when % (str/replace (.getAbsolutePath %) ".jsonl" ".par.edn")))))))

(defn- write-par-to-sidecar!
  "Write or append a PAR to a session's .par.edn sidecar file."
  [sidecar-path par]
  (try
    (let [existing (when (.exists (io/file sidecar-path))
                     (edn/read-string (slurp sidecar-path)))
          pars (if (vector? existing) existing (if existing [existing] []))
          updated (conj pars par)]
      (spit sidecar-path (pr-str updated))
      {:ok true :path sidecar-path})
    (catch Exception e
      {:ok false :err (.getMessage e) :path sidecar-path})))

(defn- par-multi!
  "Write a PAR to multiple session sidecars as a nexus point.
   Expected body keys:
     par/id - unique PAR identifier
     par/timestamp - ISO timestamp
     par/sessions - vector of session IDs
     par/participants - vector of participant keywords
     par/questions - the 5 PAR questions
     par/tags - tags like [:joint :nexus]"
  [body]
  (let [par-id (or (:par/id body) (str "par-joint-" (subs (str (java.util.UUID/randomUUID)) 0 8)))
        timestamp (or (:par/timestamp body) (str (java.time.Instant/now)))
        sessions (:par/sessions body)
        participants (:par/participants body)
        questions (:par/questions body)
        tags (or (:par/tags body) [:joint :nexus])]
    (if (empty? sessions)
      {:ok false :err "No sessions specified"}
      (let [par {:id par-id
                 :timestamp timestamp
                 :title (or (:par/title body) "Joint PAR")
                 :sessions sessions
                 :participants participants
                 :questions questions
                 :tags tags}
            results (for [session-id sessions]
                      (if-let [path (session-id->sidecar-path session-id)]
                        (assoc (write-par-to-sidecar! path par) :session-id session-id)
                        {:ok false :err "Session not found" :session-id session-id}))]
        {:ok (every? :ok results)
         :par-id par-id
         :results (vec results)
         :sessions-written (count (filter :ok results))
         :sessions-total (count sessions)}))))

;; =============================================================================
;; Dashboard: Unified Status View
;; =============================================================================

(defn- compute-active-peripherals
  "Extract active peripherals from recent activity.
   Returns list of {:agent :source :last-seen :event-count}."
  [activity-entries]
  (let [by-agent (group-by (fn [e] [(:agent e) (:source e)]) activity-entries)]
    (->> by-agent
         (map (fn [[[agent source] events]]
                {:agent agent
                 :source source
                 :last-seen (:at (first events))
                 :event-count (count events)}))
         (sort-by :last-seen #(compare %2 %1))
         vec)))

(defn- compute-recent-purs
  "Extract recent PUR (Pattern Use Record) events from activity."
  [activity-entries]
  (->> activity-entries
       (filter #(or (= "pur" (:event/type %))
                    (= "session/pur" (:event/type %))
                    (str/includes? (str (:event/type %)) "pur")))
       (take 10)
       (mapv (fn [e]
               {:pattern (or (:pattern/id e) (:pattern e))
                :outcome (:outcome e)
                :prediction-error (:prediction-error e)
                :at (:at e)
                :session (:session/id e)}))))

(defn- count-lab-files
  "Count files in lab directories for persistence status."
  []
  (let [lab-root (io/file (or (System/getenv "FUTON3_ROOT") ".") "lab")]
    (if (.exists lab-root)
      (let [count-in (fn [subdir]
                       (let [d (io/file lab-root subdir)]
                         (if (.exists d)
                           (count (filter #(.isFile %) (file-seq d)))
                           0)))]
        {:sessions (count-in "sessions")
         :anchors (let [f (io/file lab-root "anchors" "index.edn")]
                    (if (.exists f)
                      (count (str/split-lines (slurp f)))
                      0))
         :links (let [f (io/file lab-root "links" "graph.edn")]
                  (if (.exists f)
                    (count (str/split-lines (slurp f)))
                    0))
         :remote (count-in "remote")
         :pattern-drafts (count-in "pattern-drafts")})
      {:sessions 0 :anchors 0 :links 0 :remote 0 :pattern-drafts 0})))

(defn- dashboard-request
  "Build unified dashboard view.
   Aggregates: active peripherals, recent PURs/PARs, persistence status."
  []
  (try
    (let [activity-entries (try (svc/activity-log-entries {:limit 100}) (catch Throwable _ []))
          peripherals (compute-active-peripherals activity-entries)
          recent-purs (compute-recent-purs activity-entries)
          recent-pars (-> (rap-request {"limit" "5"}) :pars)
          lab-counts (count-lab-files)]
      {:ok true
       :timestamp (str (java.time.Instant/now))
       :peripherals {:active (take 10 peripherals)
                     :total-agents (count (distinct (map :agent peripherals)))}
       :learning {:recent-purs recent-purs
                  :recent-pars recent-pars
                  :pur-count (count recent-purs)
                  :par-count (count recent-pars)}
       :persistence {:lab-counts lab-counts
                     :total-anchors (:anchors lab-counts)
                     :total-links (:links lab-counts)}})
    (catch Throwable t
      {:ok false :err (.getMessage t)})))

(defn- dispatch [uri body]
  (case uri
    "/musn/session/create" (svc/create-session! body)
    "/musn/turn/start"    (svc/turn-start! body)
    "/musn/turn/plan"     (svc/turn-plan! body)
    "/musn/turn/select"   (svc/turn-select! body)
    "/musn/turn/action"   (svc/turn-action! body)
    "/musn/turn/use"      (svc/turn-use! body)
    "/musn/evidence/add"  (svc/evidence-add! body)
    "/musn/turn/end"      (svc/turn-end! body)
    "/musn/turn/resume"   (svc/turn-resume! body)
    "/musn/session/state" (svc/session-state! body)
    "/musn/chat/message"  (svc/chat-message! body)
    "/musn/chat/bid"      (svc/chat-bid! body)
    "/musn/chat/unlatch"  (svc/chat-unlatch! body)
    "/musn/chat/state"    (svc/chat-state! body)
    "/musn/compass"       (compass-request body)
    "/musn/hud/build"     (hud-build-request body)
    "/musn/patterns/search" (pattern-search-request body)
    "/musn/scribe/turn"   (let [{:keys [session/id role content]} body]
                            (svc/record-turn! id (keyword role) content))
    "/musn/scribe/plan"   (let [{:keys [session/id note diagram]} body]
                            (svc/record-plan! id note diagram))
    "/musn/scribe/plan-wiring"
                          (let [{:keys [session/id note wiring]} body]
                            (svc/record-plan-wiring! id note wiring))
    "/musn/scribe/native-planning"
                          (let [{:keys [session/id tool task-id subject]} body]
                            (svc/note-native-planning-detected! id tool
                                                                :task-id task-id
                                                                :subject subject))
    "/musn/scribe/native-plan"
                          (let [{:keys [session/id tasks note]} body]
                            (svc/record-native-plan! id tasks :note note))
    "/musn/par"           (svc/par! body)
    "/musn/par/multi"     (par-multi! body)
    "/musn/activity/log"  (svc/activity-log! body)
    nil))

(defn- handle-activity-ws
  "WebSocket endpoint for bidirectional activity streaming.
   - Receives: activity events from clients (logged + broadcast)
   - Sends: all activity events to connected clients"
  [req]
  (http/with-channel req channel
    ;; Register client
    (svc/register-activity-client! channel)
    (log! "[activity-ws] client connected" {:remote (:remote-addr req)})

    ;; Send recent history on connect
    (let [recent (take-last 50 (svc/activity-log-entries))
          init-msg (json/generate-string {:type "init"
                                          :count (count recent)
                                          :events (vec recent)})]
      (http/send! channel init-msg false))

    ;; Handle incoming messages (activity events from clients)
    (http/on-receive channel
      (fn [raw]
        (try
          (let [msg (json/parse-string raw true)]
            (case (:type msg)
              "ping" (http/send! channel (json/generate-string {:type "pong"}) false)
              "activity" (svc/activity-log! (:payload msg))
              ;; Default: treat as activity event directly
              (when (and (:agent msg) (:source msg))
                (svc/activity-log! msg))))
          (catch Exception e
            (log! "[activity-ws] parse error" {:err (.getMessage e)})))))

    ;; Handle disconnect
    (http/on-close channel
      (fn [status]
        (svc/unregister-activity-client! channel)
        (log! "[activity-ws] client disconnected" {:status status})))))

(defn handler [req]
  (try
    (let [uri (:uri req)
          method (:request-method req)]
      (cond
        (= uri "/health") (json-response {:ok true})

        ;; WebSocket endpoint for activity stream
        (and (= uri "/musn/activity/ws") (= method :get))
        (handle-activity-ws req)

        ;; GET endpoint for activity entries
        (and (= uri "/musn/activity/entries") (= method :get))
        (let [params (some-> (:query-string req)
                             (java.net.URLDecoder/decode "UTF-8")
                             (clojure.string/split #"&")
                             (->> (map #(clojure.string/split % #"=" 2))
                                  (into {})))
              limit (some-> (get params "limit") parse-long)
              entries (svc/activity-log-entries {:limit (or limit 20)})]
          (json-response {:ok true :entries (vec entries)}))

        ;; GET endpoint for RAP (Retrieve All PARs) - distilled learning
        (and (= uri "/rap") (= method :get))
        (let [params (some-> (:query-string req)
                             (java.net.URLDecoder/decode "UTF-8")
                             (clojure.string/split #"&")
                             (->> (map #(clojure.string/split % #"=" 2))
                                  (into {})))]
          (json-response (rap-request (or params {}))))

        ;; GET endpoint for vitality scan data (futon + personal + interface + stack learning)
        (and (= uri "/musn/vitality") (= method :get))
        (let [scan-path (io/file (or (System/getenv "FUTON3_ROOT") ".")
                                 "resources/vitality/latest_scan.json")
              futon-data (when (.exists scan-path)
                           (try
                             (json/parse-string (slurp scan-path) true)
                             (catch Exception _ nil)))
              personal-data (fetch-personal-vitality)
              interface-data (compute-interface-coverage)
              ;; Get activity entries for stack learning computation
              activity-entries (try (svc/activity-log-entries {:limit 500}) (catch Throwable _ []))
              stack-data (compute-stack-learning activity-entries)]
          (json-response {:ok true
                          :vitality (cond-> (or futon-data {})
                                      personal-data (assoc :personal_vitality personal-data)
                                      interface-data (assoc :interface_coverage interface-data)
                                      stack-data (assoc :stack_learning stack-data))}))

        ;; GET endpoint for unified dashboard view
        (and (= uri "/musn/dashboard") (= method :get))
        (json-response (dashboard-request))

        (not= :post method) (json-response 405 {:ok false :err "method not allowed"})
        :else
        (let [body (parse-json-body req)]
          (when (= uri "/musn/turn/end")
            (log! "[musn-http] turn/end"
                  {:session/id (:session/id body)
                   :turn (:turn body)
                   :remote-addr (:remote-addr req)
                   :user-agent (get-in req [:headers "user-agent"])
                   :client (get-in req [:headers "x-musn-client"])
                   :content-length (get-in req [:headers "content-length"])}))
          (if-let [resp (dispatch uri body)]
            (json-response resp)
            (json-response 404 {:ok false :err "not found"})))))
    (catch Throwable t
      (log! "[musn-http] handler error" {:uri (:uri req)
                                         :err (.getMessage t)})
      (json-response 400 {:ok false
                          :err (.getMessage t)
                          :type (keyword (.. t getClass getSimpleName))}))))

(defonce ^:private server-state (atom nil))

(defn start!
  "Start the MUSN HTTP server. Returns a stop function.
   Options:
     :port - HTTP port (default 6065)
     :ssl-port - HTTPS/WSS port (default nil, disabled)
     :ssl-context - javax.net.ssl.SSLContext for TLS
     :ssl-domain - Let's Encrypt domain (alternative to ssl-context)
     :aif-viz-port - optional AIF visualization port"
  ([] (start! {}))
  ([opts]
   (let [port (or (:port opts)
                  (some-> (System/getenv "MUSN_PORT") Long/parseLong)
                  6065)
         ssl-port (or (:ssl-port opts)
                      (some-> (System/getenv "MUSN_SSL_PORT") Long/parseLong))
         ssl-domain (or (:ssl-domain opts)
                        (System/getenv "MUSN_SSL_DOMAIN"))
         aif-viz-port (or (:aif-viz-port opts)
                          (some-> (System/getenv "MUSN_AIF_VIZ_PORT") str/trim not-empty Long/parseLong))
         stop-fns (atom [])]
     (when aif-viz-port
       (aif-viz/start! {:port aif-viz-port}))

     ;; Start HTTP server
     (log! (format "MUSN HTTP server on %d" port))
     (swap! stop-fns conj (http/run-server #'handler {:port port}))

     ;; Note: For WSS/TLS, use nginx as reverse proxy to this HTTP server

     (let [stop-all (fn [] (doseq [f @stop-fns] (f)))]
       (reset! server-state {:port port :ssl-port ssl-port :stop-fn stop-all})
       stop-all))))

(defn stop!
  "Stop the MUSN HTTP server."
  []
  (when-let [{:keys [stop-fn]} @server-state]
    (stop-fn)
    (reset! server-state nil)))

(defn -main [& _args]
  (let [port (Long/parseLong (or (System/getenv "MUSN_PORT") "6065"))]
    (binding [*out* *err*]
      (println (format "MUSN HTTP server on %d" port))
      (println (format "Logging to %s" log-path)))
    (start! {:port port})
    (binding [*out* *err*]
      (println "Press Ctrl+C to exit."))
    @(promise)))
