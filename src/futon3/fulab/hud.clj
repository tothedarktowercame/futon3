(ns futon3.fulab.hud
  "Shared HUD (Heads-Up Display) for pattern-aware sessions.

   Provides a unified context surface for fucodex, fuclaude, and fubar.el:
   - Shows what patterns are in play
   - Lets AIF suggest selections
   - Gets seeded into agent prompts
   - Gets displayed to humans
   - Gets archived with session traces"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3.pattern-hints :as hints]
            [futon3.cue-embedding :as cue]
            [futon3.glove-intent :as glove]))

(defn- now-inst []
  (java.util.Date.))

(defn- generate-hud-id []
  (str "hud-" (java.util.UUID/randomUUID)))

(defn- extract-sigils-from-intent
  "Derive sigils from intent text using cue embedding."
  [intent patterns]
  (when (and intent (not (str/blank? intent)))
    (when-let [cues (cue/intent-pattern-cues {:intent intent
                                               :patterns patterns
                                               :max-patterns 5})]
      (->> (:sigils cues)
           (remove #(or (nil? (:emoji %)) (nil? (:hanzi %))))
           (map (fn [pair] {:emoji (:emoji pair) :hanzi (:hanzi pair)}))
           distinct
           vec))))

(defn- score-source
  "Best-effort attribution for the candidate score."
  [entry]
  (cond
    (number? (:score/similarity entry)) :glove-embedding
    (number? (:score/glove-distance entry)) :glove-embedding
    (number? (:score/sigil-distance entry)) :sigil-distance
    :else :unknown))

(defn- format-candidate
  "Format a pattern entry for HUD display."
  [entry]
  {:id (:id entry)
   :score (when (number? (:score entry)) (double (:score entry)))
   :score-source (or (:score-source entry) (score-source entry))
   :score-sigil (when (number? (:score/sigil-distance entry))
                  (double (:score/sigil-distance entry)))
   :score-glove (when (number? (:score/glove-distance entry))
                  (double (:score/glove-distance entry)))
   :score-similarity (when (number? (:score/similarity entry))
                       (double (:score/similarity entry)))
   :summary (or (:summary entry) (:title entry) "")
   :sigils (:sigils entry)
   :pattern-ref (:pattern-ref entry)
   :maturity-phase (:maturity/phase entry)
   :evidence-count (:evidence/count entry)
   :next-steps-count (:next-steps/count entry)
   :precision-prior (:precision/prior entry)})

(def ^:private aif-weight-sigil 0.5)
(def ^:private aif-weight-intent 0.3)
(def ^:private aif-weight-glove 0.2)
(def ^:private white-space-tau-bump 0.2)
(def ^:private white-space-tau-min 0.7)
(def ^:private precision-bonus-scale 0.08)

(defn- candidate-signal?
  "Whether a candidate carries any usable scoring signal."
  [entry]
  (some number?
        [(:score entry)
         (:score/sigil-distance entry)
         (:score/glove-distance entry)
         (:score/intent-embed-distance entry)]))

(defn- white-space-candidates?
  "Detect when candidate signals are missing (white space)."
  [candidates]
  (or (empty? candidates)
      (not (some candidate-signal? candidates))))

(defn- bump-white-space-tau [tau]
  (let [boosted (+ (double tau) white-space-tau-bump)]
    (-> boosted
        (max white-space-tau-min)
        (min 0.95)
        double)))

(defn- normalize-weights [parts]
  (let [total (reduce + (map :weight parts))]
    (if (pos? total)
      (mapv (fn [entry]
              (update entry :weight #(/ (double %) total)))
            parts)
      parts)))

(defn- weighted-base-score [entry]
  (let [sigil (:score/sigil-distance entry)
        glove (:score/glove-distance entry)
        intent-dist (:score/intent-embed-distance entry)
        parts (cond-> []
                (number? sigil)
                (conj {:key :sigil :distance sigil :weight aif-weight-sigil})
                (number? intent-dist)
                (conj {:key :intent :distance intent-dist :weight aif-weight-intent})
                (number? glove)
                (conj {:key :glove :distance glove :weight aif-weight-glove}))
        parts (normalize-weights parts)
        base (when (seq parts)
               (reduce + (map (fn [{:keys [distance weight]}]
                                (* (double distance) (double weight)))
                              parts)))]
    {:base base
     :sigil sigil
     :intent intent-dist
     :glove glove
     :weights (into {} (map (juxt :key :weight) parts))}))

(defn- scores->tau [scores]
  (let [scores (sort scores)]
    (cond
      (< (count scores) 2) 0.5
      :else (let [best (first scores)
                  second (second scores)
                  worst (last scores)
                  range (max 1.0e-6 (- (double worst) (double best)))
                  spread (/ (- (double second) (double best)) range)
                  scaled (+ 0.35 (* 0.6 (min 1.0 spread)))]
              (-> scaled (max 0.35) (min 0.95) double)))))

(defn- score-candidates-with-aif
  "Score candidates using simple heuristics (placeholder for full AIF integration).
   Lower G is better."
  [candidates {:keys [sigils]}]
  (let [white-space? (white-space-candidates? candidates)]
    (if (empty? candidates)
      {:suggested nil
       :G-scores {}
       :G-components {}
       :white-space? white-space?
       :tau (if white-space? (bump-white-space-tau 0.5) 0.5)
       :rationale (if white-space?
                    "No candidates available (white-space: tau boosted for exploration)"
                    "No candidates available")}
      (let [;; Simple scoring: use existing score (distance-based, lower is better)
            scored (map (fn [c]
                          (let [{:keys [base sigil glove intent weights]} (weighted-base-score c)
                                base-score (double (or base (:score c) 0.5))
                                precision (double (or (:precision/prior c) 0.5))
                                precision (-> precision (max 0.0) (min 1.0))
                                precision-bonus (* precision-bonus-scale (- 0.5 precision))
                                ;; Prefer patterns whose sigils match intent sigils
                                sigil-bonus (if (and (seq sigils) (seq (:sigils c)))
                                              (let [c-sigils (set (map :emoji (:sigils c)))
                                                    i-sigils (set (map :emoji sigils))]
                                                (if (seq (set/intersection c-sigils i-sigils))
                                                  -0.1
                                                  0.0))
                                              0.0)
                                G (+ base-score sigil-bonus precision-bonus)]
                            {:id (:id c)
                             :G G
                             :components {:base base-score
                                          :sigil-distance sigil
                                          :glove-distance glove
                                          :intent-distance intent
                                          :precision-prior precision
                                          :precision-bonus precision-bonus
                                          :maturity-phase (:maturity/phase c)
                                          :evidence-count (:evidence/count c)
                                          :next-steps-count (:next-steps/count c)
                                          :weights weights
                                          :sigil-bonus sigil-bonus
                                          :source (:score-source c)
                                          :missing-score (and (nil? base)
                                                              (nil? (:score c)))}}))
                        candidates)
            sorted (sort-by :G scored)
            best (first sorted)
            G-map (into {} (map (juxt :id :G) scored))
            G-components (into {} (map (juxt :id :components) scored))
            tau (scores->tau (map :G scored))
            tau (if white-space? (bump-white-space-tau tau) tau)]
        {:suggested (:id best)
         :G-scores G-map
         :G-components G-components
         :candidate-count (count candidates)
         :white-space? white-space?
         :tau tau
         :rationale (str "G = weighted distance (sigil + intent-embed + glove) + sigil bonus + maturity prior; "
                         "lowest G among " (count candidates) " candidates"
                         (when white-space?
                           " (white-space: tau boosted for exploration)"))}))))

(defn build-hud
  "Create HUD state from intent and optional context.

   Options:
   - :intent - text describing the task/goal
   - :prototypes - vector of prototype IDs for sigil resolution
   - :sigils - explicit sigils (overrides derivation)
   - :pattern-limit - max patterns to fetch (default 4)
   - :aif-config - AIF configuration map
   - :mana - optional mana state map for prompt display
   - :musn-help - optional boolean to show MUSN help prompt
   - :musn-hud - optional boolean to show HUD AIF stats
   - :certificates - optional certificate vector for PSR/PUR"
  [{:keys [intent prototypes sigils pattern-limit certificates mana musn-help musn-hud]
    :or {pattern-limit 4}}]
  (let [all-patterns (hints/all-patterns)
        ;; Resolve sigils from intent or prototypes
        resolved-sigils (or (when (seq sigils) sigils)
                            (extract-sigils-from-intent intent all-patterns)
                            (when (seq prototypes)
                              ;; Would need prototype->sigil mapping
                              nil))
        ;; Fetch pattern hints
        hint-result (hints/hints {:sigils resolved-sigils
                                  :prototypes prototypes
                                  :pattern-limit pattern-limit})
        candidates (mapv format-candidate (:patterns hint-result))
        glove-candidates (mapv format-candidate (:glove-patterns hint-result))
        aif-candidates (hints/aif-candidates (:patterns hint-result)
                                             (:glove-patterns hint-result))
        aif-candidates (if (and intent (seq aif-candidates))
                         (glove/attach-intent-embed-distance intent aif-candidates)
                         aif-candidates)
        ;; Score with AIF
        aif-result (score-candidates-with-aif aif-candidates
                                               {:sigils resolved-sigils})
        candidate-ids (set (map :id (:patterns hint-result)))
        aif-result (assoc aif-result
                          :suggested-in-candidates
                          (contains? candidate-ids (:suggested aif-result)))
        display-candidates (->> (concat candidates glove-candidates)
                                (reduce (fn [{:keys [seen out]} cand]
                                          (let [cid (:id cand)]
                                            (if (contains? seen cid)
                                              {:seen seen :out out}
                                              {:seen (conj seen cid)
                                               :out  (conj out cand)})))
                                        {:seen #{} :out []})
                                :out
                                (take pattern-limit))]
    {:hud/id (generate-hud-id)
     :hud/timestamp (now-inst)
     ;; Input context
     :intent (or intent "unspecified")
     :prototypes (vec (or prototypes []))
     :sigils (vec (or resolved-sigils []))
     ;; Pattern candidates
     :candidates display-candidates
     :glove-candidates glove-candidates
     :fruits (mapv #(select-keys % [:id :emoji :name :score]) (:fruits hint-result))
     :paramitas (mapv #(select-keys % [:id :zh :en :score]) (:paramitas hint-result))
     :certificates (vec (or certificates []))
     ;; AIF state
     :aif aif-result
     ;; Mana state
     :mana mana
     ;; MUSN help + HUD stats
     :musn-help musn-help
     :musn-hud musn-hud
     ;; Agent response (filled after turn)
     :agent-report nil}))

(defn hud->prompt-block
  "Format HUD as text block for agent prompt injection."
  [hud]
  (let [sigil-str (if (seq (:sigils hud))
                    (->> (:sigils hud)
                         (map #(str (:emoji %) "/" (:hanzi %)))
                         (str/join " "))
                    "none")
        candidates-str (if (seq (:candidates hud))
                         (->> (:candidates hud)
                              (map-indexed
                               (fn [i c]
                                 (let [score (:score c)
                                       score-label (if (number? score)
                                                     (format "dist %.3f (0..1)" (double score))
                                                     "n/a")
                                       ref-label (when-let [pattern-ref (:pattern-ref c)]
                                                   (str " ref " pattern-ref))
                                       maturity-label (when-let [phase (:maturity-phase c)]
                                                        (format ", maturity: %s (e:%s n:%s)"
                                                                (name phase)
                                                                (or (:evidence-count c) 0)
                                                                (or (:next-steps-count c) 0)))]
                                   (format "%d. %s (score: %s, source: %s%s%s)\n   %s"
                                           (inc i)
                                           (:id c)
                                           score-label
                                           (or (some-> (:score-source c) name) "unknown")
                                           (or ref-label "")
                                           (or maturity-label "")
                                           (or (:summary c) "")))))
                              (str/join "\n"))
                         "No candidates found.")
        aif (:aif hud)
        aif-live (:aif-live hud)
        mana (:mana hud)
        mana-num (fn [value]
                   (when (number? value)
                     (format "%.0f" (double value))))
        live-num (fn [value]
                   (when (number? value)
                     (format "%.2f" (double value))))
        mana-balance (or (:balance mana) (:budget mana) 100)
        mana-line (if (map? mana)
                    (let [balance (mana-num (:balance mana))
                          budget (mana-num (:budget mana))
                          earned (mana-num (:earned mana))
                          spent (mana-num (:spent mana))
                          main (format "Mana: %s/%s"
                                       (or balance "?")
                                       (or budget "?"))
                          trail (remove nil?
                                        [(when earned (str "earned " earned))
                                         (when spent (str "spent " spent))])]
                      (if (seq trail)
                        (str main " (" (str/join ", " trail) ")")
                        main))
                    "Mana: n/a")
        help-line (when (:musn-help hud)
                    (str "[MUSN-HELP]\n"
                         "You start with 100 mana points. Gain mana by reading patterns and completing their next steps.\n"
                         "Most other actions cost mana.\n"
                         "Helper functions live in /home/joe/code/futon3/scripts — use those exact paths.\n"
                         "Related design patterns live in /home/joe/code/futon3/library; you may read/edit them if helpful.\n"
                         "Tool roster (you should emit these signals when you do the corresponding action):\n"
                         "- /home/joe/code/futon3/scripts/pattern-select library/<pattern> <state why you want to read it>\n"
                         "- /home/joe/code/futon3/scripts/pattern-use    library/<pattern> <state where you will apply it>\n"
                         "- /home/joe/code/futon3/scripts/pattern-action read|update|implement library/<pattern> <note>\n"
                         "- /home/joe/code/futon3/scripts/musn-chat      \"question for the user\" (human-contact, costs 5 mana)\n"
                         "- /home/joe/code/futon3/scripts/musn-plan      \"Plan: ...\"\n"
                         "- /home/joe/code/futon3/scripts/wide-search   <rg args>\n"
                         "- /home/joe/code/futon3/scripts/musn-hud      Use this command to bring up helpful pattern guidance and stats about your run.\n"
                         "- /home/joe/code/futon3/scripts/musn-help     Brings up this help screen.\n"
                         "Further information can be obtained by running musn-help <tool> or musn-help <pattern>\n"
                         "Before using a tool, state a 1-2 line plan.\n"
                         "Your next output should exercise musn-plan by describing your next steps.\n"
                         "[/MUSN-HELP]\n"))
        live-summary (:summary aif-live)
        live-line (when (map? live-summary)
                    (let [kind (or (:kind live-summary) "tap")
                          chosen (:chosen live-summary)
                          tau (live-num (:tau live-summary))
                          g (live-num (:g-chosen live-summary))
                          err (live-num (:prediction-error live-summary))
                          parts (remove nil?
                                        [(when kind (str "kind=" kind))
                                         (when chosen (str "chosen=" chosen))
                                         (when g (str "G=" g))
                                         (when tau (str "tau=" tau))
                                         (when err (str "err=" err))])]
                      (when (seq parts)
                        (str "AIF live: " (str/join " " parts)))))
        selection-line (when-let [sel (:last-selection aif-live)]
                         (let [pid (:pattern-id sel)
                               mode (:mode sel)]
                           (when (or pid mode)
                             (str "selection=" (or pid "?")
                                  (when mode (str " (" (name mode) ")"))))))
        use-line (when-let [use (:last-use aif-live)]
                   (let [pid (:pattern-id use)]
                     (when pid
                       (str "use=" pid))))
        stats-line (when (:musn-hud hud)
                     (let [parts (remove nil? [live-line selection-line use-line])]
                       (if (seq parts)
                         (str "AIF stats: " (str/join " | " parts))
                         "AIF stats: none")))
        white-space-label (when (:white-space? aif) "white-space tau boost")
        ;; AIF-LM-3: Compute probability for display
        aif-prob (when-let [suggested (:suggested aif)]
                   (let [g-scores (:G-scores aif)
                         tau (or (:tau aif) 1.0)
                         logits (map (fn [[id g]] [id (/ (- g) tau)]) g-scores)
                         max-logit (apply max (map second logits))
                         exp-logits (map (fn [[id l]] [id (Math/exp (- l max-logit))]) logits)
                         z (reduce + (map second exp-logits))
                         probs (into {} (map (fn [[id e]] [id (/ e z)]) exp-logits))]
                     (get probs suggested)))
        aif-str (if (:suggested aif)
                  (format "AIF suggests: %s" (:suggested aif))
                  "AIF suggests: none")]
    (str (or help-line "")
         "[FULAB-HUD]\n"
         "Intent: " (:intent hud) "\n"
         "Sigils: " sigil-str "\n"
         "\n"
         "Pattern candidates (reason about these):\n"
         candidates-str "\n"
         "\n"
         aif-str "\n"
         (when stats-line
           (str stats-line "\n"
                "TODO: consider live-updating pattern candidates when musn-hud is requested.\n"))
         "[/FULAB-HUD]")))

(defn- extract-report-block
  "Extract FULAB-REPORT block from response text."
  [text]
  (when text
    ;; Match both old (---) and new ([]) formats
    (when-let [match (or (re-find #"(?s)\[FULAB-REPORT\](.*?)\[/FULAB-REPORT\]" text)
                         (re-find #"(?s)---FULAB-REPORT---(.*?)---END-FULAB-REPORT---" text))]
      (str/trim (second match)))))

(defn- parse-report-edn
  "Parse the report block as pseudo-EDN (lenient)."
  [block]
  (when block
    (let [applied (second (re-find #":applied\s+\"([^\"]+)\"" block))
          action (second (re-find #":action\s+\"([^\"]+)\"" block))
          notes (second (re-find #":notes\s+\"([^\"]+)\"" block))]
      (when (or applied action notes)
        {:applied applied
         :action action
         :notes notes}))))

(defn parse-agent-report
  "Extract pattern applicability from agent response text.

   Looks for structured FULAB-REPORT block, or infers from content."
  [response-text]
  (if-let [block (extract-report-block response-text)]
    (or (parse-report-edn block)
        {:applied nil
         :notes block})
    ;; Fallback: look for pattern mentions in text
    (let [pattern-mention (re-find #"(?:applied|used|following)\s+(?:the\s+)?[`\"]?([a-z0-9_-]+/[a-z0-9_-]+)[`\"]?"
                                   (str/lower-case (or response-text "")))]
      (when pattern-mention
        {:applied (second pattern-mention)
         :notes "Inferred from response text"}))))

(defn hud-with-report
  "Add agent's pattern report to HUD."
  [hud agent-report]
  (assoc hud :agent-report agent-report))

(defn hud->session-events
  "Convert HUD state to session events for archival."
  [hud]
  (let [base-event {:hud/id (:hud/id hud)
                    :at (:hud/timestamp hud)}]
    (cond-> [(assoc base-event
                    :event/type :hud/initialized
                    :payload (dissoc hud :agent-report))]
      (:agent-report hud)
      (conj {:event/type :hud/agent-reported
             :hud/id (:hud/id hud)
             :at (now-inst)
             :payload {:hud/id (:hud/id hud)
                       :report (:agent-report hud)}}))))

(defn hud->psr
  "Generate a PSR from HUD state (for compatibility with existing pipeline)."
  [hud session-id turn]
  (let [aif (:aif hud)
        candidates (mapv :id (:candidates hud))
        chosen (or (get-in hud [:agent-report :applied])
                   (:suggested aif)
                   (first candidates))]
    {:psr/id (str "psr-hud-" turn)
     :session/id session-id
     :decision/id (str session-id ":hud-" turn)
     :candidates candidates
     :chosen chosen
     :certificates (vec (or (:certificates hud) []))
     :context/anchors [{:anchor/type :hud/state
                        :anchor/ref {:event/type :hud/initialized
                                     :hud/id (:hud/id hud)}}]
     :hud/source (:hud/id hud)
     :aif {:G-chosen (get (:G-scores aif) chosen)
           :G-rejected (dissoc (:G-scores aif) chosen)
           :tau (:tau aif)}
     :horizon :immediate}))

(defn hud->pur
  "Generate a PUR from HUD state (for compatibility with existing pipeline)."
  [hud session-id turn]
  (let [report (:agent-report hud)
        candidates (mapv :id (:candidates hud))
        applied (or (:applied report)
                    (get-in hud [:aif :suggested])
                    (first candidates))]
    {:pur/id (str "pur-hud-" turn)
     :session/id session-id
     :pattern/id applied
     :instance/id (str "pur-hud-" turn "-a")
     :decision/id (str session-id ":hud-" turn)
     :certificates (vec (or (:certificates hud) []))
     :fields {:context (:intent hud)
              :if "HUD presented pattern candidates"
              :however "Agent selected based on task fit"
              :then (or (:notes report) "Applied pattern to task")
              :because "Pattern matched intent and context"
              :next-steps "Verify outcome matches pattern rationale"}
     :anchors [{:anchor/type :hud/state
                :anchor/ref {:event/type :hud/initialized
                             :hud/id (:hud/id hud)}}]
     :hud/source (:hud/id hud)
     :outcome/tags (if report [:outcome/reported] [:outcome/inferred])}))

(comment
  ;; Example usage:
  (def hud (build-hud {:intent "implement belief-state schema"
                       :pattern-limit 4}))

  (println (hud->prompt-block hud))

  (def response "I applied the aif/belief-state-operational-hypotheses pattern
                 ---FULAB-REPORT---
                 :applied \"aif/belief-state-operational-hypotheses\"
                 :notes \"Task is schema definition which matches pattern scope\"
                 ---END-FULAB-REPORT---")

  (parse-agent-report response)
  ;; => {:applied "aif/belief-state-operational-hypotheses", :notes "..."}
  )
