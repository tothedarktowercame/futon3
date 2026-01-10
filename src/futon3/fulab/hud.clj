(ns futon3.fulab.hud
  "Shared HUD (Heads-Up Display) for pattern-aware sessions.

   Provides a unified context surface for fucodex, fuclaude, and fubar.el:
   - Shows what patterns are in play
   - Lets AIF suggest selections
   - Gets seeded into agent prompts
   - Gets displayed to humans
   - Gets archived with session traces"
  (:require [clojure.string :as str]
            [futon3.pattern-hints :as hints]
            [futon3.cue-embedding :as cue]))

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
   :sigils (:sigils entry)})

(defn- score-candidates-with-aif
  "Score candidates using simple heuristics (placeholder for full AIF integration).
   Lower G is better."
  [candidates {:keys [intent sigils]}]
  (if (empty? candidates)
    {:suggested nil
     :G-scores {}
     :G-components {}
     :tau 0.5
     :rationale "No candidates available"}
    (let [;; Simple scoring: use existing score (distance-based, lower is better)
          scored (map (fn [c]
                        (let [base-score (double (or (:score c) 0.5))
                              ;; Prefer patterns whose sigils match intent sigils
                              sigil-bonus (if (and (seq sigils) (seq (:sigils c)))
                                            (let [c-sigils (set (map :emoji (:sigils c)))
                                                  i-sigils (set (map :emoji sigils))]
                                              (if (seq (clojure.set/intersection c-sigils i-sigils))
                                                -0.1
                                                0.0))
                                            0.0)
                              G (+ base-score sigil-bonus)]
                          {:id (:id c)
                           :G G
                           :components {:base base-score
                                        :sigil-bonus sigil-bonus
                                        :source (:score-source c)
                                        :missing-score (nil? (:score c))}}))
                      candidates)
          sorted (sort-by :G scored)
          best (first sorted)
          G-map (into {} (map (juxt :id :G) scored))
          G-components (into {} (map (juxt :id :components) scored))]
      {:suggested (:id best)
       :G-scores G-map
       :G-components G-components
       :tau 0.7  ;; Default confidence
       :rationale (str "G = base score (distance from sigils/embedding) + sigil bonus; "
                       "lowest G among " (count candidates) " candidates")})))

(defn build-hud
  "Create HUD state from intent and optional context.

   Options:
   - :intent - text describing the task/goal
   - :prototypes - vector of prototype IDs for sigil resolution
   - :sigils - explicit sigils (overrides derivation)
   - :pattern-limit - max patterns to fetch (default 4)
   - :aif-config - AIF configuration map
   - :certificates - optional certificate vector for PSR/PUR"
  [{:keys [intent prototypes sigils pattern-limit aif-config certificates]
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
        ;; Score with AIF
        aif-result (score-candidates-with-aif candidates
                                               {:intent intent
                                                :sigils resolved-sigils})]
    {:hud/id (generate-hud-id)
     :hud/timestamp (now-inst)
     ;; Input context
     :intent (or intent "unspecified")
     :prototypes (vec (or prototypes []))
     :sigils (vec (or resolved-sigils []))
     ;; Pattern candidates
     :candidates candidates
     :glove-candidates glove-candidates
     :fruits (mapv #(select-keys % [:id :emoji :name :score]) (:fruits hint-result))
     :paramitas (mapv #(select-keys % [:id :zh :en :score]) (:paramitas hint-result))
     :certificates (vec (or certificates []))
     ;; AIF state
     :aif aif-result
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
                                                     "n/a")]
                                   (format "%d. %s (score: %s, source: %s)\n   %s"
                                           (inc i)
                                           (:id c)
                                           score-label
                                           (or (some-> (:score-source c) name) "unknown")
                                           (or (:summary c) "")))))
                              (str/join "\n"))
                         "No candidates found.")
        aif (:aif hud)
        aif-str (if (:suggested aif)
                  (format "AIF suggests: %s (G=%.2f, τ=%.1f)"
                          (:suggested aif)
                          (double (get (:G-scores aif) (:suggested aif) 0))
                          (double (:tau aif)))
                  "AIF: no suggestion")]
    (str "[FULAB-HUD]\n"
         "Intent: " (:intent hud) "\n"
         "Sigils: " sigil-str "\n"
         "\n"
         "Pattern candidates (reason about these):\n"
         candidates-str "\n"
         "\n"
         aif-str "\n"
         "\n"
         "After completing the task, report which pattern(s) you applied:\n"
         "[FULAB-REPORT]\n"
         ":applied \"pattern-id-here\"\n"
         ":notes \"why this pattern fit\"\n"
         "[/FULAB-REPORT]\n"
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
          notes (second (re-find #":notes\s+\"([^\"]+)\"" block))]
      (when (or applied notes)
        {:applied applied
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
