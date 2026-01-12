# FuLab HUD Design

A shared "Heads-Up Display" module for pattern-aware sessions across fucodex, fuclaude, and fubar.el.

## Core Concept

The HUD is the **shared context surface** that:
1. Shows what patterns are in play
2. Lets AIF suggest selections
3. Gets seeded into agent prompts
4. Gets displayed to humans
5. Gets archived with session traces

## Data Structure

```clojure
{:hud/id "hud-019b..."
 :hud/timestamp #inst "..."

 ;; Input context
 :intent "implement belief-state schema"
 :prototypes ["aif" "schema"]
 :sigils [{:emoji "🧠" :hanzi "mind"} {:emoji "📊" :hanzi "evidence"}]

 ;; Pattern candidates (from hints)
 :candidates [{:id "aif/belief-state-operational-hypotheses"
               :score 0.82
               :summary "Maintain compact belief map..."}
              {:id "aif/structured-observation-vector"
               :score 0.71
               :summary "Typed observation vector..."}]

 ;; AIF state
 :aif {:suggested "aif/belief-state-operational-hypotheses"
       :G-scores {"aif/belief-state-operational-hypotheses" 0.42
                  "aif/structured-observation-vector" 0.58}
       :tau 0.7
       :rationale "Lower G, matches intent sigils"}

 ;; Agent response (filled after turn)
 :agent-report {:applied "aif/belief-state-operational-hypotheses"
                :notes "Matched because task is schema definition"
                :events [{:kind :pattern/applied :pattern "..." :at #inst "..."}]}}
```

## Module API

```clojure
(ns futon3.fulab.hud
  "Shared HUD for pattern-aware sessions.")

;; Build HUD from intent/context
(defn build-hud
  "Create HUD state from intent and optional context."
  [{:keys [intent prototypes sigils repo-root aif-config]}]
  ;; 1. Fetch pattern hints from futon3
  ;; 2. Score candidates with AIF
  ;; 3. Return HUD structure
  )

;; Format for prompt injection
(defn hud->prompt-block
  "Format HUD as text block for agent prompt injection."
  [hud]
  ;; Returns string like:
  ;; ---FULAB-HUD---
  ;; Intent: ...
  ;; Candidates: ...
  ;; AIF suggests: ...
  ;; ---END-FULAB-HUD---
  )

;; Parse agent response
(defn parse-agent-report
  "Extract pattern applicability from agent response."
  [response-text]
  ;; Look for structured block or infer from content
  )

;; Update HUD with agent report
(defn hud-with-report
  "Add agent's pattern report to HUD."
  [hud agent-report]
  (assoc hud :agent-report agent-report))

;; Archive HUD to session
(defn hud->session-event
  "Convert HUD state to session event for archival."
  [hud]
  {:event/type :hud/state
   :at (:hud/timestamp hud)
   :payload hud})
```

## Integration Points

### fucodex / fuclaude

```bash
# New flag: --hud builds and injects HUD context
fucodex --live --hud --intent "implement belief-state schema" \
  -p "Add the schema to tatami_schema.clj"

fuclaude --live --hud --intent "review aif patterns" \
  -p "Check for dead code in aif_bridge.clj"
```

The wrapper:
1. Calls `(build-hud {:intent "..." ...})`
2. Formats with `(hud->prompt-block hud)`
3. Prepends to agent prompt
4. After response, calls `(parse-agent-report response)`
5. Archives HUD state with session

### fubar.el

```elisp
;; fubar.el calls the same Clojure module via shell/nrepl

(defun fubar-build-hud (intent)
  "Build HUD for current session."
  (fubar--call-clj 'futon3.fulab.hud/build-hud
                   :intent intent
                   :prototypes (fubar-current-prototypes)
                   :repo-root default-directory))

(defun fubar-render-hud (hud)
  "Display HUD in sidebar/modeline."
  ;; Show candidates, AIF suggestion, sigils
  ;; Clickable pattern IDs like aob-chatgpt.el
  )

(defun fubar-inject-hud-context ()
  "Add HUD block to next prompt."
  (let* ((hud (fubar-build-hud fubar-current-intent))
         (block (fubar--call-clj 'futon3.fulab.hud/hud->prompt-block hud)))
    (setq fubar--pending-hud hud)
    block))
```

### aob-chatgpt.el alignment

The existing `FROM-TATAMI-EDN` / `FROM-CHATGPT-EDN` flow maps to:
- `FROM-TATAMI-EDN` ≈ `hud->prompt-block`
- `FROM-CHATGPT-EDN` ≈ `parse-agent-report`

Could provide an adapter so aob-chatgpt.el uses the same underlying HUD module.

## Prompt Injection Format

```
---FULAB-HUD---
Intent: implement belief-state schema
Sigils: 🧠/mind 📊/evidence

Pattern candidates (reason about these):
1. aif/belief-state-operational-hypotheses (score: 0.82)
   "Maintain compact belief map of operational hypotheses"
2. aif/structured-observation-vector (score: 0.71)
   "Typed observation vector for comparable scoring"

AIF suggests: #1 (G=0.42, τ=0.7)

After completing the task, report which pattern(s) you applied:
---FULAB-REPORT---
:applied "pattern-id"
:action "implement"
:notes "why this pattern fit"
---END-FULAB-REPORT---
---END-FULAB-HUD---
```

## Archival

Each session gets HUD events:

```clojure
;; At session start
{:event/type :hud/initialized
 :at #inst "..."
 :payload {:hud/id "..."
           :intent "..."
           :candidates [...]
           :aif {...}}}

;; After agent turn
{:event/type :hud/agent-reported
 :at #inst "..."
 :payload {:hud/id "..."
           :applied "pattern-id"
           :action "implement"
           :notes "..."}}

;; Existing PSR/PUR still generated for compatibility
```

## Benefits

1. **Unified surface**: Same HUD logic for CLI agents and Emacs
2. **Visible context**: Human can see what patterns are suggested
3. **AIF integrated**: Scoring happens on explicit candidates
4. **Archival complete**: HUD state captured alongside outcomes
5. **Augmentation + Autonomy**: Human can override, agent can suggest

## Implementation Order

1. `src/futon3/fulab/hud.clj` - core module
2. Update `fuclaude` with `--hud` flag
3. Update `fucodex` with `--hud` flag
4. `fubar-hud.el` - Emacs integration
5. Optional: aob-chatgpt.el adapter
