# AIF as Pattern Selection Engine for Coding Agents

## Overview

This document describes an architecture where Active Inference Framework (AIF)
serves as the decision engine for pattern-guided coding agents (fuclaude/fucodex).
Rather than using AIF traces as evidence attached to proofs, the AIF engine
actively drives which patterns the agent applies during a coding session.

The ants work in Futon2 was proof-of-concept; now the surface is real software
development with LLM-based coding agents.

## Conceptual Reframe

**Previous model (AIF as evidence):**
```
Agent runs → produces AIF trace → attach to proof as evidence
```

**New model (AIF as engine):**
```
AIF engine → selects patterns → guides agent actions → updates beliefs
```

## Architecture

```
┌──────────────────────────────────────────────────────────┐
│                    fuclaude / fucodex                     │
├──────────────────────────────────────────────────────────┤
│  ┌────────────┐   ┌─────────────┐   ┌────────────────┐  │
│  │   Beliefs  │   │   Pattern   │   │   Observation  │  │
│  │    (μ)     │──▶│  Selector   │──▶│     Model      │  │
│  │            │   │  (min G)    │   │                │  │
│  │ - task     │   │             │   │ - tool results │  │
│  │ - progress │   │ PSR = which │   │ - test pass/   │  │
│  │ - context  │   │ pattern now │   │   fail         │  │
│  └────────────┘   └─────────────┘   └────────────────┘  │
│        ▲                                    │            │
│        │         Belief Update              │            │
│        └────────────────────────────────────┘            │
│                   PUR = what happened                    │
└──────────────────────────────────────────────────────────┘
```

## Core Components

### Generative Model (Patterns as Priors)

```
┌─────────────────────────────────────────────────┐
│                  AIF Engine                      │
│  ┌───────────┐    ┌───────────┐    ┌─────────┐ │
│  │ Generative│    │  Action   │    │ Belief  │ │
│  │   Model   │───▶│ Selection │───▶│ Update  │ │
│  │ (Patterns)│    │   (PSR)   │    │  (PUR)  │ │
│  └───────────┘    └───────────┘    └─────────┘ │
└─────────────────────────────────────────────────┘
         │                │               │
         ▼                ▼               ▼
    Pattern Lib     Which pattern    Did it work?
    as priors       to apply now?    Update beliefs
```

The pattern library becomes the agent's generative model:
- **Patterns** = policies / priors about how work should proceed
- **PSR** = action selection (choosing pattern under uncertainty)
- **PUR** = observation/outcome that updates the model
- **G (expected free energy)** = drives pattern selection
- **τ (precision)** = confidence in pattern applicability

### State Space (μ - Beliefs)

What the agent believes about the current situation:

| Belief Dimension | Examples |
|------------------|----------|
| Task state | what's done, what's blocked, what's next |
| Code health | tests passing, lint clean, types valid |
| Context fit | which patterns are relevant here |
| Uncertainty | confidence in current approach |

### Action Space (Patterns as Policies)

Patterns become the available actions. Examples from the library:

| Pattern | Licensed Actions |
|---------|------------------|
| `stack-coherence/commit-intent-alignment` | Stage changes, write commit message, commit |
| `devmap-coherence/next-steps-to-done` | Update task tracking, mark items complete |
| `code-coherence/dead-code-hygiene` | Find unused code, remove it, verify tests |
| `contributing/devmap-contribution-protocol` | Update devmap, cite evidence |

### Observation Model

How tool results become observations:

| Tool Result | Observation |
|-------------|-------------|
| Tests pass | `{:test-status :green}` |
| Tests fail | `{:test-status :red, :failures [...]}` |
| Lint errors | `{:lint-status :errors, :count N}` |
| Git status clean | `{:repo-status :clean}` |
| User feedback | `{:user-signal :approve}` or `{:user-signal :reject}` |

## Pattern Selection via Expected Free Energy

The agent selects patterns by minimizing expected free energy (G):

```clojure
(defn pattern-G [pattern beliefs observations]
  (+ (epistemic-value pattern beliefs)      ;; information gain
     (pragmatic-value pattern observations) ;; goal progress
     (- (pattern-fit pattern observations)))) ;; prediction error

(defn select-pattern [candidate-patterns beliefs observations]
  (->> candidate-patterns
       (map (fn [p] {:pattern p :G (pattern-G p beliefs observations)}))
       (sort-by :G)
       first
       :pattern))
```

This balances:
- **Epistemic value**: Does this pattern help reduce uncertainty?
- **Pragmatic value**: Does this pattern move toward the goal?
- **Pattern fit**: How well does the pattern predict current observations?

## Integration with FuLab

### PSR as Action Selection Record

When the AIF engine selects a pattern, it generates a PSR:

```clojure
{:psr/id "psr-aif-001"
 :decision/id "decision-edit-approach"
 :candidates ["code-coherence/dead-code-hygiene"
              "stack-coherence/commit-intent-alignment"]
 :chosen "code-coherence/dead-code-hygiene"
 :context/anchors [{:anchor/type :belief/state
                    :anchor/ref {:mu {:task-phase :cleanup}}}]
 :forecast {:benefits [{:tag :benefit/reduced-complexity
                        :locus {:file "src/foo.clj"}
                        :note "Dead code removal simplifies maintenance"}]}
 :aif {:G-chosen 0.42
       :G-rejected {"stack-coherence/commit-intent-alignment" 0.78}
       :tau 0.65}}
```

### PUR as Outcome Record

After applying the pattern, outcomes are logged as PUR:

```clojure
{:pur/id "pur-aif-001"
 :pattern/id "code-coherence/dead-code-hygiene"
 :decision/id "decision-edit-approach"
 :fields {:context "Cleanup phase of feature implementation"
          :if "Dead code detected in src/foo.clj"
          :however "Tests must still pass after removal"
          :then "Remove unused functions, verify tests"
          :because "Reduced complexity aids future changes"
          :next-steps "Commit if tests green"}
 :anchors [{:anchor/type :code/edit
            :anchor/ref {:file "src/foo.clj"}}]
 :aif {:prediction-error 0.12
       :tau-updated 0.71
       :belief-delta {:task-phase :cleanup -> :ready-to-commit}}}
```

### Belief Update Loop

```
Session Start
     │
     ▼
┌─────────────────────────┐
│ AIF Pattern Selector    │
│ - Current beliefs (μ)   │
│ - Pattern priors        │
│ - Expected outcomes     │
└───────────┬─────────────┘
            │ Select pattern with lowest G
            │ (logged as PSR)
            ▼
┌─────────────────────────┐
│ Pattern Application     │
│ - Pattern constraints   │
│ - Action generation     │
│ - Tool calls            │
└───────────┬─────────────┘
            │ Observe outcome
            ▼
┌─────────────────────────┐
│ Belief Update           │
│ - Did pattern fit?      │
│ - Update precision (τ)  │
│ - Log PUR               │
└───────────┬─────────────┘
            │
            ▼
       Next tick...
```

## Implementation Sketch

### Phase 1: Pattern-Policy Mapping

Define what actions each pattern licenses:

```clojure
(def pattern-policies
  {"code-coherence/dead-code-hygiene"
   {:preconditions [{:type :grep :pattern "unused|dead"}]
    :actions [:identify-dead-code :remove-dead-code :run-tests]
    :postconditions [{:type :test-status :expected :green}]}

   "stack-coherence/commit-intent-alignment"
   {:preconditions [{:type :git-status :has-changes true}]
    :actions [:stage-changes :write-commit :commit]
    :postconditions [{:type :git-status :clean true}]}})
```

### Phase 2: Observation → Belief Update

Map tool results to belief updates:

```clojure
(defn update-beliefs [beliefs observation]
  (case (:type observation)
    :test-result
    (-> beliefs
        (assoc :test-status (:status observation))
        (update :tau-tests adjust-precision (:status observation)))

    :tool-error
    (-> beliefs
        (update :uncertainty inc)
        (update :tau-approach * 0.8))

    beliefs))
```

### Phase 3: G Computation

Implement expected free energy for pattern selection:

```clojure
(defn compute-G [{:keys [pattern beliefs observations]}]
  (let [;; How well does pattern predict observations?
        fit (pattern-observation-fit pattern observations)
        ;; How much would applying this pattern reduce uncertainty?
        epistemic (expected-info-gain pattern beliefs)
        ;; How much does this pattern move toward goals?
        pragmatic (goal-alignment pattern beliefs)]
    {:G (+ (- fit) epistemic pragmatic)
     :components {:fit fit :epistemic epistemic :pragmatic pragmatic}}))
```

## Relation to Existing Work

### aif_bridge.clj

The existing bridge converts AIF traces to events. This remains useful for:
- Logging AIF decisions as session events
- Computing summary evidence (g-mean, tau-range)
- Validating pattern constraints

### Pattern Library

The 164+ patterns in `resources/sigils/patterns-index.tsv` become:
- Available policies for the AIF selector
- Priors informed by pattern rationales and hotwords
- Constraints that guide action generation

### hx.logic Validators

PUR/PSR validators ensure:
- Patterns selected are in the catalog
- Anchors resolve to actual session events
- Forecasts have checkable loci

## Open Questions

1. **Granularity**: Is pattern selection per-task, per-file, or per-action?
2. **Learning**: How do τ (precision) values persist across sessions?
3. **Hierarchy**: Can patterns compose (meta-patterns selecting sub-patterns)?
4. **Human-in-loop**: How does user approval/rejection feed back to beliefs?

## Next Steps

1. Define minimal belief state schema for coding sessions
2. Implement G computation for 3-5 pilot patterns
3. Wire into fuclaude session loop as pattern advisor
4. Capture PSR/PUR with AIF metadata
5. Evaluate: does AIF selection improve session outcomes?
