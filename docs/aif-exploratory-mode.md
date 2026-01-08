# AIF Exploratory Mode for Coding Agents

## Overview

This is a follow-on to `aif-pattern-engine.md`. That document describes the
architecture for AIF-driven pattern selection. This document addresses the
**invocation model**: how do we move from directive runs (human specifies
patterns and tasks) to exploratory runs (agent discovers, applies, and proposes
patterns)?

## The Problem with Directive Runs

Current fucodex invocation is highly directive:

```bash
./fucodex --live \
  --clock-in ants/white-space-scout \
  --clock-in ants/pheromone-trail-tuner \
  --patterns ants/white-space-scout,ants/pheromone-trail-tuner \
  exec "Make two small changes: (1) auto-select first session when stdin
        is not a TTY; (2) add --session-id option. Do not run tests."
```

Here the human:
- Specifies which patterns to use
- Specifies the exact task
- Specifies constraints (no tests)

Patterns act as **constraints on execution**, not **hypotheses to test**.

## Exploratory Mode

An exploratory run inverts the control:

```bash
./fucodex explore \
  --catalog patterns-index.tsv \
  --scope src/ants/aif \
  --max-cycles 5
```

Or even simpler:

```bash
./fucodex explore "Improve the AIF implementation"
```

The agent:
1. Reads the pattern catalog
2. Examines the codebase scope
3. Picks an applicable pattern (lowest G)
4. Makes a bounded change guided by that pattern
5. Logs PSR (what it chose) and PUR (what happened)
6. Uses evidence to pick the next pattern
7. Stops when out of ideas
8. Proposes new patterns if gaps were found

## Belief State for Exploration

The ants AIF state is `{:mu beliefs, :prec precision, :mode mode}`.

For exploratory coding, the belief state expands:

```clojure
{:mu {:patterns-tried    []        ; patterns applied this session
      :patterns-available [...]    ; from catalog
      :evidence          []        ; PSR/PUR log
      :codebase-state    {...}     ; files touched, changes made
      :uncertainty       {...}}    ; areas not yet understood
 :prec {:tau 1.0                   ; confidence in current approach
        :pattern-fit {...}}        ; per-pattern confidence
 :mode :exploring}                 ; vs :applying, :proposing
```

## The Exploration Loop

```
                    ┌─────────────────────────────────────┐
                    │  OBSERVE                            │
                    │  - Read pattern catalog             │
                    │  - Examine codebase state           │
                    │  - Note what's unclear              │
                    └──────────────┬──────────────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────────────┐
                    │  SELECT (minimize G)                │
                    │  - epistemic: what teaches most?    │
                    │  - pragmatic: what makes progress?  │
                    │  - fit: what matches context?       │
                    └──────────────┬──────────────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────────────┐
                    │  APPLY                              │
                    │  - Make bounded changes             │
                    │  - Log PSR (decision + rationale)   │
                    └──────────────┬──────────────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────────────┐
                    │  EVALUATE                           │
                    │  - Did it work?                     │
                    │  - Log PUR (outcome)                │
                    │  - Adjust tau for this pattern      │
                    └──────────────┬──────────────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────────────┐
                    │  CONTINUE / PROPOSE / EXIT          │
                    │                                     │
                    │  if (patterns left & ideas left):   │
                    │    → SELECT                         │
                    │  elif (gap found):                  │
                    │    → PROPOSE new pattern            │
                    │  else:                              │
                    │    → EXIT with summary              │
                    └─────────────────────────────────────┘
```

## Expected Free Energy for Pattern Selection

When choosing which pattern to apply, compute G for each candidate:

```
G(pattern) = epistemic_value(pattern)
           + pragmatic_value(pattern)
           - pattern_fit(pattern, context)
```

Where:
- **Epistemic value**: How much would applying this pattern reduce uncertainty
  about the codebase? (High for unexplored areas)
- **Pragmatic value**: How much does this pattern align with the objective?
  (High if pattern rationale matches stated goal)
- **Pattern fit**: How well does the pattern's preconditions match current
  observations? (Low penalty if constraints satisfied)

This mirrors the ants implementation in `pattern_efe.clj`.

## Evidence Structure

Each cycle produces PSR and PUR:

**PSR (Pattern Selection Record)**:
```clojure
{:psr/id "psr-001"
 :cycle 1
 :candidates ["code-coherence/dead-code-hygiene"
              "stack-coherence/test-before-commit"]
 :chosen "code-coherence/dead-code-hygiene"
 :rationale "Found unused functions in pattern_sense.clj"
 :G-scores {"code-coherence/dead-code-hygiene" 0.42
            "stack-coherence/test-before-commit" 0.78}
 :tau 0.65}
```

**PUR (Pattern Use Record)**:
```clojure
{:pur/id "pur-001"
 :psr/id "psr-001"
 :pattern "code-coherence/dead-code-hygiene"
 :actions-taken [{:type :edit :file "src/ants/aif/pattern_sense.clj"
                  :description "Removed unused helper function"}]
 :outcome :success
 :prediction-error 0.12
 :tau-updated 0.71
 :notes "Simpler than expected, no dependencies"}
```

## Exit Conditions

The agent exits exploration when:

1. **Ideas exhausted**: All applicable patterns tried, no new changes identified
2. **Uncertainty bottomed out**: tau is high across all remaining patterns
   (nothing left to learn)
3. **Cycle limit reached**: `--max-cycles` exceeded
4. **Gap found**: No existing pattern fits, agent has a proposal to make

## Pattern Proposal

When exploration reveals a gap, the agent proposes a new pattern:

```clojure
{:proposal/id "prop-001"
 :pattern-name "aif/evidence-chain-integrity"
 :rationale "Noticed PSR/PUR pairs can become orphaned when cycles are
             interrupted. No existing pattern addresses evidence continuity."
 :suggested-preconditions ["PSR exists without matching PUR"
                           "Session has incomplete cycles"]
 :suggested-actions ["Scan for orphaned PSRs"
                     "Either complete PUR or mark as abandoned"]
 :evidence ["psr-003 has no PUR after edit failed"
            "Session summary omits incomplete cycle"]}
```

## Implementation Plan

### Phase 1: Manual Exploration (fuclaude)

1. Write an exploratory system prompt that instructs Claude to:
   - Read the pattern catalog
   - Pick patterns based on context
   - Make bounded changes
   - Log evidence in PSR/PUR format
   - Propose patterns when gaps found

2. Test with real sessions, manually triggering the loop

### Phase 2: Structured Loop (fucodex)

1. Add `explore` subcommand to fucodex
2. Implement pattern catalog reader
3. Implement G computation for pattern selection
4. Wire PSR/PUR logging into session trace
5. Add exit condition detection

### Phase 3: Learning Across Sessions

1. Persist pattern tau values across sessions
2. Use historical evidence to inform pattern selection
3. Track pattern proposal → acceptance → catalog update cycle

## Relation to Ants Implementation

The ants work in futon2 provides the template:

| Ants Concept | Exploratory Mode Equivalent |
|--------------|----------------------------|
| `pattern_sense.clj` | Pattern-context matching for code |
| `pattern_efe.clj` | G computation for pattern selection |
| `cyber-pattern :id` | Currently active pattern |
| `ticks-active` | Cycles on current pattern |
| `mode` (:outbound/:homebound) | Mode (:exploring/:applying/:proposing) |
| `constraint-satisfied?` | Pattern preconditions met? |

## Open Questions

1. **Scope boundaries**: How does the agent know what's in-scope for changes?
2. **Cycle granularity**: Is a cycle one file edit? One logical change? One
   pattern application?
3. **Human checkpoints**: Should the agent pause for approval between cycles?
4. **Catalog format**: Is patterns-index.tsv sufficient, or do we need richer
   pattern metadata?

## Next Steps

1. Draft the exploratory system prompt for fuclaude
2. Test manually with a real session
3. Iterate on evidence format
4. Build toward structured fucodex support
