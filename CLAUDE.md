# Claude Code Instructions for Futon3

## What This Repository Is Now

Futon3 was the original monolithic repository. Its code has been
refactored into three focused repos:

| Repo | Concern | Timescale |
|------|---------|-----------|
| futon3a | Pattern search + querying | fast (query) |
| futon3b | Pattern-driven development | task + glacial |
| futon3c | Real-time coordination | social (real-time) |

**Futon3 is no longer a running codebase.** Nothing here should be on
a classpath or treated as importable code. Instead, futon3 serves
three purposes:

### 1. The Pattern Library

`library/` contains 853+ flexiarg patterns across 50 namespaces. This
is the canonical pattern library for the entire futon ecosystem. When
agents select patterns (PSR), they search here. When missions ground
their ARGUE decisions in existing patterns, they reference paths like
`library/aif/structured-observation-vector.flexiarg`.

Key namespaces:
- `aif/` — Active Inference patterns (observation vectors, EFE, precision, belief state)
- `realtime/` — 13 coordination patterns (authoritative-transcript, liveness-heartbeats, etc.)
- `futon-theory/` — structural theory (stop-the-line, structural-tension-as-observation)
- `agent/` — agent behavioral patterns (sense-deliberate-act, state-is-hypothesis)
- `gauntlet/` — peripheral gauntlet patterns (aif-as-environment-not-instruction)
- `social/` — social coordination (scope-bounded-handoff)
- `code-coherence/`, `stack-coherence/`, `devmap-coherence/` — quality patterns

### 2. Devmaps and the Holistic Argument

`holes/` contains:
- **9 devmaps** (`futon0.devmap` through `futon7.devmap`, plus `futon3a.devmap`)
  — one per repository, describing what each futon aspires to be.
- **The original holistic argument** (`holistic-argument-sketch.md` + `.sexp`)
  — the pre-three-pillars version, preserved as historical record.
  The updated argument lives in `futon3c/docs/holistic-argument.md`.
- **Mission source material** (`holes/missions/`) — the original mission
  documents from before the split. Many are superseded by futon3c versions
  but remain as design documentation.

### 3. Code as Design Documentation

`src/` and `scripts/` contain the original implementations. These are
**source material for porting, not running infrastructure.** When
porting code to futon3a/3b/3c, the existing code documents what worked
and what failed — read it as design documentation, not as code to copy
blindly. See futon3c's CLAUDE.md invariant I-5.

## The Three-Pillar Connection

The futon stack is organized around three pillars (see
`futon3c/docs/three-pillars.md`):

| Pillar | What lives here | What lives elsewhere |
|--------|----------------|---------------------|
| **The Argument** | Original sketch (`holes/holistic-argument-sketch.md`) | Updated version in `futon3c/docs/holistic-argument.md` |
| **The Invariants** | Pattern library grounds many invariants | Inventory in `futon3c/docs/structural-law-inventory.sexp` |
| **The Missions** | Original mission docs, devmaps | Active missions in `futon3c/holes/`, methodology in `futon3c/docs/futonic-missions.md` |

## Working with the Pattern Library

### Reading patterns

Each `.flexiarg` file is a structured argument with fields like
`:claim`, `:ground`, `:warrant`, `:backing`, `:qualifier`, `:rebuttal`.
To understand a pattern, read the `:claim` and `:ground` first.

### Searching patterns

`resources/sigils/patterns-index.tsv` is the index. Each row has
`pattern`, `tokipona`, `truth` (sigil), `rationale`, and `hotwords`.
Search by hotwords to find relevant patterns.

### Referencing patterns in missions

When grounding ARGUE decisions in patterns, use the library path:
```
Pattern grounding: A-1 (library/agent/sense-deliberate-act,
                        library/gauntlet/aif-as-environment-not-instruction)
```

## PSR/PUR/PAR Discipline

The exploratory pattern mode (observe → select → apply → log) still
applies when working with the pattern library. See the PSR/PUR format
in the sections below.

### Pattern Selection Record (PSR)

```
## PSR
- **Pattern chosen**: `code-coherence/dead-code-hygiene`
- **Candidates considered**: dead-code-hygiene, test-before-commit
- **Rationale**: Found unused helper in pattern_sense.clj
- **Confidence**: medium
```

### Pattern Use Record (PUR)

```
## PUR
- **Pattern**: `code-coherence/dead-code-hygiene`
- **Actions taken**: Removed `unused-helper-fn`
- **Outcome**: success
- **Prediction error**: low
```

## What Does NOT Belong Here

- New running code (goes to futon3a, futon3b, or futon3c)
- New mission documents (go to the relevant repo's `holes/`)
- Infrastructure or deployment config (goes to the relevant repo)
- Dependencies on futon3 being importable (invariant I-5 in futon3c)
