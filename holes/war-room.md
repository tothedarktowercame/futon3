# War Room — Inter-Mission / Inter-Futon Coordination

> *Victory begets enmity; the defeated dwell in pain.*
> *Happily the peaceful live, discarding both victory and defeat.*
> — Dhammapada 201 (trans. Buddharakkhita)

> *Hatred is never appeased by hatred in this world.*
> *By non-hatred alone is hatred appeased. This is a law eternal.*
> — Dhammapada 5 (trans. Buddharakkhita)

> *Happy indeed we live, friendly amidst the hostile.*
> *Among those who hate, we dwell free from hatred.*
> — Dhammapada 197 (trans. Buddharakkhita)

> *War is a crime against humanity.*
> *I am therefore determined not to support any kind of war,*
> *and to strive for the removal of all causes of war.*
> — War Resisters International

This document sits one level above mission control. Individual missions
(M-coordination-rewrite, M-drawbridge-multi-agent, etc.) operate within
a single futon. The War Room coordinates across futons and across missions.

## The Holistic Argument

**[holistic-argument-sketch.md](holistic-argument-sketch.md)** — Sketch of the
third-order self-representation: the stack arguing for its own existence and
next steps. Synthesizes devmaps, missions, patterns, evidence, and commercial
analysis (futon7) into a navigable case with explicit support/attack relations.
The bulletins below provide evidence; the holistic argument composes it. A
focused "prover" pass will solidify the argument later.

## War Bulletins

Strategic assessments with cross-futon implications. Each bulletin captures
findings from a significant effort and their impact on the stack. The war room
summarises; the bulletins provide evidence and argument.

| # | Date | Title | Key Findings |
|---|------|-------|-------------|
| [1](war-bulletin-1.md) | 2026-02-14 | What First Proof Proved About the Stack | Wiring diagrams operational (not decorative); AIF+ I1-I6 validated; depth frontier = domain knowledge; nLab validation path before superpod |
| [2](war-bulletin-2.md) | 2026-02-18 | The Evidence Landscape Takes Shape | futon3c operational (614 tests, 11 peripherals); evidence supersedes war artifacts; mission control makes portfolio computable; devmap updated (P12-P16); WR-3 validated (zero structural rewrites) |
| [3](war-bulletin-3.md) | 2026-02-22 | The Self-Representing Stack | Agent-to-agent IRC coordination operational; ideal/actual self-discrepancy as system architecture; Mission Control as physics engine (statics/dynamics/empirics); surface contracts as local reflexivity; system diagram at `futon3c/docs/mission-control-system.mm` |
| [4](war-bulletin-4.md) | 2026-02-27 | The Portfolio Becomes Legible | Portfolio inference live (CONSOLIDATE + "review"); observation surface was half-blind (4/7 repos, 0% coverage, 3 saturated channels) — now calibrated; 73 missions triaged into 7 salience buckets; M-self-representing-stack visible as next priority; AIF loop validated itself (recommended review → found broken sensors → fixed them) |
| [5](war-bulletin-5.md) | 2026-03-01 | The Tickle Experiment | Multi-agent CT review pipeline: 17 PRs, ~85 proposals assessed, ~60 entries merged; quality feedback loop validated (approval rate improved across batches); infrastructure bottlenecks identified (merge gap, stateless orchestrator, silent failures); surface contracts (I-1/I-2/I-3) held under sustained load; format evolved organically; experiment closed, M-self-representing-stack resumes |
| [6](../futon3c/holes/war-bulletin-6.md) | 2026-04-10 | The Inhabitation Threshold | REPL entry ceremony fixed (4 bugs); ghost reclamation; evidence pipeline operational; futon3a semantic context retrieval per turn; Arxana evidence browser wired; JSDQ hypergraph aligned; M-stack-inhabitation opened as umbrella mission; WR-4: inhabit before building |
| [7](war-bulletin-7.md) | 2026-04-13 | The War Machine Is Operational | War Machine runs 10 scans + frame reader; WebArxana multi-focus canvas (49 commits); frame schema (hives, ants, instruments, cardinal directions); M-daily-scan Day 1 (15 probes, 5 leads); depositing signal non-zero for first time; 3 depositing patterns; WR-5: War Machine is infrastructure not a mission; WR-6: daily scan is the depositing heartbeat; WR-7: missing model ports are sorrys |
| [8](war-bulletin-8.md) | 2026-04-24 | The Stack Argues With Itself, Then Demands Evidence | Holistic argument now machine-readable (16 leaf + 1 stack AIF+ files); compression algorithm picks its own successor (next-move = step PI); live `/api/alpha/aif-stack/live` endpoint with mission overlay; Web War Machine gains :aif-stack view + Recommended Next Move tile + Audacity-style waveform + HUD tethered to playhead; **empirical session→spine→bite upsample turns C1's bites from logical to measurable** (1/3 of C1 bites observed in 14d window — the inhabitation gap is now visible); 17 e2e tests; WR-8: AIF+ files are sources of truth, prose is regenerated; WR-9: bites must be empirically tested or marked logical; WR-10: the next-move surface IS the recursive closure |

## The Three-Futon Refactoring

futon3 was a monolith handling three distinct concerns at three distinct
timescales. It has been refactored:

```
futon3 (original monolith)
  ├── futon3a  — pattern search + querying         [fast / query]
  ├── futon3b  — pattern-driven development         [task + glacial]
  └── futon3c  — real-time coordination             [social / real-time]
```

### Why Three

Each futon is an AIF loop operating at a characteristic timescale:

| Futon | Loop | Timescale | What It Observes | What It Acts On |
|-------|------|-----------|------------------|-----------------|
| futon3a | — | fast | Query, return | Pattern library, meme store |
| futon3b L0 | Gate pipeline | task (~minutes) | Task requests | Code, evidence, proof paths |
| futon3b L1 | Library evolution | glacial (~weeks) | Accumulated tensions | Pattern library |
| futon3c | Social coordination | real-time (~seconds) | Agent messages, events | Forum posts, peripheral dispatch, session state |

The three futons share the pattern library as common ground. Patterns written
in futon3b's glacial loop are used by futon3c's social loop and queried by
futon3a's search. The library is the slow variable that constrains all faster
loops (I3: timescale separation).

### Interaction Points

```
futon3c (social)  →  futon3b (task)  →  futon3b L1 (glacial)
   │                    │                    │
   │ agent dispatches   │ gate validates     │ library evolves
   │ forum coordinates  │ evidence persists  │ patterns canonize
   │ peripherals hop    │ proof paths close  │ constraints tighten
   │                    │                    │
   └───── futon3a (query) ─────────────────────┘
          searches across all stores
```

## Mission Portfolio

> **Note:** The authoritative mission inventory is computed by Mission Control
> (futon3c `mc-inventory`, `mc-coverage`). This section records strategic
> framing, not an exhaustive list. See each repo's `holes/missions/` directory
> and `arxana://view/missions` for the live state.

### Umbrella Missions (active, cross-futon)

| Mission | Repo | Status | Bulletin |
|---------|------|--------|----------|
| **M-stack-inhabitation** | futon3c | IDENTIFY | [6](../futon3c/holes/war-bulletin-6.md) |
| **M-self-representing-stack** | futon4 | VERIFY | [3](war-bulletin-3.md), [4](war-bulletin-4.md) |

M-stack-inhabitation is the current priority: inhabit the surfaces that exist
rather than building new ones. Sub-missions: M-repl-wins-over-cli (TESTING),
M-hypergraph-operator (futon5a, IDENTIFY), candidate invariant triage (65 items),
excursion pipeline triage (6 items).

### Completed Infrastructure (load-bearing, not actively developed)

- **futon3a:** Pattern search, meme store, semantic retrieval (MiniLM embeddings)
- **futon3b:** Gate pipeline, library evolution (31 tests, 109 assertions)
- **futon3c Agency:** Registry, dispatch, evidence store, blackboard, invoke
- **futon1a:** Durable XTDB evidence backend, HTTP API
- **futon4 Arxana:** Hypergraph browser, evidence viewer, docbook navigation

### Joe's Three Workstreams

Per WR-4, Joe's work divides into three mutually-reinforcing streams.
Stack inhabitation is valuable only insofar as it serves the other two:

| Stream | What it produces | Stack surface |
|--------|-----------------|---------------|
| **Mathematics** (futon6) | Prelim solutions, proof sprints, nLab validation | REPL + evidence |
| **Day job + consulting** (JSDQ) | UKRN-S, Eric/VSAT, Bristol May 12th | Hypergraph operator |
| **Stack inhabitation** (futon0-5) | Working REPL, evidence pipeline, pattern retrieval | All surfaces |

The danger (per JSDQ diagnosis): stack work becomes pi-hermit (build alone,
never deposit). WR-4 constrains this: stack work must demonstrably serve
mathematics or consulting, or it is foraging when it should be depositing.

## War Room Decisions

Decisions that affect multiple futons or multiple missions:

### WR-1: Three-futon split (2026-02-09)

**IF:** futon3 handles pattern search, gate pipelines, library evolution,
agent dispatch, forum coordination, and peripheral management
**HOWEVER:** These operate at fundamentally different timescales and have
different change rates
**THEN:** Split into futon3a (search), futon3b (development), futon3c (realtime)
**BECAUSE:** Each loop should be independently deployable, testable, and
evolvable. The AIF framework (I3: timescale separation) demands that loops
at different timescales be structurally separated.

**Evidence:**
- futon3b mission (M-coordination-rewrite) is complete and coherent without
  any reference to Agency, Forum, or peripherals
- futon3c's 13 realtime patterns are disjoint from futon3b's 12 coordination
  patterns
- futon3a already existed as a separate concern

### WR-2: Pattern library stays in futon3 (2026-02-09)

**IF:** All three futons read from the pattern library
**HOWEVER:** The library should have one authoritative location
**THEN:** futon3/library/ remains the canonical source. futon3b and futon3c
may have local copies for testing but should not diverge from the canonical
source.
**BECAUSE:** The library is the shared slow variable (I3). Multiple copies
with independent evolution would violate I4 (preference exogeneity) — fast
loops would be able to modify their own constraints.

### WR-3: Social exotype before implementation (2026-02-09)

**IF:** futon3c needs to implement Agency, Forum, peripherals, and bridges
**HOWEVER:** Three previous attempts at social-layer code failed (Agency 3
generations, Forum abandoned, bridge perpetual fix-cycle) because invariants
were discovered retroactively instead of specified proactively
**THEN:** Write the social exotype diagram and validate it before writing any
futon3c implementation code. Extend futon5's composition framework to validate
the three-diagram stack (futon3a + futon3b + futon3c) as one system.
**BECAUSE:** The derivation xenotype requires VERIFY before INSTANTIATE. The
coordination rewrite succeeded because the exotype existed before the gates.
The social loop must follow the same path. Additionally, the composition
framework is isomorphic to pheno-geno-exo stacking in futon5 — building it
earns the theoretical readiness (mana) that unblocks gated futon5 missions.

**Evidence:**
- `library/social/ARGUMENT.flexiarg` establishes S1-S3, R1-R10, C1-C6
- futon3b succeeded with exotype-first approach (8/8 checks, 31 tests)
- futon5 missions gated on mana are gated on composition validation capability

### WR-4: Inhabit before building (2026-04-10)

**IF:** The stack has extensive infrastructure (Agency, evidence stores, pattern
libraries, Arxana browser, candidate invariant queues, mission documents,
excursion logs, Bayesian models, context retrieval)
**HOWEVER:** Most of this infrastructure is uninhabited — the Candidate Invariant
queue has 65 entries with no processing pipeline, the evidence browser was
disconnected, excursions are explorations without missions, and the primary coding
surface (CLI) generated no evidence until today
**THEN:** The next phase is inhabitation, not construction. New infrastructure is
justified only when it directly serves inhabitation of existing surfaces. The
three workstreams (mathematics, consulting/JSDQ, stack inhabitation) must be
mutually reinforcing: stack work that doesn't serve math or consulting is
pi-hermit mode (build alone, never deposit) and violates the cargo-implies-depositing
constraint.
**BECAUSE:** `surface-earns-inhabitation` — uninhabited infrastructure is dead
infrastructure. `inhabitation-feeds-evolution` — without inhabitation the Baldwin
loop starves. The JSDQ diagnosis shows the same pattern at the personal level:
high cargo, low reserves, mode stuck on :foraging. Bristol is the external
depositing action; stack inhabitation is the internal one. Both require the same
policy transition: pi-scholar -> pi-consultant -> pi-free-solo.

**Evidence:**
- [Bulletin 6](../futon3c/holes/war-bulletin-6.md): evidence loop was 90% built,
  last 10% was wiring and friction reduction
- JSDQ terminal vocabulary: three constraint violations (cargo-implies-depositing,
  signal-implies-follow-up, deadline-implies-urgency) all point at depositing
- M-stack-inhabitation portfolio: 10 surfaces, most uninhabited

**Test:** Before starting any new infrastructure mission, ask: "Which existing
surface does this make more inhabitable?" If the answer is "none," defer the
mission until WR-4 is satisfied.

### WR-8: AIF+ files are sources of truth, prose is regenerated (2026-04-24)

**IF:** The holistic argument was historically maintained as prose
(`holistic-argument-sketch.md`, `holistic-argument-aif2.md`)
**HOWEVER:** Prose drifted from intent because there was no machine-readable
intent it had to track — every re-render was an editorial interpretation
**THEN:** The 16 leaf-level + 1 stack-level `*.aif.edn` files at
`futon5a/holes/stories/` are the canonical self-model. Prose deliverables
are *generated* by Babashka renderers (`render_aif2_prose.clj`,
`render_leaf_prose.clj`, `render_external_prompt.clj`) and re-rendered when
the drift detector (`detect_drift.clj`) reports source-mtime change.
Editing prose in place is a defect.
**BECAUSE:** The AIF+ format carries typed argumentative structure (claims,
supports, attacks, falsifiability, conflicts) that prose flattens. If the
typed structure is canonical, drift between argument and document becomes
detectable instead of perennial.

**Evidence:**
- 16 leaf files + THE-STACK.aif.edn (307 lines) encode all argumentative
  structure that prose was previously asserting informally
- Live endpoint `/api/alpha/aif-stack/live` projects this structure with
  mission overlay; same data feeds the Web War Machine demo
- Bulletin 8 documents the encoding effort and the prose-as-derived contract

### WR-9: Bites must be empirically tested or marked logical (2026-04-24)

**IF:** Conflict bites in the AIF+ model assert "this conflict damages this
part of the stack"
**HOWEVER:** Such claims are formal until session evidence supports them —
the cached weight on a conflict is a hypothesis, not a measurement
**THEN:** The Web War Machine's empirical join (`aif_join.cljs`) walks
session steps → repos → `devmap-<repo>` leaves → spine-ids and renders C→S
bite edges as **solid red** (with width/opacity scaling on hit count) when
the bitten spine has empirical activity in the visible window, and
**dashed grey** when the bite is still purely logical. Conflicts whose
bites remain grey-dashed across multiple windows are candidates for weight
reduction or removal.
**BECAUSE:** I1 (evidence supersedes assertion) applies inside the
self-model itself. A conflict whose damage cannot be observed is not yet
a conflict, only a worry. The visual distinction (red vs grey-dashed) makes
the inhabitation gap measurable without requiring the operator to query
anything — it stares back from the demo.

**Evidence:**
- C1 bites [S6, S8, S4] in the cached model; in today's 14d window only S8
  is empirically observed → 1/3 empirical coverage. The grey-dashed S6/S4
  edges are the inhabitation gap, made visible.
- 17 e2e tests gate the empirical-bite plumbing including
  `tests/empirical-bites.spec.ts`

### WR-10: The next-move surface is the recursive closure (2026-04-24)

**IF:** Joe asked for a hierarchical compression algorithm that names what's
been built, names THE next thing to work on, and collapses repeated structure
**HOWEVER:** A one-off prose document that does this collapses back into
drift the moment the stack moves
**THEN:** The Recommended Next Move tile in the Web War Machine reads
`:reading :next-move` directly off the live endpoint. As long as the AIF+
files are kept current (drift detector + re-render + WR-8), the tile always
recommends the move that the stack itself argues is highest expected value,
with full provenance (rationale, feeding-input, alternatives-considered) and
a click-through to the conflict it dis-bites.
**BECAUSE:** The tool that picks next-moves IS the next-move. Stepping the
Portfolio Inference AIF loop (the current recommendation) converts this from
a hand-crafted recommender into a continuously running EFE-ranker. When the
recommendation flips off "step PI," it will be because PI has been stepped
and the stack's own evidence said so. This is the recursive closure: the
compression algorithm becomes a peripheral instead of a document.

**Evidence:**
- THE-STACK.aif.edn `:reading :next-move` field (lines 268-292) encodes the
  3-criterion ranking (direct leverage / unlock cascade / self-referential
  closure) that produced "step PI" as the winner over 16 leaf-level
  candidate moves
- Live endpoint serves it; tile renders it; Show↔Hide button drives the
  conflict detail box

## Coordination Protocol

When a decision affects multiple futons:

1. Write it here (War Room) with IF/HOWEVER/THEN/BECAUSE
2. Update affected AGENTS.md files in each futon
3. If the decision changes a pattern, update futon3/library/ (canonical)
4. Cross-reference: each futon's AGENTS.md should link to this document
   for inter-futon decisions
