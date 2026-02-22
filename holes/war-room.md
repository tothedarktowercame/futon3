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

This document sits one level above mission control. Individual missions
(M-coordination-rewrite, M-drawbridge-multi-agent, etc.) operate within
a single futon. The War Room coordinates across futons and across missions.

## War Bulletins

Strategic assessments with cross-futon implications. Each bulletin captures
findings from a significant effort and their impact on the stack. The war room
summarises; the bulletins provide evidence and argument.

| # | Date | Title | Key Findings |
|---|------|-------|-------------|
| [1](war-bulletin-1.md) | 2026-02-14 | What First Proof Proved About the Stack | Wiring diagrams operational (not decorative); AIF+ I1-I6 validated; depth frontier = domain knowledge; nLab validation path before superpod |
| [2](war-bulletin-2.md) | 2026-02-18 | The Evidence Landscape Takes Shape | futon3c operational (614 tests, 11 peripherals); evidence supersedes war artifacts; mission control makes portfolio computable; devmap updated (P12-P16); WR-3 validated (zero structural rewrites) |
| [3](war-bulletin-3.md) | 2026-02-22 | The Self-Representing Stack | Agent-to-agent IRC coordination operational; ideal/actual self-discrepancy as system architecture; Mission Control as physics engine (statics/dynamics/empirics); surface contracts as local reflexivity; system diagram at `futon3c/docs/mission-control-system.mm` |

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

## Active Missions by Futon

### futon3a (stable, operational)
- No active missions. Query infrastructure works.
- Pattern search (notions/compass), meme store, typed arrows.

### futon3b
- **M-coordination-rewrite** [DONE] — Gate pipeline + library evolution
  - Part I (composition plan): DONE (CP-0)
  - Part II (gate pipeline): DONE (CP-1, CP-2, CP-2b, CP-3)
  - Part III (Level 1 glacial loop): DONE (CP-4)
  - 31 tests, 109 assertions, ct/mission.clj 8/8 checks
- Tracked in: `futon3b/AGENTS.md`, `futon3b/holes/missions/M-coordination-rewrite.md`

### futon3c (614 tests, 11 peripherals — see [Bulletin 2](war-bulletin-2.md))

**Complete:**
- **M-social-exotype** — Social exotype diagram + nested composition + bootstrap closure
- **M-agency-refactor** — Agency registry, session lifecycle, multi-agent dispatch
- **M-peripheral-model** — Hop protocol, exit conditions, peripheral chain orchestration
- **M-peripheral-behavior** — Peripheral lifecycle (start/step/stop), tool-set enforcement
- **M-dispatch-peripheral-bridge** — S-dispatch → peripheral routing
- **M-transport-adapters** — HTTP, WebSocket, IRC (F1-F6 stability), 112 tests
- **M-mission-peripheral** — 9-phase cycle machine, obligation tracking, evidence snapshots
- **M-mission-control** — Portfolio observation: cross-repo inventory, devmap coverage, mana queries, backfill (35 tests)
- **M-IRC-stability** — F1-F6 compliance, standup bell

**Active:**
- **M-peripheral-gauntlet** — Real-time multi-peripheral scenarios (alleycat 4/5 gates PASS)
- **M-forum-refactor** — Part I complete (evidence landscape supersedes forum); Parts II-III pending
- **M-sliding-blackboard** — Emacs UI + code blocks for IDE integration
- **M-psr-pur-mesh-peripheral** — Discipline peripheral (PSR/PUR mesh, wired)

**Infrastructure:** Evidence landscape (store + XTDB backend + HTTP API via futon1a),
social pipeline (5 stages), proof peripheral (DAG backend, canonical forms, cycle
detection, ledger ops). All tech debt resolved (I3 snapshot, lossy coercion, hop
exit inference). See [Bulletin 2](war-bulletin-2.md) for full assessment.

### Cross-Futon
- **futon5** — Wiring diagrams (11 EDN devmaps + 2 grounding functors), CT DSL,
  tensor math. Now readable from futon3c via mission control (`mc-devmaps`,
  `mc-coverage`). Nonstarter pre-registration framework operational (core.clj).
  Spring Break target: JAX-based CA + differentiable eigendecomposition.
  See [Bulletin 1](war-bulletin-1.md) for strategic assessment.
- **futon6** — Mathematics dictionary + First Proof sprint (6/10 correct,
  70 typed wiring diagrams, AIF+ audit). nLab validation path: 20K pages
  already available, `hyperreal.py` builds free category, wiring diagram
  extraction in progress. Superpod deferred to post-Spring-Break.
  See [Bulletin 1](war-bulletin-1.md) for full findings.
- **futon4** — Arxana hypertext layer. Scholium-based annotation provides
  edge-level critique infrastructure validated by First Proof. Evidence
  landscape viewer planned (read from `GET /api/alpha/evidence` endpoints).
- **futon1a** — Durable store. XTDB backend operational for evidence landscape.
  HTTP API: POST/GET evidence, single entry, reply chains. Must preserve
  negative knowledge (failed routes with structural obstructions).
- **Operational bridge: IRC agent coordination** — Claude and Codex coordinate
  on `#futon` in real time. Codex scopes tasks as GitHub issues, @mentions
  Claude, Claude executes. Surface contracts ensure agents know which surface
  they're on. See [Bulletin 3](war-bulletin-3.md) §1.
- **Pending bridge: futon3b → futon3c** — Gate pipeline proof-paths should
  emit evidence entries on G0 success. This connects task-timescale validation
  to social-timescale observation. See [Bulletin 2](war-bulletin-2.md) §6.
- **System self-representation (P11)** — Mission Control system diagram at
  `futon3c/docs/mission-control-system.mm`. Ideal/actual split identified;
  reflexivity loop mapped but not yet closed. See [Bulletin 3](war-bulletin-3.md).

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

## Coordination Protocol

When a decision affects multiple futons:

1. Write it here (War Room) with IF/HOWEVER/THEN/BECAUSE
2. Update affected AGENTS.md files in each futon
3. If the decision changes a pattern, update futon3/library/ (canonical)
4. Cross-reference: each futon's AGENTS.md should link to this document
   for inter-futon decisions
