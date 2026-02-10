# War Room — Inter-Mission / Inter-Futon Coordination

This document sits one level above mission control. Individual missions
(M-coordination-rewrite, M-drawbridge-multi-agent, etc.) operate within
a single futon. The War Room coordinates across futons and across missions.

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

### futon3c
- **M-social-exotype** [ACTIVE] — Social exotype diagram + nested composition
  - VERIFY step of derivation xenotype (IDENTIFY through ARGUE complete)
  - Part I: social-exotype.edn (standalone 8/8 validation) [DONE]
  - Part II: nested composition framework (futon5 contribution, Codex issue #2) [IN PROGRESS]
  - Part III: gate-governed build (Prototype 0 — bootstrap closure) [DONE]
    - 3 proof-paths in futon3b/data/proof-paths/ (Part I, II, III)
    - `futon3b/src/futon3b/bootstrap.clj` — REPL helper
  - Argument: `futon3/library/social/ARGUMENT.flexiarg`
  - Tracked in: `futon3c/holes/missions/M-social-exotype.md`
- **M-agency-refactor** [BLOCKED on M-social-exotype] — Port + refactor Agency
- **M-forum-refactor** [BLOCKED on M-social-exotype] — Port + refactor Forum
- **M-peripheral-model** [BLOCKED on M-social-exotype] — Formalize peripheral spec
- Source missions in futon3: M-drawbridge-multi-agent, M-par-session-punctuation,
  M-agency-rebuild, M-agency-forum (evidence of tensions, not specifications)

### Cross-Futon
- **futon5** — Wiring diagrams, CT DSL, tensor math. Provides the formal
  specification language for all AIF loops.
- **futon6** — Mathematics dictionary. Uses futon3's patterns and futon3a's
  search. Independent development track.
- **futon1a** — Durable store. Shared infrastructure for evidence persistence.

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
