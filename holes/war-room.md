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
- **M-coordination-rewrite** [ACTIVE] — Gate pipeline + library evolution
  - Phase 0 (search): DONE
  - Phase 1 (gate scaffold): DONE
  - Phase 2 (store integration): CURRENT
  - Phase 3 (Level 1): PLANNED
- Tracked in: `futon3b/AGENTS.md`

### futon3c (new)
- **M-agency-refactor** [PLANNED] — Port + refactor Agency from futon3
- **M-forum-refactor** [PLANNED] — Port + refactor Forum from futon3
- **M-peripheral-model** [PLANNED] — Formalize peripheral spec, implement hop
- Source missions in futon3: M-drawbridge-multi-agent, M-par-session-punctuation,
  M-agency-rebuild, M-agency-forum

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

## Coordination Protocol

When a decision affects multiple futons:

1. Write it here (War Room) with IF/HOWEVER/THEN/BECAUSE
2. Update affected AGENTS.md files in each futon
3. If the decision changes a pattern, update futon3/library/ (canonical)
4. Cross-reference: each futon's AGENTS.md should link to this document
   for inter-futon decisions
