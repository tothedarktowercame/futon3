# Futon3 Active Prototypes — Completion Plan

This plan covers the work needed to complete P0, P2, P3, P4, and P5 from the
futon3.devmap. These prototypes are all `:active` (operational but incomplete).

## Overview

| Stage | Focus | Prototypes | Gate |
|-------|-------|------------|------|
| 1 | Contract & Bridge | P0, P5 | Contract frozen, bridge designed |
| 2 | Persistence Layer | P3, P4, P5 | futon1 persistence operational |
| 3 | APIs & Adapters | P2, P4, P5 | HTTP endpoints live |
| 4 | Rollups & Testing | P0, P2, P3, P4, P5 | Golden tests pass |
| 5 | Actionable Checklist (Mana Readiness) | P0, P2, P3, P4, P5 | Mana donation surfaces live + verified |

## Stage 1: Contract & Bridge (Foundation)

**Goal**: Freeze interfaces and establish the futon3/futon3a connection.

### P0: MUSN Transport

- [x] Freeze transport contract v1 in `docs/protocol/transport-contract-v1.md`
  - Document event types: pattern-apply, gap-report, trail-capture
  - Specify NDJSON log format with schema
  - Version the contract (v1.0)

- [x] Document sandbox profile as canonical proofwork runtime
  - Capture environment variables, paths, dependencies
  - Publish as `docs/protocol/sandbox-profile.md`

### P5: Similarity Field (futon3a integration)

- [x] Design futon3/futon3a bridge architecture
  - Option A: futon3 HUD calls futon3a portal via Drawbridge ← **selected**
  - Option B: futon3 imports futon3a as library dependency
  - Option C: Merge futon3a into futon3
  - Document decision in `docs/architecture/futon3a-integration.md`

- [x] Decide: unify pattern stores vs. maintain bridge
  - futon3 has `resources/sigils/patterns-index.tsv` ← **authoritative**
  - futon3a has `resources/notions/` with embeddings ← **derived**
  - Sync is explicit; rebuilds are versioned

**Exit criteria**: Contract doc exists and is versioned. Bridge architecture decided.

---

## Stage 2: Persistence Layer (Storage)

**Goal**: Replace file-based queues with futon1 persistence.

### P4: Workday Instrumentation

- [ ] Replace `futon3/logs/workday.edn` queue with futon1 persistence
  - Design workday->graph schema (entity type, attributes)
  - Implement adapter in `src/futon3/futon1_bridge.clj`
  - Migrate existing workday.edn entries

### P5: Similarity Field

- [ ] Wire futon3a embeddings into futon3 pattern search API
  - HUD server (`src/futon3/fulab/hud.clj`) gains embedding-based ranking
  - Pattern suggestions use GloVe/MiniLM similarity, not just hotword matching
  - Fallback to token matching when embeddings unavailable

### P3: Trail & Proof-State Journal

- [ ] Persist cue annotations into trail export
  - Cue events from `/musn/cues` saved alongside trail events
  - Trail export includes `:cue/text`, `:cue/pattern-id`, `:cue/timestamp`

**Exit criteria**: Workday and cues flow to futon1. HUD uses embeddings.

---

## Stage 3: APIs & Adapters (Interfaces)

**Goal**: Expose capabilities over HTTP and wire cross-futon flows.

### P2: Check DSL & Applicability Engine

- [x] Expose `/musn/check` over HTTP
  - Endpoint accepts: pattern-id, context EDN, evidence refs
  - Returns: status, missing fields, derived tasks
  - Requires Stage 1 contract freeze

- [x] Ship futon1/futon2 adapters for checks
  - `proof->graph`: Accepted checks become futon1 relations
  - `proof->energy`: Checks emit viriya deltas for futon2 dashboards
  - Lock adapters with fixtures and schemas

### P5: Similarity Field

- [x] Add `nearest-patterns` command to fuclaude/fucodex
  - `scripts/nearest-patterns <pattern-id> [--limit N] [--method glove|sigil]`
  - Also available as RPC from runners
  - Update musn-help to document it

### P4: Workday Instrumentation

- [x] Add HTTP affordances for instrumentation stewards
  - `POST /musn/workday/submit` for external clients
  - Returns pattern hits/misses and follow-up obligations
  - Swagger/OpenAPI spec in `docs/api/`

**Exit criteria**: `/musn/check` and `/musn/workday/submit` respond. Adapters tested.

---

## Stage 4: Rollups & Testing (Polish)

**Goal**: Golden tests, instrumentation, and proof artifacts.

### P0: MUSN Transport

- [ ] Add golden transcripts for pattern-apply, gap-report, trail-capture
  - Place in `test/fixtures/golden/`
  - CI validates transcripts are reproducible

### P3: Trail & Proof-State Journal

- [ ] Implement daily rollups of clause advancement
  - Script or service that aggregates trails by day
  - Shows which devmap clauses advanced
  - Output: `lab/rollups/YYYY-MM-DD.edn`

- [ ] Add proof artifacts (HUD screenshots or /musn/cues transcripts)
  - Capture mechanism for visual proof
  - Store in `lab/artifacts/`

### P5: Similarity Field

- [ ] Publish deterministic neighbourhood tests
  - Given pattern X, neighbours are always [A, B, C] in that order
  - Tests for both sigil-distance and GloVe methods
  - Place in `test/futon3/similarity_test.clj`

### P2: Check DSL

- [ ] Add check latency instrumentation
  - Metrics: p50, p95, p99 check times
  - Log slow checks (>100ms)

### P4: Workday Instrumentation

- [ ] Integrate fubar.el alongside aob-chatgpt
  - Emacs can submit workday claims via HTTP or Drawbridge
  - Document in `contrib/futon3-emacs.el` or similar

**Exit criteria**: All golden tests pass. Rollups generate. fubar.el integrated.

---

## Stage 5: Actionable Checklist (Mana Readiness)

**Goal**: Make mana donation & precision signals routine for futon3 agents, with
clear control surfaces and evidence that the loop works.

### Checklist

**Surface A: Donation & precision plumbing**
- [ ] Add a Nonstarter control surface (CLI or RPC) that can:
  - donate mana (sospeso) with a note, and
  - cast hypothesis votes (precision).
- [ ] Ensure the surface accepts config (db path + defaults) so it works across futon instances.
- [ ] Add a minimal “mana status” command (pool balance, total donated, total funded).

**Surface B: Evidence & log linkage**
- [ ] Log mana donations and votes into the MUSN trail (event type + note + amount).
- [ ] Ensure trails include enough detail to reconstruct “why donation happened”.

**Surface C: Operational tests**
- [ ] A golden transcript that simulates:
  - performing a non-trivial action,
  - triggering sospeso,
  - donating mana + writing blast radius note.
- [ ] A vote‑reorder test showing a lower‑priority item rises above others.

**Surface D: Operator guidance**
- [ ] Short doc snippet: how agents decide “donate vs vote”.
- [ ] Example: “sospeso: ASSURANCE_REQUIRED” formatted donation note.

**Exit criteria**: Donation + vote surfaces are live, logged, and verified by golden tests; operators can follow the checklist without custom setup.

---

## Dependencies Graph

```
Stage 1 (Foundation)
├── P0: transport contract ────┐
└── P5: bridge architecture ───┤
                               │
Stage 2 (Storage)              │
├── P4: workday persistence ───┤
├── P5: embeddings in HUD ─────┤
└── P3: cue persistence ───────┤
                               │
Stage 3 (APIs)                 │
├── P2: /musn/check HTTP ◄─────┘ (needs contract)
├── P2: futon1/futon2 adapters
├── P5: nearest-patterns CLI
└── P4: /musn/workday/submit HTTP
                               │
Stage 4 (Polish)               │
├── P0: golden transcripts ◄───┘
├── P3: daily rollups
├── P3: proof artifacts
├── P5: deterministic tests
├── P2: latency instrumentation
└── P4: fubar.el integration
                               │
Stage 5 (Mana Readiness)       │
├── Donation + vote surfaces live
├── Trails include mana events
├── Golden tests for sospeso + vote reorder
└── Operator guidance published
```

---

## Estimation Notes

Each stage is designed to be completable in a focused work session. Stage 1 is
mostly documentation and decision-making. Stages 2-3 are implementation-heavy.
Stage 4 is testing and polish. Stage 5 is a short integration pass to make
mana donation surfaces operational.

The futon3a integration (P5) touches all stages because it's currently a
separate repo. The Stage 1 decision about bridge vs. merge will affect how
much work Stages 2-4 require.

---

## Related Documents

- `holes/futon3.devmap` — Full prototype definitions
- `futon3a/README.md` — Portal and sidecar architecture
- `futon3a/docs/pattern-indexing.md` — Embedding pipeline
- `docs/protocol.md` — Current transport documentation
