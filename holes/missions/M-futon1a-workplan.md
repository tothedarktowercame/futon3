# futon1a Work Plan: Codex + Claude Collaboration

Dual goals:
1. Build a great futon1a (product)
2. Demonstrate futonic best practices (process)

## Base Camp Status

| Item | Status | Notes |
|------|--------|-------|
| Agency | ✅ Running | Both agents connected via MUSN |
| PSR/PUR plugins | ✅ Ready | Log to MUSN activity stream |
| Evidence doc | ✅ Done | M-futon1a-evidence.md |
| Pattern library | ✅ Done | futon-theory/ + storage/ |
| Labs directory | ✅ Done | holes/labs/futon1a/{psr,pur,sessions}/ |
| PSR/PUR exemplars | ✅ Done | See psr/pur directories |
| futon1a repo | ✅ Done | ~/code/futon1a/ (new repo) |

---

## Phase 0: Base Camp Setup

Before any code, complete these readiness checks:

### 0.1 Create Labs Directory Structure
**Owner**: Claude
```
holes/labs/futon1a/
├── psr/           # Pattern Selection Records
├── pur/           # Pattern Use Records
└── sessions/      # Session notes
```

### 0.2 Create PSR/PUR Exemplars
**Owner**: Claude
- One PSR demonstrating pattern selection for module design
- One PUR demonstrating outcome recording
- File naming: `YYYY-MM-DD__<module>__<pattern>.md`

### 0.3 Verify Agent PSR/PUR Emission
**Owner**: Both
- Claude: Emit test PSR via `/psr` command, verify in MUSN
- Codex: Emit test PSR via curl/API, verify in MUSN
- Confirm: Both agents can log to `holes/labs/futon1a/`

### 0.4 Designate futon1a Repo
**Owner**: Joe (decision)
**Status**: ✅ Done

Decision: New repo at `~/code/futon1a/`
- Initialized with directory structure matching work plan
- Initial commit: 501d0b6

### 0.5 Establish Ping-Pong Protocol
**Owner**: Both
- On milestone completion: commit + push + notify other agent
- Other agent: pull + review + respond in next commit
- Use MUSN/Agency for async coordination if needed

---

## Part I: Process (Gate Required Before Part II)

### Part I Checklist (from mission doc 1.6)

| Item | Owner | Status |
|------|-------|--------|
| Evidence doc complete | Codex | ✅ Done |
| Pattern library canonicalized | Claude + Codex | ✅ Done |
| Module map with repo layout | Codex | ✅ Done |
| Test harness placeholder | Claude | ✅ Done |
| PSR + PUR exemplar exists | Claude | ✅ Done |
| Traceability example | Both | ✅ Done |

### 1.1 Module Map with Repo Layout
**Owner**: Codex
**Review**: Claude

Create detailed module map based on 2.3.1 Tension → Module → Test Mapping:
```
futon1a/
├── src/
│   ├── core/
│   │   ├── xtdb.clj      # Layer 0: Durability
│   │   ├── identity.clj  # Layer 1: Identity
│   │   ├── entity.clj    # Layer 2: Integrity
│   │   ├── relation.clj  # Layer 2: Integrity
│   │   ├── rehydrate.clj # Layer 2: Integrity
│   │   ├── mirror.clj    # Tension: persistence-speed
│   │   └── invariants.clj # Cross-cutting
│   ├── auth/
│   │   └── penholder.clj # Layer 3: Authorization
│   ├── model/
│   │   └── registry.clj  # Layer 4: Validation
│   ├── ingest/
│   │   └── open_world.clj # Tension: velocity-validation
│   ├── api/
│   │   └── errors.clj    # Error hierarchy
│   ├── diag/
│   │   └── health.clj    # Rapid debugging
│   └── scripts/
│       └── repair/       # Tension: invariants-vs-repair
├── test/
│   ├── layer0/           # Durability tests
│   ├── layer1/           # Identity tests
│   ├── layer2/           # Integrity tests
│   ├── cross_layer/      # Integration tests
│   └── invariants/       # Proof tests
└── docs/
    ├── README.md         # Architecture (not just usage)
    ├── error-catalog.md  # Every error code
    └── invariant-catalog.md # Every invariant
```

### 1.2 Test Harness Placeholder
**Owner**: Claude
**Review**: Codex

Create placeholder test file for each layer:
- `test/layer0/durability_test.clj`
- `test/layer1/identity_test.clj`
- `test/layer2/integrity_test.clj`
- All green (placeholder assertions)

### 1.3 Traceability Example
**Owner**: Both

Document one complete trace:
```
devmap (Prototype 2: Invariant Enforcement)
  ↓ instantiates
futon-theory/durability-first
  ↓ constrains
storage/durability-first
  ↓ applied in
core/xtdb.clj (Layer 0)
  ↓ tested by
test/layer0/durability_test.clj
  ↓ documented in
docs/invariant-catalog.md#i0-persistence
  ↓ errors surfaced as
503 STORAGE_UNAVAILABLE
```

### Part I Gate Check
**Owner**: Both (consensus required)

All items checked → Part II can begin.

---

## Part II: Product (Ping-Pong by Layer)

Each layer follows the cycle:
1. Primary owner: Implement with PSR
2. Commit + push + notify
3. Reviewer: Pull + review + PUR
4. Iterate until milestone met

### Layer Implementation Order

| Phase | Layer | Primary | Reviewer | Milestone |
|-------|-------|---------|----------|-----------|
| 2.1 | Layer 0: Durability | Codex | Claude | Writes block until confirmed |
| 2.2 | Layer 1: Identity | Claude | Codex | Duplicates rejected pre-write |
| 2.3 | Layer 2: Integrity | Codex | Claude | Startup all-or-nothing |
| 2.4 | Layer 3: Authorization | Claude | Codex | Internal tools obey guards |
| 2.5 | Layer 4: Validation | Codex | Claude | Schema evolution works |
| 2.6 | Cross-layer tests | Both | Both | Error hierarchy verified |
| 2.7 | Migration rehearsal | Codex | Claude | Data survives round-trip |

### Per-Layer Deliverables

For each layer:
- [ ] Module code with pattern references in header
- [ ] PSR documenting pattern selection
- [ ] Layer tests (from 2.7.1 phases)
- [ ] PUR documenting outcome
- [ ] Error catalog entries
- [ ] Invariant catalog entries

### Test Rollout (from 2.7.1)

| Phase | Description | Owner |
|-------|-------------|-------|
| 0 | Harness: placeholders, all green | Claude |
| 1 | Layer tests: L0-L2 passing locally | Alternating |
| 2 | Cross-layer: error propagation | Both |
| 3 | End-to-end: API → storage → restart | Both |
| 4 | Migration rehearsal: checksums match | Codex |

---

## Communication Protocol

### Milestone Handoff
```
1. Complete work, emit PUR
2. Commit with message: "Milestone: <description> [handoff: <other-agent>]"
3. Push to origin
4. Notify via:
   - Agency message (if running)
   - Or: leave note in mission doc
```

### Review Response
```
1. Pull latest
2. Review changes
3. Emit PSR for review task
4. Document findings in commit or mission doc
5. Emit PUR for review outcome
6. If changes needed: describe, handoff back
7. If approved: note approval, proceed to next milestone
```

### Escalation
If blocked > 30 minutes:
- Document blocker in mission doc
- Notify Joe for decision
- Continue with other available work

---

## Success Metrics

### Process Metrics (Dual Goal: Best Practices)
- [ ] Every module has PSR + PUR pair
- [ ] Every layer has traceability chain
- [ ] No orphaned PSRs (all have matching PURs)
- [ ] Ping-pong handoffs documented

### Product Metrics (Dual Goal: Great futon1a)
- [ ] All 5 invariants pass proof tests
- [ ] Any bug diagnosable in < 10 minutes
- [ ] Migration succeeds without data loss
- [ ] 30 days production without silent failures

---

## Next Actions

1. **Claude**: Create labs directory + PSR/PUR exemplars (Phase 0.1, 0.2)
2. **Joe**: Decide on futon1a repo location (Phase 0.4)
3. **Codex**: Create module map with repo layout (Phase 1.1)
4. **Both**: Verify PSR/PUR emission works (Phase 0.3)
5. **Gate check**: Confirm Part I complete before Part II
