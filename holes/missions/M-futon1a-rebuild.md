# Mission: futon1a Rebuild

A ground-up reconstruction of futon1, built as a demonstration of futonic best
practices. The mission has two parts: the *process* (pattern-based design with
full documentation) and the *product* (a storage layer with unbreakable core
invariants).

## Part I: Pattern-Based Design Process

futon1a will be built as a **demonstration** of the futon methodology, not just
as working code. Every design decision links to patterns, every coding session
produces Labs records, and documentation is first-class.

### 1.1 Evidence-Backed Design

Before writing code, mine futon1's git history for lessons:

- **Codex task**: Analyze all futon1 commits to extract:
  - Recurring bug patterns (what kept breaking?)
  - Fix patterns (what solutions worked?)
  - Implicit invariants (what assumptions were violated?)
  - Anti-patterns (what design choices caused problems?)

- **Output**: A pattern evidence document mapping:
  ```
  Pattern: <pattern-id>
  Evidence: commits <hash>, <hash>, ...
  Lesson: <what the history teaches>
  futon1a implication: <how this shapes the design>
  ```

### 1.2 Pattern → Code Mapping

Each module in futon1a maps to patterns from the library:

| Module | Pattern(s) | Rationale |
|--------|-----------|-----------|
| `core/xtdb.clj` | `stack-coherence/durability-first` | Persistence is the core invariant |
| `core/identity.clj` | `code-coherence/single-source-of-truth` | One ID scheme, no ambiguity |
| `core/entity.clj` | `code-coherence/fail-loud` | No silent errors |
| `core/rehydrate.clj` | `stack-coherence/all-or-nothing` | Startup succeeds or fails, no partial |
| `api/errors.clj` | `code-coherence/error-hierarchy` | Errors surface at correct layer |
| `diag/health.clj` | `stack-coherence/rapid-debugging` | Problems diagnosable in minutes |

(Patterns may not exist yet - we use the *concepts* and formalize patterns later
if valuable.)

### 1.3 Labs Integration

Every coding session on futon1a produces:

- **PSR** (Pattern Selection Record): Why this pattern for this problem?
- **PUR** (Pattern Use Record): What happened when applied?
- **Session notes**: Decisions made, alternatives considered

Labs records are stored in futon1 during the build (bootstrapping), then
migrated to futon1a when ready.

### 1.4 Documentation as First-Class

Not comments explaining code, but docs explaining *design*:

- **Module headers**: What invariant? What pattern? Why this approach?
- **README**: Teaches the architecture, not just how to run it
- **Error catalog**: Every error code, what layer, what it means, how to fix
- **Invariant catalog**: Every invariant, how it's enforced, how to test it

The goal: someone reading futon1a understands *why* it's shaped this way, not
just *what* it does.

### 1.5 Links Between Artifacts

Traceability from requirement to implementation:

```
futon1.devmap (Prototype 2: Invariant Enforcement)
  ↓ instantiates
Pattern: durability-first
  ↓ applied in
Module: core/xtdb.clj (Layer 0)
  ↓ tested by
Test: test/invariants/durability_test.clj
  ↓ documented in
Doc: README.md#layer-0-durability
  ↓ errors surfaced as
Error: 503 STORAGE_UNAVAILABLE
```

---

## Part II: futon1a Technical Specification

The actual storage layer, built around core invariants.

### 2.1 Core Invariants

These are non-negotiable. If any fails, the system is broken.

#### Invariant 0: Persistence

> **What you save is what you get back.**

- Writes return success ONLY after XTDB confirms durability
- Reads return ONLY data that is durably stored
- No fire-and-forget, no async-without-callback
- All failures throw, no silent logging

#### Invariant 1: Identity

> **One entity per identity, no ambiguity.**

- Every entity has exactly one UUID
- (source, external-id) maps to exactly one UUID
- Duplicate external IDs rejected BEFORE write
- Lookups with ambiguous matches throw, not silently pick one

#### Invariant 2: Integrity

> **Startup succeeds completely or fails loudly.**

- Rehydration loads all entities or throws
- No silent stubs, no partial loads
- Failure reports exactly what failed and why
- Relations verified to have valid endpoints

#### Invariant 3: Hierarchy

> **Errors surface at the layer that caused them.**

- Layer 0 (durability) errors → 503
- Layer 1 (identity) errors → 409
- Layer 2 (integrity) errors → 500
- Layer 3 (auth) errors → 403
- Layer 4 (validation) errors → 400

No penholder errors when the real problem is durability.

#### Invariant 4: Rapid Debugging

> **Any bug diagnosable in under 10 minutes.**

- Errors name their source: layer, operation, entity ID, expected vs actual
- One write path to trace, not six
- State inspectable at any moment
- Logs tell a story: "write started → confirmed → done" or "started → failed at X"

### 2.2 Layered Architecture

```
Layer 0: Durability (core/xtdb.clj)
   ↓ depends on nothing
Layer 1: Identity (core/identity.clj)
   ↓ depends on Layer 0
Layer 2: Integrity (core/entity.clj, core/relation.clj, core/rehydrate.clj)
   ↓ depends on Layers 0, 1
Layer 3: Authorization (auth/penholder.clj)
   ↓ depends on Layers 0, 1, 2
Layer 4: Model Validation (model/*.clj)
   ↓ depends on Layers 0, 1, 2, 3
```

Each layer is a gate. If Layer 0 fails, you get a Layer 0 error. Layer 3 never
runs if Layer 1 failed.

### 2.3 What's Different from futon1

| Aspect | futon1 | futon1a |
|--------|--------|---------|
| Durability | Verified, warnings possible | Hard failure only |
| External ID | Conflicts error | + ambiguity detection on lookup |
| Rehydration | Startup check, warnings | All-or-nothing, blocks on failure |
| Errors | Improved, some scatter | Strict layer hierarchy |
| Datascript | Primary with XTDB mirror | XTDB-only (cache later if needed) |
| Write paths | Multiple exist | Single chokepoint |
| Debugging | Hours | Minutes |

### 2.4 Testing Strategy

**Layer tests** (fast, isolated):
- Does Layer 0 block until XTDB confirms?
- Does Layer 1 reject duplicate external-id?
- Does Layer 2 fail on corrupt entity?

**Cross-layer tests** (integration):
- Duplicate → identity error surfaces, auth never runs
- XTDB down → durability error, nothing else attempted
- Restart → data survives and loads correctly

**End-to-end tests** (API to durable storage):
- POST → GET → restart → GET still works
- POST duplicate → 409, first entity unchanged
- Corrupt XTDB data → startup fails with clear error

**Invariant proofs**:
- Tests that *prove* each invariant holds, not just exercise code paths

### 2.5 Synthetic Data Mocks

Build with realistic data shapes from the start:

- Large lab session histories (thousands of events)
- Complex hypergraph structures (many-to-many relations)
- High-volume pattern records
- Media entities with lyrics, metadata, relations

This validates the schema handles real workloads, not just toy examples.

### 2.6 Migration Path

1. **Build futon1a** with passing invariant tests
2. **Run both** in parallel (different ports)
3. **Bootstrap**: Use futon1 to store Labs records during futon1a build
4. **Migrate data** via XTDB export/import
5. **Switch clients** one by one
6. **Retire futon1** when all clients migrated

futon1a's first dataset is its own creation story.

---

## Open Questions

1. **Datascript**: Start XTDB-only, add read cache later if needed?
2. **Event log**: Rely on XTDB bitemporal history, or keep separate log?
3. **Profiles**: Simplify to single store first, add profiles later?
4. **API compatibility**: Same REST shapes with stricter error codes?

---

## Success Criteria

### Part I (Process)
- [ ] Evidence document from futon1 git history
- [ ] PSR/PUR records for each coding session
- [ ] Module headers explain pattern and rationale
- [ ] README teaches architecture, not just usage
- [ ] Traceability from devmap → pattern → code → test → doc

### Part II (Product)
- [ ] All 5 core invariants pass their proof tests
- [ ] Any bug diagnosable in under 10 minutes
- [ ] Synthetic data mocks exercise realistic workloads
- [ ] Migration from futon1 succeeds without data loss
- [ ] futon1a runs in production for 30 days without silent failures
