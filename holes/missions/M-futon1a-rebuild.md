# Mission: futon1a Rebuild

A ground-up reconstruction of futon1, built as a demonstration of futonic best
practices. The mission has two parts: the *process* (pattern-based design with
full documentation) and the *product* (a storage layer with unbreakable core
invariants).

## Owner

Codex (with Claude as primary reviewer for Part I process artifacts).

## Scope

### Scope In

- Part I: Evidence-backed design + pattern-to-module mapping + PSR/PUR exemplars + traceability gates.
- Part II: Specify and implement futon1a as a deterministic storage substrate with durable invariants and a canonical HTTP interface.
- Produce the minimum docs/tests needed to make invariants verifiable (error catalog, invariant catalog, core test harness).

### Scope Out

- Any UI beyond the canonical HTTP interface.
- Non-storage features (inference, agent orchestration) outside the "thin waist" substrate.
- Unlimited expansion: if success criteria exceed 5 in a child mission, split (per mission-scoping).

## Time Box

2 weeks to complete Part I gates and begin Part II with at least one invariant implemented end-to-end (code + test + doc).

## Exit Conditions

- Part I gate is satisfied and verified with concrete artifacts (evidence doc, module map, test harness, PSR/PUR exemplars, traceability example).
- Part II is either underway with a validated first invariant, or the mission is split into smaller layer missions with explicit owners.

## Argument (Pattern-Backed)

futon1a exists to provide a deterministic, storage‑oriented substrate that
maintains a coherent graph of facts across sessions, with a canonical HTTP
interface and a documented graph‑memory contract that downstream tools can
depend on. It keeps Datascript (fast) and XTDB (persistent) in reproducible
alignment so the same world state appears across restarts, and it provides a
deterministic ingest pipeline so graph construction is reproducible. It also
supports open‑world continuity by letting background knowledge accumulate in
the same persistent store outside interactive sessions.

This purpose must survive the core tensions observed in futon1’s history. If
you want persistence but also speed, futon1a mirrors XTDB into Datascript and
treats mirroring as an invariant. If you want throughput but also durable
truth, futon1a gates success responses on durable commit and treats watchdog
ordering as a first‑class invariant. If you want fast availability but also
integrity, futon1a gates startup on full rehydration and explicit diagnostics.
If you want flexible external identifiers but also unique identity, futon1a
enforces UUID identity and rejects duplicate external‑id mappings before write.
If you want rapid open‑world ingest but also coherent state, futon1a validates
aggressively and provides repair tooling. If you want internal tooling but also
strong guardrails, futon1a forces internal writers through the same
authorization and invariant checks. If you want strict invariants but also
operational resilience, futon1a pairs enforcement with explicit
repair/backfill pathways. If you want schema evolution but also stable
contracts, futon1a formalizes migration rules and versioned descriptors. If you
want feature expansion but also determinism, futon1a isolates new features
behind determinism gates and regression tests.

Pointers to the patterns that specify these claims:

- Purpose patterns: `library/storage/deterministic-substrate.flexiarg`,
  `library/storage/canonical-interface.flexiarg`,
  `library/storage/reproducible-mirroring.flexiarg`,
  `library/storage/graph-memory-contract.flexiarg`,
  `library/storage/deterministic-ingest-pipeline.flexiarg`,
  `library/storage/open-world-continuity.flexiarg`
- Enforcement patterns: `library/storage/durability-first.flexiarg`,
  `library/storage/identity-uniqueness.flexiarg`,
  `library/storage/all-or-nothing-startup.flexiarg`,
  `library/storage/error-layer-hierarchy.flexiarg`,
  `library/storage/rapid-debugging.flexiarg`
- Tension patterns: `library/storage/persistence-speed-mirroring.flexiarg`,
  `library/storage/durability-throughput-gate.flexiarg`,
  `library/storage/startup-integrity-gate.flexiarg`,
  `library/storage/identity-flex-uniqueness.flexiarg`,
  `library/storage/open-world-velocity-validation.flexiarg`,
  `library/storage/guardrails-vs-tooling.flexiarg`,
  `library/storage/invariants-vs-repair.flexiarg`,
  `library/storage/schema-evolution-stability.flexiarg`,
  `library/storage/determinism-vs-expansion.flexiarg`

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

**Traceability example (required in session notes):**

```
devmap → pattern → module → test → doc → error
futon1.devmap:P2 → storage/durability-first → core/xtdb.clj →
test/invariants/durability_test.clj → README.md#layer-0-durability →
503 STORAGE_UNAVAILABLE
```

### 1.3.1 Start Checklist (Before Any Code)

Minimum artifacts required to begin implementation:

1. Evidence document exists and is reviewed: `holes/missions/M-futon1a-evidence.md`.
2. Pattern set is canonical and indexed: `library/storage/` + `library/futon-theory/`.
3. Initial module map exists (see 1.2) and includes a target repo layout.
4. A minimal test harness exists (even a placeholder test suite).
5. At least one PSR and one PUR exemplar are created (see 1.3.2).

### 1.3.2 PSR/PUR Storage and Naming

Operational conventions for session records:

1. Location: `holes/labs/futon1a/psr/` and `holes/labs/futon1a/pur/`.
2. Filename convention: `YYYY-MM-DD__<module>__<pattern>.md`.
3. Required fields:
   - context
   - pattern
   - decision
   - alternatives
   - outcome
   - evidence link (e.g., commit or test)

Example PSR filename: `2026-02-07__core-xtdb__storage-durability-first.md`.

Current exemplars (2026-02-07):
- PSR: `holes/labs/futon1a/psr/2026-02-07__layer0-xtdb__durability-gate.md`
- PSR: `holes/labs/futon1a/psr/2026-02-07__layer1-identity__uuid-uniqueness.md`
- PSR: `holes/labs/futon1a/psr/2026-02-07__layer2-integrity__rehydrate-entity.md`
- PSR: `holes/labs/futon1a/psr/2026-02-07__layer3-4__auth-validation.md`
- PSR: `holes/labs/futon1a/psr/2026-02-07__pipeline-api__write-surface.md`
- PSR: `holes/labs/futon1a/psr/2026-02-07__invariants__counter-ratchet.md`
- PUR: `holes/labs/futon1a/pur/2026-02-07__layer0-xtdb__durability-gate.md`
- PUR: `holes/labs/futon1a/pur/2026-02-07__layer1-identity__uuid-uniqueness.md`
- PUR: `holes/labs/futon1a/pur/2026-02-07__layer2-integrity__rehydrate-entity.md`
- PUR: `holes/labs/futon1a/pur/2026-02-07__layer3-4__auth-validation.md`
- PUR: `holes/labs/futon1a/pur/2026-02-07__pipeline-api__write-surface.md`
- PUR: `holes/labs/futon1a/pur/2026-02-07__invariants__counter-ratchet.md`

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

### 1.6 Part I Gate (Process Acceptance)

Do not begin Part II until all items are satisfied:

1. Evidence doc complete and reviewed.
2. Pattern library canonicalized and indexed.
3. Module map with target repo layout exists.
4. At least one PSR + PUR exemplar exists.
5. Traceability example present (devmap → pattern → code → test → doc → error).

---

## Part II: futon1a Technical Specification

The actual storage layer, built around core invariants. Part II is constrained
by `futon-theory/` (the exotype) and instantiates `storage/` patterns (the
genotype) as working code (the phenotype).

### 2.1 Theory Grounding

futon1a implements the futon-theory axioms and event protocol:

| Axiom | Requirement | futon1a Implementation |
|-------|-------------|------------------------|
| A1: Auditability | Every operation traceable | Proof-path logging on all writes |
| A2: Attributable | Changes have clear source | Session/penholder on every tx |
| A3: Evidence-driven | Progression requires proof | Invariant check before commit |
| A4: Degradation detectable | Failures surface early | Counter-ratchet on key counts |
| A5: Minimum events | Log what's needed | Structured events, not verbose logs |

Every write operation follows the **proof-path** event protocol:

```
CLOCK_IN (session start)
  → OBSERVE (read current state)
  → PROPOSE_CLAIM (declare intended change)
  → APPLY_CHANGE (execute)
  → VERIFY (confirm matches claim)
  → INVARIANT_CHECK (all invariants pass)
  → PROOF_COMMIT (durable with tx-id)
  → CLOCK_OUT (session end)
```

Theory patterns: `futon-theory/proof-path`, `futon-theory/event-protocol`,
`futon-theory/agent-contract`

### 2.2 Core Invariants

These are non-negotiable. If any fails, the system is broken. Each invariant
has a theory pattern (abstract) and a storage pattern (concrete).

#### Invariant 0: Persistence

> **What you save is what you get back.**

- Writes return success ONLY after XTDB confirms durability
- Reads return ONLY data that is durably stored
- No fire-and-forget, no async-without-callback
- All failures throw, no silent logging

Theory: `futon-theory/durability-first`
Storage: `storage/durability-first`, `storage/durability-throughput-gate`
Evidence: commits 0e2b3a5, 5c51506, 5227d75

#### Invariant 1: Identity

> **One entity per identity, no ambiguity.**

- Every entity has exactly one UUID
- (source, external-id) maps to exactly one UUID
- Duplicate external IDs rejected BEFORE write
- Lookups with ambiguous matches throw, not silently pick one

Theory: `futon-theory/single-source-of-truth`
Storage: `storage/identity-uniqueness`, `storage/identity-flex-uniqueness`
Evidence: commits d1d447d, 1bda8f7

#### Invariant 2: Integrity

> **Startup succeeds completely or fails loudly.**

- Rehydration loads all entities or throws
- No silent stubs, no partial loads
- Failure reports exactly what failed and why
- Relations verified to have valid endpoints
- Key counts must not drop (counter-ratchet)

Theory: `futon-theory/all-or-nothing`, `futon-theory/counter-ratchet`
Storage: `storage/all-or-nothing-startup`, `storage/startup-integrity-gate`
Evidence: commits 884df26, c5ffd75

#### Invariant 3: Hierarchy

> **Errors surface at the layer that caused them.**

- Layer 0 (durability) errors → 503
- Layer 1 (identity) errors → 409
- Layer 2 (integrity) errors → 500
- Layer 3 (auth) errors → 403
- Layer 4 (validation) errors → 400

No penholder errors when the real problem is durability.

Theory: `futon-theory/error-hierarchy`, `futon-theory/stop-the-line`
Storage: `storage/error-layer-hierarchy`, `storage/guardrails-vs-tooling`
Evidence: commits 7efbef7, e1e2c9d, a525acb

#### Invariant 4: Rapid Debugging

> **Any bug diagnosable in under 10 minutes.**

- Errors name their source: layer, operation, entity ID, expected vs actual
- One write path to trace, not six
- State inspectable at any moment
- Logs tell a story: "write started → confirmed → done" or "started → failed at X"

Theory: `futon-theory/rapid-debugging`
Storage: `storage/rapid-debugging`
Evidence: commits 884df26, 50383dd

### 2.3 Tension Resolutions

These design decisions resolve the tensions observed in futon1 history:

| Tension | Resolution | Pattern |
|---------|------------|---------|
| Persistence vs speed | Mirror XTDB→Datascript, mirroring is invariant | `storage/persistence-speed-mirroring` |
| Throughput vs durability | Gate success on durable commit | `storage/durability-throughput-gate` |
| Availability vs integrity | Gate startup on full rehydration | `storage/startup-integrity-gate` |
| Flexible ID vs uniqueness | Enforce UUID, reject duplicate external-id | `storage/identity-flex-uniqueness` |
| Ingest velocity vs validation | Validate aggressively + repair tooling | `storage/open-world-velocity-validation` |
| Guardrails vs internal tools | Internal writers obey same auth | `storage/guardrails-vs-tooling` |
| Invariants vs repair | Pair enforcement with explicit repair paths | `storage/invariants-vs-repair` |
| Schema evolution vs stability | Formalize migrations, version descriptors | `storage/schema-evolution-stability` |
| Determinism vs expansion | Isolate new features behind gates | `storage/determinism-vs-expansion` |

#### 2.3.1 Tension → Module → Test Mapping (Required)

Each tension must map to at least one module and one test:

| Tension | Module(s) | Test(s) |
|---------|-----------|---------|
| Persistence vs speed | `core/xtdb.clj`, `core/mirror.clj` | `test/mirror_equivalence_test.clj` |
| Throughput vs durability | `core/xtdb.clj` | `test/durability_gate_test.clj` |
| Availability vs integrity | `core/rehydrate.clj` | `test/startup_gate_test.clj` |
| Flexible ID vs uniqueness | `core/identity.clj` | `test/identity_uniqueness_test.clj` |
| Ingest velocity vs validation | `ingest/open_world.clj` | `test/open_world_validation_test.clj` |
| Guardrails vs internal tools | `auth/penholder.clj`, `scripts/*` | `test/internal_guardrails_test.clj` |
| Invariants vs repair | `core/invariants.clj`, `scripts/repair/*` | `test/repair_compliance_test.clj` |
| Schema evolution vs stability | `model/registry.clj` | `test/schema_migration_test.clj` |
| Determinism vs expansion | `pipeline/*` | `test/determinism_gate_test.clj` |

### 2.4 Layered Architecture

```
Layer 4: Model Validation (model/*.clj)           ─── 400 Bad Request
    ↑ depends on
Layer 3: Authorization (auth/penholder.clj)       ─── 403 Forbidden
    ↑ depends on
Layer 2: Integrity (entity.clj, relation.clj, rehydrate.clj) ─── 500 Internal
    ↑ depends on
Layer 1: Identity (core/identity.clj)             ─── 409 Conflict
    ↑ depends on
Layer 0: Durability (core/xtdb.clj)               ─── 503 Unavailable
    ↑ depends on nothing
```

Each layer is a gate. If Layer 0 fails, you get a Layer 0 error. Layer 3 never
runs if Layer 1 failed.

### 2.5 Interface Loops (Evolutionary Mechanism)

Each layer boundary hosts an **interface loop** for adaptation and improvement:

```
Interface I01 (L0↔L1): Durability→Identity
  - Exotype: tx-id required before identity assignment
  - Baldwin cycle: explore identity schemes → assimilate working patterns → canalize

Interface I12 (L1↔L2): Identity→Integrity
  - Exotype: UUID required before entity/relation creation
  - Baldwin cycle: explore relation validation → assimilate → canalize

Interface I23 (L2↔L3): Integrity→Authorization
  - Exotype: Valid entity required before auth check
  - Baldwin cycle: explore penholder schemes → assimilate → canalize
```

Theory patterns: `futon-theory/interface-loop`, `futon-theory/baldwin-cycle`,
`futon-theory/local-gain-persistence`

**Invariant**: Any improvement at an interface must be captured in code/patterns
or explicitly removed. No ghost capabilities.

### 2.6 What's Different from futon1

| Aspect | futon1 | futon1a |
|--------|--------|---------|
| Durability | Verified, warnings possible | Hard failure only |
| External ID | Conflicts error | + ambiguity detection on lookup |
| Rehydration | Startup check, warnings | All-or-nothing, blocks on failure |
| Errors | Improved, some scatter | Strict layer hierarchy |
| Datascript | Primary with XTDB mirror | XTDB-only (cache later if needed) |
| Write paths | Multiple exist | Single chokepoint |
| Debugging | Hours | Minutes |

### 2.7 Testing Strategy

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

#### 2.7.1 Test Rollout Phases (Operational)

1. Phase 0 (Harness): one placeholder test file per layer, all green.
2. Phase 1 (Layer tests): Layer 0–2 tests passing locally.
3. Phase 2 (Cross-layer tests): error propagation tests across layers.
4. Phase 3 (End-to-end tests): API → durable storage → restart.
5. Phase 4 (Migration rehearsal): data export/import with checksums.

### 2.8 Synthetic Data Mocks

Build with realistic data shapes from the start:

- Large lab session histories (thousands of events)
- Complex hypergraph structures (many-to-many relations)
- High-volume pattern records
- Media entities with lyrics, metadata, relations

This validates the schema handles real workloads, not just toy examples.

### 2.9 Migration Path

1. **Build futon1a** with passing invariant tests
2. **Run both** in parallel (different ports)
3. **Bootstrap**: Use futon1 to store Labs records during futon1a build
4. **Migrate data** via XTDB export/import
5. **Switch clients** one by one
6. **Retire futon1** when all clients migrated

futon1a's first dataset is its own creation story.

#### 2.9.1 Migration Validation Checklist

1. Snapshot + XTDB export checksum matches after import.
2. Invariant proofs pass on futon1a after import.
3. API compatibility smoke tests pass (same inputs → same outputs).
4. Restart cycle preserves identical world state.
5. Rollback plan documented (switch clients back to futon1).

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
- [ ] Each invariant traces to both futon-theory/ and storage/ patterns
- [ ] All 9 tension resolutions implemented and tested
- [ ] Interface loops defined at each layer boundary with PSR/PUR governance
- [ ] Proof-path event logging on all write operations
- [ ] Counter-ratchet detects unexpected count drops
- [ ] Any bug diagnosable in under 10 minutes
- [ ] Synthetic data mocks exercise realistic workloads
- [ ] Migration from futon1 succeeds without data loss
- [ ] futon1a runs in production for 30 days without silent failures

### Derivation Requirements
Every new domain pattern must demonstrate:
- [ ] Cite at least one futon-theory axiom (A1-A5) or invariant (I0-I4)
- [ ] Reference a tension resolution from git evidence
- [ ] Trace: theory pattern → domain pattern → code module → test
