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

### 2.6 Canonical HTTP API

The API is the single write chokepoint: every mutation (entity creation, ingest,
repair) passes through one pipeline that enforces all 5 layer gates. This
section specifies the contract.

Pattern: `storage/canonical-interface`
Theory: `futon-theory/error-hierarchy`, `futon-theory/proof-path`

#### 2.6.1 API Principles

1. **Single chokepoint.** All writes go through `pipeline/run-write!` or
   `pipeline/run-open-world!`. No backdoor paths.
2. **Layer gate ordering.** Every request traverses L4→L3→L2→L1→L0.
   The first gate that fails determines the error. Higher layers never run.
3. **Error shape is non-negotiable.** Every error response has the shape:
   ```json
   {"error": {"layer": N, "reason": "keyword", "context": {...}}}
   ```
   HTTP status is determined by the layer, not chosen ad hoc (see I3).
4. **Proof-path on every write.** Successful writes return a `path/id` that
   traces the full proof-path event sequence (A1: Auditability).
5. **Penholder in body.** For Prototype 1, the caller passes `penholder` in
   the request body. Authentication middleware is deferred.

#### 2.6.2 Endpoints

| Method | Path | Purpose | Request Shape | Success Response | Error Codes |
|--------|------|---------|---------------|------------------|-------------|
| GET | `/health` | System diagnostics | (none) | `200 {:status :ok, :checks {...}}` | `503` (degraded) |
| POST | `/write` | Write with layer gating | See 2.6.3 | `200 {:tx-id T, :path/id P}` | 400, 403, 409, 500, 503 |
| POST | `/ingest` | Open-world entity/relation ingest | See 2.6.4 | `200 {:counts {:entities N, :relations N}}` | 400, 403, 500, 503 |
| POST | `/models` | Register model descriptor | `{:id K, :descriptor M}` | `200 {:id K, ...}` | 400 |
| GET | `/models` | List registered models | (none) | `200 {:models [...]}` | — |
| POST | `/repair` | Repair entities | `{:entities [...], :dry-run? bool}` | `200 {:repaired N, :skipped N}` | 400, 500 |
| POST | `/repair/verify` | Verify repair outcomes | `{:prev M, :next M, :label S}` | `200 {:ok? bool}` | 400, 500 |

#### 2.6.3 Write Request Shape

```clojure
{:store          <Store>              ; injected by system (not caller-supplied in HTTP)
 :penholder      "agent-name"         ; who is writing (L3 gate)
 :allowed-penholders #{"agent-name"}  ; injected by system config
 :model          :model-id            ; model descriptor for validation (L4 gate)
 :required-keys  #{:entity/type}      ; field requirements from model
 :identity       {:source "s"         ; external identity for dedup (L1 gate)
                  :external-id "eid"}
 :tx-ops         [[:xtdb.api/put {...}]] ; XTDB transaction operations
 }
```

**Notes:**
- `:store` and `:allowed-penholders` are system-injected. HTTP callers provide
  only `:penholder`, `:model`, `:identity`, and `:tx-ops`.
- When served over HTTP, `routes.clj` must hydrate the system-injected fields
  from the running system context before passing to `pipeline/run-write!`.

#### 2.6.4 Ingest Request Shape

```clojure
{:store              <Store>
 :penholder          "agent-name"
 :allowed-penholders #{"agent-name"}
 :entities           [{:entity/type "T" :entity/id "..." ...}]
 :relations          [{:relation/type "R" :from "..." :to "..." ...}]
 :tx-ops             [...]               ; generated from entities/relations
 :require-model?     false               ; optional: skip model validation
 }
```

#### 2.6.5 Error Response Shape (Non-Negotiable)

Every error response:

```json
{
  "status": <http-code>,
  "body": {
    "error": {
      "layer": <0-4>,
      "reason": "<keyword>",
      "context": { ... }
    }
  }
}
```

HTTP status mapping (from I3: Hierarchy):

| Layer | Gate | HTTP Status | Meaning |
|-------|------|-------------|---------|
| 4 | Validation | 400 | Bad request (schema, missing fields) |
| 3 | Authorization | 403 | Penholder not allowed |
| 2 | Integrity | 500 | Entity/relation structural error |
| 1 | Identity | 409 | Duplicate external-id conflict |
| 0 | Durability | 503 | XTDB unavailable or tx failed |

Unexpected exceptions (non-ExceptionInfo) return `500 {:reason :exception, :message "..."}`.

#### 2.6.6 Read Path (Prototype 1 Requirement)

Prototype 1 requires a minimal read surface for restart-cycle verification.
The current API read surface is incomplete. Prototype 1 must include at minimum:

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/entity/:id` | Read entity by UUID |
| GET | `/entity?source=S&external-id=E` | Lookup by external identity |

The read path is required for the restart cycle test (write → restart → read
back). It must return entities as written, proving I0 (Persistence) end-to-end.

#### 2.6.7 System Context Injection (Prototype 1 Requirement)

When routes are served over HTTP, the system must inject:
- `:store` — the live XTDB store handle
- `:allowed-penholders` — from system configuration
- Health checks bound to real XTDB node status

This is the job of `system.clj` (see Prototype 1 Gate).

#### 2.6.8 Futon1 Compatibility Surface (Required by futon3)

futon3 currently depends on a small Futon1-shaped JSON surface for bridging
writes (workday/check artifacts, and lab session persistence). This compatibility
surface is real product behavior and must be specified (not left as ungrounded
code).

Endpoints (JSON):
- `GET /healthz` → health probe alias (parity with existing callers)
- `POST /entity` and `POST /api/alpha/entity` → Futon1-shaped entity upsert (compat write)
  required keys: `name`, `type`; optional: `external-id`, `notes`, `payload`
- `POST /relation` and `POST /api/alpha/relation` → Futon1-shaped relation upsert (compat write)
  required keys: `type`, `src`, `dst`; optional: `external-id`, `notes`, `payload`
- `POST /api/alpha/lab/session` → lab session checkpoint save (compat write)
  required keys: `lab/session-id` or `session/id` (plus lab payload)
- `GET /api/alpha/lab/session/:id` → lab session fetch by id (compat read)

Stop-the-line gap:
- `GET /entity?source=S&external-id=E` is specified in 2.6.6 but not yet implemented.

Invariant note (review item):
- The compat endpoints must conform to I3 (strict layer hierarchy) error shapes.
  In particular, they must not introduce ad-hoc exception-to-response mappings.

### 2.7 What's Different from futon1

| Aspect | futon1 | futon1a |
|--------|--------|---------|
| Durability | Verified, warnings possible | Hard failure only |
| External ID | Conflicts error | + ambiguity detection on lookup |
| Rehydration | Startup check, warnings | All-or-nothing, blocks on failure |
| Errors | Improved, some scatter | Strict layer hierarchy |
| Datascript | Primary with XTDB mirror | XTDB-only (cache later if needed) |
| Write paths | Multiple exist | Single chokepoint |
| Debugging | Hours | Minutes |

### 2.8 Testing Strategy

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

#### 2.8.1 Test Rollout Phases (Operational)

1. Phase 0 (Harness): one placeholder test file per layer, all green.
2. Phase 1 (Layer tests): Layer 0–2 tests passing locally.
3. Phase 2 (Cross-layer tests): error propagation tests across layers.
4. Phase 3 (End-to-end tests): API → durable storage → restart.
5. Phase 4 (Migration rehearsal): data export/import with checksums.

### 2.9 Synthetic Data Mocks

Build with realistic data shapes from the start:

- Large lab session histories (thousands of events)
- Complex hypergraph structures (many-to-many relations)
- High-volume pattern records
- Media entities with lyrics, metadata, relations

This validates the schema handles real workloads, not just toy examples.

### 2.10 Migration Path

1. **Build futon1a** with passing invariant tests
2. **Run both** in parallel (different ports)
3. **Bootstrap**: Use futon1 to store Labs records during futon1a build
4. **Migrate data** via XTDB export/import
5. **Switch clients** one by one
6. **Retire futon1** when all clients migrated

futon1a's first dataset is its own creation story.

#### 2.10.1 Migration Validation Checklist

1. Snapshot + XTDB export checksum matches after import.
2. Invariant proofs pass on futon1a after import.
3. API compatibility smoke tests pass (same inputs → same outputs).
4. Restart cycle preserves identical world state.
5. Rollback plan documented (switch clients back to futon1).

#### Checkpoint 2026-02-08: Storage Migration Complete (API Gap Remains)

**Completed:**
- Futon1 (LMDB XTDB) storage previously used by `futon3` dev was retired:
  moved from `~/code/futon3/data/default` to
  `~/code/storage/futon1-retired/futon3-data-default-20260208T120722Z`.
- Futon1a storage now lives at `~/code/storage/futon1a/default` (RocksDB XTDB).
- Logical export/import migration completed with stable checksum match:
  `17564` docs copied from the retired LMDB store into the futon1a store.

**Remaining gap (explicit):**
- The Futon1 HTTP API was *not* ported as part of the storage rebuild.
  Futon1a currently provides only a minimal compatibility surface sufficient for
  futon3 bridging writes (e.g. entity/relation writes and lab session save/load),
  plus Prototype 1 endpoints. Full `/api/alpha/*` parity remains Phase II work.

### 2.11 Model Descriptor System (Self-Describing Layer)

futon1 had a rich model descriptor system that made the system self-describing:
model descriptors defined entity shapes, the type registry tracked all types in
use, and `/api/α/meta/model/*` endpoints served the descriptors back. This was
not a convenience — it was the mechanism by which the system knew what it
contained and could verify its own integrity.

futon1a must rebuild this capability. The current model registry
(`{:fields [...] :required #{...}}`) is a stub that cannot represent the actual
complexity of futon1's descriptors.

Pattern: `storage/schema-evolution-stability`
Theory: `futon-theory/all-or-nothing`, `futon-theory/counter-ratchet`

#### 2.11.1 Model Descriptor Format

Each model descriptor is a self-contained specification:

```clojure
{:model/scope       :media               ; domain identifier (keyword)
 :schema/version    "0.1.2"              ; semver
 :schema/certificate {:penholder "..."   ; who certified this version
                      :issued-at ...}
 :entities
 {:arxana/media-track
  {:required [:entity/name :entity/external-id]
   :id-strategy :custom}                 ; or :uuid
  :arxana/media-lyrics
  {:required [:entity/name :entity/external-id :entity/source :media/sha256]
   :id-strategy :custom}}
 :operations
 {:media/ingest {:inputs [:filesystem :editor]
                 :outputs [:xtdb]}}
 :stores
 {:canonical :xtdb
  :cache :datascript}
 :invariants
 [:media/track-required
  :media/source-content
  :media/lyrics-required
  :media/lyrics-linked]}
```

**Key fields:**
- `:model/scope` — domain namespace (patterns, media, docbook, open-world, etc.)
- `:schema/version` — semver; migrations reference from/to versions
- `:schema/certificate` — who certified this descriptor and when (A2: Attributable)
- `:entities` — map of entity-type keyword → `{:required [...] :id-strategy ...}`
- `:operations` — what I/O this model supports
- `:invariants` — named constraints that `/verify` checks against live data

#### 2.11.2 futon1's Descriptors (Reference)

These are the descriptors futon1 shipped. futon1a must be able to represent and
ingest all of them:

| Scope | Entity Types | Invariants | Source |
|-------|-------------|------------|--------|
| `:patterns` | pattern/language, pattern/library, pattern/component, sigil, + 3 more | 6 (language-has-source, sigils-allowlisted, etc.) | `app/model.clj` |
| `:media` | arxana/media-track, arxana/media-lyrics | 6 (track-required, lyrics-linked, etc.) | `app/model_media.clj` |
| `:docbook` | docbook/heading, docbook/entry, docbook/toc | 8 (heading-required, toc-covers-headings, etc.) | `app/model_docbook.clj` |
| `:open-world-ingest` | open-world/entity, relation, mention, utterance, type | 3+ (entity-required, kind-valid, etc.) | `app/model_open_world.clj` |
| `:meta-model` | model/descriptor | 5 (descriptor-source-present, has-certificate, etc.) | `app/model_meta.clj` |
| `:penholder-registry` | model/penholder | 2 (entry-required, entry-schema) | `app/model_penholder.clj` |

#### 2.11.3 Type Registry

The type registry is a persistent (XTDB-stored) catalog of all entity, relation,
and intent types in use. It supports:

- **Auto-registration** — new types recorded on first encounter
- **Parent inference** — `project/chapter` infers parent `:project`
- **Aliases** — multiple names for the same canonical type
- **Three kinds** — `:entity`, `:relation`, `:intent`

Type documents:
```clojure
{:type/id       :person
 :type/kind     :entity
 :type/parent   nil          ; or inferred from namespace
 :type/aliases  [:human]}
```

#### 2.11.4 Meta/Model API Surface

Endpoints that make the system self-describing:

| Method | Path | Purpose |
|--------|------|---------|
| GET | `/meta/model` | All registered model descriptors |
| GET | `/meta/model/:scope` | Single descriptor by scope |
| GET | `/meta/model/:scope/verify` | Run invariants against live data |
| GET | `/types` | All registered types with parents and aliases |
| POST | `/types/parent` | Override a type's parent |
| POST | `/types/merge` | Merge type aliases |

The `/verify` endpoint is critical: it proves the live data matches the
descriptor's invariants. This is the runtime counterpart of the test suite —
the system checking itself.

#### 2.11.5 First Ingest: futon1's Descriptors as Graph Data

The model descriptors themselves are structured graph data:

- Each **descriptor** is an entity (`{:entity/type :model/descriptor, ...}`)
- Each **entity-type definition** within a descriptor is an entity
  (`{:entity/type :model/entity-type, :model/scope :media, :type-id :arxana/media-track, ...}`)
- Each **invariant** is a relation between a descriptor and a constraint
- The **meta-model** describes the other models (self-reference)

Ingesting futon1's 6 descriptors through `run-open-world!` is the first real
test of the system's ability to manage structured, cross-referenced data. This
replaces the "synthetic data mocks" success criterion with something concrete:
the system's own specification is its first dataset.

---

## Open Questions

1. **Datascript**: Start XTDB-only, add read cache later if needed?
2. **Event log**: Rely on XTDB bitemporal history, or keep separate log?
3. **Profiles**: Simplify to single store first, add profiles later?
4. **API compatibility**: Same REST shapes with stricter error codes?

---

## Success Criteria

### Part I (Process)
- [x] Evidence document from futon1 git history
- [x] PSR/PUR records for each coding session
- [x] Module headers explain pattern and rationale
- [x] README teaches architecture, not just usage
- [x] Traceability from devmap → pattern → code → test → doc

### Part II (Product)
- [x] All 5 core invariants pass their proof tests (Prototype 0, 82 tests)
- [x] Each invariant traces to both futon-theory/ and storage/ patterns
- [x] All 9 tension resolutions implemented and tested
- [x] Interface loops defined at each layer boundary with PSR/PUR governance
- [x] Canonical HTTP API specified in Section 2.6 (endpoints, shapes, error contract)
- [x] API serves over HTTP with system context injection (Prototype 1, f8beb11)
- [x] Read path: entity by UUID (`GET /entity/:id`) (Prototype 1, f8beb11)
- [ ] Read path: lookup by external-id (`GET /entity?source=S&external-id=E`)
- [ ] Proof-path event logging on all write operations
- [ ] Counter-ratchet detects unexpected count drops
- [ ] Any bug diagnosable in under 10 minutes
- [ ] Model descriptor system rebuilt (Section 2.11) — rich descriptors, type registry
- [ ] Meta/model API serves descriptors and runs verify (Section 2.11.4)
- [ ] futon1's 6 model descriptors ingested as first dataset (Section 2.11.5)
- [x] Migration from futon1 succeeds without data loss (17564 docs, checksum match)
- [ ] futon1a runs in production for 30 days without silent failures

### Derivation Requirements
Every new domain pattern must demonstrate:
- [ ] Cite at least one futon-theory axiom (A1-A5) or invariant (I0-I4)
- [ ] Reference a tension resolution from git evidence
- [ ] Trace: theory pattern → domain pattern → code module → test

---

## Checkpoints

### Checkpoint 1: Concurrency Stress Tests (2026-02-08)

Added stress tests exercising concurrent `durable-write!` and `validate-identity`
under load. These target the Layer 0 and Layer 1 hot paths that had no
concurrency coverage.

**Files changed:**

| File | Change |
|------|--------|
| `deps.edn` | Added `:test` alias (cognitect test-runner v0.5.1) |
| `test/futon1a/stress/durability_stress_test.clj` | **New** — 3 stress tests |
| `test/futon1a/stress/identity_stress_test.clj` | **New** — 1 stress test |

**Tests added:**

| Test | What it exercises | Result |
|------|-------------------|--------|
| (a) `concurrent-durable-write-no-append` | 50 threads call `durable-write!` with StubStore, no file append. Asserts valid proof-paths, tx-ids, no exceptions. | PASS |
| (b) `concurrent-durable-write-edn-append` | 50 threads write to a shared proof-log temp file. Asserts line count == 50, every line is valid EDN, unique `:path/id` values. | PASS |
| (c) `concurrent-durable-write-with-contention` | Same as (b) but with `SlowStore` adding 5ms latency in `tx-sync!` to widen the race window. | PASS |
| (d) `concurrent-identity-validation` | 50 threads call `validate-identity` with same external-id, alternating conflict/no-conflict. Asserts correct success/error shapes. | PASS |

**Verification:**
- `clj-kondo --lint src/ test/` — 0 errors, 0 warnings
- `clj -X:test` — 19 tests, 458 assertions, 0 failures, 0 errors

**Note on test (b):** `append-edn!` uses `spit :append true` with no locking.
The test passed because the EDN lines are short enough that OS-level writes are
effectively atomic. The race condition is still latent — it will surface under
heavier load, longer lines, or slower filesystems. A locking fix to `append-edn!`
is still warranted.

### Checkpoint 2: Full Layer Stack + Two Review Rounds (2026-02-08)

Two review-and-fix cycles brought Layers 0–4, pipeline, and API to a clean
state. Codex landed the missing modules (mirror, health, open-world, registry,
repair) and new tests; Claude reviewed twice and fixed all issues.

**Review round 1 (critical fixes, applied by Claude):**

| ID | Issue | Fix |
|----|-------|-----|
| C1 | Pipeline ordering was L3→L4→L1→L2→L0 | Reordered to L4→L3→L2→L1→L0 |
| C2 | `tx-ops` check threw L0 error (503) instead of L4 (400) | Changed to `mv/layer4-error` |
| C3 | Cross-layer error hierarchy test didn't verify L0 fields; omitted L3/L4 | Added field assertions for all 5 layers |

**Review round 1 (S/M items, applied by Codex):**

- S1: `routes/write` now catches `Exception` (not just `ExceptionInfo`)
- S2: `error->response` returns `{:reason :unknown}` on nil error
- S3: `durable-write!` catch re-throws exceptions that already carry layer info
- S4: `layer2-error` consolidated into `invariants.clj`; entity.clj and rehydrate.clj require it
- S5: `counter-ratchet` rejects non-numeric inputs
- M1: Module map updated (validation.clj, pipeline.clj, new modules)
- M2: Edge-case tests added (whitespace penholder, nil model, nil entity, etc.)
- M3: Placeholder test files deleted

**New modules landed by Codex:**

| Module | Purpose |
|--------|---------|
| `core/mirror.clj` | In-memory mirror store with mismatch detection |
| `diag/health.clj` | Health report with pluggable check functions |
| `ingest/open_world.clj` | Entity/relation ingest with optional model validation |
| `model/registry.clj` | Model descriptor registry with field/required validation |
| `scripts/repair.clj` | Repair/backfill with dry-run and counter-ratchet verification |

**Review round 2 (fixes applied by Claude):**

| ID | Issue | Fix |
|----|-------|-----|
| B1 | Phantom `xtdb-mem-kv` dep broke the build | Removed by Codex |
| S1 | Integration test called `tx-sync!` with a map | Changed to pass string only |
| S2 | `xtdb_node.clj` `tx-sync!` overloaded protocol | Simplified to string-only with Long parse fallback |
| S3 | New API handlers only caught `ExceptionInfo` | Extracted `with-error-handling` macro; all handlers now catch `Exception` |
| S5 | Registry global atom with no test isolation | Added `use-fixtures :each` to registry/ingest/routes tests |
| S6 | Test name "wins over" misleading | Renamed to "preempts" |
| M1 | Module map missing registry, repair, and new test suites | Added |
| M2 | Write response dropped proof-path | Now includes `:path/id` |
| M3 | Health always returned 200 | Returns 503 when degraded |

**Final state:**
- `clj-kondo --lint src/ test/` — 0 errors, 0 warnings
- `clj -X:test` — 66 tests, 569 assertions, 0 failures, 0 errors
- All 5 layers implemented with tests
- Cross-layer error propagation tested end-to-end through the API
- Pipeline ordering verified by dedicated test
- XTDB integration test passing with in-memory store
- All 5 previously missing modules landed with tests

### Checkpoint 3 — 2026-02-08

**What was done:**

- Added pattern/invariant/theory references to all 12 module ns docstrings
  (entity, penholder, validation, pipeline, mirror, health, open-world,
  registry, repair, invariants, errors, routes). Every module now traces to
  its invariant (I0-I4), pattern, and theory.
- Created `README-best-practice.md` documenting the Claude + Codex collaborative
  workflow, futonic discipline (layered gates, error shapes, proof-path protocol,
  PSR/PUR discipline), checkpointing, common pitfalls, and file layout.
- Traceability chain is complete: evidence → pattern → PSR → module → test → doc → error.
- Part I gate is satisfied: evidence doc, module map, PSR/PUR exemplars,
  traceability document, and module headers all cross-reference each other.

**Test state:** 66 tests, 569 assertions, 0 failures

### Checkpoint 4 — Prototype 0 (2026-02-08)

**What was done:**

Codex closed the remaining tension gates (T4, T6, T8, T9):

| Module | Purpose | Tension |
|--------|---------|---------|
| `model/expansion.clj` | Feature expansion allowlist gate (L4) | T9: Determinism vs expansion |
| `model/migration.clj` | Schema migration registry with version steps | T8: Schema evolution vs stability |
| `auth/penholder.clj` + `authorize-tooling!` | Tooling auth (L3 extension) | T6: Guardrails vs internal tooling |
| `core/pipeline.clj` + `run-open-world!` | Dedicated open-world ingest pipeline | T4: Open-world velocity vs validation |
| `cross_layer/interface_loop_test.clj` | Proves every layer returns actionable error context | I4: Rapid Debugging |
| `invariants/rapid_debugging_test.clj` | Proves health degradation surfaces correctly | I4: Rapid Debugging |

Claude reviewed — no critical or significant issues. Codex fixed two
docstring inaccuracies (84a7cef).

**Prototype 0 state:**
- 19 source modules, all with pattern/invariant header references
- 82 tests, 601 assertions, 0 failures
- All 5 invariants (I0-I4) implemented and tested
- All 9 tension resolutions have modules and tests
- Part I gate fully satisfied (traceability, PSR/PUR, evidence, module map)
- Expansion gates, tooling auth, open-world pipeline landed
- Interface loop test proves actionable error context at every layer boundary

**What Prototype 0 is:** A provably-correct scaffold. Every invariant holds,
every gate fires in the right order, every error carries the right context.
But the system cannot serve HTTP or persist to a real XTDB node.

---

## Prototype 1 Gate

**Goal:** A runnable system — futon1a starts, serves HTTP, persists to XTDB,
and survives a restart cycle with data intact.

### Exit Conditions

All of the following must be true:

1. **XTDB node lifecycle** — futon1a can start an embedded XTDB node, write
   to it, and shut it down cleanly. Node configuration is explicit (data dir,
   tx log). `system.clj` or equivalent owns the lifecycle.

2. **HTTP ring adapter** — The API surface in `routes.clj` is served over
   HTTP via Ring. A request to `/health` returns 200. A request to
   `/write` with a valid payload returns a `tx-id`.

3. **Restart cycle** — Start the system, write an entity through the HTTP API,
   stop the system, restart it, and read the entity back. Data survives the
   restart. This is the I0 (Persistence) invariant proven end-to-end, not
   just at the protocol level.

4. **Integration test** — An automated test exercises the full start → write →
   stop → restart → read cycle. This test uses a real (embedded) XTDB node,
   not a stub.

5. **All existing tests still pass** — No regressions. Prototype 0's 82 tests
   remain green.

### Scope In

- `system.clj` — XTDB node lifecycle + Ring server wiring
- Ring/Jetty HTTP adapter (dependency addition)
- `routes.clj` updates — Ring-compatible request/response handlers
- Integration test — full restart cycle
- Health check wired to real XTDB node status

### Scope Out

- Datascript mirror backend (deferred — XTDB-only for Prototype 1)
- Authentication middleware (penholder is passed in the request body for now)
- TLS, CORS, or production HTTP hardening
- Migration from futon1 data

### Open Questions (Resolve Before Implementation)

1. **Datascript**: Start XTDB-only, add Datascript read cache in Prototype 2?
   (Recommendation: yes, per Open Questions above.)
2. **Ring vs HTTP Kit**: Ring + Jetty is the standard choice for Clojure HTTP.
   Any reason to prefer otherwise?
3. **XTDB node type**: Embedded with RocksDB, or in-memory with tx-log on
   disk? (Recommendation: embedded with RocksDB for real persistence proof.)

---

## Prototype 2 Gate

**Goal:** A self-describing system — futon1a knows what it contains, can serve
its own specifications, and can verify its data against those specifications.
The system's first real dataset is its own model descriptors.

### Exit Conditions

All of the following must be true:

1. **Model descriptor format** — futon1a can store and retrieve model descriptors
   matching the format in Section 2.11.1: scope, version, certificate, entity
   definitions (with required fields and id-strategy), operations, invariants.
   The current `{:fields [...] :required #{...}}` registry is replaced or
   extended to handle the full descriptor shape.

2. **Type registry** — Entity, relation, and intent types are tracked in XTDB
   (not just an in-memory atom). Auto-registration on first encounter. Parent
   inference from namespaces. Alias support.

3. **Meta/model API** — `GET /meta/model` returns all registered descriptors.
   `GET /meta/model/:scope` returns a single descriptor. `GET /types` returns
   the type registry. These are read endpoints, not write-only.

4. **Verify endpoint** — `GET /meta/model/:scope/verify` runs the descriptor's
   named invariants against live XTDB data and returns pass/fail per invariant.
   At least one descriptor has at least one working verify check.

5. **First ingest** — futon1's 6 model descriptors (patterns, media, docbook,
   open-world, meta-model, penholder) are ingested as entities and relations
   through `run-open-world!`. Each descriptor becomes an entity. Each entity-type
   definition becomes an entity. Each invariant becomes a relation. The meta-model
   describes the other models.

6. **Compat surface specified** — The futon1-compat endpoints currently in
   `http/app.clj` (`/entity`, `/relation`, `/api/alpha/lab/session`) are added
   to Section 2.6 of the spec. No unspecified code.

7. **All existing tests still pass** — No regressions.

### Scope In

- Model descriptor storage (XTDB documents, not just in-memory atom)
- Type registry (XTDB-backed, with auto-registration and parent inference)
- `GET /meta/model`, `GET /meta/model/:scope`, `GET /types` endpoints
- `GET /meta/model/:scope/verify` with at least one working invariant check
- Ingest pipeline for futon1 model descriptors as graph data
- Spec update for compat endpoints

### Scope Out

- Full invariant verification for all 6 descriptors (one working is enough)
- Datascript mirror (still XTDB-only)
- StackExchange or PlanetMath ingest (that's futon6 territory)
- Production hardening
