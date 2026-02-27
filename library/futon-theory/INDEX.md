# Futon Theory Pattern Library

Design patterns that specify futon theory with sufficient generality to drive
development of the entire FUTON stack.

## Design Principle: Abstract Theory, Concrete Domains

These patterns are **abstract** - they define constraints and structures without
implementation-specific language. Domain-specific instantiations live elsewhere:

- `library/storage/` - Storage layer patterns (XTDB, Datascript specifics)
- `library/network/` - Network layer patterns (future)
- `library/simulation/` - Simulation patterns (future)

**Durability is enforced via invariants (I0), not axioms.** The axioms (A1-A5)
are operational (how work proceeds); invariants (I0-I4) are structural (what
must hold). This separation keeps theory portable across domains.

## Core Concepts

### The Proof Path

The fundamental unit of work in futon theory. Every change follows an auditable
path from intent to durable proof.

```
Intent → CLOCK_IN → OBSERVE → PROPOSE_CLAIM → APPLY_CHANGE
       → VERIFY → INVARIANT_CHECK → PROOF_COMMIT → CLOCK_OUT
```

See: [proof-path.flexiarg](proof-path.flexiarg)

### Axioms (A1-A5)

| Axiom | Statement | Pattern |
|-------|-----------|---------|
| A1 | Auditability first | proof-path, event-protocol |
| A2 | Changes attributable | agent-contract |
| A3 | Evidence-driven progression | symbolic-geodesic |
| A4 | Degradation detectable | counter-ratchet, error-hierarchy |
| A5 | Minimum viable events | minimum-viable-events |

### Invariants (I0-I4)

| Layer | Invariant | Statement | Pattern |
|-------|-----------|-----------|---------|
| 0 | Persistence | What you save is what you get back | durability-first |
| 1 | Identity | One entity per identity, no ambiguity | single-source-of-truth |
| 2 | Integrity | Startup succeeds completely or fails loudly | all-or-nothing |
| 3 | Hierarchy | Errors surface at the layer that caused them | error-hierarchy |
| 4 | Debugging | Any bug diagnosable in under 10 minutes | rapid-debugging |

### Supporting Patterns

| Pattern | Purpose |
|---------|---------|
| event-protocol | Canonical event sequence for proof paths |
| symbolic-geodesic | Shortest defensible path from intent to verified claim |
| retrospective-stability | Proof paths survive future refinement without rewriting |
| agent-contract | Requirements for agents operating in the stack |
| counter-ratchet | Key counts must not drop unexpectedly |
| stop-the-line | Block changes when invariants fail |
| curry-howard-operational | Specs mediate future state into present |
| task-as-arrow | Tasks are BHK arrows; gates compose into proof paths |
| retroactive-canonicalization | Specs emerge from ancestors via naming, selection, canalization |
| structural-tension-as-observation | Structural tension is the observation vector for library evolution; two nested AIF loops |

## Futonic Logic

The formal vocabulary, composition rules, and recognition loop that the
patterns above presuppose. Not a replacement for the patterns but a
specification of the symbolic operations they perform.

| Symbol | Role | In Practice |
|--------|------|-------------|
| 象 | configuration | Wiring diagrams, flexiarg patterns, proof paths, sessions |
| 部 | decomposition-regime | Timescale split, gate pipeline, derivation xenotype, Baldwin cycle |
| 咅 | articulation | PSR, argument claims, mission scoping, git commits |
| 鹽 = (⿱ 鹵 皿) | generative-composition | Pattern library (鹵) + evidence shapes (皿) → validated proof-path |
| 間 | honest-interval | Structured interval between components; read before composing |
| 香 | embodied-salience | Pattern search, hotword matching, tension scanning |
| 味 | embodied-evaluation | PUR, mana confidence bins, structural checks |
| 🔮 = (能 . 捨) | regulator | Sospeso protocol, gate rejection, mission parking |
| 未知 / 非死非活 | non-equilibrium-interval | Abandoned missions, sospeso interval, pre-implementation state |

Axiom A7 (compositional salience): generativity requires both 鹵 and 皿
perceivable under the same 部, and only after perceiving 間 (the honest
interval) between components. Agency/Forum/Drawbridge had 鹵 (potential)
but no 皿 (recording protocol) — composition could not form.

See: [futonic-logic.flexiarg](futonic-logic.flexiarg)

## Evolutionary Patterns (from futon5)

How the stack evolves while maintaining auditability.

### Four Types (型)

| Type | Description | In Futon |
|------|-------------|----------|
| Genotype (基因型) | Replicable internal representation | Rules, patterns, DSL, priors |
| Phenotype (表型) | Observable behavior in environment | Outputs, test metrics, API responses |
| Exotype (外顯子型) | Interface shape (connectable) | Layer contracts, event protocols |
| Xenotype (異種型) | Portable control structure | Baldwin cycle, reusable at any interface |

See: [four-types.flexiarg](four-types.flexiarg)

### Baldwin Cycle

```
EXPLORE (within exotype constraints)
    ↓ successful adaptation
ASSIMILATE (fix into genotype, with evidence)
    ↓ proven success
CANALIZE (remove freedom, make success cheap)
```

See: [baldwin-cycle.flexiarg](baldwin-cycle.flexiarg)

### Evolutionary Patterns

| Pattern | Purpose |
|---------|---------|
| four-types | Genotype/phenotype/exotype/xenotype framework |
| baldwin-cycle | Explore → assimilate → canalize loop |
| interface-loop | Baldwin cycle at each layer boundary |
| local-gain-persistence | Gains must persist or be explicitly deleted |
| xenotype-portability | Same adaptation skeleton, many instantiations |
| retroactive-canonicalization | How ancestors become theory via naming/selection/canalization |

### Key Invariant

**LocalGainMustEitherPersistOrBeDeleted**: Any functional gain at an interface
must either be assimilated to genotype (replayable) or explicitly removed.
No ghost capabilities.

See: [local-gain-persistence.flexiarg](local-gain-persistence.flexiarg)

### Theory as Exotype (Meta-Level)

The four-types framework applies recursively:

```
Level 0 (Meta):     futon-theory ─────────────── exotype
                         ↓ constrains
Level 1 (Domain):   storage-patterns ─────────── genotype
                         ↓ instantiates
Level 2 (Code):     futon1a ─────────────────── phenotype
```

The same derivation xenotype applies to all domains:

| Domain | Derived Patterns | Implementation |
|--------|------------------|----------------|
| Storage | write-through-with-sync, unique-constraint | futon1a |
| Network | message-with-receipt, session-bracketing | futon3 |
| Simulation | explore-then-fix, layer-adaptation | futon5 |
| Coordination | distributed-proof-path | futon4 |

See: [theory-as-exotype.flexiarg](theory-as-exotype.flexiarg)

## Mission Control Patterns

Patterns for governing missions at the project level.

### Mission Lifecycle

Missions progress through defined states with evidence requirements:

```
:greenfield → :scoped → :active → :blocked → :review → :done
```

See: [mission-lifecycle.flexiarg](mission-lifecycle.flexiarg)

### Mission Control Patterns

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| mission-lifecycle | 🔃/马 | State machine for mission progression |
| mission-scoping | 🎒/王 | Bounded ownership and success criteria |
| mission-dependency | 〰️/双 | DAG of mission relationships |
| coordination-protocol | 🎎/人 | Multi-agent handoff and review |
| progress-signal | ✌️/门 | Evidence accumulation and health tracking |

### Four Types Applied to Missions

| Type | Mission Application |
|------|---------------------|
| Genotype | Mission template (required fields, state machine) |
| Phenotype | Actual mission work (commits, artifacts) |
| Exotype | Work plan structure (phases, gates, handoffs) |
| Xenotype | Ping-pong review cycle (reusable across missions) |

## Layered Architecture

```
Layer 4: Model Validation (400 Bad Request)
    ↑ depends on
Layer 3: Authorization (403 Forbidden)
    ↑ depends on
Layer 2: Integrity (500 Internal Server Error)
    ↑ depends on
Layer 1: Identity (409 Conflict)
    ↑ depends on
Layer 0: Durability (503 Service Unavailable)
    ↑ depends on nothing
```

Each layer is a gate. If Layer N fails, Layers N+1 and above never run.
Errors surface at the lowest failing layer.

## Usage

These patterns drive:
- **futon1a**: Ground-up rebuild with unbreakable core invariants
- **futon3 checks**: Verification DSL for proof paths
- **Lab sessions**: Agent behavior conforming to event protocol
- **Stack coherence**: Cross-repo invariant enforcement

## Example: futon1a Module Mapping

| Module | Pattern(s) |
|--------|------------|
| `core/xtdb.clj` | durability-first |
| `core/identity.clj` | single-source-of-truth |
| `core/entity.clj` | all-or-nothing |
| `core/rehydrate.clj` | all-or-nothing |
| `api/errors.clj` | error-hierarchy |
| `diag/health.clj` | rapid-debugging |

## References

- Mission: [M-futon1a-rebuild.md](../../holes/missions/M-futon1a-rebuild.md)
- Sketch: [SKETCH-futon1a.md](~/code/futon1/SKETCH-futon1a.md)
- Stack: [futon0/README.md](~/code/futon0/README.md)
