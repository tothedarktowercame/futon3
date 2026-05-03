# Mission: Coordination Rewrite
Status: archived

Rewrite futon3's coordination layer as a six-gate pipeline operating over a
unified typed graph, composing futon3's active components (P0-P5) with
futon3a's inference rules (typed arrows, validation pipeline, chain scoring)
through the 12 coordination patterns derived from futon-theory.

This mission is the INSTANTIATE step of the derivation xenotype:
IDENTIFY -> MAP -> DERIVE -> ARGUE -> VERIFY -> **INSTANTIATE**.

## Owner

Claude (with Codex as primary implementer for Part II code modules).

## Reviewer

Codex reviews Part I process artifacts.
Claude reviews Part II code for pattern conformance and gate integrity.
Cross-validation per `coordination/cross-validation-protocol`.

## Scope

### Scope In

- Part I: Composition plan — map futon3 components and futon3a inference rules
  to the gate pipeline, with PSR/PUR exemplars and traceability gates.
- Part II: Implement the gate pipeline as `futon3.gate.*` namespaces, composing
  existing code rather than replacing it.
- Part III: Wire Level 1 (glacial loop) — tension observer and canonicalizer
  that close the library evolution feedback loop.
- Produce the minimum artifacts needed to make gate invariants verifiable:
  gate rejection catalog, evidence shape catalog, integration test harness.

### Scope Out

- Rewriting futon3's MUSN transport (P0) — it stays as the IO layer.
- Rewriting futon3a — its modules are composed, not replaced.
- UI beyond existing HTTP/WS surfaces.
- Joy metrics (P8) and training ground (P10) — they fall out as consequences
  but are separate missions.
- More than 5 success criteria per prototype gate — split if exceeded
  (per `futon-theory/mission-scoping`).

## Time Box

3 weeks to complete Part I gate and land Prototype 0 (gate pipeline scaffold
with all six gates rejecting malformed input, no Level 1 yet).

## Exit Conditions

- Part I gate is satisfied with concrete artifacts (composition map,
  evidence shapes, PSR/PUR exemplars, traceability chain).
- Prototype 0: all six gates enforce their patterns and produce structured
  rejections. Integration test exercises the full G5->G0 path.
- The exotype diagram (`coordination-exotype.edn`) validates the prototype
  via `ct/mission.clj` — all 8 checks pass on the concrete instantiation.

## Interface Signature

### Mission Diagram

```mermaid
graph LR
    subgraph inputs
        IR[I-request<br/>http-request]
        IE[I-environment<br/>xtdb-entity]
        IM[I-missions<br/>config / slow]
        IP[I-patterns<br/>config / glacial]
        IRG[I-registry<br/>config / slow]
        IT[I-tensions<br/>xtdb-entity / glacial]
    end

    subgraph "Level 0: Gate Pipeline"
        G5[G5 Task Specification<br/>coordination/task-shape-validation] --> G4[G4 Agent Authorization<br/>coordination/capability-gate]
        G4 --> G3[G3 Pattern Reference<br/>coordination/mandatory-psr]
        G3 --> G2[G2 Execution<br/>coordination/bounded-execution]
        G2 --> G1[G1 Validation<br/>coordination/mandatory-pur]
        G1 --> G0[G0 Evidence Durability<br/>coordination/session-durability-check]
    end

    subgraph "Level 0: Default Mode"
        CD[C-default<br/>chapter0/default-mode-network]
    end

    subgraph "Level 1: Library Evolution"
        L1O[L1-observe<br/>structural-tension-as-observation] --> L1C[L1-canon<br/>retroactive-canonicalization]
    end

    subgraph outputs
        OA[O-artifacts<br/>xtdb-entity]
        OE[O-evidence<br/>xtdb-entity]
        OP[O-proof-path<br/>proof-path]
        OV[O-events<br/>xtdb-entity]
        OER[O-errors<br/>error-response]
        OC[O-canonizations<br/>xtdb-entity / glacial]
    end

    IR --> G5
    IM --> G5
    IRG --> G4
    IP --> G3
    IP --> G1
    IE --> G2
    G2 --> IE

    G3 -.->|PSR| G2
    G3 -.->|PSR| G1

    IE --> CD
    IRG --> CD
    CD --> IR
    CD --> OV

    G2 --> OA
    G2 --> OV
    G1 --> OE
    G0 --> OE
    G0 --> OP
    G0 --> OV

    G5 --> OER
    G4 --> OER
    G3 --> OER
    G2 --> OER
    G1 --> OER
    G0 --> OER

    IT --> L1O
    IP --> L1C
    L1C --> IP
    L1C --> OC
```

### Input Ports

| Port | Type | Source | Timescale | Constraint? | Description |
|------|------|--------|-----------|-------------|-------------|
| `I-request` | `http-request` | human / mission queue / peer agent | fast | no | Raw work request to be coordinated |
| `I-environment` | `xtdb-entity` | codebase / project state / running services | fast | no | What agents observe and act upon |
| `I-missions` | `config` | mission specs / devmap | slow | yes | What counts as success |
| `I-patterns` | `config` | `library/**/*.flexiarg` | glacial | yes | How to do things well (dual role: constraint for L0, environment for L1) |
| `I-registry` | `config` | agent registry / capability declarations | slow | yes | Who can do what |
| `I-tensions` | `xtdb-entity` | accumulated gap-PSRs / PARs / cross-repo evidence | glacial | no | Observation vector for library evolution |

### Output Ports

| Port | Type | Consumer | Spec Ref | Description |
|------|------|----------|----------|-------------|
| `O-artifacts` | `xtdb-entity` | codebase / review | `coordination/artifact-registration` | What the coordinated work produced |
| `O-evidence` | `xtdb-entity` | pattern library / meta-learning | `coordination/par-as-obligation` | PSR/PUR/PAR records documenting pattern use |
| `O-proof-path` | `proof-path` | audit / replay / reconstruction | `coordination/session-durability-check` | Audit trail: when you cross the gate, there is still a path to replay |
| `O-events` | `xtdb-entity` | monitoring / diagnostics / forum | `futon-theory/event-protocol` | Observable telemetry |
| `O-errors` | `error-response` | human / retry / monitoring | `futon-theory/error-hierarchy` | Structured gate rejections with gate attribution |
| `O-canonizations` | `xtdb-entity` | pattern library / genealogy graph | `futon-theory/retroactive-canonicalization` | Library evolution actions (glacial) |

### Composition Constraint

Every output port must be:
1. **Specified** in a coordination pattern (spec-ref column)
2. **Implemented** in a gate module (`futon3.gate.*`)
3. **Tested** (at least one test exercises the port)
4. **Traceable** to the exotype diagram (edge in `coordination-exotype.edn`)

If an output port exists in code but not in the exotype diagram, that is a
stop-the-line violation (`futon-theory/stop-the-line`).

## Argument (Pattern-Backed)

The full argument is in `library/coordination/ARGUMENT.flexiarg`. Summary:

Futon3's IFR says it should "turn messy activity into organised knowledge by
checking work against shared patterns and producing auditable records." The
current architecture cannot do this because its knowledge is scattered across
disconnected stores — the pattern store in futon1, typed arrows in futon3a's
SQLite, trails in futon3's event log, and session evidence in flat files.

The coordination patterns (12 patterns across 6 gates) specify a pipeline
that enforces pattern use, validates outputs, and persists evidence. Futon3a
already provides much of the pipeline infrastructure (typed arrows, validation
schemas, chain scoring, compass navigation) under different names. The rewrite
composes these existing bodies of work through a unified gate pipeline into a
single typed graph.

The coordination system is a two-level AIF wiring diagram: a fast loop
governing task execution through the gate pipeline and a glacial loop
governing library evolution through canonicalization, sharing the same
boundary. The AIF framework provides six checkable invariants (I1-I6) that
the rewrite must satisfy at both levels. The abstract diagram
(`coordination-exotype.edn`) passes all 8 machine checks via `ct/mission.clj`.

Pattern references:

- Gate patterns: `library/coordination/*.flexiarg` (12 patterns, 2 per gate)
- Theory patterns: `library/futon-theory/task-as-arrow.flexiarg`,
  `library/futon-theory/retroactive-canonicalization.flexiarg`,
  `library/futon-theory/structural-tension-as-observation.flexiarg`
- AIF specification: `futon5/docs/chapter0-aif-as-wiring-diagram.md`
- Exotype diagram: `futon5/data/missions/coordination-exotype.edn`
- Concrete diagram: `futon5/data/missions/futon3-coordination.edn`
- Full argument: `library/coordination/ARGUMENT.flexiarg`
- Gate mapping: `futon5/docs/gate-pattern-mapping.md`

## Part I: Composition Plan

### 1.1 Composition Map

The rewrite composes, not replaces. Each gate maps to existing futon3 and
futon3a components:

| Gate | futon3 Component | futon3a Component | New Code |
|------|------------------|-------------------|----------|
| G5 Task Specification | P4 Workday bridge (entry point) | `sidecar.validation` (shape enforcement) | `gate.task` — shape validation + mission binding |
| G4 Agent Authorization | — (no current model) | — | `gate.auth` — registry lookup + capability match |
| G3 Pattern Reference | P1 Pattern store + P5 Similarity field | `futon.notions` (search) + `futon.compass` (navigation) | `gate.pattern` — PSR creation + search protocol |
| G2 Execution | P2 Check DSL (evaluation engine) | `meme.arrow` (typed edges) + `sidecar.store` (proposal pipeline) | `gate.exec` — bounded execution + artifact registration |
| G1 Validation | P3 Trails (session records) | `sidecar.validation` (schema) + `meme.arrow/chain-score` (quality) | `gate.validate` — PUR creation + cross-validation |
| G0 Evidence Durability | — (ad hoc persistence) | — | `gate.evidence` — durability check + PAR obligation |

### 1.2 Evidence Shape Catalog

Each gate produces typed evidence records. These are the morphisms in the
gate pipeline:

| Record | Produced At | Shape | Consumed By |
|--------|-------------|-------|-------------|
| Task Spec | G5 | `{:task/id, :task/mission-ref, :task/scope, :task/typed-io, :task/success-criteria, :task/intent}` | G4, G3, G2 |
| Assignment | G4 | `{:assign/agent-id, :assign/task-id, :assign/capabilities, :assign/exclusive?}` | G2 |
| PSR | G3 | `{:psr/id, :psr/task-id, :psr/pattern-ref, :psr/candidates, :psr/rationale, :psr/type [:selection :gap]}` | G2, G1 |
| Artifact | G2 | `{:artifact/id, :artifact/task-id, :artifact/type, :artifact/ref, :artifact/registered-at}` | G1, G0 |
| PUR | G1 | `{:pur/id, :pur/psr-ref, :pur/criteria-eval, :pur/outcome, :pur/prediction-error}` | G0 |
| PAR | G0 | `{:par/id, :par/session-ref, :par/what-worked, :par/what-didnt, :par/prediction-errors, :par/suggestions}` | L1 (via O-evidence → I-tensions) |

### 1.3 Theory Grounding

The gate pipeline implements futon-theory axioms:

| Axiom | Requirement | Gate Implementation |
|-------|-------------|---------------------|
| A1: Auditability | Every operation traceable | Proof-path through all six gates; G0 persists the full path |
| A2: Attributable | Changes have clear source | G4 assigns agent; G2 registers artifacts with agent attribution |
| A3: Evidence-driven | Progression requires proof | G3 requires PSR before execution; G1 requires PUR after |
| A4: Degradation detectable | Failures surface early | Each gate can reject; errors carry gate attribution |
| A5: Minimum events | Log what's needed | Evidence shapes are typed and minimal; no verbose logs |

Chapter 0 invariants (from `chapter0-aif-as-wiring-diagram.md`):

| Invariant | Statement | How Enforced |
|-----------|-----------|--------------|
| I1: Boundary | The gate pipeline IS the system boundary | All work enters through G5, exits through G0 |
| I2: Observation-action asymmetry | Only G2 touches the environment | Other gates read config/evidence only |
| I3: Timescale separation | Slow constrains fast | Missions (slow) → G5; Patterns (glacial) → G3, G1; Registry (slow) → G4 |
| I4: Preference exogeneity | No fast→constraint path | No output feeds back to missions/patterns/registry at fast timescale |
| I5: Model adequacy | Models match environment | PSR selection matches task shape; PUR evaluates outcome |
| I6: Compositional closure | System sustains without deliberation | C-default provides inter-task baseline behavior |

### 1.4 Dual Interpretation

Each gate mediates between agent problems and pattern problems (from
`coordination/INDEX.md`):

| Gate | Agency Reading | Pattern Reading |
|------|---------------|-----------------|
| G5 | Is the agent's task well-defined? | Is the task shaped so a pattern can guide it? |
| G4 | Can this agent do this work? | Does this pattern apply to this domain? |
| G3 | Has the agent committed to an approach? | Has a pattern been engaged with? |
| G2 | Agent stays within budget | Pattern application stays within scope |
| G1 | Agent's work gets evaluated | Pattern's guidance gets evaluated |
| G0 | Agent's session is recoverable | Pattern use evidence is persistent |

### 1.5 Traceability Chain

Every design decision must trace from devmap through pattern to code:

```
devmap → coordination-pattern → theory-pattern → exotype-edge → gate-module → test → evidence-shape → error
futon3.devmap:IFR → coordination/mandatory-psr → futon-theory/task-as-arrow → G3→G2:http-request → gate.pattern/select! → test/gate/pattern_test.clj → {:psr/id ...} → G3/PATTERN_NOT_FOUND
```

### 1.6 PSR/PUR Storage

Following futon1a convention:

1. Location: `holes/labs/coordination/psr/` and `holes/labs/coordination/pur/`.
2. Filename: `YYYY-MM-DD__<gate>__<pattern>.md`.
3. Required fields: context, pattern, decision, alternatives, outcome, evidence link.

### 1.7 Part I Gate (Process Acceptance)

Do not begin Part II until all items are satisfied:

1. Composition map complete and reviewed (§1.1).
2. Evidence shape catalog defines all inter-gate record types (§1.2).
3. Theory grounding maps each axiom and invariant to gate behavior (§1.3).
4. At least one PSR + PUR exemplar exists (§1.6).
5. Traceability example present (devmap → pattern → exotype → gate → test → evidence → error).
6. Reviewer (Codex) has signed off on Part I artifacts.

---

## Part II: Gate Pipeline Implementation

The gate pipeline composes existing code through a unified namespace structure.
Part II is constrained by the coordination-exotype diagram (the exotype) and
instantiates the 12 coordination patterns (the genotype) as working code
(the phenotype).

### 2.1 Namespace Layout

```
futon3.gate.task       ;; G5 — task-shape-validation, intent-to-mission-binding
futon3.gate.auth       ;; G4 — capability-gate, assignment-binding
futon3.gate.pattern    ;; G3 — mandatory-psr, pattern-search-protocol
futon3.gate.exec       ;; G2 — bounded-execution, artifact-registration
futon3.gate.validate   ;; G1 — mandatory-pur, cross-validation-protocol
futon3.gate.evidence   ;; G0 — session-durability-check, par-as-obligation
futon3.gate.pipeline   ;; G5→G0 composition (BHK arrow composition)
futon3.gate.default    ;; C-default — inter-task baseline behavior
futon3.gate.shapes     ;; Evidence shape specs (shared across gates)
futon3.gate.errors     ;; Gate rejection catalog
```

### 2.2 Gate Error Catalog

Every gate rejection has a typed error with gate attribution. Following
futon1a's error hierarchy:

| Gate | Error Key | HTTP Status | Meaning |
|------|-----------|-------------|---------|
| G5 | `:g5/missing-mission-ref` | 400 | Task has no mission reference |
| G5 | `:g5/missing-success-criteria` | 400 | Task has no success criteria |
| G5 | `:g5/mission-not-active` | 400 | Referenced mission is not in `:active` state |
| G4 | `:g4/agent-not-registered` | 403 | Agent not in registry |
| G4 | `:g4/capability-mismatch` | 403 | Agent lacks required capabilities |
| G4 | `:g4/already-assigned` | 409 | Task already assigned to another agent |
| G3 | `:g3/no-psr` | 400 | No PSR provided and no gap declaration |
| G3 | `:g3/pattern-not-found` | 404 | Referenced pattern not in library |
| G2 | `:g2/budget-exceeded` | 408 | Execution budget (time/tokens) exceeded |
| G2 | `:g2/artifact-unregistered` | 500 | Produced artifact not registered in graph |
| G1 | `:g1/no-pur` | 400 | No PUR produced for PSR |
| G1 | `:g1/criteria-not-met` | 422 | Success criteria evaluation failed |
| G0 | `:g0/durability-failed` | 503 | Evidence not persisted |
| G0 | `:g0/no-par` | 400 | No PAR produced before session close |

### 2.3 Tension Resolutions

These design decisions resolve the tensions from the ARGUMENT (E1-E7):

| Tension | Resolution | Pattern |
|---------|------------|---------|
| E1: Pattern store disconnected from phylogeny | Compose P1 (patterns) + futon3a arrows into one graph | `coordination/mandatory-psr` |
| E2: Checks don't enforce pattern selection | G3 makes PSR mandatory before G2 executes | `coordination/mandatory-psr` |
| E3: Trails lack typed evidence | Evidence shapes at each gate produce typed, linked records | `coordination/mandatory-pur` |
| E4: Workday input unvalidated | G5 validates task shape before pipeline proceeds | `coordination/task-shape-validation` |
| E5: Search not wired to pipeline | G3 wraps notions/compass search in a gate with PSR output | `coordination/pattern-search-protocol` |
| E6: Greenfield prototypes stalled | They fall out as graph queries once pipeline + graph exist | composition (§1.1) |
| E7: Graph convergence unexploited | futon3a inference rules become the edge types in the unified graph | `futon-theory/task-as-arrow` |

#### 2.3.1 Tension → Module → Test Mapping

| Tension | Module(s) | Test(s) |
|---------|-----------|---------|
| E1: Disconnected stores | `gate.pattern`, `gate.shapes` | `test/gate/pattern_test.clj` |
| E2: No mandatory PSR | `gate.pattern`, `gate.pipeline` | `test/gate/pipeline_test.clj` |
| E3: Untyped trails | `gate.validate`, `gate.evidence` | `test/gate/evidence_test.clj` |
| E4: Unvalidated tasks | `gate.task` | `test/gate/task_test.clj` |
| E5: Unwired search | `gate.pattern` | `test/gate/pattern_search_test.clj` |
| E6: Stalled greenfield | `gate.pipeline` (enables P6-P11) | `test/gate/pipeline_integration_test.clj` |
| E7: Separate graph | `gate.shapes`, `gate.exec` | `test/gate/graph_test.clj` |

### 2.4 Composition from Existing Code

The gate modules compose existing implementations:

```
gate.task:
  wraps: sidecar.validation (shape checking)
  calls: futon3 mission store (mission-ref lookup)
  new:   mission-binding check, intent extraction

gate.auth:
  new:   agent registry, capability matching, exclusive assignment
  reads: I-registry (config)

gate.pattern:
  wraps: futon.notions (keyword + embedding search)
  wraps: futon.compass (GFE-inspired navigation)
  wraps: futon3 P1 pattern store (pattern entities)
  calls: futon3a meme.arrow (phylogeny traversal)
  new:   PSR shape creation, gap declaration, search logging

gate.exec:
  wraps: futon3 P2 check DSL (evaluation)
  wraps: futon3a sidecar.store (proposal→fact pipeline)
  calls: futon3a meme.arrow (typed edge creation)
  new:   budget enforcement, artifact registration

gate.validate:
  wraps: futon3a sidecar.validation (schema enforcement)
  wraps: futon3a meme.arrow/chain-score (quality scoring)
  new:   PUR shape creation, cross-validation dispatch

gate.evidence:
  calls: futon1a durability layer (proof-path persistence)
  new:   session durability check, PAR obligation enforcement
```

### 2.5 Testing Strategy

**Gate tests** (fast, isolated):
- Does G5 reject tasks without mission-ref?
- Does G4 reject unregistered agents?
- Does G3 reject tasks without PSR or gap declaration?
- Does G2 halt on budget exceeded?
- Does G1 reject when success criteria fail?
- Does G0 reject when evidence not persisted?

**Pipeline tests** (integration):
- Full G5→G0 path with valid input produces proof-path
- First failing gate short-circuits; later gates never run
- Error carries gate attribution (which gate rejected)

**Cross-gate evidence tests**:
- PSR created at G3 is available at G1 for PUR creation
- PUR created at G1 is available at G0 for PAR creation
- Proof-path at G0 contains all evidence from all gates

**Exotype conformance test**:
- Run `ct/mission.clj` validators against the concrete instantiation
- All 8 checks pass (completeness, coverage, orphans, type safety,
  spec coverage, I3, I4, I6)

### 2.6 Part II Gate (Code Acceptance)

Do not begin Part III until:

1. All six gate namespaces exist with pattern/theory header references.
2. Gate error catalog fully implemented (§2.2).
3. Pipeline composition test passes (G5→G0 with valid and invalid inputs).
4. Evidence shapes validate against `gate.shapes` specs.
5. Cross-gate evidence flow tested (PSR→PUR→PAR chain).
6. `ct/mission.clj` validates the concrete instantiation against the exotype.
7. Reviewer (Claude) has reviewed for pattern conformance.

---

## Part III: Level 1 — Library Evolution

The glacial loop. This is the Level 1 gap identified in structural
verification — the concrete diagram has all Level 0 roles but no
`L1-observe` or `L1-canon`. Part III closes that gap.

### 3.1 Tension Observer

`L1-observe` reads accumulated evidence (gap-PSRs, PARs, cross-repo
patterns) and produces structural tension observations using the three
criteria from `futon-theory/structural-tension-as-observation`:

1. **Structural irritation** (结构刺激): Recurring gap-PSRs across sessions
2. **Pre-symbolic pressure** (前符号压力): Tasks hard to scope/match/validate
3. **Trans-situational reappearance** (跨情境复现): Same fix in different contexts

Pattern: `futon-theory/structural-tension-as-observation`

### 3.2 Canonicalizer

`L1-canon` reads observed tensions + current library state and produces
canonization events following the three-step Baldwin cycle from
`futon-theory/retroactive-canonicalization`:

1. **NAMING**: Recurring practice gets a stable name
2. **SELECTION**: Named practice shows trans-situational reappearance
3. **CANALIZATION**: Selected practice constrains future work

The canonicalizer writes to `I-patterns` (the library). This is the glacial
loop closing — library evolution modifying the constraints that govern task
execution. I3 allows this: glacial → glacial. I4 is clean: no output port
reaches I-patterns through a fast path.

Pattern: `futon-theory/retroactive-canonicalization`

### 3.3 Part III Gate

1. Tension observer produces structured observations from accumulated evidence.
2. Canonicalizer produces canonization events that update the library.
3. Full loop test: gap-PSR accumulation → tension observation → canonization
   → library update → subsequent G3 uses the new pattern.
4. I3 check: all Level 1 edges are glacial→glacial.
5. I4 check: no fast output→I-patterns path.
6. `ct/mission.clj` validates the complete two-loop diagram.

---

## Success Criteria

### Part I (Process)
- [ ] Composition map maps all six gates to existing code (§1.1)
- [ ] Evidence shape catalog defines all inter-gate record types (§1.2)
- [ ] Theory grounding covers all 5 axioms and 6 invariants (§1.3)
- [ ] Traceability from devmap → pattern → exotype → gate → test → evidence → error
- [ ] PSR/PUR exemplar exists
- [ ] Reviewer sign-off on Part I artifacts

### Part II (Product)
- [ ] All six gate namespaces implemented with pattern/theory headers
- [ ] Gate error catalog fully implemented with gate attribution
- [ ] Pipeline test: G5→G0 with valid input produces complete proof-path
- [ ] Pipeline test: first failing gate short-circuits, error carries gate id
- [ ] Evidence shapes validate against specs
- [ ] PSR→PUR→PAR chain tested end-to-end
- [ ] `ct/mission.clj` validates concrete instantiation: 8/8 checks pass
- [ ] Reviewer sign-off on code

### Part III (Level 1)
- [ ] L1-observe produces structured tension observations
- [ ] L1-canon produces canonization events that update library
- [ ] Full glacial loop test: accumulate → observe → canonize → updated constraint
- [ ] I3/I4 checks pass for the two-loop diagram
- [ ] `ct/mission.clj` validates complete diagram: 8/8 checks pass

### Derivation Requirements
- [ ] Every gate module cites its coordination patterns (2 per gate)
- [ ] Every coordination pattern cites its theory grounding (axiom or invariant)
- [ ] Every tension resolution (E1-E7) maps to module + test
- [ ] Trace: devmap IFR → coordination pattern → theory → exotype edge → gate → test

## Specification Reference

- **Abstract diagram (exotype):** `futon5/data/missions/coordination-exotype.edn`
- **Concrete diagram (Level 0):** `futon5/data/missions/futon3-coordination.edn`
- **Static analysis (existing):** `futon5/data/missions/futon3-agent-loop.edn`
- **Coordination patterns:** `library/coordination/INDEX.md`
- **Theory patterns:** `library/futon-theory/INDEX.md`
- **Gate mapping:** `futon5/docs/gate-pattern-mapping.md`
- **Full argument:** `library/coordination/ARGUMENT.flexiarg`
- **AIF specification:** `futon5/docs/chapter0-aif-as-wiring-diagram.md`
- **Validator:** `futon5/src/futon5/ct/mission.clj`
- **futon1a exemplar:** `holes/missions/M-futon1a-rebuild.md`

## Checkpoints

(To be filled as work progresses. Each checkpoint records what was done,
what was reviewed, and what changed as a result of review — following the
futon1a checkpoint convention.)

---

## Derivation Provenance

This mission is the product of the derivation xenotype applied to the
coordination domain:

| Step | What Was Done | Artifact |
|------|---------------|----------|
| IDENTIFY | Audited ~39 ancestral patterns across 5 library directories | `futon5/docs/gate-pattern-mapping.md` §IDENTIFY |
| MAP | Mapped ancestors to six gates using proof path ↔ gate alignment | `futon5/docs/gate-pattern-mapping.md` §MAP |
| DERIVE | Wrote 12 coordination patterns (2 per gate) as flexiarg specs | `library/coordination/*.flexiarg` |
| ARGUE | Constructed argument from pattern conclusions + futon3a evidence | `library/coordination/ARGUMENT.flexiarg` |
| VERIFY (structural) | Translated to exotype diagram; 8/8 checks pass | `futon5/data/missions/coordination-exotype.edn` |
| VERIFY (empirical) | Traced git history; all 7 tensions + structural cycle confirmed | `ARGUMENT.flexiarg` §EMPIRICAL VERIFICATION |
| INSTANTIATE | This mission document | `holes/missions/M-coordination-rewrite.md` |

The derivation xenotype (`futon-theory/xenotype-portability`) is the same
adaptation skeleton used for the futon1a storage domain. The coordination
domain is a second instantiation of the same xenotype — different domain,
same derivation structure, same verification methods.
