# Coordination Patterns

Domain-specific patterns for the futon3 agent coordination pipeline,
derived from futon-theory via the derivation xenotype (IDENTIFY -> MAP ->
DERIVE -> VERIFY -> INSTANTIATE).

These patterns specify the six mandatory gates (G5-G0) that every agent
action must pass through. They are the coordination-domain equivalent of
the storage patterns that drove the futon1a rebuild.

## Derivation

```
Level 0 (Meta):     futon-theory ──────────── exotype (constraints)
                         | constrains
Level 1 (Domain):   coordination/ ─────────── genotype (these patterns)
                         | instantiates
Level 2 (Code):     futon3.gate.* ─────────── phenotype (future)
```

See: `futon5/docs/gate-pattern-mapping.md` for the full derivation trace
including theory grounding, ancestral pattern evidence, and gap analysis.

## Gate Pipeline

```
G5 Task Specification ─── Is the task well-defined?
  │
G4 Agent Authorization ── Is the agent registered and capable?
  │
G3 Pattern Reference ──── Has a pattern been selected?
  │
G2 Execution ───────────── Do the work.
  │
G1 Validation ──────────── Does the output satisfy criteria?
  │
G0 Evidence Durability ── Is everything persisted?
```

## Patterns by Gate

### G5: Task Specification

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| [task-shape-validation](task-shape-validation.flexiarg) | G5/形 | Task must have mission-ref, scope, typed-io, success-criteria, intent |
| [intent-to-mission-binding](intent-to-mission-binding.flexiarg) | G5/绑 | Task must bind to an existing active mission |

### G4: Agent Authorization

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| [capability-gate](capability-gate.flexiarg) | G4/能 | Agent capabilities must match task requirements |
| [assignment-binding](assignment-binding.flexiarg) | G4/派 | Agent must be exclusively assigned to the task |

### G3: Pattern Reference

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| [mandatory-psr](mandatory-psr.flexiarg) | G3/选 | Every task must carry a PSR (selection or gap) |
| [pattern-search-protocol](pattern-search-protocol.flexiarg) | G3/搜 | Pattern search follows a defined, logged protocol |

### G2: Execution

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| [bounded-execution](bounded-execution.flexiarg) | G2/限 | Execution has budgets; exceeding them pauses, not crashes |
| [artifact-registration](artifact-registration.flexiarg) | G2/录 | All produced artifacts registered in HX before leaving G2 |

### G1: Validation

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| [mandatory-pur](mandatory-pur.flexiarg) | G1/验 | Every PSR produces a matching PUR with criteria evaluation |
| [cross-validation-protocol](cross-validation-protocol.flexiarg) | G1/校 | Critical tasks require independent validation by second agent |

### G0: Evidence Durability

| Pattern | Sigil | Purpose |
|---------|-------|---------|
| [session-durability-check](session-durability-check.flexiarg) | G0/固 | Session reconstructable from persisted events alone |
| [par-as-obligation](par-as-obligation.flexiarg) | G0/省 | Every proof path produces a PAR before CLOCK_OUT |

## Dual Interpretation: Agency and Pattern Problems

Each coordination pattern has a dual reading — one through the lens of
agent problems, one through the lens of pattern problems. Both are
coordination problems; the gate is where they converge.

| Gate | Agency Reading | Pattern Reading |
|------|---------------|-----------------|
| G5 task-shape | Is the agent's task well-defined? | Is the task shaped so a pattern can guide it? |
| G5 mission-binding | Is the agent authorized by a mission? | Is the pattern's scope anchored to a mission? |
| G4 capability | Can this agent do this work? | Does this pattern apply to this domain? |
| G4 assignment | Is one agent responsible? | Is one pattern selected (not a muddle of three)? |
| G3 mandatory-psr | Has the agent committed to an approach? | Has a pattern been engaged with? |
| G3 search-protocol | How did the agent decide what to do? | How was the library queried? |
| G2 bounded-execution | Agent stays within budget | Pattern application stays within scope |
| G2 artifact-registration | Agent's outputs are attributed | Pattern's outputs are traceable |
| G1 mandatory-pur | Agent's work gets evaluated | Pattern's guidance gets evaluated |
| G1 cross-validation | Second agent reviews the first | Second pattern perspective checks the first |
| G0 durability | Agent's session is recoverable | Pattern use evidence is persistent |
| G0 par-obligation | Agent reflects on what happened | Pattern library learns from use |

This duality is structural, not accidental. Futon3 replaces "ants" with
"agents" and "pheromone" with "patterns" — coordination is the relationship
between agents and patterns, and the gates sit at the intersection.

## Relationship to Ancestors

These 12 patterns organize and subsume ~39 ancestral patterns from:
- `agency/` (8 patterns: identity, delivery, lifecycle, routing, ...)
- `agent/` (14 patterns: sense-deliberate-act, scope, evidence, ...)
- `fulab/` (11 patterns: clock-in, clock-out, pattern-dep, ...)
- `musn/` (3+ patterns: declare-scope, expensive-move-consent, ...)
- `sidecar/` (1 pattern: validation-enforcement-gate)

The ancestors remain in the library. They are evidence that the gates are
needed — each ancestor emerged from a real failure that a gate would have
prevented. The coordination patterns provide the structural enforcement
that the ancestors could only describe.

## Verification

The coordination patterns were verified two ways. This is the VERIFY step of
the derivation xenotype: IDENTIFY → MAP → DERIVE → ARGUE → **VERIFY** → INSTANTIATE.

**Method 1 — Structural:** Translate pattern conclusions into a typed directed
graph (`futon5/data/missions/coordination-exotype.edn`) with 6 inputs,
6 outputs, 9 components, 35 edges encoding the two-loop AIF structure.
Validate against 8 machine checks via `ct/mission.clj`.

**Method 2 — Empirical:** Trace git history of futon3 (~200 commits) and
futon3a (~30 commits) to check whether the theoretical tensions (E1-E7)
match the actual tensions from development. Manual version of what L1-observe
would do from the pattern graph in the future.

**Structural results:** All 8 checks pass. Key findings:

| Finding | Detail |
|---------|--------|
| Level 1 gap | Concrete diagram has all Level 0 roles but no L1-observe or L1-canon |
| Composability | Concrete O-evidence type-matches exotype I-tensions (Level 0 feeds Level 1) |
| I4 clean | No output→constraint path; glacial loop feedback passes through external environment |
| I3 governs | L1-canon → I-patterns is glacial→glacial (constitutional amendment, not wireheading) |
| I4 blind spot | Validator should add component→constraint check for multi-loop diagrams |

**Empirical results:** All 7 tensions (E1-E7) confirmed in git history, plus
an emergent finding: the structural cycle (runtime grows flat → escape valve
created → content flows back without structure → validation bolted on after
the fact). This cycle is what retroactive canonicalization predicts.

**Note on method:** The empirical verification was done by manual git
archaeology. In the future, the same verification should be traceable from
the pattern phylogeny graph — asking not "what commits exist?" but "which
ancestors were canonized, and does the genealogy trace back to real structural
tensions?" The git log tells a morphogenetic story without theoretical
salience; the pattern graph, if built carefully, would tell the same story
with typed edges and construction proofs.

The exotype diagram IS the specification. A concrete coordination system is
valid iff it projects onto this diagram preserving structure, types, timescale
separations, and invariants.

See: [ARGUMENT.flexiarg](ARGUMENT.flexiarg) §VERIFICATION and §EMPIRICAL
VERIFICATION for full details.

## Specification Reference

- **Abstract diagram (exotype):** `futon5/data/missions/coordination-exotype.edn`
- Concrete diagram (Level 0): `futon5/data/missions/futon3-coordination.edn`
- Static analysis (existing): `futon5/data/missions/futon3-agent-loop.edn`
- Theory: `library/futon-theory/INDEX.md`
- Mapping: `futon5/docs/gate-pattern-mapping.md`
- Argument: [ARGUMENT.flexiarg](ARGUMENT.flexiarg)
