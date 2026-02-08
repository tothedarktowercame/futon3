# Evidence: futon1 Git History Lessons for futon1a

This document captures recurring bug patterns, fix patterns, implicit invariants, and anti-patterns observed in futon1 history. It is the evidence base for futon1a’s pattern-driven design.

## Owner

Codex

## Scope

### Scope In

- Extract recurring bug patterns, fix patterns, implicit invariants, and anti-patterns from futon1 history.
- Record concrete evidence (commit hashes + short diff notes) that informs futon1a design.
- Produce a pattern-evidence map usable for PSR/PUR traceability.

### Scope Out

- Implementing futon1a itself (covered by `M-futon1a-rebuild.md`).
- Exhaustive archaeology of every futon1 commit (focus on load-bearing clusters).

## Time Box

1 day (bounded extraction; split if it expands).

## Exit Conditions

- Evidence doc is complete enough to drive futon1a Part I mapping and has been reviewed by the other agent/human.
- If additional archaeology is needed, stop and create a follow-up mission with a narrower target (specific subsystem or timeframe).

## Recurring Bug Patterns (Observed)

- **Durability drift and watchdog ordering**
  - Evidence: 0e2b3a5, 5c51506, 644ea0a, bd6db70
  - Lesson: XTDB durability and watchdog sequencing broke often enough to warrant repeated fixes and enforcement.

- **Identity and duplicate entities**
  - Evidence: d1d447d, 1bda8f7
  - Lesson: Ambiguous external IDs and non-UUID identifiers led to duplicates and needed explicit uniqueness enforcement.

- **Open-world ingest fragility**
  - Evidence: 7b46312, 99d54c4
  - Lesson: Ingest pathways for open-world data needed repeated hardening and repair scripts.

- **Auth/penholder boundary drift**
  - Evidence: 7efbef7, 65349e7, e1e2c9d
  - Lesson: Authorization checks were easy to bypass or misapply without strict boundary checks.

- **Mirror integrity/metadata loss**
  - Evidence: fb05441, 50383dd
  - Lesson: Mirrored XTDB data lost metadata or drifted, requiring tests and guidance updates.

## Fix Patterns (Observed)

- **Explicit invariants and hard failures**
  - Evidence: 5c51506, 5227d75
  - Lesson: Invariant enforcement and explicit failure paths stabilized correctness.

- **Startup diagnostics and gating**
  - Evidence: 884df26, dc3cb55
  - Lesson: Startup checks and diagnostics catch mis-sync and configuration issues early.

- **Repair and backfill scripts as operational reality**
  - Evidence: 7b46312, c5ffd75
  - Lesson: The system repeatedly needed repair/backfill workflows for real data.

## Implicit Invariants (Extracted)

- **Durability-first**
  - Evidence: 0e2b3a5, 5c51506, 5227d75
  - Lesson: Writes must be durable before success; durability checks are non-optional.
  - futon1a implication: Layer 0 must block on durable commit and fail loudly.

- **Single-source identity / uniqueness**
  - Evidence: d1d447d, 1bda8f7
  - Lesson: External ID ambiguity leads to duplicate entities and long-term drift.
  - futon1a implication: One UUID per entity; unique external-id mapping enforced pre-write.

- **All-or-nothing startup**
  - Evidence: 884df26, c5ffd75
  - Lesson: Partial rehydration/repair introduces hidden inconsistency.
  - futon1a implication: Rehydration must fully succeed or fail with diagnostic detail.

- **Layered error responsibility**
  - Evidence: 7efbef7, e1e2c9d, a525acb
  - Lesson: Auth and invariants need explicit guards to prevent silent bypass.
  - futon1a implication: Errors must surface at the layer that caused them.

- **Rapid debugging requirement**
  - Evidence: 884df26, 50383dd
  - Lesson: Tests + diagnostics reduced drift and made failures actionable.
  - futon1a implication: Traceable error context + tests proving invariants.

## Anti-Patterns (Inferred)

- **Silent leniency in durability or identity**
  - Evidence: repeated enforcement commits (0e2b3a5, 5c51506, d1d447d)
  - Anti-pattern: “Accept then fix later” behavior leads to repair debt.

- **Mixed write paths and ad hoc repair**
  - Evidence: 7b46312, c5ffd75
  - Anti-pattern: Multiple entry points and late repair scripts indicate inconsistent write contracts.

- **Auth checks applied inconsistently**
  - Evidence: 7efbef7, 65349e7, e1e2c9d
  - Anti-pattern: Authorization logic scattered across routes or writers leads to bypass risk.

## Tension Survey (Git Evidence)

The following tensions are extracted from futon1 diffs and clustered by the recurring conflicts they expose.

1. **Durability vs throughput**
   - Evidence: 0e2b3a5, 5c51506, 644ea0a, bd6db70
   - Diff notes: Added XTDB durable proof and watchdog logic; follow‑up commits fixed watchdog ordering and state assumptions.
   - Lesson: Durable confirmation must be a hard gate, and watchdogs need deterministic ordering to avoid false confidence.

2. **Availability vs integrity at startup**
   - Evidence: 884df26, c5ffd75
   - Diff notes: Startup diagnostics and XTDB sync were added to avoid stale state; cleanup scripts and fixes were needed to pass invariants.
   - Lesson: Startup must block on integrity, even if it delays availability.

3. **Identity flexibility vs uniqueness**
   - Evidence: d1d447d, 1bda8f7
   - Diff notes: Enforced UUID IDs, re‑mapped external IDs, and rejected duplicate external‑id usage.
   - Lesson: Accepting non‑UUID IDs or ambiguous externals creates long‑term drift.

4. **Open‑world ingest velocity vs validation**
   - Evidence: 7b46312, 99d54c4, 5e833b9
   - Diff notes: Hardened ingest logic, fixed conditional paths, added repair scripts, and ensured open‑world fields are mirrored into XTDB.
   - Lesson: Open‑world ingest needs strict validation and repair tooling to stay coherent.

5. **Mirroring speed vs consistency**
   - Evidence: fb05441, 50383dd
   - Diff notes: Fixed metadata loss in XTDB mirroring and added mirroring tests/guidance.
   - Lesson: Mirroring must be tested as a first‑class invariant, not assumed.

6. **Authorization guardrails vs internal tooling**
   - Evidence: 7efbef7, e1e2c9d, 65349e7, a525acb
   - Diff notes: Added penholder header support, enforced guards on internal writers, restricted valid penholders, and added charon guard paths.
   - Lesson: Internal tools must obey the same auth guards or they become bypass vectors.

7. **Invariant enforcement vs operational repair**
   - Evidence: 5227d75, c5ffd75
   - Diff notes: Expanded invariant enforcement and later added cleanup/backfill scripts to restore compliance.
   - Lesson: Strong invariants are necessary, but the system needs explicit repair pathways.

8. **Schema evolution vs contract stability**
   - Evidence: 773394f
   - Diff notes: Added model registry, descriptors, and open‑world invariants across API and graph memory.
   - Lesson: Schema evolution must be formalized to avoid compatibility drift.

9. **Determinism vs feature expansion**
   - Evidence: 7611ea2, e1301f3
   - Diff notes: Fixed determinism gate baselines and resolved ingestion errors while adding pattern embeddings.
   - Lesson: New features must preserve determinism gates or be isolated behind tests.

## Diff Evidence Notes

- **0e2b3a5**: Added XTDB durable proof and watchdog loop (`apps/graph-memory/src/app/store.clj`, `apps/graph-memory/src/app/store_manager.clj`).
- **d1d447d**: Enforced UUID‑only IDs and external‑id uniqueness checks in store (`apps/graph-memory/src/app/store.clj`).
- **7b46312**: Added open‑world repair script and hardened ingest path (`apps/open-world-ingest/src/open_world_ingest/storage.clj`, `apps/graph-memory/src/scripts/open_world_repair_entities_xtdb.clj`).
- **5e833b9**: Ensured open‑world fields mirrored into XTDB (`apps/graph-memory/src/app/store.clj`).
- **884df26**: Fixed XTDB sync and added startup diagnostics (`apps/graph-memory/src/scripts/model_certify.clj`, `apps/graph-memory/src/app/store_manager.clj`).
- **fb05441 / 50383dd**: Fixed XTDB mirror metadata loss and added mirroring tests (`apps/basic-chat-demo/src/app/store.clj`, tests).
- **7efbef7 / e1e2c9d / 65349e7 / a525acb**: Hardened penholder and charon guard paths across handlers and writers.
- **5227d75 / c5ffd75**: Expanded invariants and added cleanup/backfill scripts for compliance.
- **773394f**: Introduced model registry + open‑world invariants and related tooling.
- **7611ea2 / e1301f3**: Fixed determinism gate baselines and ingest errors during feature expansion.

## Pattern Evidence Map

```
Pattern: stack-coherence/durability-first
Evidence: commits 0e2b3a5, 5c51506, 5227d75
Lesson: durability had to be explicitly enforced and repeatedly fixed
futon1a implication: block on durable commit, fail loud

Pattern: code-coherence/single-source-of-truth
Evidence: commits d1d447d, 1bda8f7
Lesson: external-id ambiguity creates duplicate entities
futon1a implication: unique (source, external-id) mapping, pre-write reject

Pattern: stack-coherence/all-or-nothing
Evidence: commits 884df26, c5ffd75
Lesson: startup/rehydration must gate on full integrity
futon1a implication: rehydrate all or throw; no partial load

Pattern: code-coherence/error-hierarchy
Evidence: commits 7efbef7, e1e2c9d, a525acb
Lesson: auth/invariant guardrails need explicit boundary checks
futon1a implication: errors surfaced at the layer that caused them

Pattern: stack-coherence/rapid-debugging
Evidence: commits 884df26, 50383dd
Lesson: diagnostics and tests reduced drift and clarified failures
futon1a implication: make bugs diagnosable in minutes
```

## Notes

- Evidence above includes diff-level notes for the main storage tensions. Expand with additional commits or issue references if deeper provenance is needed.
- Next step: tie each tension explicitly to futon1a module boundaries and add PSR/PUR scaffolds.
