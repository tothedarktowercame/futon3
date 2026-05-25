# Mission-Coherence Pilot Worksheet

Date: 2026-05-11
Analyst: Codex, under Joe's direction
Purpose: test whether mission-pattern mining operations produce a useful
issue-pool on a small but rich pilot corpus before widening to a full
first pass.

## Pilot corpus

1. `futon3c/holes/missions/M-war-machine.md`
2. `futon3/holes/missions/M-live-geometric-stack.md`
3. `futon3c/holes/missions/M-mission-peripheral.md`

These three were chosen because they are already the aggregation surfaces
named in `M-mission-coherence-patterns.md` and they represent distinct
mission styles: strategic, geometric, and machinic.

## Worksheet shape

Each extracted row records:

- `row-id`: stable pilot identifier
- `mission`: source mission
- `kind`: problem, solution, invariant, operating claim, or relation
- `cluster`: normalized issue family
- `issue`: the extracted issue in normalized wording
- `evidence-mode`: explicit self-description, repeated structure,
  failure mode, relation-structure, or completion criterion
- `candidate`: whether the cluster currently looks like a candidate
  mission-coherence pattern

## Normalized clusters

- `C1` live-current surface
- `C2` legibility / inspectability
- `C3` evidence beyond tests or prose snapshots
- `C4` explicit cycle or phase discipline
- `C5` queryable source of truth
- `C6` cross-mission / cross-stack coordination
- `C7` boundary / containment discipline
- `C8` abstract schema / generalizable form

## Extracted issues

| row-id | mission | kind | cluster | issue | evidence-mode | candidate |
|---|---|---|---|---|---|---|
| WM-01 | War Machine | problem | C1 | Manual strategic synthesis drifts from reality; mission state must be continuously synthesized rather than periodically narrated. | explicit self-description | yes |
| WM-02 | War Machine | solution | C2 | Mission inventory needs to be rendered as a legible triage signal, not just a pile of mission files. | explicit self-description | yes |
| WM-03 | War Machine | invariant | C5 | Strategic assessment should be produced from live evidence and inventory surfaces, not from manually maintained summary docs. | explicit self-description | yes |
| WM-04 | War Machine | solution | C1 | A mission/peripheral should become an always-available operating surface rather than a one-off report. | completion criterion | yes |
| WM-05 | War Machine | operating claim | C4 | Mission work is organized as an observe/infer/act/evaluate loop, even when the loop is described at a strategic level. | explicit self-description | yes |
| WM-06 | War Machine | relation | C6 | Missions are not isolated units; they are coordinated through sibling, parent, and source relations inside a larger operational body. | relation-structure | yes |
| WM-07 | War Machine | invariant | C3 | Mission outputs should stay close to evidence flow and performance constraints, so the operator can use them in live work. | completion criterion | yes |
| LGS-01 | Live Geometric Stack | invariant | C5 | A mission's deliverable is not "re-runnable derivation" but an authoritative, queryable substrate that withstands dispute. | explicit self-description | yes |
| LGS-02 | Live Geometric Stack | problem | C1 | Liveness cannot be aspirational; a mission fails if its claimed structure is not continuously current. | failure mode | yes |
| LGS-03 | Live Geometric Stack | invariant | C8 | Mission output should be shaped abstractly enough to span multiple domains and codebases without per-domain exceptions. | explicit self-description | yes |
| LGS-04 | Live Geometric Stack | solution | C5 | Derived quantities and mission-relevant readings should come from a common structured substrate rather than ad hoc side pipelines. | explicit self-description | yes |
| LGS-05 | Live Geometric Stack | relation | C6 | Missions, patterns, evidence, docs, and code belong to one connected substrate rather than separate descriptive worlds. | explicit self-description | yes |
| LGS-06 | Live Geometric Stack | failure mode | C7 | Completion claims must survive contact with current state; otherwise the mission exhibits completion rot and needs explicit detection. | failure mode | yes |
| LGS-07 | Live Geometric Stack | solution | C2 | A common substrate should support multiple surfaces (HUD, browser, zapper) without rewriting the underlying mission knowledge. | explicit self-description | yes |
| MP-01 | Mission Peripheral | problem | C4 | Ad hoc mission discipline is not enough; missions need an explicit cycle machine with gated phases and required outputs. | explicit self-description | yes |
| MP-02 | Mission Peripheral | invariant | C3 | Mission evidence includes design decisions, corpus checks, framing validations, and reasoning context, not just test results. | explicit self-description | yes |
| MP-03 | Mission Peripheral | solution | C5 | Mission obligations should be first-class structured records in the evidence landscape, not only prose in markdown. | explicit self-description | yes |
| MP-04 | Mission Peripheral | solution | C2 | Mission work should become inspectable through automatic tagging and phase-aware evidence, not only after-the-fact explanation. | explicit self-description | yes |
| MP-05 | Mission Peripheral | operating claim | C8 | A mission is an instance of a more general loop and grammar, not a one-off project-management artifact. | explicit self-description | yes |
| MP-06 | Mission Peripheral | invariant | C7 | When a mission step hits a boundary, the correct move is to park and contain it with a recorded blocker rather than force continuation. | explicit self-description | yes |
| MP-07 | Mission Peripheral | relation | C6 | Mission state and mission evidence should share one canonical store so that cross-mission learning becomes queryable. | explicit self-description | yes |

## Immediate counts

- Raw extracted issue rows: 21
- Distinct normalized clusters: 8
- Clusters with support from at least 2 missions: 8
- Clusters with support from all 3 missions: 4 (`C2`, `C3`, `C5`, `C6`)

## Pilot notes

- This worksheet intentionally uses one normalized issue per row rather
  than sentence-by-sentence extraction. It tests whether the operation is
  legible and repeatable before scaling the density.
- The pilot is sufficient to show recurrence exists across mission
  styles.
- The pilot is not sufficient to prove the full library yet; it only
  shows that the extraction operation produces coherent clusters rather
  than noise.
