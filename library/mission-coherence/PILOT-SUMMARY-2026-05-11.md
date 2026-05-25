# Mission-Coherence Pilot Summary

Date: 2026-05-11
Corpus: `M-war-machine.md`, `M-live-geometric-stack.md`,
`M-mission-peripheral.md`
Companion worksheet:
`futon3/library/mission-coherence/PILOT-WORKSHEET-2026-05-11.md`

## Question

Can we perform mission-pattern mining on a reasonably-sized corpus, or
does the extraction process collapse into one-off readings and manual
overhead?

## Short answer

Yes, the operation is viable.

The three-mission pilot produced a coherent issue-pool, clear
cross-mission recurrence, and a workable worksheet shape. The main limit
is not that mining fails; it is that a full library will need either a
larger corpus or denser extraction than this pilot used.

## What the pilot actually did

The pilot treated each mission as a source of extractable issues in the
broad sense used by the method note: problems, decisions, invariants,
failure modes, operating claims, and relation-structures.

For each mission, the pass:

1. extracted normalized issues,
2. grouped them into recurring clusters,
3. marked the evidence mode that justified each issue, and
4. checked whether clusters recurred across multiple mission styles.

The resulting worksheet contains:

- 21 raw issue rows
- 8 normalized clusters
- 8 clusters supported by at least 2 missions
- 4 clusters supported by all 3 missions

## Most important recurring candidates

1. Mission artifacts want to be live operating surfaces, not static
   prose snapshots.
2. Mission value depends on inspectability and legibility, not just on
   private operator understanding.
3. Mission evidence is richer than tests or status prose; it includes
   reasoning context, structure, and live state.
4. Missions tend toward a queryable source of truth rather than
   free-floating markdown descriptions.
5. Missions are coordination objects across other missions, codebases,
   and evidence surfaces.
6. Boundary handling matters: good mission structure records blockers and
   contains them instead of forcing fake progress.
7. Mission form appears generalizable: these docs repeatedly treat a
   mission as an instance of a larger machine, loop, or abstract
   substrate.

## What this proves

- A small pilot corpus is enough to test the mining operation itself.
- Rich mission documents do in fact yield reusable normalized issues.
- Recurrence appears across strategically different mission types, which
  is exactly what this mission needs if it is going to derive
  cross-cutting mission-coherence patterns.

## What this does not yet prove

- That the current corpus size is enough for the final pattern library.
- That the extracted clusters are already sharp enough to be promoted
  directly into `flexiarg` patterns without a wider pass.
- That every important mission pattern is visible from aggregation
  surfaces alone.

## Feasibility judgment

The mining operation is feasible, but the full first pass should be
slightly more ambitious than this pilot.

Two acceptable continuations:

1. Keep the current worksheet shape and expand to the upper end of the
   planned corpus, around 10-12 rich missions.
2. Keep the corpus size near 8-10 missions, but extract more densely
   within each mission so the issue-pool is materially larger.

Given the method note's warning that fewer than roughly 150 issues is a
weak data pool, the safest reading is:

- 3 missions are enough to validate the operation.
- 8 missions may be enough for a provisional library if extraction is
  dense.
- 10-12 rich missions is a better target if we want the first serious
  pattern set to stand on something stronger than anecdote.

## Recommendation

Proceed with the mining mission.

Use this pilot worksheet shape as the base format, and widen next to a
10-12 mission corpus centered on the same aggregation surfaces plus their
most directly connected missions.
