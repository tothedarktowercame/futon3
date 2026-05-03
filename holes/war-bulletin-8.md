# War Bulletin 8: The Stack Argues With Itself, Then Demands Evidence

**Date:** 2026-04-24
**Context:** M-stack-geometry-anthology cluster + War Machine continued
  evolution. Single intensive session 2026-04-24, picking up after
  M-daily-scan and the Julia-demo trigger.
**Trigger:** A fresh attempt at writing a "holistic argument" prose
  document (`futon5a/holes/holistic-argument-aif2.md`) failed — it
  read no better than the original sketch. The diagnosis: prose was
  the wrong artefact. What was missing was a *machine-readable*
  argument that the stack could carry, project, and check against
  evidence. By turn's end, the stack does carry it, the demo speaks
  the same language, and the dominant conflict's bites have been
  put under empirical observation for the first time.

## The Arc

Each bulletin so far added a capability the loop needed. This one
turns the loop's *self-description* into a first-class data structure
and then makes that description testable against session evidence.

| Bulletin | What it added | What was still implicit |
|----------|--------------|------------------------|
| 1 (Feb 14) | Wiring diagrams operational | Strategic synthesis |
| 2 (Feb 18) | Evidence landscape | Cross-futon coordination |
| 3 (Feb 22) | Self-representing stack | Dynamic self-assessment |
| 4 (Feb 27) | Portfolio inference | Observation calibration |
| 5 (Mar 1)  | Multi-agent quality loop | Infrastructure resilience |
| 6 (Apr 10) | Inhabitation threshold | Operator's strategic view |
| 7 (Apr 13) | War Machine + daily scan | Stack's own argument as data |
| **8 (Apr 24)** | **AIF+ self-model + empirical bite check** | **Continuous re-derivation of the next move** |

## Findings

### Finding 1: The Stack Now Has a Machine-Readable Self-Model

`futon5a/holes/stories/THE-STACK.aif.edn` (307 lines) and 16
companion `leaf-*.aif.edn` files encode the holistic argument as
typed AIF+ data: claims, supports, attacks, falsifiability
conditions, preferences, would-refute / closes edges, and weighted
load-bearing conflicts. The stack is now *one thesis (S0), three
pillars (S1/S2/S3), one cycle (S4), one AIF substrate (S6), two
foundational substrates (S7/S8), one organising frame (S9), four
load-bearing conflicts (C1-C4)*, with every node tagged by its
:origin (the leaf and internal n-id it derives from).

This is not a diagram. It is data. It can be queried, projected,
diffed, and held accountable.

### Finding 2: The Compression Algorithm Picked Its Own Successor

The stack-level :reading :next-move field on THE-STACK.aif.edn is
not an editor's opinion; it is the output of a 3-criterion ranking
applied to the 16 leaf-level next-moves:

1. Direct leverage on the dominant conflict
2. Unlock cascade for downstream substrate
3. Self-referential closure of the algorithm itself

The winner — *step the Portfolio Inference AIF loop using THE-STACK
as the input observation* — closes the recursive gap: the tool for
picking next-moves IS the next-move. Joe asked for "the part that
picks next-moves picks itself." The AIF+ encoding made that
self-pick a one-line declaration the demo can render.

### Finding 3: The Self-Model Runs Inside the Stack, Not Beside It

`futon3c/src/futon3c/aif/stack_generator.clj` exposes
`/api/alpha/aif-stack/live`, which reads the cached EDN and overlays
*live* mission status from `mission-control-inventory` onto each
spine node. Each overlaid node carries `:live-status?` (true for
mission-derived, `:leaf-derived` for cached, false otherwise). A
drift detector script (`futon5a/scripts/detect_drift.clj`) compares
file mtimes against cached `:source-mtime` baselines so re-rendering
is gated on actual change.

Result: the holistic argument is no longer an artefact you read; it
is a peripheral the stack queries continuously.

### Finding 4: The Demo Now Speaks AIF+

The Web War Machine (`futon0/web/war-machine`) gained a fifth
view-mode `:aif-stack` that renders the spine (S0-S9) and conflicts
(C1-C4) as hex cells, with role-based color (thesis/pillar/cycle/
substrate/frame) and severity-based conflict fill. A "Recommended
Next Move" tile in the left panel reads `:reading :next-move`
straight off the live endpoint and renders the recommendation with
collapsible Why / Feeding-input / Alternatives sections. A
Show↔Hide toggle drives the detail box to the conflict the move
dis-bites.

A free-floating Audacity-style waveform in the toolbar plots
session activity over the visible window. Click-to-jump and
drag-to-select are wired; when a selection is active the tick loop
loops within it. The HUD's leading indicator is no longer an
abstract counter — it shows wall-clock playhead time, with `(N% of
selection)` when a selection is set.

17 Playwright e2e tests gate this surface. Every view mode renders
distinct content (no silent fallbacks); duplicate-key bugs that
produced ghost cells are blocked at the test level.

### Finding 5: The Inhabitation Gap Is Now Visually Measurable

This is the move that converts C1 from a logical assertion into an
*empirical* observation. The chain:

```
session step :repos      ──parse──> "devmap-<repo>" leaf name
spine .origin string     ──parse──> set of leaf names
                                       │
            (intersection lights up the spine cell)
                                       │
hot-spot pheromone + bite-edge stroke ──> "this bite just fired"
```

`war-machine.client.aif-join` (new namespace) does the join. In
:aif-stack mode the ant pheromone now lights up spine cells when
the corresponding repo activity is observed. A new `<g.bite-edges>`
SVG layer draws C→S lines: **solid red** (width/opacity scaling
with hit count) when the bitten spine has been touched in the
window, **dashed grey** when the bite is still purely logical.

Concrete reading from the 14d window today: of C1's three bites
(S6, S8, S4), only S8 has empirical pressure. S6 and S4 are still
grey-dashed. *That grey is the inhabitation gap, made visible* —
the parts of the spine the work isn't reaching.

C1 is no longer just an assertion. It is a measurement.

## WR Decisions

### WR-8: AIF+ Files Are Sources of Truth, Prose Is Regenerated

The 16 leaf + 1 stack AIF+ files at `futon5a/holes/stories/` are
the canonical self-model. Prose deliverables (`THE-STACK.md`,
`EXTERNAL.md`, `holistic-argument-aif2.md`) are *generated* from
them via Babashka renderers (`render_aif2_prose.clj`,
`render_leaf_prose.clj`, `render_external_prompt.clj`). The drift
detector treats prose as derived: regenerate on change, do not edit
in place. This closes the long-standing tension where prose drifted
from intent because there was no intent it had to track.

### WR-9: Bites Must Be Empirically Tested or Marked Logical

Conflict bites in the AIF+ model are claims of the form
*"this conflict damages this part of the stack."* Such claims are
purely formal until session evidence supports them. The empirical
join in the War Machine is the test. Conflicts whose bites remain
grey-dashed across multiple windows are candidates for downgrade
(weight reduction or removal). Conflicts whose bites turn solid red
are confirmed as load-bearing. The cached weight in the EDN is now
a *hypothesis*; the running War Machine carries the *posterior*.

### WR-10: The Next-Move Surface Is the Recursive Closure

The Recommended Next Move tile is not a UI affordance. It is the
algorithm Joe asked for, made continuous: read the stack's own
:reading :next-move, render it with full provenance, and let the
operator click into the conflict it dis-bites. As long as the AIF+
files are kept current (drift detector + re-render), the tile will
always recommend the move that the stack itself argues is highest
expected value. When the next-move stops being PI-step, it will be
because PI has been stepped and the stack's own evidence said so.

## Status

```
S0 (Thesis: stack maintains itself)        — STRENGTHENED
                                             (machine-readable model encoded)
S1 (Pillar I: argument operational)        — STRENGTHENED
                                             (16 leaves + stack meta now typed)
S2 (Pillar II: invariants)                 — STABLE
S3 (Pillar III: missions)                  — STABLE
S4 (Cycle: 5-step generative)              — STABLE
S5 (Self-representation substrate)         — STRENGTHENED
                                             (live endpoint + overlay)
S6 (AIF substrate: PI/HO/MC)               — UNCHANGED (PI step-count = 0)
                                             ← THE NEXT MOVE'S TARGET
S7 (Pattern canon)                         — STABLE
S8 (Durable storage / Arxana)              — STABLE
S9 (a-sorry-enterprise frame)              — STABLE

C1 (Inhabitation gap, weight 7)            — NOW EMPIRICALLY MONITORED
                                             1/3 bites observed in 14d window
C2 (Closure-evidence drift, weight 4)      — NOW EMPIRICALLY MONITORED
C3 (Weak generalisation evidence, weight 4)— NOW EMPIRICALLY MONITORED
C4 (Internal grasp ≠ external reach, w 3)  — NOW EMPIRICALLY MONITORED

A1 (Observation half-blind)                — FURTHER ADDRESSED
                                             (bite empiricality now observable)
A4 (Hermit trap)                           — UNDER TREATMENT (Julia demo today)
```

## What's Built (Concrete Inventory)

| Artefact | Path | Lines |
|----------|------|-------|
| Stack-level AIF+ meta | `futon5a/holes/stories/THE-STACK.aif.edn` | 307 |
| Leaf-level AIF+ files (×16) | `futon5a/holes/stories/leaf-*.aif.edn` | ~150 each |
| AIF+ live endpoint | `futon3c/src/futon3c/aif/stack_generator.clj` | 222 |
| HTTP handler | `futon3c/src/futon3c/transport/http.clj#handle-aif-stack-live` | — |
| Drift detector | `futon5a/scripts/detect_drift.clj` | — |
| Prose generators | `futon5a/scripts/render_*.clj` (4 scripts) | — |
| Client-facing prose | `futon5a/holes/EXTERNAL.md`, `THE-STACK.md` | — |
| Demo doc | `futon5a/holes/DEMO-julia.md`, `DEMO-julia-walkthrough.md` | — |
| Excursion log | `futon5a/holes/excursions/E-aif-stack.md` | — |
| Web War Machine — AIF+ view | `futon0/web/war-machine/src/war_machine/client/{hex,graph}.cljs` | — |
| Web War Machine — empirical join | `futon0/web/war-machine/src/war_machine/client/aif_join.cljs` | 130 |
| Web War Machine — waveform | `.../waveform.cljs` | 211 |
| Web War Machine — next-move tile | `.../core.cljs` | — |
| Web War Machine — tests | `futon0/web/war-machine/tests/*.spec.ts` (4 files, 17 tests) | — |
| Stack HUD self-description hook | `futon0/contrib/stack-hud-self-description.el` | — |

## What's Next (As the Stack Itself Argues)

The stack's :reading :next-move says: **step the Portfolio
Inference AIF loop using THE-STACK as the input observation.**
That single move dis-bites C1 at its load-bearing point (S6),
unlocks the capability-AIF pivot (S6→Hypergraph Operator), and
*continues* the recursive closure begun by this bulletin: PI
ranking the candidate next-moves makes the compression algorithm
a running peripheral instead of a one-off document.

When the next bulletin is written, the question to ask the War
Machine is: *did stepping PI turn the C1-bites red?*
