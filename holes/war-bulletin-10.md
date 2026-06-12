# War Bulletin 10: The War Machine Works — and Reaches for a Real Manifold

**Date:** 2026-05-31
**Context:** Two weeks since bulletin-9 (2026-05-17). In that window the
  War Machine crossed from *built* to *demonstrated*: `M-war-machine-first-outing`
  closed 2026-05-30 (futon3c `24df6fc`) having driven a full mission lifecycle
  in one session and cleared an entire open-sorry queue under the WM's own
  ranking. The act of demonstrating it surfaced the deepest structural finding
  of the cycle — the WM ran *out of moves*, and tracing why exposed that the
  stack's "geometry" is a scalar field painted on a graph, not a manifold.
  This bulletin reads the demonstration and the gap it opened as one move, and
  marks a cross-mission / cross-futon crossing point.
**Trigger:** Joe's review of the first-outing close, his "that's a bit weird
  for an AIF model" observation (which seeded `futon2/holes/M-aif2.md`), and
  his frank pushback on the "thin scalar field" as the blocker for AIF-as-he-wants,
  M-differentiable-code, and a geometrized codebase.

## The Arc

| Bulletin | What it added | What was still implicit |
|----------|--------------|------------------------|
| 7 (Apr 13) | War Machine + daily scan operational | Does it actually drive work? |
| 8 (Apr 24) | AIF+ self-model + empirical bite check | Continuous re-derivation of next move |
| 9 (May 17) | Outward-facing operational layer + UKRN-S exemplar ported inward | Does the WM-pilot loop run, unattended, on a real queue? |
| **10 (May 31)** | **WM demonstrated end-to-end (queue cleared); its starvation traced to three nested gaps; the design that fixes them (aif2); and the geometry the whole stack now needs** | **The ground metric on substrate-2 — the single primitive four missions converge on** |

Bulletin-9 said *the stack writes its own job description and asks whether the
work is purchasable.* Bulletin-10 says *the stack ran its own pilot, cleared the
board, hit the floor — and the floor turned out to be the mathematics.*

## Findings

### Finding 1: The War Machine works — full lifecycle, queue cleared, zero fake-finishes

`M-war-machine-first-outing` closed 2026-05-30 (futon3c `24df6fc`). The WM-pilot
loop was demonstrated end-to-end: codex-2 piloted (claude-5 coaching by whistle),
driven unattended-across-turns by `/loop` + `ScheduleWakeup` + a background
whistle, and **cleared the entire open-sorry queue following the WM's ranking** —
4 discharges plus one honest decompose and one honest partial, **zero
fake-finishes, zero hard-halts**. The question the WB-7/8 arc left open ("is the
War Machine infrastructure that actually drives work, or a dashboard?") is
answered: it drives work, demonstrably, on a real queue, with earned-closure
discipline holding under unattended multi-turn operation.

### Finding 2: The deepest finding of the outing was a *limit*, not the success

The run was **100% bootstrapping, 0% outward** (`M-war-machine-first-outing-expectations.md`):
the queue exhausted into `:learn-action-class` *before any outward `:open-mission`
surfaced*. The WM was **starved of input sources, not done**. This is the
"weird for an AIF model — you'd think it has a built-in way to learn things"
observation that seeded `M-aif2`. A demonstrated success that *ran out of
substrate* is more informative than a mixed run would have been: the gap is the
finding.

### Finding 3: Three nested gaps sit behind the starvation

Tracing why the queue went dry (this session, 2026-05-31) found three, in
increasing depth:

1. **Tension *enriches*, it does not *generate*.** In `futon2.report.war-machine/judge`
   candidates come from exactly four proposers (bootstrap / pattern / mission /
   sorry); structural-pressure (ΔT from the geometric stack) is then applied by
   `enrich-candidates-with-structural-pressure` — and only to `:address-sorry`
   candidates (`war_machine.clj:248,271`). It is a **weight over an existing
   menu; it cannot put a dish on the menu.** So a manifold full of tension still
   yields nothing when the proposer registries are dry. (`E-g-incorporates-deltaT.md:12`
   confirms ΔT shipped as a "post-AIF tie-breaker" — narrow by design.)
2. **The action-class inventory is fixed *code*, enumerated in three places** —
   `forward-model/action-types` (a `def` set), the `predict-effects`/`can-propose?`
   defmulti arms, and the outer loop's hardcoded `classes`/`substrate-dispatch`.
   The WM cannot `conj` to its own support; extending it is a human code edit.
3. **`:learn-action-class` is blind to genuinely-missing kinds.** `gap-actions`
   iterates the *fixed* set, so the WM can say "enable the declared class
   `:fire-pattern`" but never "you lack a class for X." **The boundary is
   invisible to the agent** — even its self-model of capability gaps is bounded
   by its fixed support.

### Finding 4: `M-aif2` — endogenous niche construction — fixes all three with one self-similar primitive

`futon2/holes/M-aif2.md` was opened and driven IDENTIFY → VERIFY this session.
The design: **one `extensible-registry` primitive** (`{producer, credit (Beta,
the R12 apparatus ported up), admissibility (consent-gate), status}`)
instantiated at **three self-similar strata** — S3 targets (already data), S2
action-classes (make them data/hidden-state), S1 proposers (add a *tension-proposer*
that **generates** candidates from ∇T; make the proposer-set itself extensible).
**Meta-meta-learning** is the claim that S1 and S2 use the *identical* primitive,
so "learn-to-value-targets / install-action-classes / install-proposers" is one
mechanism at three heights; it terminates because the floor-raising operation is
the same at every level (recursion in *data*, not new code), gated by
admissibility. The basic `aif` library is preserved frozen as the reduced model
(`aif` = `aif2` with all registries frozen).

Two corrections landed alongside:
- **The niche-construction principle was scoped, not reversed.** Joe's pushback
  (Kiverstein & Miller's playfulness; the variational free-energy approach to
  niche construction) showed the AIF *meta-level is in the framework* — "an agent
  can't extend its own niche" is a property of our current architecture, not a
  theorem. The `library/aif/niche-construction.flexiarg` headline now carries
  that scope qualifier + an endogenous-niche path.
- **Tension's elegant wire:** resolving "what action does a tension propose?"
  gives a mapping `κ`(node-type → action-class) that is *itself a registry*;
  when `κ` routes to a class that doesn't exist, that absence **is** the
  candidate-class signal — Gap-1's false-floor mechanised into Gap-2's
  niche-construction trigger.

### Finding 5: The crossing point — the "thin scalar field" is a shared bottleneck, and the path is a *ground metric* on substrate-2

Doing `aif2` properly forces the question the stack has deferred since
`M-live-geometric-stack`: substrate-2 has a scalar `T: nodes → [0,1]` plus
discrete `ΔT` along edges — **a function on a graph, not a manifold.** None of
the objects rich AIF runs on (distance, tangent space, curvature, natural
gradient) exist. This same deficiency blocks three other missions:

- **`M-aif2`** — the tension-proposer needs a principled tension, not the
  ad-hoc signed-grad-vs-`(1−T)` the VERIFY spike exposed (Finding 6).
- **`M-differentiable-code`** (futon5, early pilot) — gradients need *continuous*
  data; notes to this effect are landing in that mission now.
- **`E-codebase-manifold`** (new follow-up, M-aif2 §6) — Joe's idea of animating
  the codebase as an evolving manifold (futons as attractor basins; the
  stereolithographic view with its time-axis restored). Its *reusable* core is
  not the animation but a robust manifold description.

The convergence: **all four need one missing primitive — a ground metric on
substrate-2** (some blend of dependency, git co-change, and semantic distance).
From it, the rest follows:
- **Curvature becomes rigorous and means the right thing.** *Ollivier–Ricci
  curvature* — `κ(x,y) = 1 − W₁(μ_x, μ_y)/d(x,y)`, defined via the **Wasserstein
  (earth-mover) metric** Rob flagged — is *discrete* (computable on the
  hypergraph today) and **negative on bridges/bottlenecks**: exactly the
  "high-curvature regions to ameliorate" of the WM-as-Turing-machine-on-a-manifold
  framing. Its continuum limit is Ricci curvature — the **discrete-and-continuous
  bridge** Joe wants ("both manifolds").
- **One geometry is already latent and unused.** The WM's R1 belief is a
  per-entity posterior over a 7-element status set — points on a simplex, i.e.
  *already a statistical manifold*; R7 precision is the Fisher-information shadow.
  Fisher–Rao (information geometry) is sitting there unexploited.

Honest scope: substrate-2's "geometry" is today a *notion*, not a Riemannian
object. Defining the ground metric **is the work**; the animation is its
demonstrator.

### Finding 6: A reusable VERIFY method was captured — `logic-model-before-code`

The first outing's VERIFY (`futon3c.logic.outing-invariants`, operator directive
"check the logic model before we write the code") was lifted into a stack-wide
pattern: `library/mission-coherence/logic-model-before-code.flexiarg` — verify a
design's invariants as a `core.logic` + pldb model over an *abstract adversarial
trace* (conforming witness ⇒ 0 violations; one adversarial trace per invariant ⇒
caught), *before* any harness code. Its **first reuse** was `M-aif2`'s VERIFY
Stage A (`futon3c/src/futon3c/logic/aif2_invariants.clj`, `run-verify` ⇒
`:verified? true`, 4/4 invariants), and it did what it promised: it verified the
design structurally and *shrank* the empirical spike (Stage B) to its irreducible
core — which is what surfaced the tension-polarity finding cleanly.

## WR Decisions

### WR-18: The War Machine is demonstrated, not hypothesised

The first outing closed with the queue cleared under the WM's own ranking,
unattended across turns, with earned-closure discipline intact. Bulletin-grade
status: the "is it real" question from WB-7/8 is answered yes. Future work
builds *on* a working WM, not *toward* one.

### WR-19: Tension must GENERATE, not only rank

A live geometric stack should *always give the WM something to do*. The current
wiring (ΔT as a sorry-only tie-breaker) cannot — it ranks an existing menu. The
fix is a **tension-proposer**: an S1 source that emits candidate actions at
high-tension regions of the manifold ("field drives the head"). This is the
proximate fix to the starvation and the first `aif2` build slice.

### WR-20: The action-class inventory becomes data, governed by one self-similar primitive

The support (action-classes) and the proposer-set move from code to an
`extensible-registry`; the *same* register/credit/admissibility machinery
governs every stratum (meta-meta-learning). **Admissibility is the
recursion-safety gate** — per-entry consent is what stops self-modification from
running away (generalising `feedback_admissibility_as_recursion_safety` to
per-stratum). The basic `aif` stays frozen as the reduced/reference model.

### WR-21: The geometrization upgrade is a shared cross-mission dependency — build the ground metric on substrate-2

The "thin scalar field" is deprecated as a *target* (kept as the v0 it always
was). The single primitive **a ground metric on substrate-2** sits *under*
`M-aif2`, `M-differentiable-code`, `M-live-geometric-stack`, and
`E-codebase-manifold`. Build it from dependency + git co-change + semantic
distance; derive curvature (Ollivier–Ricci, via the Wasserstein metric),
continuity (spectral/diffusion embedding), and the manifold reconstruction from
it. Exploit the two geometries the stack already gestures at: **Fisher–Rao**
(internal, latent in the WM belief simplex) and **optimal-transport / Wasserstein**
(external, over the codebase as a metric-measure space). This likely warrants its
own mission, with the four above depending on it.

### WR-22: `logic-model-before-code` is a sanctioned VERIFY method

Captured as `library/mission-coherence/logic-model-before-code` (+ index row),
distinct from the sibling `social/tension-before-code` (tests-first): verify the
*design* as an executable logic model over adversarial traces before code or its
tests exist. Carve out empirical/substrate-dependent guarantees for a spike — the
model tells you exactly which residue still needs one.

## Status

```
S0 (Thesis: stack maintains itself)        — STRENGTHENED
                                             (WM demonstrably cleared its own
                                              queue under its own ranking)
S2 (Pillar II: invariants)                 — STRENGTHENED
                                             (logic-model-before-code captured;
                                              mission-coherence namespace gains
                                              its first verify-discipline pattern)
S3 (Pillar III: missions)                  — STRENGTHENED
                                             (M-war-machine-first-outing CLOSED;
                                              M-aif2 IDENTIFY→VERIFY in one session)
S5 (Self-representation substrate)         — UPGRADE-NAMED
                                             (substrate-2's scalar field named as
                                              inadequate; ground-metric upgrade is
                                              WR-21)
S6 (AIF substrate)                         — DEMONSTRATED + LIMIT-FOUND + FIX-DESIGNED
                                             (WM works; starvation traced to 3
                                              nested gaps; aif2 extensible-registry
                                              designed + VERIFY-passed; geometry is
                                              the next substrate upgrade)
S7 (Pattern canon)                         — STRENGTHENED
                                             (niche-construction corrected;
                                              logic-model-before-code added;
                                              tension-as-candidate-source flagged)

A1 (Observation half-blind)                — NEW FACET NAMED
                                             (tension wired as ranking-not-
                                              generation is an observation→action
                                              gap distinct from the WB-4 sensor gaps)
WM-as-TM-on-manifold                       — THROUGH-LINE MADE CONCRETE
                                             (curvature ≈ tension; niche
                                              construction = reshaping the
                                              metric-measure space)
```

## What's Built / Moved (since bulletin-9)

| Artefact | Path | Status |
|----------|------|--------|
| **M-war-machine-first-outing** | `futon3c/holes/missions/M-war-machine-first-outing.md` (+ `-expectations.md`, `-wiring.edn`) | **CLOSED 2026-05-30 (`24df6fc`)** — full lifecycle, queue cleared |
| First-outing VERIFY logic model | `futon3c/src/futon3c/logic/outing_invariants.clj` | the pattern's origin instance |
| R12 dual-loop apparatus | `futon2.aif.intrinsic-values` + `wm_outer_loop.clj` | landed 2026-05-21 (per `futon-aif-completeness.md`) |
| **M-aif2** (endogenous niche construction) | `futon2/holes/M-aif2.md` | **IDENTIFY→VERIFY complete 2026-05-31; INSTANTIATE pending** |
| aif2 AIF+ meta-model (exotype) | `futon5/data/missions/aif2-exotype.edn` | **validated — `ct.mission/validate` 8/8** |
| aif2 VERIFY Stage-A logic model | `futon3c/src/futon3c/logic/aif2_invariants.clj` | **`:verified? true`, 4/4 invariants** |
| aif2 VERIFY Stage-B spike (read-only) | `futon3c/scripts/aif2_tension_spike.clj` | tension generates a candidate; false-floor→trigger fires; polarity finding logged |
| **`logic-model-before-code` pattern** | `futon3/library/mission-coherence/logic-model-before-code.flexiarg` (+ index) | **new — first mission-coherence verify-discipline flexiarg** |
| niche-construction correction | `futon3/library/aif/niche-construction.flexiarg` | scope qualifier + endogenous-niche path |
| **E-codebase-manifold** (follow-up excursion) | `M-aif2.md` §6 | recorded — codebase-as-evolving-manifold + robust manifold description |

## What's Next

The crossing point reorders the queue around the geometry, because building the
`aif2` tension-proposer on the thin scalar field would mean wiring the
niche-constructor to sense a shadow (WR-21):

1. **Decide the geometrization mission** — promote WR-21 to a scoped mission
   (working title `M-substrate-metric` or similar): define the ground metric on
   substrate-2; derive Ollivier–Ricci curvature; sketch the continuous embedding.
   `M-aif2`, `M-differentiable-code`, `M-live-geometric-stack`, `E-codebase-manifold`
   depend on it.
2. **Reconcile the tension-polarity convention** (the Stage-B finding): signed
   grad (`mission_delta_t`) vs `(1 − T)` (`war_machine`) — replace both with a
   curvature-grounded tension once the metric exists.
3. **`M-aif2` INSTANTIATE slice 1** — the tension-proposer as a credited +
   admissibility-gated S1 registry entry, *reading the upgraded metric* (or, if
   sequenced first, the v0 field with the polarity fixed and a noted upgrade path).
4. **Read M-differentiable-code's landing continuous-data notes** and confirm the
   shared-metric convergence before either mission hardens its own version.
5. **`E-codebase-manifold`** — once a metric exists, the animation is a
   W₂-mass-flow demonstrator and a year-in-review artefact.

When the next bulletin is written, the questions to ask are: *did the ground
metric get defined, and did curvature replace the scalar field as the WM's
tension source? did the aif2 tension-proposer ship and stop the WM starving? did
the four missions actually share one metric, or did they fork? and did the
geometry buy the AIF the stack always wanted — natural-gradient free-energy
descent over a real statistical manifold, instead of a number painted on a node?*
