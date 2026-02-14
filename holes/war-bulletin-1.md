# War Bulletin 1 — What First Proof Proved About the Stack

**Date:** 2026-02-14
**Scope:** Cross-futon strategic assessment
**Trigger:** Completion of futon6 First Proof sprint; return to superpod preparation

## Context

On Feb 11-12, futon6 executed a sprint against the "First Proof" challenge
(10 open mathematical problems, 48-hour window). The sprint produced 70 typed
wiring diagrams, 6 correct solutions, 2 wrong answers, and 2 partial results.
This bulletin extracts the cross-futon strategic implications.

## Finding 1: Wiring Diagrams Are Operational Infrastructure

The sprint's most important discovery was meta-methodological. Typed wiring
diagrams with edge-level critique are not a documentation format — they are
the critical infrastructure that enables verification at scale.

**Evidence:** 10 proofs were generated in 95 minutes (~7 min/problem).
Verification took ~4 hours — a 1:16 generation-to-verification ratio. Wiring
diagrams with IATC edge types (assert, challenge, reform, clarify, query,
exemplify, reference, agree, retract) enabled node-level targeting of gaps,
catching confidence laundering and structural errors that global review missed.

**Specific catches:** P9's rank-1 witness bug (critical), P6's edge-vs-vertex
sparsification conflation (major), P3's cascade-vs-transposition error (major).
All found via edge-typed critique, not by rereading the proof.

**Implication for futon5:** The Feb 10 design decision — "connectives are wires,
not annotations" (commit `12bddef`) — is now empirically validated. The wiring
diagram framework is not theoretical scaffolding; it is the operational core.

## Finding 2: AIF+ Invariants Hold Under Load

The AIF+ method audit applied all six invariants to the sprint's meta-argument:

| Invariant | Status | Evidence |
|-----------|--------|----------|
| I1: Boundary integrity | Pass | 23 typed nodes, 27 edges, clear inside/outside |
| I2: Observe/action asymmetry | Pass | Evidence ledgers (observe) vs. metacognitive interrupts (act) separated |
| I3: Timescale separation | Pass | Per-commit events < design patterns < architectural decisions |
| I4: Preference exogeneity | Pass | BMI falsification honestly reported; no retroactive rewriting |
| I5: Model adequacy | Pass | 19 commit attestations ground the argument in verifiable git history |
| I6: Compositional closure | Pass | The metacognitive interrupt that the argument prescribes was itself exercised |

The generate/verify split maps exactly to I2 (observe/action asymmetry). The
multi-agent architecture (Joe coaching, Claude drafting, Codex verifying) works
because it respects this separation. Coaching occurs at the observe-to-propose
boundary, not mid-act.

**Implication for futon3c:** The peripheral model's observe/action split is
validated. The proof peripheral (just committed, `7d1a5d0`) should maintain this
separation: proof-load/dag-check/canonical-get are observations; cycle-advance
and ledger-upsert are actions.

## Finding 3: Negative Knowledge Transfers

When proof attempts failed, they produced structural theorems about *why*:

- **P4 SOS infeasibility:** 7 scripts trying Putinar certificates produced one
  theorem: "interior zeros where all constraints are strict structurally block
  Putinar certificates." Killed an entire approach cleanly.
- **P6 technique exhaustion:** 6 subsample-and-concentrate methods, all hitting
  the quadratic-vs-linear wall. Theorem: "this wall is structural to the
  technique class, not contingent on method choice."

These dead-end theorems compressed hours of exploration into transmissible
facts. This is negative pheromone in ant-colony terms — and it is *more
valuable* than positive results because it prevents redundant exploration.

**Implication for futon1a:** The evidence store must preserve failed routes
with their structural obstructions explicitly stated. Not just "P4 SOS didn't
work" but "P4 SOS is structurally blocked because [theorem]." The proof
peripheral's `failed-route-add` tool exists for this reason.

## Finding 4: The Depth Frontier

The two wrong answers (P1, P7) weren't reasoning failures — they were
domain-knowledge gaps:

- **P1 (Phi^4_3 measure shift):** Required regularity structures expertise.
  The Cameron-Martin argument is valid for GFF but assumes false equivalence
  (Phi^4_3 ~ GFF). The obstruction requires understanding renormalization
  constant divergence under shift.
- **P7 (lattice rational acyclic cover):** Required L-theory/Novikov conjecture
  expertise. Fowler's criterion works for finite CW complexes, but the upgrade
  to closed manifolds is not difficult — it is *mathematically impossible*.

Both followed the same pattern: constructive arguments biased toward YES,
missing deep structural theorems that make the answer NO.

**Implication for the stack:** The frontier of AI-assisted mathematics is not
reasoning capability but domain-knowledge integration. The path forward:
structured knowledge bases (nLab, Arxana) + systematic adversarial testing
(mandatory falsification cycles).

## Finding 5: Layer-Switching as First-Class Operation

The P6 breakthrough after 8 hours of cycling in linear algebra:

> Joe: "What kind of problem is this? How would you teach it to an undergrad?"
> Claude (15 min later): "The combinatorial layer. Turan + leverage + pigeonhole."
> Result: K_n proved with c=1/3 (tighter than official's universal c=1/42).

This coaching move — layer enumeration over gap closure — converted an open
problem into a solved one. It represents a qualitative shift from dispatch
("close gap X") to reframing ("what mathematical layer is this?").

**Implication for futon3a/3b:** Query should support layer-switching as a
first-class operation. Not just "return papers about barrier functions" but
"find analogies in the combinatorial layer." The pre-superpod reduction layers
design (`pre-superpod-reduction-layers.md`) already indexes problems by layer.

## Strategic Assessment: The nLab Validation Path

### The Problem with Going Straight to Superpod

The superpod pipeline (futon6, stages 1-7) processes 140K+ SE threads on a
GPU cluster. Stages 1-6 are functional. Stage 7 (thread-to-wiring) is designed
but unimplemented. Running Stage 7 on the superpod without first validating the
wiring diagram extraction on a corpus with known structure is premature.

### Why nLab First

nLab is the ideal validation corpus:

1. **Already available.** `/home/joe/code/nlab-content/` has 20,441 pages.
2. **Already partially processed.** `hyperreal.py` builds the free category
   (objects = pages, morphisms = wiki-links, enrichment = shared NER terms).
   `nlab-prevalidate.py` runs entity extraction, NER, and scope detection.
3. **Structurally rich.** nLab's wiki-link graph *is* a categorical wiring
   diagram. Pages about adjoint functors link to functors, natural
   transformations, units, counits — the links encode compositional structure.
4. **Ground-truth available.** nLab pages explicitly describe universal
   properties, adjunctions, exact sequences — formal structures with known
   wiring diagram representations. Topos Institute's Parmesan/MathFoldr work
   provides independent SVO triple extraction for validation.
5. **On-laptop scale.** 20K pages, ~390MB. No GPU required for extraction.
   ML validation (embeddings, clustering) feasible with local resources.

### What Exists vs. What's Missing

| Component | Status |
|-----------|--------|
| nLab corpus (20K pages) | Complete |
| Entity/relation extraction | Complete (`nlab-prevalidate.py`) |
| NER term spotting (19K terms) | Complete |
| Scope detection (7 binding patterns) | Complete |
| Free category construction | Complete (`hyperreal.py`) |
| Morphism enrichment (shared vocabulary) | Complete |
| Path queries (categorical composition) | Complete |
| Futon5 export | Skeletal (truncated, JSON not EDN) |
| Wiring diagram generation from category | Missing |
| Tensor embedding of diagrams | Missing (Spring Break target) |
| Quality validation loop | Missing |
| Persistence to futon1a | Missing |

### The Role of Arxana

Arxana (futon4) is the annotation layer that closes the loop. First Proof
showed that edge-level critique — not global review — catches confidence
laundering. Arxana's scholium-based architecture was designed for exactly this:
any text span links to any other with typed annotations. In the nLab context:

- Scholia on nLab pages become typed edges in the wiring diagram
- Cross-page annotations become morphism enrichments
- The hyperreal dictionary (nLab as free category) feeds into Arxana's
  link-graph persistence (M-arxana-graph-persistence, completed)

## Strategic Reordering

```
NOW        nLab wiring extraction (on-laptop)
           - Complete hyperreal.py → futon5 integration
           - Generate wiring diagrams from categorical structure
           - Validate against Topos/Parmesan ground truth

SPRING     futon5 JAX push (CA + differentiable embeddings)
BREAK      - Batched eigendecomposition on nLab diagram tensors
           - Learned manifold embedding (replace discrete Jaccard)
           - CA experiments with nLab-derived structural features
           - First Proof's 70 diagrams as calibration data

POST-      Superpod run (validated pipeline)
BREAK      - Stage 7 on 140K SE threads
           - Embedding space already trained on nLab
           - Quality metrics established from nLab validation

PARALLEL   futon3c proof peripheral (Codex, alleycat protocol)
           - Joint testing when Codex work is ready
           - Peripheral integration with proof DAG
```

## Independent Contributions Worth Noting

The sprint produced mathematical results that stand on their own:

- **P9:** Degree-3 polynomial test for quadrifocal tensor rank-1 detection
  (vs. official's degree-5). Potentially more efficient.
- **P10:** Improved preconditioner (spectral equivalence delta < 1 vs.
  delta in [5.2, 22.7]). Official commentary: "better than human solution."
- **P6 K_n:** c=1/3 (tighter than universal c=1/42 for complete graphs).
  Elementary proof (Turan + trace + pigeonhole).
- **P4 n=3:** Identity Phi_3 * disc = 18 a_2^2, complementary to official's
  Hessian approach. Case 3c closed via 6561 certified PHCpack paths.

## War Room Implications

This bulletin informs the following operational updates:

1. **Cross-Futon section** in war-room.md should reflect the futon5/6
   integration path via wiring diagrams.
2. **futon3c missions** should note that the proof peripheral is committed
   and awaiting Codex alleycat testing.
3. **WR-3 (exotype before implementation)** is reinforced: First Proof
   empirically validates that typed structure (wiring diagrams) before
   implementation (proofs) catches errors that post-hoc review misses.
4. **New coordination need:** nLab extraction is a cross-futon effort
   (futon6 scripts, futon5 tensor machinery, futon4 annotation layer,
   futon1a persistence). This may warrant a War Room decision (WR-4).
