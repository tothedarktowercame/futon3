# Paper-Shapes Index

**Date:** 2026-04-27
**Owner mission:** `futon3/holes/missions/M-pattern-application-diagnostic.md`
**Sister mission:** `futon6/holes/missions/M-superpod-mark3.md` (R-2 deliverable)
**Source pilot:** `futon3/holes/excursions/E-math-prototype-pilot.md`

## Purpose

A hierarchical taxonomy of *paper-shape patterns* — patterns that
classify what kind of contribution a mathematics paper makes — usable
as the choice space for the arxiv-aware Stage 3 prompt in
`superpod-mark3` and as a hand-tagging vocabulary for operator-level
analysis.

Five **family parents** (in `math-strategy/`) organize the
existing math-informal pattern library plus genuinely new leaves
(in `math-informal/`) into a coarse-then-fine taxonomy. Coarse
(family) is robust to LLM uncertainty and useful as a session-level
classifier; fine (leaf) carries the precise analytic content.

This file is the operator-readable index. The arxiv-aware Stage 3
prompt should consume the same taxonomy structurally — typically by
loading the family parents and member-pattern leaves directly from
disk, or via a derived EDN if a future operational reason demands it.

## The taxonomy

### 1. Existence Result `math-strategy/existence-result`

> Your paper shows that an object of a desired class exists.

| Leaf                                          | Where it lives                                        | Depth                |
|-----------------------------------------------|-------------------------------------------------------|----------------------|
| `construct-an-explicit-witness`               | math-informal/                                        | constructive         |
| `construct-auxiliary-object`                  | math-informal/                                        | constructive (indirect) |
| `use-probabilistic-method`                    | math-informal/                                        | pure-classical / non-constructive |
| `numerical-scout`                             | math-informal/                                        | with-error-bounds    |

Pilot exemplars: arxiv-2604.21888v1 (Kneser graph Hamiltonicity),
arxiv-math/0603597v1 (Gevrey ultardistributions),
arxiv-2604.20840v1 (Z/2-harmonic forms).

### 2. Characterization Result `math-strategy/characterization-result`

> Your paper describes what an object or class *is*.

| Leaf                                          | Where it lives           | Subtype                      |
|-----------------------------------------------|--------------------------|------------------------------|
| `exhaustion-as-theorem`                       | math-informal/           | full / classification        |
| `structural-obstruction-as-theorem`           | math-informal/           | failure-mode (method-side)   |
| `split-into-cases`                            | math-informal/           | dichotomy / classification   |
| **`structural-characterization`** (new)       | math-informal/           | full description             |
| **`failure-mode-characterization`** (new)     | math-informal/           | failure-regime (theorem-side)|
| **`complexity-classification`** (new)         | math-informal/           | complexity-theoretic         |

Pilot exemplars: arxiv-2604.20815v1 (Zarankiewicz dichotomy),
arxiv-2604.20827v1 (entropic OT failure-mode),
arxiv-2604.20787v1 (Exchange-number complexity),
arxiv-math/0603592v1 (KMS classification),
arxiv-math/0603593v1 (AFD outer-action classification).

### 3. Structural-Relation Result `math-strategy/structural-relation-result`

> Your paper relates two structures.

| Leaf                                          | Where it lives           | Direction        |
|-----------------------------------------------|--------------------------|------------------|
| `transport-across-isomorphism`                | math-informal/           | symmetric / via  |
| `find-the-right-abstraction`                  | math-informal/           | generalization   |
| `reduce-to-known-result`                      | math-informal/           | cross-domain     |
| **`structural-inclusion`** (new)              | math-informal/           | directional      |
| **`structural-equivalence`** (new)            | math-informal/           | bidirectional    |

Pilot exemplars: arxiv-2604.20808v1 (Coxeter cohomology equivalence),
arxiv-2604.20785v1 (Thurston norm inclusion),
arxiv-2604.20818v1 (Coburn → edge-modes bridge),
arxiv-2604.20778v1 (infinite matroid generalization),
arxiv-math/0603598v1 (Noether to fractional control).

### 4. Property-of-Object Result `math-strategy/property-of-object-result`

> Your paper establishes a property of a fixed object.

| Leaf                                          | Where it lives           | Property class       |
|-----------------------------------------------|--------------------------|----------------------|
| `estimate-by-bounding`                        | math-informal/           | bounds               |
| `monotone-approximation`                      | math-informal/           | asymptotic           |
| `pass-to-a-subsequence`                       | math-informal/           | convergence          |
| `regularity-and-asymptotics` (placeholder)    | math-informal/ (future)  | PDE-flavoured        |
| `rigidity-from-decay-conditions` (placeholder)| math-informal/ (future)  | analytic-rigidity    |

Pilot exemplars: arxiv-2604.20798v1 (PDE solution regularity),
arxiv-2604.20794v1 (Hardy uncertainty / rigidity).

The two placeholder leaves are deliberately unwritten in v0 — the
pilot only saw 2 papers in this family, both PDE-domain-shaped.
Future hand-tagging extensions are expected to populate them as the
domain-specific tail accretes.

### 5. Clarification (meta-tag) `math-strategy/clarification-meta`

> Not a strategic pattern. Marks papers where the
> `(context, tension, move)` triple cannot be meaningfully decomposed.

Two cases:

- `:single-axis-clarification` — paper is genuinely single-axis
  (clarifies one principle, surveys, extends a definition).
- `:extraction-failure` — slot extractor failed; producer bug to
  fix, not a paper property.

Pilot exemplars: arxiv-2604.20820v1 (S-Prime principle),
arxiv-2604.20788v1 (E-measure),
arxiv-math/0603594v1 (Galois group rings).

## Distribution observed in the pilot (24 tagged papers)

```
characterization        8 papers   (33 %)
structural-relation     6 papers   (25 %)
existence               3 papers   (13 %)
clarification (meta)    3 papers   (13 %)
property-of-object      2 papers   (8 %)
property tail / unique  2 papers   (8 %)
```

Characterization + structural-relation cover **58 %** of the corpus.
Existence is smaller than colloquial intuition suggests; property-of-
object is the smallest family with strong domain specialization.

## Use as a prompt choice space

The arxiv-aware Stage 3 prompt (R-1 of M-superpod-mark3) consumes
this taxonomy as follows:

1. Load all five family parents and their member leaves.
2. Present the LLM with both family-level options and leaf-level
   options.
3. Require the LLM to emit *both* a family tag and a leaf tag,
   plus a brief rationale.
4. If the LLM is uncertain at the leaf level, emit only the family
   tag plus a `:leaf-uncertain` flag — preserves coarse signal
   without forcing premature precision.
5. If the LLM finds the triple collapsed, emit
   `:family clarification-meta` with `:reason
   {single-axis|extraction-failure|other}`.

This consumption pattern is **prototype 1** of the choice-space
shape; Stage 3 prompt design (R-1) should also use it as the
schema check for output validation.

## Open additions (post-v0)

- Domain-specific leaves under property-of-object: regularity,
  rigidity, decay, stability — accrete as the hand-tagging extends.
- Leaf-level cross-references to formal patterns in
  `math-formalization/` once the formal/informal bridge is built
  (separate excursion).
- A derived `.edn` mirror at `futon6/data/arxiv-proof-patterns.edn`
  if the runner finds disk-load-and-parse over the per-file
  flexiargs unergonomic — *only if* that genuinely adds convenience
  rather than confusion. v0 leaves the flexiargs as canonical.

## Cross-references

- `M-pattern-application-diagnostic.md` (futon3) — IFR + typed-slot
  schema this taxonomy realises.
- `M-superpod-mark3.md` (futon6) — R-2 deliverable consumed here.
- `E-math-prototype-pilot.md` (futon3) — the 25-paper hand-tagging
  this taxonomy crystallises.
- `library/math-informal/INDEX.md` (futon3, future) — flat list of
  proof-strategy patterns; this file extends with the family
  hierarchy.
