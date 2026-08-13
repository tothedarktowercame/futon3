# Proposed split of the 77 math patterns — FOR REVIEW, nothing moved

**claude-2, 2026-08-13, at Joe's request. This is a manifest to be ruled on,
not an executed change. No file has been moved and no header edited.**

Two cuts, in order:

1. **By KIND** — universal heuristic / subject-specific / meta-process /
   formalization. This is the cut that matters for retrieval, because the
   meta patterns match on process vocabulary that appears in *every* problem
   and so pollute every query.
2. **By SUBJECT** — arXiv `math.XX`, per Joe's scheme.

## Cut 1 — by kind

| kind | count | where it goes |
|---|---:|---|
| universal heuristic | 24 | `math-informal/` (stays) |
| subject-specific informal | 15 | `math-informal-XX/` |
| meta / proof-process | 20 | `math-strategy/` (its real job) |
| formalization, general | 6 | `math-formalization/` (stays) |
| formalization, subject | 12 | `math-formalization-XX/` |

## `math-informal/` — universal, stays put (24)

argue-by-contradiction · check-the-extreme-cases · construct-an-explicit-witness ·
construct-auxiliary-object · dualise-the-problem · exploit-symmetry ·
failure-mode-characterization · find-the-right-abstraction ·
induction-and-well-ordering · local-to-global · parametric-tension-dissolution ·
quotient-by-irrelevance · reduce-to-known-result · separate-into-independent-pieces ·
split-into-cases · structural-characterization · structural-equivalence ·
structural-inclusion · the-diagonal-argument · transport-across-isomorphism ·
try-a-simpler-case · unfold-the-definition · work-examples-first ·
construct-an-explicit-witness

Rationale: each applies in any subject. `failure-mode-characterization`
("characterize where a result FAILS") and `parametric-tension-dissolution`
("two parts conflict on a parameter — change the setup") read as meta but are
genuine mathematical moves, so they stay.

## Cut 2 — subject moves out of `math-informal`

### → `math-informal-CA` (Classical Analysis and ODEs) — 6

| pattern | why |
|---|---|
| `epsilon-of-room` | Tao's phrase; pure CA idiom |
| `pass-to-a-subsequence` | compactness extraction |
| `monotone-approximation` | measure/integration |
| `estimate-by-bounding` | the analysis reflex |
| `show-both-inequalities` | ≤ and ≥ to get = |
| `optimise-a-free-parameter` | choosing ε, λ |

**This is the group Joe flagged** — the mined analysis patterns sitting
undifferentiated in `math-informal`.

### → `math-informal-CT` (Category Theory) — 1 joins the existing 6

`verify-universal-property` — "show it satisfies the relevant universal
property". It belongs with the CT six and is not currently with them.
**This one has a direct experimental consequence**: the commissioning test
treats the CT six as the discriminating set, so a CT query hitting this
pattern would score as a miss when it is in fact correct.

### → `math-informal-CO` (Combinatorics) — 2

`use-probabilistic-method` (Erdős) · `count-over-a-decomposition` (class
equation, Burnside, double counting)

### → `math-informal-NA` (Numerical Analysis) — 2

`numerical-scout` (map the landscape numerically before proving) ·
`hybrid-certification` (numerics to locate, exact arithmetic to certify)

### → `math-informal-RA` (Rings and Algebras) — 1

`encode-as-algebra`

### → `math-informal-LO` (Logic) — 1

`complexity-classification` (place a decision problem in a class via
reduction or constructive membership)

## Cut 1 — meta moves to `math-strategy/`, which is what it is for (20)

Already there (13): characterization-result · clarification-meta ·
compose-independent-lemmas · constraint-tension-resolution ·
construct-through-a-finite-correspondence\* · convention-bridge ·
existence-result · hypothesis-category-check · non-circularity-check ·
preemptive-objection-clearance · property-of-object-result ·
route-exploration-and-pivot · structural-relation-result

Moving in from `math-informal` (3): `exhaustion-as-theorem` ·
`structural-obstruction-as-theorem` · `technique-landscape-map`

All three are moves *about* proving rather than moves *in* a proof: "when
approaches fail at the same barrier, prove the barrier is fundamental";
"characterize a method's failure as a theorem"; "build a typed library of
techniques before attempting the proof".

\* `construct-through-a-finite-correspondence` is mined content, not meta —
it is a genuine construction strategy (Galois/Pontryagin/Stone). **Flagged as
misfiled: propose `math-informal-RA` or keep in `math-strategy`, Joe's call.**

**`clarification-meta` is titled "Clarification (Meta-Tag, not a Pattern)".**
It declares itself not to be a pattern. Propose deletion, or move outside the
library entirely.

## `math-formalization/` — split general tooling from subject lemma shapes

### General Lean/formalization tooling — stays (6)

coercion-bridge · field-simp-reciprocal · tactic-algebra-interference ·
construction-cost-asymmetry · lift-prove-upstairs-reflect-by-injectivity ·
transport-across-an-instance-diamond

### → `math-formalization-CA` — 8

ae-integral-zero · lp-norm-comparison · measure-restrict-simplify ·
metric-cauchy-convergence · hilbert-projection-properties ·
continuous-linear-map-composition · rpow-exponent-limit ·
separation-function-from-distance

### Singletons — 4

`complex-polynomial-bound` → CV · `finite-group-classification` → GR ·
`connected-union-via-common-point` → GN ·
`close-bijectivity-by-counting-not-inverting` → GR or general

## The singleton problem, and a recommendation

`-RA`, `-LO`, `-CV`, `-GN` would each hold **one file**. A one-file directory
buys nothing for retrieval and adds a glob to maintain — and directory globs
are exactly what broke the pattern channel this morning.

**Recommendation: directories only where there is mass** (`-CA` informal 6,
`-CA` formalization 8, `-CT` 7, `-CO` 2), and carry the fine-grained arXiv
code in the **`@subjects` metadata field**, which already exists, flows
into the pattern entity, and can scope retrieval as a store predicate rather
than a path glob. That is the lesson of this morning applied: scope by
queryable attribute, not by directory.

## Execution hazards, if this is approved

1. **Moving a file changes its pattern id** (`<family>/<name>`). The watcher
   will create a new entity; the old one becomes an orphan. ~40 orphans.
2. `@flexiarg` headers must be rewritten to match the new path, or id and
   location disagree.
3. `patterns-index.tsv` rows must move with them.
4. Any pattern referenced by `@instantiates` elsewhere breaks —
   `transport-across-isomorphism` and `existence-result` are both cited by
   mined patterns.

So this wants: manifest approved → mechanical execution → orphan sweep →
verification that the 77 still resolve. The classification is judgement and
should stay with Joe or the captain; the execution is mechanical and belongs
in a packet.
