# The math split — RULED, ready for mechanical execution

**claude-2, 2026-08-17.** Supersedes `MANIFEST-math-split-proposal.md`
(2026-08-13, written for 77 patterns; there are now **83**). This sheet carries
every one of the 83 to a destination, so the execution packet needs no
judgement.

## Rulings that shaped it

- **2026-08-13, Joe:** `verify-universal-property` → `math-informal-CT`;
  `clarification-meta` out of the library. **Already executed** (`363d172`).
- **2026-08-17, Joe:** primary category is the **directory**, arXiv-style, and
  **singletons get their own directory** — *"so that we don't have to reorganise
  them later… we should be adding more algebra, topology, analysis, and measure
  theory patterns as we go with APM shortly."* This **overturns** the old
  manifest's mass-threshold recommendation.
- **2026-08-17, Joe:** cross-listing is the `@cross-list` field; `@subjects` is
  its deprecated spelling.

Directory naming is `<kind>-<CODE>`, `CODE` being the arXiv subject code without
its `math.` prefix — the convention `math-informal-CT` already followed.

---

## A. `math-informal/` — universal heuristics, stay put (21)

argue-by-contradiction · check-the-extreme-cases · construct-an-explicit-witness ·
construct-auxiliary-object · dualise-the-problem · exploit-symmetry ·
failure-mode-characterization · find-the-right-abstraction ·
induction-and-well-ordering · local-to-global · parametric-tension-dissolution ·
quotient-by-irrelevance · reduce-to-known-result ·
separate-into-independent-pieces · split-into-cases ·
structural-characterization · structural-equivalence · structural-inclusion ·
the-diagonal-argument · transport-across-isomorphism · try-a-simpler-case ·
unfold-the-definition · work-examples-first

Each applies in any subject. `failure-mode-characterization` and
`parametric-tension-dissolution` read as meta but are genuine mathematical
moves, so they stay (unchanged from the 2026-08-13 rationale).

## B. Subject moves out of `math-informal` (13)

| → | patterns |
|---|---|
| `math-informal-CA` (6) | epsilon-of-room · pass-to-a-subsequence · monotone-approximation · estimate-by-bounding · show-both-inequalities · optimise-a-free-parameter |
| `math-informal-CO` (2) | use-probabilistic-method · count-over-a-decomposition |
| `math-informal-NA` (2) | numerical-scout · hybrid-certification |
| `math-informal-RA` (1) | encode-as-algebra |
| `math-informal-LO` (1) | complexity-classification |

`-CA` is the group Joe originally flagged: mined analysis patterns sitting
undifferentiated among the universals.

## C. Kind moves, `math-informal` → `math-strategy` (3)

exhaustion-as-theorem · structural-obstruction-as-theorem ·
technique-landscape-map — all moves *about* proving rather than moves *in* a
proof.

## D. `math-informal-CT` — already correct (7)

chase-the-diagram · check-it-on-generators · compare-universal-properties ·
factor-and-lift · strictify-via-coherence · transpose-across-an-adjunction ·
verify-universal-property. **No moves.**

## E. `math-formalization/` — general Lean tooling stays (11)

coercion-bridge · field-simp-reciprocal · tactic-algebra-interference ·
construction-cost-asymmetry · lift-prove-upstairs-reflect-by-injectivity ·
transport-across-an-instance-diamond · **stamp-siblings-from-one-compiled-branch**
· **weld-range-lemmas-at-representation-seams** ·
**close-bijectivity-by-counting-not-inverting**

The three in bold are ruled here for the first time. All are subject-neutral
Lean discipline: compile one branch before stamping siblings; prove the range
before the endpoint at a representation seam; count rather than invert. **This
settles the old manifest's open question on `close-bijectivity` (GR *or*
general) in favour of general** — the pattern is about proof economy in finite
settings, not about groups.

## F. `math-formalization` subject moves (13)

| → | patterns |
|---|---|
| `math-formalization-CA` (11) | ae-integral-zero · lp-norm-comparison · measure-restrict-simplify · metric-cauchy-convergence · hilbert-projection-properties · continuous-linear-map-composition · rpow-exponent-limit · separation-function-from-distance · **layer-cake-crossover-split** · **transfer-derivatives-via-eventuallyeq** · **radialize-via-gauge-rescale** |
| `math-formalization-CV` (1) | complex-polynomial-bound |
| `math-formalization-GR` (1) | finite-group-classification |
| `math-formalization-GN` (1) | connected-union-via-common-point |
| `math-formalization-MG` (1) | **chart-a-polytope-sphere-by-perimeter-walk** |

Newly ruled (bold): `layer-cake-crossover-split` already declares
`@subjects analysis, measure-theory` — CA primary, cross-list FA.
`transfer-derivatives-via-eventuallyeq` is derivative transfer under local
equality — CA. `radialize-via-gauge-rescale` is gauge rescaling of a convex
body to a norm sphere — CA primary, cross-list MG.
`chart-a-polytope-sphere-by-perimeter-walk` is polyhedral-norm-sphere charting —
metric geometry, MG.

## G. `math-strategy/` — proof-process patterns (17)

Already there (13): characterization-result · compose-independent-lemmas ·
constraint-tension-resolution · convention-bridge · existence-result ·
hypothesis-category-check · non-circularity-check ·
preemptive-objection-clearance · property-of-object-result ·
route-exploration-and-pivot · structural-relation-result ·
**plan-first-attempt** · **isolate-computational-kernel-before-transport**

Arriving from C (3): exhaustion-as-theorem · structural-obstruction-as-theorem ·
technique-landscape-map

`plan-first-attempt` is proof process, unambiguously.
`isolate-computational-kernel-before-transport` declares
`@subjects analysis, formalization`, but its content is *proof architecture* —
isolate the scalar kernel before measure/coercion transport — so it stays in
`math-strategy` with `@cross-list [CA]`.

### `construct-through-a-finite-correspondence` — ruled: → `math-informal-RA`

The old manifest flagged it as misfiled and left the call open. It is a genuine
construction strategy (Galois / Pontryagin / Stone — realize a constrained
object through a finite one and an order-reversing correspondence), not a move
about proving. It joins `encode-as-algebra` in `-RA`, which stops that
directory being a singleton.

*This one is my judgement, not Joe's — flagged so it can be overturned cheaply.*

---

## Execution hazards — carry into the packet

1. **Moving a file changes its pattern id**, and **103 edges point at math
   patterns** (97 from within `math-*`, **6 from outside: `agent` 5,
   `peripherals` 1**). 35 distinct math patterns are targets. Worst hit:
   transport-across-isomorphism (9 inbound), split-into-cases (8),
   structural-relation-result (7), find-the-right-abstraction (6),
   reduce-to-known-result (6), estimate-by-bounding (6).
   **Moves and reference rewrites must be one atomic change.**
2. `@flexiarg` / `@arg` header lines must be rewritten to match the new path.
3. `resources/sigils/patterns-index.tsv` rows must move with them.
4. Directive migrations in the same pass: `@instantiates`→`@why`,
   `@see`/`@related`→`@see-also`, `@family`/`@childof`→`@up`,
   `@subjects`→`@cross-list` (arXiv codes), and **delete the 141
   `:generation-config` uses** (`@allow-new-claims`, `@ban`, `@max-iterations`,
   `@length` — verified to have no consumer in futon3a/3b/3c/4).
5. The watcher must be **stopped** for the move (it is, as of 2026-08-17 09:59Z,
   at cycle 847) and restarted after a verification pass.

## Verification the packet owes

- All 83 patterns resolve at their new ids; zero orphans.
- Zero dangling edge targets across the **whole** library, not just `math-*` —
  the 6 external inbound edges are the ones a math-only check would miss.
- `@flexiarg` id equals the path for every file.
