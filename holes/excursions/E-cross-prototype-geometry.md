# Excursion: Cross-Prototype Geometry — same calculus, two manifolds

**Status:** STUB / parked (2026-04-27)
**Owner mission:** `holes/missions/M-pattern-application-diagnostic.md`
**Parent excursion:** `holes/excursions/E-math-prototype-pilot.md`
  (this excursion is open-question item 4 from there).

## Goal

Test the prototype-1-as-instance-of-a-series claim
empirically: define the codebase analogue of T (the tension
scalar) on a small futon-stack region, compute it, and see
whether high-T sits where the operator already feels active
tension. If yes, the geometric commitment is validated cross-
prototype; if no, we learn what the codebase domain demands
that the math domain doesn't.

## Mapping

| Math-side definition (validated, E-math-prototype-pilot.md) | Codebase analogue (this excursion) |
|--------------------------------------------------------------|------------------------------------|
| Vertex: `claim`                                              | Vertex: `function-spec` (docstring claim, type signature, mission-claim, invariant claim) |
| Vertex: `proof`                                              | Vertex: `test`, `runtime-evidence`, `invariant-check-result` |
| Edge: `derivation` (claim → proof_of_target)                 | Edge: `coverage` (function-spec → test_proving_it) |
| `T(claim) = 1 if no incident derivation, else 0`             | `T(function-spec) = 1 if no incident coverage, else 0` |
| `ΔT` ≈ Tornhill hotspot (load-bearing concept)               | `ΔT` ≈ Tornhill hotspot (load-bearing function) |

## Tasks

1. Pick a futon-stack region (e.g. `futon3c/src/futon3c/aif/`
   — small, recently-active, has both code and tests).
2. Build a v0 hypergraph projection: parse `.clj` files into
   `(ns, var, defn-spec)` vertices; parse test files into
   `(test, asserted-spec)` vertices; emit `coverage` edges
   where a test asserts a defn's behaviour (heuristic: test
   name string-matches defn name).
3. Compute `T(spec) = 1 if no incident coverage else 0`.
4. Compute `(ΔT)(v) = sum of (∇T) over incident edges` per
   vertex. Read the top-10 vertices by `|ΔT|`. Are these the
   functions / specs that intuitively feel like the "active
   tension" of the region?
5. Reverse check: ask Joe (or the operator working in that
   region) to point at the 5 functions that feel most "owed
   work" — where TODOs live mentally even if uncoded. Does the
   computed top-K `|ΔT|` overlap?

## Pitfalls to look for

- **Naive coverage detection (string match) misses indirect
  coverage.** If `function-foo` is tested only via `function-
  bar` calling it, the v0 won't see that. Refinement: walk
  call-graph for transitive coverage. v0 deliberately uses the
  cheaper definition to see how much it gets right anyway.
- **Codebase has no analogue of "claim".** Function defns
  rarely have explicit specs. Use docstring presence as a
  proxy; functions with `nil` docstring contribute T=1
  trivially. This may dominate the signal — flag if so.
- **Mission docs introduce another vertex class.** Mission-
  claims should also be in the hypergraph; their coverage is
  by code that demonstrably implements them. Probably want
  `mission-claim → impl` edges by hand-curation for v0.

## Expected payoff

If high-T regions of the codebase hypergraph correspond to the
operator's intuitive tension map, we have:
- (a) Empirical validation of the geometric commitment cross-
  prototype.
- (b) The base case for an automated "where's the active
  tension" surface in the Stack HUD.
- (c) Strong evidence that the same `(T, ∇, Δ)` machinery runs
  on different graphs — the prototype-N generalisation claim.

If low-T regions feel intensely active to the operator, or
high-T regions feel inert, we learn what's missing from the
codebase hypergraph (probably: the "specs" the codebase
actually has aren't well-encoded as `function-spec` vertices;
specs live in mission docs, comments, conversation history, or
nowhere explicit).

## References

- Pilot validating the math side:
  `holes/excursions/E-math-prototype-pilot.md` §"Tension
  scalar demo on paper #5".
- §"Geometric commitment" of the parent mission for the
  calculus and the why.
- §"What this means for M-futon-enrichment" of the parent
  mission — Tornhill's hotspot is the same `ΔT`.
- futon4 `M-futon-enrichment.md` already proposes
  `var → mission`, `var → evidence`, `var → tension`
  enrichment layers; this excursion's hypergraph projection
  is a v0 implementation of those edges restricted to a
  single subdirectory.
