# PSR: Phase 2 — Geometric Layer (T, ∇T, ΔT, drift) over futon1a

context: phase 1 (edge taxonomy lift) landed clean — 562 vars,
50 coverage edges, 766 calls, 63 vocab-uses, 145 term-defines
live in futon1a under label `futon2-phase1`. Phase 2's job is
to compute the geometric quantities the parent excursion
(`E-cross-prototype-geometry.md`) defined — T, ∇T, ΔT, drift
between connected components — by querying the substrate
rather than projecting from source. This proves the geometric
layer is *derived*, not stored, and it makes the diagnostic
queryable in O(query) time without re-running the bb projection.

patterns:
- `storage/canonical-interface` — read via the same HTTP API
  the writes used; no direct XTDB access from this script.
- `futon-theory/counter-ratchet` — geometric quantities are
  monotone in the sense that for fixed substrate the answer
  is deterministic; re-querying produces identical results.
- `storage/durability-first` — phase 2 only reads; durability
  is phase 1's responsibility.

decision:
- New script `futon3/scripts/geometric_layer_phase2.clj` (bb).
  Reads the substrate-2 phase-1 hyperedges via the futon1a
  HTTP API (filtered by `:repo` prop). Computes the geometric
  quantities in-memory and emits a report identical in shape
  to `v0_codebase_hypergraph.clj`'s output — same top-K
  ±ΔT lists, same component summary, same drift hotspots.
- Computation method: pull all `code/v05/var`, `code/v05/test`,
  `code/v05/coverage`, `code/v05/calls`, `code/v05/vocabulary-use`,
  `code/v05/namespace`, `code/v05/term`, `code/v05/contains`
  for the target label; build in-memory adjacency; compute T,
  ∇T, ΔT, components, drift. Identical algorithm to v0.5's
  bb script.
- **Forward-compatibility note:** the algorithm is XTDB-shaped
  (could be expressed as a Datalog query joining the typed
  edges) so phase 2's logic ports cleanly into the futon1a
  JVM via Drawbridge nREPL when phase 4 (liveness) lands.
  For phase 2 we keep it client-side to avoid touching the
  running JVM.
- Tests live alongside phase-1 tests; assert the XTDB-derived
  geometric quantities exactly match the bb v0.5 projection
  numbers — same regression discipline as phase 1.

alternatives:
- Compute via Drawbridge nREPL into futon1a JVM (rejected for
  phase 2 only: deferred to phase 4 when the watcher needs
  in-process compute anyway; for now keep the architecture
  same as phase 1's HTTP-only access — easier to test).
- Add a derived-edge layer in futon1a itself (`code/v05/T`,
  `code/v05/deltaT`) computed by an ingestion step (rejected:
  the parent mission specifies geometric quantities are
  *derived*, not stored — adding stored derived edges
  duplicates state and creates the very stale-projection
  problem substrate-2 exists to prevent).

outcome (target):
- `bb geometric_layer_phase2.clj --label futon2-phase1` prints
  a report with: counts (matching phase-1 counts), top-15 +ΔT
  vars, top-15 −ΔT vars, component summary, drift hotspots —
  byte-for-byte identical to the `v0_codebase_hypergraph.clj`
  output for futon2.
- Tests pass: regression invariant (XTDB-derived geometry =
  bb-projection geometry), determinism invariant (same query
  twice = same result).
- Establishes the read-side architecture for phase 4-7: any
  surface (Stack HUD, Arxana Browser, futonic-zapper console)
  consumes the geometric layer via the same query shape.

confidence: high. The algorithm is identical to v0.5's; the
data is identical to v0.5's (verified in phase 1's regression
test); the only new code is the futon1a HTTP query loop. Risk:
HTTP-roundtrip-per-edge could be slow, in which case batched
queries or a single bulk fetch is the fix.
