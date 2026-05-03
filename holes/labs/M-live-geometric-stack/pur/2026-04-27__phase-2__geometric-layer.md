# PUR: Phase 2 — Geometric Layer (T, ∇T, ΔT, drift) over futon1a

pattern (re-confirmed):
- `storage/canonical-interface` ✓ (HTTP API only; no direct XTDB access)
- `futon-theory/counter-ratchet` ✓ (geometric quantities deterministic
  across re-runs)
- `storage/durability-first` ✓ (phase-2 only reads; no writes)

actions taken:
- Created `futon3/scripts/geometric_layer_phase2.clj` (bb).
- Created
  `futon3/holes/labs/M-live-geometric-stack/tests/phase_2_geometric_test.clj`.

outcome: success. 12/12 invariants pass. The geometric layer
is now a *derived* read against the substrate, not a stored
re-projection.

| Quantity | bb v0.5 (from source) | XTDB (derived) | Match |
|---|---:|---:|---|
| paired-vars (distinct qname) | 22 | 22 | ✓ |
| tests | 46 | 46 | ✓ |
| coverage-edges | 50 | 50 | ✓ |
| call-edges | 766 | 766 | ✓ |
| vocab-edges | 63 | 63 | ✓ |
| components | 34 | 34 | ✓ |
| nonzero-ΔT vertices | 114 | 114 | ✓ |
| top +ΔT (15 vars) | aif-step +6, ... | identical | ✓ |
| top −ΔT (15 vars) | choose-action −21, ... | identical | ✓ |
| component-size top-10 | (1 2 2 2 6 7 9 18 169 368) | identical | ✓ |
| drift-hotspots | 10 pairs | identical | ✓ |
| determinism | n/a | re-run = identical output | ✓ |

prediction errors:

1. **paired-vars dedupe discrepancy (recurrence of phase-1
   finding).** First test run failed `paired-vars: bb=25
   xtdb=22`. Same root cause as phase 1: bb counts each
   defn occurrence; XTDB collapses duplicates via L1 stable
   ID. **3 of the 3 duplicate defns in `ants.war` are paired**
   (covered by tests) — so dedupe drops 3 from the bb count.
   Fix: regression test now compares `paired-distinct =
   |covered-qnames ∩ var-qnames|` on the bb side, normalising
   away the duplicates. The discrepancy is logged as
   evidence (per phase-1 PUR's note) and not as a bug.
   This is a *consistent* finding across phases — substrate-2's
   L1 stricter discipline catches duplicate defns reliably.

2. **HTTP query latency was a non-issue.** PSR confidence
   note worried about HTTP-roundtrip-per-edge being slow.
   In practice, fetching all hyperedges of a type in one
   request (with `&limit=20000`) keeps total fetch time well
   under a second for the futon2-phase1 dataset (~2,400
   hyperedges across 9 types ≈ 9 HTTP calls). Will need
   batched / paginated queries when total store size grows
   past ~100k edges, but not for phase 2.

3. **Self-test typo caught a small bug.** First version of
   the regression test had a malformed assert
   (`(= xc :paired-vars (:paired-vars bbc) (:paired-vars xc))`
   — `xc` and `:paired-vars` weren't meant to be there).
   Surfaced as a `FAIL: (skip in or-form)` line; quick to
   spot and remove. Reminder: tests should be reviewed at
   the same care as production code.

invariants verified at the live store:

| Invariant | Result |
|---|---|
| Counts match bb v0.5 (modulo distinct-qname normalise) | PASS (7/7 metrics) |
| Top +ΔT set identical | PASS |
| Top −ΔT set identical | PASS |
| Component-size top-10 identical | PASS |
| Drift-hotspots identical | PASS |
| Determinism (same query twice = same output) | PASS |

substrate-2 phase 2 status: **complete**. The substrate is
now operationally a working geometry-layer query target.
The same script can be re-run on any other label (any
codebase ingested via phase 1) without modification —
ready for the pyramidal expansion.

connections:
- The 562 var × 766 call hypergraph is now a queryable
  store, not a projection. Surfaces (Stack HUD, Arxana
  Browser) can re-point to this query shape directly.
- The duplicate-defn finding in `ants.war` remains
  unaddressed in code; it's a candidate SORRY for the
  boundary-growth interface (M-pattern-application-
  diagnostic).
- Phase 3 (commits-as-vertices) becomes the next move
  per Joe's pyramidal strategy: but only *after* phase 1
  + phase 2 are run on a second codebase
  (futon3 / futon3c / futon4) to validate the
  schema cross-codebase before phase 3 hardens around it.

next: per Joe's pyramidal expansion strategy, broaden
phase 1 to a second codebase before continuing into phase 3.
Candidate next codebase: futon3 (smallest of the
remaining stack, has its own AIF library, would test
the schema against a much richer substrate).
