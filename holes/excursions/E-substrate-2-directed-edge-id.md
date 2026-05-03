# Excursion: Substrate-2 Directed-Edge Stable-ID Collision

**Status:** RESOLVED (2026-04-27, same day as discovery) via option (a)
**Owner mission:** `holes/missions/M-live-geometric-stack.md`
**Discovered during:** pyramidal expansion phase 1 on futon1a.

## Resolution

Option (a) from §"Fix options" landed:
`futon3/scripts/ingest_v05_to_futon1a.clj` now appends a synthetic
third endpoint `(str "dir:" src "→" dst)` for the directed edge
types `{calls, coverage, vocabulary-use, term-defines, contains}`.
The sort-based stable-ID then differs by direction; old vs new
direction-pairs no longer collide.

Re-ingested under fresh labels `futon2-d`, `futon4-d`, `futon1a-d`
to leave the original collisions visible as historical evidence.

Verification (2026-04-27):

| Codebase | bb call edges | XTDB call edges (new -d label) | Pass? |
|---|---:|---:|---|
| futon2 | 766 | 766 | ✓ |
| futon4 | 128 | 128 | ✓ |
| futon1a | 508 | 508 (was 507 collapsed) | ✓ |

Strict regression now holds on all three labels;
phase-1 40/40 invariants, phase-2 12/12 invariants pass
without tolerance.

Phase-2 geometric layer reader and phase-1 endpoint-resolution
test both updated to skip synthetic `dir:` endpoints when reading.

## Follow-on note for phase 3+

The synthetic-third-endpoint scheme works but is a workaround.
For phase 4 (live JVM watcher), the proper fix is option (b) or
(c) — direction-aware stable-ID at the futon1a layer itself,
either by patching `stable-hyperedge-id` or by allowing client-
supplied `:hx/id`. Tracked here for visibility; not blocking
phases 3-7.

## The bug

futon1a's stable-ID for hyperedges is computed at
`futon1a/src/futon1a/compat/futon1_write.clj:209`:

```clojure
(defn- stable-hyperedge-id
  [{:keys [hx-type endpoint-ids]}]
  (str "hx:"
       (if (keyword? hx-type) (subs (str hx-type) 1) (str hx-type)) ":"
       (str/join "." (sort endpoint-ids))))
```

`(sort endpoint-ids)` makes the ID a function of the
*unordered set* of endpoints, not the ordered tuple. For
substrate-1's mostly-undirected edge types (`code/requires`,
`link/refers-to`), this is fine — direction was never
encoded.

Substrate-2 introduces **directed edge types** where the
order of `:hx/endpoints` carries semantics:

| Edge | Source | Target | Why direction matters |
|---|---|---|---|
| `code/v05/calls` | caller-var | callee-var | computes ∇T = T(callee) − T(caller) |
| `code/v05/coverage` | test | covered-var | T-direction asymmetric |
| `code/v05/vocabulary-use` | namespace | term | one-way (term doesn't "use" namespace) |
| `code/v05/term-defines` | doc | term | one-way (term doesn't "define" doc) |
| `code/v05/contains` | namespace | var | one-way (var doesn't "contain" ns) |

When two opposite-direction edges share the same endpoint
set (e.g., `A calls B` AND `B calls A`), they sort to the
same `:hx/id`, and one collapses into the other on the
second POST.

## Concrete observation

futon1a phase-1 ingest (label `futon1a-phase1`):

- bb v0.5 projection produced **508 distinct directed call edges**.
- futon1a stored **507 :calls hyperedges**.
- Δ = 1 → exactly one bidirectional pair collapsed.

For futon2 and futon4, the discrepancy was 0 (no mutual
recursion in the call graph at this resolution). futon1a
has at least one mutual-call pair. As the substrate
ingests more codebases, the discrepancy will grow.

## Why it matters geometrically

Suppose `A→B` and `B→A` both exist with `T(A)=1, T(B)=0`:

- `A→B` contributes `∇T = -1`, raising `ΔT(A)` by 1 (outflow gain) and lowering `ΔT(B)` by 1.
- `B→A` contributes `∇T = +1`, raising `ΔT(A)` by 1 (inflow gain) and lowering `ΔT(B)` by 1.

Both edges contribute to `ΔT`; collapsing them halves the
contribution. Top-K |ΔT| readings in regions with
mutual-call structure will be **understated**.

## Fix options

| Option | Cost | Trade-off |
|---|---|---|
| **(a) Synthetic third endpoint encoding direction** — for directed edges, append `(str src "->" dst)` as a third element of `:hx/endpoints`. The sort then differs by direction. | Low (client-side change in ingest). Re-ingest under same label is idempotent only against the OLD scheme; would need fresh label or store wipe. | Pollutes endpoint-id namespace with synthetic strings; queries `?end=A` still work for source-A but a new third class of "endpoints" appears. |
| **(b) Patch `stable-hyperedge-id` in futon1a** to optionally not sort. | High (modify futon1a; can be done via Drawbridge nREPL without restart, but invasive). | Cleanest semantically; risks breaking substrate-1 edges that relied on order-insensitivity. |
| **(c) Move stable-ID computation client-side** — substrate-2 ingest computes a direction-aware ID and passes it explicitly. Requires futon1a accepting a client-supplied `:hx/id` override. | Low (if API supports), high (if API doesn't). | Most flexible long-term; least surgical to substrate-1. |
| **(d) Accept the collapse for v1; fix in phase 4** when JVM-side writer can use a direction-aware scheme natively. | Zero immediate cost. | Geometric quantities under-count in mutual-recursion regions; futonic-zapper signatures may miss some cases. |

## Recommendation

Phase-1 stays as-is for the pyramidal expansion across
codebases (the off-by-N findings are themselves
informative about codebase shape — high N suggests
high mutual-recursion). **Phase 3 (commits-as-vertices)
needs a true fix** because commit-edge directionality is
load-bearing for `∂T/∂t`. By that point, option (b) or (c)
should be designed in.

In the interim, regression tests for directed-edge counts
should accept tolerances logged as `:bidirectional-collapse`
and the substrate-2 reports should annotate edge counts
with the source-of-truth choice (XTDB count vs source
projection count).

## References

- futon1a stable-ID code:
  `futon1a/src/futon1a/compat/futon1_write.clj:209-213`.
- Phase-1 PUR for futon1a (2026-04-27): the test failure
  that surfaced this.
- M-live-geometric-stack §"Inside-out invariant scaffolding"
  L1 invariant on stable IDs — needs a refinement note
  for directed edge classes.
