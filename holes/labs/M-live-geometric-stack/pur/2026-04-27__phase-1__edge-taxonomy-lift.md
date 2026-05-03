# PUR: Phase 1 — Edge Taxonomy Lift to futon1a

pattern (re-confirmed):
- `storage/deterministic-ingest-pipeline` ✓
- `storage/canonical-interface` ✓
- `futon-theory/counter-ratchet` ✓
- `storage/error-layer-hierarchy` ✓
- `storage/durability-first` ✓

actions taken:
- Created `futon3/scripts/ingest_v05_to_futon1a.clj` (bb).
- Created tests
  `futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj`.
- Created mission diagram
  `futon3/holes/labs/M-live-geometric-stack/diagram/phase-1-mission-diagram.edn`.
- Ingested futon2 (post-WM, label `futon2-phase1`) into live futon1a.

outcome: success. 40/40 invariant tests pass. 2,400+
hyperedges live in futon1a XTDB at HEAD `489e75f`:

| Type | Count | XTDB ↔ bb v0.5 |
|---|---:|---|
| `code/v05/var` | 562 | bb-distinct=562 ✓ (bb raw=565 over-counts duplicates) |
| `code/v05/test` | 46 | ✓ |
| `code/v05/namespace` | 58 | (not in bb output) |
| `code/v05/term` | 140 | (not in bb output) |
| `code/v05/doc` | 5 | (not in bb output) |
| `code/v05/calls` | 766 | ✓ |
| `code/v05/coverage` | 50 | ✓ |
| `code/v05/vocabulary-use` | 63 | ✓ |
| `code/v05/term-defines` | 145 | (not in bb output) |
| `code/v05/contains` | 565 | (one per var-in-ns) |

prediction errors:

1. **Penholder header value** — guessed
   `claude-substrate2-phase1`; futon1a's `FUTON1A_ALLOWED_PENHOLDERS`
   only allows `api`. First ingest run had 0/2403 success;
   diagnosed via L3 error response (well-shaped, per
   `storage/error-layer-hierarchy` — exactly the value the
   pattern promised). Fix: `(or (System/getenv "FUTON1A_PENHOLDER") "api")`
   in both ingest and test scripts.

2. **`:hx/labels` shape** — assumed map (`{:repo "x" :phase 1}`);
   substrate-1's actual convention is **flat list of tag strings**
   (`["v05" "phase-1" "futon2-phase1"]`). First post stored
   labels as `[nil nil]` (the map values silently dropped).
   Fix: labels become tag-list; `:repo` info moved into
   `:hx/props` (which IS a map). futon1a's upsert REPLACES
   props on re-POST (per existing memory note), so the fix
   landed on the next ingest run without corruption.

3. **Var-count discrepancy is a real code-coherence finding,
   not a bug.** bb v0.5 reports 565 vars; XTDB stores 562
   after L1 stable-ID dedupe. Investigation: three vars are
   defined twice in the same namespace —
   `ants.war/on-white?` (lines 19 + 1078),
   `ants.war/flush-traces!` (lines 1027 + 1047),
   `ants.war/ingest-eps`. **The substrate's L1 invariant
   surfaced a duplicate-defn smell on its very first ingest.**
   Test was updated to compare distinct-qname counts on the
   bb side (XTDB count is authoritative); the discrepancy
   itself is logged and surfaced to the operator.

4. **DELETE not routed for hyperedges.** The
   `/api/alpha/hyperedge/:id` route handles GET only; DELETE
   returns method-not-allowed despite advertising it as
   allowed. A debug hyperedge from API discovery
   (`hx:code/v05/var:debug.test/foo`) couldn't be removed
   via API. Not blocking — added a `--label` filter on
   queries, which is the right cross-codebase boundary
   anyway.

invariants verified at the live store:

| Invariant | Result |
|---|---|
| L1-stable-id (8 types) | PASS — every `:hx/id` is `hx:{type}:{eps}`, all unique |
| L1-idempotency (8 types) | PASS — re-ingest writes 0 new edges |
| L2-endpoint-resolution (calls) | PASS — 0/766 unresolved |
| L2-vocab-target-resolution | PASS — 0/63 unresolved |
| L2-counter-ratchet (9 types) | PASS — no count decreased |
| regression-vs-bb-v05 (5 metrics) | PASS — distinct-qname normalised |

substrate-2 phase 1 status: **complete**. The substrate is
now its own first user; phase 2 (geometric layer as XTDB
queries) can begin reading from these hyperedges immediately.

connections:
- `M-pattern-application-diagnostic`: the substrate now has
  the typed edges that pattern-application records will
  reference via `:pattern-application` edges (phase 5).
- The duplicate-defn finding in `ants.war` is a real
  candidate SORRY — a code-coherence pattern application
  to write once the boundary-growth interface is wired.
- `M-three-column-stack` closure-rot: substrate-1's
  declared edge types still have count 0; substrate-2's
  edges co-exist under the `code/v05/*` prefix without
  collision. A re-ingestion of substrate-1 is now safe to
  run.

next: phase 2 (geometric layer as derived XTDB queries —
T, ΔT, drift readable via the futon1a query API or via
nREPL on the futon1a JVM).
