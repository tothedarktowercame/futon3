# PSR: Phase 1 — Edge Taxonomy Lift to futon1a

context: substrate-2 phase 1. We have a working v0.5 projection
(`futon3/scripts/v0_codebase_hypergraph.clj`) producing typed
edges from clojure source + vocab markdown. The substrate-1
ingest pipeline (`futon4/scripts/ingest-three-columns.py`) exists
but has only namespace-level granularity. Phase 1 lifts the v0.5
edge classes (`:calls`, `:coverage`, `:vocabulary-use`, `:term`,
`:term-defines`) into futon1a as typed hyperedges. The store is
currently empty (completion-rot finding 2026-04-27); phase 1
operates as a clean-slate beachhead on futon2 only.

patterns:
- `storage/deterministic-ingest-pipeline` — the script's output
  is a deterministic function of (source state, vocab paths,
  repo label). Re-running with the same inputs produces the same
  hyperedge set; idempotent on stable-ID match.
- `storage/canonical-interface` — write through futon1a's HTTP
  POST `/api/alpha/hyperedge` (the canonical write surface;
  pipeline.run-write! enforces L4→L0). No bypass.
- `futon-theory/counter-ratchet` — phase 1 must never decrease
  the count of any hyperedge type within a single ingestion run.
  Re-ingestion may produce equal counts (idempotent dedupe via
  stable IDs); never lower.
- `storage/error-layer-hierarchy` — every ingest failure carries
  `{:error/layer N :error/status S :error/reason R :error/context M}`
  per `futon1a/README-best-practice.md`.
- `storage/durability-first` — L0 success is the ratification
  signal; until L0 returns tx-id the hyperedge does not count.

decision:
- New script `futon3/scripts/ingest_v05_to_futon1a.clj` (bb).
  Reuses the v0.5 projection logic; converts each typed edge to
  a futon1a `POST /hyperedge` call with `:hx/type` `code/v05/...`
  prefix to avoid collision with substrate-1's namespace.
- Hyperedge schema:
  - Vertex hyperedges (vars/tests/namespaces/terms/docs):
    `:hx/type "code/v05/var"` etc., `:hx/endpoints [qname]`
    (single-endpoint by convention for vertex registration).
  - Edge hyperedges:
    `:hx/type "code/v05/calls"` etc., `:hx/endpoints [src dst]`.
  - All carry `:hx/labels {:repo "futon2", :phase 1}` for
    cross-codebase aggregation later.
- Stable IDs computed by futon1a's `f1w/upsert-hyperedge-doc`,
  matching substrate-1's `hx:{type}:{sorted-endpoints}` scheme.
  No collisions because of the type prefix.
- Tests live in `futon3/holes/labs/M-live-geometric-stack/tests/`;
  bb-runnable; assert L1 (idempotency, stable IDs) and L2
  (endpoint resolution: every `:calls` edge target must already
  exist as a `code/v05/var` hyperedge before the edge is written).

alternatives:
- Extend `ingest-three-columns.py` directly (rejected: language
  mismatch with the v0.5 bb script means duplication or RPC; bb
  reuse is cleaner).
- Write directly to XTDB via Drawbridge nREPL (rejected:
  bypasses pipeline; violates `storage/canonical-interface`).
- Skip futon1a, ship a sibling EDN file (rejected: defeats the
  purpose; substrate-2 needs the substrate to be queryable
  alongside substrate-1 by all consumers).

outcome (target):
- `bb ingest_v05_to_futon1a.clj /home/joe/code/futon2 --vocab ...`
  produces N hyperedges where N matches the v0.5 projection's
  vertex+edge count exactly. Re-running produces zero new
  hyperedges (idempotent).
- Tests pass: stable-ID test, idempotency test, endpoint-
  resolution test, regression test (XTDB query results match
  bb v0.5 numbers for `paired-vars`, `coverage-edges`,
  `call-edges`, `vocab-edges`).
- The substrate is now its own first user: phase 2's geometric
  queries read from this data.

confidence: medium. The ingestion pattern is well-trodden via
substrate-1; the new edge classes are additive; the inside-out
invariant tests are short and self-contained. Risk: futon1a's
write pipeline may reject edges with unknown `:hx/type` strings
or require additional registration; will surface at first POST.
