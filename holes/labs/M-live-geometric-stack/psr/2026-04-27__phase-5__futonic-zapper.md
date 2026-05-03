# PSR: Phase 5 — Futonic-Zapper Signature Emission

context: phases 1–4.5 produced ≈285,000 substrate hyperedges
across 15 active labels. The seven satisficing-signatures
specified in `M-live-geometric-stack.md` §"futonic-zapper
specification" + the completion-rot signature (B-3 lineage)
need implementation as XTDB-shaped queries that emit
`code/v05/satisficing-signature` hyperedges. Substrate is
fully ready; this is the payoff phase.

patterns:
- `storage/canonical-interface` ✓ — read via the same HTTP
  API; no schema additions beyond a new `:hx/type`.
- `storage/deterministic-ingest-pipeline` ✓ — same query +
  same substrate state = same emitted signatures.
- `futon-theory/event-protocol` ✓ — each detected signature
  is a typed evidence hyperedge.
- `agent/sense-deliberate-act` ✓ — the script's loop is
  sense (query) → deliberate (classify) → act (POST).

decision:
- New script `futon3/scripts/phase_5_signatures.clj` (bb).
- For each signature, define a *query* function that returns
  a list of `{:signature :evidence-vertices :evidence-commit-range :severity :details}`
  records, then POST one `code/v05/satisficing-signature`
  hyperedge per record.
- Run across all active labels, filtered by `:repo`.
- v0 focuses on the four signatures most cleanly detectable
  on the substrate as it is *today* — defer cross-commit
  time-series signatures (quick-fix, coverage-retreat,
  load-bearing-under-tested, concept-introduced-without-
  attachment) to v0.5.

### v0 signatures (4 detectable today)

| # | Signature | Query shape |
|---|---|---|
| 1 | **Adapter shim that doesn't adapt** | Namespace whose name contains `adapter`/`bridge`/`shim`/`port`, paired with the count of `:calls` edges from any of its members to namespaces NOT in its repo. Severity ∝ deficit. |
| 2 | **Work-around (drift-without-structure)** | Connected-component pair `(C₁, C₂)` with vocab-overlap ≥ θ AND zero `:calls`/`:coverage` edges across. Already computed by `geometric_layer_phase2.clj`'s `drift-hotspots`; emit as typed hyperedges instead of in-memory list. |
| 3 | **Concept used without definition** | `:vocabulary-use` edge whose target term has zero incident `:term-defines` edges. |
| 4 | **Completion rot** | Hyperedge type whose count under any label is below a threshold AFTER the label's mission was marked COMPLETE — using substrate-1's `mission/...` types if present, else "label exists but has < N edges of expected types". |

### v0.5 signatures (need cross-commit time-series; defer)

5. Quick fix — ΔT redistribution across commits.
6. Coverage retreat — monotonic decrease in paired-vars%.
7. Load-bearing under-tested — incoming-call growth without coverage growth.
8. Concept introduced without attachment — vocab-doc commit with no
   `:vocabulary-use` edges within N subsequent commits.

These need a per-commit ΔT computation, which is doable with the
existing commits-as-vertices substrate but requires walking
`:precedes` edges and computing T(v, c) per commit. Out of scope
for v0; queued for v0.5.

### Output schema

For each detected signature:

```clojure
{:hx/type "code/v05/satisficing-signature"
 :hx/endpoints [signature-id severity-marker]
 :hx/labels   ["v05" "phase-5" repo-label signature-name]
 :hx/props
 {:repo                  repo-label
  :phase                 5
  :signature             signature-name  ; e.g. "adapter-shim-no-adapt"
  :severity              keyword         ; :low :medium :high
  :ts                    millis
  :evidence-vertices     [qname...]
  :evidence-commit-range [from-sha to-sha]   ; if applicable
  :details               {<sig-specific>}}}
```

Stable ID via the directed-endpoints convention (third synthetic
endpoint encoding signature + repo + content-hash) so re-runs
on unchanged substrate are idempotent.

alternatives:
- Per-signature script files (rejected: composition is harder; one
  script with per-signature functions is clearer).
- Computing signatures inside the watcher daemon (rejected: phase 5
  is *queryable*, not necessarily *live* — separation of concerns).
- Storing signatures as substrate-1 violation records (rejected:
  the substrate-2 `code/v05/satisficing-signature` type is the
  canonical home; substrate-1's mechanism is older and partial).

outcome (target):
- Running `bb phase_5_signatures.clj --label <l>` against any
  active label produces a report and emits N
  `code/v05/satisficing-signature` hyperedges.
- v0 exercises 4 signatures across 15 labels; expect dozens
  to hundreds of emitted records (depending on stack hygiene).
- Re-runs are idempotent.
- Spot-check: at least the WM-disconnection finding (the original
  "work-around" example from the cross-prototype-geometry
  excursion) is re-emitted as a typed signature record on
  futon2-d.

confidence: high. The query patterns are well-trodden — each
signature is a join over already-ingested edge classes; nothing
new about the substrate. Risk: the volume of detected signatures
on a stack of 285k hyperedges may be very large. v0 should
include severity thresholds so the output is operator-reviewable.
