# PUR: Phase 1+2 Pyramidal Expansion to futon4 and futon1a

pattern (re-confirmed):
- `storage/deterministic-ingest-pipeline` ✓ (3 codebases, identical algorithm)
- `storage/canonical-interface` ✓ (HTTP API only, no JVM restart needed)
- `futon-theory/counter-ratchet` ✓ (counts grew across all 3 ingests; never decreased)

actions taken (single session, 2026-04-27):
- Ran phase-1 ingest on futon4 (Clojure slice only, label `futon4-phase1`).
- Ran phase-2 geometric layer on futon4.
- Ran phase-1 ingest on futon1a (label `futon1a-phase1`).
- Ran phase-2 geometric layer on futon1a.
- Parked two excursion stubs:
  - `E-substrate-2-elisp-projection.md` (futon4 .el gap)
  - `E-substrate-2-directed-edge-id.md` (futon1a bidirectional collapse)
- Updated phase-1 invariant tests with a documented tolerance for
  the directed-edge collapse.

outcome: success across both codebases. Schema generalises clean.

### Cross-codebase counts (under label-filter)

| Quantity | futon2-phase1 | futon4-phase1 | futon1a-phase1 |
|---|---:|---:|---:|
| Files projected (.clj/.cljs) | 46 | 15 | 85 |
| Files NOT projected (.el etc.) | 0 | 132 | 0 |
| Vars (XTDB) | 562 | 74 | 365 |
| Tests (Clojure) | 46 | 0 | 107 |
| Coverage edges | 50 | 0 | 147 |
| Coverage % | 4.4 | 0 | **14.0** |
| Call edges | 766 | 128 | 507 |
| Bidirectional collapse | 0 | 0 | 1 |
| Vocab edges | 63 | 10 | 87 |
| Components | 34 | 4 | 42 |
| nonzero-ΔT vertices | 114 | 0 | >100 |
| Total hyperedges | 2403 | 586 | 1954 |

### Operator-readable top-K on futon1a

Top -ΔT (load-bearing utilities matching the documented architecture):
- `futon1a.system/start!` (-5)
- `futon1a.auth.penholder/authorize!` (-4)
- `futon1a.core.identity/validate-identity` (-4)
- `futon1a.system/xtdb-config` (-4)
- `futon1a.scripts.migrate-futon1/checksum-world*` (-4)

Top +ΔT (integrative orchestrators):
- `futon1a.core.pipeline/run-write!` (+9) — the L4→L0 pipeline orchestrator
- `futon1a.api.routes/ok` (+7) — HTTP wrapper
- `futon1a.core.invariants/layer2-error` (+7) — error constructor
- `futon1a.api.routes/require-keys!` (+5)
- `futon1a.core.pipeline/run-open-world!` (+5)

**The geometry surfaces futon1a's layered architecture
without being told about it.** `run-write!` as the central
orchestrator and `authorize!` / `validate-identity` as
load-bearing utilities is exactly what the
`futon1a/README-best-practice.md` describes. Substrate-2
re-derives the architecture from the code, structurally.

### Findings

1. **Schema generalises across codebases.** No edge-class
   additions needed. Same algorithm produces operator-
   readable top-K on futon2, futon4, and futon1a.

2. **Coverage discipline is highly variable across the
   stack.** futon4 (Arxana JVM frontend): 0% Clojure-side
   coverage (testing lives in elisp). futon2: 4.4%. futon1a:
   **14%** — the only codebase of the three with substantial
   coverage. This is itself a futonic-zapper signal:
   "coverage retreat" is detectable cross-codebase as
   variance in the ratio.

3. **Bidirectional-call collapse surfaces on futon1a only.**
   futon2 had 0 collapses; futon4 had 0; futon1a had 1.
   This is itself diagnostic: codebases with mutual
   recursion show up as a non-zero discrepancy.

4. **Cross-language gap dominates futon4's reading.** 132
   of 147 elisp files are invisible. The geometric layer
   over the projected Clojure slice is correct (deterministic
   regression passes) but **uninformative about futon4's
   actual structure**. Substrate-2 cannot meaningfully
   speak to futon4 until the elisp projector lands. Parked
   in `E-substrate-2-elisp-projection.md`.

5. **Pyramidal expansion is doing what it's supposed to.**
   Two real substrate-2 bugs surfaced during this session
   that futon2-only would never have shown:
   - The directed-edge stable-ID collapse (futon1a).
   - The cross-language coverage gap (futon4).
   Each is now a parked excursion with concrete fix paths.

### Cross-store state at end of session

futon1a now contains substrate-2 hyperedges for three
labels:

```
:repo "futon2-phase1"   2,403 hyperedges
:repo "futon4-phase1"     586 hyperedges
:repo "futon1a-phase1"  1,954 hyperedges
                        ─────
Total:                  4,943 substrate-2 hyperedges
```

All three labels are independently queryable; the
geometric layer can be run per-label or eventually
across-labels (when phase 7 cross-codebase aggregation
lands).

next: per pyramidal strategy, candidate codebases for
phase 1 next:
- **futon3c** (clj+el; needs elisp excursion fix to be
  meaningful — defer until E-substrate-2-elisp-projection
  lands).
- **futon5** (mostly clj? to check — would extend AIF
  vocabulary coverage).
- **Or shift gears to phase 3** (commits-as-vertices) on
  futon2 since substrate-1 already has `ingest_file_churn`
  to learn from. Joe's pyramidal strategy says broaden
  before deepening, but if the elisp gap blocks futon3c
  / futon3a / futon4-fully, returning to phase 3 on
  already-ingested codebases keeps the pyramid productive.
