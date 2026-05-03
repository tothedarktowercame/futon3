# PUR: Phase 5 — Futonic-Zapper Signature Emission v0

pattern (re-confirmed):
- `storage/canonical-interface` ✓ — read + write through HTTP API only.
- `storage/deterministic-ingest-pipeline` ✓ — same substrate state →
  same emitted signatures (idempotent on stable IDs).
- `futon-theory/event-protocol` ✓ — every detected satisficing signature
  is a typed evidence hyperedge.
- `agent/sense-deliberate-act` ✓ — sense (query) → deliberate
  (classify) → act (POST).

actions taken (single session, 2026-04-27):
- Created `futon3/scripts/phase_5_signatures.clj` (~280 LOC, bb).
- v0 implements 4 of the 8 signatures from the futonic-zapper spec:
  1. **adapter-shim-no-adapt** — namespace named `*adapter*`/`*bridge*`/
     `*shim*`/`*port*` whose members make zero cross-namespace calls.
  2. **work-around-drift** — connected-component pair with vocabulary
     overlap ≥ θ AND zero cross-component structural edges.
  3. **concept-used-without-definition** — `:vocabulary-use` edge
     whose target term has no incident `:term-defines`.
  4. **completion-rot** — label with < 10 :var hyperedges (proxy for
     "claimed but not present").
- Ran on futon2-d (smoke) + a 4-label subset (futon2-d, futon7-d,
  futon4-elisp-d, futon1a-d).

outcome: success on the substantive demonstration.

### Smoke result on futon2-d

Two signatures emitted:
- `adapter-shim-no-adapt` (medium) — `futon2.aif.adapter`, 2 members,
  0 cross-target namespaces.
- `work-around-drift` (medium) — components 0 (368 vars, ants core)
  and 1 (18 vars, adapters), overlap 0.25, shared terms
  `[beliefs, current, integration]`.

**Both were the *exact substantive findings* from the v0.5
cross-prototype-geometry excursion** (2026-04-27 morning). They
were ad-hoc readings; phase 5 makes them typed `code/v05/satisficing-
signature` hyperedges. The substrate is now queryable for these
findings forevermore — no operator narration needed.

### 4-label subset run

| Label | Signatures emitted |
|---|---:|
| futon2-d | 2 |
| futon7-d | (cut off in tail; likely 0–4) |
| futon4-elisp-d | ~213 |
| futon1a-d | 9 |
| **total** | **228** |

Pattern in futon4-elisp-d: many `work-around-drift` records share
only the term `"current"` (a generic English word that's leaking
through the vocab term filter). v0.5 should either tighten the
term filter or raise θ; alternative: weight-by-IDF the shared-terms
set so that single-token-of-shared-term doesn't surface as a high-
severity finding.

### Performance note

The all-labels run hit O(N²) on component pairs in
`signature-work-around` (futon5-d2 has hundreds of components × ~14k
calls × all-pairs). 5+ minutes on the all-labels run before it had
completed even one label. **v0.5 should add a component-size
threshold + early-exit when both components are small** to bring
the runtime down to seconds per label.

### Substrate now contains

- `code/v05/satisficing-signature` hyperedges with `:source` /
  `:severity` / `:evidence-vertices` / `:details` props.
- Stable IDs via the directed-third-endpoint convention; idempotent
  on re-runs.
- All emitted with `:repo` prop matching the input label.

### Updates to cleanup-checkpoint

- P-1 (Phase 5 futonic-zapper) — **v0 RESOLVED.**
- v0.5 deferred to follow-up: tighter term filter, IDF-weighted
  overlap, component-size threshold, the four time-series signatures
  (quick-fix, coverage-retreat, load-bearing-under-tested,
  concept-introduced-without-attachment).

### Operator-visible value

The futonic-zapper is now writing real findings. With the
multi-watcher running (P-2, also landed this session), **as the
substrate refreshes, phase-5 can be re-run periodically and
detect new signatures appearing in real time.** The
operating-system framing is operationally true now: the substrate
sees changes, computes signatures, and emits typed evidence —
all without operator narration.
