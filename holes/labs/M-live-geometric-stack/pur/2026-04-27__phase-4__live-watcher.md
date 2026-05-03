# PUR: Phase 4 — Live Watcher

pattern (re-confirmed):
- `realtime/liveness-heartbeats` ✓ — watcher emits a
  `code/v05/watcher-event` per cycle whether or not source
  changed; substrate has a continuous liveness signal.
- `storage/deterministic-ingest-pipeline` ✓ — re-ingest is
  idempotent via stable IDs; watcher does no dedupe.
- `agent/sense-deliberate-act` ✓ — fs scan → hash compare
  → ingest decision → POST is the AIF inner loop.
- `futon-theory/event-protocol` ✓ — every cycle is a typed
  event hyperedge with structured props.
- `storage/canonical-interface` ✓ — HTTP API only; no JVM
  patching of futon1a (per CLAUDE.md no-restart rule).
- `storage/durability-first` ✓ — cycle is "complete" only
  when watcher-event POST returns 200.

actions taken (single session, 2026-04-27):
- Created `futon3/scripts/watcher_daemon.clj` (210 LOC, bb).
- Created
  `futon3/holes/labs/M-live-geometric-stack/tests/phase_4_watcher_test.clj`
  (170 LOC).
- Smoke-tested on futon2-d (2 cycles, no edits): cold cache
  → 58 files ingested in 3s; warm cache → 26ms heartbeat.
  Both cycles emitted watcher-event hyperedges.
- Ran invariant test on futon2-d: 5/5 PASS.

outcome: success.

| Invariant | Result |
|---|---|
| Heartbeat — watcher-event hyperedge per cycle | PASS |
| Ingestion path — file change → substrate update | PASS (mid-run mutation picked up within 1 cycle) |
| Discovery — new defn between cycles is captured | PASS (`watcher-saw-this` landed) |
| Idempotency — second run on unchanged tree adds 0 vars | PASS (566 == 566) |
| Heartbeat — even no-change runs emit events | PASS (6 → 8 over 2-cycle second run) |

Total tests across all phases / all codebases now: **265 PASS / 0 FAIL.**

prediction errors:

1. **Watcher-event stable-ID collision across runs** —
   first cycle of run-A and first cycle of run-B both
   produced the stable ID
   `hx:code/v05/watcher-event:<repo>/cycle-1.1.dir:...`,
   so futon1a's L1 dedupe collapsed them. The first
   invariant-test run failed two assertions because of
   this. Same root cause as
   `E-substrate-2-directed-edge-id.md`. Fix: include a
   per-run-id (process-start timestamp) in the
   evidence-id endpoint:
   `<repo>/run-<ts>/cycle-<n>`. Each invocation now
   produces distinct hyperedges. Tests then 5/5 PASS.
   This is the same lesson as before: any time-series-of-
   typed-events on a substrate-1 store needs a
   per-instance discriminator in the endpoint.

2. **v0 simplification was the right call.** Re-running
   the entire phase-1 ingest on every detected change
   feels wasteful but is *correct* — idempotent, no
   per-file resolution complexity, no risk of stale
   per-file state. Smoke test showed 3s cold and 26ms
   warm; the watcher does NOT re-ingest on heartbeat
   cycles (only when files actually changed). Phase 4.5
   per-file optimization is worth it for futon3c-scale
   tests but not for phase 4 v0.

### What this delivers

The mission's central system invariant — "the hypergraph
projection is always current with the on-disk state" —
is now enforced by code, not aspiration. Concrete:

- A long-running `bb watcher_daemon.clj --root <repo>
  --label <tag>` keeps any single repo current with its
  filesystem.
- Heartbeat cadence (5s default) gives operators a
  liveness signal queryable as `code/v05/watcher-event`
  count over time.
- The watcher restart-safe: hash cache persisted to
  `/tmp/substrate2-watcher-<label>.edn`; cold restart
  re-discovers state without false re-ingestion.
- The watcher composes with phases 1-3 without changes:
  ingest_v05_to_futon1a.clj is invoked unmodified.
- The watcher composes with the elisp projector: scans
  .el alongside .clj/.cljs/.cljc.
- The watcher emits self-instrumentation evidence per
  the parent mission's discipline, exactly the shape
  named in the §"Implementation discipline" sketch.

### What phase 4 does NOT yet do (queued for phase 4.5)

- **Per-file ingest.** v0 re-runs the full repo ingest
  on any change. For small repos this is fine (~3s);
  for futon3c-scale (~25s) it would mean the watcher
  is "behind" most of the time during heavy editing.
- **inotify integration.** Polling has 5s lag; inotify
  would make changes propagate in sub-second time.
- **Cascade staleness.** When file F changes, vertices
  whose source is F are re-ingested; their incoming
  edges (records that depend on those vertices) should
  be flagged `:edge/witness-stale`. v0 does not yet
  emit staleness flags.
- **Multi-repo orchestrator.** v0 is one watcher per
  repo. Production would want one orchestrator
  watching all configured roots in one process.
- **Git post-commit hook.** Could complement polling
  by triggering immediate cycles on commit.

These are all phase 4.5 / phase 5+ work, explicitly out
of scope for v0.

### Phase ordering and what next

Per Joe's stick-to-the-ordering directive (2026-04-27),
phase 4 is now done. Phase 5 (futonic-zapper signature
emission) is next.

The substrate now has everything the seven satisficing-
signatures need:
- Quick fix → ΔT redistribution: needs commit-vertices
  (phase 3 ✓) + ΔT computation (phase 2 ✓).
- Work-around → drift-without-structure: needs vocab
  (phase 1 ✓) + components (phase 2 ✓).
- Adapter shim that doesn't adapt: needs namespace
  + calls (phase 1 ✓).
- Coverage retreat: needs commit-time series of
  paired-vars count (phase 3 ✓).
- Load-bearing under-tested: needs |ΔT| time series
  (phase 3 ✓).
- Concept introduced without attachment: needs vocab
  doc commits + vocab-use edges (phases 1+3 ✓).
- Concept used without definition: needs vocab + use
  edges (phase 1 ✓).

Plus the new "completion rot" signature surfaced
during phase 1 pre-flight.

Phase 5's job is to write each signature as an
XTDB-shaped query against the substrate, emit the
result as a `code/v05/satisficing-signature` hyperedge,
and demonstrate operator-readable output on
futon2/futon4-elisp/futon1a/futon3c/futon4-d.

connections:
- The watcher daemon makes phase 5's signatures
  *continuously* available, not just one-shot. Phase 5
  signatures emitted by the watcher are the start of
  the futonic-zapper itself.
- M-stack-stereolithography's "exists / coming out of
  the bath" semilattice is now queryable continuously
  rather than at ingest-script invocation time.
