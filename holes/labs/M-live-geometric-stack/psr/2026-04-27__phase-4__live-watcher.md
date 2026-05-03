# PSR: Phase 4 — Live Watcher

context: phases 1-3 produced ≈74,000 substrate hyperedges across
4 codebases (futon2, futon4, futon4-elisp, futon1a, futon3c).
The substrate is meaningfully large; "re-run the script every
time" is no longer adequate. Phase 4 promotes the substrate
from "static one-shot ingestion" to "live and current" — the
parent mission's central system invariant ("the hypergraph
projection is always current with on-disk state") becomes
substrate-enforced rather than aspirational.

patterns:
- `realtime/liveness-heartbeats` — the watcher emits a
  heartbeat (cycle-event evidence) on every poll, not just
  on actionable changes. Operator can see "watcher is alive"
  without inspecting logs.
- `storage/deterministic-ingest-pipeline` — re-ingests are
  idempotent via stable IDs (phases 1-3); the watcher does
  not need its own dedupe.
- `agent/sense-deliberate-act` — the watcher is the
  sense-loop: filesystem events → hash compare → ingest
  decision → POST.
- `futon-theory/event-protocol` — every watcher cycle emits
  a typed evidence hyperedge (`code/v05/watcher-event`).
  Self-instrumentation is per the mission's discipline.
- `storage/canonical-interface` — writes through the same
  HTTP API as phases 1-3; no bypass of futon1a's pipeline.
- `storage/durability-first` — a watcher cycle is "complete"
  only when L0 success is recorded for the cycle's evidence.

decision:
- New script `futon3/scripts/watcher_daemon.clj` (bb).
  Long-running process; one watcher per repo (configurable).
  Polling-based v0 (default 5s interval); inotify upgrade
  is phase 4.5 if polling latency proves inadequate.
- Per-cycle algorithm:
  1. Walk repo for all source files (clj/cljs/cljc/el).
  2. Compute SHA-256 of each file.
  3. Compare with in-memory hash cache (also persisted to
     `/tmp/substrate2-watcher-<label>.edn` for restart safety).
  4. If any file's hash changed (or cache is cold), invoke
     the existing `ingest_v05_to_futon1a.clj` for the repo
     (idempotent against substrate; only new/changed edges
     actually land).
  5. Emit a `code/v05/watcher-event` hyperedge (one per
     cycle) with props
     `{:repo :ts :cycle-n :files-scanned :files-changed
       :duration-ms :phase "v0-poll"}`.
  6. Update cache.
- v0 simplification: full-repo re-ingest on any change.
  Per-file ingest is phase 4.5 work (saves CPU during heavy
  editing). Phase 4 v0 prioritises *correctness* over
  *throughput*.
- L0 invariant: cycle is complete only when the
  `watcher-event` POST returns 200. If it fails, the cycle
  is logged as failed and retried next tick.
- L2 invariant: hash-cache state is consistent with
  substrate. If the cache is restored from disk on restart
  but substrate has been wiped, the next cycle's
  re-ingest catches up (idempotent).

alternatives:
- inotify-based watcher (rejected for v0: native dep
  complications; bb-only is portable).
- futon1a server-side ingestion (rejected: per-CLAUDE.md
  no-restart rule on futon1a; we cannot patch the JVM
  in-place to do server-side filesystem watching).
- Per-file ingest from start (rejected for v0: simpler
  to start with whole-repo; phase 4.5 can optimize once
  we have measurements).
- Git post-commit hook (rejected for v0 alone; could
  *complement* polling later by triggering an immediate
  cycle).

outcome (target):
- `bb watcher_daemon.clj --root /home/joe/code/futon2
  --label futon2-d` runs forever, polling every 5s,
  emitting one watcher-event hyperedge per cycle.
- Modifying a file in the watched repo causes a re-ingest
  within ≤ 2 cycles (≤ 10s).
- Idempotent: untouched files = zero new substrate edges.
- Self-instrumentation: `code/v05/watcher-event` count
  is monotonic; queryable via futon1a as a liveness
  signal.
- Tests pass: latency invariant (file change → substrate
  update within 10s), idempotency (cycles with no
  changes = no new substrate edges), self-instrumentation
  (every cycle = one watcher-event).

confidence: medium. The polling loop is straightforward;
the risk is the existing ingest's runtime under heavy
load — futon3c phase 1 took ~25s, so a 5s polling
interval would mean the watcher is "behind" most of the
time during heavy editing on a big codebase. Acceptable
for v0; phase 4.5 (per-file ingest + inotify) is the
optimization. The watcher is a *correctness* device for
phase 4, a *throughput* device for phase 4.5.
