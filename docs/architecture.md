# MUSN Architecture (Futon3)

## Guiding Principles
- Small, replaceable components with explicit contracts and EDN/JSON boundaries.
- Deterministic graph core (Datascript) with provenance captured at ingest time.
- Transport never blocks graph/semantics; core.async buffers provide back-pressure and replay.

## Module Map
| Layer | Namespace | Responsibilities |
|-------|-----------|------------------|
| f0 | `f0.clock` | Time abstraction, rev + monotonic timestamps for events and sessions. |
| f1 | `f1.graph` | Datascript connection owner; exposes `init`, `put!`, `q`, `tx-log`, `snapshot`, `load-fixture!`, schema management. |
| f2 | `f2.transport` | HTTP + WebSocket endpoints (http-kit). Validates protocol v1 frames, enforces quotas/timeouts, pushes sanitized commands to graph/semantics via core.async. Hosts optional REPL wiring. |
| f2 | `f2.repl` | Optional evaluator: SAFE mode via SCI sandbox + MUSN DSL, ADMIN mode via gated unsafe eval (token + local network). |
| repl | `repl.http` | Drawbridge server for nREPL-over-HTTP (CIDER) access with token/IP guarding. |
| f2 | `f2.semantics` | Lightweight reasoning: relationship suggestions, pattern instantiation, consistency checks with provenance + confidence. Pure functions fed by snapshots. |
| f2 | `f2.ui` | HTTP views for People/Practices/Sessions plus `POST /musn/export` to write `scenario.edn`. Delegates storage to graph + semantics. |
| f2 | `f2.musn` | System orchestrator. Boots clock, graph, transport, semantics, ui; manages lifecycle for `make dev/demo` targets. |

## Dataflow Narrative
1. **Ingest**: Transport accepts `hello|event|session|eval|bye` frames. Each event/session JSON is decoded, validated via schema spec (resources/schemas). Optional `eval` frames are routed to `f2.repl` when enabled. Valid data are converted to EDN and placed onto per-client async channels capped at 512 pending messages.
2. **Normalization**: `f2.musn` drains channels, annotates provenance/time using `f0.clock/now` + graph schema version, and calls `f1.graph/put!`.
3. **Storage**: Graph transacts against Datascript using schema keyed by entity kinds (person, practice, session, pattern). `tx-log` exposes immutable history for replay/demos. `snapshot` produces consistent value passed to semantics.
4. **Semantics**: On demand or on schedule, semantics functions analyze graph snapshots to propose links, instantiate patterns, or flag consistency issues. Output is EDN list of maps with `:confidence`, `:support-eids`, and `:prov` to ensure auditability.
5. **Presentation**: UI handler adapts graph + semantic data to JSON responses for `/musn/people`, `/musn/practices`, `/musn/sessions`. Export endpoint writes aggregated session scenario to `/dev/scenario.edn`. SAFE REPL results are also logged back into history for transparency.

## Error & Resilience Model
- **Transport**: Each client gets bounded queues and 200 ms processing budget per ingest message (100 ms for SAFE `eval`). Oversized payloads or timeouts return `{status 429|408}` plus diagnostic code; channel drained. Replayed events are deduplicated via caller-supplied `:id` + checksum tracked in graph.
- **REPL**: Default `:repl.mode :off` rejects eval requests (`repl-disabled`). SAFE mode uses SCI sandbox (allowlisted namespaces, no interop, 64 KB code / 1 MB result caps). ADMIN mode requires matching token + local IP (`127.0.0.1`/`::1`) before falling back to native `load-string`.
- **Graph**: Schema version increments stored in `:musn/schema-version`. Unknown types -> `ex-info` with `:graph/error :unknown-type`. `put!` always returns `[tx-report error]`; callers must check.
- **Semantics/UI**: Pure functions return data-or-error map. UI wraps all responses in `{status body}`; errors logged and surfaced as machine-readable JSON.
- **Dev Flow**: `make dev` spins up musn system (graph + transport + ui). `make demo` seeds Datascript via demo NDJSON, then exposes `/musn/sessions` for inspection. `make test` runs per-module unit tests + golden fixtures.

## Replaceability Hooks
- Graph API only depends on `datascript.core/conn`. Future XTDB swap is isolated behind the same protocol.
- Transport boundaries are JSON/EDN; semantics never sees sockets.
- `f0.clock` swappable for deterministic testing – tests inject fixed instants.

## Future Work Notes
- Move persistence from in-memory snapshots to XTDB once ready.
- Expand schema registry under `/resources/schemas` to drive validation + codegen.
- Build CLI commands for `bye` and structured replay once QA adds more fixtures.
