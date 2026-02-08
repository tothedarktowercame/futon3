# Mission: Agency Rebuild (A0-A5 Invariants)

**Status:** :scoped
**Date:** 2026-02-08
**Owner:** Joe (handoff requires explicit update to this header)
**Primary:** Codex
**Reviewer:** Claude
**Parent:** `holes/missions/M-agency-forum.md`
**Enables:** `holes/missions/M-agency-forum.md` (Agency layer), `holes/missions/M-par-session-punctuation.md` (reliable coordination substrate)

## Motivation

Agency exists because futon3’s core job (per `holes/futon3.devmap`) is to turn messy activity into organised knowledge by running pattern-related checks against a canon and producing auditable records (proof-states, trails, workday instrumentation) that other futons can use; that only works if there is a stable coordination layer that can reliably route requests across the actual places work happens (humans, agent processes, forum threads, MUSN rooms, WebSockets, CLI runners) while preserving continuity and producing deterministic transcripts. We have already seen the approach work at the “agent runtime” level via Drawbridge: a single agent session can multiplex inputs from multiple sources (HTTP, WebSocket, Agency messages) into one coherent conversation context, which is exactly the kind of bridge that makes higher-level pattern checking and journaling feasible instead of bespoke scripts. The rebuild is about making Agency a trustworthy backbone for that devmap argument by making its guarantees explicit, testable, and enforced.

## Patterns (Normative)

Invariant set:
- `library/agency/invariants.flexiarg`

Supporting patterns (must guide implementation and tests):
- `library/agency/delivery-receipt.flexiarg` (A0 delivery receipts)
- `library/agency/single-routing-authority.flexiarg` (A1 single routing authority)
- `library/agency/self-attribution.flexiarg` (A1 agents speak for themselves)
- `library/agency/state-atomicity.flexiarg` (A2 all-or-nothing state transitions)
- `library/agency/loud-failure.flexiarg` (A3 loud failure / error hierarchy)
- `library/agency/bounded-lifecycle.flexiarg` (A5 bounded resources)
- `library/agency/identifier-separation.flexiarg` (supports A1/A2: typed IDs across layers)

Pattern-coherence checks for this mission doc and its follow-ons:
- `library/pattern-coherence/scope-boundaries.flexiarg`
- `library/pattern-coherence/internal-coherence.flexiarg`
- `library/pattern-coherence/evidence-alignment.flexiarg`

Related completed missions (evidence and prior art, not the rebuild itself):
- `holes/missions/M-make-agency-work-properly.md` (session continuity and identifier separation failures)
- `holes/missions/M-agency-unified-routing.md` (agent registry introduced)

## Scope

### Scope In

- Make Agency uphold A0-A5 in code, tests, and observable evidence.
- Replace multi-tier routing split-brain with single routing authority semantics (including WS and local/in-JVM paths).
- Implement deterministic lifecycle management for all transient in-memory resources (no fire-and-forget TTL futures as the primary mechanism).
- Enforce identifier separation across layers (transport IDs vs LLM continuity IDs vs protocol IDs).
- Replace boolean/best-effort send semantics with receipt-or-explicit-failure result types for bell/whistle and for outbound HTTP integrations.
- Make loud-failure behavior mechanical: remove catch-and-swallow, add explicit error results and context.
- Add proof tests for invariants and at least one integration test that exercises Drawbridge multiplexing via Agency (WS + HTTP inputs converge into one agent session).

### Scope Out

- Re-architecting all of futon3 (Forum, MUSN transport, pattern canon, futon1/futon2 exports) beyond the explicit Agency boundary and its interfaces.
- New peripheral features unrelated to Agency correctness (new tools, new bridges, new UI).
- Making Agency production-grade as a scheduler/daemon manager (supervision, load balancing, multi-node HA) unless required to satisfy A0-A5 proofs.

## Dependencies

### Blocks (Hard Prerequisites)

- None identified. This mission is a leaf that can proceed immediately.

### Enables (Soft Prerequisites / Prior Art)

- `holes/missions/M-agency-unified-routing.md` (done): agent registry prior art for single routing authority.
- `holes/missions/M-make-agency-work-properly.md` (done): session continuity and identifier separation failures and fixes.

### Shares (Shared Decisions / Resources)

- Shares interfaces and identifier vocabulary with `holes/missions/M-agency-forum.md` and `holes/missions/M-agency-forum.md`'s child missions.
- Depends operationally on MUSN transport being available for standup self-reporting flows, but this is not a correctness blocker for the invariant proofs themselves.

## Success Criteria (Top-Level)

1. A0-A5 proof tests exist and are green under `clj -M:test` (new `test/futon3/agency/invariants/` suite).
2. Single routing authority: for any agent-id, exactly one authoritative route exists at a time, and all message sends resolve against it (no concat/distinct across stores).
3. Identifier separation enforced: forum thread IDs cannot clobber LLM resume IDs; transport/session/protocol IDs are typed and logged with type.
4. Bounded lifecycle: no Agency state atom grows without bound in normal operation; pending requests are deterministically cleaned; disconnect cascades are explicit.
5. Loud failure + debugging: failures surface at the causing layer with agent-id + operation + cause; at least one “known failure” scenario can be diagnosed from logs alone within 10 minutes.

## Time Box

5 focused days total, split across phases below. If any phase exceeds its budget by >50%, stop and split into a narrower mission rather than expanding scope.

## Exit Conditions

- Stop when the success criteria above are met; create follow-on missions for nice-to-haves.
- Stop when the time box expires; re-scope and split remaining work into smaller missions with explicit owners.
- Stop immediately if satisfying any invariant requires expanding beyond Scope Out boundaries; escalate via a new mission proposal with tests that justify the expansion.

## Work Plan (Concrete Rebuild Outline)

### Phase 0: Base Camp (0.5 day)

- Confirm current Agency boundary and interfaces:
  - HTTP endpoints: run/rollover/whistle/bell/connected/ack/secret/rendezvous.
  - WS protocol and message types (bell/whistle + whistle-response).
  - Drawbridge integration paths (local handler, WS client).
- Inventory the wire protocol mismatch and decide the transition direction.
- Document the canonical “typed identifiers” vocabulary as a concrete key-name table.
- Inventory Agency state atoms and their current lifecycle mechanisms.

#### Phase 0 Outputs (Decision + Inventories)

**Wire protocol decision (page vs whistle):**
- Decision: Agency sends `{"type":"whistle"}` on the wire for sync request/response.
- Backwards compatibility:
  - HTTP endpoint `/agency/page` remains an alias of `/agency/whistle`.
  - Receive side accepts `{"type":"page"}` as an alias where needed.
- Rationale: “page” was always a misnomer; “whistle” captures the synchronous contract. Migration is safe because receive-side aliasing covers mixed fleets.

**Typed identifier vocabulary (key-name table):**

Transport-layer identifiers (addressing where to post/stream):
| Key | Meaning | Where used |
|---|---|---|
| `forum.thread-id` | Forum thread address | Agency run payload `:forum {:thread-id ...}`, Forum dispatch path |
| `forum.post-id` | Forum post address (if present) | Forum layer (future/optional) |
| `musn.url` | MUSN base URL | Agency run payload `:musn {:url ...}`; standup payload |
| `musn.session-id` | MUSN session identifier | Agency run payload `:musn {:session-id ...}` |
| `agency.url` | Agency base URL | Peripheral prompts and standup bell payloads |
| `agency.ws-url` | Agency WebSocket URL | Drawbridge/agent WS clients |

Continuity-layer identifiers (what the LLM runner resumes):
| Key | Meaning | Where used |
|---|---|---|
| `resume-id` | LLM session/thread id to resume (Codex/Claude) | `/agency/run` and `run-peripheral!` payload; persisted as `:agent/current-thread-id` |
| `agent.current-thread-id` | Persisted LLM resume id (LLM continuity only) | `src/futon3/agency/service.clj` state |

Protocol identifiers (coordination proofs and request correlation):
| Key | Meaning | Where used |
|---|---|---|
| `agent-id` | Logical agent identity | Everywhere (routing + attribution) |
| `bell-type` | Bell subtype | `{"type":"bell","bell-type":"..."}` |
| `secret-id` | Secret handle for ack/receipt | test-bell + rendezvous + standup |
| `request-id` | Correlates whistle request/response | `{"type":"whistle","request-id":"..."}` and `whistle-response` |
| `rendezvous-id` | Standup rendezvous identifier (currently equals secret-id) | `/agency/rendezvous/:id` and standup bell payload |
| `session-id` | Transport-session id on WS connect (not LLM resume id) | Agency WS query param and connected-agents entry |

**Agency state atoms (current lifecycle status):**

| Atom | Location | Purpose | Current lifecycle | Notes / risks |
|---|---|---|---|---|
| `secrets` | `src/futon3/agency/http.clj` | transient secrets for test-bell + rendezvous | TTL via `future (sleep ttl)` then `dissoc` | race window; not lazy-validated |
| `acks` | `src/futon3/agency/http.clj` | records ack values keyed by secret-id | unbounded | violates A5 unless bounded/evicted |
| `rendezvous-state` | `src/futon3/agency/http.clj` | standup rendezvous tracking (expected/acked) | TTL via `future` cleanup | late-ack ambiguity; should cascade/close deterministically |
| `connected-agents` | `src/futon3/agency/http.clj` | WS-connected agents and channels | removed on WS close; no TTL sweep | liveness only via ping timestamp; no cascade to pending whistles |
| `local-handlers` | `src/futon3/agency/http.clj` | legacy in-JVM drawbridge handlers | manual register/unregister | can conflict with registry; needs single authority story |
| `pending-whistles` | `src/futon3/agency/http.clj` | in-flight whistle promises | removed on response; timeout cleanup via `future` | orphan risk on disconnect; cleanup races |

Note: Agency also has a local agent registry atom (`futon3.agency.registry/agent-registry`) that is intended to become the single routing authority; its lifecycle should be reviewed together with the above during A1 work.

### Phase 1: Spec-to-Tests (1.5 days)

- Create a proof-test harness for each invariant (A0-A5), initially failing where reality diverges.
- Add an enforcement check for A3 (no silent catch-and-swallow) that fails the build if forbidden patterns exist in `src/futon3/agency/`.
- Add an integration test covering:
  - WS connect + register + whistle roundtrip.
  - Local/in-JVM agent invocation path (registry).

#### Phase 1 Results (2026-02-08)

Test run command:
- `clj -X:test`

Result summary:
- `Ran 134 tests containing 365 assertions.`
- `18 failures, 1 errors.`

Notes:
- The Phase 1 deliverable landed as invariant proof tests under `test/futon3/agency/invariants/` plus `test/futon3/agency/integration_test.clj`.
- Several failures/errors are pre-existing outside the Agency invariant work (e.g., golden transcript drift, fulab harness JSON encoding, similarity determinism, library coherence); Phase 2 should not treat these as Agency rebuild blockers unless they are causally linked.

Agency rebuild gaps now explicitly documented as failing tests (expected):
- A0: `send-to-agent!` returns boolean rather than explicit receipt/failure; whistle failure omits `:agent-id`; `:throw-exceptions false` call sites lack status checks.
- A1: the same `agent-id` can exist in multiple routing stores simultaneously (registry + local-handlers + ws), violating single-routing-authority semantics.
- A2: corrupted agent state loads as nil; continuity id can be set to nil; persistence write path is not atomic (no temp+rename).
- A3: silent catch-and-return-nil sites exist; `:throw-exceptions false` call sites lack explicit status checks.
- A5: `acks` is unbounded; disconnect does not cascade cleanup to pending whistles; secrets rely on async cleanup (TTL is not enforced lazily-on-read).

Passing Agency integration coverage (sanity checks):
- Registry whistle roundtrip via HTTP handler passes: `test/futon3/agency/integration_test.clj`
- Bell + ack roundtrip passes: `test/futon3/agency/integration_test.clj`
- Rendezvous ack tracking passes: `test/futon3/agency/integration_test.clj`

### Phase 2: Implementation (2 days)

- A1: unify routing resolution into a single authority and remove multi-tier “distinct concat” behavior.
- A0: change send APIs to return receipt/failure results; propagate non-2xx from outbound HTTP calls as explicit failures.
- A5: replace future-based TTL cleanup with deterministic lifecycle (lazy-on-read, periodic sweep, and/or cascaded cleanup).
- A2 + identifier separation: make continuity state update rules explicit and enforced (LLM resume IDs only in continuity slots).
- A3: remove silent catches, return typed errors with layer attribution.

#### Phase 2 Results (2026-02-08)

Phase 2 implementation landed in these commits:
- A3 loud failure status checks + eliminate catch `_ nil`: `5057c38`
- A1 enforce single routing authority (cross-store eviction watches): `290b0e9`
- A0 explicit delivery receipts for `send-to-agent!` + whistle error attribution: `ea73e0a`
- A5 bounded lifecycle (acks bounded, deterministic secret TTL on read, disconnect cascades): `798481b`
- A2 atomic state (corruption loud, atomic state writes, continuity non-nil): `67cabf4`

Acceptance results:
- Agency invariant proof tests are now green:
  - `test/futon3/agency/invariants/a0_delivery_test.clj`
  - `test/futon3/agency/invariants/a1_identity_test.clj`
  - `test/futon3/agency/invariants/a2_atomicity_test.clj`
  - `test/futon3/agency/invariants/a3_loud_failure_test.clj`
  - `test/futon3/agency/invariants/a5_bounded_test.clj`
- Agency integration tests remain green:
  - `test/futon3/agency/integration_test.clj`

Notes:
- A full `clj -X:test` run still reports failures/errors outside Agency (golden transcript drift, fulab harness JSON encoding, similarity determinism, library coherence). These are not treated as Agency rebuild blockers unless causally linked.

### Phase 3: Validation + Evidence (1 day)

- Run a “soak” style test (bounded time) that sends bells/whistles repeatedly and asserts bounded growth and deterministic cleanup.
- Exercise standup rendezvous flow (bell + ack + self-report) end-to-end and assert:
  - delivery receipts recorded,
  - self-attribution preserved,
  - explicit failure on missing ack/timeout.
- Record evidence pointers (logs, metrics, test runs) that directly support the invariant claims.

## Notes: Scope Check (against futon-theory/mission-scoping)

This mission explicitly declares owner, scope in/out, success criteria, time box, and exit conditions. If the rebuild work expands into Forum or MUSN redesign, that is a scope breach and must be split into a new mission rather than absorbed here.
