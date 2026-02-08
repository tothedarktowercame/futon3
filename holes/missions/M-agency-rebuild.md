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
  - WS protocol and message types (bell/whistle-response).
  - Drawbridge integration paths (local handler, WS client).
- Inventory current Agency state atoms and their lifecycle requirements (secrets, acks, rendezvous-state, pending requests, connected agents, registry).
- Decide and document the canonical “typed identifiers” vocabulary used in payloads and state.

### Phase 1: Spec-to-Tests (1.5 days)

- Create a proof-test harness for each invariant (A0-A5), initially failing where reality diverges.
- Add an enforcement check for A3 (no silent catch-and-swallow) that fails the build if forbidden patterns exist in `src/futon3/agency/`.
- Add an integration test covering:
  - WS connect + register + whistle roundtrip.
  - Local/in-JVM agent invocation path (registry).

### Phase 2: Implementation (2 days)

- A1: unify routing resolution into a single authority and remove multi-tier “distinct concat” behavior.
- A0: change send APIs to return receipt/failure results; propagate non-2xx from outbound HTTP calls as explicit failures.
- A5: replace future-based TTL cleanup with deterministic lifecycle (lazy-on-read, periodic sweep, and/or cascaded cleanup).
- A2 + identifier separation: make continuity state update rules explicit and enforced (LLM resume IDs only in continuity slots).
- A3: remove silent catches, return typed errors with layer attribution.

### Phase 3: Validation + Evidence (1 day)

- Run a “soak” style test (bounded time) that sends bells/whistles repeatedly and asserts bounded growth and deterministic cleanup.
- Exercise standup rendezvous flow (bell + ack + self-report) end-to-end and assert:
  - delivery receipts recorded,
  - self-attribution preserved,
  - explicit failure on missing ack/timeout.
- Record evidence pointers (logs, metrics, test runs) that directly support the invariant claims.

## Notes: Scope Check (against futon-theory/mission-scoping)

This mission explicitly declares owner, scope in/out, success criteria, time box, and exit conditions. If the rebuild work expands into Forum or MUSN redesign, that is a scope breach and must be split into a new mission rather than absorbed here.
