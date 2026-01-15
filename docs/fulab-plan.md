# FuLab Plan: Agent Patterns -> Concrete Changes

Goal: expand agent-pattern next steps into a coherent, staged change plan for the fulab suite.

1. Require scope declaration in HUD prompts.
   Pattern source: agent/scope-before-action (next step: "Add scope declaration to your agent's initialization or planning phase.")
   Change: add a Scope line to `src/futon3/fulab/hud.clj` prompt block with in-bounds, out-of-bounds, and exit condition guidance.

2. Log scope boundaries in session metadata.
   Pattern source: agent/scope-before-action (next step: "Record explicit in/out-of-scope fields in fulab session metadata and log scope-expansion events.")
   Change: extend session event payloads to include `:scope/in`, `:scope/out`, `:scope/exit`, and `:scope/expanded?` in the HUD or session-start records.

3. Add explicit intent-handshake event at session start.
   Pattern source: agent/intent-handshake-is-binding (next step: "Add an intent-handshake event to the session start protocol.")
   Change: emit a `:session/intent-handshake` event with intent, scope, constraints, and success criteria before any tool calls.

4. Block tool actions until handshake recorded.
   Pattern source: agent/intent-handshake-is-binding (next step: "Block tool actions until the handshake is logged.")
   Change: add a gate in the tool-dispatch path that rejects tool calls without a prior intent-handshake event.

5. Record intent/scope changes as first-class events.
   Pattern source: agent/intent-handshake-is-binding (next step: "Record intent changes as first-class events with rationale.")
   Change: introduce `:session/intent-change` events with `:change/rationale` and update reports to summarize changes.

6. Define evidence requirements for claim types.
   Pattern source: agent/evidence-over-assertion (next step: "Identify claim types...define what counts as adequate evidence.")
   Change: document claim types (facts/completions/diagnoses) and the acceptable anchors for each in `fulab-pattern-check` guidance.

7. Validate claims for missing anchors.
   Pattern source: agent/evidence-over-assertion (next step: "Add validation that flags claims missing their evidence anchors...")
   Change: extend `fulab-pattern-check` to flag unsupported claims and track provisional claims separately.

8. Auto-attach tool/file anchors into PUR/PSR.
   Pattern source: agent/evidence-over-assertion (next step: "Auto-attach tool/file anchors...")
   Change: propagate `lab/tool-uses` into PUR/PSR records and surface missing evidence in `fulab-session-report`.

9. Add trail query helpers for resume flows.
   Pattern source: agent/trail-enables-return (next step: "Add fulab trail query helpers (CLI or HUD)...")
   Change: add a helper to query `lab/trace` for recent decisions and surface a short "trail recap" in HUD on resume.

10. Extend handoff summaries with open questions and provisional claims.
    Pattern source: agent/handoff-preserves-context (next step: "Extend lab stubs/summaries to capture open questions...")
    Change: add fields to `fulab-summary`/`fulab-session-report` for open questions, scope boundaries, and provisional claim ledger.
