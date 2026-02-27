# Mission: Multi-Agent Drawbridge

**Status:** :in-progress
**Date:** 2026-02-08
**Owner:** Joe
**Primary:** TBD
**Reviewer:** TBD
**Parent:** `holes/missions/M-agency-rebuild.md`

## Motivation

The current drawbridge architecture is 1:1: one JVM, one agent-id, one subprocess it manages. This means every Claude or Codex session needs its own JVM process, each consuming ~200-400MB of heap for routing logic that amounts to shuffling strings between sockets. On a Linode with limited memory, this doesn't scale to the 3-4 concurrent Claude sessions that are a normal working pattern.

The drawbridge's job is lightweight: route messages between Agency (bells, whistles) and agent sessions, plus IRC bridging. It should not own or spawn the agent processes — they already exist independently (CLI sessions, Emacs interfaces, etc.) and should register with the drawbridge to get routing.

Discovered during Agency rebuild Phase 3d: attempting to run a Claude drawbridge sidecar alongside the main dev JVM caused OOMs. The fundamental issue is architectural, not tuning.

## Design

### Current (1:1, drawbridge owns subprocess)

```
[Agency] <--WS--> [Drawbridge JVM 1: spawns + owns claude subprocess]
[Agency] <--WS--> [Drawbridge JVM 2: spawns + owns codex subprocess]
```

Each drawbridge spawns and manages one subprocess. The subprocess has no independent existence — the drawbridge starts it, feeds it, and kills it. This conflates routing (the drawbridge's job) with process management (not its job).

### Target (N:1, agents register with router)

```
[Agency + Drawbridge Router in same JVM]
  |
  |-- registered: "claude-agency"  <-- Claude CLI session (already running, registers itself)
  |-- registered: "claude-forum"   <-- another Claude CLI session (registers itself)
  |-- registered: "codex"          <-- Codex CLI session on laptop (registers via WS)
  |
  |-- IRC bridge: routes messages to/from registered agents by agent-id
  |-- HTTP API: POST /drawbridge/:agent-id for external callers
```

The agent sessions already exist. They register with the drawbridge to receive routing (bells, whistles, IRC messages). The drawbridge doesn't spawn them, doesn't own them, doesn't manage their lifecycle. When a session ends, it deregisters. When a new session starts, it registers.

This is the same model as Agency itself: agents connect and register; Agency routes, it doesn't spawn.

For remote agents (Codex on laptop), WS registration connects the remote drawbridge to the Agency. The remote drawbridge follows the same pattern — agents register with it locally.

### Key changes from current architecture

1. **`agent-state`**: single atom → map of agent-id → agent-state (invoke-fn, session-id, IRC nick, etc.)
2. **No subprocess management**: drawbridge doesn't start/stop claude or codex processes. It accepts registrations.
3. **Registration API**: `(register-agent! "claude-agency" {:invoke-fn f :session-id s})` / `(deregister-agent! "claude-agency")`
4. **Message routing**: when bell/whistle arrives for agent-id X, look up X in registry, call its invoke-fn
5. **IRC**: per-agent nick, per-agent room membership. Messages from IRC routed to correct agent by nick/mention.
6. **HTTP API**: `POST /drawbridge/:agent-id` routes to the registered agent's invoke-fn
7. **dev.sh**: no more sidecar JVMs for drawbridges. The main dev JVM hosts the router. Agents register when they start.

### How existing sessions register

A Claude Code session (like this one) registers via an Agency HTTP call or local-handler registration:

```clj
;; From within the session (e.g., via repl-eval or MCP tool):
(drawbridge/register-agent! "claude-agency"
  {:invoke-fn (fn [text] ...)   ; how to send input to this session
   :resume-id "abc123"          ; LLM session continuity
   :irc-nick "claude-agency"})  ; IRC identity for standup
```

The invoke-fn is the key: it's how the drawbridge sends messages INTO the session. For a Claude CLI subprocess using stream-json, it writes to stdin. For a session accessible via HTTP, it POSTs. The drawbridge doesn't care about the mechanism — it just calls the function.

### Identifier alignment (from agency/identifier-separation)

- `agent-id`: logical identity for routing ("claude-agency", "claude-forum")
- `resume-id`: LLM session continuity (passed to `claude --resume`)
- `session-id`: transport-layer WS session (for remote agents)

Multiple agent-ids can be backed by the same LLM model (Claude) with different resume-ids. The drawbridge routes by agent-id; it doesn't know or care about the underlying model.

## Scope In

- Refactor drawbridge/core.clj: multi-agent state, registration API
- Remove subprocess spawning from drawbridge (claude.clj, codex.clj become registration helpers, not process managers)
- Local-handler registration for co-located agents (same JVM as Agency)
- Per-agent IRC routing
- Remove drawbridge sidecar JVM blocks from dev.sh

## Scope Out

- Agency-to-Agency federation (separate mission, see futon3-tighten-survey.md)
- New peripheral types beyond Claude/Codex CLI
- Changes to the Agency invariants (A0-A5) themselves

## Success Criteria

1. One JVM hosts Agency + drawbridge router with 2+ agents registered simultaneously.
2. Each agent participates in standup from its own session context.
3. Memory overhead for the routing layer is bounded (no per-agent JVM).
4. Agents register/deregister without restarting the JVM or each other.
5. `agent-id` and `resume-id` remain separate per identifier-separation pattern.

## Dependencies

- Agency rebuild Phase 2 (A0-A5 enforcement) — done
- Agency rebuild Phase 3a (drawbridge A1 fix) — done
