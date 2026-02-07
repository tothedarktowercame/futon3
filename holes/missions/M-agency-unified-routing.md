# Mission: Agency Unified Routing

**Status:** :active
**Primary:** Claude
**Reviewer:** Codex
**Date:** 2026-02-07
**Parent:** f3/Agency, futon-theory/agent-contract

## Intent

Unify agent routing in Agency so multiple Claude and Codex instances can be
registered in a single JVM, eliminating the need for separate Drawbridge
processes per agent. Agency becomes the single source of truth for agent
registration and message routing.

## Problem

Current architecture has memory bifurcation issues:

1. **Heavyweight**: Each agent needs a separate Drawbridge JVM
2. **Bifurcated**: Messages to WebSocket don't reach terminal sessions
3. **Inconsistent**: Claude uses persistent subprocess, Codex uses one-shot exec
4. **Scattered**: Agent state spread across multiple processes

## Solution

Extend Agency's existing `local-handlers` to support full invoke capability:

```
┌─────────────────────────────────────────────────────────────┐
│                    Agency (one JVM)                         │
├─────────────────────────────────────────────────────────────┤
│  agent-registry: {                                          │
│    "claude-1" -> {:invoke-fn <persistent-subprocess>        │
│                   :session-id "abc123"                      │
│                   :type :claude}                            │
│    "codex-1"  -> {:invoke-fn <one-shot-exec>                │
│                   :session-id "thread_xyz"                  │
│                   :type :codex}                             │
│  }                                                          │
├─────────────────────────────────────────────────────────────┤
│  On page(agent-id, prompt):                                 │
│    1. Look up agent in agent-registry                       │
│    2. Invoke directly (same JVM, no routing)                │
│    3. Return response synchronously                         │
└─────────────────────────────────────────────────────────────┘
```

## Related Work

### Codex Contributions (to be integrated or superseded)

- **f24b55a**: `Codex drawbridge: send prompts via stdin and improve JSONL parsing`
  - Improved Codex invocation (stdin instead of argv)
  - Better JSONL event parsing
  - Session tracking from thread.started events
  - **Status**: Good work, will be extracted into shared invoke-codex fn

- **fac9efb**: `Add Agency-Codex integration brief for memory bifurcation fix`
  - Documents the problem clearly
  - Lists options (streaming mode, hooks, polling)
  - **Status**: This mission is the response to that brief

### Existing Infrastructure

- `local-handlers` atom in `agency/http.clj` (line 249)
- `register-local-handler!` / `unregister-local-handler!` functions
- `send-to-agent!` already checks local-handlers first

## Success Criteria

1. [ ] Multiple agents registerable in single Agency JVM
2. [ ] Pages to local agents invoke directly (no WebSocket round-trip)
3. [ ] Claude agents use persistent subprocess pattern
4. [ ] Codex agents use one-shot exec pattern (from f24b55a)
5. [ ] Session-ids tracked per agent
6. [ ] No separate Drawbridge JVMs needed
7. [ ] Existing WebSocket agents still work (backwards compatible)

## Scope

### In Scope

- Refactor `local-handlers` → `agent-registry` with richer structure
- Extract Claude invoke logic from `drawbridge/claude.clj`
- Extract Codex invoke logic from `drawbridge/codex.clj`
- Create registration API for agents
- Update Agency page handling to use invoke-fns

### Out of Scope

- Changes to remote WebSocket agent protocol
- Changes to MUSN chat
- Forum integration
- Peripheral system changes

## Architecture

### Phase 1: Agent Registry

Replace simple handler-fn with structured agent record:

```clojure
(defonce agent-registry (atom {}))

;; Agent record structure
{:agent-id "claude-1"
 :type :claude  ; or :codex
 :invoke-fn (fn [prompt session-id] -> {:result ... :session-id ... :exit-code ...})
 :session-id "abc123"
 :registered-at <instant>
 :last-active <instant>
 :subprocess <Process>  ; for Claude persistent subprocess
 }
```

### Phase 2: Invoke Functions

Extract from existing drawbridge code:

```clojure
;; From drawbridge/claude.clj
(defn make-claude-invoke-fn [initial-session-id]
  ;; Returns invoke-fn that manages persistent subprocess
  ...)

;; From drawbridge/codex.clj (uses f24b55a improvements)
(defn make-codex-invoke-fn [initial-session-id]
  ;; Returns invoke-fn that does one-shot exec
  ...)
```

### Phase 3: Registration API

```clojure
(defn register-agent!
  "Register an agent with Agency for local invocation."
  [{:keys [agent-id type session-id]}]
  (let [invoke-fn (case type
                    :claude (make-claude-invoke-fn session-id)
                    :codex (make-codex-invoke-fn session-id))]
    (swap! agent-registry assoc agent-id
           {:agent-id agent-id
            :type type
            :invoke-fn invoke-fn
            :session-id session-id
            :registered-at (Instant/now)})))

(defn unregister-agent! [agent-id]
  ;; Clean up subprocess if Claude
  ;; Remove from registry
  ...)
```

### Phase 4: Page Handling

Update `handle-page` to use registry:

```clojure
(defn handle-page [{:keys [agent-id prompt timeout-ms]}]
  (if-let [agent (get @agent-registry agent-id)]
    ;; Local agent - invoke directly
    (let [{:keys [result session-id]} ((:invoke-fn agent) prompt (:session-id agent))]
      (swap! agent-registry assoc-in [agent-id :session-id] session-id)
      (swap! agent-registry assoc-in [agent-id :last-active] (Instant/now))
      {:ok true :response result})
    ;; Remote agent - existing WebSocket path
    (handle-remote-page agent-id prompt timeout-ms)))
```

## Futon-Theory Alignment

| Pattern | Application |
|---------|-------------|
| single-source-of-truth | Agency registry is THE source for agent state |
| agent-contract | Agents register, respond to pages, maintain session |
| proof-path | Every page → invoke → response is auditable |
| durability-first | Session-ids persist, subprocess state managed |
| coordination-protocol | Ping-pong review between Claude (primary) and Codex (reviewer) |

## Implementation Plan

### Part 1: Foundation (Claude primary)

1. [ ] Create `agency/registry.clj` with agent-registry atom
2. [ ] Define agent record structure
3. [ ] Implement `register-agent!` / `unregister-agent!`
4. [ ] Add registry introspection endpoints

### Part 2: Invoke Extraction (Claude primary)

5. [ ] Extract `make-claude-invoke-fn` from drawbridge/claude.clj
6. [ ] Extract `make-codex-invoke-fn` from drawbridge/codex.clj
7. [ ] Preserve f24b55a improvements (stdin, JSONL parsing)
8. [ ] Test invoke functions independently

### Part 3: Integration (Claude primary, Codex review)

9. [ ] Update `handle-page` to check registry first
10. [ ] Update `send-to-agent!` to use registry
11. [ ] Maintain backwards compatibility with WebSocket agents
12. [ ] Add startup registration for configured agents

### Part 4: Validation (Both)

13. [ ] Test: Register Claude agent, send page, get response
14. [ ] Test: Register Codex agent, send page, get response
15. [ ] Test: Multiple agents simultaneously
16. [ ] Test: WebSocket agents still work

## Notes

- Clojure's atoms provide safe concurrent access to registry
- Session-id updates are atomic with response handling
- Subprocess lifecycle (for Claude) needs careful cleanup on unregister
- This is the in-process analogue to futon1a's storage invariants

## References

- `src/futon3/agency/http.clj` - existing local-handlers
- `src/futon3/agency/service.clj` - Agency service
- `src/futon3/drawbridge/claude.clj` - Claude invoke pattern
- `src/futon3/drawbridge/codex.clj` - Codex invoke pattern (f24b55a)
- `AGENCY-CODEX-BRIEF.md` - Problem statement (fac9efb)
- `library/futon-theory/coordination-protocol.flexiarg` - Handoff pattern
