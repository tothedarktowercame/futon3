# Mission: Emacs Cursor Peripheral
Status: in progress

**Parent:** f0/P4 (Hypertext Navigation), f3/P2 (Agent Perception)
**Date:** 2026-05-24

## Owner

TBD. Assign a single owner before implementation.

## Intent

Create a read-only Emacs peripheral that gives an agent a visible cursor
"body" inside the existing Emacs session, using the canonical futon3c
peripheral registry and WebSocket transport.

This mission does **not** introduce a separate cursor server, a parallel
protocol, or a bypass around peripheral invariants. Emacs is the WS client.
futon3c remains the only authority for peripheral lifecycle.

## Current Status

This mission is no longer just a sketch. A basic canonical embodiment now
exists:

- `:emacs-cursor` is registered in the canonical peripheral registry.
- the canonical WS lifecycle supports `peripheral_event`.
- Emacs can connect as a WS client and start `:emacs-cursor`.
- live editor state is projected into the agent-facing prompt contract as a
  `surface-projection`.
- a canonical `minibuffer` write surface exists and can carry structured
  commands, including a basic `eval-sexp` command.

So the current system already supports a minimal "inhabited" cursor
peripheral: the agent can see bounded live Emacs context and can ask the
connected Emacs session to perform explicit read/control actions through the
same authenticated peripheral channel.

What does **not** yet exist:

- a visible companion cursor/region that actively tracks the user
- a companion caption, posframe, or "clippy but not awful" UI
- policy for pair-editing or silent mutation
- performance/debug instrumentation specific to buffer switches and editor lag

## Decision

The earlier sketch assumed an ad hoc Emacs-specific WebSocket service. That is
not acceptable now that futon3c has:

- a canonical peripheral registry in `futon3c/resources/peripherals.edn`
- a canonical runner seam in `futon3c.peripheral.runner/PeripheralRunner`
- a canonical WS lifecycle in `futon3c.transport.ws`

Therefore this mission is rewritten around those seams.

## Scope

### Scope In

- Add a new peripheral spec, tentatively `:emacs-cursor`, to the canonical
  peripheral registry.
- Implement a read-only peripheral runner that consumes editor context and
  emits cursor observations.
- Implement an Emacs minor mode that connects over the existing WS transport,
  using the current Emacs session as the embodiment surface.
- Render a visible remote cursor overlay in a live buffer.
- Support read-only modes only: `follow`, `observe`, `scout`.

### Scope Out

- Pair editing or buffer mutation.
- A second bespoke WS server.
- Shared multi-user permissions and ACL design.
- Rich UI polish, onboarding, or command palettes.
- `multiple-cursors.el` integration beyond possible future rendering reuse.

## Read-Only First

The first version must stay structurally read-only.

Allowed:

- agent follows the user's point and visible window
- agent observes a region or file context
- agent scouts elsewhere in the codebase without editing
- agent surfaces commentary via ordinary chat/message channels

Forbidden in v0:

- `insert`
- `delete`
- buffer writes of any kind
- implicit escalation from observe/follow into pair

If later pair-editing is desired, that is a separate mission with separate
review.

## Why this is not just `emacsclient`

The new seam is intentionally different from shelling out to `emacsclient`.

`emacsclient` is an out-of-band RPC path into Emacs. It can be useful for
one-off automation, but it does not by itself make the agent *inhabit* an
editor surface. In particular, it does not provide:

- a canonical peripheral lifecycle attached to an authenticated agent session
- a prompt-visible runtime surface contract saying "you are currently in
  Emacs-space"
- a live read surface projected into the agent registry
- a typed write surface whose responses flow back through the same WS session
- a place to hang embodiment state such as mode, focus, observed buffer, or
  future companion-cursor overlays

The smart-cursor path is therefore not just "another way to evaluate Elisp".
It is an embodied transport contract:

- the agent is attached to a specific live editor session
- Emacs context is surfaced as agent-readable runtime state
- control messages come back over canonical `peripheral_event`
- the same seam can later host overlays, captions, timers, and instrumentation

That is the architectural reason to keep this mission distinct from
`emacsclient`, even though both can ultimately cause Emacs Lisp to run.

## Architecture

### 1. Canonical peripheral registration

Add a new registry entry in `futon3c/resources/peripherals.edn`, tentatively:

```clojure
:emacs-cursor
{:peripheral/id :emacs-cursor
 :peripheral/tools #{:cursor-context :cursor-mode :cursor-focus}
 :peripheral/scope :live-emacs-session
 :peripheral/entry #{:user-request :from-any}
 :peripheral/exit #{:user-request :session-end :blocked}
 :peripheral/context {:session-id :inherit
                      :agent-id :inherit
                      :editor-id :required}}
```

The names may change, but the important point is that the cursor peripheral is
registered exactly like other peripherals instead of hiding behind an
Emacs-only sidecar.

### 2. Canonical WS session lifecycle

The Emacs client must use the existing WS flow:

1. connect to the futon3c WS endpoint
2. send `{"type":"ready","agent_id":"..."}` and wait for `ready_ack`
3. send `{"type":"peripheral_start","peripheral_id":"emacs-cursor"}`
4. stream editor state through `tool_action`
5. stop via `{"type":"peripheral_stop","reason":"..."}` or on disconnect

No custom connect handshake should be introduced.

### 3. Context enters through `tool_action`

The current WS protocol already has a typed action channel:

```json
{"type":"tool_action","tool":"cursor-context","args":[{...snapshot...}]}
{"type":"tool_action","tool":"cursor-mode","args":["follow"]}
{"type":"tool_action","tool":"cursor-focus","args":[{...focus target...}]}
```

That is the correct ingress path for editor context. The peripheral runner can
treat these as state updates rather than shell-style tools.

### 4. Cursor state exits through a new canonical WS event

There is one real transport gap: the existing WS protocol handles
`ready/peripheral_start/tool_action/peripheral_stop`, but it does not yet have
a canonical typed frame for unsolicited peripheral-to-client UI events.

This mission should therefore add a **new canonical frame**, for example:

```json
{"type":"peripheral_event",
 "peripheral_id":"emacs-cursor",
 "event":"cursor_state",
 "payload":{"buffer":"foo.clj","point":142,"mark":151,"mode":"observe"}}
```

and similarly:

```json
{"type":"peripheral_event",
 "peripheral_id":"emacs-cursor",
 "event":"speak",
 "payload":{"message":"Watch the nil path here."}}
```

This is a structural addition to the existing WS tooling, not a special-case
side channel.

Implementation consequence:

- extend `futon3c.transport.protocol`
- extend `futon3c.transport.ws`
- extend WS shapes/tests for the new event frame

### 5. Basic read/control write surface

The mission originally framed v0 as purely read-only. In practice, a minimal
control surface is already useful and has now been added in basic form:

```json
{"type":"peripheral_event",
 "peripheral_id":"emacs-cursor",
 "event":"minibuffer",
 "payload":{"command":"eval-sexp",
            "request-id":"req-1",
            "sexp":"(with-current-buffer \"*scratch*\" (buffer-name))"}}
```

This is still materially different from pair-editing. The current command
channel is explicit, typed, and attached to the live peripheral session. It is
best understood as a controlled embodiment seam for read/control requests, not
as blanket authorization to edit buffers arbitrarily.

## Emacs embodiment

The Emacs side is a minor mode running in the existing editor session. It
should:

- open the futon3c WS connection
- send readiness + peripheral start
- capture local editor context on movement/window changes
- render the remote cursor and region as overlays
- clean up overlays and stop the peripheral on disconnect

The first embodiment is intentionally narrow:

- one visible remote cursor
- one optional observed region
- one active Emacs session

No attempt should be made to solve multi-user cursor multiplexing yet.

The current implementation reaches this embodiment only partway:

- the live context bridge exists
- the write surface exists
- the visible companion cursor is still pending

## Rendering prior art

The strongest local prior art is `~/.emacs.d/lisp/crdt.el/crdt.el`, which
already renders remote pseudo cursors and regions using overlays. In
particular, it demonstrates:

- a stable pseudo-cursor overlay category
- separate cursor and region overlays per remote participant
- cursor movement via `move-overlay`
- cleanup on remote disappearance

That is the right rendering model for v0.

Relevant local supporting material:

- `futon3/contrib/websocket-fix.el`
- `futon3/contrib/fubar-agency.el`
- `futon3/contrib/fuclient-claude-stream.el`

These are transport/client prior art, not new protocol authorities.

`multiple-cursors.el` is plausible future UX prior art, but it is not part of
the initial architecture.

## Minimal context model

The peripheral does not need the entire buffer on every movement. Start with a
bounded snapshot:

- buffer name
- file path
- point
- mark, if active
- visible window start/end
- visible text slice
- major mode
- project root, if known
- monotonic editor tick or sequence number

The runner should prefer explicit snapshots over trying to infer editor state
from chat transcripts or shell state.

The current implementation already carries enough information to support a
basic "buffer" surface and a "minibuffer" surface:

- current buffer identity
- visible text slice
- user cursor position
- latest minibuffer exchange
- debug/session metadata

That is sufficient for bounded inspection, explicit cursor-following, and
future instrumentation work.

## Modes

### `follow`

- remote cursor trails the user's attention
- primary use: passive observation during explanation

### `observe`

- remote cursor/region stays pinned to a user-selected target
- primary use: keep watch on one function while the user works elsewhere

### `scout`

- agent may move the remote cursor independently, but remains read-only
- primary use: inspect nearby definitions or call sites

### Not in v0: `pair`

`pair` is intentionally excluded. It would require a distinct write path,
gating rules, and security review.

## Next Embodiment Layer

The most compelling next step is not "more arbitrary Elisp". It is a visible
companion cursor mode:

- a remote cursor that follows the user's point in real time
- an optional observed region or scout cursor
- a lightweight caption or posframe for contextual suggestions
- explicit mode changes between `follow`, `observe`, and `scout`

The target interaction should feel less like an RPC shell and more like a
helpful embodied partner: "clippy but not terrible" is a reasonable informal
goal, provided the UI remains quiet, intentional, and easy to dismiss.

The natural local rendering direction is still overlays first, with posframe
or caption UI as a later layer.

## Motivating Use Case: Editor Slowdowns

The immediate practical motivation is not aesthetic. It is debugging editor
slowdowns while moving between buffers.

An inhabited smart cursor should eventually be able to:

- tag along as the user switches buffers
- timestamp focus/window/buffer transitions
- capture bounded context before and after a jump
- correlate lag with buffer type, visible text size, hooks, overlays, or mode
- surface hypotheses in-context while the slowdown is happening

That is exactly the kind of task where embodiment matters. A shell command or
detached `emacsclient` call can inspect Emacs, but it does not naturally live
inside the same attention loop as the user. The cursor peripheral can.

## Exit Conditions

- A registered `:emacs-cursor` peripheral can be started through the canonical
  WS flow.
- An Emacs minor mode connects to futon3c and completes the readiness
  handshake.
- Editor context reaches the runner through `tool_action`.
- futon3c emits canonical typed cursor events back to Emacs.
- A live `surface-projection` reaches the agent-facing prompt/runtime view.
- A canonical `minibuffer` write surface supports structured commands.
- Emacs renders a visible remote cursor overlay in a live buffer.

The first four conditions are now satisfied in basic form. The visible
companion cursor overlay remains the main missing embodiment feature.

## Risks

1. The transport gap for server-pushed peripheral events is real and must be
   solved canonically, not patched around.
2. Emacs 31 `websocket.el` behavior has known rough edges, so reuse the local
   websocket fixes rather than assuming a clean client environment.
3. Cursor embodiment is easy to over-scope into collaborative editing. Resist
   that expansion in v0.
4. A generic `eval-sexp` seam is powerful. Keep it explicit, typed, and
   attached to the authenticated peripheral session; do not let it collapse
   back into an unstructured side channel.

## Implementation Order

1. Record the implemented canonical seam in this mission.
2. Add the visible companion cursor overlay so the embodiment is present on
   screen, not just in transport state.
3. Add lightweight caption/posframe support for contextual suggestions.
4. Add explicit instrumentation hooks for buffer-switch slowdown debugging.
5. Verify connect, follow, observe, scout, and cleanup on disconnect under
   real editor movement.

## Related

- `futon3c/resources/peripherals.edn`
- `futon3c/src/futon3c/peripheral/runner.clj`
- `futon3c/src/futon3c/peripheral/registry.clj`
- `futon3c/src/futon3c/transport/ws.clj`
- `futon3c/src/futon3c/transport/protocol.clj`
- `futon3c/test/futon3c/transport/ws_peripheral_test.clj`
- `~/.emacs.d/lisp/crdt.el/crdt.el`

## Notes

The core idea still stands: the cursor is the agent's embodiment in
Emacs-space. The important correction is architectural. That embodiment must
be expressed as a normal futon3c peripheral with canonical WS transport, not
as a bespoke cursor tunnel.
