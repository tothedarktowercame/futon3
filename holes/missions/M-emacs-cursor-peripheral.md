# Mission: Emacs Cursor Peripheral

**Status:** :greenfield
**Parent:** f0/P4 (Hypertext Navigation), f3/P2 (Agent Perception)
**Date:** 2026-01-29

## Owner

TBD (likely Claude for Emacs UX + Codex for service plumbing; assign a single owner before starting implementation).

## Scope

### Scope In

- Define the minimal protocol between Emacs and the cursor peripheral (context stream + commands).
- Implement a prototype Emacs minor mode that can connect, stream context, and render a cursor overlay.
- Enforce mode constraints (observe/follow are read-only; pair mode gated and off by default).

### Scope Out

- Full production-grade UX (polish, keymaps, onboarding).
- Multi-user/shared sessions and permissions.
- Editing/pair mode by default (treat as a separate, security-reviewed follow-up).

## Time Box

1-2 days for a prototype (connect + overlay + basic commands), then re-scope.

## Exit Conditions

- Prototype demonstrates connect + context streaming + visible cursor overlay in Emacs.
- If security/UX complexity dominates, stop and split into smaller missions (protocol vs overlay vs edit gating).

## Intent

Create an Emacs peripheral where Claude has a persistent cursor "avatar" - a visible presence in the editor that can follow the user, observe regions, scout ahead, or pair-edit.

This is different from invoking Claude via emacsclient. The peripheral would give Claude embodied presence in Emacs-space.

## Vision

```
┌─────────────────────────────────────────────────────────────────────┐
│  *scratch*                                                    [EL] │
├─────────────────────────────────────────────────────────────────────┤
│  (defun my-function ()                                              │
│    "Docstring here."█                    ← your cursor              │
│    (let ((x 1)                                                      │
│          (y 2))                                                     │
│      (+ x y◆)))                          ← Claude cursor (◆)       │
├─────────────────────────────────────────────────────────────────────┤
│  [Claude: observe mode] [watching: my-function] [◆ following █]    │
└─────────────────────────────────────────────────────────────────────┘
```

## Cursor Modes

| Mode | Claude Can | Use Case |
|------|-----------|----------|
| follow | See what you see, cursor trails yours | Passive observation |
| observe | Watch a specific region | Monitor while you work elsewhere |
| scout | Move independently (read-only) | Explore ahead in codebase |
| pair | Edit at cursor position | Active collaboration |

## What Claude Perceives

- Current buffer, point, mark, region
- Major/minor modes, file path, project root
- Visible window contents
- Recent edits (undo history as intent signal)
- Your cursor movements (attention signal)

## Architecture (Sketch)

### Emacs Side

```elisp
(defvar claude-cursor-overlay nil "Overlay showing Claude's position")
(defvar claude-peripheral-ws nil "WebSocket connection to peripheral")

(define-minor-mode claude-peripheral-mode
  "Enable Claude cursor peripheral."
  :lighter " ◆Claude"
  :global t
  (if claude-peripheral-mode
      (claude-peripheral--connect)
    (claude-peripheral--disconnect)))

(defun claude-peripheral--send-context ()
  "Stream editor context to Claude peripheral."
  (when (and claude-peripheral-ws (websocket-openp claude-peripheral-ws))
    (websocket-send-text
     claude-peripheral-ws
     (json-encode
      `(:type "context"
        :buffer ,(buffer-name)
        :file ,(buffer-file-name)
        :point ,(point)
        :line ,(line-number-at-pos)
        :mode ,(symbol-name major-mode)
        :visible ,(buffer-substring-no-properties
                   (window-start) (window-end)))))))

(defun claude-peripheral--on-message (ws frame)
  "Handle commands from Claude peripheral."
  (let* ((payload (json-parse-string (websocket-frame-text frame) :object-type 'plist))
         (cmd (plist-get payload :command)))
    (pcase cmd
      ("move" (claude-peripheral--move-cursor (plist-get payload :to)))
      ("highlight" (claude-peripheral--highlight (plist-get payload :region)))
      ("insert" (when claude-peripheral-pair-mode
                  (claude-peripheral--insert (plist-get payload :text))))
      ("speak" (message "[Claude] %s" (plist-get payload :message))))))
```

### Peripheral Side (TypeScript/Python)

- WebSocket server receives context stream
- Maintains internal model of visible editor state
- Sends cursor movement, highlight, insert commands
- Mode-constrained: observe mode blocks insert commands

### Communication Protocol

```json
// Emacs → Claude (context stream)
{"type": "context", "buffer": "*scratch*", "point": 142, "visible": "..."}

// Claude → Emacs (commands)
{"command": "move", "to": {"line": 15, "col": 4}}
{"command": "highlight", "region": {"start": 100, "end": 150}, "face": "claude-attention"}
{"command": "speak", "message": "I notice this function lacks error handling"}
{"command": "insert", "text": "(error-handling code)", "at": 142}
```

## Why Not Yet

1. Simpler peripherals (chat, explore, reflect) not yet battle-tested
2. Requires significant Emacs package development
3. WebSocket state management adds complexity
4. Mode transitions (follow→pair) need careful UX design
5. Security implications of "pair" mode need thought

## Prerequisites

- [ ] Chat peripheral working reliably
- [ ] Peripheral hop mechanics proven
- [ ] MUSN WebSocket transport stable
- [ ] Clear use cases beyond "cool demo"

## Related

- `scripts/fuclaude-chat-bridge.ts` - simpler peripheral model
- `docs/peripheral-spec.md` - capability envelope architecture
- `fulab/meetings/2026-01-27-irc-coordination-review.md` - peripheral concept origin
- Google Docs cursors, VSCode Live Share, tmux sharing - prior art

## Open Questions

- Should Claude cursor be visible to other users in shared sessions?
- How to handle Claude "looking away" (context window limits)?
- What's the right idle timeout before Claude stops tracking?
- Can multiple Claude instances have multiple cursors?

## Notes

Brainstormed 2026-01-29. The cursor becomes Claude's "body" in Emacs-space - an embodiment rather than a tool. Worth pursuing once the peripheral foundation is solid.
