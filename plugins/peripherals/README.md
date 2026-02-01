# Peripherals Plugin

Peripheral hop commands - detach into focused modes and return.

## Overview

The `peripherals` plugin provides **hop commands** that let you temporarily leave
Claude Code for a focused task in another environment, then return with session
continuity preserved.

## The Hop Pattern

```
┌─────────────────┐
│  Claude Code    │  ← You're here
│  (session X)    │
└────────┬────────┘
         │ /peripherals:par
         ▼
┌─────────────────┐
│    Emacsclient  │  ← Focused PAR writing
│   (PAR buffer)  │
└────────┬────────┘
         │ Close frame
         ▼
┌─────────────────┐
│  Claude Code    │  ← Back here, same session X
│  (session X)    │
└─────────────────┘
```

The key property: **session continuity**. Same `--resume` ID works before and after.

## Commands

### /peripherals:par - PAR Hop

Hop into Emacs for a Post-Action Review:

```
/peripherals:par                     # Auto-titled PAR
/peripherals:par "Testing complete"  # Named PAR
```

What happens:
1. Creates PAR template in temp file
2. Opens Emacsclient frame with that file
3. Waits for frame close (C-x 5 0)
4. Captures edited PAR content
5. Logs to MUSN activity stream
6. Returns to Claude Code

### /peripherals:hop - General Hop

Hop to any registered peripheral:

```
/peripherals:hop par "Debrief"       # Same as /peripherals:par
/peripherals:hop explore src/core    # Read-only exploration
/peripherals:hop chat #lab           # IRC chat mode
```

## Available Peripherals

| Peripheral | Environment | Use Case |
|------------|-------------|----------|
| `par` | Emacsclient | Structured reflection |
| `explore` | CLI peripheral | Read-only investigation |
| `chat` | IRC bridge | Multi-agent coordination |
| `test` | Test runner | Focused test execution |

## Why Hop?

**Separation of concerns**: Right tool for each task
- Emacs for structured writing (PAR)
- CLI peripheral for autonomous exploration
- IRC for real-time discussion

**Human focus**: Some tasks benefit from leaving the AI loop
- PAR writing needs unhurried reflection
- Chat coordination may need back-and-forth

**Session preservation**: Memory travels with the hop
- Context from before the hop is available after
- Can reference pre-hop work in post-hop session

## Comparison: futon vs peripherals

| Aspect | /futon:* | /peripherals:* |
|--------|----------|----------------|
| Mode | Interactive | Hop/detach |
| UI | AskUserQuestion | External app |
| Blocking | No | Yes (waits for return) |
| Use case | Pattern selection | Focused tasks |

## Emacs Integration

For PAR hops, Emacs should have:

1. Emacsclient server running: `M-x server-start`
2. Frame close with C-x 5 0 returns to Claude Code

Optional enhanced integration:
```elisp
(defun par-mode-setup ()
  "Setup for PAR editing."
  (markdown-mode)
  (local-set-key (kbd "C-c C-c") 'save-buffer))

(add-to-list 'auto-mode-alist '("/par-.*\\.md\\'" . par-mode-setup))
```

## Installation

1. Add to `~/.claude/settings.json`:
   ```json
   {
     "enabledPlugins": {
       "peripherals@futon3": true
     }
   }
   ```

2. Ensure Emacs server is running for PAR hops

3. Restart Claude Code

## Session ID Flow

The hop preserves session identity:

```
Claude Code (session: abc123)
    │
    │ /peripherals:par
    │
    ▼
Emacs (knows session: abc123 from PAR metadata)
    │
    │ Close frame
    │
    ▼
Claude Code (session: abc123, can --resume)
```

This means you can:
- `--resume abc123` from terminal after a hop
- Reference pre-hop context in post-hop work
- Chain multiple hops in one session

## See Also

- `/futon:*` - Interactive pattern commands
- `/futon-peripherals:*` - Original combined plugin
- `scripts/fuclaude-peripheral.ts` - Autonomous peripheral wrapper
- `README-peripherals.md` - Peripheral architecture docs
