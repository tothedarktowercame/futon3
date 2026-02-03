---
description: Hop to a peripheral mode and return when done
argument-hint: <peripheral> [args]
---

General-purpose peripheral hop - detach to a focused mode and return.

## What This Does

The `/peripherals:hop` command provides a unified interface for peripheral hops:
1. Validates the target peripheral
2. Spawns the peripheral process
3. Waits for completion
4. Captures output/artifacts
5. Returns to Claude Code session

## Usage

```
/peripherals:hop par "Debrief"           # Hop to PAR in Emacs
/peripherals:hop explore src/core        # Hop to read-only exploration
/peripherals:hop chat #lab               # Hop to IRC chat mode
```

## Available Peripherals

| Peripheral | Description | Returns |
|------------|-------------|---------|
| `par` | Post-Action Review in Emacs | PAR content |
| `explore` | Read-only codebase exploration | Findings summary |
| `chat` | IRC/Forum discussion mode | Conversation log |
| `test` | Focused test running | Test results |

## Process

1. **Parse Arguments**

   Extract peripheral name and arguments:
   ```
   /peripherals:hop <peripheral> [args...]
   ```

2. **Validate Peripheral**

   Check if peripheral is known. If not, list available options.

3. **Dispatch to Peripheral**

   Each peripheral has its own mechanism:

   - **par**: `emacsclient --create-frame <par-file>`
   - **explore**: `./scripts/fuclaude-peripheral.ts --no-agency --explore <path>`
   - **chat**: Drawbridge IRC integration (in-JVM, no subprocess)
   - **test**: `./scripts/test-runner.sh <args>`

   For **chat**, use the Drawbridge's IRC functions via REPL:
   ```clojure
   ;; Connect to IRC
   (futon3.drawbridge.claude/connect-irc! {:room "lab" :nick "claude"})

   ;; Send messages (or let Claude respond to IRC messages automatically)
   (futon3.drawbridge.claude/send-to-irc! "Hello!")

   ;; When done, disconnect and get transcript
   (futon3.drawbridge.claude/disconnect-irc!)
   ```

4. **Wait for Completion**

   The hop blocks until the peripheral process exits:
   - Emacs frame closed
   - Peripheral script exits
   - User sends quit signal

5. **Capture Artifacts**

   Collect outputs from the peripheral:
   - PAR: Read the PAR file content
   - Explore: Read findings from output
   - Chat: Read conversation log
   - Test: Read test results

6. **Return to Session**

   Output summary and continue:
   ```
   ## Hop Complete

   Peripheral: <name>
   Duration: <time spent>
   Artifacts: <what was captured>

   Session resumed.
   ```

## The Hop Architecture

```
┌─────────────────────────────────────────────┐
│              Claude Code Session            │
│           (session-id: abc123)              │
└─────────────────┬───────────────────────────┘
                  │
    ┌─────────────┼─────────────┐
    │             │             │
    ▼             ▼             ▼
┌───────┐   ┌─────────┐   ┌─────────┐
│  PAR  │   │ Explore │   │  Chat   │
│(Emacs)│   │  (CLI)  │   │  (IRC)  │
└───┬───┘   └────┬────┘   └────┬────┘
    │            │             │
    └────────────┼─────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────┐
│              Claude Code Session            │
│           (session-id: abc123)              │  ← Same session!
└─────────────────────────────────────────────┘
```

## Session Continuity

The key property of a hop is **session continuity**:
- Same session ID before and after
- Claude Code context preserved
- Can `--resume` into the same conversation
- Memory of what happened before the hop

This differs from starting a new session - hops are round trips.

## Chat Hop via Drawbridge

The **chat** peripheral uses the Claude Drawbridge's built-in IRC integration:

1. **Connect**: `(futon3.drawbridge.claude/connect-irc! {:room "lab" :nick "claude"})`
2. **Messages flow automatically**: IRC → Claude → IRC (responses echo back)
3. **Disconnect**: `(futon3.drawbridge.claude/disconnect-irc!)` returns transcript

The Drawbridge maintains the same Claude session (via `--resume`), so memory persists
throughout the hop. When you return, you have full context of the IRC conversation.

### Chat Hop Example

```
> /peripherals:hop chat lab

Connecting to IRC #lab...

[In IRC]
<joe> Hey Claude, what do you think about the new Drawbridge?
<claude> The Drawbridge architecture is clean - HTTP API for input,
         Java-WebSocket for streaming, --resume for session persistence.
<joe> Can you explain the Agency integration?
<claude> Agency registers local handlers for in-JVM agents...
<joe> /hop-done

Hop complete. Transcript captured.

## Hop Summary
- Room: #lab
- Duration: 3m 42s
- Messages: 6
- Transcript available in context.
```

## Creating Custom Peripherals

To add a new peripheral:

1. Create a script that:
   - Accepts arguments
   - Runs to completion (doesn't daemonize)
   - Outputs artifacts to a known location

2. Add dispatch logic to this command

3. Document in the peripherals table above

## Example

```
> /peripherals:hop explore src/futon3/agency

Hopping to explore peripheral...
Target: src/futon3/agency
Mode: read-only

[Explore peripheral runs, user investigates]
[User exits with Ctrl-D or 'exit']

## Hop Complete

Peripheral: explore
Duration: 8m 23s
Findings:
- Agency WebSocket at /agency/ws
- Bell dispatch in service.clj:handle-ring-bell
- 3 registered agent types

Session resumed. Findings available for reference.
```

Begin by parsing the peripheral name and arguments.
