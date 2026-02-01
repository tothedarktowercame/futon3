---
description: Hop into a PAR session in Emacsclient, return when done
argument-hint: [title]
---

Detach from Claude Code into a focused PAR (Post-Action Review) session in Emacs.
Control returns to Claude Code when the Emacs frame closes.

## What This Does

The `/peripherals:par` command implements a **peripheral hop**:
1. Spawns an Emacsclient frame with a PAR buffer
2. Waits for the frame to close
3. Captures the PAR content
4. Logs to the Lab activity stream
5. Returns control to Claude Code session

This preserves session continuity - same `--resume` ID works before and after the hop.

## Usage

```
/peripherals:par                    # Start PAR with auto-generated title
/peripherals:par "Session debrief"  # Start PAR with specific title
```

## Process

1. **Generate PAR Title**

   Use $ARGUMENTS if provided, otherwise generate from session context:
   - Recent commits: `git log --oneline -3`
   - Files changed: `git diff --stat HEAD~3`
   - Current time

2. **Create PAR Buffer Content**

   Generate initial PAR template:
   ```markdown
   # PAR: <title>
   Date: <timestamp>
   Session: <session-id if available>

   ## What Happened
   <!-- What actually occurred? What approaches were taken? -->

   ## Perspectives
   <!-- What's your unique view? What did you notice? -->

   ## What We Learned
   <!-- Explicit takeaways. What was learned? -->

   ## Forward
   <!-- What should happen next? Actions or experiments? -->
   ```

3. **Spawn Emacsclient Frame**

   Create a temporary file with PAR content and open in Emacs:

   ```bash
   PAR_FILE=$(mktemp /tmp/par-XXXXXX.md)
   echo "<PAR content>" > "$PAR_FILE"

   # Open in new frame, wait for close
   emacsclient --create-frame "$PAR_FILE"
   ```

   The `--create-frame` without `-n` makes emacsclient wait until the frame closes.

4. **Capture Completed PAR**

   After frame closes, read the (possibly edited) PAR:
   ```bash
   PAR_CONTENT=$(cat "$PAR_FILE")
   rm "$PAR_FILE"
   ```

5. **Log to Activity Stream**

   ```bash
   curl -s -X POST http://localhost:6065/musn/activity/log \
     -H "Content-Type: application/json" \
     -d '{
       "event/type": "session/par",
       "agent": "claude",
       "source": "peripheral-hop",
       "par/title": "<title>",
       "par/content": "<captured content>",
       "session/id": "<session id>"
     }'
   ```

6. **Return to Claude Code**

   Output summary and continue session:
   ```
   ## PAR Complete

   Title: <title>
   Sections completed: <count non-empty sections>
   Logged to: Lab activity stream

   Session resumed. You can continue working or use /futon:psr for next task.
   ```

## The Hop Pattern

```
┌─────────────────┐
│  Claude Code    │
│  (interactive)  │
└────────┬────────┘
         │ /peripherals:par
         ▼
┌─────────────────┐
│  Emacsclient    │
│  (PAR buffer)   │  ← User edits PAR
└────────┬────────┘
         │ Close frame (C-x 5 0)
         ▼
┌─────────────────┐
│  Claude Code    │
│  (resumed)      │  ← Same session continues
└─────────────────┘
```

## Why Hop?

- **Focused environment**: Emacs for structured writing, Claude Code for coding
- **Human reflection**: PAR benefits from unhurried human thought
- **Session continuity**: Same conversation context before and after
- **Separation of concerns**: Right tool for each task

## Emacs Integration

The PAR buffer can use markdown-mode or a custom par-mode if defined.
User closes the frame when done (C-x 5 0 or clicking close).

For enhanced integration, you could define in Emacs:
```elisp
(defun par-session (title)
  "Open a PAR buffer with TITLE."
  (let ((buf (generate-new-buffer (format "*PAR: %s*" title))))
    (switch-to-buffer buf)
    (markdown-mode)
    (insert (format "# PAR: %s\n\n" title))
    buf))
```

## Example

```
> /peripherals:par "Testing refactor complete"

Hopping to Emacs for PAR...
Opening: /tmp/par-abc123.md

[Emacs frame opens with PAR template]
[User fills in sections]
[User closes frame with C-x 5 0]

## PAR Complete

Title: Testing refactor complete
Sections completed: 4/4
Logged to: Lab activity stream

Session resumed. Ready for next task.
```

Begin by generating the PAR title and content template.
