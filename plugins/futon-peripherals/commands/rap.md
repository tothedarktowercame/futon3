---
description: Retrieve All PARs - load distilled learning from past sessions
argument-hint: "[?tags=tag1,tag2] [?limit=N] [?since=YYYY-MM-DD]"
---

Retrieve prior learning from PAR (Post-Action Review) sidecars to seed this session with distilled insights.

## What This Does

The `/rap` command fetches accumulated learning from past sessions:
1. Scans all `.par.edn` sidecar files in `~/.claude/projects/`
2. Filters by tags, date, or limit if specified
3. Returns formatted context ready for injection

RAP is the "open paren" to PAR's "close paren":
```
(rap                          ; open - retrieve prior learning
  ... session work ...
  par)                        ; close - capture new learning
```

## Usage

```
/rap                          # Get recent PARs (default limit 10)
/rap ?tags=websocket,crdt     # Filter by tags
/rap ?since=2026-01-30        # Filter by date
/rap ?limit=5                 # Limit results
/rap ?tags=architecture ?limit=3  # Combine filters
```

## Process

1. **Fetch from /rap endpoint**

   Use curl to fetch from the MUSN HTTP service:

   ```bash
   curl -s "http://localhost:6065/rap" | jq .
   ```

   With filters:
   ```bash
   curl -s "http://localhost:6065/rap?tags=websocket&limit=5" | jq .
   ```

2. **Present the Learning**

   The response includes:
   - `pars` - array of PAR summaries
   - `context` - pre-formatted markdown block

   Display the `context` field to show the user what prior learning is available.

3. **Offer to Inject**

   Ask if the user wants to use this context to inform the current session's work.

## Response Format

```json
{
  "ok": true,
  "count": 2,
  "pars": [
    {
      "id": "par-20260131-210600",
      "session-id": "64570417-...",
      "title": "Lab Session Streaming & PAR Sidecar Architecture",
      "timestamp": "2026-01-31T21:06:00Z",
      "tags": ["lab", "websocket", "par"],
      "learned": "http-kit rejects unmasked WebSocket frames..."
    }
  ],
  "context": "# Prior Learning (from 2 PARs)\n\n## Lab Session..."
}
```

## Why Use RAP

- **Bootstrap sessions** with relevant prior insights
- **Avoid re-learning** what was already discovered
- **Transfer principles** across projects (not just file state)
- **Build on past work** rather than starting cold

## Relationship to RALPH

RALPH (Ralph Wiggum technique) provides implicit memory via file artifacts. RAP provides **explicit memory** via distilled reflections:

| RALPH | RAP |
|-------|-----|
| Reads files from past iterations | Reads PARs from past sessions |
| Project-specific state | Transferable principles |
| "What files changed" | "What did we learn" |

They complement each other - RALPH for tactical iteration, RAP for strategic context.

## Example Output

```markdown
# Prior Learning (from 2 PARs)

## Stack HUD and PAR workflow
*Session: 64570417-... | 2026-01-31T20:15:41Z*
*Tags: stack-hud, par, crdt, collaboration*

**Intention:** Make PARs first class for fulab agents...

**Learned:** CRDT + emacsclient = elegant real-time human-agent
collaboration. Stack HUD needed clear separation of local vs remote
service concerns.

**Forward:** Add CRDT auto-start to server init.el...

---

## Lab Session Streaming & PAR Sidecar Architecture
*Session: 64570417-... | 2026-01-31T21:06:00Z*
*Tags: lab, websocket, par, architecture*

**Learned:** Sidecar files provide a clean way to augment immutable
data streams. http-kit rejects unmasked WebSocket frames from
non-browser clients - Java-WebSocket is more permissive.
```

Begin by fetching from the /rap endpoint, then present the prior learning to the user.
