# PAR: Post-Action Review for AI Sessions

PAR (Post-Action Review) is a structured reflection workflow for capturing distilled learning from AI collaboration sessions.

## The 5 Questions

1. **Intention** - What did we expect to learn or make together?
2. **Happening** - What actually happened? What approaches worked?
3. **Perspectives** - What are different angles on what occurred?
4. **Learned** - What explicit takeaways emerged?
5. **Forward** - What should we change or do next?

## Architecture

PARs are stored as **sidecar files** alongside Claude Code session transcripts:

```
~/.claude/projects/-home-joe/
  64570417-4354-40b8-b6a5-db804f69a1d0.jsonl      # Raw session transcript
  64570417-4354-40b8-b6a5-db804f69a1d0.par.edn    # PAR sidecar
```

The sidecar format (EDN):

```clojure
[{:id "par-20260131-201541"
  :timestamp "2026-01-31T20:15:41Z"
  :title "Stack HUD and PAR workflow"
  :tags [:stack-hud :par :crdt :collaboration]
  :questions
  {:intention "Make PARs first class for fulab agents..."
   :happening "Built phoebe/live-status endpoint..."
   :perspectives "Builder: Plumbing now in place..."
   :learned "CRDT + emacsclient = elegant collaboration..."
   :forward "Add CRDT auto-start to server init.el..."}}]
```

Multiple PARs per session are supported - they merge into the timeline by timestamp.

## Lab Integration

The `lab-ws` WebSocket server (port 5056) merges JSONL events with PAR sidecars when streaming sessions to the Lab viewer. PARs appear inline in the timeline with their own visual treatment.

```elisp
;; In Emacs, connect to stream a session with PARs
M-x fuclient-claude-stream-connect
```

## PAR vs RALPH

[RALPH](https://ghuntley.com/ralph/) (Ralph Wiggum technique) and PAR solve different problems:

| Aspect | RALPH | PAR |
|--------|-------|-----|
| **Focus** | Task completion through iteration | Transferable understanding |
| **Memory** | File artifacts + git history | Distilled, searchable summaries |
| **Learning** | Implicit (reads own past changes) | Explicit (captures WHY things worked) |
| **Scope** | Within a single task | Across tasks and sessions |
| **Persistence** | Project-specific file state | Portable insights |

### RALPH (tactical iteration)

RALPH feeds the same prompt repeatedly until task completion. "Memory" is raw state - Claude reads files it modified in previous iterations. This works well for well-defined tasks with automatic verification (tests, linters).

```bash
/ralph-loop "Build a REST API" --completion-promise "COMPLETE"
```

### PAR (strategic reflection)

PAR captures distilled insights that transfer across projects:

> "CRDT + emacsclient = elegant real-time human-agent collaboration"

That's a principle, not project-specific file state. Future sessions can retrieve relevant PARs by tag or semantic similarity.

### Complementary Use

They work well together:

1. **RALPH** iterates on a task until completion
2. **PAR** captures the learnings after completion
3. **Future sessions** get seeded with relevant PARs as context

## Workflow

### Creating a PAR

During or after a session, use the CRDT-based collaborative PAR editor:

```elisp
(require 'futon-crdt-par)
(futon-start-par)  ; Opens collaborative PAR buffer
```

Or create the sidecar file directly alongside the session JSONL.

#### Local sidecar write (Codex JSONL)

For Codex sessions stored as JSONL (e.g., `~/.codex/sessions/...`), you can
write the PAR sidecar locally without relying on MUSN sidecar resolution:

```elisp
(futon-par-set-jsonl-path "/home/joe/.codex/sessions/2026/02/01/<session>.jsonl")
(futon-par-submit)
```

This writes `<session>.par.edn` next to the JSONL file and enables live PAR
streaming via lab-ws when a watcher is active.

### Retrieving PARs: The `/rap` Endpoint

The `/rap` endpoint (port 6065) scans all `.par.edn` sidecars and returns distilled learning:

```bash
# Get all PARs (default limit 10)
curl localhost:6065/rap

# Filter by tags
curl "localhost:6065/rap?tags=websocket,architecture"

# Filter by date
curl "localhost:6065/rap?since=2026-01-31"

# Limit results
curl "localhost:6065/rap?limit=5"
```

Response includes:
- `pars` - array of PAR summaries (id, session-id, title, timestamp, tags, learned)
- `context` - pre-formatted markdown block ready for injection into a session

Example `context` output:
```markdown
# Prior Learning (from 2 PARs)

## Stack HUD and PAR workflow
*Session: 64570417-... | 2026-01-31T20:15:41Z*
*Tags: stack-hud, par, crdt, collaboration*

**Intention:** Make PARs first class for fulab agents...
**Learned:** CRDT + emacsclient = elegant collaboration...
```

### Injecting into Sessions

The `context` field from `/rap` can be pasted directly into a session prompt to seed it with prior learning. This is the "better RALPH" workflow - instead of implicit file-based memory, you get explicit distilled insights.

## What's Implemented

- **PAR sidecar format** - `.par.edn` files alongside session JSONL
- **Lab streaming** - `lab-ws` (port 5056) merges JSONL + PAR events by timestamp
- **Emacs viewer** - `fuclient-claude-stream.el` renders PARs inline in session stream
- **`/rap` endpoint** - retrieves and formats PARs for context injection
- **CRDT PAR editor** - `futon-crdt-par.el` for collaborative PAR authoring

## Future Directions

- **Claude Code hook** - auto-inject relevant PARs on session start
- **Elisp integration** - `M-x futon-inject-pars` to fetch and insert into chatgpt-shell
- **CLI tool** - `./scripts/rap` for quick terminal access
- **PAR file watcher** - live updates when sidecar changes (currently requires reconnect)
- **PAR creation UI** - create PARs directly in Lab viewer
- **Embeddings search** - semantic similarity matching for PAR retrieval
- **PSR/PUR integration** - connect Pattern Selection/Use Records to PAR workflow
- **`futon-par-submit` update** - write to sidecar instead of MUSN session
