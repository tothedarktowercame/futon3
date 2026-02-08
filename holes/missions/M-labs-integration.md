# Mission: Labs Integration Improvements

## Owner

TBD (this document mixes completed work and open follow-ups; assign an owner for the remaining open items before continuing).

## Scope

### Scope In

- Track and prioritize remaining Labs integration gaps (PAR workflow validation, overlay persistence, Arxana navigation bugs).
- Ensure "implemented" items remain stable (lab persistence to futon1, smart streaming).
- Split large buckets into smaller missions when they become actionable.

### Scope Out

- New major features (e.g., affect heatmap) unless promoted into a dedicated mission.
- Rewriting the entire labs pipeline (focus on incremental fixes).

## Time Box

4-8 hours to triage and split open items into bounded child missions with explicit owners.

## Exit Conditions

- Open unchecked items are either completed or moved into dedicated missions with owners/time boxes.
- This mission doc becomes an index/status page rather than an ever-growing backlog.

## Context

Labs provides session capture and browsing via Arxana overlays. The core streaming
pipeline works (futon3 MUSN → futon4-lab-bridge → lab/raw/*.json → arxana-lab viewer),
but several issues need attention.

## Blocking Issue - Lab Persistence to Futon1

**Status: IMPLEMENTED** (2026-02-06)

Sessions are now persisted to Futon1 XTDB via the `/api/alpha/lab/session` endpoint.
Persistence is triggered by:

1. **PAR submission** - Every PAR checkpoint triggers a save
2. **Context compaction** - When Claude Code/Codex compresses context
3. **Periodic timer** - Every 20 minutes (configurable)

### Configuration

```clojure
;; Enable via environment:
;; FUTON1_LAB_ENABLED=true
;; FUTON1_API_BASE=http://localhost:8080

;; Or programmatically:
(require '[futon3.musn.service :as svc])
(svc/configure-futon1! {:enabled? true
                        :api-base "http://localhost:8080"})

;; Start periodic saves:
(svc/start-periodic-saves!)
```

### HTTP Endpoints

- `POST /musn/context/compacted` - Trigger save on context compaction
- `POST /musn/futon1/configure` - Configure persistence settings
- `POST /musn/futon1/status` - Check persistence status

### Key Files

| File | Changes |
|------|---------|
| `futon3/src/futon3/futon1_bridge.clj` | Added `record-lab-session!` |
| `futon3/src/futon3/musn/service.clj` | Added persistence hooks and scheduler |
| `futon3/src/futon3/musn/http.clj` | Added HTTP endpoints |
| `futon1/apps/api/src/api/handlers/lab.clj` | Lab session ingest API |

## Bug Fixes (2026-02-06)

- [x] **PARs appearing at end of lab timeline** - The `futon4-lab-bridge.el` wasn't
      including `lab/all-events` in the raw JSON output. Events were accumulated but
      never written. Fixed by adding `lab/all-events` and `lab/source` to the output.
      File: `futon3/contrib/futon4-lab-bridge.el` line 245

- [x] **PAR sequence counter bug** - `create-par!` was reading `(:events entry)`
      instead of `(:events @(:lab-session entry))`, causing sequence to always be 1.
      File: `futon3/src/futon3/musn/service.clj` line 2280

## PAR Workflow Issues

- [ ] **No validation before submission** - PARs can be submitted without all 5
      questions answered. No word count limits or quality checks.
      File: `futon0/contrib/futon-crdt-par.el`

- [ ] **Fragile response parsing** - Agency response structure is nested
      (`:response/:result`) and error handling is minimal.
      File: `futon0/contrib/futon-par-peripheral.el` lines 106-132

- [ ] **Sidecar path resolution** - Falls back gracefully but may create `.par.edn`
      in unexpected locations if path not explicit.
      File: `futon-crdt-par.el` lines 325-334

## Arxana/Labs Integration Issues

- [ ] **Overlay highlighting hardcoded buffer** - `arxana-hop-to-link` hardcodes
      `*Claude Stream*` buffer name; won't work if buffer named differently.
      File: `futon0/contrib/arxana-hop.el` lines 73-75

- [ ] **Lab files browser navigation** - `arxana-browser-lab-browse-files` doesn't
      properly update `arxana-browser--context`, breaking back-navigation.
      File: `futon4/dev/arxana-browser-lab.el` lines 384-399

- [ ] **Enrichment async error handling** - If enrichment request times out,
      pending count decremented but enrichment not stored; no user feedback.
      File: `futon3/contrib/futon4-lab-bridge.el` lines 110-135

- [ ] **Session deduplication** - README-lab.md mentions dedupe needed but
      implementation not found. May have duplicate raw files if upload interrupted.

## Affect Events Integration (New Feature)

- [ ] **Add affect/transition to lab timeline** - Render affect events alongside
      PSR/PUR/AIF with new `arxana-lab-affect-face`

- [ ] **Affect overlay in session viewer** - Show trigger text, affect type, and
      novel terms that emerged in the lookahead window

- [ ] **Filter/query by affect** - "Show sessions with :joy affects" or
      "show all affects mentioning term X"

- [ ] **Affect heatmap view** - Density of affect types across days/weeks

## Smart Streaming (2026-02-06)

**Status: IMPLEMENTED**

To prevent OOM from repeatedly streaming large session files (90MB+), the
`fuclient-claude-stream` now supports smart streaming:

1. **Futon1 cache check** - Before streaming, checks if session exists in Futon1
2. **Cached load** - If found, loads from Futon1 XTDB (fast, no streaming)
3. **Delta streaming** - If JSONL has more lines than cached events, streams only new data
4. **Size threshold** - Only checks cache for files > 1MB (configurable)

### Configuration

```elisp
;; Enable/disable Futon1 cache (default: t)
(setq fuclient-claude-stream-use-futon1-cache t)

;; Futon1 API base URL
(setq fuclient-claude-stream-futon1-api-base "http://localhost:8080")

;; Size threshold in KB (default: 1024 = 1MB)
(setq fuclient-claude-stream-cache-threshold-kb 1024)
```

### Key Changes

| File | Changes |
|------|---------|
| `futon3/contrib/fuclient-claude-stream.el` | Added Futon1 cache lookup, delta streaming |
| `futon3/src/futon3/lab/ws.clj` | Added `offset` query param for incremental streaming |

### WebSocket Protocol

- `?path=/path/to/session.jsonl` - Stream full session
- `?path=/path/to/session.jsonl&offset=500` - Stream from line 500 onwards
- `?path=/path/to/session.jsonl&offset=500&par_offset=3` - Stream from line 500, skip first 3 PARs
- Response `type: "delta"` indicates incremental update
- Response includes `new-par-count` for PARs added since cache

### PAR Handling

When doing delta streaming, the client passes `par_offset` (number of PARs in cache).
The server reads the full PAR sidecar but skips the first N entries, returning only
new PARs. The PAR watcher remains active for live updates.

## Missing/Gap Areas

- [ ] **Overlay persistence** - Overlays created dynamically but not saved/restored;
      link highlights cleared on buffer refresh; no annotation storage

- [ ] **Pattern proposal integration** - PSR/PUR data not mined for pattern
      suggestions; no direct integration to propose patterns from lab sessions

## Key Files

| File | Purpose |
|------|---------|
| `futon3/src/futon3/futon1_bridge.clj` | Bridge to Futon1 API (lab persistence, workday, checks) |
| `futon3/src/futon3/musn/service.clj` | MUSN session management, PAR, persistence scheduling |
| `futon4/dev/arxana-lab.el` | Lab notebook viewer, MUSN timeline, hyperlinks |
| `futon4/dev/arxana-browser-lab.el` | Arxana browser integration for lab sessions |
| `futon3/contrib/futon4-lab-bridge.el` | Stream MUSN events to lab/raw, enrichment |
| `futon0/contrib/futon-crdt-par.el` | Collaborative PAR creation and submission |
| `futon0/contrib/futon-par-peripheral.el` | Agent-based PAR participation via CRDT |
| `futon3/contrib/arxana-hop.el` | Anchor linking and navigation |
| `futon1/apps/api/src/api/handlers/lab.clj` | Lab session XTDB ingest API |

## Related Documentation

- `futon3/docs/fulab-plumbing.org` - Architecture notes
- `futon3/README-lab.md` - Lab upload and processing
- `futon3/README-affect.md` - Affect signal processing
