# Codex Handoff: FuClaude Demo Pipeline Completion

## Context

The pattern competence pipeline (PUR/PSR validation via hx.logic) is ~90% complete.
All tests pass (70/70). What's missing is the bridge from live Claude sessions to
the session EDN format that validators can check.

## What's Done

### Core Infrastructure
- `src/f2/claude.clj` - Clock-in/out protocol, event emission, pattern tracking
- `src/f2/ui.clj` - HTTP API endpoints (`/claude/run`, `/claude/events/`, etc.)
- `src/futon3/hx/logic.clj` - core.logic validators for PUR (V1-V7) and PSR (W1-W5)
- `src/futon3/fulab/pattern_competence.clj` - Helper functions for claims/verification

### CLI Tools
- `fulab-pattern-claim` - Write PUR/PSR templates into session files
- `fulab-pattern-check` - Validate claims and append verification events
- `fulab-pattern-report` - Generate coverage reports
- `fulab-pattern-suggest` - Draft PSR from context (unverified)

### Data
- `resources/sigils/patterns-index.tsv` - 164+ patterns with IDs
- `lab/sessions/claude-2026-01-01-001.edn` - Exemplar session file (hand-authored)

### Emacs Client
- `contrib/futon3-claude.el` - `my-futon3-claude-run-with-pattern` with clock-in support

## What's Missing

### 1. JSONL → Session EDN Translator

**File to create:** `dev/lab-import-session.clj`

**Purpose:** Convert Claude Code's native JSONL transcript into the session EDN format
that PUR anchor validators can resolve against.

**Input:** Claude Code session JSONL (typically `~/.claude/sessions/<id>.jsonl`)

**Output:** Session EDN file in `lab/sessions/<session-id>.edn`

**Key transformation:** Tool calls must become `:events` entries with the right shape:

```clojure
;; From JSONL tool_use:
{"type": "tool_use", "name": "Edit", "input": {"file_path": "src/foo.clj", ...}}

;; To session EDN event:
{:event/type :code/edit
 :file "src/foo.clj"
 :fn nil  ; or extracted if possible
 :action :modified
 :description "Edit tool call"
 :at #inst "2026-01-07T..."}
```

**Mapping rules:**
- `Edit` tool → `:event/type :code/edit`, `:action :modified`
- `Write` tool → `:event/type :code/edit`, `:action :added`
- `Read` tool → `:event/type :code/read` (optional, for traceability)
- `Bash` tool → `:event/type :shell/command`

**Session structure:**
```clojure
{:session/id "claude-2026-01-07-xxx"
 :session/agent :claude
 :clock-in {:clock-in/pattern-id "..."
            :clock-in/intent "..."
            :clock-in/timestamp #inst "..."}
 :events [...] ;; Transformed from JSONL
 :artifacts [...] ;; File paths touched
 :clock-out {:session/status :success
             :clock-out/timestamp #inst "..."}}
```

### 2. CLI Wrapper

**File to create:** `fulab-session-import` (bash wrapper)

```bash
#!/usr/bin/env bash
# Usage: fulab-session-import --jsonl PATH --session-id ID [--pattern-id PID] [--intent TEXT]
```

### 3. Integration with f2/claude.clj Event Stream

**Optional enhancement:** Instead of post-hoc JSONL import, consume events from
`(:events-ch session)` in real-time and write to session file incrementally.

This would require a lab capture consumer in `src/futon3/fulab/capture.clj`:

```clojure
(defn start-capture! [session-id events-ch lab-root]
  ;; Consume events from channel, append to session file
  ...)
```

Wire into `f2/claude.clj` `start-session!` to optionally enable capture.

## Validation Test

After implementation, this should work:

```bash
# 1. Import a real Claude session
./fulab-session-import --jsonl ~/.claude/sessions/abc123.jsonl \
                       --session-id demo-test-001 \
                       --pattern-id "fulab/clock-in"

# 2. Add a PUR claim (could be manual or auto-generated)
./fulab-pattern-claim --session-id demo-test-001 --kind pur --stdin <<'EOF'
{:pur/id "pur-demo-1"
 :session/id "demo-test-001"
 :pattern/id "fulab/clock-in"
 :instance/id "demo-1-a"
 :fields {:context "Testing import pipeline"
          :if "Session JSONL exists"
          :however "No EDN format yet"
          :then "Import and validate"
          :because "Demo readiness"
          :next-steps "Run check"}
 :anchors [{:anchor/type :code/edit
            :anchor/ref {:event/type :code/edit
                         :file "src/some/file.clj"}}]}
EOF

# 3. Validate - anchors should resolve to imported events
./fulab-pattern-check --session-id demo-test-001 --dry-run
# Expected: [lab-pattern-check] :pattern/use-claimed pur-demo-1 -> pass

# 4. Report
./fulab-pattern-report --session-id demo-test-001
```

## Reference Files

Read these to understand the target format:
- `lab/sessions/claude-2026-01-01-001.edn` - Exemplar session
- `test/pattern_competence_test.clj` - Validator test cases
- `src/futon3/hx/logic.clj` - Anchor resolution logic (search for `anchor-resolves?`)
- `dev/lab-export-claude.clj` - Existing JSONL reader (outputs org, not EDN)

## Priority

1. **P0:** `dev/lab-import-session.clj` + `fulab-session-import` wrapper
2. **P1:** Test with real Claude session JSONL
3. **P2:** Optional real-time capture integration

## Success Criteria

- Can import any Claude Code JSONL into session EDN format
- PUR anchors referencing files edited in that session resolve correctly
- `fulab-pattern-check` passes for valid claims against imported sessions
