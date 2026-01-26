# Mission Queue Format

**Version**: 0.1
**Status**: Draft

---

## Overview

EDN-first format for tracking missions through the futon3a pipeline. Designed for:
- Manual execution with structured logging
- Future automation via P4 mission queue runner
- Integration with gate-check pattern from futon5

---

## Queue File

Location: `holes/missions/queue.edn`

```clojure
{:queue/version "0.1"
 :queue/updated #inst "2026-01-23T..."
 :missions [...]}
```

---

## Mission Entry Schema

```clojure
{:mission/id "M1"                      ; required, unique
 :mission/name "Smoke Test + H1-Lite"  ; human-readable
 :mission/type :stabilization          ; :stabilization, :hub, :integration, :exploration
 :mission/week [1 2]                   ; target weeks
 :mission/depends ["P0" "P1"]          ; dependency IDs
 :mission/gate "futon3a/P11"           ; gate requirement
 :mission/spec "M1-smoke-test.md"      ; path to spec file
 :mission/status :pending              ; :pending, :running, :passed, :failed, :blocked
 :mission/runs [...]}                  ; run history
```

---

## Mission Status States

| Status    | Meaning                                      |
|-----------|----------------------------------------------|
| :pending  | Not yet started                              |
| :blocked  | Waiting on dependencies                      |
| :running  | Currently being executed                     |
| :passed   | All success criteria met                     |
| :failed   | One or more criteria failed (needs retry)    |
| :skipped  | Intentionally skipped this cycle             |

---

## Run Entry Schema

```clojure
{:run/id "M1-run-001"                  ; unique run ID
 :run/started #inst "2026-01-23T..."
 :run/ended #inst "2026-01-23T..."
 :run/executor "joe"                   ; who/what ran it
 :run/mode :manual                     ; :manual, :automated
 :run/status :passed                   ; :passed, :failed, :aborted
 :run/criteria {...}                   ; per-criterion results
 :run/artifacts [...]}                 ; paths to artifacts
```

---

## Criteria Results Schema

```clojure
{:criteria
 {:portal-ping          {:status :pass :evidence "responded in 12ms"}
  :pattern-query        {:status :pass :evidence "returned 27 patterns"}
  :session-create       {:status :pass :evidence "musn-abc12345"}
  :log-valid-edn        {:status :fail :evidence "line 47 parse error"}
  :query-latency        {:status :skip :evidence "Futon1 not running"}}}
```

Status values: `:pass`, `:fail`, `:skip`

---

## Example Queue

```clojure
{:queue/version "0.1"
 :queue/updated #inst "2026-01-23T10:00:00Z"
 :missions
 [{:mission/id "M1"
   :mission/name "Smoke Test + H1-Lite"
   :mission/type :stabilization
   :mission/week [1 2]
   :mission/depends ["P0" "P1"]
   :mission/gate "futon3a/P11"
   :mission/spec "M1-smoke-test.md"
   :mission/status :pending
   :mission/runs []}

  {:mission/id "M2"
   :mission/name "Log Verification + Iteration"
   :mission/type :stabilization
   :mission/week [2 3]
   :mission/depends ["M1"]
   :mission/gate "futon3a/P11"
   :mission/spec "M2-log-verify.md"
   :mission/status :blocked
   :mission/runs []}

  {:mission/id "M3"
   :mission/name "Cross-Repo Hub Integration"
   :mission/type :hub
   :mission/week [3 4]
   :mission/depends ["M2"]
   :mission/gate "futon3a/P11"
   :mission/spec "M3-hub-integration.md"
   :mission/status :blocked
   :mission/runs []}]}
```

---

## Gate Check Integration

Missions can emit gate-check events compatible with futon5's pattern:

```clojure
{:event :gate-check
 :gate :mission/M1
 :timestamp 1706000000000
 :criteria {...}
 :all-pass? true
 :ready-for :M2}
```

This enables:
- Automatic dependency resolution
- Kanban-style mission tracking
- Integration with futon5 summary tools

---

## CLI Operations (Future P4)

```bash
# List pending missions
./scripts/mission-queue list --status pending

# Start a mission run
./scripts/mission-queue start M1 --executor joe

# Record criterion result
./scripts/mission-queue criterion M1-run-001 portal-ping --status pass --evidence "12ms"

# Complete a run
./scripts/mission-queue complete M1-run-001 --status passed

# Show mission status
./scripts/mission-queue status M1
```

---

## Notes

- Runs are append-only; failed runs stay in history
- Artifacts should be absolute paths or relative to mission spec
- The queue file is the single source of truth for mission state
- Manual execution logs to `/tmp/mission-<id>-<run>.log`
