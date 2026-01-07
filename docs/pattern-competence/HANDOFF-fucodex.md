# Handoff: Fully Working Fucodex Lab Session

This plan focuses on Fucodex + Fubar (Codex CLI + Emacs logging) and keeps paths
aligned with the futon3 repo layout.

## Goal

Produce a complete, checkable lab session that:
- lives in `futon3/lab/sessions/<session-id>.edn`
- includes resolvable `:events` anchors for PUR/PSR validation
- can be claimed/checked/reported via `fulab-pattern-*` CLI

## Ground Truth Paths (repo)

- Session store (target): `futon3/lab/sessions/`
- Fubar event log (source): `futon3/resources/fubar-events.edn`
- Pattern catalog: `futon3/resources/sigils/patterns-index.tsv`
- Pattern competence tools: `futon3/fulab-pattern-claim`, `futon3/fulab-pattern-check`, `futon3/fulab-pattern-report`
- Example session shape: `futon3/lab/sessions/claude-2026-01-01-001.edn`

## P0 — Minimal viable session (import + anchors)

### 1) Build an importer for Fubar events

**File:** `futon3/dev/lab-import-fubar.clj`

**Input:** `futon3/resources/fubar-events.edn`

**Output:** `futon3/lab/sessions/<session-id>.edn`

**Mapping (start simple):**
- `:event/type :clock-in/start` → `:clock-in` block + a `:events` entry
- `:event/type :pattern/used` → `:events` entry with `:event/type :pattern/used`
- `:event/type :clock-out/complete` → `:clock-out` block + `:events` entry
- `:artifacts` (if present) → `:artifacts` vector at top-level

**Event shape (anchorable):**
```edn
{:event/type :code/edit
 :file "src/f2/codex.clj"
 :fn "record-artifact!"
 :action :modified
 :description "Edit tool call"
 :at #inst "2026-01-07T..."}
```

If fubar events include only pattern/clock events, emit a minimal
`:event/type :pattern/used` event and include `:pattern/id` so
anchors can resolve against those events.

### 2) Create a CLI wrapper

**File:** `futon3/fulab-session-import-fubar`

```bash
#!/usr/bin/env bash
# Usage: fulab-session-import-fucodex --session-id ID [--lab-root PATH]
```

Implementation should run:
```
clojure -M futon3/dev/lab-import-fubar.clj --session-id ID --lab-root futon3/lab
```

### 3) Validate a PUR anchor against imported events

Example (manual) PUR:
```edn
{:pur/id "pur-demo-1"
 :session/id "demo-fucodex-001"
 :pattern/id "fulab/clock-in"
 :instance/id "demo-1-a"
 :fields {:context "Imported fubar events"
          :if "Clock-in was recorded"
          :however "No EDN session existed"
          :then "Import and validate"
          :because "Fucodex readiness"
          :next-steps "Run check"}
 :anchors [{:anchor/type :pattern/used
            :anchor/ref {:event/type :pattern/used
                         :pattern/id "fulab/clock-in"}}]}
```

Commands:
```bash
futon3/fulab-pattern-claim --session-id demo-fucodex-001 --kind pur --stdin < pur.edn
futon3/fulab-pattern-check --session-id demo-fucodex-001 --dry-run
futon3/fulab-pattern-report --session-id demo-fucodex-001
```

## P1 — Enrich anchors with file-level evidence

Add one or more `:code/edit` events by extracting artifact paths from
fubar events or by parsing Codex tool logs (if available in the session).

At minimum, add these to `:events` and `:artifacts`:
- `src/f2/codex.clj`
- `contrib/futon3-codex.el`

This enables anchors like:
```edn
{:anchor/type :code/edit
 :anchor/ref {:event/type :code/edit
              :file "src/f2/codex.clj"}}
```

## P2 — Full PSR/PUR round-trip

1) Draft PSR with explicit alternatives and forecast loci:
```bash
futon3/fulab-pattern-claim --session-id demo-fucodex-001 --kind psr --decision-id decision-1
```

2) Edit the PSR to include `:context/anchors` and forecast `:locus` anchors.

3) Add matching `:decision/id` to a PUR for the same session.

4) Verify and report:
```bash
futon3/fulab-pattern-check --session-id demo-fucodex-001
futon3/fulab-pattern-report --session-id demo-fucodex-001
```

## P3 — Optional real-time capture (future)

If we want streaming capture for fucodex runs, add a small consumer that
subscribes to the `f2/codex` event channel and appends to the session file
in `futon3/lab/sessions/`. This keeps the importer as a fallback.

## Success Criteria

- `futon3/lab/sessions/<session-id>.edn` exists and is append-only
- PUR anchors resolve against the imported session `:events`
- PSR anchors resolve and `fulab-pattern-check` reports pass
- Report links PSR ↔ PUR via `:decision/id`
