# Realtime Pattern-Check Loop (Spec)

This document specifies the realtime pattern-check loop that validates sigils and
signals pattern/devmap coherence issues from live chat without blocking the IRC
relay. It is read-only with respect to the repo (no writes outside JSONL logs).

## 1) Buffer mechanism

### Inputs
- Line events from IRC (preferred) or a notify-log tail.
- Each line is normalized into a line item with:
  - `ts` (ISO-8601 UTC), `source` (irc | notify-log), `raw` (string)
  - optional metadata: `channel`, `nick`, `host`, `msg_id`

### Buffering
- Maintain an in-memory ring buffer of line items (default `max_buffer_lines=200`).
- Debounce into batches using:
  - `max_wait` (default 2s)
  - `max_lines` (default 50)
- Flush conditions:
  - `max_lines` reached, or
  - `max_wait` elapsed since first line in batch

### Overload behavior
- If buffer exceeds `max_buffer_lines`, drop oldest items and record `dropped`.
- If processing lags behind ingestion by >2 batches, skip to the most recent
  batch and mark `skipped_batches` in the JSONL output.
- The loop must remain responsive; correctness is best-effort under overload.

## 2) Pattern matchers (chops, PSR/PUR)

### Sigil extraction + validation
- Extract candidate sigils by scanning for `emoji/hanzi` tokens.
- For each candidate, validate with `futon3.chops`:
  - `(chops/validate-sigil sigil)`
  - Record `valid?`, `decoded.word1`, `decoded.word2`, and `decoded.reading`.
- Maintain counts:
  - `sigils.checked`, `sigils.valid`, `sigils.invalid`.

### PSR / PUR matcher
- Detect Pattern Status Reports (PSR) and Pattern Update Reports (PUR).
- Accept both compact and expanded formats:
  - `PSR: pattern-id=<id> status=<status> ...`
  - `PUR: pattern-id=<id> change=<summary> ...`
- Extract `pattern-id` and any known key-value pairs; store raw remainder for
  unknown fields.
- Track totals: `psr.seen`, `pur.seen`, and arrays of `psr.ids`, `pur.ids`.

### Duplicate sigil pair detection
- Build a cached map of sigil-pairs from devmaps/pattern library files.
- Flag duplicates when the same sigil pair appears in multiple devmaps.
- Cache refreshes on a timer (default 60s) to keep checks fast and read-only.

## 3) JSONL schema

### Output file
- Append-only JSONL file at `/tmp/musn_pattern_checks.jsonl`.
- One record per processed batch.

### Record schema (v1)
```
{
  "ts": "2026-01-27T18:05:12Z",
  "source": "irc",
  "batch": {
    "lines": 18,
    "dropped": 0,
    "skipped_batches": 0,
    "latency_ms": 120
  },
  "sigils": {
    "checked": 7,
    "valid": 7,
    "invalid": []
  },
  "sigil_reads": [
    {"sigil": "🐜/予", "word1": "lili", "word2": "e", "reading": "lili e"}
  ],
  "duplicates": {
    "pairs": [],
    "count": 0
  },
  "psr": {"seen": 2, "ids": ["stack-blocker-detection"]},
  "pur": {"seen": 1, "ids": ["pattern-differentiation-alarms"]},
  "errors": []
}
```

### Notes
- `sigil_reads` is optional and only includes validated sigils with decoded
  readings (to keep records compact).
- `errors` contains non-fatal exceptions or parse warnings.

## 4) Integration points

### Inputs
- IRC stream integration (primary): subscribe to relay output or socket feed.
- Notify-log tail (fallback): read-only file tailer.

### Outputs
- JSONL append to `/tmp/musn_pattern_checks.jsonl`.
- Optional IRC warnings (rate-limited) for:
  - invalid sigils
  - duplicate sigil pairs

### Repo artifacts
- Read-only scans of devmaps/pattern library for duplicate sigil pairs.
- Optional periodic audit via `scripts/audit-sigils` (out of band).
- Optional migration hints via `scripts/migrate-sigils --dry-run`.

## 5) Async model

### Threads / queues
- Ingest thread: reads lines, enqueues line items into the buffer.
- Worker thread: builds batches, runs matchers, appends JSONL.
- Use a bounded queue; on overflow, drop oldest items and log `overload`.

### Timing constraints
- Batch processing must not block ingest.
- JSONL writes are simple append operations and should complete quickly.
- If processing falls behind, skip batches rather than slow the relay.

### Error handling
- All matcher failures are non-fatal; emit to `errors` and continue.
- Invalid inputs never crash the loop.
