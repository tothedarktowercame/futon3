# Realtime Pattern-Check Loop (Spec)

## Scope
Continuously validate sigils and detect PSR/PUR activity from live chat without blocking
IRC relay or Codex streaming. Output is read-only JSONL; no repo writes or migrations.

## 1) Buffer Mechanism
- **Sources**: IRC stream, notify-log tail, or other line-oriented feed.
- **Normalization**: trim whitespace; ignore empty lines; attach source metadata.
- **Batching**: debounce into small batches (`max_wait=2s` or `max_lines=50`).
- **Backlog cap**: bounded buffer (default `max_buffer=1000` lines). If overflow,
  drop oldest lines and record `dropped_lines` for the batch.
- **Batch metadata**: assign `batch_id` (monotonic), `window_start`, `window_end`,
  and `source` to each batch for traceability.

## 2) Pattern Matchers
### 2.1 Sigil extraction + chops
- Extract `emoji/hanzi` tokens from each line (simple tokenization; allow multiple
  per line).
- Validate each candidate with `futon3.chops/validate-sigil`.
- Record `invalid_sigils` (unique, per batch) and `sigils_checked` count.
- Optional: retain decoded readings for diagnostics (do not emit to IRC by default).

### 2.2 PSR/PUR detection
- Detect structured PSR/PUR lines in JSON/EDN payloads emitted by live runs.
- Recognize event names:
  - `:pattern/selection-claimed` (PSR)
  - `:pattern/use-claimed` (PUR)
  - `turn/select` (PSR)
  - `turn/use` (PUR)
- Extract identifiers when present: `:pattern/id`, `:session/id`, `:decision/id`.
- Report `psr_seen` and `pur_seen` counts; optionally include `psr_ids` / `pur_ids`.

### 2.3 Duplicate sigil pair detection
- Read devmap + pattern files (configurable globs) and map `sigil -> locations`.
- Flag duplicates where the same sigil pair appears in multiple locations.
- Cache the scan results and refresh on a timer (e.g. every 60s) to avoid
  per-batch filesystem churn.

## 3) JSONL Schema
- Append JSONL records to `/tmp/musn_pattern_checks.jsonl`.
- One record per processed batch; fast append only.

Required fields:
- `ts` (ISO-8601 UTC)
- `batch_id` (int)
- `source` (string)
- `batch_lines` (int)
- `sigils_checked` (int)
- `invalid_sigils` (array of strings)
- `duplicate_pairs` (array of strings)
- `psr_seen` (int)
- `pur_seen` (int)

Optional fields:
- `dropped_lines` (int)
- `window_start` / `window_end` (ISO-8601)
- `psr_ids` / `pur_ids` (array of ids)
- `latency_ms` (int)
- `overload` (boolean)

Example record:
```
{"ts":"2026-01-27T18:05:12Z","batch_id":42,"source":"irc","batch_lines":18,"sigils_checked":7,"invalid_sigils":["🐺/内"],"duplicate_pairs":["🐜/予"],"psr_seen":2,"pur_seen":1,"dropped_lines":0}
```

## 4) Integration Points
- **IRC ingest**: live stream or notify-log tail as the primary feed.
- **Optional IRC warnings**: rate-limited notices for invalid sigils or duplicates.
- **Devmap/pattern scans**: read-only checks for duplicate sigils across devmaps.
- **AIF/PSR/PUR sources**: session files (`lab/sessions/<session-id>.edn`) and
  live stream events (`:pattern/selection-claimed`, `:pattern/use-claimed`).
- **Batch audits**: optional periodic full checks via `scripts/audit-sigils`.

## 5) Async Model
- **Ingest thread**: non-blocking; pushes lines into a bounded queue.
- **Worker**: parses + validates batches off the ingest thread.
- **Output**: JSONL append is synchronous per batch but must stay under 10ms.
- **Overload**: when queue is full, drop oldest lines and emit a batch with
  `overload=true` and `dropped_lines`.
- **Responsiveness**: prioritize low latency over completeness; skipping is ok
  as long as it is reported.
