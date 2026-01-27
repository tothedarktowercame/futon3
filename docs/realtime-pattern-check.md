# Realtime Pattern-Check Loop (Spec)

## Buffer
- Ingest new lines from IRC or a notify-log tail.
- Debounce into small batches (`max_wait=2s` or `max_lines=50`).
- If backlog grows, drop/skip older lines to preserve responsiveness.

## Matchers
- Sigil validation via `futon3.chops` (`chops/validate-sigil`).
- PSR/PUR matcher: detect structured PSR/PUR lines and extract pattern IDs.
- Duplicate sigil pair detection across devmaps (alarm condition).

## Schema (JSONL)
- Append JSONL records to `/tmp/musn_pattern_checks.jsonl`.
- One record per processed batch.

Example record:
```
{"ts":"2026-01-27T18:05:12Z","batch_lines":18,"sigils_checked":7,"invalid_sigils":["🐺/内"],"duplicate_pairs":["🐜/予"],"psr_seen":2,"pur_seen":1,"source":"irc"}
```

## Integration
- Chat ingest: IRC stream or notify-log tail.
- Optional rate-limited IRC warnings for invalid sigils or duplicates.
- Devmap/pattern files: read-only scans for duplicate sigil pairs.

## Async
- Parsing + validation runs off the chat ingest thread.
- Emit JSONL as a fast append; warnings are optional and throttled.
- Keep the loop responsive even when batches are skipped.

