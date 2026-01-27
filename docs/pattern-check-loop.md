# Realtime Pattern-Check Loop

## Purpose
Continuously validate sigils and flag devmap/pattern coherence issues from live chat
without blocking the IRC relay.

## Inputs
- IRC lines (stream or notify-log file)
- Canonical sigil sets via futon3.chops
- Devmaps and pattern library (for duplicate sigil checks)

## Outputs
- JSONL reports to /tmp/musn_pattern_checks.jsonl
- Optional IRC warnings for invalid sigils or duplicate pairs

## Core Loop
1) Ingest new lines from IRC or notify-log.
2) Batch with debounce (e.g. max_wait=2s, max_lines=50).
3) Extract sigils and pattern IDs.
4) Validate sigils with futon3.chops; detect duplicate sigil pairs in devmaps.
5) Emit JSONL report; optionally emit IRC warnings.

## Minimal First Pass (Low Risk)
- Read-only: no repo writes.
- No IRC warnings, just JSONL output.
- Short batches keep latency low.

## Optional Extensions
- Periodic full audit via scripts/audit-sigils.
- Live stats endpoint (HTTP/WS).
- Migrations suggestion via scripts/migrate-sigils --dry-run.
