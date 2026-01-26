# MUSN Sandbox Profile (v1)

This document defines the canonical sandbox profile for running check jobs over
MUSN transport. The profile is a fixed runtime configuration that makes
transcripts reproducible and establishes the A->B association between proposals
and evidence collection.

## Sandbox Profile: sandbox-v1

Goals:
- Safe transport (no ADMIN eval).
- Deterministic run-id and clock for golden transcripts.
- Stable configuration for check jobs (pattern-apply, gap-report, trail-capture).

Profile settings:
- Transport: JSON lines over WebSocket, HTTP fallback enabled.
- REPL mode: `:off` or `:safe` only. ADMIN is disabled.
- Clock: fixed timestamp for golden transcript runs.
- Run-id: fixed seed so `run-id` values are deterministic.

## Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `FUTON3_LOG_PATH` | Base directory for MUSN logs | `futon3/logs/` |
| `FUTON3_PATTERN_PATH` | Pattern library location | `resources/sigils/patterns-index.tsv` |
| `FUTON3A_PORTAL` | Path to futon3a portal script | `../futon3a/scripts/portal` |
| `FUTON3A_ROOT` | Root of futon3a installation | (unset) |

## Runtime Dependencies

The sandbox profile expects:
- Clojure 1.11+
- Java 17+ (for `java.time` APIs)
- Access to `f0.clock` namespace for deterministic timestamps
- Pattern library at the configured path

## Running the Sandbox (Clojure Runner)

Start the transport with the default config:

```
clj -M -m f2.musn
```

To run the sandbox profile with explicit overrides from a Clojure runner:

```
clj -M -e "(require 'f2.musn) (f2.musn/start! {:repl {:mode :safe}})"
```

If you need a fixed clock and run-id seed for golden transcript replay, start the
system with a deterministic clock and a fixed `run-id` seed (wire this into the
runtime before capturing transcripts):

```
clj -M -e "(require 'f2.musn) (f2.musn/start! {:repl {:mode :safe} :clock {:fixed \"2026-01-01T00:00:00Z\"} :run-id-seed \"RUN-GOLDEN\"})"
```

Note: the fixed clock and run-id seed are required for byte-for-byte transcript
validation in `docs/protocol/golden-transcripts.md`. If the runtime does not yet
accept these fields, treat this as the expected wiring for the sandbox profile.

## Sending Check Jobs

Use NDJSON requests against the transport endpoint:

```
curl -s -X POST http://localhost:5050/musn/ingest --data-binary @check-request.ndjson
```

For repeatable runs, use the request NDJSON blocks defined in
`docs/protocol/golden-transcripts.md`.

## Related Specs

- Contract details: `docs/protocol/transport-contract-v1.md`
- Golden transcripts: `docs/protocol/golden-transcripts.md`
