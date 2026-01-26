# MUSN Transport Contract v1 (Check Jobs)

This document freezes the transport contract for check jobs that drive flexiformal
proofwork. It is a check-job-specific overlay on the general transport protocol in
`docs/protocol.md`.

## Scope

Contract v1 applies to three check jobs:
- pattern-apply (implemented as `type: "check"`)
- gap-report (reserved)
- trail-capture (reserved)

The contract defines message shapes, sequencing, and NDJSON log invariants so we
can demonstrate a reliable A->B association between proposals and evidence
collection. The A->B association is: a job request (A) deterministically produces
an ack + log record (B) that can be verified in transcripts.

## Envelope

All frames are JSON lines. Every frame includes:
- `rev` (integer protocol revision, must be `1`)
- `type` (string)
- `payload` (object)

Each job payload must include a stable `msg-id` for idempotency. Repeated `msg-id`
values must return the same reply payload.

## Sequencing

1. Client connects and receives server `hello`.
2. Client sends `hello` with `rev`, `client`, `caps`.
3. Client sends one or more check-job frames.
4. Client sends `bye` or closes the connection.

Check-job frames do not require a `session` frame. They can run over the transport
independently.

## Check Job Types

### pattern-apply

Transport type: `check`

Request payload:
- `msg-id` (string, required)
- `pattern/id` (string, required)
- `context` (string or map, required)
- `evidence` (vector of strings, optional)
- `meta` (map, optional)

Reply payload:
- `ok` (boolean)
- `type` = `"check"`
- `status` (keyword string: `"applies" | "blocked" | "gap"`)
- `run-id` (string)
- `proof` (map with `pattern/id`, `context`, `evidence`, optional)
- `err` (string, on failure)

### gap-report (reserved)

Transport type: `gap-report`

Request payload:
- `msg-id` (string, required)
- `devmap/id` (string, required)
- `pattern/id` (string, optional)
- `gap/summary` (string, required)
- `evidence` (vector of strings, optional)

Reply payload:
- `ok` (boolean)
- `type` = `"gap-report"`
- `run-id` (string)
- `err` (string, on failure)

### trail-capture (reserved)

Transport type: `trail-capture`

Request payload:
- `msg-id` (string, required)
- `trail/id` (string, required)
- `entries` (vector of maps, required)
- `meta` (map, optional)

Reply payload:
- `ok` (boolean)
- `type` = `"trail-capture"`
- `run-id` (string)
- `err` (string, on failure)

## NDJSON Log Invariants

Check-job runs must emit NDJSON log records with a stable mapping:
- One request line yields one reply line.
- The reply line includes the same `type` and a `run-id`.
- `msg-id` is stable across retries and is used for idempotency.

This is the machine-checkable A->B association:
- A = request line with `msg-id` and job payload.
- B = reply line with matching `type`, `run-id`, and deterministic outcome.

## Error Handling

If a required field is missing or malformed, return:
- `{"ok":false,"err":"invalid-payload"}`

If `rev` is unsupported, return:
- `{"ok":false,"err":"unsupported-rev"}`

Unsupported types must return:
- `{"ok":false,"err":"unsupported-type"}`

## Related Specs

- General transport overview: `docs/protocol.md`
- Golden transcripts for check jobs: `docs/protocol/golden-transcripts.md`
- Sandbox profile for deterministic replay: `docs/protocol/sandbox-profile.md`
