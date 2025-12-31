# Golden Transcripts (Check Jobs)

This document defines canonical golden transcripts for the check-job transport
contract. The transcripts are used to validate that the A->B association
(request -> deterministic reply) is stable and machine-checkable.

## Transcript Format

Each transcript is a pair of NDJSON streams:
- `request.ndjson` (client frames)
- `reply.ndjson` (server frames)

Golden transcripts are byte-for-byte. To keep them deterministic, use the
sandbox profile described in `docs/sandbox/README.md` so the run-id and clock are
fixed.

## Transcript 1: pattern-apply (check)

Request (`check-request.ndjson`):

{"rev":1,"type":"hello","client":"golden","caps":["check"]}
{"rev":1,"type":"check","msg-id":"golden-check-001","payload":{"pattern/id":"library/devmap-coherence/ifr-f3-piti","context":"golden: workday submit proves proof trail and empowered action","evidence":["workday submit","proof trail","empowered action"]}}
{"rev":1,"type":"bye"}

Reply (`check-reply.ndjson`):

{"ok":true,"type":"ack","rev":1,"run-id":"RUN-GOLDEN-0001","client":"C-1"}
{"ok":true,"type":"check","run-id":"RUN-GOLDEN-0002","status":"applies","missing":[],"derived":[],"proof":{"proof/id":"PROOF-RUN-GOLDEN-0002","proof/run-id":"RUN-GOLDEN-0002","proof/recorded":"2026-01-01T00:00:00.000Z","pattern/id":"library/devmap-coherence/ifr-f3-piti","pattern/title":"musi (joy/empowered action)","pattern/rationale":"Joy-as-action: pattern checks prove obligations advance","pattern/hotwords":["workday submit","empowered action","proof trail"],"pattern/hanzi":"弓 (type-3 playful diligence)","check/context":"golden: workday submit proves proof trail and empowered action","check/evidence":["workday submit","proof trail","empowered action"],"check/sigils":[],"check/prototypes":[],"check/origin":{"source":"ws","client-id":"C-1","msg-id":"golden-check-001"},"proof/status":"applies","proof/hits":["empowered action","proof trail","workday submit"],"proof/similarity":1.0}}
{"ok":true,"type":"bye"}

## Transcript 2: gap-report (reserved)

Request (`gap-report-request.ndjson`):

{"rev":1,"type":"hello","client":"golden","caps":["gap-report"]}
{"rev":1,"type":"gap-report","payload":{"msg-id":"golden-gap-001","devmap/id":"futon3","gap/summary":"golden: missing trail-capture contract"}}
{"rev":1,"type":"bye"}

Reply (`gap-report-reply.ndjson`):

{"ok":true,"type":"ack","run-id":"RUN-GOLDEN-0002","rev":1}
{"ok":true,"type":"gap-report","run-id":"RUN-GOLDEN-0002"}
{"ok":true,"type":"bye"}

## Transcript 3: trail-capture (reserved)

Request (`trail-capture-request.ndjson`):

{"rev":1,"type":"hello","client":"golden","caps":["trail-capture"]}
{"rev":1,"type":"trail-capture","payload":{"msg-id":"golden-trail-001","trail/id":"trail-golden-001","entries":[{"t":"2026-01-01T00:00:00Z","intent":"golden: record proof trail"}]}}
{"rev":1,"type":"bye"}

Reply (`trail-capture-reply.ndjson`):

{"ok":true,"type":"ack","run-id":"RUN-GOLDEN-0003","rev":1}
{"ok":true,"type":"trail-capture","run-id":"RUN-GOLDEN-0003"}
{"ok":true,"type":"bye"}

## Determinism Requirements

- The sandbox profile must pin the run-id seed and clock so the reply NDJSON
  matches exactly.
- If a reply field is nondeterministic, it must be removed or pinned by the
  sandbox profile. The expected output stays byte-for-byte.

## Related Specs

- Contract details: `docs/protocol/transport-contract-v1.md`
- Sandbox profile and runner usage: `docs/sandbox/README.md`
