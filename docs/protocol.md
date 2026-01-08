# MUSN Transport Protocol v1

The transport layer exposes JSON-lines over WebSocket (preferred) with HTTP `POST /musn/ingest` fallback. Every message includes `rev`, `type`, and `payload`. Unknown types must be rejected with `{"ok":false,"err":"unsupported-type"}`.

## Session Life-cycle
1. Client connects.
2. Client sends `hello` declaring name + capabilities. Negotiated rev must be `1`.
3. Client streams `event`, `session-close`, `export`, `run`, `status`, or optional `eval` messages (only if the REPL module is enabled). Each receives either `ack`/`status` or `err` JSON.
4. Client sends `bye` or closes connection. Server drains per-client channel and acknowledges.

## Message Formats
- `hello`: `{"rev":1,"type":"hello","payload":{"client":"NAME","caps":[...]}}` (payload is optional if the fields are top-level).
- `ack`: `{"ok":true,"type":"ack","eid":"E-000123","run-id":"RUN-1a2b3c","rev":1}` (events echo `eid`; session/export/run acks swap in `sid`/`scenario-path`/`job-id`).
- `event`: contains ISO-8601 timestamp `t`, `actor`, `verb`, `object`, and `prov` block (file + line), plus `msg-id`. Size limit 8 KiB.
- `session-close`: `{"rev":1,"type":"session-close","payload":{"msg-id":"...","sid":"S-123"}}`.
- `export`: `{"rev":1,"type":"export","payload":{"msg-id":"...","sid":"S-123"}}`.
- `run`: `{"rev":1,"type":"run","payload":{"msg-id":"...","sid":"S-123","policy":{...}}}` (or `scenario-path` instead of `sid`).
- `status`: `{"rev":1,"type":"status","payload":{"msg-id":"...","job-id":"JOB-123"}}`.
- `eval` (optional): `{"rev":1,"type":"eval","payload":{"code":"(+ 1 2)","mode":"safe","token":"…"}}`.
  - `mode` is optional. If omitted, the server’s `:repl.mode` config is used (`OFF`/`SAFE`/`ADMIN`).
  - SAFE mode runs inside a SCI sandbox (allowlisted symbols, no Java interop, no I/O). ADMIN mode evaluates arbitrary Clojure but requires both a matching token and a local (`127.0.0.1`/`::1` by default) client.
  - Response: `{"ok":true,"type":"eval","mode":"safe","run-id":"RUN-1234abcd","result":…}` or `{"ok":false,"type":"eval","err":"repl-disabled"}`.
- `query`: legacy frame kept for compatibility; the server currently returns `{ok:false, err:"query-disabled"}` to prompt callers to adopt `eval`.
- `bye`: `{"rev":1,"type":"bye"}` (no payload).

## Transport Guarantees
- **Ordering**: Messages processed in arrival order per client; different clients isolated via distinct async channels.
- **Idempotency**: Caller-provided `msg-id` is used for deduplication; duplicates short-circuit with the same reply payload.
- **Timeouts**: 100 ms CPU budget for SAFE `eval`. Timeouts surface as `{"ok":false,"type":"eval","err":"eval-timeout"}` without dropping the socket.
- **Execution caps**: `status` calls time out after 200 ms, while `run` submissions enforce a 5 s limit before returning `{ok:false,err:"timeout"}`.
- **Validation**: Schemas in `src/f2/schemas.edn` drive structural validation before adapter calls.

## Error Codes
- `unsupported-type`
- `invalid-payload`
- `timeout`
- `over-quota`
- `server-error`
- `repl-disabled`
- `admin-auth-failed`
- `eval-error` / `eval-timeout`

## HTTP Fallback
`POST /musn/ingest` accepts newline-delimited JSON frames using same schema. Response body is newline-delimited `ack/err` objects.
