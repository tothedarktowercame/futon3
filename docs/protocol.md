# MUSN Transport Protocol v1

The transport layer exposes JSON-lines over WebSocket (preferred) with HTTP `POST /musn/ingest` fallback. Every message includes `rev`, `type`, and `payload`. Unknown types must be rejected with `{"ok":false,"err":"unsupported-type"}`.

## Session Life-cycle
1. Client connects → server replies `{"rev":1,"type":"hello","caps":["event","session","eval"],"id":"srv"}`.
2. Client sends `hello` declaring name + capabilities. Negotiated rev must be `1`.
3. Client streams `event`, `session`, or optional `eval` messages (only if the REPL module is enabled). Each receives either `ack`/`run` or `err` JSON.
4. Client sends `bye` or closes connection. Server drains per-client channel and acknowledges.

## Message Formats
- `hello`: `{"rev":1,"type":"hello","client":"NAME","caps":[...]} `.
- `ack`: `{"ok":true,"id":"E-000123","rev":1}` (id echoes persisted event/session id).
- `event`: contains ISO-8601 timestamp `t`, `actor`, `verb`, `object`, and `prov` block (file + line). Optional `id`. Size limit 8 KiB.
- `session`: includes `sid`, ordered `events`, and `topic`. Max 100 events reference.
- `eval` (optional): `{"rev":1,"type":"eval","payload":{"code":"(+ 1 2)","mode":"safe","token":"…"}}`.
  - `mode` is optional. If omitted, the server’s `:repl.mode` config is used (`OFF`/`SAFE`/`ADMIN`).
  - SAFE mode runs inside a SCI sandbox (allowlisted symbols, no Java interop, no I/O). ADMIN mode evaluates arbitrary Clojure but requires both a matching token and a local (`127.0.0.1`/`::1` by default) client.
  - Response: `{"ok":true,"type":"eval","mode":"safe","run-id":"RUN-1234abcd","result":…}` or `{"ok":false,"type":"eval","err":"repl-disabled"}`.
- `query`: legacy frame kept for compatibility; the server currently returns `{ok:false, err:"query-disabled"}` to prompt callers to adopt `eval`.
- `bye`: `{"rev":1,"type":"bye"}` (no payload).

## Transport Guarantees
- **Ordering**: Messages processed in arrival order per client; different clients isolated via distinct async channels.
- **Idempotency**: Caller-provided `id`/`sid` combined with SHA of payload; duplicates short-circuit with same `ack` id.
- **Timeouts**: 200 ms for ingest commands; 100 ms CPU budget for SAFE `eval`. Timeouts surface as `{"ok":false,"err":"timeout"}` (ingest) or `{"ok":false,"type":"eval","err":"eval-timeout"}` without dropping the socket.
- **Validation**: Schemas in `/resources/schemas/*.edn` drive structural validation before graph ingestion.

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
