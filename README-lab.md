# Lab Upload Coordination (Codex -> Futon3)

Goal
- Stream Codex session JSONL from laptop to server over WSS.
- Store uploads on server FS as temporary redundancy until Futon1 persistence is live.
- Preserve PAR nodes in the stream.

Current implementation (Feb 1, 2026)
- Server endpoint: GET /fulab/lab/upload/ws (WebSocket)
- Upload storage root: lab/remote/
  - Session file: lab/remote/<session-id>.jsonl
  - Meta file: lab/remote/<session-id>.meta.edn
- Uploaded sessions appear in /fulab/lab/sessions/active and /fulab/lab/sessions
- PAR events are supported in JSONL parsing and in upload ingestion

Message protocol (client -> server)
- init
  - {"type":"init","session-id":"...","project":"...","source":"codex","originator":"laptop","cwd":"...","events":[...]}
  - "events" should be raw JSONL entries (maps) or raw JSON strings
- event
  - {"type":"event","session-id":"...","event":{...}}
- par (optional)
  - {"type":"par","session-id":"...","par":{...}}
- ping
  - {"type":"ping"} -> server replies {"type":"pong","at":"..."}

Notes
- WSS is required; no auth required.
- Server assumes TLS is terminated in front of Futon3 (reverse proxy).
- Session id is sanitized for filesystem usage.
- Upload storage is temporary; can be deleted once Futon1 persistence is wired.

Laptop uploader (reference)
- Script: dev/codex_upload_ws.clj
- Example:
  clojure -M -m futon3.dev.codex-upload-ws \
    --url wss://<server>/fulab/lab/upload/ws \
    --path ~/.codex/sessions/<session>.jsonl \
    --project /home/you/code/project \
    --originator laptop \
    --cwd /home/you/code/project

Questions for Claude
- Confirm how PAR should be encoded for Codex sessions (raw JSONL entry or separate "par" message).
- Confirm how/when server should delete lab/remote/* after Futon1 persistence.
- Confirm whether we need to dedupe repeated init uploads (current behavior appends).
- Confirm expected WSS termination config (nginx/caddy) and URL.

Checklist
- [ ] Verify WSS endpoint reachable from laptop.
- [ ] Upload one Codex session and confirm file appears in lab/remote.
- [ ] Verify /fulab/lab/sessions/active lists the uploaded session.
- [ ] Verify PAR node is displayed in lab stream UI.
