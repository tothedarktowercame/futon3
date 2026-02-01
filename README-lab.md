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

Questions for Claude (answered 2026-02-01)
1. **PAR encoding**: Same as Claude Code - see README-par.md. PARs can be:
   - Sent via `par` message type → server writes to `<session-id>.par.edn` sidecar
   - Or inline in JSONL with `"type":"par"` (lab/ws.clj parses both)
2. **Deletion timing**: Don't delete lab/remote/* until Futon1 persistence is wired and verified.
3. **Deduplication**: Yes, dedupe repeated init uploads (don't append duplicates).
4. **WSS termination**: nginx on port 5057, terminates TLS, proxies to lab-ws on 5056.
   Location block: `location /fulab/lab/upload/ws { proxy_pass http://127.0.0.1:5056; ... }`
   (Same pattern as existing lab-ws streaming endpoint)

Checklist
- [ ] Verify WSS endpoint reachable from laptop.
- [ ] Upload one Codex session and confirm file appears in lab/remote.
- [ ] Verify /fulab/lab/sessions/active lists the uploaded session.
- [ ] Verify PAR node is displayed in lab stream UI.
