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

Connection Details
- **WSS endpoint**: `wss://<hostname>:5057/fulab/lab/upload/ws`
- **Backend**: `http://127.0.0.1:5056/fulab/lab/upload/ws`
- **TLS termination**: nginx on port 5057
- **Required client headers**: None special - standard WebSocket upgrade headers suffice

nginx config (add to existing 5057 server block):
```nginx
server {
    listen 5057 ssl;
    server_name <hostname>;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    # Lab WebSocket (streaming + upload)
    location /fulab/lab/ {
        proxy_pass http://127.0.0.1:5056;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_read_timeout 86400;  # 24h for long-lived connections
    }
}
```

The same 5056 backend handles both endpoints:
- `/fulab/lab/upload/ws` - laptop → server upload
- `/?path=...` - server → client streaming

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

Troubleshooting

**Diagnostics (run on Linode):**
```bash
# 1. Check lab-ws is listening
ss -ltnp | grep 5056
# Expected: LISTEN 0 50 *:5056 *:* users:(("java",...))

# 2. Check nginx is listening
ss -ltnp | grep 5057
# Expected: LISTEN 0 511 0.0.0.0:5057 0.0.0.0:*

# 3. Test direct backend
python3 -c "
import socket
s = socket.socket()
s.settimeout(2)
s.connect(('127.0.0.1', 5056))
s.send(b'GET /fulab/lab/upload/ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: test==\r\nSec-WebSocket-Version: 13\r\n\r\n')
print(s.recv(200).decode()[:100])
"
# Expected: HTTP/1.1 101 Web Socket Protocol Handshake

# 4. Check lab-ws status via REPL
./scripts/repl-eval '(futon3.lab.ws/status)'
# Expected: {:running true, :port 5056, :clients N}
```

**If lab-ws keeps dying:**
The server has a supervisor that auto-restarts it every 10 seconds. Start it with:
```clojure
(futon3.lab.ws/start-supervisor! {:port 5056})
```

**Common issues:**
- 502 from nginx: lab-ws not running or not accepting connections
- Handshake timeout: Check `ss -ltnp | grep 5056` shows LISTEN with low queue (not 40+)
- Path mismatch: Server accepts `/fulab/lab/upload/ws`, `/upload/ws`, or paths ending in `/lab/upload/ws`

Checklist
- [ ] Verify WSS endpoint reachable from laptop.
- [ ] Upload one Codex session and confirm file appears in lab/remote.
- [ ] Verify /fulab/lab/sessions/active lists the uploaded session.
- [ ] Verify PAR node is displayed in lab stream UI.
