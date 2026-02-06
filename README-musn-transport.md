# MUSN Transport Details

This document covers the MUSN transport layer, IRC bridge, and WebSocket protocol.
For the overview and quickstart, see `README.md`.

## MUSN IRC Bridge (remote)

To expose the IRC bridge on a server, enable auth + allowlisting and bind to 0.0.0.0:
```bash
MUSN_IRC_PASSWORD='your-secret' \
MUSN_IRC_ALLOWLIST='203.0.113.10,198.51.100.0/24' \
clojure -M -m scripts.musn-irc-bridge --host 0.0.0.0 --port 6667 --musn-url http://127.0.0.1:6065
```
Clients must send `PASS your-secret` before `NICK`/`USER`/`JOIN`. For encryption, put the port behind
an SSH tunnel or a TLS terminator (nginx stream / stunnel).
To have fuclaude/fucodex post chat via IRC, pass `--musn-pass` (and optionally
`--musn-irc-host`/`--musn-irc-port`) so `scripts/musn-chat` uses the bridge.

Hotloading fixes into the running server (optional):
```bash
ADMIN_TOKEN=$(cat .admintoken) clojure -M -e "
(require 'cemerick.drawbridge.client)
(require '[clojure.tools.nrepl :as nrepl])
(let [conn (nrepl/url-connect (str \"http://127.0.0.1:6767/repl?token=\" (System/getenv \"ADMIN_TOKEN\")))
      client (nrepl/client conn 2000)]
  (doall (nrepl/message client {:op \"eval\" :code \"(load-file \\\"src/futon3/fulab/hud.clj\\\")\"})))
"
```
See `README-drawbridge.md` for more details.

## Fucodex I Ching Demo

Run `./scripts/demo-fucodex-iching.sh` to clock in the I Ching hexagram patterns from
`library/iching/` and generate a short demo note (no code changes).

## MUSN Chat Supervisor

To drive fucodex entirely from IRC, run:
```bash
./scripts/musn_chat_supervisor.clj --room lab --bot-name fucodex --no-sandbox --approval-policy never
```
In IRC, post:
```
!new Review the new I Ching patterns and post a summary
!new token:fucodex2 Start a parallel review with a separate session
!task Continue with the active fucodex session
!task fucodex2 Continue with the fucodex2 session (after !new token:fucodex2)
!task sid:musn-abc123 Continue with a specific MUSN session id
```
`!new` establishes a fresh session id for a token (defaults to the bot name).
`!task` reuses the most recent session for that token; if no session exists yet,
it creates a new one. Use `token:`/`sid:` prefixes to force a target.
Use `--approval-policy never` (or `--allow-edits`) to avoid interactive approvals, or omit
it to keep Codex defaults. `--yolo` is an alias for `--no-sandbox` (dangerous).
Session mappings persist in `/tmp/musn_chat_sessions.edn`; delete it to reset.
To open the MUSN HUD/viewer for a session id, run in Emacs:
```
M-x fubar-musn-view-session
```

## WebSocket Walkthrough

```
wscat -c ws://localhost:5050/musn/ws
> {"type":"hello","client":"alice","caps":["eval","message"]}
< {"ok":true,"type":"ack","rev":1,"run-id":"RUN-af03f1e8","client":"C-5a2b7c1d"}
> {"type":"eval","payload":{"code":"(+ 1 2)"}}
< {"ok":false,"type":"eval","err":"repl-disabled"}
```

### Enabling the REPL

The REPL is **OFF** by default. Start the system with a config override to expose SAFE mode (SCI sandbox, allowlisted namespaces, 64 KB code / 1 MB result caps, 100 ms CPU budget):

```clojure
(require '[f2.musn :as musn])
(musn/stop!)
(def system (musn/start! {:repl {:mode :safe}}))
```

Now evals succeed:
```
> {"type":"eval","payload":{"code":"(mapv :id (musn.api/clients))"}}
< {"ok":true,"type":"eval","mode":"safe","run-id":"RUN-af03f1e8","result":["C-5a2b7c1d"]}
```

SAFE mode exposes a tiny DSL under `musn.api/*` (currently `clients`, `history`, `links`, `now`) plus a curated subset of `clojure.core`/`clojure.set`.

To unlock ADMIN mode (arbitrary `load-string`), provide a token and restrict it to local addresses:

```clojure
(musn/stop!)
(def system (musn/start! {:repl {:mode :admin
                                :admin-token "top-secret"
                                :admin-allow #{"127.0.0.1" "::1"}}}))
```

```
> {"type":"eval","payload":{"code":"(System/getProperty \"user.dir\")","mode":"admin","token":"top-secret"}}
< {"ok":true,"type":"eval","mode":"admin","run-id":"RUN-d7b0e995","result":"/home/joe/code/futon3"}
```

### HTTP Ingest

Eval frames are WebSocket-only (we need the client identity + remote IP for gating). Other frames can be posted as NDJSON:
```bash
curl -X POST http://localhost:5050/musn/ingest \
  --data '{"type":"event","payload":{"t":"2024-03-20T14:01:00Z","actor":"nora","verb":"observes","object":"practice:pairing","prov":{"file":"demo","line":1}}}'
```
Each line receives an `ack`/`err` JSON line in response.

### MUSN Transport Acceptance Tests

`clojure -M:test` exercises the Prototype 0 brief end-to-end:
- `test/f2/router_test.clj` replays 100 identical `event` frames (same `msg-id`) to ensure idempotent `eid`/`run-id` acks, asserts `export` never touches the scenario path on disk, and validates `run` → `status` transitions return a `:done` state with completion metrics via the mock F3 adapter.
- `test/transport_test.clj` drives the websocket control path so killing one client leaves peers untouched while the shared history logs a `:disconnect`, proving the per-client supervision rule in the MUSN spec.

### Groundhog Day demo (in progress)

- The deterministic transcript captured in `../futon5/resources/demos/groundhog_day_raw.json` (via `futon5.llm.relay`) is the canonical story we replay through MUSN. After the Futon3→Futon1 adapter lands we will ship a `make groundhog` target that turns this JSON into hello/event/workday/check frames, streams them through `/musn/ingest`, and verifies Futon1 receives matching proof/workday entities.
- `scripts/groundhog_day_ingest.clj` converts the JSON transcript into `dev/groundhog_day.ndjson` plus a clock-out snippet. Run it whenever you refresh the Futon5 capture.
- `scripts/run_groundhog_day.sh` posts the resulting NDJSON to `/musn/ingest` so you can replay the loop without opening Emacs.
- `scripts/groundhog_day_push_futon1.clj` posts the same NDJSON into a Futon1 API profile (default `testing`). Set `FUTON1_API_BASE`/`FUTON1_PROFILE` as needed so the demo appears under the right profile.

Non-interactive run:
```bash
cd futon3
./scripts/groundhog_day_ingest.clj
./scripts/run_groundhog_day.sh
FUTON1_API_BASE=http://localhost:8080/api/alpha \
FUTON1_PROFILE=testing \
  ./scripts/groundhog_day_push_futon1.clj
```
This reproduces the hello→event→workday→check→bye flow and mirrors the
proof/workday entities into Futon1 without touching the HUD.

## Drawbridge (nREPL over HTTP)

You can expose a full nREPL endpoint over HTTP using Cemerick Drawbridge. It is disabled by default—add `{:drawbridge {:enabled? true ...}}` when calling `musn/start!`, or use the helper make target.

```
(musn/stop!)
(def system (musn/start! {:drawbridge {:enabled? true
                                      :bind "127.0.0.1"
                                      :port 6767
                                      :allow ["127.0.0.1" "::1"]
                                      :token (System/getenv "ADMIN_TOKEN")}}))
```

If no token is supplied (env var or explicit), the Drawbridge listener is skipped for safety.

### `make repl`
```
make repl   # reads $ADMIN_TOKEN or .admintoken, starts Drawbridge on 127.0.0.1:6767
```

### Emacs / CIDER
1. Start Drawbridge locally or tunnel it (`ssh -N -L 6767:127.0.0.1:6767 user@server`).
2. `M-x cider-connect-clj-http`
   - URL: `http://localhost:6767/repl`
   - Headers: add `X-Admin-Token: <your-token>`.

### CLI sanity check
```
curl -s -H "X-Admin-Token: $$ADMIN_TOKEN" \
     -X POST http://localhost:6767/repl \
     -d '(+ 1 2 3)'
# => 6
```

Keep the listener bound to `127.0.0.1` (default) and tunnel when working remotely, or place it behind HTTPS + a reverse proxy. Never reuse your app/admin token here.

## UI Routes

- `GET /musn/clients` – connected clients, remote addr, current REPL mode, last activity
- `GET /musn/sessions` – recent history grouped by client (including SAFE eval outputs)
- `GET /musn/practices` – reasoning stubs (link suggestions + pattern instances)
- `GET /musn/tatami/status` – 24h summary of tatami sessions (emoji/fruit counts)
