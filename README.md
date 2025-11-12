# Futon3 MUSN Sandbox

A modern Clojure rewrite of the vintage Monster Mountain / MUSN server. The transport provides a battle-tested ingest path plus an **optional** multi-user REPL (SAFE via SCI, ADMIN via tokenized unsafe eval) while we wait to slot in an external persistence layer.

## Prerequisites
- JDK 11+ and `clojure` CLI tools (`brew install clojure/tools-deps` or similar)
- Network access the first time you run `make test`/`make dev` so dependencies can be cached into the repo-local `.m2/` directory

## Layout Highlights
- `src/f2/transport.clj` – WebSocket/HTTP bus, async back-pressure, optional REPL wiring
- `src/f2/repl.clj` – SAFE (SCI sandbox) + ADMIN (token-gated) evaluators
- `src/f2/ui.clj` – JSON façade (`/musn/clients`, `/musn/sessions`, `/musn/practices`, `/musn/export`)
- `src/f2/musn.clj` – system lifecycle + demo ingest hooks
- `src/f2/semantics.clj` – placeholder reasoning over transport history
- `dev/demo_events.ndjson` – sample ingest for `make demo`

## Quick Start
1. Install deps (first run populates `.m2/`):
   ```bash
   make test
   ```
2. Boot the stack:
   ```bash
   make dev
   ```
   - Transport (WS+HTTP) → `http://localhost:5050`
   - UI endpoints → `http://localhost:6060`
3. (Optional) Load sample events:
   ```bash
   make demo
   # streams dev/demo_events.ndjson via the ingest path
   ```

## WebSocket Walkthrough
```
wscat -c ws://localhost:5050/musn/ws
> {"type":"hello","client":"alice","caps":["eval","message"]}
< {"type":"hello","ok":true,"client":"C-5a2b7c1d"}
> {"type":"eval","payload":{"code":"(+ 1 2)"}}
< {"ok":false,"type":"eval","err":"repl-disabled"}
```

### Enabling the REPL
The REPL is **OFF** by default. Start the system with a config override to expose SAFE mode (SCI sandbox, allowlisted namespaces, 64 KB code / 1 MB result caps, 100 ms CPU budget):

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
- `POST /musn/export` – writes `dev/scenario.edn` snapshot to disk

## Running Tests
```bash
make test
```
Runs Datascript fixture tests, semantics checks, and transport/REPL dispatch tests.

## Next Steps
- Swap the Datascript placeholder with the upcoming external graph/persistence package
- Expand the `musn.api` DSL surfaced to SAFE evals (graph queries, semantic helpers, etc.)
- Layer auth/tooling on top of the `eval` frame (CLI helpers, per-token quotas, observability)
