# Drawbridge + MUSN quickstart (hotload fixes into running server)

Steps to start both servers:

1) MUSN HTTP (port 6065)

```bash
clojure -M -m futon3.musn.http
# prints: MUSN HTTP server on 6065
```

2) Drawbridge + transport/UI (ports: 6767 drawbridge, 5050 transport, 6060 UI)

```bash
ADMIN_TOKEN=$(cat .admintoken) FUTON3_DRAWBRIDGE=1 ./scripts/dev.sh
# prints: Drawbridge on http://127.0.0.1:6767/repl (allowed: ["127.0.0.1" "::1"])
#         Transport on 5050, UI on 6060
```

Testing drawbridge (from repo root):

```bash
ADMIN_TOKEN=$(cat .admintoken) clojure -M -e "
(require 'cemerick.drawbridge.client)
(require '[clojure.tools.nrepl :as nrepl])
(let [conn   (nrepl/url-connect (str \"http://127.0.0.1:6767/repl?token=\" (System/getenv \"ADMIN_TOKEN\")))
      client (nrepl/client conn 2000)]
  (prn (doall (nrepl/message client {:op \"eval\" :code \"(+ 1 2)\"}))))
"
```

Hotload a source file into the running server:

```bash
ADMIN_TOKEN=$(cat .admintoken) clojure -M -e "
(require 'cemerick.drawbridge.client)
(require '[clojure.tools.nrepl :as nrepl])
(let [conn   (nrepl/url-connect (str \"http://127.0.0.1:6767/repl?token=\" (System/getenv \"ADMIN_TOKEN\")))
      client (nrepl/client conn 2000)]
  (doall (nrepl/message client {:op \"eval\" :code \"(require 'futon3.fulab.hud)\"}))
  (doall (nrepl/message client {:op \"eval\" :code \"(load-file \\\"src/futon3/fulab/hud.clj\\\")\"})))
"
```

If you change multiple namespaces, repeat the `load-file` block per file (e.g., `pattern_hints.clj`).

Notes:
- Drawbridge must allow 127.0.0.1 (it does by default with FUTON3_DRAWBRIDGE=1).
- Keep both terminals running; drawbridge won’t start the MUSN HTTP server.
