# Drawbridge live REPL checklist (Futon3)

Goal: prove that a running Futon3 instance can be safely inspected live via
Drawbridge (HTTP REPL), using a minimal read-only query script.

## Preconditions
- Futon3 server can start locally.
- You have a token for the Drawbridge admin gate (env var or .admintoken).
- You are connecting from localhost only.

## Enable Drawbridge
1. Start Futon3 with Drawbridge enabled (local-only):
   ```clojure
   (require '[f2.musn :as musn])
   (musn/stop!)
   (def system
     (musn/start! {:drawbridge {:enabled? true
                                :bind "127.0.0.1"
                                :port 6767
                                :allow ["127.0.0.1" "::1"]
                                :token (System/getenv "ADMIN_TOKEN")}}))
   ```
2. Verify the server stays up and the Drawbridge endpoint is listening.

Shortcut: `make repl` (uses `ADMIN_TOKEN` or `.admintoken`) starts Drawbridge
directly:
```bash
cd futon3
make repl
```

If you start Drawbridge first, you can then boot MUSN from the REPL:
```clojure
(require '[f2.musn :as musn])
(def system (musn/start!))
```

## Connect via REPL client
1. In Emacs, run `M-x cider-connect-clj-http`.
2. URL: `http://localhost:6767/repl`
3. Headers: `X-Admin-Token: <token>`

## Minimal safe query script
Evaluate the following in the REPL:
```clojure
(require '[musn.api :as api])

{:clients (api/clients)
 :history (api/history)
 :links   (api/links)
 :now     (api/now)}
```

Expected: evaluation succeeds without errors and returns maps/vectors.

## Live change (Codex)
Goal: perform one reversible, non-destructive change on the running server.

Options (pick one):
- REPL-local marker (no persistence): define and update a session marker.
  ```clojure
  (def drawbridge-session-marker (atom {:at (java.time.Instant/now)
                                        :note "codex-live-change"}))
  (swap! drawbridge-session-marker assoc :status :ok)
  ```
- If you have a known safe in-server action, run it here and document it
  (for example, a no-op event or a single test ingest frame), then immediately
  revert or clean up.

Record the change and outcome in the transcript.

## Record evidence
- Save a short transcript (time, server config, and the REPL output) to a file:
  `futon3/logs/drawbridge-session-YYYY-MM-DD.md`.

## Exit
- Disconnect the REPL client.
- Disable Drawbridge if not actively in use.

## Notes
- Keep Drawbridge bound to localhost; do not expose it to the network.
- If the token is missing or incorrect, the REPL should refuse to connect.
