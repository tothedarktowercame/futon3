# Bug Report: websocket.el fails with 400 error on Emacs 31 but works on Emacs 29

## Summary

WebSocket connections via `websocket.el` through nginx SSL termination fail with HTTP 400 "Bad Request" on Emacs 31, but work correctly on Emacs 29.3. The same endpoint works with curl, Java HTTP client, and raw TLS connections from Emacs 31.

## Environment

**Failing:**
- Emacs 31.0.50 (built from master, late January 2026)
- websocket.el 1.12
- Ubuntu 24.04, GnuTLS 3.8.3

**Working:**
- Emacs 29.3
- Same websocket.el 1.12
- Same Ubuntu 24.04, GnuTLS 3.8.3

## Steps to Reproduce

1. Set up a WebSocket server behind nginx SSL termination (or use any WSS endpoint)

2. Test with Emacs 31:
```elisp
(require 'websocket)
(setq websocket-debug t)

(websocket-open "wss://your-server:port/?param=value"
                :on-open (lambda (ws) (message "OPEN"))
                :on-message (lambda (ws frame)
                              (message "MSG: %s" (websocket-frame-text frame)))
                :on-close (lambda (ws) (message "CLOSE"))
                :on-error (lambda (ws type err) (message "ERR: %s %S" type err)))
```

3. Check `*websocket*` buffer - shows:
```
[WS] Sending handshake, key: xxx
[WS] Received: HTTP/1.1 400 Bad Request
Server: nginx/1.24.0 (Ubuntu)
...
```

4. Same code on Emacs 29.3: works correctly, receives 101 Switching Protocols

## Diagnostic Results

All tests from the **same machine** to the **same endpoint**:

| Client | Result |
|--------|--------|
| curl with WebSocket headers | ✓ 101 Switching Protocols |
| Java HTTP client (babashka) | ✓ Connected, receives data |
| Emacs 31 raw TLS + manual HTTP | ✓ 404 (request accepted, path invalid) |
| Emacs 31 websocket.el | ✗ 400 Bad Request |
| Emacs 29.3 websocket.el | ✓ Works |

### curl test (works):
```bash
curl -v -H "Connection: Upgrade" -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
  "https://server:5057/?path=/test"
# Returns: HTTP/1.1 101 Web Socket Protocol Handshake
```

### Java/babashka test (works):
```clojure
(require '[babashka.http-client.websocket :as ws])
(ws/websocket {:uri "wss://server:5057/?path=/test"
               :on-open (fn [ws] (println "Connected!"))
               :on-message (fn [ws msg last?] (println "Got:" msg))})
;; Successfully connects and receives messages
```

### Raw TLS from Emacs 31 (works):
```elisp
(let ((proc (open-network-stream "test" "*test*" "server" 5057 :type 'tls)))
  (process-send-string proc
    "GET /?path=/test HTTP/1.1\r\nHost: server:5057\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: xxx\r\nSec-WebSocket-Version: 13\r\n\r\n"))
;; Returns 404 (path not found) - but request format is ACCEPTED
```

## Analysis

The issue is specific to how `websocket.el` constructs and sends the HTTP upgrade request on Emacs 31. Since:
- Raw TLS connections work (Emacs network layer is fine)
- The same websocket.el code works on Emacs 29
- Other clients work from the same machine

Something changed in Emacs 31 that affects how websocket.el's request is transmitted or formatted.

### Investigated but ruled out:
- GnuTLS version (same 3.8.3 on both)
- `gnutls-algorithm-priority` settings
- `process-adaptive-read-buffering`
- `network-security-level`

### Commits checked (no obvious culprit):
- `src/gnutls.c` - only housekeeping changes since Emacs 29
- `lisp/net/network-stream.el` - minor changes
- `src/process.c` - many changes but none obviously related

## Workaround

Use a subprocess bridge - babashka connects via WSS and outputs to stdout, Emacs reads from the process:

```elisp
(start-process "wss-bridge" buffer "bb" "wss-bridge.clj" wss-url)
```

## Request

Could someone with Emacs internals knowledge investigate what changed between 29.3 and 31 that would cause nginx to reject the WebSocket upgrade request with 400 Bad Request?

The fact that raw `open-network-stream` with `:type 'tls` works suggests the issue is in how websocket.el's `process-send-string` output is handled or buffered in Emacs 31.
