# M-WS: WebSocket Log Stream to Emacs

**Type**: Feature (Future)
**Depends**: SSE notebook viewer (current), MUSN session events
**Related**: `src/f2/transport.clj` (SSE endpoint), `src-cljs/futon3/notebook/core.cljs`

---

## Objective

Stream MUSN session events to Emacs via WebSocket, enabling:
1. Real-time log tailing in a dedicated buffer
2. Structured span navigation (jump to related events)
3. Filtering by level, source, run_id

This extends the SSE-based notebook viewer to support bidirectional Emacs integration.

---

## Architecture

### Server Side

Expose `ws://127.0.0.1:5050/fulab/logs` (or per-session `/fulab/session/:id/ws`).

Emit structured JSON events:

```json
{
  "ts": "2026-01-26T19:12:33.123Z",
  "level": "INFO",
  "src": "fuclaude",
  "msg": "started drawbridge hydrate",
  "run_id": "musn-abc123",
  "span_id": "turn-7",
  "parent_span_id": "session-init",
  "tool": "drawbridge",
  "path": "/home/joe/code/futon3/src/f2/transport.clj",
  "ok": true
}
```

Key fields for span tracking:
- `run_id` - one wrapper session (maps to MUSN session-id)
- `span_id` / `parent_span_id` - tree of actions (maps to turn events)
- `tool` - e.g., `drawbridge`, `git`, `build`, `read`, `edit`
- `path` - file touched (enables jump-to-file)
- `ok` / `err` - outcome

### Emacs Side

Minor mode `fuclient-logs-mode` with:
- `M-x fuclient-logs-connect` - connect to WebSocket
- `M-x fuclient-logs-disconnect` - disconnect
- `M-x fuclient-logs-clear` - clear buffer
- `M-x fuclient-logs-filter-level` - filter by level
- `M-x fuclient-logs-narrow-run` - narrow to run_id
- `RET` on path - jump to file:line

Buffer: `*FuLogs*`

---

## Emacs Client Skeleton

```elisp
;; Requires: websocket (GNU ELPA / MELPA)
(require 'websocket)
(require 'json)

(defgroup fuclient-logs nil
  "Streaming logs into Emacs from futon3."
  :group 'tools)

(defcustom fuclient-logs-url "ws://127.0.0.1:5050/fulab/logs"
  "WebSocket URL that streams log events."
  :type 'string
  :group 'fuclient-logs)

(defcustom fuclient-logs-buffer-name "*FuLogs*"
  "Buffer name for streamed logs."
  :type 'string
  :group 'fuclient-logs)

(defvar fuclient-logs--ws nil)

(defun fuclient-logs--buffer ()
  (get-buffer-create fuclient-logs-buffer-name))

(defun fuclient-logs--insert (s)
  (with-current-buffer (fuclient-logs--buffer)
    (let ((inhibit-read-only t)
          (at-end (= (point) (point-max))))
      (goto-char (point-max))
      (insert s)
      (when (and at-end (get-buffer-window (current-buffer)))
        (goto-char (point-max))))))

(defun fuclient-logs--format-message (payload)
  "Format PAYLOAD (string). If JSON, make a readable line."
  (condition-case _
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (obj (json-read-from-string payload))
             (ts (or (alist-get 'ts obj) ""))
             (lvl (or (alist-get 'level obj) ""))
             (msg (or (alist-get 'msg obj) payload))
             (src (or (alist-get 'src obj) "")))
        (format "%s %-5s %-12s %s\n" ts lvl src msg))
    (error
     (if (string-suffix-p "\n" payload) payload (concat payload "\n")))))

(defun fuclient-logs-connect (&optional url)
  "Connect to the log WebSocket and stream into buffer."
  (interactive)
  (when fuclient-logs--ws
    (ignore-errors (websocket-close fuclient-logs--ws)))
  (let ((u (or url fuclient-logs-url)))
    (setq fuclient-logs--ws
          (websocket-open
           u
           :on-open (lambda (_ws)
                      (fuclient-logs--insert (format ";; connected: %s\n" u)))
           :on-message (lambda (_ws frame)
                         (fuclient-logs--insert
                          (fuclient-logs--format-message
                           (websocket-frame-text frame))))
           :on-close (lambda (_ws)
                       (fuclient-logs--insert ";; disconnected\n"))
           :on-error (lambda (_ws type err)
                       (fuclient-logs--insert
                        (format ";; error (%S): %S\n" type err))))))
  (pop-to-buffer (fuclient-logs--buffer)))

(defun fuclient-logs-disconnect ()
  "Disconnect the log WebSocket."
  (interactive)
  (when fuclient-logs--ws
    (ignore-errors (websocket-close fuclient-logs--ws))
    (setq fuclient-logs--ws nil))
  (fuclient-logs--insert ";; manually disconnected\n"))

(defun fuclient-logs-clear ()
  "Clear the log buffer."
  (interactive)
  (with-current-buffer (fuclient-logs--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))
```

---

## Implementation Steps

1. **Server: Add WebSocket endpoint** (`/fulab/logs` or `/fulab/session/:id/ws`)
   - Reuse existing `websocket-handler` pattern from `/musn/ws`
   - Subscribe to MUSN session events
   - Emit JSON with span fields

2. **Server: Enrich events with span metadata**
   - Add `span_id` generation to `record-turn!`
   - Track parent spans for nested tool calls
   - Include file paths from tool actions

3. **Emacs: Install websocket package**
   - `M-x package-install websocket`

4. **Emacs: Add fuclient-logs.el**
   - Start with skeleton above
   - Add `fuclient-logs-mode` minor mode
   - Add clickable paths (buttons)

5. **Emacs: Add span navigation**
   - `fuclient-logs-narrow-run` - filter to run_id
   - `fuclient-logs-timeline` - visualize span tree

---

## Success Criteria

- [ ] WebSocket endpoint streams events in real-time
- [ ] Events include span_id and parent_span_id
- [ ] Emacs buffer receives and formats events
- [ ] Clicking path jumps to file
- [ ] Can filter by level or run_id

---

## Notes

- SSE is sufficient for read-only notebook viewing (already implemented)
- WebSocket enables bidirectional features (future: send commands from Emacs)
- Consider multiplexing multiple sessions into one stream with run_id filtering
- The existing `/musn/ws` endpoint could be extended rather than adding new

---

## Origin

Ideas from ChatGPT session (2026-01-26), adapted for futon3/fuclaude architecture.
