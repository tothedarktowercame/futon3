;;; fuclient-claude-stream.el --- Claude Code JSONL live streaming -*- lexical-binding: t; -*-

;;; Commentary:

;; Stream Claude Code session transcripts in real-time via WebSocket.
;;
;; This package connects to futon3's lab-ws Java-WebSocket server on port 5056
;; which streams the full Claude Code JSONL history and pushes new events
;; as they are written.
;;
;; Usage:
;;   M-x fuclient-claude-stream-connect
;;   - Prompts for JSONL path (defaults to current session)
;;   - Opens *Claude Stream* buffer with live updates
;;
;; Keybindings in *Claude Stream* buffer:
;;   g   - Reconnect
;;   c   - Clear buffer
;;   q   - Disconnect and quit
;;   RET - On a file path, open that file
;;
;; Configuration:
;;   (setq fuclient-claude-stream-server "ws://localhost:5056")
;;   (setq fuclient-claude-stream-default-path "~/.claude/projects/.../session.jsonl")
;;
;; For Codex/remote use, set the server to your futon3 host:
;;   (setq fuclient-claude-stream-server "ws://your-server:5056")

;;; Code:

(require 'websocket)
(require 'json)
(require 'url-util)

(defgroup fuclient-claude-stream nil
  "Claude Code JSONL live streaming."
  :group 'communication)

(defcustom fuclient-claude-stream-server "ws://localhost:5056"
  "WebSocket server URL for lab-ws (Java-WebSocket server)."
  :type 'string
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-default-path nil
  "Default JSONL path to stream. If nil, prompts user."
  :type '(choice (const nil) string)
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-tail-lines 50
  "Number of recent lines to fetch on connect."
  :type 'integer
  :group 'fuclient-claude-stream)

(defface fuclient-claude-stream-user-face
  '((t :foreground "#88c0d0" :weight bold))
  "Face for user messages."
  :group 'fuclient-claude-stream)

(defface fuclient-claude-stream-assistant-face
  '((t :foreground "#a3be8c"))
  "Face for assistant messages."
  :group 'fuclient-claude-stream)

(defface fuclient-claude-stream-tool-face
  '((t :foreground "#b48ead"))
  "Face for tool use/result."
  :group 'fuclient-claude-stream)

(defface fuclient-claude-stream-timestamp-face
  '((t :foreground "#4c566a"))
  "Face for timestamps."
  :group 'fuclient-claude-stream)

(defface fuclient-claude-stream-par-face
  '((t :foreground "#ebcb8b" :weight bold))
  "Face for PAR (Post-Action Review) events."
  :group 'fuclient-claude-stream)

(defvar fuclient-claude-stream--websocket nil
  "Current WebSocket connection.")

(defvar fuclient-claude-stream--current-path nil
  "Current JSONL path being streamed.")

(defvar fuclient-claude-stream--buffer-name "*Claude Stream*"
  "Buffer name for stream output.")

(defvar fuclient-claude-stream--ping-timer nil
  "Timer for keepalive pings.")

(defvar fuclient-claude-stream--reconnect-timer nil
  "Timer for auto-reconnect.")

(defvar fuclient-claude-stream--connected nil
  "Non-nil if we received data (confirms connection worked).")

(defcustom fuclient-claude-stream-auto-reconnect t
  "Whether to automatically reconnect on disconnect."
  :type 'boolean
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-reconnect-delay 3
  "Seconds to wait before attempting reconnect."
  :type 'integer
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-futon1-api-base "http://localhost:8080"
  "Futon1 API base URL for fetching cached lab sessions."
  :type 'string
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-use-futon1-cache t
  "Whether to check Futon1 for cached sessions before streaming.
When enabled, will fetch completed sessions from Futon1 XTDB instead
of streaming the full JSONL file, dramatically reducing memory usage
for large sessions."
  :type 'boolean
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-cache-threshold-kb 1024
  "File size threshold (in KB) above which to prefer Futon1 cache.
Sessions larger than this will check Futon1 first.
Set to 0 to always check Futon1."
  :type 'integer
  :group 'fuclient-claude-stream)

(defun fuclient-claude-stream--local-server-p ()
  "Return non-nil if the configured server is local."
  (let ((server (or fuclient-claude-stream-server "")))
    (or (string-match-p "\\`ws://localhost\\b" server)
        (string-match-p "\\`ws://127\\.0\\.0\\.1\\b" server)
        (string-match-p "\\`wss://localhost\\b" server)
        (string-match-p "\\`wss://127\\.0\\.0\\.1\\b" server))))

(defun fuclient-claude-stream--extract-session-id (jsonl-path)
  "Extract session ID (UUID) from JSONL-PATH.
Paths look like: ~/.claude/projects/-home-joe/64570417-4354-40b8-b6a5-db804f69a1d0.jsonl"
  (when (string-match "\\([0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\)" jsonl-path)
    (match-string 1 jsonl-path)))

(defun fuclient-claude-stream--file-size-kb (path)
  "Return file size of PATH in KB, or nil if file doesn't exist."
  (when (file-exists-p path)
    (/ (file-attribute-size (file-attributes path)) 1024)))

(defun fuclient-claude-stream--fetch-from-futon1 (session-id callback)
  "Fetch session SESSION-ID from Futon1 asynchronously.
Calls CALLBACK with (ok doc event-count) where ok is t on success."
  (let ((url (format "%s/api/alpha/lab/session/%s"
                     fuclient-claude-stream-futon1-api-base
                     (url-hexify-string session-id))))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (funcall callback nil nil 0)
         (goto-char (point-min))
         (when (re-search-forward "\n\n" nil t)
           (let* ((json-object-type 'alist)
                  (json-array-type 'list)
                  (data (condition-case nil
                            (json-read)
                          (error nil))))
             (if (and data (cdr (assq 'ok data)))
                 (let* ((doc (cdr (assq 'doc data)))
                        (events (cdr (assq 'events doc)))
                        (event-count (or (cdr (assq 'event-count doc))
                                         (length events))))
                   (funcall callback t doc event-count))
               (funcall callback nil nil 0))))))
     nil t t)))

(defun fuclient-claude-stream--get-buffer ()
  "Get or create the stream buffer."
  (let ((buf (get-buffer-create fuclient-claude-stream--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'fuclient-claude-stream-mode)
        (fuclient-claude-stream-mode)))
    buf))

(defun fuclient-claude-stream--format-event (event)
  "Format EVENT for display."
  (let* ((type (or (cdr (assq 'type event)) "unknown"))
         (timestamp (or (cdr (assq 'timestamp event)) ""))
         (text (or (cdr (assq 'text event)) ""))
         (tool-name (cdr (assq 'tool-name event)))
         (ts-str (if (> (length timestamp) 19)
                     (substring timestamp 11 19)
                   timestamp)))
    (concat
     ;; Timestamp
     (propertize (format "[%s] " ts-str)
                 'face 'fuclient-claude-stream-timestamp-face)
     ;; Type indicator and content
     (pcase type
       ("user"
        (concat (propertize "USER: " 'face 'fuclient-claude-stream-user-face)
                (fuclient-claude-stream--truncate text 500)))
       ("assistant"
        (concat (propertize "AGENT: " 'face 'fuclient-claude-stream-assistant-face)
                (fuclient-claude-stream--truncate text 500)))
       ("tool_use"
        (propertize (format "TOOL: %s" (or tool-name "?"))
                    'face 'fuclient-claude-stream-tool-face))
       ("tool_result"
        (propertize "RESULT: ..."
                    'face 'fuclient-claude-stream-tool-face))
       ("summary"
        (propertize "[CONTEXT COMPACTED]"
                    'face 'font-lock-comment-face))
       ("par"
        (concat (propertize "PAR: " 'face 'fuclient-claude-stream-par-face)
                "\n" text))
       (_
        (format "[%s]" type)))
     "\n")))

(defun fuclient-claude-stream--truncate (text max-len)
  "Truncate TEXT to MAX-LEN characters."
  (if (> (length text) max-len)
      (concat (substring text 0 (- max-len 3)) "...")
    text))

(defun fuclient-claude-stream--append (text)
  "Append TEXT to the stream buffer."
  (let ((buf (fuclient-claude-stream--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when at-end
          (goto-char (point-max)))))))

(defun fuclient-claude-stream--on-message (_ws frame)
  "Handle incoming WebSocket FRAME.
Defensively handles malformed frames during SSL handshake."
  (condition-case err
      (when (and frame (websocket-frame-p frame))
        (setq fuclient-claude-stream--connected t)  ; Mark as successfully connected
        (let* ((payload (websocket-frame-text frame))
               (json-object-type 'alist)
               (json-array-type 'list)
               (data (condition-case nil
                         (json-read-from-string payload)
                       (error nil))))
          (when data
            (let ((msg-type (cdr (assq 'type data))))
              (pcase msg-type
                ("init"
                 (let ((events (cdr (assq 'events data)))
                       (line-count (cdr (assq 'line-count data)))
                       (par-count (cdr (assq 'par-count data))))
                   (fuclient-claude-stream--append
                    (format "--- Connected: %d lines, %d events, %d PARs ---\n\n"
                            (or line-count 0) (length events) (or par-count 0)))
                   (dolist (event events)
                     (fuclient-claude-stream--append
                      (fuclient-claude-stream--format-event event)))))
                ("delta"
                 ;; Incremental update from offset
                 (let ((events (cdr (assq 'events data)))
                       (offset (cdr (assq 'offset data)))
                       (new-par-count (cdr (assq 'new-par-count data)))
                       (line-count (cdr (assq 'line-count data))))
                   (fuclient-claude-stream--append
                    (format "--- Delta from line %d: %d new events%s ---\n\n"
                            (or offset 0)
                            (length events)
                            (if (and new-par-count (> new-par-count 0))
                                (format ", %d new PARs" new-par-count)
                              "")))
                   (dolist (event events)
                     (fuclient-claude-stream--append
                      (fuclient-claude-stream--format-event event)))))
                ("event"
                 (let ((event (cdr (assq 'event data))))
                   (fuclient-claude-stream--append
                    (fuclient-claude-stream--format-event event))))
                ("pong"
                 nil) ;; Keepalive response, ignore
                ("error"
                 (fuclient-claude-stream--append
                  (format "ERROR: %s\n" (cdr (assq 'err data))))))))))
    (error
     ;; Ignore errors during SSL handshake quirks
     (message "[claude-stream] on-message error (ignoring): %s" err))))

(defun fuclient-claude-stream--on-close (_ws)
  "Handle WebSocket close.
Ignore spurious close events during SSL handshake (when we haven't
received any data yet)."
  (message "[claude-stream] on-close called, connected=%s" fuclient-claude-stream--connected)
  ;; Only act on close if we actually received data (not spurious SSL handshake close)
  (if fuclient-claude-stream--connected
      (progn
        (fuclient-claude-stream--append "\n--- Disconnected ---\n")
        (when fuclient-claude-stream--ping-timer
          (cancel-timer fuclient-claude-stream--ping-timer)
          (setq fuclient-claude-stream--ping-timer nil))
        (setq fuclient-claude-stream--websocket nil)
        (setq fuclient-claude-stream--connected nil)
        ;; Auto-reconnect if enabled and we have a path
        (when (and fuclient-claude-stream-auto-reconnect
                   fuclient-claude-stream--current-path)
          (fuclient-claude-stream--schedule-reconnect)))
    ;; Spurious close during handshake - ignore
    (message "[claude-stream] Ignoring spurious close during handshake")))

(defun fuclient-claude-stream--on-error (_ws _type err)
  "Handle WebSocket error ERR.
Ignore spurious 400 errors that occur during SSL handshake but don't
prevent the connection from working."
  (message "[claude-stream] on-error: type=%s err=%s" _type err)
  (let ((err-str (format "%s" err)))
    (unless (string-match-p "400" err-str)
      (fuclient-claude-stream--append
       (format "\n--- Error: %s ---\n" err)))))

(defun fuclient-claude-stream--format-futon1-event (event)
  "Format Futon1 EVENT for display.
Futon1 events have :event/type instead of type."
  (let* ((type (or (cdr (assq 'event/type event))
                   (cdr (assq 'type event))
                   "unknown"))
         (timestamp (or (cdr (assq 'at event))
                        (cdr (assq 'timestamp event))
                        ""))
         (text (or (cdr (assq 'text event))
                   (cdr (assq 'content event))
                   ""))
         (tool-name (or (cdr (assq 'tool/name event))
                        (cdr (assq 'tool-name event))))
         (ts-str (cond
                  ((stringp timestamp)
                   (if (> (length timestamp) 19)
                       (substring timestamp 11 19)
                     timestamp))
                  (t ""))))
    (concat
     ;; Timestamp
     (propertize (format "[%s] " ts-str)
                 'face 'fuclient-claude-stream-timestamp-face)
     ;; Type indicator and content
     (pcase (if (keywordp type) (symbol-name type) (format "%s" type))
       ((or ":turn/user" "turn/user" "user")
        (concat (propertize "USER: " 'face 'fuclient-claude-stream-user-face)
                (fuclient-claude-stream--truncate text 500)))
       ((or ":turn/agent" "turn/agent" "assistant")
        (concat (propertize "AGENT: " 'face 'fuclient-claude-stream-assistant-face)
                (fuclient-claude-stream--truncate text 500)))
       ((or ":tool/use" "tool/use" "tool_use")
        (propertize (format "TOOL: %s" (or tool-name "?"))
                    'face 'fuclient-claude-stream-tool-face))
       ((or ":tool/result" "tool/result" "tool_result")
        (propertize "RESULT: ..."
                    'face 'fuclient-claude-stream-tool-face))
       ((or ":context/compacted" "context/compacted" "summary")
        (propertize "[CONTEXT COMPACTED]"
                    'face 'font-lock-comment-face))
       ((or ":session/par" "session/par" "par")
        (concat (propertize "PAR: " 'face 'fuclient-claude-stream-par-face)
                "\n" text))
       (_
        (format "[%s]" type)))
     "\n")))

(defvar fuclient-claude-stream--cached-par-count 0
  "Number of PARs in the last loaded Futon1 cache.
Used to detect new PARs when streaming delta.")

(defun fuclient-claude-stream--render-futon1-session (doc)
  "Render Futon1 session DOC to the stream buffer."
  (let* ((events (cdr (assq 'events doc)))
         (session-id (or (cdr (assq 'lab/session-id doc))
                         (cdr (assq 'session/id doc))
                         "?"))
         (event-count (or (cdr (assq 'event-count doc))
                          (length events)))
         (par-count (or (cdr (assq 'par-count doc)) 0))
         (trigger (or (cdr (assq 'lab/trigger doc)) "unknown")))
    ;; Track PAR count for delta streaming
    (setq fuclient-claude-stream--cached-par-count par-count)
    (fuclient-claude-stream--append
     (format "--- Loaded from Futon1 cache (trigger: %s) ---\n" trigger))
    (fuclient-claude-stream--append
     (format "--- Session: %s | %d events, %d PARs ---\n\n"
             session-id event-count par-count))
    (dolist (event events)
      (fuclient-claude-stream--append
       (fuclient-claude-stream--format-futon1-event event)))
    (fuclient-claude-stream--append "\n--- End of cached session ---\n")))

(defun fuclient-claude-stream--should-use-cache-p (path)
  "Return non-nil if we should try Futon1 cache for PATH."
  (and fuclient-claude-stream-use-futon1-cache
       (let ((size-kb (fuclient-claude-stream--file-size-kb path)))
         (or (null size-kb) ;; remote file, can't check size
             (>= size-kb fuclient-claude-stream-cache-threshold-kb)))))

(defun fuclient-claude-stream--schedule-reconnect ()
  "Schedule a reconnection attempt."
  (when fuclient-claude-stream--reconnect-timer
    (cancel-timer fuclient-claude-stream--reconnect-timer))
  (fuclient-claude-stream--append
   (format "--- Reconnecting in %d seconds... ---\n"
           fuclient-claude-stream-reconnect-delay))
  (setq fuclient-claude-stream--reconnect-timer
        (run-with-timer
         fuclient-claude-stream-reconnect-delay nil
         (lambda ()
           (setq fuclient-claude-stream--reconnect-timer nil)
           (when fuclient-claude-stream--current-path
             (condition-case err
                 (fuclient-claude-stream-connect
                  fuclient-claude-stream--current-path)
               (error
                (fuclient-claude-stream--append
                 (format "--- Reconnect failed: %s ---\n" err))
                ;; Try again
                (fuclient-claude-stream--schedule-reconnect))))))))

(defun fuclient-claude-stream--start-ping-timer ()
  "Start keepalive ping timer."
  (when fuclient-claude-stream--ping-timer
    (cancel-timer fuclient-claude-stream--ping-timer))
  (setq fuclient-claude-stream--ping-timer
        (run-with-timer 30 30
                        (lambda ()
                          (when (and fuclient-claude-stream--websocket
                                     (websocket-openp fuclient-claude-stream--websocket))
                            (websocket-send-text fuclient-claude-stream--websocket
                                                 "{\"type\":\"ping\"}"))))))

(defun fuclient-claude-stream--connect-websocket (path &optional offset par-offset)
  "Connect to WebSocket stream for PATH.
Optionally start from OFFSET lines and PAR-OFFSET PARs (for delta streaming)."
  (let* ((encoded-path (url-hexify-string path))
         (url (cond
               ((and offset par-offset)
                (format "%s?path=%s&offset=%d&par_offset=%d"
                        fuclient-claude-stream-server
                        encoded-path
                        offset
                        par-offset))
               (offset
                (format "%s?path=%s&offset=%d"
                        fuclient-claude-stream-server
                        encoded-path
                        offset))
               (t
                (format "%s?path=%s"
                        fuclient-claude-stream-server
                        encoded-path)))))
    (message "[claude-stream] Connecting to: %s" url)
    (setq fuclient-claude-stream--current-path path)
    (setq fuclient-claude-stream--connected nil)
    (setq fuclient-claude-stream--websocket
          (websocket-open url
                          :on-open (lambda (_ws) (message "[claude-stream] WebSocket connected"))
                          :on-message #'fuclient-claude-stream--on-message
                          :on-close #'fuclient-claude-stream--on-close
                          :on-error #'fuclient-claude-stream--on-error))
    (fuclient-claude-stream--start-ping-timer)))

;;;###autoload
(defun fuclient-claude-stream-connect (&optional jsonl-path)
  "Connect to Claude Code JSONL stream at JSONL-PATH.
If `fuclient-claude-stream-use-futon1-cache' is enabled and the file
exceeds `fuclient-claude-stream-cache-threshold-kb', will first check
Futon1 for a cached version to avoid streaming large files repeatedly."
  (interactive)
  (let ((path (or jsonl-path
                  fuclient-claude-stream-default-path
                  (read-file-name "Claude JSONL file: "
                                  "~/.claude/projects/"))))
    ;; Expand and validate path
    (setq path (expand-file-name path))
    (unless (or (file-exists-p path)
                (not (fuclient-claude-stream--local-server-p)))
      (user-error "File not found: %s" path))

    ;; Disconnect existing
    (fuclient-claude-stream-disconnect)

    ;; Clear buffer and show connecting message
    (with-current-buffer (fuclient-claude-stream--get-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Connecting to %s...\n" path))))

    ;; Show buffer
    (pop-to-buffer (fuclient-claude-stream--get-buffer))

    ;; Smart streaming: check Futon1 first for large files
    (let ((session-id (fuclient-claude-stream--extract-session-id path)))
      (if (and session-id (fuclient-claude-stream--should-use-cache-p path))
          ;; Try Futon1 cache first
          (progn
            (fuclient-claude-stream--append
             (format "Checking Futon1 cache for session %s...\n" session-id))
            (fuclient-claude-stream--fetch-from-futon1
             session-id
             (lambda (ok doc event-count)
               (if ok
                   ;; Found in cache! Render from Futon1
                   (progn
                     (fuclient-claude-stream--render-futon1-session doc)
                     ;; Check if we need to stream delta (new JSONL lines or new PARs)
                     (let ((file-lines (fuclient-claude-stream--count-jsonl-lines path))
                           (cached-pars fuclient-claude-stream--cached-par-count))
                       (when (and file-lines (> file-lines event-count))
                         (fuclient-claude-stream--append
                          (format "\n--- Streaming delta (from line %d, par %d)... ---\n"
                                  event-count cached-pars))
                         ;; Connect with offset and par_offset to get only new events
                         (fuclient-claude-stream--connect-websocket
                          path event-count cached-pars))))
                 ;; Not in cache, fall back to full streaming
                 (fuclient-claude-stream--append
                  "Not found in Futon1 cache, streaming full file...\n\n")
                 (fuclient-claude-stream--connect-websocket path)))))
        ;; Small file or no session-id, just stream directly
        (fuclient-claude-stream--connect-websocket path)))))

(defun fuclient-claude-stream--count-jsonl-lines (path)
  "Count lines in JSONL file at PATH efficiently. Returns nil for remote files."
  (when (and (file-exists-p path)
             (fuclient-claude-stream--local-server-p))
    ;; Use wc -l for efficiency - avoids loading 90MB into Emacs
    (let ((result (shell-command-to-string
                   (format "wc -l < %s" (shell-quote-argument path)))))
      (when (string-match "^\\([0-9]+\\)" result)
        (string-to-number (match-string 1 result))))))

;;;###autoload
(defun fuclient-claude-stream-disconnect ()
  "Disconnect from the stream."
  (interactive)
  ;; Cancel reconnect timer to prevent auto-reconnect after manual disconnect
  (when fuclient-claude-stream--reconnect-timer
    (cancel-timer fuclient-claude-stream--reconnect-timer)
    (setq fuclient-claude-stream--reconnect-timer nil))
  (when fuclient-claude-stream--ping-timer
    (cancel-timer fuclient-claude-stream--ping-timer)
    (setq fuclient-claude-stream--ping-timer nil))
  ;; Temporarily disable auto-reconnect for this close
  (let ((fuclient-claude-stream-auto-reconnect nil))
    (when (and fuclient-claude-stream--websocket
               (websocket-openp fuclient-claude-stream--websocket))
      (websocket-close fuclient-claude-stream--websocket)))
  (setq fuclient-claude-stream--websocket nil)
  (setq fuclient-claude-stream--connected nil))

(defun fuclient-claude-stream-reconnect ()
  "Reconnect to the current stream."
  (interactive)
  (if fuclient-claude-stream--current-path
      (fuclient-claude-stream-connect fuclient-claude-stream--current-path)
    (call-interactively #'fuclient-claude-stream-connect)))

(defun fuclient-claude-stream-clear ()
  "Clear the stream buffer."
  (interactive)
  (with-current-buffer (fuclient-claude-stream--get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "--- Buffer cleared ---\n\n"))))

(defvar fuclient-claude-stream-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'fuclient-claude-stream-reconnect)
    (define-key map (kbd "c") #'fuclient-claude-stream-clear)
    (define-key map (kbd "q") #'fuclient-claude-stream-disconnect)
    map)
  "Keymap for `fuclient-claude-stream-mode'.")

(define-derived-mode fuclient-claude-stream-mode special-mode "Claude-Stream"
  "Major mode for viewing Claude Code JSONL stream."
  (setq-local truncate-lines t)
  (setq buffer-read-only t))

;;;###autoload
(defun fuclient-claude-stream-check-cache (&optional jsonl-path)
  "Check Futon1 cache status for JSONL-PATH.
Shows whether the session is cached and how many events are stored."
  (interactive)
  (let ((path (or jsonl-path
                  fuclient-claude-stream--current-path
                  (read-file-name "Claude JSONL file: "
                                  "~/.claude/projects/"))))
    (setq path (expand-file-name path))
    (let ((session-id (fuclient-claude-stream--extract-session-id path))
          (file-size-kb (fuclient-claude-stream--file-size-kb path))
          (file-lines (fuclient-claude-stream--count-jsonl-lines path)))
      (if (not session-id)
          (message "Could not extract session ID from path: %s" path)
        (message "Checking Futon1 cache for session %s..." session-id)
        (fuclient-claude-stream--fetch-from-futon1
         session-id
         (lambda (ok doc event-count)
           (if ok
               (message "Session %s: CACHED (%d events). File: %s KB, %s lines. Delta: %s"
                        session-id
                        event-count
                        (or file-size-kb "?")
                        (or file-lines "?")
                        (if (and file-lines (> file-lines event-count))
                            (format "%d new events" (- file-lines event-count))
                          "none"))
             (message "Session %s: NOT CACHED. File: %s KB, %s lines"
                      session-id
                      (or file-size-kb "?")
                      (or file-lines "?")))))))))

;;;###autoload
(defun fuclient-claude-stream-clear-path ()
  "Clear the current session path to prevent auto-reconnect."
  (interactive)
  (fuclient-claude-stream-disconnect)
  (setq fuclient-claude-stream--current-path nil)
  (message "Session path cleared. Auto-reconnect disabled."))

(provide 'fuclient-claude-stream)

;;; fuclient-claude-stream.el ends here
