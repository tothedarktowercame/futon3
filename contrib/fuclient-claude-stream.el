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

(defcustom fuclient-claude-stream-auto-reconnect t
  "Whether to automatically reconnect on disconnect."
  :type 'boolean
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-reconnect-delay 3
  "Seconds to wait before attempting reconnect."
  :type 'integer
  :group 'fuclient-claude-stream)

(defun fuclient-claude-stream--local-server-p ()
  "Return non-nil if the configured server is local."
  (let ((server (or fuclient-claude-stream-server "")))
    (or (string-match-p "\\`ws://localhost\\b" server)
        (string-match-p "\\`ws://127\\.0\\.0\\.1\\b" server)
        (string-match-p "\\`wss://localhost\\b" server)
        (string-match-p "\\`wss://127\\.0\\.0\\.1\\b" server))))

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
  "Handle incoming WebSocket FRAME."
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
          ("event"
           (let ((event (cdr (assq 'event data))))
             (fuclient-claude-stream--append
              (fuclient-claude-stream--format-event event))))
          ("pong"
           nil) ;; Keepalive response, ignore
          ("error"
           (fuclient-claude-stream--append
            (format "ERROR: %s\n" (cdr (assq 'err data))))))))))

(defun fuclient-claude-stream--on-close (_ws)
  "Handle WebSocket close."
  (message "[claude-stream] on-close called")
  (fuclient-claude-stream--append "\n--- Disconnected ---\n")
  (when fuclient-claude-stream--ping-timer
    (cancel-timer fuclient-claude-stream--ping-timer)
    (setq fuclient-claude-stream--ping-timer nil))
  (setq fuclient-claude-stream--websocket nil)
  ;; Auto-reconnect if enabled and we have a path
  (when (and fuclient-claude-stream-auto-reconnect
             fuclient-claude-stream--current-path)
    (fuclient-claude-stream--schedule-reconnect)))

(defun fuclient-claude-stream--on-error (_ws _type err)
  "Handle WebSocket error ERR.
Ignore spurious 400 errors that occur during SSL handshake but don't
prevent the connection from working."
  (message "[claude-stream] on-error: type=%s err=%s" _type err)
  (let ((err-str (format "%s" err)))
    (unless (string-match-p "400" err-str)
      (fuclient-claude-stream--append
       (format "\n--- Error: %s ---\n" err)))))

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

;;;###autoload
(defun fuclient-claude-stream-connect (&optional jsonl-path)
  "Connect to Claude Code JSONL stream at JSONL-PATH."
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

    ;; Build URL (lab-ws on 5056 uses direct query params, no path prefix)
    (let* ((encoded-path (url-hexify-string path))
           (url (format "%s?path=%s"
                        fuclient-claude-stream-server
                        encoded-path)))
      (message "[claude-stream] Connecting to: %s" url)

      ;; Clear buffer
      (with-current-buffer (fuclient-claude-stream--get-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Connecting to %s...\n" path))))

      ;; Connect
      (setq fuclient-claude-stream--current-path path)
      (setq fuclient-claude-stream--websocket
            (websocket-open url
                            :on-open (lambda (_ws) (message "[claude-stream] WebSocket connected"))
                            :on-message #'fuclient-claude-stream--on-message
                            :on-close #'fuclient-claude-stream--on-close
                            :on-error #'fuclient-claude-stream--on-error))

      ;; Start ping timer
      (fuclient-claude-stream--start-ping-timer)

      ;; Show buffer
      (pop-to-buffer (fuclient-claude-stream--get-buffer)))))

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
  (setq fuclient-claude-stream--websocket nil))

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

(provide 'fuclient-claude-stream)

;;; fuclient-claude-stream.el ends here
