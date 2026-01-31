;;; ws-debug.el --- WebSocket connection debugger -*- lexical-binding: t; -*-

;;; Commentary:
;; Step-by-step WebSocket debugging for SSL connections through nginx.
;; Load this file and run M-x ws-debug-connect to see exactly what happens.

;;; Code:

(require 'websocket)

(defvar ws-debug--log-buffer "*WS Debug*"
  "Buffer for debug output.")

(defvar ws-debug--ws nil
  "Current websocket connection.")

(defvar ws-debug--event-count 0
  "Count of events received.")

(defun ws-debug-log (category fmt &rest args)
  "Log to debug buffer with CATEGORY and formatted message."
  (let ((buf (get-buffer-create ws-debug--log-buffer))
        (timestamp (format-time-string "%H:%M:%S.%3N"))
        (msg (apply #'format fmt args)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "[%s] %-12s %s\n" timestamp category msg)))
    (message "[%s] %s: %s" timestamp category msg)))

(defun ws-debug--inspect (label obj)
  "Log detailed inspection of OBJ with LABEL."
  (ws-debug-log "INSPECT" "%s:" label)
  (ws-debug-log "INSPECT" "  type-of: %s" (type-of obj))
  (ws-debug-log "INSPECT" "  princ: %S" obj)
  (when (and obj (listp obj))
    (ws-debug-log "INSPECT" "  length: %d" (safe-length obj))
    (ws-debug-log "INSPECT" "  car: %S" (car-safe obj)))
  (when (vectorp obj)
    (ws-debug-log "INSPECT" "  length: %d" (length obj)))
  (when (and (fboundp 'websocket-frame-p) obj)
    (ws-debug-log "INSPECT" "  websocket-frame-p: %s" (websocket-frame-p obj))))

(defun ws-debug--on-open (ws)
  "Handle open event."
  (ws-debug-log "ON-OPEN" "Called!")
  (ws-debug--inspect "ws object" ws)
  (ws-debug-log "ON-OPEN" "websocket-openp: %s" (websocket-openp ws)))

(defun ws-debug--on-message (ws frame)
  "Handle message event."
  (setq ws-debug--event-count (1+ ws-debug--event-count))
  (ws-debug-log "ON-MESSAGE" "Event #%d" ws-debug--event-count)
  (ws-debug--inspect "ws" ws)
  (ws-debug--inspect "frame" frame)
  (when (and frame (websocket-frame-p frame))
    (let ((text (websocket-frame-text frame)))
      (ws-debug-log "ON-MESSAGE" "frame-text length: %d" (length text))
      (ws-debug-log "ON-MESSAGE" "frame-text preview: %.200s..." text))))

(defun ws-debug--on-close (ws)
  "Handle close event."
  (ws-debug-log "ON-CLOSE" "Called!")
  (ws-debug--inspect "ws" ws))

(defun ws-debug--on-error (ws type err)
  "Handle error event."
  (ws-debug-log "ON-ERROR" "Called!")
  (ws-debug-log "ON-ERROR" "type: %S" type)
  (ws-debug--inspect "err" err)
  (ws-debug--inspect "ws" ws))

(defun ws-debug-connect (url)
  "Connect to URL with full debugging."
  (interactive "sWebSocket URL: ")
  ;; Reset state
  (setq ws-debug--event-count 0)

  ;; Prepare buffer
  (with-current-buffer (get-buffer-create ws-debug--log-buffer)
    (erase-buffer)
    (insert "=== WebSocket Debug Session ===\n")
    (insert (format "URL: %s\n" url))
    (insert (format "Time: %s\n" (current-time-string)))
    (insert "================================\n\n"))

  (pop-to-buffer ws-debug--log-buffer)

  ;; Close existing
  (when (and ws-debug--ws (websocket-openp ws-debug--ws))
    (ws-debug-log "SETUP" "Closing existing connection")
    (websocket-close ws-debug--ws))

  (ws-debug-log "SETUP" "About to call websocket-open")
  (ws-debug-log "SETUP" "URL: %s" url)

  (condition-case err
      (progn
        (setq ws-debug--ws
              (websocket-open url
                              :on-open #'ws-debug--on-open
                              :on-message #'ws-debug--on-message
                              :on-close #'ws-debug--on-close
                              :on-error #'ws-debug--on-error))
        (ws-debug-log "SETUP" "websocket-open returned")
        (ws-debug--inspect "returned ws" ws-debug--ws))
    (error
     (ws-debug-log "SETUP" "websocket-open threw error!")
     (ws-debug--inspect "error" err)))

  (ws-debug-log "SETUP" "Setup complete, waiting for callbacks..."))

(defun ws-debug-connect-linode ()
  "Connect to the Linode WSS endpoint for testing."
  (interactive)
  (let ((path "/home/joe/.claude/projects/-home-joe/64570417-4354-40b8-b6a5-db804f69a1d0.jsonl"))
    (ws-debug-connect
     (format "wss://172-236-28-208.ip.linodeusercontent.com:5057?path=%s"
             (url-hexify-string path)))))

(defun ws-debug-connect-local ()
  "Connect to local lab-ws for testing."
  (interactive)
  (let ((path (read-file-name "JSONL file: " "~/.claude/projects/")))
    (ws-debug-connect
     (format "ws://localhost:5056?path=%s"
             (url-hexify-string (expand-file-name path))))))

(defun ws-debug-disconnect ()
  "Disconnect current debug session."
  (interactive)
  (when ws-debug--ws
    (ws-debug-log "USER" "Manual disconnect requested")
    (condition-case err
        (websocket-close ws-debug--ws)
      (error (ws-debug-log "USER" "Error closing: %S" err)))
    (setq ws-debug--ws nil)))

(defun ws-debug-status ()
  "Show current connection status."
  (interactive)
  (ws-debug-log "STATUS" "ws-debug--ws: %S" ws-debug--ws)
  (ws-debug-log "STATUS" "event-count: %d" ws-debug--event-count)
  (when ws-debug--ws
    (ws-debug-log "STATUS" "websocket-openp: %s" (websocket-openp ws-debug--ws))))

(provide 'ws-debug)
;;; ws-debug.el ends here
