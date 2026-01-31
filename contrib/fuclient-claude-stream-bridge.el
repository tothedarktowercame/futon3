;;; fuclient-claude-stream-bridge.el --- Claude stream via bb WSS bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Workaround for Emacs 31 WSS bug: use babashka as WSS proxy.
;; bb connects via WSS and outputs JSON to stdout, we read from the process.

;;; Code:

(require 'json)

(defgroup fuclient-claude-stream-bridge nil
  "Claude Code streaming via bb WSS bridge."
  :group 'communication)

(defcustom fuclient-claude-stream-bridge-bb-path "bb"
  "Path to babashka executable."
  :type 'string
  :group 'fuclient-claude-stream-bridge)

(defcustom fuclient-claude-stream-bridge-script
  (expand-file-name "~/code/futon3/scripts/wss-bridge.clj")
  "Path to the WSS bridge script."
  :type 'string
  :group 'fuclient-claude-stream-bridge)

(defvar fuclient-claude-stream-bridge--process nil
  "Current bridge process.")

(defvar fuclient-claude-stream-bridge--buffer-name "*Claude Stream (Bridge)*"
  "Buffer name for stream output.")

(defface fuclient-claude-stream-bridge-user-face
  '((t :foreground "#88c0d0" :weight bold))
  "Face for user messages."
  :group 'fuclient-claude-stream-bridge)

(defface fuclient-claude-stream-bridge-assistant-face
  '((t :foreground "#a3be8c"))
  "Face for assistant messages."
  :group 'fuclient-claude-stream-bridge)

(defface fuclient-claude-stream-bridge-tool-face
  '((t :foreground "#b48ead"))
  "Face for tool use/result."
  :group 'fuclient-claude-stream-bridge)

(defface fuclient-claude-stream-bridge-timestamp-face
  '((t :foreground "#4c566a"))
  "Face for timestamps."
  :group 'fuclient-claude-stream-bridge)

(defun fuclient-claude-stream-bridge--get-buffer ()
  "Get or create the stream buffer."
  (get-buffer-create fuclient-claude-stream-bridge--buffer-name))

(defun fuclient-claude-stream-bridge--format-event (event)
  "Format EVENT for display."
  (let* ((type (or (cdr (assq 'type event)) "unknown"))
         (timestamp (or (cdr (assq 'timestamp event)) ""))
         (text (or (cdr (assq 'text event)) ""))
         (tool-name (cdr (assq 'tool-name event)))
         (ts-str (if (> (length timestamp) 19)
                     (substring timestamp 11 19)
                   timestamp)))
    (concat
     (propertize (format "[%s] " ts-str)
                 'face 'fuclient-claude-stream-bridge-timestamp-face)
     (pcase type
       ("user"
        (concat (propertize "USER: " 'face 'fuclient-claude-stream-bridge-user-face)
                (truncate-string-to-width text 500)))
       ("assistant"
        (concat (propertize "AGENT: " 'face 'fuclient-claude-stream-bridge-assistant-face)
                (truncate-string-to-width text 500)))
       ("tool_use"
        (propertize (format "TOOL: %s" (or tool-name "?"))
                    'face 'fuclient-claude-stream-bridge-tool-face))
       ("tool_result"
        (propertize "RESULT: ..."
                    'face 'fuclient-claude-stream-bridge-tool-face))
       ("par"
        (concat (propertize "PAR: " 'face 'font-lock-warning-face)
                "\n" text))
       (_
        (format "[%s]" type)))
     "\n")))

(defun fuclient-claude-stream-bridge--append (text)
  "Append TEXT to the stream buffer."
  (let ((buf (fuclient-claude-stream-bridge--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when at-end
          (goto-char (point-max)))))))

(defun fuclient-claude-stream-bridge--filter (proc output)
  "Process filter for bridge OUTPUT from PROC."
  (dolist (line (split-string output "\n" t))
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string line)))
          (let ((msg-type (cdr (assq 'type data))))
            (pcase msg-type
              ("init"
               (let ((events (cdr (assq 'events data)))
                     (line-count (cdr (assq 'line-count data))))
                 (fuclient-claude-stream-bridge--append
                  (format "--- Connected (bridge): %d lines, %d events ---\n\n"
                          (or line-count 0) (length events)))
                 (dolist (event events)
                   (fuclient-claude-stream-bridge--append
                    (fuclient-claude-stream-bridge--format-event event)))))
              ("event"
               (let ((event (cdr (assq 'event data))))
                 (fuclient-claude-stream-bridge--append
                  (fuclient-claude-stream-bridge--format-event event))))
              ("error"
               (fuclient-claude-stream-bridge--append
                (format "ERROR: %s\n" (cdr (assq 'err data))))))))
      (error nil))))

(defun fuclient-claude-stream-bridge--sentinel (proc event)
  "Process sentinel for PROC with EVENT."
  (fuclient-claude-stream-bridge--append
   (format "\n--- Bridge %s ---\n" (string-trim event))))

;;;###autoload
(defun fuclient-claude-stream-bridge-connect (wss-url)
  "Connect to WSS-URL via bb bridge."
  (interactive "sWSS URL: ")
  ;; Kill existing
  (when (and fuclient-claude-stream-bridge--process
             (process-live-p fuclient-claude-stream-bridge--process))
    (kill-process fuclient-claude-stream-bridge--process))

  ;; Clear buffer
  (with-current-buffer (fuclient-claude-stream-bridge--get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Connecting via bridge to %s...\n" wss-url))))

  ;; Start bridge process
  (setq fuclient-claude-stream-bridge--process
        (start-process "wss-bridge"
                       nil  ; no buffer, we use filter
                       fuclient-claude-stream-bridge-bb-path
                       fuclient-claude-stream-bridge-script
                       wss-url))

  (set-process-filter fuclient-claude-stream-bridge--process
                      #'fuclient-claude-stream-bridge--filter)
  (set-process-sentinel fuclient-claude-stream-bridge--process
                        #'fuclient-claude-stream-bridge--sentinel)

  (pop-to-buffer (fuclient-claude-stream-bridge--get-buffer)))

;;;###autoload
(defun fuclient-claude-stream-bridge-disconnect ()
  "Disconnect the bridge."
  (interactive)
  (when (and fuclient-claude-stream-bridge--process
             (process-live-p fuclient-claude-stream-bridge--process))
    (kill-process fuclient-claude-stream-bridge--process))
  (setq fuclient-claude-stream-bridge--process nil))

(provide 'fuclient-claude-stream-bridge)
;;; fuclient-claude-stream-bridge.el ends here
