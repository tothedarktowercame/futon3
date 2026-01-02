;;; futon3-claude.el --- Claude RPC client for Futon3 -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs interface to the Claude subprocess RPC layer in Futon3.
;; Provides commands to run Claude sessions, stream events, and
;; approve/deny tool calls via voice or keyboard.

;;; Code:

(require 'json)
(require 'url)
(require 'futon3-bridge)
(require 'fubar)

(defvar my-futon3-claude-buffer "*Claude*"
  "Buffer for displaying Claude session output.")

(defvar my-futon3-claude-current-session nil
  "Currently active Claude session ID.")

(defvar my-futon3-claude-pending-calls nil
  "List of pending tool calls awaiting approval.")

(defvar my-futon3-claude-auto-approve nil
  "When non-nil, automatically approve all tool calls.")

;; --- HTTP helpers ---

(defun my-futon3-claude--request (method path &optional payload)
  "Make a request to the Claude RPC endpoint."
  (my-futon3-ensure-running)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload
                                (encode-coding-string (json-encode payload) 'utf-8)))
         (url (concat (string-remove-suffix "/" my-futon3-ui-base-url) path))
         (buffer (url-retrieve-synchronously url t t 5)))
    (unless buffer
      (error "No response from Futon3"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "\n\n" nil 'move)
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (when (and body (not (string-empty-p (string-trim body))))
              (json-parse-string body :object-type 'plist :array-type 'list
                                 :null-object nil :false-object nil))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; --- Session management ---

(defvar my-futon3-claude-default-pattern nil
  "Default pattern-id for clock-in. Set per-project via .dir-locals.el.")

(defun my-futon3-claude-run (prompt &optional cwd pattern-id intent)
  "Start a Claude session with PROMPT.
CWD defaults to `default-directory'.
PATTERN-ID and INTENT enable fulab clock-in for the session."
  (interactive "sPrompt: ")
  (let* ((cwd (or cwd default-directory))
         (pattern-id (or pattern-id my-futon3-claude-default-pattern))
         (payload `(("prompt" . ,prompt)
                    ("cwd" . ,cwd)))
         ;; Add fulab clock-in params if provided
         (payload (if pattern-id
                      (append payload `(("pattern-id" . ,pattern-id)
                                        ("intent" . ,(or intent prompt))))
                    payload))
         (result (my-futon3-claude--request "POST" "/claude/run" payload)))
    (if (plist-get result :ok)
        (let ((session-id (plist-get result :session-id)))
          (setq my-futon3-claude-current-session session-id)
          (setq my-futon3-claude-pending-calls nil)
          ;; Log fubar clock-in when pattern-id is provided
          (when pattern-id
            (my-fubar-log-clock-in session-id pattern-id intent))
          (message "Claude session started: %s%s"
                   session-id
                   (if pattern-id (format " [%s]" pattern-id) ""))
          (my-futon3-claude--start-streaming session-id)
          session-id)
      (error "Failed to start Claude session: %s" (plist-get result :error)))))

(defun my-futon3-claude-run-with-pattern (pattern-id intent prompt &optional cwd)
  "Start a Claude session clocked in on PATTERN-ID with INTENT.
PROMPT is the initial prompt, CWD defaults to `default-directory'."
  (interactive "sPattern: \nsIntent: \nsPrompt: ")
  (my-futon3-claude-run prompt cwd pattern-id intent))

(defun my-futon3-claude-cancel ()
  "Cancel the current Claude session."
  (interactive)
  (when my-futon3-claude-current-session
    (let ((session-id my-futon3-claude-current-session))
      (my-futon3-claude--request
       "POST" (format "/claude/cancel/%s" session-id))
      ;; Log fubar clock-out before clearing session
      (my-fubar-log-clock-out session-id (list :session/status :cancelled))
      (message "Cancelled session: %s" session-id)
      (setq my-futon3-claude-current-session nil))))

(defun my-futon3-claude-sessions ()
  "List all Claude sessions."
  (interactive)
  (let ((result (my-futon3-claude--request "GET" "/claude/sessions")))
    (if (called-interactively-p 'any)
        (message "Sessions: %S" (plist-get result :sessions))
      (plist-get result :sessions))))

;; --- Fubar event helpers ---

(defun my-futon3-claude-log-pattern-used (pattern-id &optional reason)
  "Log a pattern dependency for the current Claude session.
PATTERN-ID is the pattern being used, REASON explains why."
  (when my-futon3-claude-current-session
    (my-fubar-log-pattern-used my-futon3-claude-current-session
                               pattern-id
                               (or reason "claude-context"))))

(defun my-futon3-claude-record-artifact (path &optional action)
  "Record an artifact modification for the current Claude session.
PATH is the file path, ACTION is :created, :modified, or :deleted."
  (when my-futon3-claude-current-session
    (my-fubar-record-artifact my-futon3-claude-current-session
                              path
                              (or action :modified))))

(defvar my-futon3-claude--file-tools '("Edit" "Write" "Read" "NotebookEdit")
  "Tool names that operate on files and should track artifacts.")

(defun my-futon3-claude--maybe-record-artifact (event)
  "Extract file path from EVENT and record as artifact if applicable."
  (when my-futon3-claude-current-session
    (let* ((tool (plist-get event :tool/name))
           (params (plist-get event :tool/params))
           (path (or (plist-get params :file_path)
                     (plist-get params :path)
                     (plist-get params :notebook_path))))
      (when (and tool path (member tool my-futon3-claude--file-tools))
        (let ((action (cond
                       ((string= tool "Write") :created)
                       ((string= tool "Read") :read)
                       (t :modified))))
          (my-futon3-claude-record-artifact path action))))))

;; --- Approval flow ---

(defun my-futon3-claude-approve (&optional call-id)
  "Approve a pending tool call."
  (interactive)
  (let ((call-id (or call-id (car my-futon3-claude-pending-calls))))
    (when (and my-futon3-claude-current-session call-id)
      (my-futon3-claude--request
       "POST" (format "/claude/approve/%s/%s"
                      my-futon3-claude-current-session call-id))
      (setq my-futon3-claude-pending-calls
            (delete call-id my-futon3-claude-pending-calls))
      (message "Approved: %s" call-id))))

(defun my-futon3-claude-deny (&optional call-id reason)
  "Deny a pending tool call."
  (interactive)
  (let ((call-id (or call-id (car my-futon3-claude-pending-calls)))
        (reason (or reason "Denied by user")))
    (when (and my-futon3-claude-current-session call-id)
      (my-futon3-claude--request
       "POST" (format "/claude/deny/%s/%s"
                      my-futon3-claude-current-session call-id)
       `(("reason" . ,reason)))
      (setq my-futon3-claude-pending-calls
            (delete call-id my-futon3-claude-pending-calls))
      (message "Denied: %s" call-id))))

(defun my-futon3-claude-send-input (input)
  "Send arbitrary INPUT to the Claude session PTY."
  (interactive "sInput: ")
  (when my-futon3-claude-current-session
    (my-futon3-claude--request
     "POST" (format "/claude/input/%s" my-futon3-claude-current-session)
     `(("input" . ,input)))))

;; --- Event streaming ---

(defvar my-futon3-claude--ws-process nil
  "WebSocket process for event streaming.")

(defvar-local my-futon3-claude--events-offset 0
  "Offset for polling events from current session.")

(defvar-local my-futon3-claude--stats-marker nil
  "Marker for updating stats line in place.")

(defun my-futon3-claude--format-stats (stats)
  "Format STATS plist as a summary string."
  (let ((events (or (plist-get stats :events) 0))
        (tool-calls (or (plist-get stats :tool-calls) 0))
        (messages (or (plist-get stats :assistant-messages) 0))
        (cost (or (plist-get stats :cost-usd) 0))
        (duration (or (plist-get stats :duration-ms) 0)))
    (format "[events:%d tools:%d msgs:%d cost:$%.4f time:%dms]"
            events tool-calls messages cost duration)))

(defun my-futon3-claude--update-stats (stats)
  "Update the stats line in the Claude buffer."
  (when (and stats my-futon3-claude--stats-marker)
    (let ((buf (get-buffer my-futon3-claude-buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
            (goto-char my-futon3-claude--stats-marker)
            (let ((inhibit-read-only t))
              (delete-region (point) (line-end-position))
              (insert (my-futon3-claude--format-stats stats)))))))))

(defun my-futon3-claude--start-streaming (session-id)
  "Start streaming events from SESSION-ID into the Claude buffer."
  (let ((buf (get-buffer-create my-futon3-claude-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n--- Session: %s ---\n" session-id))
      (insert "Stats: ")
      (setq-local my-futon3-claude--stats-marker (point-marker))
      (insert "[starting...]\n")
      (setq-local my-futon3-claude--events-offset 0))
    (display-buffer buf)
    ;; Poll for events
    (run-with-timer 0.5 nil #'my-futon3-claude--poll-session session-id)))

(defun my-futon3-claude--poll-session (session-id)
  "Poll session status and events."
  (when (string= session-id my-futon3-claude-current-session)
    (condition-case err
        (let ((current-offset (with-current-buffer
                                  (get-buffer-create my-futon3-claude-buffer)
                                my-futon3-claude--events-offset)))
          ;; Fetch new events starting from offset
          (let ((events-data (my-futon3-claude--request
                              "GET" (format "/claude/events/%s?offset=%d"
                                            session-id current-offset))))
            (when events-data
              (let ((events (plist-get events-data :events)))
                (dolist (event events)
                  (my-futon3-claude--insert-event event))
                ;; Update offset
                (with-current-buffer (get-buffer-create my-futon3-claude-buffer)
                  (setq-local my-futon3-claude--events-offset
                              (plist-get events-data :count))))))
          ;; Check session status and update stats
          (let ((session (my-futon3-claude--request
                          "GET" (format "/claude/session/%s" session-id))))
            (when session
              ;; Update live stats display
              (my-futon3-claude--update-stats (plist-get session :stats))
              (let ((status (plist-get session :status)))
                (cond
                 ((member status '("running" "starting"))
                  ;; Still running, poll again
                  (run-with-timer 0.5 nil #'my-futon3-claude--poll-session session-id))
                 ((member status '("finished" "cancelled" "error"))
                  ;; Fetch final events
                  (let ((final-events (my-futon3-claude--request
                                       "GET" (format "/claude/events/%s" session-id))))
                    (when final-events
                      (let ((offset (with-current-buffer
                                        (get-buffer-create my-futon3-claude-buffer)
                                      my-futon3-claude--events-offset)))
                        (dolist (event (seq-drop (plist-get final-events :events) offset))
                          (my-futon3-claude--insert-event event)))))
                  ;; Log fubar clock-out with session status
                  (my-fubar-log-clock-out session-id
                                          (list :session/status
                                                (intern (concat ":" status))))
                  (my-futon3-claude--insert-event
                   `(:event/type "session/finished" :session/status ,status))
                  (setq my-futon3-claude-current-session nil)))))))
      (error (message "Poll error: %s" err)))))

(defun my-futon3-claude--insert-event (event)
  "Insert EVENT into the Claude buffer."
  (let ((buf (get-buffer my-futon3-claude-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; Handle both keyword (:event/type) and string ("event/type") keys
        (let ((type (or (plist-get event :event/type)
                        (plist-get event (intern ":event/type")))))
          (when (keywordp type)
            (setq type (substring (symbol-name type) 1)))  ;; :foo -> "foo"
          (cond
           ((string= type "assistant/delta")
            (when-let ((text (plist-get event :assistant/text)))
              (insert text)))
           ((string= type "assistant/message")
            (when-let ((text (plist-get event :assistant/text)))
              (insert (format "\n%s\n" text))))
           ((string= type "tool/call")
            (let ((call-id (plist-get event :tool/call-id))
                  (tool (plist-get event :tool/name)))
              (push call-id my-futon3-claude-pending-calls)
              (insert (format "\n[TOOL CALL: %s] %s\n" tool call-id))
              (unless my-futon3-claude-auto-approve
                (message "Tool call pending: %s %s (approve/deny)" tool call-id))))
           ((string= type "tool/approved")
            (insert (format "[APPROVED: %s]\n" (plist-get event :tool/call-id)))
            ;; Track artifacts for file-modifying tools
            (my-futon3-claude--maybe-record-artifact event))
           ((string= type "tool/denied")
            (insert (format "[DENIED: %s]\n" (plist-get event :tool/call-id))))
           ((string= type "session/finished")
            (insert (format "\n--- Session %s ---\n"
                            (or (plist-get event :session/status) "finished"))))
           ((string= type "session/started")
            ;; Skip - we already print session header in --start-streaming
            nil)
           ((string= type "session/result")
            ;; Skip - assistant/message already shows the response
            nil)
           ((string= type "system/init")
            ;; Silently handle init event
            nil)
           ((string= type "raw")
            ;; Debug: show raw events
            nil)
           (t
            (insert (format "[%s]\n" type)))))))))

;; --- Voice command integration ---

(defun my-futon3-claude-voice-command (command)
  "Handle a voice COMMAND for Claude.
Recognized commands: approve, deny, cancel, yes, no."
  (interactive "sVoice command: ")
  (let ((cmd (downcase (string-trim command))))
    (cond
     ((member cmd '("approve" "yes" "ok" "rocket" "go"))
      (my-futon3-claude-approve))
     ((member cmd '("deny" "no" "stop" "reject"))
      (my-futon3-claude-deny))
     ((member cmd '("cancel" "abort" "quit"))
      (my-futon3-claude-cancel))
     (t
      (message "Unknown voice command: %s" cmd)))))

;; --- Minor mode ---

(defvar my-futon3-claude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'my-futon3-claude-approve)
    (define-key map (kbd "C-c C-d") #'my-futon3-claude-deny)
    (define-key map (kbd "C-c C-k") #'my-futon3-claude-cancel)
    (define-key map (kbd "C-c C-r") #'my-futon3-claude-run)
    map)
  "Keymap for `my-futon3-claude-mode'.")

(define-minor-mode my-futon3-claude-mode
  "Minor mode for interacting with Claude via Futon3."
  :lighter " Claude"
  :keymap my-futon3-claude-mode-map)

(provide 'futon3-claude)

;;; futon3-claude.el ends here
