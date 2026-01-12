;;; futon3-codex.el --- Codex RPC client for Futon3 -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs interface to the Codex subprocess RPC layer in Futon3.
;; Provides commands to run Codex sessions, stream events, and
;; approve/deny tool calls via voice or keyboard.

;;; Code:

(require 'json)
(require 'url)
(require 'futon3-bridge)

(defvar my-futon3-codex-buffer "*Codex*"
  "Buffer for displaying Codex session output.")

(defvar my-futon3-codex-current-session nil
  "Currently active Codex session ID.")

(defvar my-futon3-codex-session-registry nil
  "Alist mapping Futon3 session IDs to Codex CLI session IDs.")

(defvar my-futon3-codex-session-cwd nil
  "Alist mapping Futon3 session IDs to their original cwd.")

(defvar my-futon3-codex-pending-calls nil
  "List of pending tool calls awaiting approval.")

(defvar my-futon3-codex-auto-approve nil
  "When non-nil, automatically approve all tool calls.")

(defvar my-futon3-codex-inject-hud-on-input t
  "When non-nil, prepend the FuLab HUD block to user input at checkpoints.")

(defun my-futon3-codex--maybe-inject-hud (prompt)
  "Return PROMPT with HUD context when available."
  (if (and my-futon3-codex-inject-hud-on-input
           (fboundp 'fubar-hud-inject-into-prompt))
      (condition-case _err
          (fubar-hud-inject-into-prompt prompt)
        (error prompt))
    prompt))

(defvar my-futon3-codex-show-tool-trace nil
  "When non-nil, render non-approval tool trace events in the Codex buffer.")

(defvar my-futon3-codex-show-tool-results t
  "When non-nil, render tool result output in the Codex buffer.")

(defvar my-futon3-codex-show-assistant-deltas nil
  "When non-nil, render assistant delta events in the Codex buffer.")

(defvar my-futon3-codex-show-assistant-messages nil
  "When non-nil, render assistant messages in the Codex buffer.")

(defvar my-futon3-codex-show-fulab-report t
  "When non-nil, render FULAB-REPORT blocks even if messages are hidden.")

;; --- HTTP helpers ---

(defun my-futon3-codex--request (method path &optional payload)
  "Make a request to the Codex RPC endpoint."
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

(defvar my-futon3-codex-default-pattern nil
  "Default pattern-id for clock-in. Set per-project via .dir-locals.el.")

(defun my-futon3-codex--remember-session-cwd (session-id cwd)
  "Record CWD for SESSION-ID."
  (when (and session-id cwd)
    (setq my-futon3-codex-session-cwd
          (assq-delete-all session-id my-futon3-codex-session-cwd))
    (push (cons session-id cwd) my-futon3-codex-session-cwd)))

(defun my-futon3-codex--remember-cli-session-id (session-id cli-session-id)
  "Record CLI-SESSION-ID for SESSION-ID."
  (when (and session-id cli-session-id)
    (setq my-futon3-codex-session-registry
          (assq-delete-all session-id my-futon3-codex-session-registry))
    (push (cons session-id cli-session-id) my-futon3-codex-session-registry)))

(defun my-futon3-codex-run (prompt &optional cwd pattern-id intent)
  "Start a Codex session with PROMPT.
CWD defaults to `default-directory'.
PATTERN-ID and INTENT enable fulab clock-in for the session."
  (interactive "sPrompt: ")
  (let* ((cwd (or cwd default-directory))
         (pattern-id (or pattern-id my-futon3-codex-default-pattern))
         (payload `(("prompt" . ,prompt)
                    ("cwd" . ,cwd)))
         ;; Add fulab clock-in params if provided
         (payload (if pattern-id
                      (append payload `(("pattern-id" . ,pattern-id)
                                        ("intent" . ,(or intent prompt))))
                    payload))
         (result (my-futon3-codex--request "POST" "/codex/run" payload)))
    (if (plist-get result :ok)
        (let ((session-id (plist-get result :session-id)))
          (setq my-futon3-codex-current-session session-id)
          (setq my-futon3-codex-pending-calls nil)
          (my-futon3-codex--remember-session-cwd session-id cwd)
          (message "Codex session started: %s%s"
                   session-id
                   (if pattern-id (format " [%s]" pattern-id) ""))
          (my-futon3-codex--start-streaming session-id)
          session-id)
      (error "Failed to start Codex session: %s" (plist-get result :error)))))

(defun my-futon3-codex-run-with-pattern (pattern-id intent prompt &optional cwd)
  "Start a Codex session clocked in on PATTERN-ID with INTENT.
PROMPT is the initial prompt, CWD defaults to `default-directory'."
  (interactive "sPattern: \nsIntent: \nsPrompt: ")
  (my-futon3-codex-run prompt cwd pattern-id intent))

(defun my-futon3-codex-continue (prompt)
  "Continue the most recent Codex session with PROMPT."
  (interactive "sFollow-up: ")
  (let* ((last-session (car my-futon3-codex-session-registry))
         (session-id (car last-session))
         (cli-session-id (cdr last-session))
         (stored-cwd (cdr (assq session-id my-futon3-codex-session-cwd))))
    (if cli-session-id
        (my-futon3-codex-resume cli-session-id prompt stored-cwd)
      (error "No previous session to continue"))))

(defun my-futon3-codex-resume (cli-session-id prompt &optional cwd)
  "Resume Codex CLI session CLI-SESSION-ID with PROMPT."
  (interactive "sSession ID: \nsPrompt: ")
  (let* ((cwd (or cwd default-directory))
         (prompt (my-futon3-codex--maybe-inject-hud prompt))
         (result (my-futon3-codex--request
                  "POST" "/codex/resume"
                  `(("codex-cli-session-id" . ,cli-session-id)
                    ("prompt" . ,prompt)
                    ("cwd" . ,cwd)))))
    (if (plist-get result :ok)
        (let ((session-id (plist-get result :session-id)))
          (setq my-futon3-codex-current-session session-id)
          (setq my-futon3-codex-pending-calls nil)
          (my-futon3-codex--remember-session-cwd session-id cwd)
          (message "Codex session resumed: %s" session-id)
          (my-futon3-codex--start-streaming session-id)
          session-id)
      (error "Failed to resume Codex session: %s" (plist-get result :error)))))

(defun my-futon3-codex-cancel ()
  "Cancel the current Codex session."
  (interactive)
  (when my-futon3-codex-current-session
    (my-futon3-codex--request
     "POST" (format "/codex/cancel/%s" my-futon3-codex-current-session))
    (message "Cancelled session: %s" my-futon3-codex-current-session)
    (setq my-futon3-codex-current-session nil)))

(defun my-futon3-codex-sessions ()
  "List all Codex sessions."
  (interactive)
  (let ((result (my-futon3-codex--request "GET" "/codex/sessions")))
    (if (called-interactively-p 'any)
        (message "Sessions: %S" (plist-get result :sessions))
      (plist-get result :sessions))))

;; --- Approval flow ---

(defun my-futon3-codex-approve (&optional call-id)
  "Approve a pending tool call."
  (interactive)
  (let ((call-id (or call-id (car my-futon3-codex-pending-calls))))
    (when (and my-futon3-codex-current-session call-id)
      (my-futon3-codex--request
       "POST" (format "/codex/approve/%s/%s"
                      my-futon3-codex-current-session call-id))
      (setq my-futon3-codex-pending-calls
            (delete call-id my-futon3-codex-pending-calls))
      (message "Approved: %s" call-id))))

(defun my-futon3-codex-deny (&optional call-id reason)
  "Deny a pending tool call."
  (interactive)
  (let ((call-id (or call-id (car my-futon3-codex-pending-calls)))
        (reason (or reason "Denied by user")))
    (when (and my-futon3-codex-current-session call-id)
      (my-futon3-codex--request
       "POST" (format "/codex/deny/%s/%s"
                      my-futon3-codex-current-session call-id)
       `(("reason" . ,reason)))
      (setq my-futon3-codex-pending-calls
            (delete call-id my-futon3-codex-pending-calls))
      (message "Denied: %s" call-id))))

(defun my-futon3-codex-send-input (input)
  "Send arbitrary INPUT to the Codex session PTY."
  (interactive "sInput: ")
  (when my-futon3-codex-current-session
    (let ((input (my-futon3-codex--maybe-inject-hud input)))
      (my-futon3-codex--request
       "POST" (format "/codex/input/%s" my-futon3-codex-current-session)
       `(("input" . ,input))))))

(defun my-futon3-codex-pattern-action (action pattern-id &optional note)
  "Record a lightweight pattern ACTION for PATTERN-ID with optional NOTE."
  (interactive "sAction (read/implement/update): \nsPattern id: \nsNote: ")
  (unless my-futon3-codex-current-session
    (error "No current Codex session"))
  (let ((payload `(("pattern-id" . ,pattern-id)
                   ("action" . ,action)
                   ("note" . ,note))))
    (my-futon3-codex--request
     "POST" (format "/codex/pattern-action/%s" my-futon3-codex-current-session)
     payload)))

;; --- Event streaming ---

(defvar my-futon3-codex--ws-process nil
  "WebSocket process for event streaming.")

(defvar-local my-futon3-codex--events-offset 0
  "Offset for polling events from current session.")

(defvar-local my-futon3-codex--stats-marker nil
  "Marker for updating stats line in place.")

(defvar-local my-futon3-codex--seen-events nil
  "Hash table of event IDs already rendered.")

(defvar-local my-futon3-codex--seen-events nil
  "Hash table of event IDs already rendered.")

(defun my-futon3-codex--format-stats (stats)
  "Format STATS plist as a summary string."
  (let ((events (or (plist-get stats :events) 0))
        (tool-calls (or (plist-get stats :tool-calls) 0))
        (messages (or (plist-get stats :assistant-messages) 0))
        (cost (or (plist-get stats :cost-usd) 0))
        (duration (or (plist-get stats :duration-ms) 0)))
    (format "[events:%d tools:%d msgs:%d cost:$%.4f time:%dms]"
            events tool-calls messages cost duration)))

(defun my-futon3-codex--update-stats (stats)
  "Update the stats line in the Codex buffer."
  (when (and stats my-futon3-codex--stats-marker)
    (let ((buf (get-buffer my-futon3-codex-buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (save-excursion
            (goto-char my-futon3-codex--stats-marker)
            (let ((inhibit-read-only t))
              (delete-region (point) (line-end-position))
              (insert (my-futon3-codex--format-stats stats)))))))))

(defun my-futon3-codex--start-streaming (session-id)
  "Start streaming events from SESSION-ID into the Codex buffer."
  (let ((buf (get-buffer-create my-futon3-codex-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n--- Session: %s ---\n" session-id))
      (insert "Stats: ")
      (setq-local my-futon3-codex--stats-marker (point-marker))
      (insert "[starting...]\n")
      (setq-local my-futon3-codex--events-offset 0)
      (setq-local my-futon3-codex--seen-events (make-hash-table :test 'equal)))
    (display-buffer buf)
    ;; Poll for events
    (run-with-timer 0.5 nil #'my-futon3-codex--poll-session session-id)))

(defun my-futon3-codex--poll-session (session-id)
  "Poll session status and events."
  (when (string= session-id my-futon3-codex-current-session)
    (condition-case err
        (let ((current-offset (with-current-buffer
                                  (get-buffer-create my-futon3-codex-buffer)
                                my-futon3-codex--events-offset)))
          ;; Fetch new events starting from offset
          (let ((events-data (my-futon3-codex--request
                              "GET" (format "/codex/events/%s?offset=%d"
                                            session-id current-offset))))
            (when events-data
              (let ((events (plist-get events-data :events)))
                (dolist (event events)
                  (my-futon3-codex--insert-event event))
                ;; Update offset
                (with-current-buffer (get-buffer-create my-futon3-codex-buffer)
                  (setq-local my-futon3-codex--events-offset
                              (+ current-offset (length events)))))))
          ;; Check session status and update stats
          (let ((session (my-futon3-codex--request
                          "GET" (format "/codex/session/%s" session-id))))
            (when session
              ;; Update live stats display
              (my-futon3-codex--update-stats (plist-get session :stats))
              (let ((status (plist-get session :status)))
                (cond
                 ((member status '("running" "starting"))
                  ;; Still running, poll again
                  (run-with-timer 0.5 nil #'my-futon3-codex--poll-session session-id))
                 ((member status '("finished" "cancelled" "error"))
                  ;; Fetch final events
                  (let ((final-events (my-futon3-codex--request
                                       "GET" (format "/codex/events/%s" session-id))))
                    (when final-events
                      (let ((offset (with-current-buffer
                                        (get-buffer-create my-futon3-codex-buffer)
                                      my-futon3-codex--events-offset)))
                        (dolist (event (seq-drop (plist-get final-events :events) offset))
                          (my-futon3-codex--insert-event event)))))
                  (let ((final-session (my-futon3-codex--request
                                        "GET" (format "/codex/session/%s" session-id))))
                    (when final-session
                      (my-futon3-codex--remember-cli-session-id
                       session-id (plist-get final-session :codex-cli-session-id))))
                  (my-futon3-codex--insert-event
                   `(:event/type "session/finished" :session/status ,status))
                  (setq my-futon3-codex-current-session nil)))))))
      (error (message "Poll error: %s" err)))))

(defun my-futon3-codex--insert-event (event)
  "Insert EVENT into the Codex buffer."
  (let ((buf (get-buffer my-futon3-codex-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        (condition-case err
            (let ((event-id (or (plist-get event :event/id)
                                (plist-get event (intern ":event/id")))))
              (when (or (null event-id)
                        (not my-futon3-codex--seen-events)
                        (not (gethash event-id my-futon3-codex--seen-events)))
                (when (and event-id my-futon3-codex--seen-events)
                  (puthash event-id t my-futon3-codex--seen-events))
                (let ((type (or (plist-get event :event/type)
                                (plist-get event (intern ":event/type")))))
                  (when (keywordp type)
                    (setq type (substring (symbol-name type) 1)))  ;; :foo -> "foo"
                  (cond
                   ((string= type "assistant/delta")
                    (when my-futon3-codex-show-assistant-deltas
                      (let ((text (plist-get event :assistant/text)))
                        (when text
                          (insert text)))))
                   ((string= type "assistant/message")
                    (let ((text (plist-get event :assistant/text)))
                      (cond
                       (my-futon3-codex-show-assistant-messages
                        (when text
                          (insert (format "
%s
" text))))
                       (my-futon3-codex-show-fulab-report
                        (when (and text (string-match "\\[FULAB-REPORT\\]\\(.\\|\n\\)*?\\[/FULAB-REPORT\\]" text))
                          (insert (format "
%s
"
                                          (string-trim (match-string 0 text)))))))))
                   ((string= type "pattern/action")
                    (let ((pid (plist-get event :pattern/id))
                          (action (plist-get event :pattern/action))
                          (note (plist-get event :pattern/note)))
                      (insert (format "
[PATTERN] %s %s%s
"
                                      (or action "action")
                                      (or pid "unknown")
                                      (if (and note (> (length note) 0))
                                          (format " - %s" note)
                                        "")))))
                   ((string= type "tool/call")
                    (let* ((call-id (plist-get event :tool/call-id))
                           (tool (plist-get event :tool/name))
                           (status (or (plist-get event :tool/status)
                                       (plist-get event (intern ":tool/status"))))
                           (status-str (cond
                                        ((symbolp status) (symbol-name status))
                                        ((stringp status) status)
                                        (t nil))))
                      (when (or (null status-str) (string= status-str "pending"))
                        (push call-id my-futon3-codex-pending-calls)
                        (unless my-futon3-codex-auto-approve
                          (message "Tool call pending: %s %s (approve/deny)" tool call-id)))))
                   ((string= type "session/finished")
                    (insert (format "
--- Session %s ---
"
                                    (or (plist-get event :session/status) "finished"))))
                   ((string= type "session/error")
                    (let ((msg (plist-get event :error/message)))
                      (insert (format "
--- Session error ---
%s
"
                                      (or msg "unknown error")))))
                   ((string= type "session/result")
                    (let ((cli-session-id (plist-get event :result/codex-session-id))
                          (session-id (or (plist-get event :event/session)
                                          (plist-get event (intern ":event/session")))))
                      (my-futon3-codex--remember-cli-session-id session-id cli-session-id)))
                   (t nil)))))
          (error
           (message "Codex render error: %s" err)))))))

;; --- Voice command integration ---

(defun my-futon3-codex-voice-command (command)
  "Handle a voice COMMAND for Codex.
Recognized commands: approve, deny, cancel, yes, no."
  (interactive "sVoice command: ")
  (let ((cmd (downcase (string-trim command))))
    (cond
     ((member cmd '("approve" "yes" "ok" "rocket" "go"))
      (my-futon3-codex-approve))
     ((member cmd '("deny" "no" "stop" "reject"))
      (my-futon3-codex-deny))
     ((member cmd '("cancel" "abort" "quit"))
      (my-futon3-codex-cancel))
     (t
      (message "Unknown voice command: %s" cmd)))))

;; --- Minor mode ---

(defvar my-futon3-codex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'my-futon3-codex-approve)
    (define-key map (kbd "C-c C-d") #'my-futon3-codex-deny)
    (define-key map (kbd "C-c C-k") #'my-futon3-codex-cancel)
    (define-key map (kbd "C-c C-r") #'my-futon3-codex-run)
    map)
  "Keymap for `my-futon3-codex-mode'.")

(define-minor-mode my-futon3-codex-mode
  "Minor mode for interacting with Codex via Futon3."
  :lighter " Codex"
  :keymap my-futon3-codex-mode-map)

(provide 'futon3-codex)

;;; futon3-codex.el ends here
