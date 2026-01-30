;;; stack-resume.el --- Session browser and resume for MUSN -*- lexical-binding: t; -*-

;; Author: Futon Stack
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes
;; URL: https://github.com/holtzermann17/futon3

;;; Commentary:

;; stack-resume.el provides a session browser for MUSN (the Multi-User
;; Session Network).  It allows you to search past agent sessions by
;; keyword and resume them with the appropriate agent (Claude, Codex,
;; or ChatGPT).
;;
;; This enables a workflow where you can find a past conversation about
;; a topic (e.g., "cyberants" or "blast radius") and jump right back in
;; with full context.

;;; Setup:

;; 1. Ensure MUSN server is running (default: http://localhost:6065)
;;
;; 2. Add to your init.el:
;;
;;    (add-to-list 'load-path "/path/to/futon3/contrib")
;;    (require 'stack-resume)
;;
;; 3. Optionally configure:
;;
;;    (setq stack-resume-api-url "http://localhost:6065")  ; MUSN server
;;    (setq stack-resume-fuclaude-command "claude")        ; Claude CLI
;;    (setq stack-resume-fucodex-command "codex")          ; Codex CLI
;;    (setq stack-resume-working-dir "~/code/myproject")   ; Working dir

;;; Usage:

;; Interactive commands:
;;
;;   M-x stack-resume-browse     Open the session browser (lists all sessions)
;;   M-x stack-resume-search     Search sessions by keyword
;;
;; From the Stack HUD:
;;
;;   Click [Browse All] in the Session section to open the browser.

;;; Browser Keybindings:

;;   RET   View session details (events, turns, patterns used)
;;   r     Resume session with auto-detected agent
;;   R     Resume session with manually chosen agent
;;   s, /  Search/filter sessions
;;   g     Refresh the session list
;;   n, p  Next/previous line
;;   q     Quit browser

;;; Supported Agents:

;; The browser detects which agent originally ran the session and
;; launches the appropriate tool:
;;
;;   fuclaude, claude, musn  ->  Claude Code CLI
;;   fucodex, codex          ->  Codex CLI
;;   aob, chatgpt            ->  Opens context file for manual paste
;;
;; When resuming, the session's conversation history is exported as
;; markdown and passed to the agent via --resume flag.

;;; API Endpoints Used:

;; This package calls the MUSN HTTP API:
;;
;;   GET /musn/sessions              List all sessions
;;   GET /musn/sessions/search?q=... Search sessions by keyword
;;   GET /musn/sessions/:id          Fetch full session details

;;; Integration with Stack HUD:

;; This package adds a [Browse All] button to fubar-hud.el's Session
;; section.  Clicking it opens the session browser.
;;
;; To use from Lisp:
;;
;;   (stack-resume-from-hud)  ; Same as stack-resume-browse

;;; Future Work:

;; This is a demo/prototype.  The full implementation should integrate
;; with Arxana/futon4's Lab browser for:
;;
;;   - Hypertext navigation of session artifacts
;;   - Cross-session linking (anchors and links)
;;   - Semantic search via embeddings (futon3a)
;;   - Session archival to futon1 long-term storage

;;; Code:

(require 'json)
(require 'url)

(defgroup stack-resume nil
  "Session browser and resume for MUSN."
  :group 'fubar
  :prefix "stack-resume-")

(defcustom stack-resume-api-url "http://localhost:6065"
  "Base URL for MUSN API."
  :type 'string
  :group 'stack-resume)

(defcustom stack-resume-buffer-name "*Stack Sessions*"
  "Name of the session browser buffer."
  :type 'string
  :group 'stack-resume)

(defcustom stack-resume-detail-buffer-name "*Session Detail*"
  "Name of the session detail buffer."
  :type 'string
  :group 'stack-resume)

(defcustom stack-resume-agents
  '(("fuclaude" . stack-resume--launch-fuclaude)
    ("fucodex"  . stack-resume--launch-fucodex)
    ("codex"    . stack-resume--launch-fucodex)
    ("aob"      . stack-resume--launch-aob)
    ("chatgpt"  . stack-resume--launch-aob)
    ("musn"     . stack-resume--launch-fuclaude))
  "Alist mapping agent names to launch functions."
  :type '(alist :key-type string :value-type function)
  :group 'stack-resume)

(defcustom stack-resume-fuclaude-command "claude"
  "Command to launch fuclaude/Claude Code."
  :type 'string
  :group 'stack-resume)

(defcustom stack-resume-fucodex-command "codex"
  "Command to launch fucodex/Codex CLI."
  :type 'string
  :group 'stack-resume)

(defcustom stack-resume-working-dir nil
  "Working directory for agent launches. If nil, uses current directory."
  :type '(choice (const nil) directory)
  :group 'stack-resume)

;;; State

(defvar stack-resume--sessions nil
  "Current list of sessions.")

(defvar stack-resume--current-query nil
  "Current search query, if any.")

(defvar-local stack-resume--session-at-point nil
  "Session data at point (buffer-local in detail buffer).")

;;; Faces

(defface stack-resume-session-id
  '((t :foreground "cyan" :weight bold))
  "Face for session IDs."
  :group 'stack-resume)

(defface stack-resume-agent-fuclaude
  '((t :foreground "medium purple"))
  "Face for fuclaude agent."
  :group 'stack-resume)

(defface stack-resume-agent-fucodex
  '((t :foreground "orange"))
  "Face for fucodex agent."
  :group 'stack-resume)

(defface stack-resume-agent-other
  '((t :foreground "gray70"))
  "Face for other agents."
  :group 'stack-resume)

(defface stack-resume-timestamp
  '((t :foreground "gray60"))
  "Face for timestamps."
  :group 'stack-resume)

(defface stack-resume-event-count
  '((t :foreground "green"))
  "Face for event counts."
  :group 'stack-resume)

(defface stack-resume-match
  '((t :foreground "yellow" :weight bold))
  "Face for search matches."
  :group 'stack-resume)

;;; API

(defun stack-resume--api-get (endpoint callback)
  "GET from ENDPOINT and call CALLBACK with parsed JSON."
  (let ((url (concat stack-resume-api-url endpoint)))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let ((json-object-type 'plist)
             (json-array-type 'list))
         (condition-case err
             (let ((data (json-read)))
               (funcall callback data))
           (error
            (message "Stack Resume: Parse error - %s" (error-message-string err))))))
     nil t)))

(defun stack-resume--fetch-sessions (callback)
  "Fetch all sessions and call CALLBACK with results."
  (stack-resume--api-get "/musn/sessions"
    (lambda (data)
      (when (plist-get data :ok)
        (funcall callback (plist-get data :sessions))))))

(defun stack-resume--search-sessions (query callback)
  "Search sessions for QUERY and call CALLBACK with results."
  (let ((encoded-query (url-hexify-string query)))
    (stack-resume--api-get (format "/musn/sessions/search?q=%s&limit=50" encoded-query)
      (lambda (data)
        (when (plist-get data :ok)
          (funcall callback (plist-get data :sessions)))))))

(defun stack-resume--fetch-session (session-id callback)
  "Fetch full session SESSION-ID and call CALLBACK with result."
  (stack-resume--api-get (format "/musn/sessions/%s" session-id)
    (lambda (data)
      (when (plist-get data :ok)
        (funcall callback (plist-get data :session))))))

;;; Formatting

(defun stack-resume--agent-face (agent)
  "Return face for AGENT name."
  (cond
   ((member agent '("fuclaude" "claude" "musn")) 'stack-resume-agent-fuclaude)
   ((member agent '("fucodex" "codex")) 'stack-resume-agent-fucodex)
   (t 'stack-resume-agent-other)))

(defun stack-resume--format-timestamp (ts)
  "Format timestamp TS for display."
  (if ts
      (if (> (length ts) 16)
          (substring ts 0 16)
        ts)
    "????-??-??"))

(defun stack-resume--format-session-line (session)
  "Format SESSION as a single line for the browser."
  (let* ((id (plist-get session :id))
         (agent (or (plist-get session :agent) "?"))
         (events (or (plist-get session :event_count) 0))
         (last-event (plist-get session :last_event))
         (score (plist-get session :score))
         (matches (plist-get session :matches))
         (preview (plist-get session :preview)))
    (concat
     ;; Timestamp
     (propertize (stack-resume--format-timestamp last-event)
                 'face 'stack-resume-timestamp)
     " "
     ;; Agent
     (propertize (format "%-10s" agent)
                 'face (stack-resume--agent-face agent))
     " "
     ;; Event count
     (propertize (format "%3d" events)
                 'face 'stack-resume-event-count)
     " "
     ;; Session ID (truncated)
     (propertize (if (> (length id) 24)
                     (concat (substring id 0 21) "...")
                   (format "%-24s" id))
                 'face 'stack-resume-session-id)
     ;; Score (if search result)
     (when score
       (propertize (format " [%.0f%%]" (* 100 score))
                   'face 'stack-resume-match))
     ;; Matches (if search result)
     (when matches
       (concat " "
               (propertize (mapconcat #'identity matches ",")
                           'face 'stack-resume-match)))
     ;; Preview (if available)
     (when preview
       (concat "\n    "
               (propertize (if (> (length preview) 80)
                               (concat (substring preview 0 77) "...")
                             preview)
                           'face 'font-lock-comment-face))))))

;;; Buffer rendering

(defun stack-resume--render-sessions (sessions &optional query)
  "Render SESSIONS in the browser buffer. QUERY is optional search term."
  (setq stack-resume--sessions sessions)
  (setq stack-resume--current-query query)
  (let ((buf (get-buffer-create stack-resume-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (stack-resume-mode)
      ;; Header
      (insert (propertize "Stack Session Browser" 'face 'bold))
      (insert "\n")
      (if query
          (insert (format "Search: %s (%d results)\n\n"
                          (propertize query 'face 'stack-resume-match)
                          (length sessions)))
        (insert (format "%d sessions\n\n" (length sessions))))
      ;; Column headers
      (insert (propertize "Date             Agent      Evts Session ID\n"
                          'face 'font-lock-comment-face))
      (insert (propertize (make-string 70 ?─) 'face 'font-lock-comment-face))
      (insert "\n")
      ;; Sessions
      (if (null sessions)
          (insert (propertize "No sessions found.\n" 'face 'font-lock-warning-face))
        (dolist (session sessions)
          (let ((start (point)))
            (insert (stack-resume--format-session-line session))
            (insert "\n")
            (put-text-property start (point) 'stack-resume-session session))))
      ;; Footer
      (insert "\n")
      (insert (propertize "[RET] view  [r] resume  [R] resume (choose agent)  [s] search  [g] refresh  [q] quit"
                          'face 'font-lock-comment-face))
      (goto-char (point-min))
      (forward-line 5))  ;; Move to first session
    (pop-to-buffer buf)))

(defun stack-resume--render-detail (session)
  "Render SESSION detail in a separate buffer."
  (let ((buf (get-buffer-create stack-resume-detail-buffer-name))
        (inhibit-read-only t)
        (id (plist-get session :session/id))
        (agent (plist-get session :session/agent))
        (events (plist-get session :events)))
    (with-current-buffer buf
      (erase-buffer)
      (special-mode)
      (setq-local stack-resume--session-at-point session)
      ;; Header
      (insert (propertize "Session Detail\n" 'face 'bold))
      (insert (make-string 60 ?─))
      (insert "\n\n")
      ;; Metadata
      (insert (format "ID:     %s\n" (propertize (or id "?") 'face 'stack-resume-session-id)))
      (insert (format "Agent:  %s\n" (propertize (format "%s" (or agent "?"))
                                                  'face (stack-resume--agent-face (format "%s" agent)))))
      (insert (format "Events: %d\n\n" (length events)))
      ;; Events
      (insert (propertize "Events:\n" 'face 'bold))
      (insert (make-string 40 ?─))
      (insert "\n")
      (dolist (event events)
        (let ((type (plist-get event :event/type))
              (at (plist-get event :at))
              (payload (plist-get event :payload)))
          (insert (propertize (stack-resume--format-timestamp (format "%s" at))
                              'face 'stack-resume-timestamp))
          (insert " ")
          (insert (propertize (format "%s" (or type "?"))
                              'face 'font-lock-keyword-face))
          ;; Content preview
          (when-let ((content (or (plist-get payload :content)
                                  (plist-get event :description)
                                  (plist-get payload :note))))
            (insert "\n  ")
            (insert (propertize (if (> (length content) 100)
                                    (concat (substring content 0 97) "...")
                                  content)
                                'face 'font-lock-doc-face)))
          (insert "\n")))
      ;; Footer with resume options
      (insert "\n")
      (insert (make-string 60 ?─))
      (insert "\n")
      (insert (propertize "[r] resume  [R] resume (choose agent)  [q] quit"
                          'face 'font-lock-comment-face))
      (goto-char (point-min))
      ;; Local keymap for detail buffer
      (local-set-key "r" #'stack-resume-resume-at-point)
      (local-set-key "R" #'stack-resume-resume-choose-agent)
      (local-set-key "q" #'quit-window))
    (display-buffer buf '(display-buffer-pop-up-window))))

;;; Agent launchers

(defun stack-resume--session-context-file (session)
  "Write SESSION context to a temp file, return path."
  (let* ((id (or (plist-get session :session/id) "unknown"))
         (file (make-temp-file (format "session-%s-" id) nil ".md"))
         (events (plist-get session :events)))
    (with-temp-file file
      (insert (format "# Session Context: %s\n\n" id))
      (insert (format "Agent: %s\n\n" (plist-get session :session/agent)))
      (insert "## Conversation History\n\n")
      (dolist (event events)
        (let ((type (plist-get event :event/type))
              (payload (plist-get event :payload)))
          (when (memq type '(:turn/user :turn/agent turn/user turn/agent))
            (let ((role (plist-get payload :role))
                  (content (plist-get payload :content)))
              (when content
                (insert (format "### %s\n\n%s\n\n"
                                (if (memq role '(:user user "user")) "User" "Assistant")
                                content))))))))
    file))

(defun stack-resume--launch-fuclaude (session)
  "Launch fuclaude/Claude Code to resume SESSION."
  (let* ((id (plist-get session :session/id))
         (context-file (stack-resume--session-context-file session))
         (dir (or stack-resume-working-dir default-directory))
         (cmd (format "%s --resume %s" stack-resume-fuclaude-command context-file)))
    (message "Launching Claude Code with session context: %s" id)
    ;; Open a terminal with claude
    (let ((default-directory dir))
      (if (fboundp 'vterm)
          (progn
            (vterm (format "*claude:%s*" (substring id 0 (min 12 (length id)))))
            (vterm-send-string cmd)
            (vterm-send-return))
        ;; Fallback to ansi-term
        (ansi-term "/bin/bash" (format "claude:%s" (substring id 0 (min 12 (length id)))))
        (comint-send-string nil (concat cmd "\n"))))))

(defun stack-resume--launch-fucodex (session)
  "Launch fucodex/Codex CLI to resume SESSION."
  (let* ((id (plist-get session :session/id))
         (context-file (stack-resume--session-context-file session))
         (dir (or stack-resume-working-dir default-directory))
         (cmd (format "%s --resume %s" stack-resume-fucodex-command context-file)))
    (message "Launching Codex with session context: %s" id)
    (let ((default-directory dir))
      (if (fboundp 'vterm)
          (progn
            (vterm (format "*codex:%s*" (substring id 0 (min 12 (length id)))))
            (vterm-send-string cmd)
            (vterm-send-return))
        (ansi-term "/bin/bash" (format "codex:%s" (substring id 0 (min 12 (length id)))))
        (comint-send-string nil (concat cmd "\n"))))))

(defun stack-resume--launch-aob (session)
  "Launch aob-chatgpt to resume SESSION."
  (let* ((id (plist-get session :session/id))
         (context-file (stack-resume--session-context-file session)))
    (message "AOB/ChatGPT resume: context written to %s" context-file)
    (message "Manual step: paste context into ChatGPT or use aob tool")
    ;; For now, just open the context file
    (find-file-other-window context-file)))

(defun stack-resume--detect-agent (session)
  "Detect the appropriate agent for SESSION."
  (let ((agent (format "%s" (or (plist-get session :session/agent)
                                (plist-get session :agent)
                                "fuclaude"))))
    (or (cdr (assoc agent stack-resume-agents))
        (cdr (assoc "fuclaude" stack-resume-agents)))))

;;; Interactive commands

;;;###autoload
(defun stack-resume-browse ()
  "Open the session browser."
  (interactive)
  (message "Loading sessions...")
  (stack-resume--fetch-sessions #'stack-resume--render-sessions))

;;;###autoload
(defun stack-resume-search (query)
  "Search sessions for QUERY."
  (interactive "sSearch sessions: ")
  (message "Searching for '%s'..." query)
  (stack-resume--search-sessions
   query
   (lambda (sessions)
     (stack-resume--render-sessions sessions query))))

(defun stack-resume-refresh ()
  "Refresh the session list."
  (interactive)
  (if stack-resume--current-query
      (stack-resume-search stack-resume--current-query)
    (stack-resume-browse)))

(defun stack-resume-view-at-point ()
  "View the session at point."
  (interactive)
  (if-let ((session (get-text-property (point) 'stack-resume-session)))
      (let ((id (plist-get session :id)))
        (message "Loading session %s..." id)
        (stack-resume--fetch-session id #'stack-resume--render-detail))
    (user-error "No session at point")))

(defun stack-resume-resume-at-point ()
  "Resume the session at point with its detected agent."
  (interactive)
  (let ((session (or (get-text-property (point) 'stack-resume-session)
                     stack-resume--session-at-point)))
    (if session
        (let* ((id (or (plist-get session :id) (plist-get session :session/id)))
               (launcher (stack-resume--detect-agent session)))
          ;; If we only have metadata, fetch the full session first
          (if (plist-get session :events)
              (funcall launcher session)
            (message "Loading full session %s..." id)
            (stack-resume--fetch-session id launcher)))
      (user-error "No session at point"))))

(defun stack-resume-resume-choose-agent ()
  "Resume the session at point, choosing the agent."
  (interactive)
  (let ((session (or (get-text-property (point) 'stack-resume-session)
                     stack-resume--session-at-point)))
    (if session
        (let* ((id (or (plist-get session :id) (plist-get session :session/id)))
               (agent-name (completing-read "Agent: " (mapcar #'car stack-resume-agents) nil t))
               (launcher (cdr (assoc agent-name stack-resume-agents))))
          (if (plist-get session :events)
              (funcall launcher session)
            (message "Loading full session %s..." id)
            (stack-resume--fetch-session id launcher)))
      (user-error "No session at point"))))

;;; Mode definition

(defvar stack-resume-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'stack-resume-view-at-point)
    (define-key map "r" #'stack-resume-resume-at-point)
    (define-key map "R" #'stack-resume-resume-choose-agent)
    (define-key map "s" #'stack-resume-search)
    (define-key map "/" #'stack-resume-search)
    (define-key map "g" #'stack-resume-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map)
  "Keymap for `stack-resume-mode'.")

(define-derived-mode stack-resume-mode special-mode "StackResume"
  "Major mode for browsing MUSN sessions.

\\{stack-resume-mode-map}")

;;; Integration with Stack HUD

(defun stack-resume-hud-widget ()
  "Return a widget string for the Stack HUD."
  (let ((count (length stack-resume--sessions)))
    (format "[Sessions: %d] " (if (> count 0) count "?"))))

;;;###autoload
(defun stack-resume-from-hud ()
  "Open session browser from Stack HUD."
  (interactive)
  (stack-resume-browse))

(provide 'stack-resume)
;;; stack-resume.el ends here
