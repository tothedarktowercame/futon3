;;; fubar.el --- Human agent interface for MUSN -*- lexical-binding: t; -*-

;;; Commentary:
;; fubar.el provides the same MUSN turn lifecycle as fucodex/fuclaude,
;; but for human agents working in Emacs.
;;
;; Turn lifecycle:
;;   1. fubar-session-create  - start a new session
;;   2. fubar-turn-start      - begin a turn with intent
;;   3. fubar-turn-select     - select a pattern to work with
;;   4. fubar-turn-action     - log actions (read, implement, update)
;;   5. fubar-turn-use        - claim pattern use with evidence
;;   6. fubar-evidence-add    - add evidence files
;;   7. fubar-turn-end        - complete the turn
;;
;; All operations POST to the MUSN service at `fubar-musn-url`.

;;; Code:

(require 'json)
(require 'url)
(require 'seq)
(require 'subr-x)

;;; Configuration

(defgroup fubar nil
  "Human agent interface for MUSN."
  :group 'tools
  :prefix "fubar-")

(defcustom fubar-musn-url "http://localhost:6065"
  "Base URL for the MUSN service."
  :type 'string
  :group 'fubar)

(defcustom fubar-client-id "fubar"
  "Client identifier sent with MUSN requests."
  :type 'string
  :group 'fubar)

(defcustom fubar-request-timeout 5
  "Timeout in seconds for MUSN HTTP requests."
  :type 'integer
  :group 'fubar)

(defcustom fubar-auto-show-ui t
  "When non-nil, raise MUSN viewer and HUD on new sessions/turns."
  :type 'boolean
  :group 'fubar)

(defcustom fubar-chat-room "lab"
  "Default MUSN chat room name."
  :type 'string
  :group 'fubar)

(defcustom fubar-chat-author-name nil
  "Optional display name for chat messages (defaults to `fubar-client-id`)."
  :type '(choice (const nil) string)
  :group 'fubar)

(defcustom fubar-chat-poll-interval 2
  "Seconds between chat room polls."
  :type 'number
  :group 'fubar)
(defcustom fubar-chat-auto-open-runner t
  "Auto-open the MUSN runner when a chat message announces a new session."
  :type 'boolean
  :group 'fubar)
(defcustom fubar-chat-auto-open-authors '("fucodex")
  "Chat author names that may trigger auto-opening the MUSN runner.
Nil means allow any author."
  :type '(repeat string)
  :group 'fubar)

;;; State

(defvar fubar--current-session nil
  "Current MUSN session plist with :session/id, :turn, etc.")

(defvar fubar--current-turn nil
  "Current turn number.")

(defvar fubar--selected-pattern nil
  "Currently selected pattern ID.")

(defvar fubar--candidates nil
  "Current candidate patterns from HUD.")

(defvar fubar--musn-help-pending t
  "Whether the MUSN help screen should be emitted on the next HUD request.")

(defvar fubar--session-registry (make-hash-table :test 'equal)
  "Registry of known sessions keyed by session id.")

(defvar fubar--session-order nil
  "List of session ids in most-recent-first order.")

(defvar fubar--active-session nil
  "Session id currently bound to the HUD.")

(defvar fubar--log-buffer-name "*fubar-log*"
  "Buffer name for fubar event logs.")

(defvar fubar--last-aif-line nil
  "Most recent formatted AIF line.")

(defvar fubar--modeline-string nil
  "Modeline string showing the latest AIF signal.")

(defvar fubar--last-hud nil
  "Most recent HUD map produced during a turn.")

(defvar fubar-chat--room nil
  "Active chat room name.")

(defvar fubar-chat--last-cursor 0
  "Last chat cursor seen for polling.")

(defvar fubar-chat--poll-timer nil
  "Timer for chat polling.")

(defvar fubar-chat--last-state nil
  "Last chat room state payload.")

(defvar fubar-chat--buffer-name "*fubar-chat*"
  "Buffer name for fubar chat room.")
(defvar fubar-chat--last-auto-session nil
  "Last MUSN session auto-opened from chat.")

;;; HUD sync (optional, when fubar-viewer/hud is available)

(defun fubar--sync-hud ()
  "Sync current fubar session into the HUD/state poll when available."
  (when-let ((session fubar--current-session))
    (let ((sid (plist-get session :session/id)))
      (when (and sid (boundp 'fubar-musn-session-id))
        (setq fubar-musn-session-id sid))
      (when (get-buffer "*FuLab HUD*")
        (with-current-buffer "*FuLab HUD*"
          (when (eq fubar-hud--intent-source :agent)
            (setq fubar-hud--intent-source nil)
            (setq fubar-hud--intent nil))))
      (when (and sid (fboundp 'fubar-hud-set-session-id))
        (fubar-hud-set-session-id sid))
      (when (fboundp 'fubar-musn--fetch-state)
        (fubar-musn--fetch-state))
      (when (fboundp 'fubar-musn--maybe-start-state-poll)
        (fubar-musn--maybe-start-state-poll)))))

(defun fubar--maybe-show-ui (intent)
  "Raise MUSN viewer and HUD when configured."
  (when (and fubar-auto-show-ui (fboundp 'fubar-musn-view-2up))
    (let ((log (and (boundp 'fubar-musn--default-log) fubar-musn--default-log)))
      (fubar-musn-view-2up log intent))))

;;; Modeline

(define-minor-mode fubar-modeline-mode
  "Show latest fubar AIF signal in the global modeline."
  :global t
  :group 'fubar
  (if fubar-modeline-mode
      (progn
        (unless (member 'fubar--modeline-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(fubar--modeline-string))))
        (setq fubar--modeline-string (or fubar--last-aif-line " fubar:aif=none")))
    (setq global-mode-string
          (delq 'fubar--modeline-string global-mode-string))
    (setq fubar--modeline-string nil)))

;;; Logging

(defun fubar--log-line (line)
  "Append LINE to the fubar log buffer."
  (let ((buf (get-buffer-create fubar--log-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert line "\n")))
  (when (boundp 'fubar-musn--default-log)
    (let ((log fubar-musn--default-log))
      (when (and (stringp log) (file-writable-p log))
        (with-temp-buffer
          (insert line "\n")
          (write-region (point-min) (point-max) log t 'silent))))))

(defun fubar--maybe-aif-map (value)
  (when (and (listp value)
             (or (plist-get value :tau)
                 (plist-get value :tau-updated)
                 (plist-get value :prediction-error)
                 (plist-get value :G)
                 (plist-get value :G-chosen)))
    value))

(defun fubar--extract-aif (resp)
  "Extract an AIF map from RESP when present."
  (let ((candidates (list (plist-get resp :aif)
                          (plist-get (plist-get resp :psr) :aif)
                          (plist-get (plist-get resp :pur) :aif)
                          (plist-get (plist-get resp :selection/reason) :aif)
                          (plist-get (plist-get resp :reason) :aif))))
    (seq-find #'fubar--maybe-aif-map candidates)))

(defun fubar--format-aif-line (event aif)
  (let* ((g (or (plist-get aif :G) (plist-get aif :G-chosen)))
         (tau (or (plist-get aif :tau) (plist-get aif :tau-updated)))
         (err (plist-get aif :prediction-error))
         (delta (plist-get aif :evidence-delta))
         (parts (delq nil (list (when g (format "G=%.3f" (float g)))
                                (when tau (format "tau=%.3f" (float tau)))
                                (when err (format "err=%.3f" (float err)))
                                (when delta (format "d=%.3f" (float delta)))))))
    (when (seq parts)
      (format "[aif] %s %s" event (string-join parts " ")))))

(defun fubar--note-response (event resp)
  "Log AIF signals from RESP for EVENT and update modeline."
  (when resp
    (when-let ((aif (fubar--extract-aif resp)))
      (when-let ((line (fubar--format-aif-line event aif)))
        (setq fubar--last-aif-line (concat " " line))
        (setq fubar--modeline-string fubar--last-aif-line)
        (fubar--log-line (format "%s %s" (fubar--now-iso) line))))))

(defun fubar--log-event (event resp)
  "Log a fubar EVENT to the log buffer (and viewer log file)."
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (line (format "%s [fubar] event=%s session=%s turn=%s"
                       (fubar--now-iso)
                       event
                       (or sid "unknown")
                       (or turn 0))))
    (fubar--log-line line)))

;;; Utilities

(defun fubar--now-iso ()
  "Return current time as ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))

(defun fubar-register-session (session-id &optional type intent interactive)
  "Register SESSION-ID with optional TYPE and INTENT.
If INTERACTIVE is non-nil, bind the HUD to this session."
  (let* ((sid (and session-id (stringp session-id) (string-trim session-id)))
         (now (fubar--now-iso)))
    (when (and sid (not (string-empty-p sid)))
      (let ((entry (list :id sid
                         :type (or type "unknown")
                         :intent intent
                         :updated-at now)))
        (puthash sid entry fubar--session-registry)
        (setq fubar--session-order (cons sid (delete sid fubar--session-order)))
        (when interactive
          (setq fubar--active-session sid)
          (when (fboundp 'fubar-hud-set-session-id)
            (fubar-hud-set-session-id sid))
          (when (and intent (fboundp 'fubar-hud-set-intent))
            (fubar-hud-set-intent intent)))))))

(defun fubar-active-session ()
  "Return the currently active HUD session id."
  fubar--active-session)

(defun fubar--uuid ()
  "Generate a simple UUID."
  (let* ((seed (format "%s-%s-%s" (float-time) (random) (emacs-pid)))
         (hex (md5 seed)))
    (format "%s-%s-%s-%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 12 16)
            (substring hex 16 20)
            (substring hex 20 32))))

(defun fubar--repo-root ()
  "Return the current repository root."
  (or (locate-dominating-file default-directory ".git")
      default-directory))

(defun fubar--git-head ()
  "Return current git HEAD commit."
  (let ((default-directory (fubar--repo-root)))
    (string-trim
     (shell-command-to-string "git rev-parse HEAD 2>/dev/null"))))

(defun fubar--build-certificates ()
  "Build certificate list for MUSN."
  (let ((commit (fubar--git-head))
        (root (fubar--repo-root)))
    (when (and commit (not (string-empty-p commit)))
      (list `((certificate/type . "git/commit")
              (certificate/ref . ,commit)
              (certificate/repo . ,root))))))

(defun fubar--plist->alist (plist)
  "Convert PLIST to an alist for json-encode."
  (let (out)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (setq out (cons (cons key val) out))))
    (nreverse out)))

;;; HTTP

(defun fubar--post (path payload)
  "POST PAYLOAD to MUSN at PATH, return parsed response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Accept" . "application/json")
            ("X-Musn-Client" . ,fubar-client-id)
            ("User-Agent" . ,(format "fubar/%s" fubar-client-id))))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (url (concat (string-trim-right fubar-musn-url "/") path))
         (buffer (url-retrieve-synchronously url t t fubar-request-timeout)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((json-object-type 'plist)
                    (json-array-type 'vector)
                    (json-key-type 'keyword))
                (condition-case nil
                    (json-read)
                  (error nil)))))
        (kill-buffer buffer)))))

;;; MUSN API

(defun fubar-session-create (&optional session-id)
  "Create a new MUSN session, optionally with SESSION-ID."
  (interactive "sSession ID (blank for auto): ")
  (let* ((certs (fubar--build-certificates))
         (payload `((client . ((id . ,fubar-client-id)))
                    ,@(when (and session-id (not (string-empty-p session-id)))
                        `((session/id . ,session-id)))
                    ,@(when certs
                        `((policy . ((certificates . ,(vconcat certs)))))))))
    (let ((resp (fubar--post "/musn/session/create" payload)))
      (setq fubar--current-session resp)
      (setq fubar--current-turn 0)
      (setq fubar--selected-pattern nil)
      (setq fubar--musn-help-pending t)
      (fubar-register-session (plist-get resp :session/id) "fubar" nil t)
      (fubar--maybe-show-ui nil)
      (fubar--sync-hud)
      (fubar--log-event "session/create" resp)
      (fubar--note-response "session/create" resp)
      (message "[fubar] Session created: %s" (plist-get resp :session/id))
      resp)))

(defun fubar-turn-start (intent)
  "Start a new turn with INTENT."
  (interactive "sIntent: ")
  (require 'fubar-hud nil t)
  (unless fubar--current-session
    (fubar-session-create))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn (1+ (or fubar--current-turn 0)))
         (hud-map (when (fboundp 'fubar-hud--build-hud)
                    (fubar-hud--build-hud intent)))
         (hud-map (if (and hud-map (not (plist-get hud-map :intent)))
                    (plist-put (copy-sequence hud-map) :intent intent)
                    hud-map))
         (hud (if hud-map
                (fubar--plist->alist hud-map)
                `((intent . ,intent))))
         (hud (if fubar--musn-help-pending
                (append hud '((musn-help . t)))
                hud))
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (hud . ,hud))))
    (let ((resp (fubar--post "/musn/turn/start" payload)))
      (setq fubar--current-turn turn)
      (setq fubar--selected-pattern nil)
      (setq fubar--musn-help-pending nil)
      (setq fubar--last-hud hud-map)
      (fubar-register-session sid "fubar" intent (called-interactively-p 'interactive))
      ;; Extract candidates from HUD response.
      (when-let ((hud (plist-get resp :hud)))
        (setq fubar--candidates (plist-get hud :candidates)))
      (unless fubar--candidates
        (setq fubar--candidates (plist-get hud-map :candidates)))
      (when (and hud-map (fboundp 'fubar-hud-apply-hud-state))
        (fubar-hud-apply-hud-state hud-map))
      (when (fboundp 'fubar-hud-set-intent)
        (fubar-hud-set-intent intent))
      (when (fboundp 'fubar-hud-refresh)
        (fubar-hud-refresh))
      (fubar--maybe-show-ui intent)
      (fubar--sync-hud)
      (fubar--log-event "turn/start" resp)
      (fubar--note-response "turn/start" resp)
      (message "[fubar] Turn %d started" turn)
      resp)))

(defun fubar-turn-select (pattern-id &optional reason)
  "Select PATTERN-ID for the current turn with optional REASON."
  (interactive
   (list (completing-read "Pattern: "
                          (mapcar (lambda (c) (plist-get c :id)) fubar--candidates))
         (read-string "Reason: ")))
  (unless fubar--current-session
    (user-error "No active session. Run fubar-session-create first"))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (candidates (if fubar--candidates
                         (vconcat (mapcar (lambda (c) (plist-get c :id)) fubar--candidates))
                       (vector pattern-id)))
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (candidates . ,candidates)
                    (chosen . ,pattern-id)
                    (reason . ((mode . "use")
                               ,@(when (and reason (not (string-empty-p reason)))
                                   `((note . ,reason))))))))
    (let ((resp (fubar--post "/musn/turn/select" payload)))
      (setq fubar--selected-pattern pattern-id)
      (fubar--log-event "turn/select" resp)
      (fubar--note-response "turn/select" resp)
      (message "[fubar] Selected: %s" pattern-id)
      resp)))

(defun fubar-turn-action (action &optional note files)
  "Log ACTION on current pattern with optional NOTE and FILES."
  (interactive
   (list (completing-read "Action: " '("read" "implement" "update" "discover"))
         (read-string "Note: ")))
  (unless fubar--selected-pattern
    (user-error "No pattern selected. Run fubar-turn-select first"))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (pattern/id . ,fubar--selected-pattern)
                    (action . ,action)
                    ,@(when (and note (not (string-empty-p note)))
                        `((note . ,note)))
                    ,@(when files
                        `((files . ,(vconcat files)))))))
    (let ((resp (fubar--post "/musn/turn/action" payload)))
      (fubar--log-event "turn/action" resp)
      (fubar--note-response "turn/action" resp)
      (message "[fubar] Action logged: %s on %s" action fubar--selected-pattern)
      resp)))

(defun fubar-log-pattern-read (pattern-id &optional note)
  "Log a read action for PATTERN-ID without changing selection."
  (unless fubar--current-session
    (user-error "No active session. Run fubar-session-create first"))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (pattern/id . ,pattern-id)
                    (action . "read")
                    ,@(when (and note (not (string-empty-p note)))
                        `((note . ,note))))))
    (let ((resp (fubar--post "/musn/turn/action" payload)))
      (fubar--log-event "turn/action" resp)
      (fubar--note-response "turn/action" resp)
      (message "[fubar] Read logged: %s" pattern-id)
      resp)))

(defun fubar-turn-use (&optional note)
  "Claim use of current pattern with optional NOTE."
  (interactive "sNote: ")
  (unless fubar--selected-pattern
    (user-error "No pattern selected. Run fubar-turn-select first"))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (pattern/id . ,fubar--selected-pattern)
                    ,@(when (and note (not (string-empty-p note)))
                        `((note . ,note))))))
    (let ((resp (fubar--post "/musn/turn/use" payload)))
      (fubar--log-event "turn/use" resp)
      (fubar--note-response "turn/use" resp)
      (message "[fubar] Use claimed: %s" fubar--selected-pattern)
      resp)))

(defun fubar-evidence-add (files &optional note)
  "Add FILES as evidence for current pattern with optional NOTE."
  (interactive
   (list (list (read-file-name "Evidence file: "))
         (read-string "Note: ")))
  (unless fubar--selected-pattern
    (user-error "No pattern selected. Run fubar-turn-select first"))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (pattern/id . ,fubar--selected-pattern)
                    (files . ,(vconcat files))
                    ,@(when (and note (not (string-empty-p note)))
                        `((note . ,note))))))
    (let ((resp (fubar--post "/musn/evidence/add" payload)))
      (fubar--log-event "evidence/add" resp)
      (fubar--note-response "evidence/add" resp)
      (message "[fubar] Evidence added: %s" (car files))
      resp)))

(defun fubar-turn-end (&optional summary)
  "End current turn with optional SUMMARY."
  (interactive "sSummary: ")
  (unless fubar--current-session
    (user-error "No active session"))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn fubar--current-turn)
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    ,@(when (and summary (not (string-empty-p summary)))
                        `((summary . ((note . ,summary))))))))
    (let ((resp (fubar--post "/musn/turn/end" payload)))
      (setq fubar--selected-pattern nil)
      (fubar--log-event "turn/end" resp)
      (fubar--note-response "turn/end" resp)
      (message "[fubar] Turn %d ended" turn)
      resp)))

(defun fubar-musn-help ()
  "Request the MUSN help screen on the next HUD render."
  (interactive)
  (setq fubar--musn-help-pending t)
  (when (fboundp 'fubar-hud-refresh)
    (fubar-hud-refresh))
  (message "[fubar] MUSN help will be shown on the next HUD render."))

;;; Convenience commands

(defun fubar-quick-use (pattern-id intent)
  "Quick workflow: start turn, select PATTERN-ID, claim use with INTENT."
  (interactive "sPattern ID: \nsIntent: ")
  (fubar-turn-start intent)
  (fubar-turn-select pattern-id intent)
  (fubar-turn-action "implement" intent)
  (fubar-turn-use intent)
  (fubar-turn-end intent))

(defun fubar-status ()
  "Show current fubar session status."
  (interactive)
  (if fubar--current-session
      (message "[fubar] Session: %s, Turn: %d, Pattern: %s"
               (plist-get fubar--current-session :session/id)
               (or fubar--current-turn 0)
               (or fubar--selected-pattern "none"))
    (message "[fubar] No active session")))

(defun fubar-reset ()
  "Reset fubar state."
  (interactive)
  (setq fubar--current-session nil
        fubar--current-turn nil
        fubar--selected-pattern nil
        fubar--candidates nil)
  (setq fubar--last-aif-line nil
        fubar--modeline-string (if fubar-modeline-mode " fubar:aif=none" nil))
  (message "[fubar] State reset"))

;;; Chat rooms (MUSN pause-latch + bids)

(defvar fubar-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'fubar-chat-refresh)
    (define-key map (kbd "s") #'fubar-chat-send)
    (define-key map (kbd "b") #'fubar-chat-bid)
    (define-key map (kbd "u") #'fubar-chat-unlatch)
    (define-key map (kbd "q") #'fubar-chat-leave)
    map)
  "Keymap for `fubar-chat-mode'.")

(define-derived-mode fubar-chat-mode special-mode "FuBar-Chat"
  "Major mode for MUSN chat rooms."
  (setq truncate-lines t))

(defun fubar-chat--room ()
  (or fubar-chat--room fubar-chat-room))

(defun fubar-chat--author ()
  (let ((name (or fubar-chat-author-name fubar-client-id)))
    (if (and name (not (string-empty-p name)))
        `((id . ,fubar-client-id) (name . ,name))
      `((id . ,fubar-client-id)))))

(defun fubar-chat--buffer ()
  (let ((buf (get-buffer-create fubar-chat--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'fubar-chat-mode)
        (fubar-chat-mode)))
    buf))

(defun fubar-chat--append-line (line)
  (with-current-buffer (fubar-chat--buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert line "\n"))))

(defun fubar-chat--event-type (event)
  (let ((etype (plist-get event :event/type)))
    (cond
     ((keywordp etype) (symbol-name etype))
     ((symbolp etype) (symbol-name etype))
     (t (format "%s" etype)))))

(defun fubar-chat--format-event (event)
  (let* ((etype (fubar-chat--event-type event))
         (payload (plist-get event :payload))
         (at (or (plist-get event :at) (plist-get payload :at)))
         (at (if (stringp at) at (format "%s" at))))
    (cond
     ((string= etype "chat/message")
      (let* ((author (plist-get payload :author))
             (name (or (plist-get author :name) (plist-get author :id) "anon"))
             (text (plist-get payload :text)))
        (format "[%s] %s: %s (latch)" at name text)))
     ((string= etype "chat/bid")
      (let* ((bidder (plist-get payload :bidder))
             (name (or (plist-get bidder :name) (plist-get bidder :id) "anon"))
             (note (or (plist-get payload :note) ""))
             (bid-id (plist-get payload :bid/id)))
        (format "[%s] bid %s by %s %s" at (or bid-id "?") name note)))
     ((string= etype "chat/unlatch")
      (let* ((accepted (plist-get payload :accepted))
             (bid-id (plist-get accepted :bid/id))
             (by (plist-get payload :by))
             (name (or (plist-get by :name) (plist-get by :id) "chair")))
        (format "[%s] unlatch by %s (accepted %s)" at name (or bid-id "?"))))
     (t
     (format "[%s] %s" at etype)))))

(defun fubar-chat--extract-session-id (text)
  (when (and text (stringp text))
    (when (string-match "Starting \\(musn-[A-Za-z0-9._-]+\\)\\b" text)
      (match-string 1 text))))

(defun fubar-chat--author-allowed-p (name)
  (or (null fubar-chat-auto-open-authors)
      (member (downcase (or name ""))
              (mapcar #'downcase fubar-chat-auto-open-authors))))

(defun fubar-chat--maybe-auto-open (event)
  (when (and fubar-chat-auto-open-runner
             (string= (fubar-chat--event-type event) "chat/message"))
    (let* ((payload (plist-get event :payload))
           (author (plist-get payload :author))
           (name (or (plist-get author :name) (plist-get author :id) "anon"))
           (text (plist-get payload :text))
           (sid (fubar-chat--extract-session-id text)))
      (when (and sid
                 (not (string-empty-p sid))
                 (not (equal sid fubar-chat--last-auto-session))
                 (fubar-chat--author-allowed-p name))
        (setq fubar-chat--last-auto-session sid)
        (require 'fubar-viewer nil t)
        (when (fboundp 'fubar-musn-view-session)
          (fubar-musn-view-session sid))))))

(defun fubar-chat--apply-events (events)
  (when (vectorp events)
    (dotimes (idx (length events))
      (let ((event (aref events idx)))
        (fubar-chat--append-line (fubar-chat--format-event event))
        (fubar-chat--maybe-auto-open event)))))

(defun fubar-chat-refresh ()
  "Poll the current chat room for new events."
  (interactive)
  (let* ((room (fubar-chat--room))
         (payload `((room . ,room)
                    ,@(when (> fubar-chat--last-cursor 0)
                        `((since . ,fubar-chat--last-cursor)))))
         (resp (fubar--post "/musn/chat/state" payload)))
    (when (plist-get resp :ok)
      (setq fubar-chat--last-state (plist-get resp :state))
      (setq fubar-chat--last-cursor (or (plist-get resp :cursor) fubar-chat--last-cursor))
      (fubar-chat--apply-events (plist-get resp :events)))))

(defun fubar-chat-join (&optional room)
  "Join a MUSN chat ROOM (default `fubar-chat-room')."
  (interactive "sRoom: ")
  (setq fubar-chat--room (if (and room (not (string-empty-p room)))
                             room
                           fubar-chat-room))
  (setq fubar-chat--last-cursor 0)
  (setq fubar-chat--last-state nil)
  (display-buffer (fubar-chat--buffer))
  (fubar-chat--append-line (format "== Joined room %s ==" fubar-chat--room))
  (fubar-chat-refresh)
  (fubar-chat-start-poll))

(defun fubar-chat-leave ()
  "Leave the current MUSN chat room and stop polling."
  (interactive)
  (fubar-chat-stop-poll)
  (setq fubar-chat--room nil)
  (message "[fubar] Chat polling stopped"))

(defun fubar-chat-start-poll ()
  "Start polling the chat room."
  (when fubar-chat--poll-timer
    (cancel-timer fubar-chat--poll-timer))
  (setq fubar-chat--poll-timer
        (run-at-time 0 fubar-chat-poll-interval #'fubar-chat-refresh)))

(defun fubar-chat-stop-poll ()
  "Stop polling the chat room."
  (when fubar-chat--poll-timer
    (cancel-timer fubar-chat--poll-timer)
    (setq fubar-chat--poll-timer nil)))

(defun fubar-chat-send (text)
  "Send TEXT to the current chat room."
  (interactive "sMessage: ")
  (let* ((room (fubar-chat--room))
         (payload `((room . ,room)
                    (msg-id . ,(fubar--uuid))
                    (author . ,(fubar-chat--author))
                    (text . ,text)))
         (resp (fubar--post "/musn/chat/message" payload)))
    (if (plist-get resp :ok)
        (fubar-chat-refresh)
      (message "[fubar] Chat send failed"))))

(defun fubar-chat-bid (&optional note)
  "Raise a bid (hand) to unlatch the room."
  (interactive "sBid note: ")
  (let* ((room (fubar-chat--room))
         (payload `((room . ,room)
                    (msg-id . ,(fubar--uuid))
                    (bidder . ,(fubar-chat--author))
                    ,@(when (and note (not (string-empty-p note)))
                        `((note . ,note)))))
         (resp (fubar--post "/musn/chat/bid" payload)))
    (if (plist-get resp :ok)
        (fubar-chat-refresh)
      (message "[fubar] Chat bid failed"))))

(defun fubar-chat--pending-bids ()
  (let* ((state fubar-chat--last-state)
         (bids (plist-get state :bids))
         (out nil))
    (when (vectorp bids)
      (dotimes (idx (length bids))
        (let* ((bid (aref bids idx))
               (status (plist-get bid :status))
               (pending (or (eq status :pending)
                            (string= (format "%s" status) "pending")))
               (bid-id (plist-get bid :bid/id)))
          (when (and pending bid-id)
            (push bid-id out)))))
    (nreverse out)))

(defun fubar-chat-unlatch (&optional bid-id)
  "Unlatch the room by accepting BID-ID."
  (interactive)
  (let* ((bids (fubar-chat--pending-bids))
         (bid-id (or bid-id
                     (when bids
                       (completing-read "Accept bid: " bids nil t)))))
    (if (or (null bid-id) (string-empty-p bid-id))
        (message "[fubar] No pending bids")
      (let* ((room (fubar-chat--room))
             (payload `((room . ,room)
                        (msg-id . ,(fubar--uuid))
                        (bid/id . ,bid-id)
                        (by . ,(fubar-chat--author))))
             (resp (fubar--post "/musn/chat/unlatch" payload)))
        (if (plist-get resp :ok)
            (fubar-chat-refresh)
          (message "[fubar] Chat unlatch failed"))))))

;;; Launch fucodex into chat

(defun fubar-chat-launch-fucodex (prompt &optional intent room)
  "Launch fucodex MUSN run with PROMPT and join the chat ROOM."
  (interactive "sPrompt: ")
  (require 'fubar-viewer)
  (let* ((room (or (and room (not (string-empty-p room)) room) fubar-chat-room))
         (author (or fubar-chat-author-name fubar-client-id)))
    (when room
      (fubar-chat-join room))
    (fubar-musn-launch-and-view prompt intent room author)))

;;; Integration with other modes

(defun fubar-log-file-action (action)
  "Log ACTION on current buffer's file."
  (when (and fubar--selected-pattern buffer-file-name)
    (fubar-turn-action action nil (list buffer-file-name))))

(defun fubar-after-save-hook ()
  "Hook to log file saves as evidence."
  (when (and fubar--selected-pattern buffer-file-name)
    (fubar-evidence-add (list buffer-file-name) "saved")))

;; Optional: auto-log saves as evidence
;; (add-hook 'after-save-hook #'fubar-after-save-hook)

;;; Kill fuclaude/fucodex processes

(defun fubar-kill-fuclaude ()
  "Kill any running fuclaude/fucodex interactive sessions.
Useful when C-c doesn't propagate through EAT or other terminal emulators."
  (interactive)
  (let ((killed 0))
    ;; Kill claude --resume processes (interactive sessions)
    (dolist (line (process-lines "pgrep" "-f" "claude.*--resume"))
      (when (string-match "\\([0-9]+\\)" line)
        (let ((pid (match-string 1 line)))
          (call-process "kill" nil nil nil pid)
          (setq killed (1+ killed)))))
    ;; Kill fuclaude wrapper processes
    (dolist (line (process-lines "pgrep" "-f" "fuclaude --cli"))
      (when (string-match "\\([0-9]+\\)" line)
        (let ((pid (match-string 1 line)))
          (call-process "kill" nil nil nil pid)
          (setq killed (1+ killed)))))
    ;; Kill fucodex wrapper processes
    (dolist (line (process-lines "pgrep" "-f" "fucodex --cli"))
      (when (string-match "\\([0-9]+\\)" line)
        (let ((pid (match-string 1 line)))
          (call-process "kill" nil nil nil pid)
          (setq killed (1+ killed)))))
    (if (> killed 0)
        (message "[fubar] Killed %d process(es)" killed)
      (message "[fubar] No fuclaude/fucodex processes found"))))

(defalias 'eat-kill-fuclaude 'fubar-kill-fuclaude
  "Alias for `fubar-kill-fuclaude' for discoverability.")

;;; PAR (Post-Action Review) integration

(defun fubar-par-buffers ()
  "Return a list of active PAR buffers (buffers matching *PAR: *)."
  (seq-filter
   (lambda (buf)
     (string-prefix-p "*PAR: " (buffer-name buf)))
   (buffer-list)))

(defun fubar-par-jump ()
  "Jump to an active PAR buffer, prompting if multiple exist."
  (interactive)
  (let ((par-bufs (fubar-par-buffers)))
    (cond
     ((null par-bufs)
      (message "[fubar] No active PAR buffers"))
     ((= 1 (length par-bufs))
      (pop-to-buffer (car par-bufs)))
     (t
      (let* ((names (mapcar #'buffer-name par-bufs))
             (chosen (completing-read "PAR buffer: " names nil t)))
        (pop-to-buffer chosen))))))

(defcustom fubar-futon3-root "~/code/futon3"
  "Root directory of the futon3 repository."
  :type 'directory
  :group 'fubar)

(defun fubar-par-bell (title &optional agents)
  "Ring the PAR bell for TITLE, summoning AGENTS.
If AGENTS is nil, queries Agency for all connected agents.
This calls the par-bell.sh script to create a collaborative PAR session."
  (interactive "sPAR title: ")
  (let ((script (expand-file-name "scripts/par-bell.sh" fubar-futon3-root)))
    (if (file-executable-p script)
        (async-shell-command
         (if agents
             (format "%s --title %s --agents %s"
                     (shell-quote-argument script)
                     (shell-quote-argument title)
                     (shell-quote-argument agents))
           (format "%s --title %s"
                   (shell-quote-argument script)
                   (shell-quote-argument title)))
         "*PAR Bell*")
      (message "[fubar] par-bell.sh not found"))))

;;; Test Bell - simple ACK from agents

(defcustom fubar-agency-url "http://localhost:7070"
  "Base URL for the Agency service."
  :type 'string
  :group 'fubar)

(defcustom fubar-test-bell-agents '("fucodex" "fuclaude")
  "Fallback agents to ping when no connected agents are discovered."
  :type '(repeat string)
  :group 'fubar)

(defcustom fubar-agency-global-urls '("http://localhost:7070")
  "Agency base URLs to use for global test bells."
  :type '(repeat string)
  :group 'fubar)

(defvar fubar-test-bell--pending nil
  "Alist of (agent . status) for pending test-bell requests.")

(defvar fubar-test-bell--buffer "*Test Bell*"
  "Buffer for test-bell results.")

(defun fubar-test-bell--post-async (agent callback)
  "POST ACK request to AGENT via Agency, call CALLBACK with (agent . result)."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (payload `((agent-id . ,agent)
                    (peripheral . "chat")
                    (prompt . "ACK? Reply with just 'ACK' to confirm you're connected.")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (url (concat (string-trim-right fubar-agency-url "/") "/agency/run")))
    (url-retrieve
     url
     (lambda (status agent callback)
       (let ((result
              (if (plist-get status :error)
                  (cons 'error (format "%s" (plist-get status :error)))
                (goto-char (point-min))
                (if (re-search-forward "\n\n" nil t)
                    (condition-case err
                        (let* ((json-object-type 'plist)
                               (resp (json-read)))
                          (if (plist-get resp :ok)
                              (cons 'ok (or (plist-get resp :response)
                                            (plist-get resp :result)
                                            "ACK"))
                            (cons 'error (or (plist-get resp :error) "unknown"))))
                      (error (cons 'error (format "parse: %s" err))))
                  (cons 'error "no response body")))))
         (kill-buffer (current-buffer))  ; clean up url buffer
         (funcall callback agent result)))
     (list agent callback)
     t t)))

(defun fubar-test-bell--update-buffer-safe ()
  "Schedule buffer update on main event loop (safe from async callbacks)."
  (run-at-time 0 nil #'fubar-test-bell--update-buffer))

(defun fubar-test-bell--update-buffer ()
  "Update the test-bell buffer with current status."
  (with-current-buffer (get-buffer-create fubar-test-bell--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "=== Test Bell ===\n\n")
      (dolist (entry fubar-test-bell--pending)
        (let ((agent (car entry))
              (status (cdr entry)))
          (insert (format "  %-12s  " agent))
          (cond
           ((eq status 'pending)
            (insert "⏳ waiting...\n"))
           ((and (consp status) (eq (car status) 'verified))
            (insert (propertize "✓ VERIFIED\n" 'face 'success)))
           ((and (consp status) (eq (car status) 'mismatch))
            (insert (propertize (format "⚠ MISMATCH (%s)\n" (cdr status))
                                'face 'warning)))
           ((and (consp status) (eq (car status) 'ok))
            (insert (format "✓ %s\n" (cdr status))))
           ((and (consp status) (eq (car status) 'error))
            (insert (propertize (format "✗ %s\n" (cdr status))
                                'face 'error)))
           (t
            (insert (format "? %s\n" status))))))
      (insert "\n")
      (let ((done (seq-count (lambda (e) (not (eq (cdr e) 'pending)))
                             fubar-test-bell--pending))
            (total (length fubar-test-bell--pending)))
        (insert (format "Progress: %d/%d\n" done total))
        (when (= done total)
          (insert "\nDone."))))))

(defun fubar-test-bell (&optional agents)
  "Ring a test bell to get ACK from AGENTS (default: `fubar-test-bell-agents').
Displays results in *Test Bell* buffer as responses arrive."
  (interactive)
  (let ((agents (or agents fubar-test-bell-agents)))
    (setq fubar-test-bell--pending
          (mapcar (lambda (a) (cons a 'pending)) agents))
    (pop-to-buffer (get-buffer-create fubar-test-bell--buffer))
    (fubar-test-bell--update-buffer)
    (dolist (agent agents)
      (fubar-test-bell--post-async
       agent
       (lambda (agent result)
         (setf (alist-get agent fubar-test-bell--pending nil nil #'equal) result)
         (fubar-test-bell--update-buffer-safe))))))

;;; Test Bell V2 - with secret verification

(defvar fubar-test-bell-v2--pending nil
  "Alist of (agent . (secret-id secret-value status)) for v2 test-bell.")

(defvar fubar-test-bell-v2--buffer "*Test Bell V2*"
  "Buffer for v2 test-bell results.")

(defun fubar-test-bell-v2--create-secret (callback &optional base-url)
  "Create a secret via Agency, call CALLBACK with (secret-id . secret-value)."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode '((ttl-ms . 60000))) 'utf-8))
         (agency-base (string-trim-right (or base-url fubar-agency-url) "/"))
         (url (concat agency-base "/agency/secret")))
    (url-retrieve
     url
     (lambda (status callback)
       (let ((result
              (if (plist-get status :error)
                  nil
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (condition-case nil
                      (let* ((json-object-type 'plist)
                             (resp (json-read)))
                        (when (plist-get resp :ok)
                          (cons (plist-get resp :secret-id)
                                (plist-get resp :value))))
                    (error nil))))))
         (kill-buffer (current-buffer))
         (funcall callback result)))
     (list callback)
     t t)))

(defun fubar-test-bell-v2--summon-agent (agent secret-id secret-value callback &optional base-url)
  "Summon AGENT with SECRET-ID, expecting them to return SECRET-VALUE."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (agency-base (string-trim-right (or base-url fubar-agency-url) "/"))
         (payload `((agent-id . ,agent)
                    (peripheral . "test-bell-ack")
                    (prompt . ,(format "Test bell at %s. Secret ID: %s

Run this command to fetch the secret:
  curl -s %s/agency/secret/%s

Then respond with ONLY the secret value from the JSON response."
                                       timestamp secret-id agency-base secret-id))))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (url (concat agency-base "/agency/run")))
    (url-retrieve
     url
     (lambda (status agent secret-value callback)
       (let ((result
              (if (plist-get status :error)
                  (cons 'error (format "%s" (plist-get status :error)))
                (goto-char (point-min))
                (if (re-search-forward "\n\n" nil t)
                    (condition-case err
                        (let* ((json-object-type 'plist)
                               (resp (json-read))
                               (response (or (plist-get resp :response) "")))
                          (if (plist-get resp :ok)
                              (if (string-match-p (regexp-quote secret-value) response)
                                  (cons 'verified response)
                                (cons 'mismatch response))
                            (cons 'error (or (plist-get resp :error) "unknown"))))
                      (error (cons 'error (format "parse: %s" err))))
                  (cons 'error "no response body")))))
         (kill-buffer (current-buffer))
         (funcall callback agent result)))
     (list agent secret-value callback)
     t t)))

(defun fubar-test-bell-v2--update-buffer ()
  "Update the v2 test-bell buffer."
  (with-current-buffer (get-buffer-create fubar-test-bell-v2--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "=== Test Bell V2 (Secret Verification) ===\n\n")
      (dolist (entry fubar-test-bell-v2--pending)
        (let* ((agent (car entry))
               (data (cdr entry))
               (secret-id (nth 0 data))
               (secret-value (nth 1 data))
               (status (nth 2 data)))
          (insert (format "  %-12s  [%s]\n" agent (or secret-id "no-secret")))
          (insert (format "                Expected: %s\n" (or secret-value "?")))
          (cond
           ((eq status 'pending)
            (insert "                Status: ⏳ waiting...\n"))
           ((and (consp status) (eq (car status) 'verified))
            (insert (format "                Status: ✓ VERIFIED - %s\n"
                            (truncate-string-to-width (cdr status) 40))))
           ((and (consp status) (eq (car status) 'mismatch))
            (insert (format "                Status: ⚠ MISMATCH - got: %s\n"
                            (truncate-string-to-width (cdr status) 40))))
           ((and (consp status) (eq (car status) 'error))
            (insert (format "                Status: ✗ ERROR - %s\n" (cdr status))))
           (t
            (insert (format "                Status: ? %s\n" status))))
          (insert "\n")))
      (let ((verified (seq-count (lambda (e)
                                   (and (consp (nth 2 (cdr e)))
                                        (eq (car (nth 2 (cdr e))) 'verified)))
                                 fubar-test-bell-v2--pending))
            (total (length fubar-test-bell-v2--pending)))
        (insert (format "Verified: %d/%d\n" verified total))))))

(defun fubar-test-bell-v2--update-buffer-safe ()
  "Schedule v2 buffer update on main event loop."
  (run-at-time 0 nil #'fubar-test-bell-v2--update-buffer))

(defun fubar-test-bell-v2 (&optional agents base-url buffer-name)
  "Ring test bell v2 with secret verification for AGENTS.
Creates a unique secret per agent, summons them to retrieve it,
and verifies they return the correct value."
  (interactive)
  (let* ((agents (or agents fubar-test-bell-agents))
         (base-url (or base-url fubar-agency-url))
         (buffer-name (or buffer-name fubar-test-bell-v2--buffer)))
    (setq fubar-test-bell-v2--buffer buffer-name)
    (setq fubar-test-bell-v2--pending nil)
    (pop-to-buffer (get-buffer-create fubar-test-bell-v2--buffer))
    (erase-buffer)
    (insert "=== Test Bell V2 ===\n\nCreating secrets...\n")
    ;; Create secrets and summon agents
    (dolist (agent agents)
      (fubar-test-bell-v2--create-secret
       (lambda (secret-pair)
         (if secret-pair
             (let ((secret-id (car secret-pair))
                   (secret-value (cdr secret-pair)))
               (push (list agent secret-id secret-value 'pending)
                     fubar-test-bell-v2--pending)
               (fubar-test-bell-v2--update-buffer-safe)
               (fubar-test-bell-v2--summon-agent
                agent secret-id secret-value
                (lambda (agent result)
                  (let ((entry (assoc agent fubar-test-bell-v2--pending)))
                    (when entry
                      (setf (nth 3 entry) result)))
                  (fubar-test-bell-v2--update-buffer-safe))))
           (push (list agent nil nil '(error . "failed to create secret"))
                 fubar-test-bell-v2--pending)
           (fubar-test-bell-v2--update-buffer-safe)))
       base-url))))

(defun fubar--test-bell-v2-run (agents base-url buffer-name)
  "Run a v2 test bell with local state and BUFFER-NAME."
  (let ((pending nil))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== Test Bell V2 (%s) ===\n\nCreating secrets...\n" base-url))))
    (dolist (agent agents)
      (fubar-test-bell-v2--create-secret
       (lambda (secret-pair)
         (if secret-pair
             (let ((secret-id (car secret-pair))
                   (secret-value (cdr secret-pair)))
               (push (list agent secret-id secret-value 'pending) pending)
               (fubar--test-bell-v2-update buffer-name pending)
               (fubar-test-bell-v2--summon-agent
                agent secret-id secret-value
                (lambda (_agent result)
                  (let ((entry (assoc agent pending)))
                    (when entry
                      (setf (nth 3 entry) result)))
                  (fubar--test-bell-v2-update buffer-name pending))
                base-url))
           (push (list agent nil nil '(error . "failed to create secret")) pending)
           (fubar--test-bell-v2-update buffer-name pending)))
       base-url))))

(defun fubar--test-bell-v2-update (buffer-name pending)
  "Update BUFFER-NAME with PENDING v2 state."
  (run-at-time
   0 nil
   (lambda ()
     (with-current-buffer (get-buffer-create buffer-name)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (format "=== Test Bell V2 (%s) ===\n\n" buffer-name))
         (dolist (entry pending)
           (let* ((agent (car entry))
                  (data (cdr entry))
                  (secret-id (nth 0 data))
                  (secret-value (nth 1 data))
                  (status (nth 2 data)))
             (insert (format "  %-12s  [%s]\n" agent (or secret-id "no-secret")))
             (insert (format "                Expected: %s\n" (or secret-value "?")))
             (cond
              ((eq status 'pending)
               (insert "                Status: ⏳ waiting...\n"))
              ((and (consp status) (eq (car status) 'verified))
               (insert (format "                Status: ✓ VERIFIED - %s\n"
                               (truncate-string-to-width (cdr status) 40))))
              ((and (consp status) (eq (car status) 'mismatch))
               (insert (format "                Status: ⚠ MISMATCH - got: %s\n"
                               (truncate-string-to-width (cdr status) 40))))
              ((and (consp status) (eq (car status) 'error))
               (insert (format "                Status: ✗ ERROR - %s\n" (cdr status))))
              (t
               (insert (format "                Status: ? %s\n" status))))
             (insert "\n")))
         (let ((verified (seq-count (lambda (e)
                                      (and (consp (nth 2 (cdr e)))
                                           (eq (car (nth 2 (cdr e))) 'verified)))
                                    pending))
               (total (length pending)))
           (insert (format "Verified: %d/%d\n" verified total))))))))

;;; Local/Global test bells (connected-agent ACKs)

(defun fubar--agency-connected-async (base-url callback &optional type)
  "Fetch connected agents from Agency at BASE-URL, call CALLBACK with list.
Optional TYPE can be \"local\" or \"remote\" to filter agents."
  (let* ((url-request-method "GET")
         (url (concat (string-trim-right base-url "/") "/agency/connected"
                      (if type (concat "?type=" type) ""))))
    (url-retrieve
     url
     (lambda (status callback)
       (let ((agents
              (if (plist-get status :error)
                  nil
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (condition-case nil
                      (let* ((json-object-type 'plist)
                             (json-array-type 'list)
                             (resp (json-read)))
                        (when (plist-get resp :ok)
                          (let ((vals (plist-get resp :agents)))
                            (and (seq vals) vals))))
                    (error nil))))))
         (kill-buffer (current-buffer))
         (funcall callback agents)))
     (list callback)
     t t)))

(defcustom fubar-test-bell-timeout-seconds 20
  "Seconds to wait for test-bell ACK before marking timeout."
  :type 'integer
  :group 'fubar)

(defun fubar--bell-ack-status-async (base-url secret-id callback)
  "Fetch ack status for SECRET-ID from Agency at BASE-URL."
  (let* ((url-request-method "GET")
         (url (concat (string-trim-right base-url "/") "/agency/ack/" secret-id)))
    (url-retrieve
     url
     (lambda (_status callback)
       (let ((result nil))
         (goto-char (point-min))
         (when (re-search-forward "\n\n" nil t)
           (condition-case nil
               (let* ((json-object-type 'plist)
                      (json-array-type 'list)
                      (resp (json-read)))
                 (setq result resp))
             (error nil)))
         (kill-buffer (current-buffer))
         (funcall callback result)))
     (list callback)
     t t)))

(defun fubar--bell-test-async (agent base-url callback)
  "Send a test bell to AGENT via BASE-URL, then poll for ack."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (payload `((agent-id . ,agent)
                    (type . "test-bell")
                    (payload . ((message . "ACK: fetch secret and reply")))))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (url (concat (string-trim-right base-url "/") "/agency/bell"))
         (start (float-time)))
    (url-retrieve
     url
     (lambda (status agent base-url callback start)
       (let ((secret-id nil)
             (secret-value nil))
         (if (plist-get status :error)
             (funcall callback agent (cons 'error (format "%s" (plist-get status :error))))
           (goto-char (point-min))
           (when (re-search-forward "\n\n" nil t)
             (condition-case nil
                 (let* ((json-object-type 'plist)
                        (json-array-type 'list)
                        (resp (json-read)))
                   (setq secret-id (plist-get (plist-get resp :bell) :secret-id))
                   (setq secret-value (plist-get resp :secret-value)))
               (error nil)))
           (if (or (null secret-id) (null secret-value))
               (funcall callback agent (cons 'error "no secret in bell response"))
             (fubar--bell-ack-poll agent base-url secret-id secret-value callback start))))
       (kill-buffer (current-buffer)))
     (list agent base-url callback start)
     t t)))

(defun fubar--bell-ack-poll (agent base-url secret-id secret-value callback start-time)
  "Poll Agency for ack until timeout."
  (fubar--bell-ack-status-async
   base-url secret-id
   (lambda (resp)
     (let ((ok (plist-get resp :ok))
           (ack (plist-get resp :ack)))
       (cond
        ((and ok ack)
         (let ((value (plist-get ack :value)))
           (if (string= value secret-value)
               (funcall callback agent (cons 'verified value))
             (funcall callback agent (cons 'mismatch (or value ""))))))
        ((> (- (float-time) start-time) fubar-test-bell-timeout-seconds)
         (funcall callback agent (cons 'error "timeout")))
        (t
         (run-at-time 0.5 nil #'fubar--bell-ack-poll
                      agent base-url secret-id secret-value callback start-time)))))))

(defun fubar-test-local ()
  "Test local (in-JVM) agents at local Agency via bell+ack."
  (interactive)
  (let ((base-url fubar-agency-url))
    (fubar--agency-connected-async
     base-url
     (lambda (agents)
       (let ((agents (or agents fubar-test-bell-agents)))
         (if (null agents)
             (message "[fubar] No local agents at %s" base-url)
           (setq fubar-test-bell--pending
                 (mapcar (lambda (a) (cons a 'pending)) agents))
           (setq fubar-test-bell--buffer "*Test Local*")
           (pop-to-buffer (get-buffer-create fubar-test-bell--buffer))
           (fubar-test-bell--update-buffer)
           (dolist (agent agents)
             (fubar--bell-test-async
              agent base-url
              (lambda (agent result)
                (setf (alist-get agent fubar-test-bell--pending nil nil #'equal) result)
                (fubar-test-bell--update-buffer-safe)))))))
     "local")))

(defun fubar-test-global ()
  "Test all Agency URLs by ACKing connected agents at each.
 Runs a separate buffer per Agency."
  (interactive)
  (dolist (base-url fubar-agency-global-urls)
    (let ((base-url (string-trim-right base-url "/")))
      (fubar--agency-connected-async
       base-url
       (lambda (agents)
         (let* ((agents (or agents fubar-test-bell-agents))
                (buf (format "*Test Global: %s*" base-url)))
           (if (null agents)
               (message "[fubar] No connected agents at %s" base-url)
             (setq fubar-test-bell--pending
                   (mapcar (lambda (a) (cons a 'pending)) agents))
             (setq fubar-test-bell--buffer buf)
             (pop-to-buffer (get-buffer-create fubar-test-bell--buffer))
             (fubar-test-bell--update-buffer)
             (dolist (agent agents)
               (fubar--bell-test-async
                agent base-url
                (lambda (agent result)
                  (setf (alist-get agent fubar-test-bell--pending nil nil #'equal) result)
                  (fubar-test-bell--update-buffer-safe)))))))))))

;; Backwards-compatible aliases
(defalias 'fubar-test-bell 'fubar-test-local)

;;; Test Bell WS - ring connected agents via WebSocket

(defun fubar-test-bell-ws (&optional agent-id)
  "Ring the bell to connected agents via WebSocket.
If AGENT-ID is nil, rings all connected agents.
The bell creates a secret and pushes it to agents via WebSocket."
  (interactive "sAgent ID (blank for all): ")
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (payload (if (and agent-id (not (string-empty-p agent-id)))
                      `((agent-id . ,agent-id)
                        (type . "test-bell")
                        (payload . ((message . "Ring ring! Fetch the secret."))))
                    `((agent-id . "all")
                      (type . "test-bell")
                      (payload . ((message . "Ring ring! Fetch the secret."))))))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (url (concat (string-trim-right fubar-agency-url "/") "/agency/bell")))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "[fubar] Bell failed: %s" (plist-get status :error))
         (goto-char (point-min))
         (when (re-search-forward "\n\n" nil t)
           (condition-case err
               (let* ((json-object-type 'plist)
                      (resp (json-read)))
                 (if (plist-get resp :ok)
                     (message "[fubar] Bell rang! Secret: %s, Agents: %s"
                              (plist-get resp :secret-value)
                              (or (plist-get resp :agents)
                                  (plist-get resp :agent-id)))
                   (message "[fubar] Bell failed: %s" (plist-get resp :error))))
             (error (message "[fubar] Parse error: %s" err)))))
       (kill-buffer (current-buffer)))
     nil t t)))

(defun fubar-agency-connected ()
  "Show currently connected agents."
  (interactive)
  (let* ((url-request-method "GET")
         (url (concat (string-trim-right fubar-agency-url "/") "/agency/connected")))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "[fubar] Failed: %s" (plist-get status :error))
         (goto-char (point-min))
         (when (re-search-forward "\n\n" nil t)
           (condition-case err
               (let* ((json-object-type 'plist)
                      (resp (json-read))
                      (agents (plist-get resp :agents)))
                 (if agents
                     (message "[fubar] Connected agents: %s" (mapconcat #'identity agents ", "))
                   (message "[fubar] No agents connected")))
             (error (message "[fubar] Parse error: %s" err)))))
       (kill-buffer (current-buffer)))
     nil t t)))

(provide 'fubar)

(fubar-modeline-mode 1)

;;; fubar.el ends here
