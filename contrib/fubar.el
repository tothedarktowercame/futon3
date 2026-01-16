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

;;; State

(defvar fubar--current-session nil
  "Current MUSN session plist with :session/id, :turn, etc.")

(defvar fubar--current-turn nil
  "Current turn number.")

(defvar fubar--selected-pattern nil
  "Currently selected pattern ID.")

(defvar fubar--candidates nil
  "Current candidate patterns from HUD.")

;;; Utilities

(defun fubar--now-iso ()
  "Return current time as ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))

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
      (message "[fubar] Session created: %s" (plist-get resp :session/id))
      resp)))

(defun fubar-turn-start (intent)
  "Start a new turn with INTENT."
  (interactive "sIntent: ")
  (unless fubar--current-session
    (fubar-session-create))
  (let* ((sid (plist-get fubar--current-session :session/id))
         (turn (1+ (or fubar--current-turn 0)))
         (payload `((session/id . ,sid)
                    (turn . ,turn)
                    (hud . ((intent . ,intent))))))
    (let ((resp (fubar--post "/musn/turn/start" payload)))
      (setq fubar--current-turn turn)
      (setq fubar--selected-pattern nil)
      ;; Extract candidates from HUD response
      (when-let ((hud (plist-get resp :hud)))
        (setq fubar--candidates (plist-get hud :candidates)))
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
      (message "[fubar] Action logged: %s on %s" action fubar--selected-pattern)
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
      (message "[fubar] Turn %d ended" turn)
      resp)))

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
  (message "[fubar] State reset"))

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

(provide 'fubar)

;;; fubar.el ends here
