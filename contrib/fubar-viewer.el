;;; fubar-viewer.el --- MUSN viewer (pauses, hints, trail) -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only viewer for MUSN runs. Shows pauses/hints from the MUSN stream and
;; minimal trail/AIF snippets. Assumes a MUSN HTTP service is running and env
;; FUTON3_MUSN_URL points to it (default http://localhost:6065).

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'autorevert)
(require 'subr-x)

(defgroup fubar-musn-viewer nil
  "MUSN viewer UI settings."
  :group 'fubar)

(defvar fubar-musn-url (or (getenv "FUTON3_MUSN_URL") "http://localhost:6065")
  "Base URL for MUSN service.")

(defvar fubar-musn-view-buffer "*FuBar MUSN Viewer*")
(defvar fubar-musn-session-id nil)
(defvar fubar-musn-turn nil)
(defvar fubar-musn--proc nil)
(defvar fubar-musn--default-log "/tmp/musn_stream.log")
(defvar fubar-musn--state-refresh-seconds 3
  "Poll interval (seconds) to refresh HUD intent/state from MUSN.")
(defvar fubar-musn--state-timer nil)
(defvar fubar-musn--session-scan-timer nil)
(defvar fubar-musn-view-detail-buffer "*FuBar MUSN Detail*")
(defvar-local fubar-musn--partial "")

(defcustom fubar-musn-stream-folds '(:all)
  "Categories to fold in the MUSN viewer."
  :type '(repeat symbol)
  :group 'fubar-musn-viewer)

(defcustom fubar-musn-stream-unfold-types '(:aif)
  "Categories to keep expanded even when folding is on."
  :type '(repeat symbol)
  :group 'fubar-musn-viewer)

(defcustom fubar-musn-stream-summary-max 200
  "Maximum characters to show in folded summaries."
  :type 'integer
  :group 'fubar-musn-viewer)

(defcustom fubar-musn-stream-category-rules
  '((:aif . "\\[aif\\]")
    (:pattern . "\\[pattern-")
    (:hud . "\\[hud-")
    (:pause . "\\[MUSN-PAUSE\\]\\|\\[musn-pause\\]")
    (:session . "\\[musn-session\\]")
    (:musn . "\\[musn\\]")
    (:error . "\\[musn-stream\\] ERROR\\|\\bERROR\\b"))
  "Regex rules to map log lines into fold categories."
  :type '(repeat (cons symbol regexp))
  :group 'fubar-musn-viewer)

(defvar fubar-musn-view-font-lock-keywords
  '(("^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[^ ]+\\)" . font-lock-comment-face)
    ("\\[aif\\]" . font-lock-constant-face)
    ("\\[pattern-[^]]+\\]" . font-lock-keyword-face)
    ("\\[hud-[^]]+\\]" . font-lock-preprocessor-face)
    ("\\[MUSN-PAUSE\\]\\|\\[musn-pause\\]" . font-lock-warning-face)
    ("\\[musn-session\\]" . font-lock-builtin-face)))

(defvar fubar-musn-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'fubar-musn-view-detail)
    (define-key map [mouse-1] #'fubar-musn-view-detail)
    map))

(define-derived-mode fubar-musn-view-mode special-mode "FuBar-MUSN"
  "Major mode for MUSN stream viewing."
  (setq-local font-lock-defaults '(fubar-musn-view-font-lock-keywords))
  (setq-local truncate-lines t)
  (setq-local fubar-musn--partial ""))

(defun fubar-musn--extract-session-id (text)
  (when (and text (string-match "\\[musn-session\\]\\s-*\\(musn-[A-Za-z0-9]+\\)" text))
    (match-string 1 text)))

(defun fubar-musn--extract-hud-intent (text)
  (when (and text (string-match "\\[hud-intent\\]\\s-*[*\"(]*\\(.*\\?*[^)\"]\\)" text))
    (let ((intent (string-trim (match-string 1 text))))
      (when (not (string-empty-p intent))
        intent))))

(defun fubar-musn--extract-handshake (text)
  (let ((session-id nil)
        (intent nil))
    (dolist (line (split-string (or text "") "\n" t))
      (let ((sid (fubar-musn--extract-session-id line)))
        (when sid (setq session-id sid)))
      (let ((found (fubar-musn--extract-hud-intent line)))
        (when found (setq intent found))))
    (list :session-id session-id :intent intent)))

(defun fubar-musn--apply-handshake (session-id intent)
  (when session-id
    (setq fubar-musn-session-id session-id)
    (when (fboundp 'fubar-hud-set-session-id)
      (with-current-buffer (get-buffer-create "*FuLab HUD*")
        (fubar-hud-set-session-id session-id)))
    (fubar-musn--maybe-start-state-poll))
  (when (and intent (fboundp 'fubar-hud-set-intent))
    (with-current-buffer (get-buffer-create "*FuLab HUD*")
      (fubar-hud-set-intent intent))))

(defun fubar-musn--sync-handshake-from-text (text)
  (let* ((handshake (fubar-musn--extract-handshake text))
         (session-id (plist-get handshake :session-id))
         (intent (plist-get handshake :intent)))
    (when (or session-id intent)
      (fubar-musn--apply-handshake session-id intent)
      t)))

(defun fubar-musn--sync-handshake-from-log (path)
  "Replay the latest HUD intent/session handshake from PATH."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (fubar-musn--sync-handshake-from-text (buffer-string)))))

(defun fubar-musn-replay-handshake (&optional path)
  "Replay the latest HUD intent handshake from PATH (default log)."
  (interactive)
  (let ((log (or path fubar-musn--default-log)))
    (if (fubar-musn--sync-handshake-from-log log)
        (message "HUD handshake replayed from %s" log)
      (message "No HUD handshake found in %s" log))))

(defun fubar-musn--json-request (method path payload)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (url-retrieve-synchronously (concat (string-remove-suffix "/" fubar-musn-url) path) t t 5)))
    (unless buffer (error "No response from MUSN"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "\n\n" nil 'move)
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (when (not (string-empty-p (string-trim body)))
              (json-parse-string body :object-type 'plist :array-type 'list :null-object nil :false-object nil))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun fubar-musn--render-line (line)
  (when (and line (not (string-empty-p line)))
    (with-current-buffer (get-buffer-create fubar-musn-view-buffer)
      (goto-char (point-max))
      (if (derived-mode-p 'fubar-musn-view-mode)
          (fubar-musn--insert-line line)
        (insert line "\n")))))

(defun fubar-musn-display-pause (pause)
  (let* ((reason (plist-get pause :reason))
         (note (and reason (plist-get reason :note))))
    (fubar-musn--render-line (format "[MUSN-PAUSE] %s" (json-encode pause)))
    (fubar-musn--render-line (format "  reason: %s" note))
    (fubar-musn--render-line "  resume: M-x fubar-musn-resume")
    (display-buffer fubar-musn-view-buffer)))

(defun fubar-musn-view-toggle ()
  "Toggle the MUSN viewer buffer."
  (interactive)
  (if (get-buffer-window fubar-musn-view-buffer)
      (delete-window (get-buffer-window fubar-musn-view-buffer))
    (display-buffer (get-buffer-create fubar-musn-view-buffer))))

(defun fubar-musn-resume (note)
  "Resume current MUSN session with NOTE."
  (interactive "sResume note: ")
  (unless (and fubar-musn-session-id fubar-musn-turn)
    (user-error "No MUSN session/turn to resume"))
  (let ((resp (fubar-musn--json-request "POST" "/musn/turn/resume"
                                        `(("session/id" . ,fubar-musn-session-id)
                                          ("turn" . ,fubar-musn-turn)
                                          ("note" . ,note)))))
    (fubar-musn--render-line (format "[MUSN-RESUME] %s" (json-encode resp)))
    (display-buffer fubar-musn-view-buffer)))

(defun fubar-view-open (path)
  "Open MUSN log at PATH (defaults to /tmp/musn_stream.log) in tailing view."
  (interactive "fMUSN log file: ")
  (let ((log (or (and (stringp path) (not (string-empty-p path)) path)
                 fubar-musn--default-log)))
    (find-file-other-window log)
    (auto-revert-tail-mode 1)
    (goto-char (point-max))
    (message "Following MUSN log: %s" log)))

(defun fubar-musn--line-category (line)
  (or (cl-loop for (cat . re) in fubar-musn-stream-category-rules
               when (string-match-p re line)
               return cat)
      :other))

(defun fubar-musn--fold-line-p (category)
  (or (and (member :all fubar-musn-stream-folds)
           (not (member category fubar-musn-stream-unfold-types)))
      (member category fubar-musn-stream-folds)))

(defun fubar-musn--summarize-line (line)
  (let* ((single (replace-regexp-in-string "[\r\n\t]+" " " (or line "")))
         (single (replace-regexp-in-string " +" " " (string-trim single)))
         (max fubar-musn-stream-summary-max))
    (if (and max (> (length single) max))
        (concat (substring single 0 max) "...")
      single)))

(defun fubar-musn--insert-line (line)
  (let* ((category (fubar-musn--line-category line))
         (folded (fubar-musn--fold-line-p category))
         (display (if folded (fubar-musn--summarize-line line) (string-trim-right line)))
         (start (point)))
    (insert display "\n")
    (add-text-properties
     start (max start (1- (point)))
     `(fubar-musn-full ,line
                       fubar-musn-category ,category
                       fubar-musn-folded ,folded
                       keymap ,fubar-musn-view-mode-map
                       mouse-face highlight))))

(defun fubar-musn-view-detail (&optional pos)
  "Show the full log line at POS (or point) in a detail buffer."
  (interactive)
  (let* ((pos (or pos (point)))
         (full (or (get-text-property pos 'fubar-musn-full)
                   (thing-at-point 'line t))))
    (unless (and full (not (string-empty-p (string-trim full))))
      (user-error "No detail available"))
    (with-current-buffer (get-buffer-create fubar-musn-view-detail-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-trim-right full))
        (goto-char (point-min))
        (view-mode 1))
      (pop-to-buffer (current-buffer)))))

(defun fubar-musn--unescape-text (txt)
  (let* ((txt (replace-regexp-in-string "\\\\r\\\\n" "\n" txt))
         (txt (replace-regexp-in-string "\\\\n" "\n" txt))
         (txt (replace-regexp-in-string "\\\\t" "\t" txt)))
    (replace-regexp-in-string "\\\\\"" "\"" txt)))

(defun fubar-musn--stream-filter (proc chunk)
  "Process filter for streaming MUSN log output with folding."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((txt (fubar-musn--unescape-text chunk))
             (txt (concat fubar-musn--partial txt))
             (lines (split-string txt "\n" nil))
             (complete (butlast lines))
             (remainder (car (last lines))))
        (setq fubar-musn--partial remainder)
        (goto-char (point-max))
        (dolist (line complete)
          (when (and line (not (string-empty-p (string-trim line))))
            ;; session id handshake
            (when-let ((sid (fubar-musn--extract-session-id line)))
              (setq fubar-musn-session-id sid)
              (fubar-musn--maybe-start-state-poll))
            ;; detect HUD intent handshake lines: "[hud-intent] ..."
            (when (string-match-p "\\[hud-intent\\]" line)
              (when-let ((intent (fubar-musn--extract-hud-intent line)))
                (fubar-musn--apply-handshake nil intent)))
            (fubar-musn--insert-line line)))))))

(defun fubar-musn--maybe-start-state-poll ()
  "Start polling MUSN state if session id is known."
  (when (and fubar-musn-session-id (null fubar-musn--state-timer))
    (setq fubar-musn--state-timer
          (run-at-time 0 fubar-musn--state-refresh-seconds #'fubar-musn--fetch-state))))

(defun fubar-musn--stop-session-scan ()
  (when (timerp fubar-musn--session-scan-timer)
    (cancel-timer fubar-musn--session-scan-timer)
    (setq fubar-musn--session-scan-timer nil)))

(defun fubar-musn--scan-raw-stream ()
  "Scan *FuLab Raw Stream* for [musn-session] and start state polling."
  (let ((buf (get-buffer "*FuLab Raw Stream*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (let ((text (buffer-string)))
            (when (fubar-musn--sync-handshake-from-text text)
              (fubar-musn--stop-session-scan)
              (fubar-musn--fetch-state)
              (fubar-musn--maybe-start-state-poll))))))))

(defun fubar-musn-stop-state-poll ()
  "Stop polling MUSN state."
  (interactive)
  (when fubar-musn--state-timer
    (cancel-timer fubar-musn--state-timer)
    (setq fubar-musn--state-timer nil)))

(defun fubar-musn-approve (&optional note)
  "Approve and resume the current MUSN turn after intent handshake.
Sends /musn/turn/resume with the latest session/turn. NOTE defaults to \"proceed\"."
  (interactive "sApprove note (default: proceed): ")
  (let ((sid fubar-musn-session-id)
        (turn (or fubar-musn-turn 1))
        (note (if (string-empty-p note) "proceed" note)))
    (unless sid
      (user-error "No MUSN session id yet"))
    (fubar-musn--json-request "POST" "/musn/turn/resume"
                              `(("session/id" . ,sid)
                                ("turn" . ,turn)
                                ("note" . ,note)))
    (message "MUSN resumed: %s turn %s" sid turn)))

(defun fubar-musn-view-stream (&optional path)
  "Follow MUSN stream by tailing PATH (default /tmp/musn_stream.log) with basic unescaping."
  (interactive)
  (let* ((log (or (and (stringp path) (not (string-empty-p path)) path)
                  fubar-musn--default-log))
         (buf (get-buffer-create fubar-musn-view-buffer)))
    (fubar-musn--sync-handshake-from-log log)
    (when (process-live-p fubar-musn--proc)
      (delete-process fubar-musn--proc))
    (with-current-buffer buf
      (erase-buffer)
      (fubar-musn-view-mode))
    (setq fubar-musn--proc
          (start-process "fubar-musn-tail" buf "tail" "-f" "-n" "200" log))
    (set-process-filter fubar-musn--proc #'fubar-musn--stream-filter)
    (set-process-query-on-exit-flag fubar-musn--proc nil)
    (display-buffer buf)
    (message "Streaming MUSN log: %s" log)))

(defun fubar-musn--fetch-state ()
  "Fetch current MUSN session state via HTTP and update the HUD."
  (when (and fubar-musn-session-id fubar-musn-url)
    (condition-case _err
        (let* ((url-request-method "POST")
               (url-request-extra-headers '(("Content-Type" . "application/json")))
               (payload (json-encode `(("session/id" . ,fubar-musn-session-id))))
               (url-request-data (encode-coding-string payload 'utf-8))
               (buffer (url-retrieve-synchronously
                        (concat (string-remove-suffix "/" fubar-musn-url) "/musn/session/state")
                        t t 5)))
          (when buffer
            (unwind-protect
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (search-forward "\n\n" nil 'move)
                  (let* ((body (buffer-substring-no-properties (point) (point-max)))
                         (parsed (and (not (string-empty-p (string-trim body)))
                                      (json-parse-string body :object-type 'plist
                                                         :array-type 'list
                                                         :null-object nil :false-object nil)))
                         (ok (plist-get parsed :ok))
                         (state (plist-get parsed :state))
                         (turn (and state (plist-get state :turn)))
                         (hud (and state (plist-get state :hud)))
                         (intent (and hud (plist-get hud :intent))))
                    (when ok
                      (when turn
                        (setq fubar-musn-turn turn))
                      (with-current-buffer (get-buffer-create "*FuLab HUD*")
                        (fubar-hud--ensure-mode)
                        (when (fboundp 'fubar-hud-set-session-id)
                          (fubar-hud-set-session-id fubar-musn-session-id))
                        (cond
                         (hud
                          ;; Apply full MUSN HUD payload and render immediately so it does not get overwritten.
                          (setq fubar-hud--current-hud hud)
                          (cond
                           ((fboundp 'fubar-hud-apply-hud-state)
                            (fubar-hud-apply-hud-state hud))
                           ((fboundp 'fubar-hud--render)
                            (fubar-hud--render hud))))
                         ((and intent (fboundp 'fubar-hud-set-intent))
                          (setq fubar-hud--intent intent)
                          (fubar-hud-refresh))))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      ;; ignore failures
      nil))))
(defun fubar-musn-view-2up (&optional path intent)
  "Open a 2-up view: MUSN stream on the left, *FuLab HUD* on the right.
PATH defaults to /tmp/musn_stream.log. If INTENT is provided (or entered
interactively), set the HUD intent immediately as the handshake."
  (interactive
   (list nil
         (or (getenv "FUTON3_MUSN_INTENT")
             (let ((current (when (get-buffer "*FuLab HUD*")
                              (with-current-buffer "*FuLab HUD*"
                                (and (boundp 'fubar-hud--intent) fubar-hud--intent)))))
               (read-string "HUD intent (blank keeps current): " nil nil current)))))
  (let ((log (or (and (stringp path) (not (string-empty-p path)) path)
                 fubar-musn--default-log)))
    (fubar-musn-view-stream log)
    (split-window-right)
    (other-window 1)
    (let ((hud-buf (get-buffer-create "*FuLab HUD*")))
      (switch-to-buffer hud-buf)
      (when (and intent (not (string-empty-p intent)) (fboundp 'fubar-hud-set-intent))
        (with-current-buffer hud-buf
          (fubar-hud-set-intent intent))))
    (other-window -1)))

;; One-shot launcher for MUSN runs via fucodex (best effort inference of patterns)
(defun fubar-musn-launch-and-view (prompt)
  "Run fucodex in MUSN mode with PROMPT, stream MUSN log, and open 2-up view.
Assumes MUSN server is running at `fubar-musn-url`. Patterns are not required;
the agent can infer from the prompt."
  (interactive "sPrompt: ")
  (let* ((default-directory fubar-hud-futon3-root)
         (intent prompt)
         (log fubar-musn--default-log)
         (fucodex-path (expand-file-name "fucodex" fubar-hud-futon3-root))
         (env (concat "FUTON3_MUSN_INTENT=" (shell-quote-argument intent)
                      " FUTON3_MUSN_URL=" (shell-quote-argument fubar-musn-url)
                      " FUCODEX_PREFLIGHT=off"))
         (cmd (mapconcat #'identity
                         (list env fucodex-path "--live" "--musn"
                               "--musn-url" fubar-musn-url
                               (shell-quote-argument prompt))
                         " ")))
    ;; Reset the local MUSN stream log so we capture the handshake from the top.
    (when (and log (file-exists-p log))
      (with-temp-file log))
    (fubar-musn-view-2up nil intent)
    (let ((buf (get-buffer-create fubar-hud-stream-buffer-name)))
      (with-current-buffer buf
        (setq default-directory fubar-hud-futon3-root)
        (erase-buffer)
        (start-process-shell-command "fubar-musn-launch" buf cmd))
      (message "Launched MUSN run with intent: %s" intent))))

(provide 'fubar-viewer)
;;; fubar-viewer.el ends here
