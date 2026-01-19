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

(declare-function fubar-hud--show-run-layout "fubar-hud" (stream-buf))

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

(defun fubar-musn--log-path (session-id)
  "Return log path for SESSION-ID."
  (let* ((sid (and session-id (string-trim session-id)))
         (safe (and sid (not (string-empty-p sid))
                    (replace-regexp-in-string "[^a-zA-Z0-9._-]+" "-" sid))))
    (if (and safe (not (string-empty-p safe)))
        (format "/tmp/musn_stream.%s.log" safe)
      fubar-musn--default-log)))
(defun fubar-musn--known-sessions ()
  "Return known MUSN session ids from log files."
  (let ((files (ignore-errors (directory-files "/tmp" nil "^musn_stream\\..+\\.log$"))))
    (let ((ids (mapcar (lambda (file)
                         (when (string-match "^musn_stream\\.\\(.+\\)\\.log$" file)
                           (match-string 1 file)))
                       (or files '()))))
      (delete-dups (delq nil ids)))))
(defvar fubar-musn--state-refresh-seconds 3
  "Poll interval (seconds) to refresh HUD intent/state from MUSN.")
(defvar fubar-musn--state-timer nil)
(defvar fubar-musn--session-scan-timer nil)
(defvar fubar-musn-view-detail-buffer "*FuBar MUSN Detail*")
(defvar-local fubar-musn--partial "")
(defvar-local fubar-musn--log-path nil)
(defvar-local fubar-musn--last-line-start nil)
(defvar-local fubar-musn--last-line-end nil)
(defvar-local fubar-musn--pause-seen nil)
(defvar-local fubar-musn--pause-reason nil)
(defvar-local fubar-musn--last-pause-marker nil)
(defvar-local fubar-musn--run-finished nil)
(defvar-local fubar-musn--session-locked nil)

(defcustom fubar-musn-stream-folding-enabled t
  "Whether folding is enabled in the MUSN viewer."
  :type 'boolean
  :group 'fubar-musn-viewer)

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
    (:musn . "\\[musn[^]]*\\]")
    (:error . "\\[musn-stream\\] ERROR\\|\\bERROR\\b"))
  "Regex rules to map log lines into fold categories."
  :type '(repeat (cons symbol regexp))
  :group 'fubar-musn-viewer)

(defvar fubar-musn-view-font-lock-keywords
  '(("^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[^ ]+\\)" . font-lock-comment-face)
    ("\\[musn[^]]*\\] file_change\\b" . fubar-musn-file-change-face)
    ("\\[musn[^]]*\\] command_execution\\b" . fubar-musn-command-face)
    ("\\[musn[^]]*\\] reasoning\\b" . fubar-musn-reasoning-face)
    ("\\[aif\\]" . font-lock-constant-face)
    ("\\[pattern-[^]]+\\]" . font-lock-keyword-face)
    ("\\[hud-[^]]+\\]" . font-lock-preprocessor-face)
    ("\\[MUSN-PAUSE\\]\\|\\[musn-pause\\]" . font-lock-warning-face)
    ("\\[musn-warning\\]" . font-lock-warning-face)
    ("\\[musn-session\\]" . font-lock-builtin-face)))

(defface fubar-musn-file-change-face
  '((t :foreground "dark green"))
  "Face for file change summaries."
  :group 'fubar-musn-viewer)

(defface fubar-musn-command-face
  '((t :foreground "orange"))
  "Face for command execution summaries."
  :group 'fubar-musn-viewer)

(defface fubar-musn-reasoning-face
  '((t :foreground "purple"))
  "Face for reasoning summaries."
  :group 'fubar-musn-viewer)

(defface fubar-musn-pause-banner-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for MUSN pause banner."
  :group 'fubar-musn-viewer)

(defface fubar-musn-finished-banner-face
  '((t :inherit success :weight bold))
  "Face for MUSN finished banner."
  :group 'fubar-musn-viewer)

(defface fubar-musn-running-banner-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for MUSN running banner."
  :group 'fubar-musn-viewer)

(defconst fubar-musn--musn-prefix-re "\\[musn[^]]*\\]")

(defun fubar-musn--extract-intent-from-prompt (prompt)
  "Extract an Intent block from PROMPT when present."
  (when (and prompt (stringp prompt))
    (let* ((lines (split-string prompt "\n"))
           (start (cl-position-if (lambda (line)
                                    (string-match-p "^\\s-*Intent\\s-*:" line))
                                  lines))
           (intent-lines nil))
      (when start
        (let ((line (nth start lines)))
          (setq intent-lines
                (list (string-trim (replace-regexp-in-string
                                    "^\\s-*Intent\\s-*:\\s*" "" line))))))
      (when start
        (let ((idx (1+ start)))
          (while (and (< idx (length lines))
                      (let ((line (nth idx lines)))
                        (not (string-empty-p (string-trim line)))))
            (push (string-trim (nth idx lines)) intent-lines)
            (setq idx (1+ idx)))))
      (when intent-lines
        (string-trim (string-join (nreverse intent-lines) " "))))))

(defun fubar-musn--summarize-intent (intent)
  "Normalize INTENT to a single, viewer-friendly line."
  (when (and intent (stringp intent))
    (let* ((line (car (split-string intent "\n")))
           (clean (string-trim (replace-regexp-in-string "\\s-+" " " (or line intent)))))
      (if (> (length clean) 160)
          (concat (substring clean 0 157) "…")
        clean))))

(defun fubar-musn--resolve-intent (prompt intent)
  "Resolve INTENT for PROMPT, preferring explicit intent blocks."
  (let ((trimmed (and intent (string-trim intent))))
    (cond
     ((and trimmed (not (string-empty-p trimmed)))
      (fubar-musn--summarize-intent trimmed))
     ((and prompt (not (string-empty-p (string-trim prompt))))
      (let ((extracted (fubar-musn--extract-intent-from-prompt prompt)))
        (fubar-musn--summarize-intent (or extracted prompt))))
     (t nil))))

(defun fubar-musn--musn-prefix (line)
  (when (and line (string-match fubar-musn--musn-prefix-re line))
    (match-string 0 line)))

(defvar fubar-musn-view-mode-map (make-sparse-keymap))

(let ((map fubar-musn-view-mode-map))
  (define-key map (kbd "RET") #'fubar-musn-view-detail)
  (define-key map [mouse-1] #'fubar-musn-view-detail)
  (define-key map (kbd "g") #'fubar-musn-view-refresh)
  (define-key map (kbd "f") #'fubar-musn-toggle-folding)
  (define-key map (kbd "p") #'fubar-musn-jump-to-latest-pause))

(define-derived-mode fubar-musn-view-mode special-mode "FuBar-MUSN"
  "Major mode for MUSN stream viewing."
  (setq-local font-lock-defaults '(fubar-musn-view-font-lock-keywords))
  (setq-local truncate-lines t)
  (let ((table (copy-syntax-table (syntax-table))))
    ;; Treat quotes as punctuation so unmatched JSON strings do not
    ;; leak font-lock highlighting across the buffer.
    (modify-syntax-entry ?\" "." table)
    (set-syntax-table table))
  (setq-local fubar-musn--partial "")
  (setq-local fubar-musn--last-line-start nil)
  (setq-local fubar-musn--last-line-end nil)
  (setq-local fubar-musn--pause-seen nil)
  (setq-local fubar-musn--pause-reason nil)
  (setq-local fubar-musn--last-pause-marker nil)
  (setq-local fubar-musn--run-finished nil)
  (setq-local fubar-musn--run-active nil)
  (setq-local fubar-musn--session-locked nil)
  (setq-local header-line-format nil))

(defun fubar-musn--generate-session-id ()
  (format "musn-%s"
          (substring (md5 (format "%s-%s" (float-time) (random))) 0 8)))

(defun fubar-musn--pause-line-p (line)
  (let ((case-fold-search t))
    (and line (string-match-p "\\[musn-pause\\]" line))))

(defun fubar-musn--run-started-line-p (line)
  (let ((case-fold-search t))
    (or (and line (string-match-p (concat fubar-musn--musn-prefix-re " turn\\.started\\b") line))
        (let ((event-type (fubar-musn--event-type-from-line line)))
          (and event-type (string= event-type "turn.started"))))))

(defun fubar-musn--run-finished-line-p (line)
  (let ((case-fold-search t))
    (or (and line (string-match-p (concat fubar-musn--musn-prefix-re " turn\\.completed\\b") line))
        (let ((event-type (fubar-musn--event-type-from-line line)))
          (and event-type (string= event-type "turn.completed"))))))

(defun fubar-musn--stringify (value)
  (cond
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   ((null value) nil)
   (t (format "%s" value))))

(defun fubar-musn--format-reason (reason)
  (when reason
    (let* ((rtype (fubar-musn--stringify (plist-get reason :type)))
           (note (fubar-musn--stringify (plist-get reason :note)))
           (note (and note (string-trim note))))
      (cond
       ((and rtype note) (format "%s: %s" rtype note))
       (rtype rtype)
       (note note)
       (t nil)))))

(defun fubar-musn--pause-reason-from-line (line)
  (when (and line (string-match "\\[MUSN-PAUSE\\]\\s-+\\(.*\\)$" line))
    (let* ((json-str (match-string 1 line))
           (pause (ignore-errors
                    (json-parse-string json-str
                                       :object-type 'plist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object nil)))
           (reason (and pause (plist-get pause :reason))))
      (fubar-musn--format-reason reason))))

(defun fubar-musn--update-pause-banner ()
  (setq header-line-format
        (cond
         (fubar-musn--pause-seen
          (let ((reason (and fubar-musn--pause-reason
                             (not (string-empty-p fubar-musn--pause-reason))
                             (format " (%s)" fubar-musn--pause-reason))))
            (propertize (format " MUSN PAUSED%s -- resume in HUD "
                                (or reason ""))
                        'face 'fubar-musn-pause-banner-face)))
         (fubar-musn--run-finished
          (propertize " MUSN RUN FINISHED "
                      'face 'fubar-musn-finished-banner-face))
         (fubar-musn--run-active
          (propertize " FUCODEX RUNNING "
                      'face 'fubar-musn-running-banner-face))
         (t nil))))

(defun fubar-musn--set-run-active (active)
  (setq fubar-musn--run-active (and active t))
  (when active
    (setq fubar-musn--run-finished nil)
    (setq fubar-musn--pause-seen nil)
    (setq fubar-musn--pause-reason nil)
    (when (fboundp 'fubar-hud-set-musn-paused)
      (fubar-hud-set-musn-paused nil)))
  (fubar-musn--update-pause-banner))

(defun fubar-musn--set-run-finished (finished)
  (setq fubar-musn--run-finished (and finished t))
  (when finished
    (setq fubar-musn--run-active nil))
  (when finished
    (setq fubar-musn--pause-seen nil)
    (setq fubar-musn--pause-reason nil)
    (when (fboundp 'fubar-hud-set-musn-paused)
      (fubar-hud-set-musn-paused nil)))
  (fubar-musn--update-pause-banner))

(defun fubar-musn--note-pause (pos line)
  (setq fubar-musn--pause-seen t)
  (when-let ((reason (fubar-musn--pause-reason-from-line line)))
    (setq fubar-musn--pause-reason reason))
  (fubar-musn--update-pause-banner)
  (if (markerp fubar-musn--last-pause-marker)
      (set-marker fubar-musn--last-pause-marker pos (current-buffer))
    (setq fubar-musn--last-pause-marker (copy-marker pos)))
  (when (fboundp 'fubar-hud-note-pause)
    (fubar-hud-note-pause fubar-musn--pause-reason)))

(defun fubar-musn--set-paused (paused &optional reason)
  (setq fubar-musn--pause-seen (and paused t))
  (cond
   ((not paused)
    (setq fubar-musn--pause-reason nil))
   (reason
    (setq fubar-musn--pause-reason reason)))
  (fubar-musn--update-pause-banner))

(defun fubar-musn-jump-to-latest-pause ()
  "Jump to the latest MUSN pause line in the viewer."
  (interactive)
  (let ((buf (get-buffer fubar-musn-view-buffer)))
    (unless buf
      (user-error "MUSN viewer buffer not available"))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (unless (markerp fubar-musn--last-pause-marker)
        (user-error "No MUSN pause event seen yet"))
      (let ((pos (marker-position fubar-musn--last-pause-marker)))
        (unless pos
          (user-error "No MUSN pause event seen yet"))
        (goto-char pos)
        (recenter 2)))))

(defun fubar-musn--extract-session-id (text)
  (when (and text (string-match "\\[musn-session\\]\\s-*\\(musn-[A-Za-z0-9]+\\)" text))
    (match-string 1 text)))

(defun fubar-musn--extract-hud-intent (text)
  (when (and text
             (string-match
              (concat
               "\\`\\(?:"
               "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
               "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
               "\\(?:\\.[0-9]+\\)?Z?\\s-+\\)?"
               "\\[hud-intent\\]\\s-*\\(.*\\)$")
              text))
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
  (let ((session-changed nil))
    (when session-id
      (let ((prior fubar-musn-session-id))
        (setq session-changed (and prior (not (string= prior session-id))))
        (when (or (not fubar-musn--session-locked)
                  (null prior)
                  (string= prior session-id))
          (setq fubar-musn-session-id session-id)
          (when session-changed
            (setq fubar-musn-turn nil)
            (setq fubar-musn--pause-seen nil)
            (setq fubar-musn--pause-reason nil)
            (setq fubar-musn--run-active nil)
            (setq fubar-musn--run-finished nil)
            (fubar-musn--update-pause-banner))
          (fubar-musn--maybe-start-state-poll))))
  (when (or session-id intent)
    (with-current-buffer (get-buffer-create "*FuLab HUD*")
      (when (fboundp 'fubar-hud--ensure-mode)
        (fubar-hud--ensure-mode))
      (when (and session-id (boundp 'fubar-hud--session-id))
        (when (or (null fubar-hud--session-id)
                  (string-empty-p fubar-hud--session-id)
                  (string= fubar-hud--session-id session-id))
          (setq fubar-hud--session-id session-id)))
      (when (and session-changed (boundp 'fubar-hud--musn-paused))
        (setq fubar-hud--musn-paused nil
              fubar-hud--musn-pause-seen nil
              fubar-hud--musn-pause-reason nil)
        (when (fboundp 'fubar-hud-set-musn-paused)
          (fubar-hud-set-musn-paused nil)))
      (when (and intent (boundp 'fubar-hud--intent))
        (when (or (null fubar-hud--intent)
                  (string-empty-p fubar-hud--intent))
          (setq fubar-hud--intent intent))
        (when (boundp 'fubar-hud--intent-source)
          (setq fubar-hud--intent-source :agent)))
      (when (boundp 'fubar-hud--current-hud)
        (setq fubar-hud--current-hud nil))
      (when (fboundp 'fubar-hud-refresh)
        (fubar-hud-refresh))))))

(defun fubar-musn--sync-handshake-from-text (text)
  (let* ((handshake (fubar-musn--extract-handshake text))
         (session-id (plist-get handshake :session-id))
         (intent (plist-get handshake :intent)))
    (when (or session-id intent)
      (fubar-musn--apply-handshake session-id intent)
      t)))

(defun fubar-musn--sync-handshake-from-log (path)
  "Replay the latest HUD intent/session handshake from PATH."
  (when (and path (file-readable-p path) (null fubar-musn-session-id))
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
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("X-Musn-Client" . "fubar-viewer")
                                      ("User-Agent" . "fubar-viewer")))
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
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (if (derived-mode-p 'fubar-musn-view-mode)
            (fubar-musn--insert-line line)
          (insert line "\n"))))))

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
  (when fubar-musn-stream-folding-enabled
    (or (and (member :all fubar-musn-stream-folds)
             (not (member category fubar-musn-stream-unfold-types)))
        (member category fubar-musn-stream-folds))))

(defun fubar-musn--normalize-token (raw)
  (cond
   ((and raw
         (>= (length raw) 2)
         (string-prefix-p "\"" raw)
         (string-suffix-p "\"" raw))
    (substring raw 1 (1- (length raw))))
   ((and raw (string-prefix-p ":" raw))
    (substring raw 1))
   (t raw)))

(defun fubar-musn--extract-item-type (line)
  (when (string-match ":item\\s-+{[^}]*:type\\s-+\\(\"[^\"]+\"\\|:[^,}[:space:]]+\\)" line)
    (fubar-musn--normalize-token (match-string 1 line))))

(defun fubar-musn--extract-change-path (line)
  (when (string-match ":path\\s-+\"\\([^\"]+\\)\"" line)
    (match-string 1 line)))

(defun fubar-musn--extract-command (line)
  (when (string-match ":command\\s-+\"\\([^\"]+\\)\"" line)
    (match-string 1 line)))

(defun fubar-musn--extract-text (line)
  (when (string-match ":text\\s-+\"\\([^\"]+\\)\"" line)
    (match-string 1 line)))

(defun fubar-musn--summarize-text (text)
  (when text
    (let* ((clean (replace-regexp-in-string "\\\\[rnt]" " " text))
           (clean (replace-regexp-in-string "[\r\n\t]+" " " clean))
           (clean (replace-regexp-in-string "\\*\\*" "" clean))
           (clean (replace-regexp-in-string " +" " " (string-trim clean))))
      (when (not (string-empty-p clean))
        (if (> (length clean) 80)
            (concat (substring clean 0 80) "...")
          clean)))))

(defun fubar-musn--extract-timestamp (line)
  (let ((line (fubar-musn--compress-timestamp line)))
    (when (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" line)
      (match-string 1 line))))

(defun fubar-musn--event-type-from-line (line)
  (when (string-match (concat fubar-musn--musn-prefix-re
                              " event (\\({:type\\s-+\\(\"[^\"]+\"\\|:[^,}[:space:]]+\\)\\)") line)
    (fubar-musn--normalize-token (match-string 2 line))))

(defun fubar-musn--rewrite-item-completed (line)
  (when (string-match-p (concat fubar-musn--musn-prefix-re " event") line)
    (let ((event-type (fubar-musn--event-type-from-line line))
          (prefix (or (fubar-musn--musn-prefix line) "[musn]"))
          (item-type (fubar-musn--extract-item-type line))
          (path (fubar-musn--extract-change-path line))
          (command (fubar-musn--extract-command line))
          (text (fubar-musn--extract-text line)))
      (when (and event-type (string= event-type "item.completed")
                 item-type)
        (let ((ts (fubar-musn--extract-timestamp line)))
          (cond
           ((and (string= item-type "file_change") path)
            (if ts
                (format "%s %s %s %s" ts prefix item-type path)
              (format "%s %s %s" prefix item-type path)))
           ((and (string= item-type "command_execution") command)
            (if ts
                (format "%s %s %s %s" ts prefix item-type command)
              (format "%s %s %s" prefix item-type command)))
           ((string= item-type "reasoning")
            (let ((summary (fubar-musn--summarize-text text)))
              (if ts
                  (format "%s %s %s%s" ts prefix item-type (if summary (concat " " summary) ""))
                (format "%s %s%s" prefix item-type (if summary (concat " " summary) "")))))
           (t nil)))))))

(defun fubar-musn--rewrite-event-line (line)
  (if (not (string-match (concat fubar-musn--musn-prefix-re " event") line))
      line
    (or (fubar-musn--rewrite-item-completed line)
        (let ((event-type (fubar-musn--event-type-from-line line)))
          (if (not event-type)
              line
            (let* ((prefix (or (fubar-musn--musn-prefix line) "[musn]"))
                   (line (replace-regexp-in-string
                          (concat (regexp-quote prefix) " event")
                          (format "%s %s" prefix event-type)
                          line t t)))
              (setq line (replace-regexp-in-string
                          "({:type\\s-+\\(?:\"[^\"]+\"\\|:[^,}[:space:]]+\\)\\s-*,?\\s-*"
                          "({"
                          line))
              line))))))

(defun fubar-musn--flatten-line (line)
  (let* ((line (fubar-musn--rewrite-event-line line))
         (line (fubar-musn--compress-timestamp line))
         (single (replace-regexp-in-string "[\r\n\t]+" " " (or line "")))
         (single (replace-regexp-in-string " +" " " (string-trim single))))
    single))

(defun fubar-musn--summarize-line (line)
  (let* ((single (fubar-musn--flatten-line line))
         (cut (string-match (regexp-quote " ({") single))
         (single (if cut
                     (concat (substring single 0 cut) " ...")
                   single))
         (max fubar-musn-stream-summary-max))
    (if (and max (> (length single) max))
        (concat (substring single 0 max) "...")
      single)))

(defun fubar-musn--compress-timestamp (line)
  (replace-regexp-in-string
   "\\b\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\.[0-9]+Z?\\b"
   "\\1"
   (or line "")))

(defun fubar-musn--timestamp-line-p (line)
  (string-match-p
   "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
   (or line "")))

(defun fubar-musn--insert-line (line)
  (let* ((category (fubar-musn--line-category line))
         (folded (fubar-musn--fold-line-p category))
         (display (if folded
                      (fubar-musn--summarize-line line)
                    (fubar-musn--flatten-line line)))
         (start (point)))
    (insert display "\n")
    (add-text-properties
     start (point)
     `(fubar-musn-full ,line
                       fubar-musn-category ,category
                       fubar-musn-folded ,folded
                       keymap ,fubar-musn-view-mode-map
                       mouse-face highlight))
    (when (string-match-p "\\[musn-session\\]" line)
      (fubar-musn--set-run-finished nil)
      (fubar-musn--set-run-active nil))
    (when (fubar-musn--run-started-line-p line)
      (fubar-musn--set-run-active t))
    (when (fubar-musn--pause-line-p line)
      (fubar-musn--note-pause start line))
    (when (fubar-musn--run-finished-line-p line)
      (fubar-musn--set-run-finished t))
    (setq fubar-musn--last-line-start start)
    (setq fubar-musn--last-line-end (point))))

(defun fubar-musn--append-to-last-line (line)
  (when fubar-musn--last-line-start
    (let* ((start fubar-musn--last-line-start)
           (full (or (get-text-property start 'fubar-musn-full) ""))
           (category (get-text-property start 'fubar-musn-category))
           (folded (get-text-property start 'fubar-musn-folded))
           (base (string-trim-right full))
           (joined (if (string-empty-p base)
                       line
                     (concat base "\n" line)))
           (display (if folded
                        (fubar-musn--summarize-line joined)
                      (fubar-musn--flatten-line joined))))
      (save-excursion
        (goto-char start)
        (let ((line-end (line-end-position)))
          (delete-region start line-end)
          (insert display)
          (setq fubar-musn--last-line-end (point))
          (add-text-properties
           start (point)
           `(fubar-musn-full ,joined
                             fubar-musn-category ,category
                             fubar-musn-folded ,folded
                             keymap ,fubar-musn-view-mode-map
                             mouse-face highlight))))
      (when (string-match-p "\\[musn-session\\]" joined)
        (fubar-musn--set-run-finished nil)
        (fubar-musn--set-run-active nil))
      (when (fubar-musn--run-started-line-p joined)
        (fubar-musn--set-run-active t))
      (when (fubar-musn--pause-line-p joined)
        (fubar-musn--note-pause start joined))
      (when (fubar-musn--run-finished-line-p joined)
        (fubar-musn--set-run-finished t))
      t)))

(defun fubar-musn-view-detail (&optional pos)
  "Show the full log line at POS (or point) in a detail buffer."
  (interactive)
  (let* ((pos (or pos (point)))
         (line-start (line-beginning-position))
         (full (or (get-text-property pos 'fubar-musn-full)
                   (get-text-property line-start 'fubar-musn-full)
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

(defun fubar-musn-view-refresh (&optional path)
  "Refresh the MUSN viewer by reloading the log file."
  (interactive)
  (let ((log (or path fubar-musn--log-path fubar-musn--default-log)))
    (fubar-musn-view-stream log)
    (message "MUSN viewer refreshed (%s)"
             (if fubar-musn-stream-folding-enabled "folded" "expanded"))))

(defun fubar-musn-toggle-folding ()
  "Toggle folding in the MUSN viewer and refresh the buffer."
  (interactive)
  (setq fubar-musn-stream-folding-enabled (not fubar-musn-stream-folding-enabled))
  (fubar-musn-view-refresh)
  (message "MUSN folding %s"
           (if fubar-musn-stream-folding-enabled "enabled" "disabled")))

(defun fubar-musn--unescape-text (txt)
  (let* ((txt (replace-regexp-in-string "\\\\r\\\\n" "\n" txt))
         (txt (replace-regexp-in-string "\\\\n" "\n" txt))
         (txt (replace-regexp-in-string "\\\\t" "\t" txt)))
    (replace-regexp-in-string "\\\\\"" "\"" txt)))

(defun fubar-musn--stream-filter (proc chunk)
  "Process filter for streaming MUSN log output with folding."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
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
                (when (or (not fubar-musn--session-locked)
                          (and fubar-musn-session-id
                               (string= fubar-musn-session-id sid)))
                  (setq fubar-musn-session-id sid)
                  (fubar-musn--maybe-start-state-poll)))
              ;; HUD sigils line: refresh state to keep HUD in sync
              (when (string-match-p "\\[hud-sigils\\]" line)
                (fubar-musn--fetch-state))
              ;; detect HUD intent handshake lines: "[hud-intent] ..."
              (when (and (not fubar-musn--session-locked)
                         (string-match-p "\\[hud-intent\\]" line))
                (when-let ((intent (fubar-musn--extract-hud-intent line)))
                  (fubar-musn--apply-handshake nil intent)))
              (if (and (not (fubar-musn--timestamp-line-p line))
                       (fubar-musn--append-to-last-line line))
                  nil
                (fubar-musn--insert-line line)))))))))

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
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fubar-musn-view-mode))
      (setq-local fubar-musn--session-locked (and fubar-musn-session-id t)))
    (with-current-buffer buf
      (setq-local fubar-musn--log-path log))
    (let ((snapshot (with-temp-buffer
                      (when (and (file-exists-p log)
                                 (executable-find "tail"))
                        (when (zerop (call-process "tail" nil t nil "-n" "200" log))
                          (buffer-string))))))
      (when (and snapshot (not (string-empty-p snapshot)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (insert snapshot)))))
    (setq fubar-musn--proc
          (start-process "fubar-musn-tail" buf "tail" "-f" "-n" "0" log))
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
                         (prior-turn fubar-musn-turn)
                         (halt-map (and state (plist-get state :halt)))
                         (halted (or (and state (plist-get state :halt?))
                                     (and halt-map t)))
                         (halted? (and halted t))
                         (halt-reason (fubar-musn--format-reason halt-map))
                         (paused? (and halted? (not fubar-musn--run-finished)))
                         (hud (and state (plist-get state :hud)))
                         (intent (and hud (plist-get hud :intent))))
                    (when ok
                      (when turn
                        (setq fubar-musn-turn turn))
                      (when (and prior-turn turn (not (equal prior-turn turn)))
                        ;; Safety net: turn advanced, so clear stale paused state.
                        (setq paused? nil)
                        (setq halt-reason nil))
                      (let ((viewer-buf (get-buffer fubar-musn-view-buffer)))
                        (when viewer-buf
                          (with-current-buffer viewer-buf
                            (fubar-musn--set-paused paused? (when paused? halt-reason)))))
                      (with-current-buffer (get-buffer-create "*FuLab HUD*")
                        (fubar-hud--ensure-mode)
                        (cond
                         ((fboundp 'fubar-hud-set-musn-paused)
                          (fubar-hud-set-musn-paused paused? (when paused? halt-reason)))
                         ((boundp 'fubar-hud--musn-paused)
                          (setq fubar-hud--musn-paused paused?)))
                        (let ((hud-session (and (boundp 'fubar-hud--session-id) fubar-hud--session-id)))
                          (when (and hud-session (not (string-empty-p hud-session))
                                     (not (string= hud-session fubar-musn-session-id)))
                            (cl-return-from fubar-musn--fetch-state nil)))
                        (when (fboundp 'fubar-hud-set-session-id)
                          (fubar-hud-set-session-id fubar-musn-session-id))
                        (let ((has-candidates (seq (plist-get hud :candidates)))
                              (has-sigils (seq (plist-get hud :sigils)))
                              (has-aif (plist-get hud :aif)))
                          (cond
                           ((and hud (or has-candidates has-sigils has-aif))
                            ;; Apply full MUSN HUD payload and render immediately so it does not get overwritten.
                            (setq fubar-hud--current-hud hud)
                            (cond
                             ((fboundp 'fubar-hud-apply-hud-state)
                              (fubar-hud-apply-hud-state hud))
                             ((fboundp 'fubar-hud--render)
                              (fubar-hud--render hud))))
                           ((and intent (fboundp 'fubar-hud-set-intent))
                            (setq fubar-hud--intent intent)
                            (fubar-hud-refresh))))))))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))))
      ;; ignore failures
      nil)))

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
    (let ((hud-buf (get-buffer-create "*FuLab HUD*")))
      (when (and intent (not (string-empty-p intent)) (fboundp 'fubar-hud-set-intent))
        (with-current-buffer hud-buf
          (when (boundp 'fubar-hud--current-hud)
            (setq fubar-hud--current-hud nil))
          (fubar-hud-set-intent intent))))
    (if (fboundp 'fubar-hud--show-run-layout)
        (fubar-hud--show-run-layout (get-buffer fubar-musn-view-buffer))
      (split-window-right)
      (other-window 1)
      (switch-to-buffer "*FuLab HUD*")
      (other-window -1))))

(defun fubar-musn-view-session (session-id &optional intent)
  "Open MUSN viewer for SESSION-ID using its per-session log file."
  (interactive
   (let* ((default (or fubar-musn-session-id ""))
          (candidates (fubar-musn--known-sessions))
          (prompt (if candidates
                      (format "MUSN session id (default %s): " default)
                    (format "MUSN session id%s: "
                            (if (string-empty-p default) "" (format " (default %s)" default)))))
          (input (if candidates
                     (completing-read prompt candidates nil nil nil nil default)
                   (read-string prompt nil nil default))))
     (list input)))
  (let* ((sid (and session-id (string-trim session-id)))
         (log (fubar-musn--log-path sid)))
    (when (and log (not (file-exists-p log)))
      (with-temp-file log))
    (when (and sid (not (string-empty-p sid)))
      (setq fubar-musn-session-id sid)
      (setq fubar-musn--session-locked t)
      (when (fboundp 'fubar-hud-set-session-id)
        (fubar-hud-set-session-id sid))
      (when (fboundp 'fubar-musn--maybe-start-state-poll)
        (fubar-musn--maybe-start-state-poll)))
    (fubar-musn-view-2up log intent)))

(defun fubar-musn--log-empty-p (path)
  (or (not (file-exists-p path))
      (= 0 (file-attribute-size (file-attributes path)))))

(defun fubar-musn--latest-file (dir pattern)
  (let ((files (directory-files dir t pattern)))
    (car (sort files (lambda (a b)
                       (time-less-p (nth 5 (file-attributes b))
                                    (nth 5 (file-attributes a))))))))

(defun fubar-musn--latest-clojure-error ()
  (let ((latest (fubar-musn--latest-file "/tmp" "^clojure-.*\\.edn$")))
    (when (and latest (file-readable-p latest))
      (with-temp-buffer
        (insert-file-contents latest)
        (goto-char (point-min))
        (when (re-search-forward ":clojure.main/message\\s-*\"\\([^\"]+\\)\"" nil t)
          (match-string 1))))))

(defun fubar-musn--note-launch-error (buf message)
  (let ((line (format "[musn-launch] %s" message)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "\n" line "\n"))))
    (message "%s" line)))

(defun fubar-musn--check-launch (proc log buf)
  (when (and (processp proc)
             (not (process-live-p proc))
             (fubar-musn--log-empty-p log))
    (let ((err (or (fubar-musn--latest-clojure-error)
                   "MUSN stream failed to start (no log output)")))
      (fubar-musn--note-launch-error buf err))))

;; One-shot launcher for MUSN runs via fucodex (best effort inference of patterns)
(defun fubar-musn-launch-and-view (prompt &optional intent chat-room chat-author)
  "Run fucodex in MUSN mode with PROMPT, stream MUSN log, and open 2-up view.
Optional INTENT overrides the HUD intent and MUSN handshake; otherwise fucodex
computes a summary intent from PROMPT. CHAT-ROOM and CHAT-AUTHOR enable MUSN
chat routing and display name. Assumes MUSN server is running at
`fubar-musn-url`. Patterns are not required; the agent can infer from the
prompt."
  (interactive "sPrompt: ")
  (let* ((default-directory fubar-hud-futon3-root)
         (intent (let ((trimmed (and intent (string-trim intent))))
                   (when (and trimmed (not (string-empty-p trimmed)))
                     trimmed)))
         (approval-policy (when (fboundp 'fubar-hud--current-approval-policy)
                            (fubar-hud--current-approval-policy)))
         (approval-policy (when (and (stringp approval-policy)
                                     (not (string-empty-p approval-policy)))
                            approval-policy))
         (no-sandbox? (and approval-policy (string= approval-policy "never")))
         (chat-room (let ((room (or chat-room (when (boundp 'fubar-chat-room) fubar-chat-room))))
                      (when (and room (not (string-empty-p room)))
                        room)))
         (chat-author (let ((author (or chat-author
                                        (when (boundp 'fubar-chat-author-name) fubar-chat-author-name))))
                        (when (and author (not (string-empty-p author)))
                          author)))
         (session-id (fubar-musn--generate-session-id))
         (log (fubar-musn--log-path session-id))
         (fucodex-path (expand-file-name "fucodex" fubar-hud-futon3-root))
         (env (string-join
               (delq nil
                     (list (concat "FUTON3_MUSN_SESSION_ID=" (shell-quote-argument session-id))
                           (concat "FUTON3_MUSN_LOG=" (shell-quote-argument log))
                           (concat "FUTON3_MUSN_PROMPT=" (shell-quote-argument prompt))
                           (when intent
                             (concat "FUTON3_MUSN_INTENT=" (shell-quote-argument intent)))
                           (when chat-room
                             (concat "FUCODEX_CHAT_ROOM=" (shell-quote-argument chat-room)))
                           (when chat-author
                             (concat "FUCODEX_CHAT_AUTHOR=" (shell-quote-argument chat-author)))
                           (concat "FUTON3_MUSN_URL=" (shell-quote-argument fubar-musn-url))
                           (when no-sandbox? "FUTON3_MUSN_REQUIRE_APPROVAL=0")
                           "FUCODEX_PREFLIGHT=off"))
               " "))
         (args (append (list fucodex-path "--live" "--musn"
                             "--musn-url" fubar-musn-url)
                       (when approval-policy
                         (list "--approval-policy" approval-policy))
                       (when no-sandbox?
                         (list "--no-sandbox"))
                       (list (shell-quote-argument prompt))))
         (cmd (string-join (cons env args) " ")))
    (setq fubar-musn-session-id session-id)
    (setq fubar-musn--session-locked t)
    (fubar-musn--set-paused nil)
    (fubar-musn--maybe-start-state-poll)
    (when (fboundp 'fubar-register-session)
      (fubar-register-session session-id "fucodex" intent (called-interactively-p 'interactive)))
    (when (fboundp 'fubar-hud-set-session-id)
      (fubar-hud-set-session-id session-id))
    (when (fboundp 'fubar-hud-set-musn-paused)
      (fubar-hud-set-musn-paused nil))
    (when (and chat-room (fboundp 'fubar-chat-join))
      (fubar-chat-join chat-room))
    ;; Reset the local MUSN stream log so we capture the handshake from the top.
    (when log
      (with-temp-file log))
    (fubar-musn-view-2up log intent)
    (let ((buf (get-buffer-create fubar-hud-stream-buffer-name)))
      (with-current-buffer buf
        (setq default-directory fubar-hud-futon3-root)
        (erase-buffer)
        (let ((proc (start-process-shell-command "fubar-musn-launch" buf cmd)))
          (run-at-time 1 nil #'fubar-musn--check-launch proc log buf)))
      (message "Launched MUSN run%s" (if intent (format " with intent: %s" intent) "")))))

(provide 'fubar-viewer)
;;; fubar-viewer.el ends here
