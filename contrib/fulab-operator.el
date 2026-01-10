;;; fulab-operator.el --- Operator helpers for Fulab evidence runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs wrappers to run fucodex evidence tickets without shell copy/paste.

;;; Code:

(require 'subr-x)
(require 'comint)
(require 'term)
(require 'fubar-hud)

(defgroup fulab-operator nil
  "Operator helpers for Fulab evidence runs."
  :group 'futon3)

(defcustom fulab-operator-futon3-root
  (expand-file-name "~/code/futon3")
  "Root directory of the futon3 repository."
  :type 'directory
  :group 'fulab-operator)

(defcustom fulab-operator-buffer-name "*Fulab Operator*"
  "Buffer for fucodex operator output."
  :type 'string
  :group 'fulab-operator)

(defcustom fulab-operator-default-use-hud t
  "When non-nil, enable HUD for operator runs."
  :type 'boolean
  :group 'fulab-operator)

(defcustom fulab-operator-heartbeat-seconds 5
  "Seconds between operator heartbeat checks."
  :type 'number
  :group 'fulab-operator)

(defcustom fulab-operator-heartbeat-idle-seconds 6
  "Seconds of silence before emitting a heartbeat."
  :type 'number
  :group 'fulab-operator)

(defcustom fulab-operator-resume-buffer-name "*FuLab Resume*"
  "Buffer name for resume terminal."
  :type 'string
  :group 'fulab-operator)

(defvar fulab-operator--last-session-id nil
  "Last session id parsed from fucodex output.")

(defvar fulab-operator--last-command nil
  "Last fucodex command executed by the operator.")

(defvar fulab-operator--last-report-hash nil
  "Last FULAB report hash ingested from a session trace.")

(defconst fulab-operator--e01-patterns
  "code-coherence/dead-code-hygiene,stack-coherence/commit-intent-alignment")

(defconst fulab-operator--e01-intent
  "Refactor aif_bridge.clj for dead code hygiene.")

(defconst fulab-operator--e01-prompt
  "Review aif_bridge.clj for dead code or unused helpers. Remove any found. Commit if tests pass.")

(defconst fulab-operator--e01b-patterns
  "code-coherence/dead-code-hygiene,stack-coherence/commit-intent-alignment")

(defconst fulab-operator--e01b-intent
  "Refactor workday.clj for dead code hygiene.")

(defconst fulab-operator--e01b-prompt
  "Review workday.clj for dead code or unused helpers. Remove any found. Commit if tests pass.")

(defconst fulab-operator--e02-patterns
  "aif/belief-state-operational-hypotheses,aif/structured-observation-vector")

(defconst fulab-operator--e02-clock-in
  "aif/belief-state-operational-hypotheses")

(defconst fulab-operator--e02-intent
  "Define belief-state schema from AIF pattern next-steps.")

(defconst fulab-operator--e02-prompt
  "Implement the first next-step from aif/belief-state-operational-hypotheses: define :aif/belief-state schema in tatami_schema.clj")

(defun fulab-operator--fucodex-path ()
  "Return the absolute path to the fucodex script."
  (expand-file-name "fucodex" fulab-operator-futon3-root))

(defun fulab-operator--output-buffer (use-hud)
  "Return the buffer for output based on USE-HUD."
  (if (and use-hud (boundp 'fubar-hud-stream-buffer-name))
      (get-buffer-create fubar-hud-stream-buffer-name)
    (get-buffer-create fulab-operator-buffer-name)))

(defun fulab-operator--maybe-show-hud (intent)
  "Show HUD with INTENT when available."
  (when (featurep 'fubar-hud)
    (when (and intent (not (string-empty-p intent)))
      (fubar-hud-set-intent intent))
    (fubar-hud-show)
    (when (boundp 'fubar-hud-stream-buffer-name)
      (display-buffer (get-buffer-create fubar-hud-stream-buffer-name)))))

(defun fulab-operator--normalize-clock-in (value)
  "Normalize clock-in VALUE to a CSV string."
  (cond
   ((stringp value) (string-trim value))
   ((listp value) (string-join (mapcar #'string-trim value) ","))
   (t "")))

(defun fulab-operator--parse-session-id (text)
  "Return the last session id found in TEXT."
  (when (stringp text)
    (let ((case-fold-search nil)
          (matches nil)
          (pos 0))
      (while (string-match "\\[lab-export\\] session=\\([[:alnum:]-]+\\)" text pos)
        (setq matches (match-string 1 text))
        (setq pos (match-end 0)))
      matches)))

(defun fulab-operator--process-sentinel (proc event)
  "Capture session id when fucodex PROC finishes EVENT."
  (when (string-match-p "finished\\|exited" event)
    (when-let ((timer (process-get proc 'fulab-heartbeat-timer)))
      (cancel-timer timer)
      (process-put proc 'fulab-heartbeat-timer nil))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((session-id (fulab-operator--parse-session-id
                             (buffer-substring-no-properties (point-min) (point-max)))))
            (when session-id
              (setq fulab-operator--last-session-id session-id)
              (when (featurep 'fubar-hud)
                (fubar-hud-set-session-id session-id)
                (when-let ((intent (process-get proc 'fulab-intent)))
                  (fubar-hud-set-intent intent)))
              (when (featurep 'fubar-hud)
                (let ((hud-buf (get-buffer fubar-hud-buffer-name)))
                  (when (buffer-live-p hud-buf)
                    (with-current-buffer hud-buf
                      (setq fubar-hud--operator-status nil)
                      (fubar-hud-refresh)))))
              (fulab-operator--schedule-trace-scan session-id)
              (message "Fulab session captured: %s" session-id))))))))

(defun fulab-operator--parse-fulab-report (text)
  "Return plist for FULAB report in TEXT, or nil."
  (when (and text (string-match "---FULAB-REPORT---\\([\\s\\S]*?\\)---END-FULAB-REPORT---" text))
    (let* ((report-text (match-string 1 text))
           (applied (when (string-match ":applied \"\\([^\"]+\\)\"" report-text)
                      (match-string 1 report-text)))
           (notes (when (string-match ":notes \"\\([^\"]+\\)\"" report-text)
                    (match-string 1 report-text))))
      (list :applied applied :notes notes :raw report-text))))

(defun fulab-operator--ingest-fulab-report (report)
  "Push REPORT into HUD and stream buffers."
  (let* ((stream-buf (if (boundp 'fubar-hud-stream-buffer-name)
                         (get-buffer fubar-hud-stream-buffer-name)
                       (get-buffer fulab-operator-buffer-name)))
         (report-text (plist-get report :raw)))
    (when (and report-text stream-buf)
      (with-current-buffer stream-buf
        (goto-char (point-max))
        (insert "\n---FULAB-REPORT---\n")
        (insert (string-trim report-text))
        (insert "\n---END-FULAB-REPORT---\n"))))
  (when (featurep 'fubar-hud)
    (let ((hud-buf (get-buffer fubar-hud-buffer-name)))
      (when (buffer-live-p hud-buf)
        (with-current-buffer hud-buf
          (setq fubar-hud--last-agent-report report)
          (fubar-hud-refresh))))))

(defun fulab-operator--scan-trace-file (path)
  "Scan PATH for FULAB report and ingest it once."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (let* ((text (buffer-string))
             (hash (secure-hash 'sha1 text))
             (report (fulab-operator--parse-fulab-report text)))
        (when (and report (not (equal hash fulab-operator--last-report-hash)))
          (setq fulab-operator--last-report-hash hash)
          (fulab-operator--ingest-fulab-report report))))))

(defun fulab-operator--schedule-trace-scan (session-id)
  "Schedule a short scan loop for SESSION-ID trace file."
  (let ((trace-path (expand-file-name (format "lab/trace/%s.org" session-id)
                                      fulab-operator-futon3-root))
        (scan-count 0)
        (timer nil))
    (setq timer
          (run-at-time 0.6 0.8
                       (lambda ()
                         (setq scan-count (1+ scan-count))
                         (fulab-operator--scan-trace-file trace-path)
                         (when (>= scan-count 12)
                           (when timer
                             (cancel-timer timer))))))))

(defun fulab-operator--process-filter (proc output)
  "Insert OUTPUT into PROC buffer and update heartbeat markers."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert output)
            (set-marker (process-mark proc) (point)))
          (when moving
            (goto-char (process-mark proc)))))))
  (process-put proc 'fulab-last-output-time (float-time)))

(defun fulab-operator--heartbeat-tick (proc buf)
  "Emit a heartbeat line when PROC is idle and BUF is live."
  (if (not (process-live-p proc))
      (when-let ((timer (process-get proc 'fulab-heartbeat-timer)))
        (cancel-timer timer)
        (process-put proc 'fulab-heartbeat-timer nil))
    (let* ((now (float-time))
           (last-output (or (process-get proc 'fulab-last-output-time) now))
           (last-heartbeat (process-get proc 'fulab-last-heartbeat-time)))
      (when (and (>= (- now last-output) fulab-operator-heartbeat-idle-seconds)
                 (or (null last-heartbeat)
                     (>= (- now last-heartbeat) fulab-operator-heartbeat-seconds)))
        (process-put proc 'fulab-last-heartbeat-time now)
        (let ((status (format "waiting for codex stream... (%ds idle)"
                              (floor (- now last-output))))
              (intent (process-get proc 'fulab-intent)))
          (if (featurep 'fubar-hud)
              (let ((hud-buf (get-buffer fubar-hud-buffer-name)))
                (when (buffer-live-p hud-buf)
                  (with-current-buffer hud-buf
                    (when (and intent (or (null fubar-hud--intent)
                                          (string-empty-p fubar-hud--intent)))
                      (setq fubar-hud--intent intent))
                    (setq fubar-hud--operator-status status)
                    (fubar-hud-refresh))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (goto-char (point-max))
                (insert (format "[operator] %s\n" status))
                (insert "           Tip: M-x fulab-operator-open-resume\n")))))))))

(defun fulab-operator--start-heartbeat (proc buf)
  "Start heartbeat timer for PROC writing to BUF."
  (let ((timer (run-at-time 2 fulab-operator-heartbeat-seconds
                            (lambda ()
                              (fulab-operator--heartbeat-tick proc buf)))))
    (process-put proc 'fulab-heartbeat-timer timer)))

(defun fulab-operator--start-fucodex (prompt &rest opts)
  "Run fucodex with PROMPT and OPTS (plist).
Supported keys: :patterns, :clock-in, :aif-select, :session-id, :hud, :intent."
  (let* ((fucodex (fulab-operator--fucodex-path))
         (patterns (plist-get opts :patterns))
         (clock-in (fulab-operator--normalize-clock-in (plist-get opts :clock-in)))
         (aif-select (plist-get opts :aif-select))
         (session-id (plist-get opts :session-id))
         (use-hud (if (plist-member opts :hud)
                      (plist-get opts :hud)
                    fulab-operator-default-use-hud))
         (intent (or (plist-get opts :intent) prompt))
         (buf (fulab-operator--output-buffer use-hud))
         (args (list "--live")))
    (unless (file-executable-p fucodex)
      (error "fucodex not executable at %s" fucodex))
    (when use-hud
      (setq args (append args (list "--hud" "--intent" intent)))
      (fulab-operator--maybe-show-hud intent))
    (when (and patterns (not (string-empty-p patterns)))
      (setq args (append args (list "--patterns" patterns))))
    (when (and clock-in (not (string-empty-p clock-in)))
      (setq args (append args (list "--clock-in" clock-in))))
    (when (and session-id (not (string-empty-p session-id)))
      (setq args (append args (list "--session-id" session-id))))
    (when aif-select
      (setq args (append args (list "--aif-select"))))
    (setq args (append args (list prompt)))
    (setq fulab-operator--last-command (cons fucodex args))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n--- fucodex: %s ---\n"
                      (substring prompt 0 (min 70 (length prompt)))))
      (insert (format "Thinking about: %s\n"
                      (substring intent 0 (min 70 (length intent))))))
    (display-buffer buf)
    (let ((default-directory fulab-operator-futon3-root)
          (proc (apply #'start-process "fucodex" buf fucodex args)))
      (process-put proc 'fulab-intent intent)
      (set-process-filter proc #'fulab-operator--process-filter)
      (process-put proc 'fulab-last-output-time (float-time))
      (fulab-operator--start-heartbeat proc buf)
      (set-process-sentinel proc #'fulab-operator--process-sentinel)
      (message "Started fucodex run.")
      proc)))

;;;###autoload
(defun fulab-operator-open-resume (&optional session-id)
  "Open a terminal and insert a codex resume command for SESSION-ID."
  (interactive
   (let ((default-id fulab-operator--last-session-id))
     (list (read-string (format "Codex session ID%s: "
                                (if default-id
                                    (format " [%s]" (substring default-id 0 (min 8 (length default-id))))
                                  ""))
                        nil nil default-id))))
  (let* ((sid (and session-id (not (string-empty-p session-id)) session-id))
         (cmd (if sid
                  (format "codex resume %s" sid)
                "codex resume "))
         (buf (if (fboundp 'eat)
                  (let ((target-name fulab-operator-resume-buffer-name))
                    (dolist (b (buffer-list))
                      (when (string-prefix-p target-name (buffer-name b))
                        (kill-buffer b)))
                    (let ((eat-buf (eat nil t)))
                      (with-current-buffer eat-buf
                        (rename-buffer target-name t))
                      eat-buf))
                (let ((shell (or (getenv "SHELL") "/bin/bash")))
                  (ansi-term shell fulab-operator-resume-buffer-name)))))
    (let ((sender
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (cond
                  ((fboundp 'eat--send-string)
                   (when-let ((proc (get-buffer-process buf)))
                     (eat--send-string proc cmd)
                     (eat--send-string proc "\n")))
                  ((derived-mode-p 'term-mode)
                   (term-send-raw-string (concat cmd "\n")))
                  ((derived-mode-p 'comint-mode)
                   (comint-send-string (get-buffer-process buf) (concat cmd "\n")))
                  (t
                   (goto-char (point-max))
                   (insert cmd "\n"))))))))
      ;; Delay send so the shell prompt is ready.
      (run-at-time 0.4 nil sender))
    (display-buffer buf)
    (message "Opened resume terminal: %s" (buffer-name buf))))

;;;###autoload
(defun fulab-operator-run-fucodex (prompt patterns &optional aif-select clock-in use-hud intent)
  "Run a generic fucodex live session from Emacs.
PROMPT is the user task, PATTERNS is a CSV string.
When AIF-SELECT is non-nil, let AIF choose the pattern.
When CLOCK-IN is non-nil, pass it as the clock-in pattern."
  (interactive
   (list (read-string "Prompt: ")
         (read-string "Patterns (CSV): ")
         (y-or-n-p "Use AIF select? ")
         (let ((value (read-string "Clock-in pattern (optional): ")))
           (unless (string-empty-p value) value))
         (y-or-n-p "Enable HUD? ")
         (let ((value (read-string "HUD intent (optional): ")))
           (unless (string-empty-p value) value))))
  (fulab-operator--start-fucodex prompt
                                 :patterns patterns
                                 :aif-select aif-select
                                 :clock-in clock-in
                                 :hud use-hud
                                 :intent intent))

;;;###autoload
(defun fulab-operator-run-e01 ()
  "Run E01 evidence ticket via fucodex."
  (interactive)
  (fulab-operator--start-fucodex fulab-operator--e01-prompt
                                 :patterns fulab-operator--e01-patterns
                                 :aif-select t
                                 :intent fulab-operator--e01-intent
                                 :hud t))

;;;###autoload
(defun fulab-operator-run-e01b ()
  "Run E01b evidence ticket via fucodex."
  (interactive)
  (fulab-operator--start-fucodex fulab-operator--e01b-prompt
                                 :patterns fulab-operator--e01b-patterns
                                 :aif-select t
                                 :intent fulab-operator--e01b-intent
                                 :hud t))

;;;###autoload
(defun fulab-operator-run-e02 ()
  "Run E02 evidence ticket via fucodex."
  (interactive)
  (fulab-operator--start-fucodex fulab-operator--e02-prompt
                                 :patterns fulab-operator--e02-patterns
                                 :clock-in fulab-operator--e02-clock-in
                                 :intent fulab-operator--e02-intent
                                 :hud t))

;;;###autoload
(defun fulab-operator-last-session ()
  "Echo the last session id captured from fucodex output."
  (interactive)
  (if fulab-operator--last-session-id
      (message "Last fulab session: %s" fulab-operator--last-session-id)
    (message "No fulab session captured yet.")))

(provide 'fulab-operator)
;;; fulab-operator.el ends here
