;;; fubar-hud.el --- HUD display for fulab sessions -*- lexical-binding: t -*-

;; Provides a Heads-Up Display showing pattern context for fulab sessions.
;; Humans see the same patterns/AIF signals that AI agents see.

;;; Commentary:

;; This module displays the fulab HUD - the shared context surface for
;; pattern-aware sessions. It shows:
;; - Current intent and sigils
;; - Pattern candidates with scores
;; - AIF suggestion and confidence (tau)
;; - Agent report (after turn completion)
;;
;; Usage:
;;   (fubar-hud-show)           ; Show HUD in side window
;;   (fubar-hud-refresh)        ; Refresh with current intent
;;   (fubar-hud-set-intent ...) ; Set intent and rebuild HUD

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(defgroup fubar-hud nil
  "HUD display for fulab pattern-aware sessions."
  :group 'fubar
  :prefix "fubar-hud-")

(defcustom fubar-hud-buffer-name "*FuLab HUD*"
  "Name of the HUD buffer."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-futon3-root
  (expand-file-name "~/code/futon3")
  "Root directory of futon3 repository."
  :type 'directory
  :group 'fubar-hud)

(defcustom fubar-hud-server-url
  "http://localhost:5050"
  "Base URL for the futon3 server HUD endpoint."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-certify-commit t
  "When non-nil, request git commit certificates for HUD output."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-clojure-deps
  "{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"} futon2/futon2 {:local/root \"../futon2\"}}}"
  "Clojure deps for running HUD commands (fallback when server unavailable)."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-window-width 50
  "Width of HUD side window."
  :type 'integer
  :group 'fubar-hud)

(defcustom fubar-hud-pattern-click-action 'fubar-hud-browse-pattern
  "Function to call when a pattern ID is clicked."
  :type 'function
  :group 'fubar-hud)

(defcustom fubar-hud-stream-buffer-name "*FuLab Raw Stream*"
  "Name of the buffer showing the live fucodex stream."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-staging-file "/tmp/fulab-next.txt"
  "Path for storing the staged next-move text."
  :type 'file
  :group 'fubar-hud)

(defcustom fubar-hud-pause-flag-file "/tmp/fulab-stream-paused"
  "Path for the pause flag file used by the stream formatter."
  :type 'file
  :group 'fubar-hud)

(defcustom fubar-hud-auto-refresh-seconds 2
  "Seconds between HUD auto-refreshes when the HUD buffer is visible."
  :type 'number
  :group 'fubar-hud)

;;; State

(defvar-local fubar-hud--current-hud nil
  "Current HUD state as plist.")

(defvar-local fubar-hud--prompt-block nil
  "Cached HUD prompt block.")

(defvar-local fubar-hud--intent nil
  "Current intent string.")

(defvar-local fubar-hud--session-id nil
  "Current session ID.")

(defvar fubar-hud--stream-paused nil
  "Non-nil when the stream process is paused.")

(defvar fubar-hud--staged-next nil
  "Staged next-move text, if any.")

(defvar fubar-hud--auto-refresh-timer nil
  "Timer used to auto-refresh the HUD.")

;;; Faces

(defface fubar-hud-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for HUD section headers."
  :group 'fubar-hud)

(defface fubar-hud-intent-face
  '((t :inherit font-lock-string-face))
  "Face for intent text."
  :group 'fubar-hud)

(defface fubar-hud-sigil-face
  '((t :inherit font-lock-constant-face :height 1.2))
  "Face for sigils."
  :group 'fubar-hud)

(defface fubar-hud-pattern-id-face
  '((t :inherit link))
  "Face for pattern IDs."
  :group 'fubar-hud)

(defface fubar-hud-score-face
  '((t :inherit font-lock-number-face))
  "Face for scores."
  :group 'fubar-hud)

(defface fubar-hud-aif-suggestion-face
  '((t :inherit success :weight bold))
  "Face for AIF suggested pattern."
  :group 'fubar-hud)

(defface fubar-hud-tau-face
  '((t :inherit font-lock-type-face))
  "Face for tau (confidence) value."
  :group 'fubar-hud)

;;; HUD Building (server-first)

(defun fubar-hud--parse-json (text)
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (json-read-from-string text)))

(defun fubar-hud--http-post-json (url payload)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("content-type" . "application/json")))
         (url-request-data (json-encode payload))
         (buf (url-retrieve-synchronously url t t 5.0)))
    (when buf
      (with-current-buffer buf
        (unwind-protect
            (when (boundp 'url-http-end-of-headers)
              (goto-char url-http-end-of-headers)
              (let ((body (buffer-substring-no-properties (point) (point-max))))
                (fubar-hud--parse-json body)))
          (kill-buffer buf))))))

(defun fubar-hud--build-hud-server (intent &optional patterns)
  "Build HUD via futon3 server."
  (let* ((prototypes (when (and patterns (not (string-empty-p patterns)))
                       (split-string patterns "," t "[[:space:]]+")))
         (payload `(("intent" . ,(or intent "unspecified"))
                    ("prototypes" . ,prototypes)
                    ("pattern-limit" . 4)
                    ("certify-commit" . ,(and fubar-hud-certify-commit t))
                    ("repo-root" . ,fubar-hud-futon3-root)))
         (url (concat (replace-regexp-in-string "/$" "" fubar-hud-server-url)
                      "/fulab/hud/format"))
         (result (ignore-errors (fubar-hud--http-post-json url payload))))
    (when (and result (plist-get result :ok))
      (let ((hud (plist-get result :hud))
            (prompt (plist-get result :prompt)))
        (when prompt
          (setq fubar-hud--prompt-block prompt))
        hud))))

(defun fubar-hud--run-clj (command &rest args)
  "Run lab-hud.clj COMMAND with ARGS, return output."
  (let* ((default-directory fubar-hud-futon3-root)
         (cmd (format "clojure -Sdeps '%s' -M dev/lab-hud.clj %s %s"
                      fubar-hud-clojure-deps
                      command
                      (mapconcat #'shell-quote-argument args " "))))
    (shell-command-to-string cmd)))

(defun fubar-hud--build-hud (intent &optional patterns)
  "Build HUD for INTENT, optionally seeding PATTERNS."
  (or (fubar-hud--build-hud-server intent patterns)
      (let* ((args (list "--intent" (or intent "unspecified")))
             (args (if patterns
                       (append args (list "--prototypes" patterns))
                     args))
             (output (apply #'fubar-hud--run-clj "build" args)))
        (condition-case nil
            (car (read-from-string output))
          (error nil)))))

(defun fubar-hud--format-hud (hud)
  "Get formatted prompt block for HUD."
  (or fubar-hud--prompt-block
      (let ((temp-file (make-temp-file "fubar-hud-" nil ".edn")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (prin1 hud (current-buffer)))
              (fubar-hud--run-clj "format" "--hud-file" temp-file))
          (delete-file temp-file)))))

;;; Rendering

(defun fubar-hud--normalize-sigil-text (value)
  "Normalize sigil VALUE to a displayable string."
  (cond
   ((stringp value) (decode-coding-string value 'utf-8 t))
   (value (format "%s" value))
   (t "")))

(defun fubar-hud--render-sigils (sigils)
  "Render SIGILS as formatted string."
  (if (and sigils (> (length sigils) 0))
      (mapconcat (lambda (s)
                   (let ((emoji (fubar-hud--normalize-sigil-text (plist-get s :emoji)))
                         (hanzi (fubar-hud--normalize-sigil-text (plist-get s :hanzi))))
                     (propertize (format "%s/%s" emoji hanzi)
                                 'face 'fubar-hud-sigil-face)))
                 sigils " ")
    (propertize "none" 'face 'font-lock-comment-face)))

(defun fubar-hud--make-pattern-button (pattern-id)
  "Create clickable button for PATTERN-ID."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive)
                                 (funcall fubar-hud-pattern-click-action pattern-id)))
    (define-key map (kbd "RET") (lambda () (interactive)
                                   (funcall fubar-hud-pattern-click-action pattern-id)))
    (propertize pattern-id
                'face 'fubar-hud-pattern-id-face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "Click to view %s" pattern-id))))

(defun fubar-hud--render-candidate (candidate index)
  "Render CANDIDATE at INDEX."
  (let* ((id (plist-get candidate :id))
         (score (plist-get candidate :score))
         (summary (plist-get candidate :summary)))
    (concat
     (format "%d. " (1+ index))
     (fubar-hud--make-pattern-button id)
     " "
     (propertize (format "(%.2f)" score) 'face 'fubar-hud-score-face)
     "\n"
     (when (and summary (not (string-empty-p summary)))
       (concat "   " (propertize (truncate-string-to-width summary 40 nil nil "...")
                                 'face 'font-lock-doc-face)
               "\n")))))

(defun fubar-hud--render-aif (aif)
  "Render AIF section."
  (if aif
      (let ((suggested (plist-get aif :suggested))
            (tau (plist-get aif :tau))
            (g-scores (plist-get aif :G-scores))
            (rationale (plist-get aif :rationale)))
        (concat
         (propertize "AIF Suggestion\n" 'face 'fubar-hud-header-face)
         (if suggested
             (concat
              "  Pattern: "
              (propertize suggested 'face 'fubar-hud-aif-suggestion-face)
              "\n"
              (when tau
                (format "  Confidence (τ): %s\n"
                        (propertize (format "%.2f" tau) 'face 'fubar-hud-tau-face)))
              (when-let ((g (and g-scores (cdr (assoc suggested g-scores)))))
                (format "  Expected FE (G): %.3f\n" g))
              (when rationale
                (format "  Rationale: %s\n" rationale)))
           "  No suggestion\n")))
    ""))

(defun fubar-hud--render-agent-report (report)
  "Render agent REPORT section."
  (if report
      (concat
       (propertize "Agent Report\n" 'face 'fubar-hud-header-face)
       (if-let ((applied (plist-get report :applied)))
           (concat
            "  Applied: "
            (fubar-hud--make-pattern-button applied)
            "\n"
            (when-let ((notes (plist-get report :notes)))
              (format "  Notes: %s\n" notes)))
         "  No pattern reported\n"))
    ""))

(defun fubar-hud--render (hud)
  "Render HUD to current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "━━━ FuLab HUD ━━━\n\n" 'face 'fubar-hud-header-face))

    ;; Session
    (when fubar-hud--session-id
      (insert (propertize "Session\n" 'face 'fubar-hud-header-face))
      (insert "  " (propertize fubar-hud--session-id 'face 'font-lock-constant-face) "\n\n"))

    ;; Intent
    (insert (propertize "Intent\n" 'face 'fubar-hud-header-face))
    (insert "  " (propertize (or (plist-get hud :intent) "unspecified")
                              'face 'fubar-hud-intent-face)
            "\n\n")

    ;; Sigils
    (insert (propertize "Sigils\n" 'face 'fubar-hud-header-face))
    (insert "  " (fubar-hud--render-sigils (plist-get hud :sigils)) "\n\n")

    ;; Candidates
    (insert (propertize "Pattern Candidates\n" 'face 'fubar-hud-header-face))
    (let ((candidates (plist-get hud :candidates)))
      (if (and candidates (> (length candidates) 0))
          (cl-loop for c in candidates
                   for i from 0
                   do (insert (fubar-hud--render-candidate c i)))
        (insert "  " (propertize "No candidates found" 'face 'font-lock-comment-face) "\n")))
    (insert "\n")

    ;; AIF
    (insert (fubar-hud--render-aif (plist-get hud :aif)))
    (insert "\n")

    ;; Staged Next Move
    (when fubar-hud--staged-next
      (insert (propertize "Staged Next\n" 'face 'fubar-hud-header-face))
      (insert "  " (propertize fubar-hud--staged-next 'face 'font-lock-string-face) "\n\n"))

    ;; Agent Report (if present)
    (when (plist-get hud :agent-report)
      (insert (fubar-hud--render-agent-report (plist-get hud :agent-report))))

    ;; Footer
    (insert "\n" (propertize "━━━━━━━━━━━━━━━━━━\n" 'face 'font-lock-comment-face))
    (insert (propertize (format-time-string "Updated: %H:%M:%S")
                        'face 'font-lock-comment-face))

    (goto-char (point-min))))

;;; Pattern Actions

(defun fubar-hud-browse-pattern (pattern-id)
  "Browse to PATTERN-ID in the library."
  (let* ((parts (split-string pattern-id "/"))
         (namespace (car parts))
         (name (cadr parts))
         (paths (list
                 (expand-file-name (format "library/%s/%s.flexiarg" namespace name)
                                   fubar-hud-futon3-root)
                 (expand-file-name (format "library/%s.flexiarg" pattern-id)
                                   fubar-hud-futon3-root)
                 (expand-file-name (format "holes/LDTS/%s/%s.flexiarg" namespace name)
                                   fubar-hud-futon3-root))))
    (if-let ((found (cl-find-if #'file-exists-p paths)))
        (find-file-other-window found)
      (message "Pattern file not found: %s" pattern-id))))

(defun fubar-hud-copy-pattern-id ()
  "Copy pattern ID at point to kill ring."
  (interactive)
  (when-let ((id (get-text-property (point) 'fubar-hud-pattern-id)))
    (kill-new id)
    (message "Copied: %s" id)))

;;; Interactive Commands

(defun fubar-hud-set-intent (intent)
  "Set INTENT and rebuild HUD."
  (interactive "sIntent: ")
  (setq fubar-hud--intent intent)
  (fubar-hud-refresh))

(defun fubar-hud-set-session-id (session-id)
  "Set SESSION-ID and rebuild HUD."
  (interactive "sSession ID: ")
  (setq fubar-hud--session-id session-id)
  (fubar-hud-refresh))

(defun fubar-hud-refresh ()
  "Refresh HUD with current intent."
  (interactive)
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      (setq fubar-hud--prompt-block nil)
      (setq fubar-hud--staged-next (fubar-hud--read-staging-file))
      (let ((hud (fubar-hud--build-hud fubar-hud--intent)))
        (when hud
          (setq fubar-hud--current-hud hud)
          (fubar-hud--render hud))))))

(defun fubar-hud-show ()
  "Show HUD in side window."
  (interactive)
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      (fubar-hud-mode)
      (unless fubar-hud--current-hud
        (fubar-hud-refresh)))
    (display-buffer-in-side-window
     buf
     `((side . right)
       (window-width . ,fubar-hud-window-width)
       (slot . 0))))
  (fubar-hud--maybe-start-auto-refresh))

(defun fubar-hud-hide ()
  "Hide HUD window."
  (interactive)
  (when-let ((win (get-buffer-window fubar-hud-buffer-name)))
    (delete-window win))
  (unless (get-buffer-window fubar-hud-buffer-name)
    (fubar-hud--stop-auto-refresh)))

(defun fubar-hud--maybe-start-auto-refresh ()
  "Start auto-refresh timer when HUD buffer is visible."
  (when (and (get-buffer-window fubar-hud-buffer-name)
             (null fubar-hud--auto-refresh-timer)
             (numberp fubar-hud-auto-refresh-seconds))
    (setq fubar-hud--auto-refresh-timer
          (run-at-time 0 fubar-hud-auto-refresh-seconds
                       (lambda ()
                         (when (get-buffer-window fubar-hud-buffer-name)
                           (with-current-buffer fubar-hud-buffer-name
                             (fubar-hud-refresh))))))))

(defun fubar-hud--stop-auto-refresh ()
  "Stop HUD auto-refresh timer."
  (when fubar-hud--auto-refresh-timer
    (cancel-timer fubar-hud--auto-refresh-timer)
    (setq fubar-hud--auto-refresh-timer nil)))

(defun fubar-hud-toggle ()
  "Toggle HUD visibility."
  (interactive)
  (if (get-buffer-window fubar-hud-buffer-name)
      (fubar-hud-hide)
      (fubar-hud-show)))

(defun fubar-hud-get-prompt-block ()
  "Get current HUD as prompt block for injection."
  (interactive)
  (if fubar-hud--current-hud
      (let ((block (fubar-hud--format-hud fubar-hud--current-hud)))
        (when (called-interactively-p 'any)
          (kill-new block)
          (message "HUD block copied to kill ring"))
        block)
    (message "No HUD loaded. Use fubar-hud-set-intent first.")
    nil))

(defun fubar-hud--stream-process ()
  "Return the process for the stream buffer, or nil."
  (when-let ((buf (get-buffer fubar-hud-stream-buffer-name)))
    (get-buffer-process buf)))

(defun fubar-hud-toggle-stream-pause ()
  "Pause or resume the stream process."
  (interactive)
  (let ((proc (fubar-hud--stream-process)))
    (if (not (process-live-p proc))
        (message "No live stream process for %s" fubar-hud-stream-buffer-name)
      (if fubar-hud--stream-paused
          (progn
            (process-send-signal 'SIGCONT proc)
            (setq fubar-hud--stream-paused nil)
            (when (file-exists-p fubar-hud-pause-flag-file)
              (delete-file fubar-hud-pause-flag-file))
            (message "Stream resumed"))
        (process-send-signal 'SIGSTOP proc)
        (setq fubar-hud--stream-paused t)
        (with-temp-file fubar-hud-pause-flag-file
          (insert "paused\n"))
        (message "Stream paused")))))

(defun fubar-hud--read-staging-file ()
  "Return the staged next-move text if present."
  (when (file-exists-p fubar-hud-staging-file)
    (let ((text (string-trim (with-temp-buffer
                               (insert-file-contents fubar-hud-staging-file)
                               (buffer-string)))))
      (unless (string-empty-p text)
        text))))

(defun fubar-hud-accept-staged ()
  "Run fucodex with the staged next-move text."
  (interactive)
  (if-let ((text (or fubar-hud--staged-next (fubar-hud--read-staging-file))))
      (let* ((default-directory fubar-hud-futon3-root)
             (resume-args (if fubar-hud--session-id
                            (list "--live" "resume" fubar-hud--session-id text)
                            (list "--live" "resume" "--last" text))))
        (apply #'start-process
               "fucodex-resume"
               "*FuLab Resume*"
               "/home/joe/code/futon3/fucodex"
               resume-args)
        (message "Sent staged next-move to fucodex."))
    (message "No staged next-move found.")))

;;; Mode Definition

(defvar fubar-hud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'fubar-hud-refresh)
    (define-key map "i" #'fubar-hud-set-intent)
    (define-key map "q" #'fubar-hud-hide)
    (define-key map "w" #'fubar-hud-copy-pattern-id)
    (define-key map "p" #'fubar-hud-toggle-stream-pause)
    (define-key map "a" #'fubar-hud-accept-staged)
    (define-key map "y" #'fubar-hud-get-prompt-block)
    map)
  "Keymap for `fubar-hud-mode'.")

(define-derived-mode fubar-hud-mode special-mode "FuLab-HUD"
  "Major mode for FuLab HUD display.

\\{fubar-hud-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; Integration Helpers

(defun fubar-hud-for-buffer (&optional buffer)
  "Build HUD for BUFFER (defaults to current) based on file context."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (file (buffer-file-name buf))
         (intent (cond
                  ((and file (string-match-p "test" file))
                   "writing tests")
                  ((and file (string-match-p "schema" file))
                   "defining schema")
                  ((and file (string-match-p "\\.clj$" file))
                   "Clojure development")
                  (t "coding task"))))
    (fubar-hud-set-intent intent)
    (fubar-hud-show)))

(defun fubar-hud-inject-into-prompt (prompt)
  "Return PROMPT with HUD block prepended."
  (if-let ((block (fubar-hud-get-prompt-block)))
      (concat block "\n\nUser task: " prompt)
    prompt))

(provide 'fubar-hud)
;;; fubar-hud.el ends here
