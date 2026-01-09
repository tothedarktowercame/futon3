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

(defcustom fubar-hud-clojure-deps
  "{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"} futon2/futon2 {:local/root \"../futon2\"}}}"
  "Clojure deps for running HUD commands."
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

;;; State

(defvar-local fubar-hud--current-hud nil
  "Current HUD state as plist.")

(defvar-local fubar-hud--intent nil
  "Current intent string.")

(defvar-local fubar-hud--session-id nil
  "Current session ID.")

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

;;; HUD Building (calls Clojure)

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
  (let* ((args (list "--intent" (or intent "unspecified")))
         (args (if patterns
                   (append args (list "--prototypes" patterns))
                 args))
         (output (apply #'fubar-hud--run-clj "build" args)))
    (condition-case nil
        (car (read-from-string output))
      (error nil))))

(defun fubar-hud--format-hud (hud)
  "Get formatted prompt block for HUD."
  (let ((temp-file (make-temp-file "fubar-hud-" nil ".edn")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (prin1 hud (current-buffer)))
          (fubar-hud--run-clj "format" "--hud-file" temp-file))
      (delete-file temp-file))))

;;; Rendering

(defun fubar-hud--render-sigils (sigils)
  "Render SIGILS as formatted string."
  (if (and sigils (> (length sigils) 0))
      (mapconcat (lambda (s)
                   (let ((emoji (plist-get s :emoji))
                         (hanzi (plist-get s :hanzi)))
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

(defun fubar-hud-refresh ()
  "Refresh HUD with current intent."
  (interactive)
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
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
       (slot . 0)))))

(defun fubar-hud-hide ()
  "Hide HUD window."
  (interactive)
  (when-let ((win (get-buffer-window fubar-hud-buffer-name)))
    (delete-window win)))

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

;;; Mode Definition

(defvar fubar-hud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'fubar-hud-refresh)
    (define-key map "i" #'fubar-hud-set-intent)
    (define-key map "q" #'fubar-hud-hide)
    (define-key map "w" #'fubar-hud-copy-pattern-id)
    (define-key map "p" #'fubar-hud-get-prompt-block)
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
