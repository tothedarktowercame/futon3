;;; claude-repl.el --- Claude REPL via Drawbridge -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Emacs peripheral for Claude subprocess
;;
;; Routes input through Agency → Drawbridge → Subprocess
;; Shares session with IRC, other peripherals, etc.
;;
;; Usage:
;;   (load "/home/joe/code/futon3/emacs/claude-repl.el")
;;   M-x claude-repl
;;
;; Or add to init.el:
;;   (load "/home/joe/code/futon3/emacs/claude-repl.el")
;;   (global-set-key (kbd "C-c C-l") 'claude-repl)

(require 'comint)
(require 'json)
(require 'url)
(require 'url-http)

;;; Configuration

(defgroup claude-repl nil
  "Claude REPL via Drawbridge."
  :group 'applications)

(defcustom claude-repl-agency-url "http://localhost:7070"
  "URL of the Agency server."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-agent-id "claude-test"
  "Agent ID for the Claude drawbridge."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-timeout 120
  "Timeout in seconds for Claude responses."
  :type 'integer
  :group 'claude-repl)

;;; Faces

(defface claude-repl-prompt-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for the user prompt."
  :group 'claude-repl)

(defface claude-repl-response-face
  '((t :foreground "#8be9fd"))
  "Face for Claude's response."
  :group 'claude-repl)

(defface claude-repl-error-face
  '((t :foreground "#ff5555"))
  "Face for errors."
  :group 'claude-repl)

;;; Core Functions

(defvar claude-repl--buffer-name "*claude-repl*"
  "Name of the Claude REPL buffer.")

(defun claude-repl--send-sync (prompt)
  "Send PROMPT to Claude via Agency and return response synchronously."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode `(("agent-id" . ,claude-repl-agent-id)
                          ("prompt" . ,prompt)))
           'utf-8))
         (url (concat claude-repl-agency-url "/agency/page"))
         (buffer (url-retrieve-synchronously url nil nil claude-repl-timeout)))
    (if buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          ;; Skip HTTP headers
          (re-search-forward "^$" nil t)
          (forward-char 1)
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (response (condition-case err
                               (json-read)
                             (error (list (cons 'error (format "JSON parse error: %s" err)))))))
            (kill-buffer buffer)
            (if (alist-get 'ok response)
                (or (alist-get 'result (alist-get 'response response))
                    (alist-get 'error (alist-get 'response response))
                    "No response")
              (or (alist-get 'error response)
                  (alist-get 'err response)
                  "Unknown error"))))
      "Connection failed")))

(defun claude-repl--input-sender (proc input)
  "Send INPUT to Claude and display response.
PROC is the comint process (dummy for our purposes)."
  (let ((inhibit-read-only t))
    ;; Show thinking indicator
    (goto-char (point-max))
    (insert (propertize "..." 'face 'claude-repl-response-face))
    (let ((thinking-start (- (point) 3)))
      (redisplay)
      ;; Get response
      (let ((response (claude-repl--send-sync input)))
        ;; Remove thinking indicator
        (delete-region thinking-start (point))
        ;; Insert response with claude> prefix
        (insert (propertize "claude> " 'face 'claude-repl-response-face)
                (propertize (concat response "\n\n")
                            'face 'claude-repl-response-face)
                ;; Add next user prompt
                (propertize "you> " 'face 'claude-repl-prompt-face
                            'rear-nonsticky t
                            'front-sticky '(read-only)))))))

;;; Mode Definition

(defvar claude-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'claude-repl-interrupt)
    (define-key map (kbd "C-c C-k") 'claude-repl-clear)
    map)
  "Keymap for `claude-repl-mode'.")

(define-derived-mode claude-repl-mode comint-mode "Claude-REPL"
  "Major mode for Claude REPL via Drawbridge.

Routes all input through Agency → Drawbridge → Claude subprocess,
sharing the session with IRC and other peripherals.

\\{claude-repl-mode-map}"
  (setq-local comint-prompt-regexp "^you> ")
  (setq-local comint-input-sender 'claude-repl--input-sender)
  (setq-local comint-process-echoes nil)
  ;; Don't need a real process, but comint wants one
  (unless (comint-check-proc (current-buffer))
    (let ((fake-proc (start-process "claude-repl" (current-buffer) "cat")))
      (set-process-query-on-exit-flag fake-proc nil)
      (setq-local comint-ptyp process-connection-type)))
  ;; Initial prompt
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "Claude REPL " 'face 'bold)
            (propertize (format "(agent: %s)\n" claude-repl-agent-id)
                        'face 'font-lock-comment-face)
            (propertize "Routed through: " 'face 'font-lock-comment-face)
            (propertize (format "%s\n" claude-repl-agency-url)
                        'face 'font-lock-comment-face)
            (propertize "Commands: " 'face 'font-lock-comment-face)
            "C-c C-k clear, C-c C-c interrupt\n\n"
            (propertize "you> " 'face 'claude-repl-prompt-face
                        'rear-nonsticky t
                        'front-sticky '(read-only)))))

(defun claude-repl-clear ()
  "Clear the REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (claude-repl-mode))

(defun claude-repl-interrupt ()
  "Interrupt current operation (placeholder)."
  (interactive)
  (message "Interrupt not implemented - response is synchronous"))

;;;###autoload
(defun claude-repl ()
  "Start or switch to the Claude REPL buffer."
  (interactive)
  (let ((buf (get-buffer-create claude-repl--buffer-name)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'claude-repl-mode)
      (claude-repl-mode))))

;;;###autoload
(defun claude-repl-send-region (start end)
  "Send region from START to END to Claude REPL."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (claude-repl)
    (goto-char (point-max))
    (insert text)
    (comint-send-input)))

;;;###autoload
(defun claude-repl-send-buffer ()
  "Send entire buffer to Claude REPL."
  (interactive)
  (claude-repl-send-region (point-min) (point-max)))

(provide 'claude-repl)

;;; claude-repl.el ends here
