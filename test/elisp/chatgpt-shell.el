;;; chatgpt-shell.el --- Minimal stub for aob-chatgpt tests -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar chatgpt-shell-mode-map (make-sparse-keymap)
  "Stub keymap so aob-chatgpt can install bindings in tests.")

(defvar chatgpt-shell-mode-hook nil)
(defvar chatgpt-shell-before-command-functions nil)
(defvar chatgpt-shell-after-command-functions nil)
(defvar chatgpt-shell-system-prompts nil)
(defvar chatgpt-shell-system-prompt nil)
(defvar chatgpt-shell-streaming nil)
(defvar chatgpt-shell-model-version nil)
(defvar chatgpt-shell-openai-key nil)

(define-derived-mode chatgpt-shell-mode fundamental-mode "ChatGPT-Shell"
  "Extremely small stub of chatgpt-shell-mode for non-interactive tests.")

(defun chatgpt-shell ()
  "Return a buffer in `chatgpt-shell-mode' for tests."
  (let ((buf (get-buffer-create "*chatgpt-shell-test*")))
    (with-current-buffer buf
      (chatgpt-shell-mode))
    buf))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here
