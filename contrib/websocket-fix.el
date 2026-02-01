;;; websocket-fix.el --- Fix websocket.el for Emacs 31 -*- lexical-binding: t; -*-

;;; Commentary:
;; Patch websocket.el to explicitly set binary coding system on TLS connections.
;; The original code uses let-bound coding-system-for-read/write but this may
;; not work correctly with Emacs 31's TLS handling.

;;; Code:

(require 'websocket)

(defun websocket-fix--ensure-binary-coding (orig-fun url &rest args)
  "Advice to ensure binary coding system on the connection."
  (let ((result (apply orig-fun url args)))
    (when (and result (websocket-p result))
      (let ((conn (websocket-conn result)))
        (when (processp conn)
          (set-process-coding-system conn 'binary 'binary))))
    result))

(advice-add 'websocket-open :around #'websocket-fix--ensure-binary-coding)

(defun websocket-fix-remove ()
  "Remove the fix."
  (interactive)
  (advice-remove 'websocket-open #'websocket-fix--ensure-binary-coding))

(provide 'websocket-fix)
;;; websocket-fix.el ends here
