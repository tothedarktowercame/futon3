;;; websocket-fix.el --- Fix websocket.el for Emacs 31 -*- lexical-binding: t; -*-

;;; Commentary:
;; Patch websocket.el to explicitly set binary coding system on TLS connections
;; BEFORE the handshake is sent.

;;; Code:

(require 'websocket)

;; Advise the handshake function to set coding system before sending
(defun websocket-fix--before-handshake (orig-fun url conn key protocols extensions custom-header-alist nowait)
  "Set binary coding system before handshake is sent."
  (when (processp conn)
    (set-process-coding-system conn 'binary 'binary))
  (funcall orig-fun url conn key protocols extensions custom-header-alist nowait))

(advice-add 'websocket-ensure-handshake :around #'websocket-fix--before-handshake)

;; Also try advising open-network-stream to ensure binary coding
(defun websocket-fix--after-open-stream (result &rest _args)
  "Set binary coding on newly opened stream."
  (when (processp result)
    (set-process-coding-system result 'binary 'binary))
  result)

(advice-add 'open-network-stream :filter-return #'websocket-fix--after-open-stream)

(defun websocket-fix-remove ()
  "Remove the fix."
  (interactive)
  (advice-remove 'websocket-ensure-handshake #'websocket-fix--before-handshake)
  (advice-remove 'open-network-stream #'websocket-fix--after-open-stream)
  (message "Websocket fix removed"))

(provide 'websocket-fix)
;;; websocket-fix.el ends here
