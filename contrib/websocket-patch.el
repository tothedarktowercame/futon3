;;; websocket-patch.el --- Patch websocket.el to log outgoing request -*- lexical-binding: t; -*-

;;; Commentary:
;; Advise websocket-open to log the actual HTTP request being sent.

;;; Code:

(require 'websocket)

(defun websocket-log-request (orig-fun url &rest args)
  "Advice to log the actual WebSocket handshake request."
  (let ((result (apply orig-fun url args)))
    result))

;; Patch the internal send to log the request
(defun websocket-patch--log-send (orig-fun process string)
  "Log the string being sent to the WebSocket process."
  (when (string-match-p "^GET " string)
    (message "=== WEBSOCKET REQUEST ===")
    (message "%s" string)
    (message "=== END REQUEST ==="))
  (funcall orig-fun process string))

(advice-add 'process-send-string :around #'websocket-patch--log-send)

(defun websocket-patch-remove ()
  "Remove the logging patch."
  (interactive)
  (advice-remove 'process-send-string #'websocket-patch--log-send)
  (message "Patch removed"))

(provide 'websocket-patch)
;;; websocket-patch.el ends here
