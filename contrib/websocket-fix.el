;;; websocket-fix.el --- Fix websocket.el missing leading slash -*- lexical-binding: t; -*-

;;; Commentary:
;; websocket.el sends "GET ?query..." instead of "GET /?query..." when
;; the URL has no path component (e.g., wss://host:port?query=...).
;; This is invalid HTTP and nginx returns 400 Bad Request.
;;
;; The bug is in websocket-ensure-handshake which uses url-filename
;; without ensuring it starts with "/".

;;; Code:

(require 'websocket)

(defun websocket-fix--ensure-leading-slash (orig-fun url conn key protocols extensions custom-header-alist nowait)
  "Advice to fix missing leading slash in WebSocket path."
  (let* ((url-struct (url-generic-parse-url url))
         (path (url-filename url-struct)))
    ;; If path doesn't start with /, prepend it
    (when (and (> (length path) 0)
               (not (string-prefix-p "/" path)))
      ;; Modify the url-struct to have correct path
      (setf (url-filename url-struct) (concat "/" path))
      ;; Reconstruct URL - but we actually need to fix the request directly
      ))
  ;; The above doesn't work because we can't easily modify the URL.
  ;; Instead, let's replace the function entirely for now.
  (funcall orig-fun url conn key protocols extensions custom-header-alist nowait))

;; Actually, let's just override the internal send with correct path
(defun websocket-fix--send-handshake (orig-fun conn string)
  "Fix the GET line to ensure leading slash."
  (when (string-match "^GET \\([^/ ]\\)" string)
    ;; Path doesn't start with / - fix it
    (setq string (replace-regexp-in-string "^GET " "GET /" string)))
  (funcall orig-fun conn string))

(advice-add 'process-send-string :around #'websocket-fix--send-handshake)

(defun websocket-fix-remove ()
  "Remove the fix."
  (interactive)
  (advice-remove 'process-send-string #'websocket-fix--send-handshake)
  (message "Websocket fix removed"))

(provide 'websocket-fix)
;;; websocket-fix.el ends here
