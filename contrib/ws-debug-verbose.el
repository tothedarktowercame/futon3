;;; ws-debug-verbose.el --- Verbose WebSocket debugging -*- lexical-binding: t; -*-

;;; Commentary:
;; Extra verbose debugging to see what websocket.el is actually sending.
;; Also tries some workarounds for TLS/ALPN issues.

;;; Code:

(require 'websocket)
(require 'url-http)

;; Enable all debugging
(setq websocket-debug t)
(setq url-debug t)

(defun ws-debug-verbose-connect ()
  "Connect with extra debugging and potential workarounds."
  (interactive)
  (let* ((url "wss://172-236-28-208.ip.linodeusercontent.com:5057?path=%2Ftmp%2Ftest.jsonl")
         ;; Try forcing HTTP/1.1 (disable HTTP/2)
         (url-mime-accept-string "*/*")
         ;; Ensure we use the right TLS settings
         (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

    (message "=== Verbose WebSocket Debug ===")
    (message "URL: %s" url)
    (message "gnutls-algorithm-priority: %s" gnutls-algorithm-priority)

    ;; Show websocket.el version
    (message "websocket.el version: %s"
             (if (boundp 'websocket-version) websocket-version "unknown"))

    ;; Try connection
    (condition-case err
        (let ((ws (websocket-open
                   url
                   :on-open (lambda (ws)
                              (message ">>> ON-OPEN <<<")
                              (message "ws ready-state: %s" (websocket-ready-state ws)))
                   :on-message (lambda (ws frame)
                                 (message ">>> ON-MESSAGE <<<")
                                 (message "frame: %S" frame)
                                 (when (websocket-frame-p frame)
                                   (message "text: %s" (websocket-frame-text frame))))
                   :on-close (lambda (ws)
                               (message ">>> ON-CLOSE <<<"))
                   :on-error (lambda (ws type err)
                               (message ">>> ON-ERROR <<<")
                               (message "type: %s" type)
                               (message "err: %S" err)))))
          (message "websocket-open returned: %S" ws)
          ws)
      (error
       (message "!!! ERROR: %S" err)
       nil))))

;; Also try with explicit custom headers
(defun ws-debug-verbose-connect-custom ()
  "Try connection with explicit headers."
  (interactive)
  (let ((url "wss://172-236-28-208.ip.linodeusercontent.com:5057?path=%2Ftmp%2Ftest.jsonl"))
    (message "=== Custom Headers Debug ===")
    (condition-case err
        (let ((ws (websocket-open
                   url
                   :custom-header-alist '(("User-Agent" . "Emacs-WebSocket/1.0"))
                   :on-open (lambda (ws) (message "OPEN"))
                   :on-message (lambda (ws frame)
                                 (message "MSG: %s"
                                          (and (websocket-frame-p frame)
                                               (substring (websocket-frame-text frame) 0
                                                          (min 100 (length (websocket-frame-text frame)))))))
                   :on-close (lambda (ws) (message "CLOSE"))
                   :on-error (lambda (ws type err) (message "ERR: %s %S" type err)))))
          (message "Connected: %S" ws))
      (error (message "Failed: %S" err)))))

(provide 'ws-debug-verbose)
;;; ws-debug-verbose.el ends here
