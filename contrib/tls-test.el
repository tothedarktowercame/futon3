;;; tls-test.el --- Test TLS connections bypassing websocket.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Isolate whether the 400 error is from websocket.el or Emacs TLS layer.

;;; Code:

(defun tls-test-plain-https ()
  "Test plain HTTPS request (no WebSocket upgrade)."
  (interactive)
  (let ((buf (get-buffer-create "*tls-test*")))
    (with-current-buffer buf (erase-buffer))
    (condition-case err
        (let ((proc (open-network-stream
                     "tls-test"
                     buf
                     "172-236-28-208.ip.linodeusercontent.com"
                     5057
                     :type 'tls
                     :nowait nil)))
          (when proc
            (message "TLS connection opened, sending request...")
            (process-send-string
             proc
             "GET / HTTP/1.1\r\nHost: 172-236-28-208.ip.linodeusercontent.com:5057\r\nConnection: close\r\n\r\n")
            (sleep-for 2)
            (pop-to-buffer buf)
            (message "Response in buffer")))
      (error
       (message "TLS connection failed: %S" err)
       (pop-to-buffer buf)))))

(defun tls-test-websocket-upgrade ()
  "Test WebSocket upgrade request via raw TLS."
  (interactive)
  (let ((buf (get-buffer-create "*tls-test*")))
    (with-current-buffer buf (erase-buffer))
    (condition-case err
        (let ((proc (open-network-stream
                     "tls-test"
                     buf
                     "172-236-28-208.ip.linodeusercontent.com"
                     5057
                     :type 'tls
                     :nowait nil)))
          (when proc
            (message "TLS connection opened, sending WebSocket upgrade...")
            (process-send-string
             proc
             (concat
              "GET /?path=/tmp/test.jsonl HTTP/1.1\r\n"
              "Host: 172-236-28-208.ip.linodeusercontent.com:5057\r\n"
              "Upgrade: websocket\r\n"
              "Connection: Upgrade\r\n"
              "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
              "Sec-WebSocket-Version: 13\r\n"
              "\r\n"))
            (sleep-for 2)
            (pop-to-buffer buf)
            (message "Response in buffer")))
      (error
       (message "TLS connection failed: %S" err)
       (pop-to-buffer buf)))))

(defun tls-test-check-alpn ()
  "Check ALPN-related info."
  (interactive)
  (message "gnutls-available-p: %S" (gnutls-available-p))
  (message "Emacs version: %s" (emacs-version))
  (when (boundp 'gnutls-algorithm-priority)
    (message "gnutls-algorithm-priority: %S" gnutls-algorithm-priority)))

(provide 'tls-test)
;;; tls-test.el ends here
