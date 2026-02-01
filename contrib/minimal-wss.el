;;; minimal-wss.el --- Minimal WSS client bypassing websocket.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Dead simple WSS client to isolate what websocket.el does wrong.

;;; Code:

(defun minimal-wss-connect (host port path)
  "Connect to HOST:PORT via TLS and do WebSocket handshake for PATH."
  (interactive "sHost: \nnPort: \nsPath: ")
  (let* ((buf (get-buffer-create "*minimal-wss*"))
         (key "dGhlIHNhbXBsZSBub25jZQ==")  ; Fixed key for testing
         proc)
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Connecting to %s:%d...\n" host port)))
    (pop-to-buffer buf)

    ;; Open TLS connection
    (setq proc (open-network-stream "minimal-wss" buf host port
                                    :type 'tls
                                    :nowait nil))

    (unless proc
      (error "Failed to connect"))

    (with-current-buffer buf
      (insert "TLS connected. Sending handshake...\n"))

    ;; Set binary coding
    (set-process-coding-system proc 'binary 'binary)

    ;; Build and send handshake - exactly like our working tls-test
    (let ((request (concat
                    (format "GET %s HTTP/1.1\r\n" path)
                    (format "Host: %s:%d\r\n" host port)
                    "Upgrade: websocket\r\n"
                    "Connection: Upgrade\r\n"
                    (format "Sec-WebSocket-Key: %s\r\n" key)
                    "Sec-WebSocket-Version: 13\r\n"
                    "\r\n")))
      (with-current-buffer buf
        (insert "--- REQUEST ---\n")
        (insert request)
        (insert "--- END REQUEST ---\n\n"))

      (process-send-string proc request))

    ;; Set up filter to show response
    (set-process-filter
     proc
     (lambda (proc output)
       (with-current-buffer (process-buffer proc)
         (goto-char (point-max))
         (insert output))))

    (set-process-sentinel
     proc
     (lambda (proc event)
       (with-current-buffer (process-buffer proc)
         (goto-char (point-max))
         (insert (format "\n--- Process: %s ---\n" event)))))

    proc))

(defun minimal-wss-test-linode ()
  "Test connection to Linode WSS endpoint."
  (interactive)
  (minimal-wss-connect
   "172-236-28-208.ip.linodeusercontent.com"
   5057
   "/?path=%2Fhome%2Fjoe%2F.claude%2Fprojects%2F-home-joe%2F64570417-4354-40b8-b6a5-db804f69a1d0.jsonl"))

(provide 'minimal-wss)
;;; minimal-wss.el ends here
