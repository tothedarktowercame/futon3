;;; capture-ws-request.el --- Capture websocket.el's actual request -*- lexical-binding: t; -*-

;;; Code:

(require 'websocket)

(defvar capture-ws--last-request nil
  "Last captured websocket request.")

(defun capture-ws--intercept (orig-fun proc string)
  "Capture strings sent to websocket processes."
  (when (and (processp proc)
             (string-match-p "^GET " string))
    (setq capture-ws--last-request string)
    (message "=== CAPTURED WEBSOCKET REQUEST ===")
    (message "%s" string)
    (message "=== END (%d bytes) ===" (length string)))
  (funcall orig-fun proc string))

(advice-add 'process-send-string :around #'capture-ws--intercept)

(defun capture-ws-show ()
  "Show the last captured request."
  (interactive)
  (if capture-ws--last-request
      (with-current-buffer (get-buffer-create "*captured-request*")
        (erase-buffer)
        (insert capture-ws--last-request)
        (pop-to-buffer (current-buffer)))
    (message "No request captured yet")))

(defun capture-ws-remove ()
  "Remove capture advice."
  (interactive)
  (advice-remove 'process-send-string #'capture-ws--intercept)
  (message "Capture removed"))

(provide 'capture-ws-request)
;;; capture-ws-request.el ends here
