;; Test WSS connection - load with M-x load-file
(require 'websocket)

(defvar test-wss nil)

(defun test-wss-connect ()
  (interactive)
  (when test-wss (websocket-close test-wss))
  (message "Connecting...")
  (setq test-wss
        (websocket-open
         "wss://172-236-28-208.ip.linodeusercontent.com:5057?path=%2Fhome%2Fjoe%2F.claude%2Fprojects%2F-home-joe%2F64570417-4354-40b8-b6a5-db804f69a1d0.jsonl"
         :on-open (lambda (ws) (message "Connected!"))
         :on-message (lambda (ws frame)
                       (message "Got %d bytes" (length (websocket-frame-text frame))))
         :on-error (lambda (ws type err)
                     (message "WSS Error: type=%s err=%s" type err))
         :on-close (lambda (ws) (message "Closed")))))

(test-wss-connect)
