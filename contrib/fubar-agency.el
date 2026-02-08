;;; fubar-agency.el --- Agency WebSocket client for human agents -*- lexical-binding: t; -*-

;;; Commentary:
;; Connects Emacs to Agency via WebSocket so the human user can
;; participate in standups and receive whistles/bells.
;;
;; Usage:
;;   (fubar-agency-connect)     ; Connect to Agency WS
;;   (fubar-agency-disconnect)  ; Disconnect
;;   (fubar-agency-status)      ; Show connection status
;;
;; When a bell arrives (e.g. standup), you are pulled into the ERC
;; channel for the standup room.  Whistles get a simple minibuffer
;; prompt or auto-response.

;;; Code:

(require 'json)
(require 'websocket)
(require 'erc nil t)

;;; Configuration

(defgroup fubar-agency nil
  "Agency WebSocket client for human agents."
  :group 'fubar
  :prefix "fubar-agency-")

(defcustom fubar-agency-ws-url "ws://localhost:7070/agency/ws"
  "WebSocket URL for Agency."
  :type 'string
  :group 'fubar-agency)

(defcustom fubar-agency-agent-id "joe"
  "Agent ID to register with Agency."
  :type 'string
  :group 'fubar-agency)

(defcustom fubar-agency-ping-interval 30
  "Seconds between keepalive pings."
  :type 'integer
  :group 'fubar-agency)

(defcustom fubar-agency-auto-respond nil
  "When non-nil, auto-respond to whistles with this string."
  :type '(choice (const nil) string)
  :group 'fubar-agency)

(defcustom fubar-agency-irc-room "standup"
  "Default ERC channel to join on standup bell."
  :type 'string
  :group 'fubar-agency)

;;; State

(defvar fubar-agency--ws nil
  "Active WebSocket connection.")

(defvar fubar-agency--ping-timer nil
  "Timer for keepalive pings.")

(defvar fubar-agency--buffer-name "*fubar-agency*"
  "Buffer name for Agency event log.")

(defvar fubar-agency--connected nil
  "Non-nil when connected to Agency.")

;;; Logging

(defun fubar-agency--log (fmt &rest args)
  "Log a formatted message to the agency buffer."
  (let ((line (format "%s %s"
                      (format-time-string "%H:%M:%S")
                      (apply #'format fmt args)))
        (buf (get-buffer-create fubar-agency--buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert line "\n")))))

;;; ERC integration

(defun fubar-agency--erc-server-buffer ()
  "Find any live ERC server buffer, or nil.
Looks for any ERC buffer with a live server process."
  (seq-find
   (lambda (buf)
     (with-current-buffer buf
       (and (derived-mode-p 'erc-mode)
            (erc-server-buffer-p)
            (erc-server-process-alive))))
   (buffer-list)))

(defun fubar-agency--erc-channel-buffer (room)
  "Find the ERC buffer for #ROOM, or nil."
  (let ((chan (if (string-prefix-p "#" room) room
               (concat "#" room))))
    (seq-find
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'erc-mode)
              (let ((target (erc-default-target)))
                (and target
                     (string-equal-ignore-case target chan))))))
     (buffer-list))))

(defun fubar-agency--join-and-show-erc (room)
  "Join #ROOM via the existing ERC connection and raise the buffer.
If no ERC server is connected, logs an error."
  (let ((chan (if (string-prefix-p "#" room) room
               (concat "#" room)))
        (server-buf (fubar-agency--erc-server-buffer)))
    (cond
     ;; Already in the room — just raise it
     ((fubar-agency--erc-channel-buffer room)
      (let ((buf (fubar-agency--erc-channel-buffer room)))
        (pop-to-buffer buf)
        (fubar-agency--log "[erc] Raised %s" chan)
        buf))
     ;; Have a server connection — join
     (server-buf
      (with-current-buffer server-buf
        (erc-join-channel chan))
      ;; Wait for the channel buffer to appear
      (let ((attempts 0) (buf nil))
        (while (and (< attempts 15) (null buf))
          (setq buf (fubar-agency--erc-channel-buffer room))
          (unless buf
            (sit-for 0.2)
            (setq attempts (1+ attempts))))
        (when buf
          (pop-to-buffer buf)
          (fubar-agency--log "[erc] Joined and raised %s" chan))
        buf))
     ;; No ERC at all
     (t
      (fubar-agency--log "[erc] No ERC server buffer found!")
      (message "[fubar-agency] No ERC connection — connect to IRC first")
      nil))))

;;; WebSocket message handling

(defun fubar-agency--send (msg)
  "Send MSG as JSON over the WebSocket."
  (when (and fubar-agency--ws
             (websocket-openp fubar-agency--ws))
    (websocket-send-text
     fubar-agency--ws
     (json-encode msg))))

(defun fubar-agency--handle-whistle (msg)
  "Handle a whistle with minibuffer prompt or auto-response."
  (let* ((request-id (plist-get msg :request-id))
         (prompt (or (plist-get msg :prompt) "ping"))
         (short (truncate-string-to-width prompt 60 nil nil "...")))
    (fubar-agency--log "[whistle] id=%s prompt=%s" request-id short)
    (if fubar-agency-auto-respond
        (progn
          (fubar-agency--send
           `((type . "whistle-response")
             (request-id . ,request-id)
             (response . ,fubar-agency-auto-respond)))
          (fubar-agency--log "[whistle] auto-responded"))
      ;; Prompt in minibuffer (deferred so we don't block the WS)
      (run-at-time
       0 nil
       (lambda ()
         (let ((response (read-string
                          (format "[Agency whistle] %s\nReply: " prompt))))
           (fubar-agency--send
            `((type . "whistle-response")
              (request-id . ,request-id)
              (response . ,response)))
           (fubar-agency--log "[whistle] replied: %s" response)))))))

(defun fubar-agency--handle-bell (msg)
  "Handle a bell by joining the ERC standup room."
  (let* ((bell-type (or (plist-get msg :bell-type) "bell"))
         (payload (plist-get msg :payload))
         (room (or (and payload (plist-get payload :room))
                   fubar-agency-irc-room))
         (bell-msg (and payload
                        (or (plist-get payload :message)
                            (format "%s" payload)))))
    (fubar-agency--log "[bell] type=%s room=%s msg=%s"
                       bell-type room (or bell-msg ""))
    ;; Pull the user into the ERC room
    (run-at-time 0 nil #'fubar-agency--pull-into-erc
                 room bell-msg)))

(defun fubar-agency--pull-into-erc (room &optional prompt)
  "Join ERC ROOM and optionally post PROMPT there."
  (let ((chan-buf (fubar-agency--join-and-show-erc room)))
    (when (and chan-buf prompt
               (not (string-empty-p prompt)))
      (with-current-buffer chan-buf
        (erc-send-message
         (format "[standup] %s"
                 (truncate-string-to-width prompt 200 nil nil "...")))))))

(defun fubar-agency--on-message (_ws frame)
  "Handle incoming WebSocket FRAME."
  (let* ((text (websocket-frame-text frame))
         (json-object-type 'plist)
         (json-key-type 'keyword)
         (json-array-type 'list)
         (msg (condition-case nil
                  (json-read-from-string text)
                (error nil))))
    (when msg
      (let ((type (plist-get msg :type)))
        (cond
         ((string= type "connected")
          (fubar-agency--log "[connected] agent-id=%s"
                             (plist-get msg :agent-id)))
         ((string= type "registered")
          (fubar-agency--log "[registered] agent-id=%s"
                             (plist-get msg :agent-id)))
         ((string= type "pong") nil)
         ((string= type "whistle")
          (fubar-agency--handle-whistle msg))
         ((string= type "bell")
          (fubar-agency--handle-bell msg))
         (t
          (fubar-agency--log "[unknown] type=%s" type)))))))

(defun fubar-agency--on-open (_ws)
  "Called when WebSocket connection opens."
  (setq fubar-agency--connected t)
  (fubar-agency--log "[open] connected as %s" fubar-agency-agent-id)
  (message "[fubar-agency] Connected as %s" fubar-agency-agent-id)
  (when fubar-agency--ping-timer
    (cancel-timer fubar-agency--ping-timer))
  (setq fubar-agency--ping-timer
        (run-at-time fubar-agency-ping-interval
                     fubar-agency-ping-interval
                     #'fubar-agency--ping)))

(defun fubar-agency--on-close (_ws)
  "Called when WebSocket connection closes."
  (setq fubar-agency--connected nil
        fubar-agency--ws nil)
  (when fubar-agency--ping-timer
    (cancel-timer fubar-agency--ping-timer)
    (setq fubar-agency--ping-timer nil))
  (fubar-agency--log "[closed] disconnected")
  (message "[fubar-agency] Disconnected"))

(defun fubar-agency--on-error (_ws _type err)
  "Called on WebSocket error."
  (fubar-agency--log "[error] %s" err)
  (message "[fubar-agency] Error: %s" err))

(defun fubar-agency--ping ()
  "Send a keepalive ping."
  (when (and fubar-agency--ws
             (websocket-openp fubar-agency--ws))
    (fubar-agency--send '((type . "ping")))))

;;; Public commands

;;;###autoload
(defun fubar-agency-connect ()
  "Connect to Agency via WebSocket."
  (interactive)
  (when (and fubar-agency--ws
             (websocket-openp fubar-agency--ws))
    (fubar-agency-disconnect))
  (let ((url (format "%s?agent-id=%s"
                     fubar-agency-ws-url fubar-agency-agent-id)))
    (fubar-agency--log "[connecting] %s" url)
    (condition-case err
        (setq fubar-agency--ws
              (websocket-open
               url
               :on-message #'fubar-agency--on-message
               :on-open    #'fubar-agency--on-open
               :on-close   #'fubar-agency--on-close
               :on-error   #'fubar-agency--on-error))
      (error
       (fubar-agency--log "[connect-error] %s" err)
       (message "[fubar-agency] Failed to connect: %s" err)))))

;;;###autoload
(defun fubar-agency-disconnect ()
  "Disconnect from Agency."
  (interactive)
  (when fubar-agency--ping-timer
    (cancel-timer fubar-agency--ping-timer)
    (setq fubar-agency--ping-timer nil))
  (when (and fubar-agency--ws
             (websocket-openp fubar-agency--ws))
    (websocket-close fubar-agency--ws))
  (setq fubar-agency--ws nil
        fubar-agency--connected nil))

;;;###autoload
(defun fubar-agency-status ()
  "Show Agency connection status."
  (interactive)
  (message "[fubar-agency] %s (agent-id: %s)"
           (if fubar-agency--connected "Connected" "Disconnected")
           fubar-agency-agent-id))

;;;###autoload
(defun fubar-agency-show-log ()
  "Display the Agency event log buffer."
  (interactive)
  (display-buffer (get-buffer-create fubar-agency--buffer-name)))

;;;###autoload
(defun fubar-agency-join (&optional room)
  "Join an ERC channel via the IRC bridge."
  (interactive "sRoom (blank for default): ")
  (let ((room (if (and room (not (string-empty-p room)))
                  room fubar-agency-irc-room)))
    (fubar-agency--join-and-show-erc room)))

(provide 'fubar-agency)
;;; fubar-agency.el ends here
