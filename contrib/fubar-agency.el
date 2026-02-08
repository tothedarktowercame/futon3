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
;; When connected, incoming whistles pull you into the ERC channel
;; so you can participate in the standup alongside agents.  The
;; whistle prompt is posted to the channel and your ERC buffer is
;; raised.  Bells appear as notifications.

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
  "When non-nil, auto-respond to whistles with this string instead of prompting."
  :type '(choice (const nil) string)
  :group 'fubar-agency)

(defcustom fubar-agency-whistle-timeout 60
  "Seconds to wait for user to respond to a whistle before timing out."
  :type 'integer
  :group 'fubar-agency)

(defcustom fubar-agency-irc-host "localhost"
  "IRC bridge hostname."
  :type 'string
  :group 'fubar-agency)

(defcustom fubar-agency-irc-port 6667
  "IRC bridge port."
  :type 'integer
  :group 'fubar-agency)

(defcustom fubar-agency-irc-password nil
  "IRC bridge password (nil if no auth required)."
  :type '(choice (const nil) string)
  :group 'fubar-agency)

(defcustom fubar-agency-irc-room "standup"
  "Default ERC channel to join on whistle/standup."
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

(defvar fubar-agency--pending-whistle nil
  "Plist of the current pending whistle, or nil.")

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
  "Find an existing ERC server buffer connected to the IRC bridge, or nil."
  (seq-find
   (lambda (buf)
     (with-current-buffer buf
       (and (derived-mode-p 'erc-mode)
            (erc-server-buffer-p)
            (erc-server-process-alive)
            (boundp 'erc-session-server)
            (stringp erc-session-server)
            (string-match-p (regexp-quote fubar-agency-irc-host)
                            erc-session-server))))
   (buffer-list)))

(defun fubar-agency--erc-channel-buffer (room)
  "Find the ERC buffer for #ROOM, or nil."
  (let ((chan (if (string-prefix-p "#" room) room (concat "#" room))))
    (seq-find
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'erc-mode)
              (let ((target (erc-default-target)))
                (and target
                     (string-equal-ignore-case target chan))))))
     (buffer-list))))

(defun fubar-agency--ensure-erc ()
  "Ensure ERC is connected to the IRC bridge. Returns the server buffer."
  (or (fubar-agency--erc-server-buffer)
      (progn
        (fubar-agency--log "[erc] Connecting to %s:%d as %s"
                           fubar-agency-irc-host
                           fubar-agency-irc-port
                           fubar-agency-agent-id)
        (erc :server fubar-agency-irc-host
             :port fubar-agency-irc-port
             :nick fubar-agency-agent-id
             :password fubar-agency-irc-password)
        ;; Give ERC a moment to connect
        (sit-for 1)
        (fubar-agency--erc-server-buffer))))

(defun fubar-agency--join-and-show-erc (room)
  "Ensure ERC is connected, join #ROOM, and raise the channel buffer."
  (let ((chan (if (string-prefix-p "#" room) room (concat "#" room))))
    (fubar-agency--ensure-erc)
    ;; Join the channel if not already in it
    (unless (fubar-agency--erc-channel-buffer room)
      (let ((server-buf (fubar-agency--erc-server-buffer)))
        (when server-buf
          (with-current-buffer server-buf
            (erc-join-channel chan)))))
    ;; Wait briefly for the channel buffer to appear
    (let ((attempts 0)
          (buf nil))
      (while (and (< attempts 10) (null buf))
        (setq buf (fubar-agency--erc-channel-buffer room))
        (unless buf
          (sit-for 0.3)
          (setq attempts (1+ attempts))))
      (when buf
        (pop-to-buffer buf)
        (fubar-agency--log "[erc] Joined and raised %s" chan))
      buf)))

;;; WebSocket message handling

(defun fubar-agency--send (msg)
  "Send MSG (a plist or alist) as JSON over the WebSocket."
  (when (and fubar-agency--ws
             (websocket-openp fubar-agency--ws))
    (websocket-send-text
     fubar-agency--ws
     (json-encode msg))))

(defun fubar-agency--handle-whistle (msg)
  "Handle an incoming whistle by pulling the user into the ERC standup room."
  (let* ((request-id (plist-get msg :request-id))
         (prompt (plist-get msg :prompt))
         (display-prompt (or prompt "Agency standup check-in"))
         (room fubar-agency-irc-room))
    (fubar-agency--log "[whistle] request=%s prompt=%s"
                       request-id
                       (truncate-string-to-width display-prompt 60 nil nil "..."))
    (if fubar-agency-auto-respond
        ;; Auto-respond mode (no ERC)
        (progn
          (fubar-agency--send
           `((type . "whistle-response")
             (request-id . ,request-id)
             (response . ,fubar-agency-auto-respond)))
          (fubar-agency--log "[whistle] auto-responded: %s" fubar-agency-auto-respond))
      ;; Interactive mode - pull user into ERC
      (setq fubar-agency--pending-whistle
            (list :request-id request-id :prompt display-prompt :room room))
      ;; Respond immediately so the standup doesn't time out
      (fubar-agency--send
       `((type . "whistle-response")
         (request-id . ,request-id)
         (response . ,(format "Joining #%s" room))))
      ;; Use run-at-time to avoid blocking the WS handler
      (run-at-time 0 nil #'fubar-agency--pull-into-erc
                   room display-prompt))))

(defun fubar-agency--pull-into-erc (room prompt)
  "Join ERC ROOM and post the standup PROMPT there."
  (let ((chan-buf (fubar-agency--join-and-show-erc room)))
    (when chan-buf
      ;; Post the prompt into the channel so it's visible
      (with-current-buffer chan-buf
        (erc-send-message
         (format "[standup] %s" (truncate-string-to-width prompt 200 nil nil "..."))))))
  (setq fubar-agency--pending-whistle nil))

(defun fubar-agency--handle-bell (msg)
  "Handle an incoming bell MSG as a notification."
  (let* ((bell-type (or (plist-get msg :bell-type) "bell"))
         (payload (plist-get msg :payload))
         (bell-msg (if payload
                       (or (plist-get payload :message) (format "%s" payload))
                     bell-type)))
    (fubar-agency--log "[bell] type=%s msg=%s" bell-type bell-msg)
    (message "[Agency Bell] %s" bell-msg)))

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
          (fubar-agency--log "[connected] agent-id=%s" (plist-get msg :agent-id)))
         ((string= type "registered")
          (fubar-agency--log "[registered] agent-id=%s" (plist-get msg :agent-id)))
         ((string= type "pong")
          nil) ; silent keepalive
         ((string= type "whistle")
          (fubar-agency--handle-whistle msg))
         ((string= type "bell")
          (fubar-agency--handle-bell msg))
         (t
          (fubar-agency--log "[unknown] type=%s" type)))))))

(defun fubar-agency--on-open (_ws)
  "Called when WebSocket connection opens."
  (setq fubar-agency--connected t)
  (fubar-agency--log "[open] connected to Agency as %s" fubar-agency-agent-id)
  (message "[fubar-agency] Connected to Agency as %s" fubar-agency-agent-id)
  ;; Start keepalive pings
  (when fubar-agency--ping-timer
    (cancel-timer fubar-agency--ping-timer))
  (setq fubar-agency--ping-timer
        (run-at-time fubar-agency-ping-interval
                     fubar-agency-ping-interval
                     #'fubar-agency--ping)))

(defun fubar-agency--on-close (_ws)
  "Called when WebSocket connection closes."
  (setq fubar-agency--connected nil)
  (setq fubar-agency--ws nil)
  (when fubar-agency--ping-timer
    (cancel-timer fubar-agency--ping-timer)
    (setq fubar-agency--ping-timer nil))
  (fubar-agency--log "[closed] disconnected from Agency")
  (message "[fubar-agency] Disconnected from Agency"))

(defun fubar-agency--on-error (_ws _type err)
  "Called on WebSocket error."
  (fubar-agency--log "[error] %s" err)
  (message "[fubar-agency] WebSocket error: %s" err))

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
  (let ((url (format "%s?agent-id=%s" fubar-agency-ws-url fubar-agency-agent-id)))
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
  (setq fubar-agency--ws nil)
  (setq fubar-agency--connected nil))

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
  "Connect to the IRC bridge via ERC and join ROOM."
  (interactive "sRoom (blank for default): ")
  (let ((room (if (and room (not (string-empty-p room))) room fubar-agency-irc-room)))
    (fubar-agency--join-and-show-erc room)))

(provide 'fubar-agency)
;;; fubar-agency.el ends here
