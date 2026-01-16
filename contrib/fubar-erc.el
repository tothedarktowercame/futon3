;;; fubar-erc.el --- ERC bridge for MUSN chat -*- lexical-binding: t; -*-

;; Lightweight ERC-style buffer for MUSN chat rooms.

(require 'erc)
(require 'fubar)

(defgroup fubar-erc nil
  "ERC bridge for MUSN chat."
  :group 'fubar
  :prefix "fubar-erc-")

(defcustom fubar-erc-room "lab"
  "Default MUSN chat room for ERC."
  :type 'string
  :group 'fubar-erc)

(defcustom fubar-erc-poll-interval 2
  "Seconds between MUSN chat polls."
  :type 'number
  :group 'fubar-erc)

(defvar-local fubar-erc-room nil)
(defvar-local fubar-erc-last-cursor 0)
(defvar-local fubar-erc-poll-timer nil)

(defvar fubar-erc-bridge-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map erc-mode-map)
    (define-key map (kbd "RET") #'fubar-erc-send-input)
    (define-key map (kbd "C-c C-s") #'fubar-erc-send-input)
    (define-key map (kbd "C-c C-r") #'fubar-erc-refresh)
    (define-key map (kbd "C-c C-j") #'fubar-erc-connect)
    (define-key map (kbd "C-c C-l") #'fubar-erc-leave)
    map)
  "Keymap for `fubar-erc-bridge-mode'.")

(define-minor-mode fubar-erc-bridge-mode
  "Minor mode for MUSN chat buffers using ERC."
  :lighter " MUSN-ERC"
  :keymap fubar-erc-bridge-mode-map)

(defun fubar-erc--buffer-name (room)
  (format "*ERC: MUSN#%s*" room))

(defun fubar-erc--author ()
  (let ((name (or fubar-chat-author-name fubar-client-id)))
    (if (and name (not (string-empty-p name)))
        `((id . ,fubar-client-id) (name . ,name))
      `((id . ,fubar-client-id)))))

(defun fubar-erc--ensure-markers ()
  (unless (and (markerp erc-insert-marker) (markerp erc-input-marker))
    (setq-local erc-input-marker (make-marker))
    (setq-local erc-insert-marker (make-marker)))
  (when (or (null (marker-position erc-input-marker))
            (null (marker-position erc-insert-marker)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (erc-display-prompt)
      (set-marker erc-input-marker (point))))
  (let ((input-pos (or (marker-position erc-input-marker) (point-max))))
    (set-marker erc-insert-marker (max (point-min) (1- input-pos))))
  (set-marker-insertion-type erc-insert-marker t)
  (set-marker-insertion-type erc-input-marker nil))

(defun fubar-erc--init-buffer (room)
  (erc-mode)
  (setq-local buffer-read-only nil)
  (setq-local erc-session-server "musn")
  (setq-local erc-server-announced-name "MUSN")
  (setq-local erc-network "MUSN")
  (setq-local erc--target (erc--target-from-string (concat "#" room)))
  (setq-local erc-server-current-nick (or fubar-chat-author-name fubar-client-id))
  (setq-local erc-input-marker (make-marker))
  (setq-local erc-insert-marker (make-marker))
  (set-marker-insertion-type erc-insert-marker t)
  (set-marker-insertion-type erc-input-marker nil)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (when (or (= (point-min) (point-max))
              (not (get-text-property (max (point-min) (1- (point-max))) 'erc-prompt)))
      (insert "\n"))
    (erc-display-prompt)
    (set-marker erc-input-marker (point))
    (let ((input-pos (marker-position erc-input-marker)))
      (set-marker erc-insert-marker (max (point-min) (1- input-pos)))))
  (setq-local erc-input-ring (make-ring (or (bound-and-true-p erc-input-ring-size) 32)))
  (setq-local fubar-erc-room room)
  (setq-local fubar-erc-last-cursor 0)
  (fubar-erc-bridge-mode 1)
  (erc-display-message nil 'notice (current-buffer)
                       (format "*** Connected to MUSN room %s" room)))

(defun fubar-erc-reconnect ()
  "Reinitialize the current MUSN ERC buffer."
  (interactive)
  (when (and fubar-erc-room (derived-mode-p 'erc-mode))
    (fubar-erc--init-buffer fubar-erc-room)
    (fubar-erc-start-poll)
    (message "[fubar-erc] Reconnected to %s" fubar-erc-room)))

(defun fubar-erc--current-input ()
  (when (and (boundp 'erc-input-marker) (marker-position erc-input-marker))
    (buffer-substring-no-properties erc-input-marker (point-max))))

(defun fubar-erc--clear-input ()
  (let ((inhibit-read-only t))
    (when (and (boundp 'erc-input-marker) (marker-position erc-input-marker))
      (delete-region erc-input-marker (point-max)))))

(defun fubar-erc--insert-message (text &optional type)
  (when (and fubar-erc-room (derived-mode-p 'erc-mode))
    (fubar-erc--ensure-markers)
    (erc-display-message nil type (current-buffer) text)))

(defun fubar-erc--format-event (event)
  (let* ((etype (plist-get event :event/type))
         (etype (cond
                 ((keywordp etype) etype)
                 ((stringp etype) (intern (concat ":" etype)))
                 (t etype)))
         (payload (plist-get event :payload)))
    (cond
     ((eq etype :chat/message)
      (let* ((author (plist-get payload :author))
             (name (or (plist-get author :name) (plist-get author :id) "anon"))
             (text (or (plist-get payload :text) "")))
        (let* ((text (replace-regexp-in-string
                      "\\[FULAB-REPORT\\][\\s\\S]*?\\[/FULAB-REPORT\\]" "" text))
               (text (replace-regexp-in-string "[ \t]+\n" "\n" text))
               (text (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))
               (text (string-trim text)))
          (list nil (format "<%s> %s" name text)))))
     ((eq etype :chat/bid)
      (let* ((bidder (plist-get payload :bidder))
             (name (or (plist-get bidder :name) (plist-get bidder :id) "anon"))
             (note (or (plist-get payload :note) ""))
             (bid-id (plist-get payload :bid/id)))
        (list 'notice (format "*** bid %s by %s %s" (or bid-id "?") name note))))
     ((eq etype :chat/unlatch)
      (let* ((accepted (plist-get payload :accepted))
             (bid-id (plist-get accepted :bid/id))
             (by (plist-get payload :by))
             (name (or (plist-get by :name) (plist-get by :id) "chair")))
        (list 'notice (format "*** unlatch by %s (accepted %s)" name (or bid-id "?")))))
     (t nil))))

(defun fubar-erc--apply-events (events)
  (when (vectorp events)
    (dotimes (idx (length events))
      (let* ((event (aref events idx))
             (formatted (fubar-erc--format-event event)))
        (when (and formatted (consp formatted))
          (fubar-erc--insert-message (cadr formatted) (car formatted)))))))

(defun fubar-erc-refresh (&optional buffer)
  "Poll the MUSN chat room for new events."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when fubar-erc-room
      (fubar-erc--ensure-markers)
      (let* ((room fubar-erc-room)
             (payload `((room . ,room)
                        ,@(when (> fubar-erc-last-cursor 0)
                            `((since . ,fubar-erc-last-cursor)))))
             (resp (fubar--post "/musn/chat/state" payload)))
        (when (plist-get resp :ok)
          (setq fubar-erc-last-cursor (or (plist-get resp :cursor) fubar-erc-last-cursor))
          (fubar-erc--apply-events (plist-get resp :events)))))))

(defun fubar-erc-start-poll ()
  "Start polling the MUSN chat room."
  (when fubar-erc-poll-timer
    (cancel-timer fubar-erc-poll-timer))
  (let ((buf (current-buffer)))
    (setq fubar-erc-poll-timer
          (run-at-time 0 fubar-erc-poll-interval
                       (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (when fubar-erc-room
                               (fubar-erc-refresh buffer)))))
                       buf))))

(defun fubar-erc-stop-poll ()
  "Stop polling the MUSN chat room."
  (when fubar-erc-poll-timer
    (cancel-timer fubar-erc-poll-timer)
    (setq fubar-erc-poll-timer nil)))

(defun fubar-erc-connect (&optional room)
  "Open an ERC bridge buffer for MUSN chat ROOM."
  (interactive "sRoom: ")
  (let* ((room (if (and room (not (string-empty-p room)))
                   room
                 fubar-erc-room))
         (room (if (and room (string-match-p "\\s-" room))
                   (progn
                     (message "[fubar-erc] Room contains whitespace, using default.")
                     fubar-erc-room)
                 room))
         (buf (get-buffer-create (fubar-erc--buffer-name room))))
    (with-current-buffer buf
      (fubar-erc--init-buffer room)
      (fubar-erc-start-poll))
    (pop-to-buffer buf)))

(defun fubar-erc-leave ()
  "Leave the current MUSN chat room."
  (interactive)
  (fubar-erc-stop-poll)
  (fubar-erc--insert-message "*** Disconnected" 'notice))

(defun fubar-erc-send-input ()
  "Send current ERC input line to MUSN chat."
  (interactive)
  (let* ((text (string-trim (or (fubar-erc--current-input) "")))
         (room fubar-erc-room)
         (author (fubar-erc--author))
         (name (or (cdr (assoc 'name author))
                   (cdr (assoc 'id author))
                   fubar-client-id)))
    (if (string-empty-p text)
        (message "[fubar-erc] Empty message")
      (ring-insert erc-input-ring text)
      (fubar-erc--clear-input)
      (fubar-erc--insert-message (format "<%s> %s" name text))
      (fubar--post "/musn/chat/message"
                   `((room . ,room)
                     (msg-id . ,(fubar--uuid))
                     (author . ,author)
                     (text . ,text))))))

(defun fubar-erc-launch-fucodex (prompt &optional intent room)
  "Launch fucodex MUSN run with PROMPT and open ERC chat for ROOM."
  (interactive "sPrompt: ")
  (require 'fubar-viewer)
  (let* ((room (or (and room (not (string-empty-p room)) room) fubar-erc-room))
         (author (or fubar-chat-author-name fubar-client-id)))
    (fubar-erc-connect room)
    (fubar-musn-launch-and-view prompt intent room author)))

(provide 'fubar-erc)

;;; fubar-erc.el ends here
