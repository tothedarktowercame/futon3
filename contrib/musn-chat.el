;;; musn-chat.el --- MUSN IRC bridge chat client -*- lexical-binding: t; -*-

;; Simple Emacs client for MUSN IRC bridge chat.
;; Connects via subprocess running musn-irc-listen.

;;; Code:

(require 'comint)

(defgroup musn-chat nil
  "MUSN IRC bridge chat client."
  :group 'communication)

(defcustom musn-chat-host "localhost"
  "IRC bridge host."
  :type 'string
  :group 'musn-chat)

(defcustom musn-chat-port 6680
  "IRC bridge port."
  :type 'integer
  :group 'musn-chat)

(defcustom musn-chat-nick "emacs"
  "Chat nickname."
  :type 'string
  :group 'musn-chat)

(defcustom musn-chat-room "lab"
  "Chat room."
  :type 'string
  :group 'musn-chat)

(defcustom musn-chat-password nil
  "IRC bridge password."
  :type '(choice (const nil) string)
  :group 'musn-chat)

(defcustom musn-chat-script "scripts/musn-irc-listen"
  "Path to listener script (relative to project root)."
  :type 'string
  :group 'musn-chat)

(defcustom musn-chat-send-script "scripts/musn-irc-send"
  "Path to send script (relative to project root)."
  :type 'string
  :group 'musn-chat)

(defvar musn-chat-process nil
  "The listener process.")

(defvar musn-chat-buffer-name "*MUSN Chat*"
  "Buffer name for chat.")

(defvar musn-chat-project-root nil
  "Project root directory.")

(defun musn-chat--find-project-root ()
  "Find futon3 project root."
  (or musn-chat-project-root
      (locate-dominating-file default-directory "scripts/musn-irc-listen")
      (error "Cannot find futon3 project root")))

(defun musn-chat--build-args ()
  "Build arguments for listener script."
  (let ((args (list "--host" musn-chat-host
                    "--port" (number-to-string musn-chat-port)
                    "--nick" musn-chat-nick
                    "--room" musn-chat-room)))
    (when musn-chat-password
      (setq args (append args (list "--pass" musn-chat-password))))
    args))

(defun musn-chat--filter (proc string)
  "Process filter for chat output."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))))

(defun musn-chat--sentinel (proc event)
  "Process sentinel for chat."
  (message "MUSN Chat: %s" (string-trim event)))

;;;###autoload
(defun musn-chat-connect (&optional host)
  "Connect to MUSN IRC chat.
With prefix arg or HOST, prompt for host. Otherwise use `musn-chat-host'."
  (interactive (list (when current-prefix-arg
                       (read-string "Host: " musn-chat-host))))
  (when (and musn-chat-process (process-live-p musn-chat-process))
    (error "Already connected. Use musn-chat-disconnect first"))
  (when host
    (setq musn-chat-host host))
  ;; Prompt for password if not set
  (unless musn-chat-password
    (setq musn-chat-password (read-passwd "IRC password: ")))
  (let* ((root (musn-chat--find-project-root))
         (default-directory root)
         (script (expand-file-name musn-chat-script root))
         (buf (get-buffer-create musn-chat-buffer-name))
         (args (musn-chat--build-args)))
    (setq musn-chat-project-root root)
    (with-current-buffer buf
      (musn-chat-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";;; Connecting to %s:%d as %s in #%s...\n\n"
                        musn-chat-host musn-chat-port
                        musn-chat-nick musn-chat-room))))
    (setq musn-chat-process
          (apply #'start-process "musn-chat" buf "python3" script args))
    (set-process-filter musn-chat-process #'musn-chat--filter)
    (set-process-sentinel musn-chat-process #'musn-chat--sentinel)
    (display-buffer buf)
    (message "MUSN Chat: connecting to %s:%d..." musn-chat-host musn-chat-port)))

(defun musn-chat-disconnect ()
  "Disconnect from MUSN IRC chat."
  (interactive)
  (when (and musn-chat-process (process-live-p musn-chat-process))
    (kill-process musn-chat-process))
  (setq musn-chat-process nil)
  (message "MUSN Chat: disconnected"))

(defun musn-chat-send (message)
  "Send MESSAGE to the chat room."
  (interactive "sMessage: ")
  (let* ((root (or musn-chat-project-root (musn-chat--find-project-root)))
         (script (expand-file-name musn-chat-send-script root))
         (args (list "--host" musn-chat-host
                     "--port" (number-to-string musn-chat-port)
                     "--nick" musn-chat-nick
                     "--room" musn-chat-room)))
    (when musn-chat-password
      (setq args (append args (list "--pass" musn-chat-password))))
    (setq args (append args (list message)))
    (apply #'start-process "musn-send" nil "python3" script args)
    (message "Sent: %s" message)))

(defun musn-chat-send-region (start end)
  "Send region as chat message."
  (interactive "r")
  (musn-chat-send (buffer-substring-no-properties start end)))

(defvar musn-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'musn-chat-send)
    (define-key map (kbd "C-c C-d") #'musn-chat-disconnect)
    (define-key map (kbd "C-c C-r") #'musn-chat-connect)
    map)
  "Keymap for musn-chat-mode.")

(define-derived-mode musn-chat-mode special-mode "MUSN-Chat"
  "Major mode for MUSN IRC chat.

\\{musn-chat-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(provide 'musn-chat)
;;; musn-chat.el ends here
