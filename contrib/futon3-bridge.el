;;; futon3-bridge.el --- Futon3 process helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to isolate Futon3 process + HTTP
;; bridge helpers so stack/HUD work can evolve independently.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)

(defconst my-futon3--repo-root
  (let* ((base (or load-file-name
                   (buffer-file-name)
                   default-directory)))
    (expand-file-name ".." (file-name-directory base)))
  "Filesystem root of the futon3 checkout used by Futon3 helpers.")

(defvar my-futon3-start-directory (file-name-as-directory my-futon3--repo-root)
  "Where to launch the Futon3 MUSN sandbox from.")

(defvar my-futon3-start-command (list my-tatami--clojure "-M:dev")
  "Command vector used to start Futon3.")

(defvar my-futon3-server-buffer "*Futon3*"
  "Buffer used to collect Futon3 stdout/stderr.")

(defvar my-futon3-process nil)
(defvar my-futon3-ui-base-url "http://localhost:6060")
(defvar my-futon3-transport-port 5050
  "TCP port Futon3 transport listens on when launched via `my-futon3-start'.")
(defvar my-futon3-last-status nil)

(declare-function my-chatgpt-shell--encode-entry-edn "aob-chatgpt" (entry))
(declare-function my-chatgpt-shell--encode-sigils "aob-chatgpt" (sigils))
(declare-function my-chatgpt-shell--encoded-prototypes "aob-chatgpt" ())
(declare-function my-chatgpt-shell--render-context "aob-chatgpt" (&optional ensure))
(declare-function my-chatgpt-shell--vector "aob-chatgpt" (items))
(declare-function my-futon3-sync-selection "futon3-hud" ())

(defun my-futon3-running-p ()
  (and my-futon3-process (process-live-p my-futon3-process)))

(defun my-futon3-start (&optional interactive)
  "Start Futon3 (MUSN sandbox) if needed."
  (interactive "p")
  (if (my-futon3-running-p)
      (when interactive (message "Futon3 already running."))
    (let ((default-directory my-futon3-start-directory))
      (let ((buf (get-buffer-create my-futon3-server-buffer)))
        (setq my-futon3-process
              (make-process :name "futon3-server"
                            :buffer buf
                            :command my-futon3-start-command
                            :stderr buf)))
      (set-process-query-on-exit-flag my-futon3-process nil)
      (set-process-sentinel my-futon3-process
                            (lambda (_proc event)
                              (when interactive
                                (message "Futon3 server event: %s" (string-trim event)))))
      (when (fboundp 'my-futon3-sync-selection)
        (my-futon3-sync-selection))
      (when interactive
        (message "Starting Futon3 (ui=%s)" my-futon3-ui-base-url))))
  my-futon3-process)

(defun my-futon3-stop ()
  "Stop the Futon3 sandbox if it is running."
  (interactive)
  (when (my-futon3-running-p)
    (kill-process my-futon3-process))
  (setq my-futon3-process nil)
  (message "Stopped Futon3."))

(defun my-futon3-ensure-running ()
  (unless (my-futon3-running-p)
    (my-futon3-start)))

(defun my-futon3--request-json (path)
  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url (concat (string-remove-suffix "/" my-futon3-ui-base-url) path)))
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url t t 1.5)))
          (unless buffer (error "No response"))
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                (if (re-search-forward "\n\n" nil t)
                    (let ((body (buffer-substring-no-properties (point) (point-max))))
                      (json-parse-string body :object-type 'plist :array-type 'list
                                         :null-object nil
                                         :false-object nil))
                  (error "Malformed response")))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))
      (error
       (setq my-futon3-last-status (list :error (error-message-string err)))
       nil))))

(defun my-futon3-refresh-status ()
  "Fetch the latest tatami status from Futon3."
  (setq my-futon3-last-status (my-futon3--request-json "/musn/tatami/status")))

(defun my-futon3-refresh-status-and-context ()
  "Refresh Futon3 status and redraw the Stack HUD."
  (interactive)
  (my-futon3-refresh-status)
  (my-chatgpt-shell--render-context t))

(defun my-futon3--tatami-url (path)
  (concat (string-remove-suffix "/" my-futon3-ui-base-url) path))

(defun my-futon3--tatami-request (method path payload)
  (my-futon3-ensure-running)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload
                                (encode-coding-string (json-encode payload) 'utf-8)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (buffer (url-retrieve-synchronously (my-futon3--tatami-url path) t t 2)))
    (unless buffer
      (error "No response from Futon3"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((status (or url-http-response-status 0)))
            (search-forward "\n\n" nil 'move)
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (if (/= status 200)
                  (error "Futon3 %s failed (%s): %s" path status body)
                (when (and body (not (string-empty-p (string-trim body))))
                  (json-parse-string body :object-type 'plist :array-type 'list
                                     :null-object nil
                                     :false-object nil)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

(defun my-futon3-fetch-intent-cues (entry)
  (when entry
    (condition-case err
        (prog1
            (my-futon3--tatami-request
             "POST" "/musn/cues"
             `(("entry-edn" . ,(my-chatgpt-shell--encode-entry-edn entry))))
          (setq my-chatgpt-shell--last-cues-error nil))
      (error
       (setq my-chatgpt-shell--last-cues-error (error-message-string err))
       (message "Futon3 cues failed: %s" (error-message-string err))
       nil))))

(defun my-futon3-fetch-hints (&optional arg)
  "Fetch nearest LDTS/fruit/paramita hints from Futon3.

ARG may be a legacy SIGILS vector or a plist (:sigils ... :intent ...)."
  (condition-case err
      (let* ((payload nil)
             (encoded (my-chatgpt-shell--encoded-prototypes))
             (sigils (cond
                      ((and (listp arg) (keywordp (car arg)))
                       (plist-get arg :sigils))
                      (t arg)))
             (intent (when (and (listp arg) (keywordp (car arg)))
                       (plist-get arg :intent))))
        (when encoded
          (push (cons "prototypes" (my-chatgpt-shell--vector encoded)) payload))
        (when sigils
          (push (cons "sigils" (my-chatgpt-shell--encode-sigils sigils)) payload))
        (when (and intent (stringp intent) (> (length (string-trim intent)) 0))
          (push (cons "intent" intent) payload))
        (prog1 (my-futon3--tatami-request "POST" "/musn/hints" payload)
          (setq my-chatgpt-shell--last-hints-error nil)))
    (error
     (setq my-chatgpt-shell--last-hints-error (error-message-string err))
     (message "Futon3 hints failed: %s" (error-message-string err))
     nil)))

(provide 'futon3-bridge)

;;; futon3-bridge.el ends here
