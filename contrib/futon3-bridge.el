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

(defvar my-tatami--clojure
  (or (executable-find "clojure") "clojure")
  "Clojure command used to start Futon3 when Tatami config is unavailable.")

(defvar my-futon3-start-command (list my-tatami--clojure "-M:dev")
  "Command vector used to start Futon3.")

(defvar my-futon3-server-buffer "*Futon3*"
  "Buffer used to collect Futon3 stdout/stderr.")

(defvar my-futon3-process nil)
(defvar my-futon3-api-base-url nil
  "Base URL for Futon3 /musn endpoints. When nil, uses `my-futon3-ui-base-url'.")
(defcustom my-futon3-request-timeout 20.0
  "Timeout in seconds for Futon3 HTTP requests."
  :type 'number
  :group 'tatami-integration)
(defcustom my-futon3-status-timeout 0.8
  "Timeout in seconds for Futon3 status requests."
  :type 'number
  :group 'tatami-integration)
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

(defvar my-futon3--blank-sigil-warning-count 0)
(defcustom my-futon3-blank-sigil-warning-limit 3
  "Maximum number of blank-sigil warnings to emit."
  :type 'integer
  :group 'tatami-integration)

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

(defun my-futon3--server-reachable-p ()
  (when (my-futon3--request-json "/musn/tatami/status")
    t))

(defun my-futon3-ensure-running ()
  (unless (or (my-futon3-running-p)
              (my-futon3--server-reachable-p))
    (my-futon3-start)))

(defun my-futon3--request-json (path &optional timeout)
  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url (concat (my-futon3--base-url) path)))
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url t t (or timeout my-futon3-request-timeout))))
          (unless buffer
            (error "No response from Futon3 (%s, %.1fs)" url (or timeout my-futon3-request-timeout)))
          (unwind-protect
              (with-current-buffer buffer
                (my-futon3--parse-json-response "/musn/tatami/status"))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))
      (error
       (setq my-futon3-last-status (list :error (error-message-string err)))
       nil))))

(defun my-futon3-refresh-status ()
  "Fetch the latest tatami status from Futon3."
  (setq my-futon3-last-status (my-futon3--request-json "/musn/tatami/status"
                                                       my-futon3-status-timeout)))

(defun my-futon3-refresh-status-and-context ()
  "Refresh Futon3 status and redraw the Stack HUD."
  (interactive)
  (my-futon3-refresh-status)
  (my-chatgpt-shell--render-context t))

(defun my-futon3--tatami-url (path)
  (concat (my-futon3--base-url) path))

(defun my-futon3--base-url ()
  (string-remove-suffix "/"
                        (or my-futon3-api-base-url
                            my-futon3-ui-base-url)))

(defun my-futon3--json-alist-p (value)
  (when (listp value)
    (let ((ok t)
          (cursor value))
      (while (and ok (consp cursor))
        (let* ((elem (car cursor))
               (key (and (consp elem) (car elem))))
          (setq ok (and (consp elem)
                        (not (consp key))
                        (or (stringp key) (symbolp key) (keywordp key)))))
        (setq cursor (cdr cursor)))
      ok)))

(defun my-futon3--json-keywordize (value)
  (cond
   ((null value) nil)
   ((my-futon3--json-alist-p value)
    (let (plist)
      (dolist (pair value)
        (let* ((raw-key (car pair))
               (key (cond
                     ((keywordp raw-key) raw-key)
                     ((symbolp raw-key) (intern (concat ":" (symbol-name raw-key))))
                     ((stringp raw-key) (intern (concat ":" raw-key)))
                     (t raw-key)))
               (val (my-futon3--json-keywordize (cdr pair))))
          (setq plist (plist-put plist key val))))
      plist))
   ((listp value)
    (mapcar #'my-futon3--json-keywordize value))
   (t value)))

(defun my-futon3--blank-sigil-present-p (value)
  (cond
   ((null value) nil)
   ((listp value)
    (or (my-futon3--blank-sigil-present-p (car value))
        (my-futon3--blank-sigil-present-p (cdr value))))
   ((and (symbolp value) (eq value :emoji)) nil)
   ((and (symbolp value) (eq value :hanzi)) nil)
   ((and (consp value) (symbolp (car value)))
    (let* ((key (car value))
           (val (cdr value)))
      (if (memq key '(:emoji :hanzi))
          (and (stringp val) (string= val " "))
        (my-futon3--blank-sigil-present-p val))))
   ((and (plistp value))
    (let ((pairs value)
          (found nil))
      (while (and pairs (not found))
        (let ((key (car pairs))
              (val (cadr pairs)))
          (when (memq key '(:emoji :hanzi))
            (setq found (and (stringp val) (string= val " "))))
          (setq pairs (cddr pairs))))
      (or found (my-futon3--blank-sigil-present-p (cdr value)))))
   (t nil)))

(defun my-futon3--maybe-warn-blank-sigils (value)
  (when (and (< my-futon3--blank-sigil-warning-count
                my-futon3-blank-sigil-warning-limit)
             (my-futon3--blank-sigil-present-p value))
    (setq my-futon3--blank-sigil-warning-count
          (1+ my-futon3--blank-sigil-warning-count))
    (message "Futon3 response contains blank sigils; check UTF-8 decoding.")))

(defun my-futon3--normalize-json-body (body)
  "Ensure BODY is UTF-8 decoded so emoji/sigils are not mojibake."
  (when body
    (if (multibyte-string-p body)
        body
      (condition-case _err
          (decode-coding-string body 'utf-8)
        (error body)))))

(defun my-futon3--tatami-request (method path payload)
  (my-futon3-ensure-running)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload
                                (encode-coding-string (json-encode payload) 'utf-8)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (url (my-futon3--tatami-url path))
         (buffer (url-retrieve-synchronously url t t my-futon3-request-timeout)))
    (unless buffer
      (error "No response from Futon3 (%s, %.1fs)" url my-futon3-request-timeout))
    (unwind-protect
        (with-current-buffer buffer
          (my-futon3--parse-json-response path))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my-futon3--parse-json-response (path)
  (goto-char (point-min))
  (let ((status (or url-http-response-status 0)))
    (search-forward "\n\n" nil 'move)
    (let ((body (my-futon3--normalize-json-body
                 (buffer-substring-no-properties (point) (point-max)))))
      (if (/= status 200)
          (error "Futon3 %s failed (%s): %s" path status body)
        (when (and body (not (string-empty-p (string-trim body))))
          (let ((parsed (my-futon3--json-keywordize
                         (json-parse-string body :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil))))
            (my-futon3--maybe-warn-blank-sigils parsed)
            parsed))))))

(defun my-futon3--json-request-async (method path payload callback)
  (my-futon3-ensure-running)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload
                                (encode-coding-string (json-encode payload) 'utf-8)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (url (my-futon3--tatami-url path))
         (url-request-timeout my-futon3-request-timeout))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error))
             (result nil)
             (msg nil))
         (unwind-protect
             (if err
                 (setq msg (format "Futon3 %s failed: %s" path err))
               (condition-case parse-err
                   (setq result (my-futon3--parse-json-response path))
                 (error
                  (setq msg (error-message-string parse-err)))))
           (when (buffer-live-p (current-buffer))
             (kill-buffer (current-buffer))))
         (when callback
           (funcall callback result msg))))
     nil t)))

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

(defun my-futon3-fetch-hints-async (arg callback)
  "Fetch /musn/hints asynchronously.
ARG may be a legacy SIGILS vector or a plist (:sigils ... :intent ...).
CALLBACK receives (RESULT ERROR)."
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
    (my-futon3--json-request-async
     "POST" "/musn/hints" payload
     (lambda (result err)
       (when err
         (setq my-chatgpt-shell--last-hints-error err)
         (message "Futon3 hints failed: %s" err))
       (when (and result (null err))
         (setq my-chatgpt-shell--last-hints-error nil))
       (when callback
         (funcall callback result err))))))

(provide 'futon3-bridge)

;;; futon3-bridge.el ends here
