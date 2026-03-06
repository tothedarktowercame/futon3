;;; testing.el --- Minimal lab-ws streaming test -*- lexical-binding: t; -*-

;;; Commentary:
;; Test lab-ws session streaming locally on Linode.
;; Run: emacs -Q -l testing.el

;;; Code:

(defvar bootstrap-version)
(let ((bf (expand-file-name "straight/repos/straight.el/bootstrap.el"
                            user-emacs-directory)))
  (unless (file-exists-p bf)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bf nil 'nomessage))

;; Install use-package via straight and set sane defaults
(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t      ; defer by default for faster init
      use-package-verbose nil)

(use-package websocket :straight t)

(require 'websocket)
(require 'json)

(defvar test-ws nil "WebSocket connection.")
(defvar test-buffer "*Lab Test*" "Output buffer.")
(defvar test-event-count 0 "Count of events received.")

(defun test-lab-connect (jsonl-path)
  "Connect to lab-ws and stream JSONL-PATH."
  (interactive "fJSONL file: ")
  (when test-ws
    (websocket-close test-ws)
    (setq test-ws nil))
  (setq test-event-count 0)
  (let ((url (format "ws://localhost:5056?path=%s"
                     (url-hexify-string (expand-file-name jsonl-path)))))
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (insert (format "Connecting to: %s\n\n" url)))
    (pop-to-buffer test-buffer)
    (setq test-ws
          (websocket-open url
                          :on-message #'test-on-message
                          :on-close #'test-on-close
                          :on-error #'test-on-error))
    (message "Connecting...")))

(defun test-on-message (_ws frame)
  "Handle incoming FRAME."
  (let* ((text (websocket-frame-text frame))
         (json-object-type 'alist)
         (json-array-type 'list)
         (data (condition-case nil
                   (json-read-from-string text)
                 (error nil))))
    (when data
      (let ((msg-type (cdr (assq 'type data))))
        (with-current-buffer (get-buffer-create test-buffer)
          (goto-char (point-max))
          (pcase msg-type
            ("init"
             (let ((events (cdr (assq 'events data)))
                   (line-count (cdr (assq 'line-count data)))
                   (par-count (cdr (assq 'par-count data))))
               (insert (format "=== INIT: %d lines, %d events, %d PARs ===\n\n"
                               (or line-count 0)
                               (length events)
                               (or par-count 0)))
               (dolist (event events)
                 (test-format-event event))
               (setq test-event-count (length events))))
            ("event"
             (let ((event (cdr (assq 'event data))))
               (cl-incf test-event-count)
               (test-format-event event)
               (message "Events: %d" test-event-count)))
            ("error"
             (insert (format "\n!!! ERROR: %s !!!\n" (cdr (assq 'err data)))))
            (_
             (insert (format "[%s]\n" msg-type)))))))))

(defun test-format-event (event)
  "Format and insert EVENT."
  (let ((type (cdr (assq 'type event)))
        (text (cdr (assq 'text event)))
        (timestamp (cdr (assq 'timestamp event))))
    (when (and type text)
      (insert (format "[%s] %s: %s\n"
                      (if timestamp (substring timestamp 11 19) "??:??:??")
                      (upcase type)
                      (if (> (length text) 200)
                          (concat (substring text 0 197) "...")
                        text))))))

(defun test-on-close (_ws)
  "Handle close."
  (message "Connection closed. Total events: %d" test-event-count)
  (with-current-buffer (get-buffer-create test-buffer)
    (goto-char (point-max))
    (insert (format "\n=== CLOSED: %d events ===\n" test-event-count))))

(defun test-on-error (_ws _type err)
  "Handle error ERR."
  (message "WebSocket error: %s" err)
  (with-current-buffer (get-buffer-create test-buffer)
    (goto-char (point-max))
    (insert (format "\n!!! ERROR: %s !!!\n" err))))

(defun test-lab-disconnect ()
  "Disconnect."
  (interactive)
  (when test-ws
    (websocket-close test-ws)
    (setq test-ws nil)
    (message "Disconnected")))

(defun test-lab-current-session ()
  "Connect to the current active session on this machine."
  (interactive)
  (let* ((sessions-url "http://localhost:5050/fulab/lab/sessions/active")
         (response (with-current-buffer
                       (url-retrieve-synchronously sessions-url t t 5)
                     (goto-char (point-min))
                     (re-search-forward "\n\n" nil t)
                     (json-read)))
         (sessions (cdr (assq 'sessions response)))
         (active (seq-find (lambda (s) (cdr (assq 'active s))) sessions)))
    (if active
        (let ((path (cdr (assq 'path active))))
          (message "Found active session: %s" path)
          (test-lab-connect path))
      (message "No active session found"))))

;; Keybindings for test buffer
(defvar test-lab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'test-lab-disconnect)
    (define-key map "g" #'test-lab-current-session)
    map))

(define-derived-mode test-lab-mode special-mode "Lab-Test"
  "Mode for lab-ws testing.")

(provide 'testing)
;;; testing.el ends here
