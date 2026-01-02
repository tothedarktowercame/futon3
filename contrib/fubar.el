;;; fubar.el --- Fulab event logging for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Capture human-side Fulab events (fubar) from Emacs integrations.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)

(defvar my-fubar--repo-root
  (let* ((base (or load-file-name
                   (buffer-file-name)
                   default-directory)))
    (expand-file-name ".." (file-name-directory base)))
  "Filesystem root of the futon3 checkout used by fubar.")

(defvar my-fubar-fulab-endpoint nil
  "Base URL for Fulab event ingestion. When nil, log to `my-fubar-edn-log-file`.")

(defvar my-fubar-fulab-endpoint-path "/fulab/event"
  "Path appended to `my-fubar-fulab-endpoint` to post events.")

(defvar my-fubar-request-timeout 2
  "Seconds to wait for Fulab HTTP requests before giving up.")

(defvar my-fubar-edn-log-file
  (expand-file-name "resources/fubar-events.edn" my-fubar--repo-root)
  "Local EDN log for Fulab events when endpoint is unavailable.")

(defvar my-fubar--sessions (make-hash-table :test 'equal)
  "Session registry for fubar stats and metadata.")

(defvar my-fubar-session-id-fn nil
  "Optional function that returns the current fubar session id.")

(defvar my-fubar--latched-session-id nil
  "When non-nil, reuse this session id for subsequent fubar events.")

(defun my-fubar--now-ms ()
  (floor (* 1000 (float-time (current-time)))))

(defun my-fubar--initial-stats ()
  (list :events 0
        :patterns-used 0
        :artifacts 0
        :docs-written 0
        :cost-usd 0.0
        :duration-ms 0
        :first-event-at nil
        :last-event-at nil))

(defun my-fubar--ensure-session (session-id)
  (or (gethash session-id my-fubar--sessions)
      (let ((session (list :id session-id
                           :pattern-id nil
                           :patterns-used (vector)
                           :artifacts (vector)
                           :docs-written (vector)
                           :society-paper nil
                           :stats (my-fubar--initial-stats))))
        (puthash session-id session my-fubar--sessions)
        session)))

(defun my-fubar--update-session (session-id updater)
  (let ((session (my-fubar--ensure-session session-id)))
    (let ((next (funcall updater session)))
      (puthash session-id next my-fubar--sessions)
      next)))

(defun my-fubar--update-stats (session type)
  (let* ((now (my-fubar--now-ms))
         (stats (plist-get session :stats))
         (first (plist-get stats :first-event-at))
         (stats (plist-put stats :events (1+ (or (plist-get stats :events) 0))))
         (stats (plist-put stats :last-event-at now))
         (stats (if first stats (plist-put stats :first-event-at now))))
    (setq stats
          (pcase type
            (:pattern/used
             (plist-put stats :patterns-used (1+ (or (plist-get stats :patterns-used) 0))))
            (:doc/written
             (plist-put stats :docs-written (1+ (or (plist-get stats :docs-written) 0))))
            (_ stats)))
    (plist-put session :stats stats)))

(defun my-fubar--uuid ()
  (let* ((seed (format "%s-%s-%s" (float-time) (random) (user-uid)))
         (hex (md5 seed)))
    (format "%s-%s-%s-%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 12 16)
            (substring hex 16 20)
            (substring hex 20 32))))

(defun my-fubar--make-event (session-id type &optional data)
  (let ((base (list :event/id (my-fubar--uuid)
                    :event/session session-id
                    :event/type type
                    :event/ts (my-fubar--now-ms)
                    :event/source :fubar)))
    (if data
        (append base data)
      base)))

(defun my-fubar--load-events ()
  (let ((file my-fubar-edn-log-file))
    (if (and file (file-readable-p file))
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (read (current-buffer)))
          (error
           (message "[fubar] Log unreadable: %s" (error-message-string err))
           []))
      [])))

(defun my-fubar--write-events (events)
  (let ((file my-fubar-edn-log-file))
    (when file
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix)
            (print-length nil)
            (print-level nil))
        (with-temp-file file
          (prin1 events (current-buffer))
          (insert "\n"))))))

(defun my-fubar--append-event (event)
  (let* ((events (my-fubar--load-events))
         (next (vconcat events (vector event))))
    (my-fubar--write-events next)
    next))

(defun my-fubar--session-snapshot (session-id)
  (let ((session (my-fubar--ensure-session session-id)))
    (list :pattern/primary (plist-get session :pattern-id)
          :pattern/trail (plist-get session :patterns-used)
          :artifacts (plist-get session :artifacts)
          :docs-written (plist-get session :docs-written)
          :stats (plist-get session :stats)
          :society-paper (plist-get session :society-paper))))

(defun my-fubar--merge-plists (base override)
  (let ((result base)
        (cursor override))
    (while cursor
      (setq result (plist-put result (car cursor) (cadr cursor)))
      (setq cursor (cddr cursor)))
    result))

(defun my-fubar--normalize-endpoint (endpoint)
  (when (and endpoint (stringp endpoint))
    (replace-regexp-in-string "/+$" "" endpoint)))

(defun my-fubar--post-event (event)
  (let* ((base (my-fubar--normalize-endpoint my-fubar-fulab-endpoint))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode event))
         (coding-system-for-write 'utf-8)
         (coding-system-for-read 'utf-8)
         (url (when base (concat base my-fubar-fulab-endpoint-path))))
    (when url
      (condition-case err
          (with-current-buffer (url-retrieve-synchronously url t t my-fubar-request-timeout)
            (kill-buffer (current-buffer))
            t)
        (error
         (message "[fubar] Fulab sync failed: %s" (error-message-string err))
         nil)))))

(defun my-fubar-load-events ()
  "Return logged Fulab events as a vector."
  (my-fubar--load-events))

(defun my-fubar-set-session-id (session-id)
  "Latch SESSION-ID for subsequent fubar events."
  (setq my-fubar--latched-session-id session-id))

(defun my-fubar-clear-session-id ()
  "Clear the latched fubar session id."
  (setq my-fubar--latched-session-id nil))

(defun my-fubar-session-id ()
  "Return the current fubar session id."
  (cond
   (my-fubar--latched-session-id my-fubar--latched-session-id)
   ((functionp my-fubar-session-id-fn) (funcall my-fubar-session-id-fn))
   (t (format "fubar-%s" (my-fubar--now-ms)))))

(defun my-fubar-get-session (session-id)
  "Return the current session snapshot for SESSION-ID."
  (my-fubar--ensure-session session-id))

(defun my-fubar-log-event (session-id type &optional data)
  "Log a Fulab event for SESSION-ID.

TYPE should be a keyword like :clock-in/start, :pattern/used, :doc/written.
DATA is a plist merged into the event payload."
  (let* ((session (my-fubar--update-session session-id
                                            (lambda (sess)
                                              (my-fubar--update-stats sess type))))
         (event (my-fubar--make-event session-id type data)))
    (when (eq type :pattern/used)
      (let* ((usage (list :pattern/id (plist-get data :pattern/id)
                          :used-at (my-fubar--now-ms)
                          :reason (plist-get data :pattern/reason)))
             (trail (vconcat (plist-get session :patterns-used) (vector usage))))
        (my-fubar--update-session session-id
                                  (lambda (sess) (plist-put sess :patterns-used trail)))))
    (when (eq type :doc/written)
      (let ((docs (vconcat (plist-get session :docs-written) (vector data))))
        (my-fubar--update-session session-id
                                  (lambda (sess) (plist-put sess :docs-written docs)))))
    (unless (and (my-fubar--post-event event) my-fubar-fulab-endpoint)
      (my-fubar--append-event event))
    event))

(defun my-fubar-log-clock-in (session-id pattern-id &optional intent context)
  (my-fubar-set-session-id session-id)
  (my-fubar--update-session session-id
                            (lambda (session)
                              (plist-put session :pattern-id pattern-id)))
  (my-fubar-log-event session-id :clock-in/start
                      (list :clock-in/pattern-id pattern-id
                            :clock-in/intent intent
                            :clock-in/context (or context (list))
                            :clock-in/timestamp (my-fubar--now-ms))))

(defun my-fubar-log-pattern-used (session-id pattern-id &optional reason)
  (my-fubar-log-event session-id :pattern/used
                      (list :pattern/id pattern-id
                            :pattern/reason reason)))

(defun my-fubar-log-doc-written (session-id doc)
  (my-fubar-log-event session-id :doc/written doc))

(defun my-fubar-record-artifact (session-id artifact-path &optional action)
  (my-fubar--update-session session-id
                            (lambda (session)
                              (let* ((artifact (list :path artifact-path
                                                     :action (or action :modified)
                                                     :at (my-fubar--now-ms)))
                                     (artifacts (vconcat (plist-get session :artifacts)
                                                        (vector artifact)))
                                     (stats (plist-get session :stats))
                                     (stats (plist-put stats :artifacts
                                                       (1+ (or (plist-get stats :artifacts) 0)))))
                                (setq session (plist-put session :artifacts artifacts))
                                (plist-put session :stats stats))))
  t)

(defun my-fubar-set-society-paper (session-id summary)
  (my-fubar--update-session session-id
                            (lambda (session)
                              (plist-put session :society-paper summary)))
  t)

(defun my-fubar-log-clock-out (session-id &optional payload)
  (let* ((snapshot (my-fubar--session-snapshot session-id))
         (body (if payload
                   (my-fubar--merge-plists snapshot payload)
                 snapshot)))
    (my-fubar-clear-session-id)
    (my-fubar-log-event session-id :clock-out/complete body)))

(defun my-fubar-clock-in (pattern-id intent)
  "Interactively clock in a fubar session."
  (interactive "sPattern id: \nsIntent: ")
  (let ((session-id (my-fubar-session-id)))
    (my-fubar-log-clock-in session-id pattern-id intent)
    (message "[fubar] clocked in %s" session-id)))

(defun my-fubar-clock-out (&optional status)
  "Interactively clock out the current fubar session."
  (interactive "sStatus (success/error): ")
  (let ((session-id (my-fubar-session-id))
        (status (if (string-empty-p (or status "")) :success (intern (concat ":" status)))))
    (my-fubar-log-clock-out session-id (list :session/status status))
    (message "[fubar] clocked out %s" session-id)))

(defun my-fubar-society-paper (summary)
  "Interactively set society paper for the current fubar session."
  (interactive "sSociety paper summary: ")
  (let ((session-id (my-fubar-session-id)))
    (my-fubar-set-society-paper session-id summary)
    (message "[fubar] society paper set for %s" session-id)))

(provide 'fubar)

;;; fubar.el ends here
