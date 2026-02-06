;;; futon4-lab-bridge.el --- Stream futon3 MUSN events to futon4 Lab -*- lexical-binding: t; -*-

;; Bridge that streams MUSN session events from futon3's WebSocket
;; to the lab/raw/ format that futon4's Lab browser reads.

;;; Commentary:
;;
;; Usage:
;;   M-x futon4-lab-bridge-connect RET <session-id> RET
;;
;; This connects to futon3's WebSocket stream for the given session
;; and incrementally writes events to lab/raw/{session-id}.json.
;; Press 'g' in futon4's Lab browser to see updates.
;;
;; For auto-refresh, enable `futon4-lab-bridge-auto-refresh'.

;;; Code:

(require 'websocket)
(require 'json)

(defgroup futon4-lab-bridge nil
  "Bridge futon3 MUSN events to futon4 Lab browser."
  :group 'tools)

(defcustom futon4-lab-bridge-host "localhost"
  "Futon3 WebSocket host."
  :type 'string)

(defcustom futon4-lab-bridge-port 5050
  "Futon3 WebSocket port."
  :type 'integer)

(defcustom futon4-lab-bridge-lab-root nil
  "Root directory for lab/ output. If nil, uses futon3's lab/ directory."
  :type '(choice (const nil) directory))

(defcustom futon4-lab-bridge-auto-refresh t
  "If non-nil, auto-refresh futon4 Lab browser on updates."
  :type 'boolean)

(defcustom futon4-lab-bridge-write-interval 2.0
  "Seconds between writing updates to lab/raw/."
  :type 'number)

(defcustom futon4-lab-bridge-enrich t
  "If non-nil, enrich events with patterns and embeddings via /lab/enrich."
  :type 'boolean)

(defcustom futon4-lab-bridge-enrich-async t
  "If non-nil, enrich events asynchronously to avoid blocking."
  :type 'boolean)

(defvar futon4-lab-bridge--ws nil
  "Active WebSocket connection.")

(defvar futon4-lab-bridge--session-id nil
  "Current session ID being streamed.")

(defvar futon4-lab-bridge--events nil
  "Accumulated events for current session.")

(defvar futon4-lab-bridge--user-messages nil
  "Accumulated user messages.")

(defvar futon4-lab-bridge--assistant-messages nil
  "Accumulated assistant messages.")

(defvar futon4-lab-bridge--files-touched nil
  "Set of files touched during session.")

(defvar futon4-lab-bridge--timestamp-start nil
  "Session start timestamp.")

(defvar futon4-lab-bridge--timestamp-end nil
  "Most recent event timestamp.")

(defvar futon4-lab-bridge--write-timer nil
  "Timer for periodic writes.")

(defvar futon4-lab-bridge--dirty nil
  "Non-nil if there are unwritten changes.")

(defvar futon4-lab-bridge--enrichments nil
  "Accumulated enrichments for events (pattern matches, embeddings, proposals).")

(defvar futon4-lab-bridge--pending-enrichments 0
  "Count of pending async enrichment requests.")

;; ---------------------------------------------------------------------------
;; Enrichment API
;; ---------------------------------------------------------------------------

(defun futon4-lab-bridge--enrich-url ()
  "Return URL for /lab/enrich endpoint."
  (format "http://%s:%d/lab/enrich"
          futon4-lab-bridge-host
          futon4-lab-bridge-port))

(defun futon4-lab-bridge--enrich-content (content msg-id callback)
  "Enrich CONTENT via /lab/enrich endpoint, call CALLBACK with result.
MSG-ID is used to associate the enrichment with its message."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (json-encode
                           `((content . ,content)
                             (session-id . ,futon4-lab-bridge--session-id)
                             (pattern-limit . 5)
                             (use-portal . t)))))
    (if futon4-lab-bridge-enrich-async
        (progn
          (cl-incf futon4-lab-bridge--pending-enrichments)
          (url-retrieve
           (futon4-lab-bridge--enrich-url)
           (lambda (status)
             (cl-decf futon4-lab-bridge--pending-enrichments)
             (if-let ((err (plist-get status :error)))
                 (message "[futon4-lab-bridge] Enrich error: %s" err)
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (let* ((json-object-type 'alist)
                        (json-array-type 'list)
                        (response (json-read)))
                   (when (cdr (assoc 'ok response))
                     (funcall callback msg-id (cdr (assoc 'enrichment response))))))))
           nil t t))
      ;; Synchronous version
      (with-current-buffer (url-retrieve-synchronously (futon4-lab-bridge--enrich-url) t t 5)
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (response (json-read)))
            (when (cdr (assoc 'ok response))
              (funcall callback msg-id (cdr (assoc 'enrichment response))))))))))

(defun futon4-lab-bridge--store-enrichment (msg-id enrichment)
  "Store ENRICHMENT for MSG-ID."
  (when enrichment
    (push `((msg-id . ,msg-id)
            (patterns . ,(cdr (assoc 'patterns enrichment)))
            (cues . ,(cdr (assoc 'cues enrichment)))
            (embedding . ,(cdr (assoc 'embedding enrichment)))
            (proposals . ,(cdr (assoc 'proposals enrichment))))
          futon4-lab-bridge--enrichments)
    (setq futon4-lab-bridge--dirty t)))

(defun futon4-lab-bridge--lab-root ()
  "Return the lab root directory."
  (or futon4-lab-bridge-lab-root
      (expand-file-name "lab" (locate-dominating-file default-directory ".git"))))

(defun futon4-lab-bridge--raw-path ()
  "Return path to lab/raw/{session-id}.json."
  (expand-file-name
   (format "raw/%s.json" futon4-lab-bridge--session-id)
   (futon4-lab-bridge--lab-root)))

(defun futon4-lab-bridge--trace-path ()
  "Return relative path to trace file."
  (format "lab/trace/%s.org" futon4-lab-bridge--session-id))

(defun futon4-lab-bridge--extract-timestamp (event)
  "Extract timestamp from EVENT."
  (or (cdr (assoc 'at event))
      (cdr (assoc 'ts event))
      (cdr (assoc 'timestamp event))))

(defun futon4-lab-bridge--extract-role (event)
  "Extract message role from EVENT (user or assistant)."
  (let ((event-type (or (cdr (assoc 'event/type event))
                        (cdr (assoc 'type event)))))
    (cond
     ((string-match-p "user" (or event-type "")) 'user)
     ((string-match-p "\\(agent\\|assistant\\|turn\\)" (or event-type "")) 'assistant)
     (t nil))))

(defun futon4-lab-bridge--extract-text (event)
  "Extract text content from EVENT."
  (or (cdr (assoc 'content (cdr (assoc 'payload event))))
      (cdr (assoc 'text (cdr (assoc 'payload event))))
      (cdr (assoc 'content event))
      (cdr (assoc 'text event))))

(defun futon4-lab-bridge--extract-files (event)
  "Extract touched files from EVENT."
  (let ((payload (cdr (assoc 'payload event))))
    (when payload
      (let ((path (cdr (assoc 'path payload)))
            (files (cdr (assoc 'files payload))))
        (cond
         (path (list path))
         ((listp files) files)
         (files (list files))
         (t nil))))))

(defun futon4-lab-bridge--process-event (event)
  "Process a single EVENT, accumulating data."
  (let ((ts (futon4-lab-bridge--extract-timestamp event))
        (role (futon4-lab-bridge--extract-role event))
        (text (futon4-lab-bridge--extract-text event))
        (files (futon4-lab-bridge--extract-files event)))
    ;; Update timestamps
    (when ts
      (unless futon4-lab-bridge--timestamp-start
        (setq futon4-lab-bridge--timestamp-start ts))
      (setq futon4-lab-bridge--timestamp-end ts))
    ;; Accumulate messages
    (when (and role text (not (string-empty-p text)))
      (let ((msg-id (format "%s:%s%03d"
                            futon4-lab-bridge--session-id
                            (if (eq role 'user) "u" "a")
                            (1+ (length (if (eq role 'user)
                                            futon4-lab-bridge--user-messages
                                          futon4-lab-bridge--assistant-messages))))))
        (if (eq role 'user)
            (push `((id . ,msg-id) (timestamp . ,ts) (text . ,text))
                  futon4-lab-bridge--user-messages)
          (push `((id . ,msg-id) (timestamp . ,ts) (text . ,text))
                futon4-lab-bridge--assistant-messages))
        ;; Enrich content with patterns/embeddings
        (when (and futon4-lab-bridge-enrich
                   (> (length text) 20))  ; Only enrich substantial content
          (futon4-lab-bridge--enrich-content
           text msg-id #'futon4-lab-bridge--store-enrichment))))
    ;; Accumulate files
    (dolist (f files)
      (when (and f (stringp f))
        (cl-pushnew f futon4-lab-bridge--files-touched :test #'equal)))
    ;; Store raw event
    (push event futon4-lab-bridge--events)
    (setq futon4-lab-bridge--dirty t)))

(defun futon4-lab-bridge--build-raw-json ()
  "Build the lab/raw JSON structure from accumulated data."
  (let ((base `((lab/session-id . ,futon4-lab-bridge--session-id)
                (lab/repo-root . ,(locate-dominating-file default-directory ".git"))
                (lab/source . "musn/futon3")
                (lab/timestamp-start . ,futon4-lab-bridge--timestamp-start)
                (lab/timestamp-end . ,futon4-lab-bridge--timestamp-end)
                (lab/user-messages . ,(vconcat (nreverse (copy-sequence futon4-lab-bridge--user-messages))))
                (lab/assistant-messages . ,(vconcat (nreverse (copy-sequence futon4-lab-bridge--assistant-messages))))
                ;; Include all events in chronological order for MUSN timeline
                (lab/all-events . ,(vconcat (nreverse (copy-sequence futon4-lab-bridge--events))))
                (lab/files-touched . ,(vconcat (sort (copy-sequence futon4-lab-bridge--files-touched) #'string<)))
                (lab/trace-path . ,(futon4-lab-bridge--trace-path))
                (lab/doc-draft-path . ,(format "lab/doc-drafts/%s.json" futon4-lab-bridge--session-id))
                (lab/errors . []))))
    ;; Add enrichments if present
    (when futon4-lab-bridge--enrichments
      (push `(lab/enrichments . ,(vconcat (nreverse (copy-sequence futon4-lab-bridge--enrichments))))
            base)
      ;; Extract unique patterns across all enrichments for summary
      (let ((all-patterns (make-hash-table :test 'equal)))
        (dolist (e futon4-lab-bridge--enrichments)
          (dolist (p (cdr (assoc 'patterns e)))
            (let ((id (cdr (assoc 'id p))))
              (when id
                (puthash id p all-patterns)))))
        (when (> (hash-table-count all-patterns) 0)
          (let (patterns-list)
            (maphash (lambda (_k v) (push v patterns-list)) all-patterns)
            (push `(lab/patterns-summary . ,(vconcat (sort patterns-list
                                                           (lambda (a b)
                                                             (> (or (cdr (assoc 'score a)) 0)
                                                                (or (cdr (assoc 'score b)) 0))))))
                  base))))
      ;; Extract proposals
      (let ((all-proposals nil))
        (dolist (e futon4-lab-bridge--enrichments)
          (dolist (p (cdr (assoc 'proposals e)))
            (push p all-proposals)))
        (when all-proposals
          (push `(lab/proposals . ,(vconcat all-proposals)) base))))
    base))

(defun futon4-lab-bridge--write-raw ()
  "Write accumulated data to lab/raw/{session-id}.json."
  (when (and futon4-lab-bridge--dirty futon4-lab-bridge--session-id)
    (let ((path (futon4-lab-bridge--raw-path))
          (data (futon4-lab-bridge--build-raw-json)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert (json-encode data)))
      (setq futon4-lab-bridge--dirty nil)
      (message "[futon4-lab-bridge] Wrote %s (%d events)"
               (file-name-nondirectory path)
               (length futon4-lab-bridge--events))
      ;; Auto-refresh futon4 browser if available
      (when futon4-lab-bridge-auto-refresh
        (futon4-lab-bridge--maybe-refresh-browser)))))

(defun futon4-lab-bridge--maybe-refresh-browser ()
  "Refresh futon4 Lab browser if visible."
  (when-let ((buf (get-buffer "*Arxana*")))
    (with-current-buffer buf
      (when (and (boundp 'arxana-browser--current-view)
                 (eq arxana-browser--current-view 'lab))
        (when (fboundp 'arxana-browser--render)
          (arxana-browser--render))))))

(defun futon4-lab-bridge--on-message (_ws frame)
  "Handle incoming WebSocket FRAME."
  (let* ((text (websocket-frame-text frame))
         (json-object-type 'alist)
         (json-array-type 'list)
         (event (condition-case nil
                    (json-read-from-string text)
                  (error nil))))
    (when event
      (futon4-lab-bridge--process-event event))))

(defun futon4-lab-bridge--on-close (_ws)
  "Handle WebSocket close."
  (message "[futon4-lab-bridge] Connection closed")
  (futon4-lab-bridge--write-raw)  ; Final write
  (when futon4-lab-bridge--write-timer
    (cancel-timer futon4-lab-bridge--write-timer)
    (setq futon4-lab-bridge--write-timer nil)))

(defun futon4-lab-bridge--reset-state ()
  "Reset accumulator state for new session."
  (setq futon4-lab-bridge--events nil
        futon4-lab-bridge--user-messages nil
        futon4-lab-bridge--assistant-messages nil
        futon4-lab-bridge--files-touched nil
        futon4-lab-bridge--timestamp-start nil
        futon4-lab-bridge--timestamp-end nil
        futon4-lab-bridge--dirty nil
        futon4-lab-bridge--enrichments nil
        futon4-lab-bridge--pending-enrichments 0))

;;;###autoload
(defun futon4-lab-bridge-connect (session-id)
  "Connect to futon3 WebSocket and stream SESSION-ID to lab/raw/."
  (interactive "sSession ID: ")
  (futon4-lab-bridge-disconnect)
  (futon4-lab-bridge--reset-state)
  (setq futon4-lab-bridge--session-id session-id)
  (let ((url (format "ws://%s:%d/fulab/session/%s/ws"
                     futon4-lab-bridge-host
                     futon4-lab-bridge-port
                     session-id)))
    (message "[futon4-lab-bridge] Connecting to %s..." url)
    (setq futon4-lab-bridge--ws
          (websocket-open url
                          :on-message #'futon4-lab-bridge--on-message
                          :on-close #'futon4-lab-bridge--on-close
                          :on-error (lambda (_ws _type err)
                                      (message "[futon4-lab-bridge] Error: %s" err))))
    ;; Start periodic write timer
    (setq futon4-lab-bridge--write-timer
          (run-with-timer futon4-lab-bridge-write-interval
                          futon4-lab-bridge-write-interval
                          #'futon4-lab-bridge--write-raw))
    (message "[futon4-lab-bridge] Connected, streaming to %s"
             (futon4-lab-bridge--raw-path))))

;;;###autoload
(defun futon4-lab-bridge-disconnect ()
  "Disconnect from futon3 WebSocket."
  (interactive)
  (when futon4-lab-bridge--write-timer
    (cancel-timer futon4-lab-bridge--write-timer)
    (setq futon4-lab-bridge--write-timer nil))
  (when futon4-lab-bridge--ws
    (futon4-lab-bridge--write-raw)  ; Final write
    (websocket-close futon4-lab-bridge--ws)
    (setq futon4-lab-bridge--ws nil)
    (message "[futon4-lab-bridge] Disconnected")))

;;;###autoload
(defun futon4-lab-bridge-status ()
  "Show current bridge status."
  (interactive)
  (if futon4-lab-bridge--ws
      (message "[futon4-lab-bridge] Connected to %s: %d events, %d files, %d enrichments%s"
               futon4-lab-bridge--session-id
               (length futon4-lab-bridge--events)
               (length futon4-lab-bridge--files-touched)
               (length futon4-lab-bridge--enrichments)
               (if (> futon4-lab-bridge--pending-enrichments 0)
                   (format " (%d pending)" futon4-lab-bridge--pending-enrichments)
                 ""))
    (message "[futon4-lab-bridge] Not connected")))

;;;###autoload
(defun futon4-lab-bridge-show-patterns ()
  "Show patterns discovered across enrichments."
  (interactive)
  (if (null futon4-lab-bridge--enrichments)
      (message "[futon4-lab-bridge] No enrichments yet")
    (let ((patterns (make-hash-table :test 'equal)))
      (dolist (e futon4-lab-bridge--enrichments)
        (dolist (p (cdr (assoc 'patterns e)))
          (let ((id (cdr (assoc 'id p))))
            (when id
              (puthash id (1+ (or (gethash id patterns) 0)) patterns)))))
      (if (= (hash-table-count patterns) 0)
          (message "[futon4-lab-bridge] No patterns found")
        (let ((sorted nil))
          (maphash (lambda (k v) (push (cons k v) sorted)) patterns)
          (setq sorted (sort sorted (lambda (a b) (> (cdr a) (cdr b)))))
          (message "[futon4-lab-bridge] Patterns: %s"
                   (mapconcat (lambda (p) (format "%s(%d)" (car p) (cdr p)))
                              sorted ", ")))))))

(provide 'futon4-lab-bridge)
;;; futon4-lab-bridge.el ends here
