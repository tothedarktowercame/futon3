;;; fuclient-logs.el --- WebSocket log streaming for MUSN sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Stream MUSN session events to Emacs in real-time via WebSocket.
;; Provides an Arxana-style structured view of notebook turns, plans, and tool actions.
;;
;; Usage:
;;   M-x fuclient-logs-connect RET <session-id> RET
;;   M-x fuclient-logs-disconnect
;;   M-x fuclient-logs-clear
;;
;; Keybindings in *FuLogs* buffer:
;;   g - reconnect
;;   c - clear buffer
;;   q - disconnect and quit
;;   RET - open file path at point
;;   f - filter by level
;;   n - narrow to run ID

;;; Code:

(require 'json)
(require 'cl-lib)

;; Optional websocket.el dependency - check availability
(declare-function websocket-open "websocket" (url &rest args))
(declare-function websocket-send-text "websocket" (ws text))
(declare-function websocket-close "websocket" (ws))

(defgroup fuclient-logs nil
  "WebSocket log streaming for MUSN sessions."
  :group 'fubar
  :prefix "fuclient-logs-")

(defcustom fuclient-logs-server-url "ws://localhost:5050"
  "WebSocket server base URL (without path)."
  :type 'string
  :group 'fuclient-logs)

(defcustom fuclient-logs-buffer-name "*FuLogs*"
  "Name of the log streaming buffer."
  :type 'string
  :group 'fuclient-logs)

(defcustom fuclient-logs-max-lines 5000
  "Maximum lines to keep in the log buffer."
  :type 'integer
  :group 'fuclient-logs)

(defcustom fuclient-logs-timestamp-format "%H:%M:%S"
  "Format for timestamps in log display."
  :type 'string
  :group 'fuclient-logs)

(defcustom fuclient-logs-show-payload t
  "When non-nil, show event payload details."
  :type 'boolean
  :group 'fuclient-logs)

;;; State

(defvar fuclient-logs--websocket nil
  "Active WebSocket connection.")

(defvar fuclient-logs--session-id nil
  "Current session ID.")

(defvar fuclient-logs--filter-level nil
  "Current filter level (nil = show all).")

(defvar fuclient-logs--narrow-run nil
  "When non-nil, only show events from this run ID.")

(defvar-local fuclient-logs--event-count 0
  "Count of events received in this session.")

;;; Faces

(defface fuclient-logs-timestamp-face
  '((t :foreground "gray60"))
  "Face for timestamps."
  :group 'fuclient-logs)

(defface fuclient-logs-event-type-face
  '((t :foreground "cyan" :weight bold))
  "Face for event types."
  :group 'fuclient-logs)

(defface fuclient-logs-turn-agent-face
  '((t :foreground "green"))
  "Face for agent turn markers."
  :group 'fuclient-logs)

(defface fuclient-logs-turn-user-face
  '((t :foreground "yellow"))
  "Face for user turn markers."
  :group 'fuclient-logs)

(defface fuclient-logs-tool-face
  '((t :foreground "orange"))
  "Face for tool actions."
  :group 'fuclient-logs)

(defface fuclient-logs-file-path-face
  '((t :foreground "dodger blue" :underline t))
  "Face for clickable file paths."
  :group 'fuclient-logs)

(defface fuclient-logs-error-face
  '((t :foreground "red" :weight bold))
  "Face for error messages."
  :group 'fuclient-logs)

(defface fuclient-logs-plan-face
  '((t :foreground "magenta"))
  "Face for plan events."
  :group 'fuclient-logs)

(defface fuclient-logs-par-face
  '((t :foreground "deep sky blue" :weight bold))
  "Face for PAR section headers."
  :group 'fuclient-logs)

(defface fuclient-logs-hint-face
  '((t :foreground "gold" :slant italic))
  "Face for MUSN hints."
  :group 'fuclient-logs)

(defface fuclient-logs-anchor-face
  '((t :foreground "spring green" :weight bold))
  "Face for anchor references."
  :group 'fuclient-logs)

(defface fuclient-logs-link-face
  '((t :foreground "light sky blue" :underline t))
  "Face for clickable link references."
  :group 'fuclient-logs)

;;; Utilities

(defun fuclient-logs--format-timestamp (iso-string)
  "Format ISO-STRING timestamp for display."
  (if iso-string
      (condition-case nil
          (let* ((parsed (parse-iso8601-time-string iso-string))
                 (time (encode-time parsed)))
            (format-time-string fuclient-logs-timestamp-format time))
        (error (substring iso-string 11 19)))
    "??:??:??"))

(defun fuclient-logs--make-file-link (path &optional line)
  "Create a clickable link for PATH, optionally with LINE."
  (let ((map (make-sparse-keymap))
        (display-path (if (> (length path) 50)
                          (concat "..." (substring path -47))
                        path)))
    (define-key map [mouse-1]
      (lambda () (interactive)
        (fuclient-logs--open-file path line)))
    (define-key map (kbd "RET")
      (lambda () (interactive)
        (fuclient-logs--open-file path line)))
    (propertize display-path
                'face 'fuclient-logs-file-path-face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "Click to open %s%s"
                                   path
                                   (if line (format ":%d" line) ""))
                'fuclient-logs-path path
                'fuclient-logs-line line)))

(defun fuclient-logs--open-file (path &optional line)
  "Open file at PATH, optionally jumping to LINE."
  (when (and path (file-exists-p path))
    (find-file-other-window path)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun fuclient-logs--extract-paths (payload)
  "Extract file paths from PAYLOAD."
  (let ((paths nil))
    (when-let ((path (plist-get payload :path)))
      (push (list :path path :line (plist-get payload :line)) paths))
    (when-let ((file (plist-get payload :file)))
      (push (list :path file :line (plist-get payload :line)) paths))
    paths))

(defun fuclient-logs--make-anchor-link (anchor-id)
  "Create a clickable link for ANCHOR-ID."
  (let ((map (make-sparse-keymap))
        (display-id (if (> (length anchor-id) 40)
                        (concat "..." (substring anchor-id -37))
                      anchor-id)))
    (define-key map [mouse-1]
      (lambda () (interactive)
        (fuclient-logs--navigate-anchor anchor-id)))
    (define-key map (kbd "RET")
      (lambda () (interactive)
        (fuclient-logs--navigate-anchor anchor-id)))
    (propertize display-id
                'face 'fuclient-logs-anchor-face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "Click to navigate to anchor %s" anchor-id)
                'fuclient-logs-anchor anchor-id)))

(defun fuclient-logs--navigate-anchor (anchor-id)
  "Navigate to ANCHOR-ID in the buffer."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward anchor-id nil t)
        (setq found (point))))
    (if found
        (progn
          (goto-char found)
          (recenter))
      (message "Anchor not found in buffer: %s" anchor-id))))

(defun fuclient-logs-show-backlinks ()
  "Show backlinks for the anchor at point."
  (interactive)
  (if-let ((anchor-id (get-text-property (point) 'fuclient-logs-anchor)))
      (let ((url (format "%s/arxana/links/%s" fuclient-logs-server-url anchor-id)))
        (message "Fetching backlinks for %s..." anchor-id)
        ;; Simple HTTP fetch - could use url-retrieve for async
        (condition-case err
            (with-temp-buffer
              (call-process "curl" nil t nil "-sS" url)
              (goto-char (point-min))
              (let* ((json (json-parse-buffer :object-type 'plist))
                     (links (plist-get json :links))
                     (incoming (plist-get links :incoming))
                     (outgoing (plist-get links :outgoing)))
                (message "Backlinks: %d incoming, %d outgoing"
                         (length incoming) (length outgoing))))
          (error (message "Failed to fetch backlinks: %s" (error-message-string err)))))
    (user-error "No anchor at point")))

(defun fuclient-logs-suggest-links ()
  "Suggest links for the anchor at point based on semantic similarity."
  (interactive)
  (if-let ((anchor-id (get-text-property (point) 'fuclient-logs-anchor)))
      (let* ((session-id fuclient-logs--session-id)
             (url (format "%s/arxana/suggest-links" fuclient-logs-server-url))
             (payload (json-encode `(:session-id ,session-id :anchor-id ,anchor-id :limit 5))))
        (message "Finding similar anchors for %s..." anchor-id)
        (condition-case err
            (with-temp-buffer
              (call-process "curl" nil t nil "-sS" "-X" "POST" url
                            "-H" "Content-Type: application/json"
                            "-d" payload)
              (goto-char (point-min))
              (let* ((json (json-parse-buffer :object-type 'plist))
                     (suggestions (plist-get json :suggestions)))
                (if suggestions
                    (let ((buf (get-buffer-create "*FuLogs Link Suggestions*")))
                      (with-current-buffer buf
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert (format "Link suggestions for: %s\n\n" anchor-id))
                          (dolist (suggestion suggestions)
                            (let ((to (plist-get suggestion :to))
                                  (type (plist-get suggestion :suggested-type))
                                  (sim (plist-get suggestion :similarity)))
                              (insert (format "  → %s\n    Type: %s, Similarity: %.2f\n\n"
                                              to type sim))))
                          (goto-char (point-min)))
                        (special-mode))
                      (display-buffer buf))
                  (message "No similar anchors found"))))
          (error (message "Failed to suggest links: %s" (error-message-string err)))))
    (user-error "No anchor at point")))

(defun fuclient-logs-auto-link ()
  "Automatically create links between similar anchors in the current session."
  (interactive)
  (unless fuclient-logs--session-id
    (user-error "Not connected to a session"))
  (let* ((url (format "%s/arxana/auto-link" fuclient-logs-server-url))
         (payload (json-encode `(:session-id ,fuclient-logs--session-id :threshold 0.25))))
    (message "Auto-linking anchors in %s..." fuclient-logs--session-id)
    (condition-case err
        (with-temp-buffer
          (call-process "curl" nil t nil "-sS" "-X" "POST" url
                        "-H" "Content-Type: application/json"
                        "-d" payload)
          (goto-char (point-min))
          (let* ((json (json-parse-buffer :object-type 'plist))
                 (created (plist-get json :links-created)))
            (message "Auto-linked: %d links created" (or created 0))))
      (error (message "Failed to auto-link: %s" (error-message-string err))))))

;;; Event Formatting

(defun fuclient-logs--format-event (event)
  "Format EVENT for display in the log buffer."
  (let* ((event-type (or (plist-get event :event/type) "unknown"))
         (at (plist-get event :at))
         (payload (plist-get event :payload))
         (role (plist-get payload :role))
         (tool (plist-get payload :tool))
         (paths (fuclient-logs--extract-paths payload))
         (timestamp (fuclient-logs--format-timestamp at))
         (lines nil))

    ;; Build the main event line
    (push (concat
           (propertize timestamp 'face 'fuclient-logs-timestamp-face)
           " "
           (propertize (format "[%s]" event-type) 'face 'fuclient-logs-event-type-face)
           (when role
             (concat " "
                     (propertize (format "<%s>" role)
                                 'face (if (string= role "agent")
                                           'fuclient-logs-turn-agent-face
                                         'fuclient-logs-turn-user-face))))
           (when tool
             (concat " "
                     (propertize (format "{%s}" tool) 'face 'fuclient-logs-tool-face))))
          lines)

    ;; Add file paths if present
    (dolist (path-info paths)
      (when-let ((path (plist-get path-info :path)))
        (push (concat "  "
                      (fuclient-logs--make-file-link
                       path
                       (plist-get path-info :line)))
              lines)))

    ;; Add content preview for certain event types
    (when fuclient-logs-show-payload
      (cond
       ;; Agent/user turn content
       ((and (member event-type '("turn/agent" "turn/user"))
             (plist-get payload :content))
        (let* ((content (plist-get payload :content))
               (preview (if (> (length content) 100)
                            (concat (substring content 0 97) "...")
                          content)))
          (push (concat "  " (propertize preview 'face 'font-lock-doc-face)) lines)))

       ;; Plan events
       ((string-prefix-p "turn/plan" event-type)
        (when-let ((diagram (plist-get payload :diagram)))
          (push (propertize "  [Plan diagram available]" 'face 'fuclient-logs-plan-face) lines)))

       ;; PAR events (session punctuation)
       ((string= event-type "session/par")
        (let* ((seq (or (plist-get event :par/sequence) "?"))
               (tags (plist-get event :par/tags))
               (tags-str (when tags
                           (mapconcat (lambda (tag)
                                        (if (keywordp tag)
                                            (substring (symbol-name tag) 0)
                                          (format "%s" tag)))
                                      tags
                                      ", ")))
               (span (plist-get event :par/span))
               (span-from (plist-get span :from-eid))
               (span-to (plist-get span :to-eid))
               (questions (plist-get event :par/questions)))
          (push (propertize (format "  [PAR #%s]" seq) 'face 'fuclient-logs-par-face) lines)
          (when tags-str
            (push (format "  Tags: %s" tags-str) lines))
          (when (or span-from span-to)
            (push (format "  Span: %s → %s" (or span-from "?") (or span-to "?")) lines))
          (dolist (pair '((:intention . "Intention")
                          (:happening . "Happening")
                          (:perspectives . "Perspectives")
                          (:learned . "Learned")
                          (:forward . "Forward")))
            (let ((value (plist-get questions (car pair))))
              (when (and (stringp value) (not (string-empty-p value)))
                (push (format "  %s: %s" (cdr pair) value) lines))))))

       ;; Tool results
       ((and tool (plist-get payload :result))
        (let* ((result (format "%s" (plist-get payload :result)))
               (preview (if (> (length result) 80)
                            (concat (substring result 0 77) "...")
                          result)))
          (push (concat "  -> " preview) lines)))

       ;; Errors
       ((plist-get payload :error)
        (push (propertize (format "  ERROR: %s" (plist-get payload :error))
                          'face 'fuclient-logs-error-face)
              lines))

       ;; Native planning hint
       ((string= event-type "planning/native-detected")
        (let ((hint (plist-get payload :hint))
              (tool-name (plist-get payload :tool)))
          (push (propertize (format "  [HINT] Native planning detected (%s)" (or tool-name "task tool"))
                            'face 'fuclient-logs-hint-face)
                lines)
          (when hint
            (push (propertize (format "  %s" hint) 'face 'fuclient-logs-hint-face) lines))))

       ;; Arxana anchor created
       ((string= event-type "arxana/anchor-created")
        (let ((anchor-id (plist-get payload :anchor/id))
              (anchor-type (plist-get payload :anchor/type))
              (turn (plist-get payload :anchor/turn)))
          (push (concat "  "
                        (propertize "[ANCHOR] " 'face 'fuclient-logs-anchor-face)
                        (fuclient-logs--make-anchor-link anchor-id)
                        (propertize (format " (%s, turn %s)" (or anchor-type "insight") (or turn "?"))
                                    'face 'font-lock-comment-face))
                lines)))

       ;; Arxana link created
       ((string= event-type "arxana/link-created")
        (let ((link-id (plist-get payload :link/id))
              (from (plist-get payload :link/from))
              (to (plist-get payload :link/to))
              (link-type (plist-get payload :link/type)))
          (push (concat "  "
                        (propertize "[LINK] " 'face 'fuclient-logs-link-face)
                        (fuclient-logs--make-anchor-link from)
                        (propertize (format " --%s--> " (or link-type "ref")) 'face 'font-lock-comment-face)
                        (fuclient-logs--make-anchor-link to))
                lines)))))

    ;; Join lines in reverse order (we pushed them)
    (string-join (nreverse lines) "\n")))

(defun fuclient-logs--format-init (data)
  "Format init message DATA."
  (let* ((session-id (plist-get data :session_id))
         (event-count (plist-get data :event_count))
         (events (plist-get data :events)))
    (concat
     (propertize (format "=== Connected to session: %s ===" session-id)
                 'face 'font-lock-keyword-face)
     "\n"
     (propertize (format "=== Loaded %d historical events ===" (or event-count 0))
                 'face 'font-lock-comment-face)
     "\n\n"
     (when events
       (mapconcat #'fuclient-logs--format-event
                  (append events nil)  ;; Convert vector to list
                  "\n"))
     (when events "\n"))))

;;; Buffer Management

(defun fuclient-logs--get-buffer ()
  "Get or create the FuLogs buffer."
  (let ((buf (get-buffer-create fuclient-logs-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'fuclient-logs-mode)
        (fuclient-logs-mode)))
    buf))

(defun fuclient-logs--append (text)
  "Append TEXT to the log buffer."
  (when-let ((buf (get-buffer fuclient-logs-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (goto-char (point-max))
        (insert text "\n")
        (cl-incf fuclient-logs--event-count)

        ;; Trim buffer if too large
        (when (> (line-number-at-pos (point-max)) fuclient-logs-max-lines)
          (let ((excess (- (line-number-at-pos (point-max)) fuclient-logs-max-lines)))
            (goto-char (point-min))
            (forward-line excess)
            (delete-region (point-min) (point))))

        ;; Move point to end if we were at end
        (when at-end
          (goto-char (point-max)))))))

(defun fuclient-logs--should-display-event (event)
  "Return non-nil if EVENT should be displayed given current filters."
  (let ((event-type (plist-get event :event/type)))
    (cond
     ;; Filter by run ID
     (fuclient-logs--narrow-run
      (let ((payload (plist-get event :payload)))
        (equal (plist-get payload :run-id) fuclient-logs--narrow-run)))

     ;; Filter by level (future: implement event type hierarchy)
     (fuclient-logs--filter-level
      t)

     ;; Show all
     (t t))))

;;; WebSocket Handlers

(defun fuclient-logs--on-message (_ws frame)
  "Handle incoming WebSocket FRAME."
  (condition-case err
      (let* ((text (websocket-frame-text frame))
             (data (json-parse-string text :object-type 'plist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil))
             (msg-type (plist-get data :type)))
        (cond
         ((string= msg-type "init")
          (fuclient-logs--append (fuclient-logs--format-init data)))

         ((string= msg-type "event")
          (when (fuclient-logs--should-display-event data)
            (fuclient-logs--append (fuclient-logs--format-event data))))

         ((string= msg-type "pong")
          nil)  ;; Silently ignore pongs

         (t
          (fuclient-logs--append
           (propertize (format "[Unknown message type: %s]" msg-type)
                       'face 'font-lock-warning-face)))))
    (error
     (fuclient-logs--append
      (propertize (format "[Parse error: %s]" (error-message-string err))
                  'face 'fuclient-logs-error-face)))))

(defun fuclient-logs--on-open (_ws)
  "Handle WebSocket connection open."
  (message "FuLogs: Connected to %s" fuclient-logs--session-id)
  (fuclient-logs--append
   (propertize (format "--- WebSocket connected at %s ---"
                       (format-time-string "%Y-%m-%d %H:%M:%S"))
               'face 'font-lock-keyword-face)))

(defun fuclient-logs--on-close (_ws)
  "Handle WebSocket connection close."
  (message "FuLogs: Disconnected")
  (when (get-buffer fuclient-logs-buffer-name)
    (fuclient-logs--append
     (propertize (format "--- WebSocket disconnected at %s ---"
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 'face 'font-lock-warning-face))))

(defun fuclient-logs--on-error (_ws type err)
  "Handle WebSocket error of TYPE with ERR details."
  (fuclient-logs--append
   (propertize (format "[WebSocket error: %s - %s]" type err)
               'face 'fuclient-logs-error-face)))

;;; Commands

;;;###autoload
(defun fuclient-logs-connect (session-id)
  "Connect to MUSN session SESSION-ID via WebSocket."
  (interactive "sSession ID: ")
  (unless (featurep 'websocket)
    (require 'websocket nil t)
    (unless (featurep 'websocket)
      (user-error "websocket.el is required. Install it via MELPA")))

  ;; Disconnect existing connection
  (when fuclient-logs--websocket
    (ignore-errors (websocket-close fuclient-logs--websocket))
    (setq fuclient-logs--websocket nil))

  (setq fuclient-logs--session-id session-id)

  ;; Prepare buffer
  (let ((buf (fuclient-logs--get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n=== Connecting to session: %s ===\n" session-id)
                            'face 'font-lock-keyword-face))))
    (display-buffer buf))

  ;; Connect
  (let ((url (format "%s/fulab/session/%s/ws"
                     fuclient-logs-server-url
                     session-id)))
    (setq fuclient-logs--websocket
          (websocket-open url
                          :on-message #'fuclient-logs--on-message
                          :on-open #'fuclient-logs--on-open
                          :on-close #'fuclient-logs--on-close
                          :on-error #'fuclient-logs--on-error))))

;;;###autoload
(defun fuclient-logs-disconnect ()
  "Disconnect from the current WebSocket session."
  (interactive)
  (when fuclient-logs--websocket
    (websocket-close fuclient-logs--websocket)
    (setq fuclient-logs--websocket nil)
    (setq fuclient-logs--session-id nil))
  (message "FuLogs: Disconnected"))

;;;###autoload
(defun fuclient-logs-reconnect ()
  "Reconnect to the last session."
  (interactive)
  (if fuclient-logs--session-id
      (fuclient-logs-connect fuclient-logs--session-id)
    (call-interactively #'fuclient-logs-connect)))

;;;###autoload
(defun fuclient-logs-clear ()
  "Clear the log buffer."
  (interactive)
  (when-let ((buf (get-buffer fuclient-logs-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq fuclient-logs--event-count 0))
      (message "FuLogs: Buffer cleared"))))

;;;###autoload
(defun fuclient-logs-filter-level (level)
  "Filter events to show only LEVEL and above."
  (interactive
   (list (completing-read "Filter level (blank = all): "
                          '("turn" "tool" "plan" "error" "")
                          nil nil nil nil "")))
  (setq fuclient-logs--filter-level (if (string-empty-p level) nil level))
  (message "FuLogs: Filter set to %s" (or fuclient-logs--filter-level "all")))

;;;###autoload
(defun fuclient-logs-narrow-run (run-id)
  "Narrow display to events from RUN-ID."
  (interactive "sRun ID (blank = all): ")
  (setq fuclient-logs--narrow-run (if (string-empty-p run-id) nil run-id))
  (message "FuLogs: %s"
           (if fuclient-logs--narrow-run
               (format "Narrowed to run %s" run-id)
             "Showing all runs")))

(defun fuclient-logs-open-path-at-point ()
  "Open the file path at point."
  (interactive)
  (if-let ((path (get-text-property (point) 'fuclient-logs-path)))
      (fuclient-logs--open-file path (get-text-property (point) 'fuclient-logs-line))
    (user-error "No file path at point")))

(defun fuclient-logs-send-ping ()
  "Send a ping to the server."
  (interactive)
  (when fuclient-logs--websocket
    (websocket-send-text fuclient-logs--websocket
                         (json-encode '(:type "ping")))
    (message "FuLogs: Ping sent")))

;;; Mode Definition

(defvar fuclient-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'fuclient-logs-reconnect)
    (define-key map "c" #'fuclient-logs-clear)
    (define-key map "q" #'fuclient-logs-disconnect)
    (define-key map "f" #'fuclient-logs-filter-level)
    (define-key map "n" #'fuclient-logs-narrow-run)
    (define-key map (kbd "RET") #'fuclient-logs-open-path-at-point)
    (define-key map "p" #'fuclient-logs-send-ping)
    (define-key map "b" #'fuclient-logs-show-backlinks)
    (define-key map "s" #'fuclient-logs-suggest-links)
    (define-key map "a" #'fuclient-logs-auto-link)
    map)
  "Keymap for `fuclient-logs-mode'.")

(define-derived-mode fuclient-logs-mode special-mode "FuLogs"
  "Major mode for viewing MUSN session logs via WebSocket.

\\{fuclient-logs-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local fuclient-logs--event-count 0))

(provide 'fuclient-logs)
;;; fuclient-logs.el ends here
