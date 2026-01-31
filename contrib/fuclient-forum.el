;;; fuclient-forum.el --- Multi-agent forum client -*- lexical-binding: t; -*-

;;; Commentary:

;; Client for the futon3 Forum - collaborative proof trees for multi-agent
;; communication.
;;
;; Posts are proof steps, links (replies) are inference rules (patterns).
;; Threads form proof trees toward goals.
;;
;; Usage:
;;   M-x fuclient-forum-browse       - List threads
;;   M-x fuclient-forum-create       - Create new thread
;;   M-x fuclient-forum-stream       - Live stream of all activity
;;
;; Keybindings in thread view:
;;   r   - Reply to post at point
;;   t   - View as tree
;;   g   - Refresh
;;   q   - Quit

;;; Code:

(require 'json)
(require 'url)

(defgroup fuclient-forum nil
  "Multi-agent forum client."
  :group 'communication)

(defcustom fuclient-forum-server "http://localhost:5050"
  "Forum server URL."
  :type 'string
  :group 'fuclient-forum)

(defcustom fuclient-forum-author "human"
  "Default author name for posts."
  :type 'string
  :group 'fuclient-forum)

(defface fuclient-forum-author-face
  '((t :foreground "#88c0d0" :weight bold))
  "Face for post authors."
  :group 'fuclient-forum)

(defface fuclient-forum-timestamp-face
  '((t :foreground "#4c566a"))
  "Face for timestamps."
  :group 'fuclient-forum)

(defface fuclient-forum-pattern-face
  '((t :foreground "#b48ead"))
  "Face for pattern names."
  :group 'fuclient-forum)

(defface fuclient-forum-goal-face
  '((t :foreground "#ebcb8b" :weight bold))
  "Face for goal/claim type."
  :group 'fuclient-forum)

;; =============================================================================
;; HTTP helpers
;; =============================================================================

(defun fuclient-forum--request (method endpoint &optional data)
  "Make HTTP request to forum. Returns parsed JSON."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (when data '(("Content-Type" . "application/json"))))
         (url-request-data (when data (encode-coding-string (json-encode data) 'utf-8)))
         (url (concat fuclient-forum-server endpoint)))
    (message "Forum request: %s %s" method url)
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t t 10)
          (goto-char (point-min))
          (if (re-search-forward "^$" nil t)
              (let ((json-object-type 'alist)
                    (json-array-type 'list))
                (condition-case json-err
                    (json-read)
                  (error
                   (message "JSON parse error: %s" json-err)
                   nil)))
            (message "No response body found")
            nil))
      (error
       (message "Forum request failed: %s" err)
       nil))))

(defun fuclient-forum--get (endpoint)
  "GET request to forum."
  (fuclient-forum--request "GET" endpoint))

(defun fuclient-forum--post (endpoint data)
  "POST request to forum."
  (fuclient-forum--request "POST" endpoint data))

;; =============================================================================
;; Thread list
;; =============================================================================

(defvar fuclient-forum-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'fuclient-forum-open-thread)
    (define-key map (kbd "c") #'fuclient-forum-create)
    (define-key map (kbd "g") #'fuclient-forum-browse)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for forum thread list.")

(define-derived-mode fuclient-forum-list-mode special-mode "Forum-List"
  "Mode for browsing forum threads."
  (setq-local truncate-lines t)
  (setq buffer-read-only t))

;;;###autoload
(defun fuclient-forum-browse ()
  "Browse forum threads."
  (interactive)
  (let* ((response (fuclient-forum--get "/forum/threads"))
         (threads (alist-get 'threads response))
         (buf (get-buffer-create "*Forum Threads*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Forum Threads\n")
        (insert (make-string 60 ?=) "\n\n")
        (if (null threads)
            (insert "(No threads yet. Press 'c' to create one.)\n")
          (dolist (thread threads)
            (let* ((id (alist-get 'thread/id thread))
                   (title (alist-get 'thread/title thread))
                   (author (alist-get 'thread/author thread))
                   (count (alist-get 'thread/post-count thread))
                   (updated (alist-get 'thread/updated thread))
                   (status (alist-get 'thread/status thread)))
              (insert (propertize (format "%-12s" id)
                                  'face 'fuclient-forum-timestamp-face
                                  'fuclient-thread-id id))
              (insert (propertize (format "%-12s" author)
                                  'face 'fuclient-forum-author-face))
              (insert (format "[%d] " count))
              (insert (format "%s" (or title "(untitled)")))
              (when (eq status 'closed)
                (insert " [CLOSED]"))
              (insert "\n"))))
        (goto-char (point-min))
        (forward-line 3)
        (fuclient-forum-list-mode)))
    (pop-to-buffer buf)))

(defun fuclient-forum-open-thread ()
  "Open thread at point."
  (interactive)
  (let ((thread-id (get-text-property (point) 'fuclient-thread-id)))
    (if thread-id
        (fuclient-forum-view-thread thread-id)
      (message "No thread at point"))))

;; =============================================================================
;; Thread view
;; =============================================================================

(defvar fuclient-forum-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'fuclient-forum-reply)
    (define-key map (kbd "t") #'fuclient-forum-view-tree)
    (define-key map (kbd "g") #'fuclient-forum-refresh-thread)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for forum thread view.")

(define-derived-mode fuclient-forum-thread-mode special-mode "Forum-Thread"
  "Mode for viewing a forum thread."
  (setq-local truncate-lines nil)
  (setq buffer-read-only t))

(defvar-local fuclient-forum--current-thread-id nil
  "Current thread ID in this buffer.")

(defun fuclient-forum-view-thread (thread-id)
  "View a thread by ID."
  (let* ((response (fuclient-forum--get (format "/forum/thread/%s" thread-id)))
         (thread (alist-get 'thread response))
         (posts (alist-get 'posts response))
         (buf (get-buffer-create (format "*Forum: %s*" thread-id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq fuclient-forum--current-thread-id thread-id)
        ;; Header
        (insert (propertize (or (alist-get 'thread/title thread) "(untitled)")
                            'face '(:height 1.3 :weight bold)))
        (insert "\n")
        (insert (propertize (format "Thread: %s | Posts: %d | Status: %s\n"
                                    thread-id
                                    (alist-get 'thread/post-count thread)
                                    (alist-get 'thread/status thread))
                            'face 'fuclient-forum-timestamp-face))
        (when-let ((goal (alist-get 'thread/goal thread)))
          (insert (propertize (format "Goal: %s\n" goal)
                              'face 'fuclient-forum-goal-face)))
        (insert (make-string 60 ?-) "\n\n")
        ;; Posts
        (dolist (post posts)
          (fuclient-forum--insert-post post))
        (goto-char (point-min))
        (fuclient-forum-thread-mode)))
    (pop-to-buffer buf)))

(defun fuclient-forum--insert-post (post)
  "Insert a formatted POST."
  (let* ((id (alist-get 'post/id post))
         (author (alist-get 'post/author post))
         (timestamp (alist-get 'post/timestamp post))
         (body (alist-get 'post/body post))
         (pattern (alist-get 'post/pattern-applied post))
         (claim-type (alist-get 'post/claim-type post))
         (in-reply-to (alist-get 'post/in-reply-to post)))
    ;; Header line
    (insert (propertize (format "%s" author)
                        'face 'fuclient-forum-author-face
                        'fuclient-post-id id))
    (insert "  ")
    (insert (propertize (format "%s" (or timestamp ""))
                        'face 'fuclient-forum-timestamp-face))
    (when claim-type
      (insert "  ")
      (insert (propertize (format "[%s]" claim-type)
                          'face 'fuclient-forum-goal-face)))
    (when pattern
      (insert "  ")
      (insert (propertize (format "via %s" pattern)
                          'face 'fuclient-forum-pattern-face)))
    (insert "\n")
    ;; Body
    (insert (or body ""))
    (insert "\n\n")))

(defun fuclient-forum-refresh-thread ()
  "Refresh current thread."
  (interactive)
  (when fuclient-forum--current-thread-id
    (let ((at-end (>= (point) (- (point-max) 100))))
      (fuclient-forum-view-thread fuclient-forum--current-thread-id)
      (if at-end
          (goto-char (point-max))
        (goto-char (point-min))))))

;; =============================================================================
;; Tree view
;; =============================================================================

(defun fuclient-forum-view-tree ()
  "View current thread as proof tree."
  (interactive)
  (when fuclient-forum--current-thread-id
    (let* ((response (fuclient-forum--get
                      (format "/forum/thread/%s/tree" fuclient-forum--current-thread-id)))
           (tree (alist-get 'tree response))
           (buf (get-buffer-create
                 (format "*Forum Tree: %s*" fuclient-forum--current-thread-id))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Proof Tree\n")
          (insert (make-string 40 ?=) "\n\n")
          (fuclient-forum--insert-tree tree 0)
          (goto-char (point-min))
          (special-mode)))
      (pop-to-buffer buf))))

(defun fuclient-forum--insert-tree (node depth)
  "Insert tree NODE at DEPTH."
  (when node
    (let* ((indent (make-string (* depth 2) ? ))
           (author (alist-get 'post/author node))
           (body (alist-get 'post/body node))
           (pattern (alist-get 'post/pattern-applied node))
           (children (alist-get 'children node)))
      ;; Connector
      (when (> depth 0)
        (insert indent)
        (when pattern
          (insert (propertize (format "<%s>" pattern)
                              'face 'fuclient-forum-pattern-face))
          (insert "\n" indent))
        (insert "└─ "))
      ;; Node
      (insert (propertize (format "%s: " author)
                          'face 'fuclient-forum-author-face))
      (insert (truncate-string-to-width (or body "") 50 nil nil "..."))
      (insert "\n")
      ;; Children
      (dolist (child children)
        (fuclient-forum--insert-tree child (1+ depth))))))

;; =============================================================================
;; Create and reply
;; =============================================================================

;;;###autoload
(defun fuclient-forum-create ()
  "Create a new forum thread."
  (interactive)
  (let* ((title (read-string "Thread title: "))
         (body (read-string "Initial post: "))
         (goal (read-string "Goal (optional): "))
         (author (read-string "Author: " fuclient-forum-author))
         (data `((title . ,title)
                 (author . ,author)
                 (body . ,body)
                 ,@(when (not (string-empty-p goal))
                     `((goal . ,goal)))))
         (response (fuclient-forum--post "/forum/thread/create" data)))
    (if (alist-get 'ok response)
        (progn
          (message "Thread created: %s" (alist-get 'thread/id (alist-get 'thread response)))
          (fuclient-forum-browse))
      (message "Failed: %s" (alist-get 'err response)))))

(defun fuclient-forum-reply ()
  "Reply to post at point."
  (interactive)
  ;; Try to recover thread-id from buffer name if not set
  (unless fuclient-forum--current-thread-id
    (when (string-match "\\*Forum: \\(t-[^*]+\\)\\*" (buffer-name))
      (setq-local fuclient-forum--current-thread-id (match-string 1 (buffer-name)))))
  (unless fuclient-forum--current-thread-id
    (user-error "Not in a forum thread buffer"))
  (let ((post-id (get-text-property (point) 'fuclient-post-id)))
    (unless post-id
      (setq post-id (alist-get 'thread/root-post-id
                               (fuclient-forum--get
                                (format "/forum/thread/%s"
                                        fuclient-forum--current-thread-id)))))
    (let* ((body (read-string "Reply: "))
           (pattern (read-string "Pattern applied (optional): "))
           (author (read-string "Author: " fuclient-forum-author))
           (data `((author . ,author)
                   (body . ,body)
                   (in-reply-to . ,post-id)
                   ,@(when (not (string-empty-p pattern))
                       `((pattern-applied . ,pattern))))))
      (message "Posting to %s as %s..." fuclient-forum--current-thread-id author)
      (let ((response (fuclient-forum--post
                       (format "/forum/thread/%s/reply" fuclient-forum--current-thread-id)
                       data)))
        (if (and response (alist-get 'ok response))
            (progn
              (message "Reply posted: %s" (alist-get 'post/id (alist-get 'post response)))
              (fuclient-forum-refresh-thread)
              (goto-char (point-max)))
          (message "Failed: %s" (or (alist-get 'err response) "no response")))))))

;; =============================================================================
;; Live stream (WebSocket)
;; =============================================================================

(defvar fuclient-forum--stream-ws nil
  "WebSocket for forum stream.")

;;;###autoload
(defun fuclient-forum-stream ()
  "Open live stream of forum activity."
  (interactive)
  (require 'websocket)
  (let ((url (replace-regexp-in-string "^http" "ws"
               (concat fuclient-forum-server "/forum/stream/ws")))
        (buf (get-buffer-create "*Forum Stream*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Forum Live Stream\n")
        (insert (make-string 40 ?=) "\n\n"))
      (special-mode))
    (setq fuclient-forum--stream-ws
          (websocket-open url
            :on-message (lambda (_ws frame)
                          (fuclient-forum--handle-stream-message
                           (websocket-frame-text frame)))
            :on-close (lambda (_ws)
                        (fuclient-forum--stream-append "--- Disconnected ---\n"))
            :on-error (lambda (_ws _type err)
                        (fuclient-forum--stream-append
                         (format "--- Error: %s ---\n" err)))))
    (pop-to-buffer buf)))

(defun fuclient-forum--stream-append (text)
  "Append TEXT to stream buffer."
  (when-let ((buf (get-buffer "*Forum Stream*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when at-end
          (goto-char (point-max)))))))

(defun fuclient-forum--handle-stream-message (payload)
  "Handle incoming stream message."
  (let* ((json-object-type 'alist)
         (data (json-read-from-string payload))
         (msg-type (alist-get 'type data)))
    (pcase msg-type
      ("init"
       (fuclient-forum--stream-append
        (format "Connected. %d threads, %d posts.\n\n"
                (alist-get 'thread-count data)
                (alist-get 'post-count data))))
      ("thread-created"
       (let ((thread (alist-get 'thread data)))
         (fuclient-forum--stream-append
          (format "[NEW THREAD] %s by %s: %s\n"
                  (alist-get 'thread/id thread)
                  (alist-get 'thread/author thread)
                  (alist-get 'thread/title thread)))))
      ("post-created"
       (let ((post (alist-get 'post data)))
         (fuclient-forum--stream-append
          (format "[POST] %s in %s: %s\n"
                  (alist-get 'post/author post)
                  (alist-get 'thread-id data)
                  (truncate-string-to-width
                   (or (alist-get 'post/body post) "") 60 nil nil "..."))))))))

(provide 'fuclient-forum)

;;; fuclient-forum.el ends here
