;;; futon3-hud.el --- Futon3 HUD state helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to collect Futon3 HUD state and
;; prototype/selection helpers in one place.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function my-futon3-claude-log-pattern-used "futon3-claude" (pattern-id &optional reason))
(defvar my-futon3-claude-current-session)

(declare-function my-chatgpt-shell--current-futons "aob-chatgpt" ())
(declare-function my-chatgpt-shell--dedupe-sigils "aob-chatgpt" (sigils))
(declare-function my-chatgpt-shell--prototype-keywords "aob-chatgpt" ())
(declare-function my-chatgpt-shell--refresh-context-all "aob-chatgpt" ())
(declare-function my-chatgpt-shell--reset-context-state "aob-chatgpt" ())
(declare-function my-futon3--suggest-intent-text "futon3-sessions" (prototypes))
(declare-function my-futon3--tatami-request "futon3-bridge" (method path payload))

(defvar my-futon3-devmap-directory (expand-file-name "holes" my-futon3--repo-root))

(defvar my-futon3--cached-prototype-display nil)
(defvar my-futon3--cached-prototype-display-mtime nil)
(defvar my-futon3--cached-prototype-sigils nil)
(defvar my-futon3--cached-prototype-sigils-mtime nil)
(defvar my-futon3--cached-prototype-metadata nil)
(defvar my-futon3--cached-prototype-metadata-mtime nil)

(defvar my-futon3-tatami-session-id nil)
(defvar my-futon3-tatami-default-prototypes '("f0/p0" "f3/p0"))
(defvar my-futon3-tatami-default-intent "chatgpt-shell block")
(defvar my-futon3-tatami-active-session nil
  "Metadata for the currently clocked Tatami target {:futons [] :prototypes [] :started ts :pattern slug?}.")
(defvar my-futon3-active-pattern nil
  "Current pattern slug being edited within the active Tatami session.")

(defun my-futon3--normalize-pattern (maybe-slug)
  (let ((trim (string-trim (or maybe-slug ""))))
    (unless (string-empty-p trim) trim)))

(defun my-futon3--keyword->prototype (kw)
  (cond
   ((keywordp kw)
    (substring (symbol-name kw) 1))
   ((symbolp kw) (symbol-name kw))
   ((stringp kw) kw)
   (t (format "%s" kw))))

(defun my-futon3--clock-context ()
  (let* ((session my-futon3-tatami-active-session)
         (proto-ids (or (plist-get session :prototypes)
                        (my-chatgpt-shell--prototype-keywords)))
         (protos (seq-filter (lambda (s) (and s (not (string-empty-p s))))
                             (mapcar #'my-futon3--keyword->prototype proto-ids)))
         (intent (or (plist-get session :intent)
                     my-futon3-tatami-default-intent)))
    (when (seq-some #'identity protos)
      (list :prototypes protos :intent intent))))

(defun my-futon3--update-session (key value)
  (setq my-futon3-tatami-active-session
        (plist-put (or my-futon3-tatami-active-session '()) key value)))

(defun my-futon3--encode-prototypes ()
  (mapcar (lambda (sym)
            (cond
             ((symbolp sym) (symbol-name sym))
             ((and (stringp sym)
                   (> (length sym) 0)
                   (char-equal (aref sym 0) ?:)) (substring sym 1))
             (t (format "%s" sym))))
          my-futon3-tatami-default-prototypes))

(defun my-futon3--read-devmap-prototypes ()
  (let* ((entries (cl-copy-list (my-futon3--prototype-metadata)))
         (sorted (sort entries (lambda (a b)
                                 (let ((fa (plist-get a :futon))
                                       (fb (plist-get b :futon))
                                       (pa (plist-get a :proto))
                                       (pb (plist-get b :proto)))
                                   (if (= fa fb)
                                       (< pa pb)
                                     (< fa fb)))))))
    (mapcar (lambda (entry)
              (cons (plist-get entry :display)
                    (plist-get entry :id)))
            sorted)))

(defun my-futon3--parse-prototype-string (text)
  (let ((parts (split-string (or text "") "[, ]" t)))
    (or (mapcar (lambda (tok)
                  (let* ((trim (string-trim tok))
                         (clean (if (and (> (length trim) 0)
                                         (char-equal (aref trim 0) ?:))
                                    (substring trim 1)
                                  trim)))
                    clean))
                parts)
        my-futon3-tatami-default-prototypes)))

(defun my-futon3--prototype-list (value)
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun my-futon3--prototype-string (proto)
  (cond
   ((null proto) nil)
   ((symbolp proto)
    (let ((name (symbol-name proto)))
      (if (and (> (length name) 0)
               (char-equal (aref name 0) ?:))
          (substring name 1)
        name)))
   ((stringp proto)
    (if (and (> (length proto) 0)
             (char-equal (aref proto 0) ?:))
        (substring proto 1)
      proto))
   (t (format "%s" proto))))

(defun my-futon3--prototype-sigil-map ()
  (let ((latest (my-futon3--prototype-devmap-mtime)))
    (when (or (null my-futon3--cached-prototype-sigils)
              (not (equal latest my-futon3--cached-prototype-sigils-mtime)))
      (let ((table (make-hash-table :test 'equal)))
        (dolist (entry (my-futon3--prototype-metadata))
          (when-let* ((sigils (plist-get entry :sigils)))
            (puthash (plist-get entry :id) sigils table)))
        (setq my-futon3--cached-prototype-sigils table
              my-futon3--cached-prototype-sigils-mtime latest)))
    my-futon3--cached-prototype-sigils))

(defun my-futon3--parse-sigil-token (token)
  (let ((trim (string-trim (or token ""))))
    (when (and (> (length trim) 0)
               (string-match "^\\([^/\\s]+\\)/\\([^/\\s]+\\)$" trim))
      (list :emoji (match-string 1 trim)
            :hanzi (match-string 2 trim)))))

(defun my-futon3--extract-sigils-from-label (label)
  (let ((start 0)
        acc)
    (while (and label (< start (length label))
                (string-match "\\[\\([^]]+\\)\\]" label start))
      (let* ((match-end-pos (match-end 0))
             (inside (match-string 1 label))
             (tokens (split-string inside "[[:space:]]+" t)))
        (dolist (tok tokens)
          (when-let* ((sigil (my-futon3--parse-sigil-token tok)))
            (push sigil acc)))
        (setq start match-end-pos)))
    (nreverse acc)))

(defun my-futon3--collect-prototype-metadata ()
  (let (entries)
    (when (file-directory-p my-futon3-devmap-directory)
      (dolist (name (directory-files my-futon3-devmap-directory nil "^futon[0-9]+\\.devmap$"))
        (let* ((file (expand-file-name name my-futon3-devmap-directory))
               (futon-num (and (string-match "futon\\([0-9]+\\)" name)
                               (string-to-number (match-string 1 name)))))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((title (and (re-search-forward "^@title \\(.*\\)$" nil t)
                               (match-string 1)))
                  (start (point)))
              (when title (goto-char start))
              (while (re-search-forward "^! instantiated-by: Prototype \\([0-9]+\\) — \\(.*\\)$" nil t)
                (let* ((proto-num (string-to-number (match-string 1)))
                       (label (match-string 2))
                       (proto-id (format "f%s/p%s" futon-num proto-num))
                       (display (format "FUTON%s — Prototype %s — %s"
                                        futon-num proto-num
                                        (or label (or title (format "Futon %d" futon-num)))))
                       (sigils (my-futon3--extract-sigils-from-label label)))
                  (push (list :id proto-id
                              :display display
                              :sigils sigils
                              :futon futon-num
                              :proto proto-num)
                        entries)))))))
    entries)))

(defun my-futon3--prototype-metadata ()
  (let ((latest (my-futon3--prototype-devmap-mtime)))
    (when (or (null my-futon3--cached-prototype-metadata)
              (not (equal latest my-futon3--cached-prototype-metadata-mtime)))
      (setq my-futon3--cached-prototype-metadata (my-futon3--collect-prototype-metadata))
      (setq my-futon3--cached-prototype-metadata-mtime latest))
    my-futon3--cached-prototype-metadata))

(defun my-futon3--prototype-devmap-mtime ()
  (let ((dir my-futon3-devmap-directory)
        (latest 0))
    (when (file-directory-p dir)
      (dolist (name (directory-files dir nil "^futon[0-9]+\\.devmap$"))
        (let* ((file (expand-file-name name dir))
               (attrs (ignore-errors (file-attributes file))))
          (when attrs
            (let ((mtime (float-time (file-attribute-modification-time attrs))))
              (when (> mtime latest)
                (setq latest mtime)))))))
    latest))

(defun my-futon3--prototype-display-map ()
  (let ((latest (my-futon3--prototype-devmap-mtime)))
    (when (or (null my-futon3--cached-prototype-display)
              (not (equal latest my-futon3--cached-prototype-display-mtime)))
      (let ((table (make-hash-table :test 'equal)))
        (dolist (entry (my-futon3--prototype-metadata))
          (puthash (plist-get entry :id)
                   (plist-get entry :display)
                   table))
        (setq my-futon3--cached-prototype-display table
              my-futon3--cached-prototype-display-mtime latest)))
    my-futon3--cached-prototype-display))

(defun my-futon3--prototype-sigils (value)
  (let* ((map (my-futon3--prototype-sigil-map))
         (ids (delq nil (mapcar #'my-futon3--prototype-string
                                (my-futon3--prototype-list value))))
         acc)
    (dolist (id ids)
      (when-let* ((sigils (and map (gethash id map))))
        (setq acc (nconc acc (cl-copy-list sigils)))))
    (when acc
      (my-chatgpt-shell--dedupe-sigils acc))))

(defun my-futon3--prototype-detail-lines (value)
  (let* ((map (my-futon3--prototype-display-map))
         (raw (seq-uniq (my-futon3--prototype-list value) #'equal)))
    (delq nil
          (mapcar (lambda (proto)
                    (let* ((id (and proto (my-futon3--prototype-string proto)))
                           (display (and map id (gethash id map)))
                           (detail (cond
                                     ((and display id)
                                      (format "%s (%s)" display id))
                                     ((and id (not (string-match-p "/" id)) map)
                                      (let ((matches nil)
                                            (needle (concat "/" id)))
                                        (maphash (lambda (key value)
                                                   (when (and (stringp key)
                                                              (string-suffix-p needle key))
                                                     (push value matches)))
                                                 map)
                                        (when matches
                                          (format "Prototype %s → %s"
                                                  id
                                                  (string-join (nreverse matches) ", ")))))
                                     (id id)
                                     (t nil))))
                      (when (and detail
                                 (not (string-empty-p (string-trim detail))))
                        detail)))
                  raw))))

(defun my-futon3--display-prototypes (value)
  (let* ((raw (my-futon3--prototype-list value))
         (strings (delq nil (mapcar #'my-futon3--prototype-string raw))))
    (cond
     ((and strings
           (seq-some (lambda (s) (string-match-p "/" s)) strings)) strings)
     (my-futon3-tatami-default-prototypes my-futon3-tatami-default-prototypes)
     (t strings))))

(defun my-futon3-sync-selection ()
  (condition-case err
      (my-futon3--tatami-request
       "POST" "/musn/tatami/select"
       `(("prototypes" . ,(my-futon3--encode-prototypes))
         ("intent" . ,my-futon3-tatami-default-intent)))
    (error
     (message "Futon3 select failed: %s" (error-message-string err))
     nil)))

(defun my-futon3-ensure-tatami-session ()
  (unless my-futon3-tatami-session-id
    (let ((resp (my-futon3--tatami-request
                 "POST" "/musn/tatami/start"
                 `(("prototypes" . ,(my-futon3--encode-prototypes))
                   ("intent" . ,my-futon3-tatami-default-intent)))))
      (setq my-futon3-tatami-session-id (plist-get resp :session-id)))))

(defun my-futon3-reset-tatami-session ()
  "Drop the cached Tatami session and reapply the current selection."
  (setq my-futon3-tatami-session-id nil)
  (my-futon3-sync-selection))

(defun my-futon3--truncate (text max-len)
  (let ((trimmed (string-trim (or text ""))))
    (if (<= (length trimmed) max-len)
        trimmed
      (concat (substring trimmed 0 max-len) "…"))))

(defun my-futon3-set-tatami-target (prototypes intent &optional pattern)
  "Interactive helper to change Futon3 tatami target defaults."
  (interactive
   (let* ((candidates (or (my-futon3--read-devmap-prototypes)
                          '(("FUTON0 — Prototype 0" . :f0/p0))))
          (names (mapcar #'car candidates))
          (table (lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))
                     (complete-with-action action names string pred))))
          (display (let ((completion-ignore-case t))
                     (completing-read "Tatami target: " table nil t nil nil nil)))
          (proto (cdr (assoc display candidates)))
          (proto-token (cond
                        ((null proto) display)
                        ((symbolp proto) (symbol-name proto))
                        ((stringp proto) proto)
                        (t (format "%s" proto))))
          (intent-default (or (my-futon3--suggest-intent-text proto-token)
                              my-futon3-tatami-default-intent))
          (intent-input (read-string "Tatami intent: " intent-default))
          (pattern-input (read-string "Pattern slug (optional): "
                                      (or my-futon3-active-pattern ""))))
     (list proto-token intent-input pattern-input)))
  (let* ((parsed (my-futon3--parse-prototype-string prototypes))
         (clean-intent (string-trim (or intent ""))))
    (when (string-empty-p clean-intent)
      (setq clean-intent (or (my-futon3--suggest-intent-text parsed)
                             my-futon3-tatami-default-intent)))
    (when (stringp pattern)
      (setq my-futon3-active-pattern (my-futon3--normalize-pattern pattern)))
    (setq my-futon3-tatami-default-prototypes parsed
          my-futon3-tatami-default-intent clean-intent)
    (setq my-futon3-tatami-active-session (list :futons (my-chatgpt-shell--current-futons)
                                               :prototypes (my-chatgpt-shell--prototype-keywords)
                                               :intent clean-intent
                                               :started (float-time)))
    (if my-futon3-active-pattern
        (my-futon3--update-session :pattern my-futon3-active-pattern)
      (setq my-futon3-tatami-active-session (plist-put my-futon3-tatami-active-session :pattern nil)))
    ;; Log pattern-dep if Claude session is active
    (when (and (boundp 'my-futon3-claude-current-session)
               my-futon3-claude-current-session)
      (dolist (proto parsed)
        (my-futon3-claude-log-pattern-used proto "tatami-target"))))
  (my-chatgpt-shell--reset-context-state)
  (my-futon3-sync-selection)
  (my-chatgpt-shell--refresh-context-all)
  (message "Set Futon3 tatami target to %s (intent %s)"
           my-futon3-tatami-default-prototypes my-futon3-tatami-default-intent))

(defun my-futon3-set-active-pattern (pattern)
  "Set the active pattern slug used for Tatami clocking."
  (interactive
   (list (read-string "Active pattern slug (blank to clear): "
                      (or my-futon3-active-pattern ""))))
  (let ((normalized (my-futon3--normalize-pattern pattern)))
    (setq my-futon3-active-pattern normalized)
    (if normalized
        (progn
          (my-futon3--update-session :pattern normalized)
          ;; Log pattern-dep if Claude session is active
          (when (and (boundp 'my-futon3-claude-current-session)
                     my-futon3-claude-current-session)
            (my-futon3-claude-log-pattern-used normalized "pattern-select")))
      (when my-futon3-tatami-active-session
        (setq my-futon3-tatami-active-session
              (plist-put my-futon3-tatami-active-session :pattern nil))))
    (my-chatgpt-shell--refresh-context-all)
    (if normalized
        (message "Clocked pattern set to %s" normalized)
      (message "Cleared active pattern"))))

(provide 'futon3-hud)

;;; futon3-hud.el ends here
