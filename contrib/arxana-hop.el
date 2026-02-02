;;; arxana-hop.el --- Hop between linked Arxana anchors -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides navigation between Arxana-linked regions and integration with
;; the futon4 pattern browser for showing session backlinks (PSR/PUR usage).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defface arxana-source-face
  '((t :background "#2d3a2d" :box (:line-width 1 :color "#7dba84")))
  "Face for anchors with outgoing links (sources).")

(defface arxana-sink-face
  '((t :background "#3a2d3a" :box (:line-width 1 :color "#b07db0")))
  "Face for anchors with incoming links (sinks).")

(defface arxana-both-face
  '((t :background "#3a382d" :box (:line-width 1 :color "#c9b867")))
  "Face for anchors with both incoming and outgoing links.")

(defface arxana-link-target-face
  '((t :background "#4a3a2a" :box (:line-width 2 :color "#ff9f43")))
  "Face for linked anchor targets when cursor is in an anchor.")

(defvar-local my-arxana-link-overlays nil
  "Overlays for link target highlights.")

(defun my-arxana-clear-link-highlights ()
  "Remove link target highlights."
  (interactive)
  (mapc #'delete-overlay my-arxana-link-overlays)
  (setq my-arxana-link-overlays nil))

(defun my-arxana-find-anchor-region (anchor-id)
  "Find buffer region for ANCHOR-ID, return (start . end) or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (when (equal (get-text-property (point) 'arxana-anchor) anchor-id)
          (setq found (cons (point)
                            (or (next-single-property-change (point) 'arxana-anchor)
                                (point-max)))))
        (goto-char (or (next-single-property-change (point) 'arxana-anchor)
                       (point-max))))
      found)))

(defun my-arxana-highlight-links ()
  "Highlight regions linked to anchor at point."
  (interactive)
  (my-arxana-clear-link-highlights)
  (when-let ((anchor-id (get-text-property (point) 'arxana-anchor)))
    (message "Finding links for %s..." anchor-id)
    (let ((url (format "http://localhost:5050/arxana/links/%s"
                       (url-hexify-string anchor-id))))
      (with-temp-buffer
        (call-process "curl" nil t nil "-sS" url)
        (goto-char (point-min))
        (condition-case err
            (let* ((json (json-parse-buffer :object-type 'plist))
                   (links (plist-get json :links))
                   (outgoing (plist-get links :outgoing))
                   (incoming (plist-get links :incoming))
                   (all-targets (append
                                 (mapcar (lambda (l) (plist-get l :link/to)) outgoing)
                                 (mapcar (lambda (l) (plist-get l :link/from)) incoming)))
                   (found-count 0))
              (dolist (target-id all-targets)
                (when-let ((region (with-current-buffer "*Claude Stream*"
                                     (my-arxana-find-anchor-region target-id))))
                  (with-current-buffer "*Claude Stream*"
                    (let ((ov (make-overlay (car region) (cdr region))))
                      (overlay-put ov 'face 'arxana-link-target-face)
                      (overlay-put ov 'arxana-link-target target-id)
                      (push ov my-arxana-link-overlays)
                      (cl-incf found-count)))))
              (message "Highlighted %d linked regions" found-count))
          (error (message "Error: %s" err)))))))

(defun my-arxana-hop-to-link ()
  "Hop to next linked anchor."
  (interactive)
  (if (null my-arxana-link-overlays)
      (progn
        (my-arxana-highlight-links)
        (when my-arxana-link-overlays
          (goto-char (overlay-start (car my-arxana-link-overlays)))))
    ;; Find next overlay after point
    (let* ((positions (sort (mapcar #'overlay-start my-arxana-link-overlays) #'<))
           (next (or (cl-find-if (lambda (p) (> p (point))) positions)
                     (car positions))))
      (when next
        (goto-char next)))))

(defun my-arxana-setup-hop-keys ()
  "Set up keybindings for arxana hop in current buffer."
  (local-set-key (kbd "C-c a h") #'my-arxana-highlight-links)
  (local-set-key (kbd "C-c a n") #'my-arxana-hop-to-link)
  (local-set-key (kbd "C-c a c") #'my-arxana-clear-link-highlights)
  (local-set-key (kbd "C-c a b") #'arxana-hop-pattern-backlinks)
  (local-set-key (kbd "C-c a s") #'arxana-hop-insert-session-backlinks))

;;; Pattern Backlinks

(defvar arxana-hop-server-url "http://localhost:5050"
  "URL for Arxana transport server.")

(defun arxana-hop--fetch-pattern-backlinks (pattern-id)
  "Fetch backlinks for PATTERN-ID from futon3 transport, return list of plists."
  (let ((url (format "%s/arxana/pattern-backlinks/%s" arxana-hop-server-url pattern-id)))
    (with-temp-buffer
      (call-process "curl" nil t nil "-sS" url)
      (goto-char (point-min))
      (let* ((json (ignore-errors (json-parse-buffer :object-type 'plist)))
             (data (plist-get json :data))
             (links (plist-get data :links)))
        (when (and links (> (length links) 0))
          (mapcar (lambda (link)
                    (list :session (plist-get link :anchor/session)
                          :turn (plist-get link :anchor/turn)
                          :type (plist-get link :anchor/type)
                          :link-type (plist-get link :link/type)
                          :content (plist-get link :anchor/content)
                          :note (plist-get link :link/note)))
                  links))))))

(defun arxana-hop-pattern-backlinks (pattern-id)
  "Show sessions that used PATTERN-ID via PSR/PUR links."
  (interactive "sPattern ID: ")
  (let ((links (arxana-hop--fetch-pattern-backlinks pattern-id)))
    (if links
        (let ((buf (get-buffer-create "*Pattern Backlinks*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Pattern: %s\nBacklinks: %d\n\n" pattern-id (length links)))
              (dolist (link links)
                (insert (format "Session: %s (turn %s)\n"
                                (plist-get link :session)
                                (plist-get link :turn)))
                (insert (format "  Type: %s → %s\n"
                                (plist-get link :type)
                                (plist-get link :link-type)))
                (insert (format "  %s\n\n"
                                (or (plist-get link :content) ""))))
              (special-mode))
            (pop-to-buffer buf)))
      (message "No backlinks found for %s" pattern-id))))

;;; Integration with futon4 pattern browser

(defun arxana-hop--insert-session-backlinks (backlinks indent)
  "Insert BACKLINKS as an ARXANA-SESSIONS block with INDENT."
  (when backlinks
    (insert (format "%s+ ARXANA-SESSIONS:\n" indent))
    (dolist (link backlinks)
      (let ((session (plist-get link :session))
            (turn (plist-get link :turn))
            (link-type (plist-get link :link-type))
            (content (plist-get link :content)))
        (insert (format "%s    - [%s] %s turn-%s"
                        indent
                        (or link-type "link")
                        (or session "?")
                        (or turn "?")))
        (when (and content (not (string-empty-p content)))
          (insert (format " — %s"
                          (truncate-string-to-width content 60 nil nil "…"))))
        (insert "\n")))
    (insert "\n")))

(defun arxana-hop--pattern-name-from-buffer ()
  "Extract pattern name from current buffer (futon4 pattern format)."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+PATTERN:\\s-*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun arxana-hop-insert-session-backlinks ()
  "Insert Arxana session backlinks for the current pattern buffer."
  (interactive)
  (let ((pattern-name (arxana-hop--pattern-name-from-buffer)))
    (unless pattern-name
      (user-error "Cannot determine pattern name from buffer"))
    (let ((backlinks (arxana-hop--fetch-pattern-backlinks pattern-name)))
      (if backlinks
          (save-excursion
            ;; Find insertion point (after ORG-TODOS or NEXT-STEPS)
            (goto-char (point-min))
            (let ((insert-pos nil)
                  (indent "  "))
              (cond
               ;; After ORG-TODOS block if present
               ((re-search-forward "^\\([ \t]*\\)\\+ ORG-TODOS:" nil t)
                (setq indent (match-string 1))
                (forward-line 1)
                (while (and (not (eobp))
                            (looking-at "^[ \t]*\\(- \\|$\\)"))
                  (forward-line 1))
                (setq insert-pos (point)))
               ;; After NEXT-STEPS block if present
               ((progn (goto-char (point-min))
                       (re-search-forward "^\\([ \t]*\\)\\+ NEXT-STEPS:" nil t))
                (setq indent (match-string 1))
                (forward-line 1)
                (while (and (not (eobp))
                            (looking-at "^[ \t]+"))
                  (forward-line 1))
                (setq insert-pos (point)))
               ;; At end of buffer
               (t (setq insert-pos (point-max))))
              (goto-char insert-pos)
              (let ((inhibit-read-only t))
                (arxana-hop--insert-session-backlinks backlinks indent))
              (message "Inserted %d session backlinks" (length backlinks))))
        (message "No session backlinks found for %s" pattern-name)))))

(defun arxana-hop--after-pattern-render (&rest _)
  "Hook to add session backlinks after futon4 pattern render."
  (when (and (derived-mode-p 'org-mode)
             (arxana-hop--pattern-name-from-buffer))
    (arxana-hop-insert-session-backlinks)))

;; Advice for futon4 integration (activate when arxana-browser-patterns loads)
(with-eval-after-load 'arxana-browser-patterns
  (advice-add 'arxana-browser-patterns--render-pattern
              :after #'arxana-hop--after-pattern-render))

(provide 'arxana-hop)
;;; arxana-hop.el ends here
