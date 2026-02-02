;;; arxana-hop.el --- Hop between linked Arxana anchors -*- lexical-binding: t; -*-

;;; Code:

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
  (local-set-key (kbd "C-c a c") #'my-arxana-clear-link-highlights))

;;; Pattern Backlinks

(defvar arxana-hop-server-url "http://localhost:5050"
  "URL for Arxana transport server.")

(defun arxana-hop-pattern-backlinks (pattern-id)
  "Show sessions that used PATTERN-ID via PSR/PUR links."
  (interactive "sPattern ID: ")
  (let ((url (format "%s/arxana/pattern-backlinks/%s" arxana-hop-server-url pattern-id)))
    (with-temp-buffer
      (call-process "curl" nil t nil "-sS" url)
      (goto-char (point-min))
      (let* ((json (ignore-errors (json-parse-buffer :object-type 'plist)))
             (data (plist-get json :data))
             (links (plist-get data :links))
             (count (plist-get data :count)))
        (if (and links (> (length links) 0))
            (let ((buf (get-buffer-create "*Pattern Backlinks*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (format "Pattern: %s\nBacklinks: %d\n\n" pattern-id count))
                  (dolist (link links)
                    (insert (format "Session: %s (turn %s)\n"
                                    (plist-get link :anchor/session)
                                    (plist-get link :anchor/turn)))
                    (insert (format "  Type: %s → %s\n"
                                    (plist-get link :anchor/type)
                                    (plist-get link :link/type)))
                    (insert (format "  %s\n\n"
                                    (or (plist-get link :anchor/content) ""))))
                  (special-mode))
                (pop-to-buffer buf)))
          (message "No backlinks found for %s" pattern-id))))))

(provide 'arxana-hop)
;;; arxana-hop.el ends here
