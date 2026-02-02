;;; fuclient-claude-stream-arxana.el --- Arxana linking for Claude Stream -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds Arxana anchor/link support to the Claude Stream buffer.
;;
;; Strategy: Since Arxana uses external link storage (not inline markup),
;; we mark linked regions via text properties and overlays.
;;
;; Based on patterns from fuclient-logs.el.

;;; Code:

(require 'cl-lib)

(defgroup fuclient-claude-stream-arxana nil
  "Arxana integration for Claude Stream."
  :group 'fuclient-claude-stream)

(defcustom fuclient-claude-stream-arxana-server "http://localhost:5050"
  "Arxana server URL (transport port)."
  :type 'string
  :group 'fuclient-claude-stream-arxana)

(defvar-local fuclient-claude-stream-arxana--session-id nil
  "Current Arxana session ID for this buffer.")

(defface fuclient-claude-stream-arxana-anchor-face
  '((t :background "#2e3440" :box (:line-width 1 :color "#5e81ac")))
  "Face for anchored regions."
  :group 'fuclient-claude-stream-arxana)

(defface fuclient-claude-stream-arxana-linked-face
  '((t :background "#3b4252" :underline (:color "#88c0d0" :style wave)))
  "Face for regions with outgoing links."
  :group 'fuclient-claude-stream-arxana)

;;; Anchor Creation

(defun fuclient-claude-stream-arxana-anchor-region (start end type content)
  "Create an anchor for region from START to END with TYPE and CONTENT.
TYPE is one of: insight, decision, question, artifact."
  (interactive
   (let ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (cons (line-beginning-position) (line-end-position)))))
     (list (car bounds) (cdr bounds)
           (completing-read "Anchor type: " '("insight" "decision" "question" "artifact"))
           (read-string "Content (or RET for selection): "
                        (buffer-substring-no-properties (car bounds) (cdr bounds))))))
  (unless fuclient-claude-stream-arxana--session-id
    (setq fuclient-claude-stream-arxana--session-id
          (format "claude-stream-%s" (format-time-string "%Y%m%d-%H%M%S"))))
  (let* ((turn (or (get-text-property start 'turn-number) 1))
         (url (format "%s/arxana/anchor/create" fuclient-claude-stream-arxana-server))
         (payload (json-encode
                   `(:session-id ,fuclient-claude-stream-arxana--session-id
                     :turn ,turn
                     :type ,type
                     :content ,content
                     :author "emacs-user"
                     :note ,(format "Created from Claude Stream at pos %d" start)))))
    (with-temp-buffer
      (call-process "curl" nil t nil "-sS" "-X" "POST" url
                    "-H" "Content-Type: application/json"
                    "-d" payload)
      (goto-char (point-min))
      (let* ((response (json-parse-buffer :object-type 'plist))
             (anchor (plist-get response :anchor))
             (anchor-id (plist-get anchor :anchor/id)))
        (when anchor-id
          ;; Mark the region with text properties
          (with-current-buffer (current-buffer)
            (let ((inhibit-read-only t))
              (add-text-properties
               start end
               `(arxana-anchor ,anchor-id
                 face fuclient-claude-stream-arxana-anchor-face
                 help-echo ,(format "Anchor: %s" anchor-id)))))
          (message "Created anchor: %s" anchor-id)
          anchor-id)))))

;;; Link Display

(defun fuclient-claude-stream-arxana--fetch-links (anchor-id)
  "Fetch links for ANCHOR-ID from Arxana server."
  (let ((url (format "%s/arxana/links/%s"
                     fuclient-claude-stream-arxana-server
                     (url-hexify-string anchor-id))))
    (with-temp-buffer
      (call-process "curl" nil t nil "-sS" url)
      (goto-char (point-min))
      (condition-case nil
          (json-parse-buffer :object-type 'plist)
        (error nil)))))

(defun fuclient-claude-stream-arxana-show-links ()
  "Show links for anchor at point."
  (interactive)
  (if-let ((anchor-id (get-text-property (point) 'arxana-anchor)))
      (let* ((response (fuclient-claude-stream-arxana--fetch-links anchor-id))
             (links (plist-get response :links))
             (incoming (plist-get links :incoming))
             (outgoing (plist-get links :outgoing)))
        (if (or incoming outgoing)
            (let ((buf (get-buffer-create "*Arxana Links*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (format "Links for: %s\n\n" anchor-id))
                  (when outgoing
                    (insert "Outgoing:\n")
                    (dolist (link outgoing)
                      (insert (format "  --%s--> %s\n"
                                      (plist-get link :link/type)
                                      (plist-get link :link/to)))))
                  (when incoming
                    (insert "\nIncoming:\n")
                    (dolist (link incoming)
                      (insert (format "  <--%s-- %s\n"
                                      (plist-get link :link/type)
                                      (plist-get link :link/from))))))
                (special-mode))
              (display-buffer buf))
          (message "No links for this anchor")))
    (user-error "No anchor at point")))

;;; Highlight All Anchored Regions

(defun fuclient-claude-stream-arxana-highlight-all ()
  "Re-apply highlighting to all anchored regions in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (< (point) (point-max))
        (let ((anchor-id (get-text-property (point) 'arxana-anchor))
              (next-change (or (next-single-property-change (point) 'arxana-anchor)
                               (point-max))))
          (when anchor-id
            (let ((inhibit-read-only t))
              (add-face-text-property (point) next-change
                                      'fuclient-claude-stream-arxana-anchor-face))
            (cl-incf count))
          (goto-char next-change)))
      (message "Highlighted %d anchored regions" count))))

;;; Keymap

(defvar fuclient-claude-stream-arxana-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a a") #'fuclient-claude-stream-arxana-anchor-region)
    (define-key map (kbd "C-c a l") #'fuclient-claude-stream-arxana-show-links)
    (define-key map (kbd "C-c a h") #'fuclient-claude-stream-arxana-highlight-all)
    map)
  "Keymap for Arxana commands in Claude Stream.")

(provide 'fuclient-claude-stream-arxana)
;;; fuclient-claude-stream-arxana.el ends here
