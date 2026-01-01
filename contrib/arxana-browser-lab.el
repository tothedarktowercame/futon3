;;; arxana-browser-lab.el --- Lab browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Lab browsing helpers for the Arxana browser.

;;; Code:

(require 'cl-lib)

(require 'arxana-lab)

(defvar arxana-browser--stack)
(defvar arxana-browser--context)
(defvar arxana-browser--buffer)

(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--render "arxana-browser-core")
(declare-function arxana-browser-patterns--ensure-frame "arxana-browser-patterns")

(defun arxana-browser--lab-format ()
  [("Session" 36 t)
   ("Start" 20 t)
   ("Files" 7 t)
   ("Focus" 0 nil)])

(defun arxana-browser--lab-file-format ()
  [("File" 32 t)
   ("When" 17 t)
   ("Kind" 8 t)
   ("Path" 0 nil)])

(defun arxana-browser--lab-row (item)
  (let* ((session (or (plist-get item :session-id) ""))
         (start (or (plist-get item :timestamp-start) ""))
         (files (or (plist-get item :files) '()))
         (focus (or (car files) "")))
    (vector session
            start
            (format "%d" (length files))
            focus)))

(defun arxana-browser--lab-file-row (item)
  (let* ((label (or (plist-get item :label) ""))
         (modified (or (plist-get item :modified) ""))
         (kind (or (plist-get item :kind) ""))
         (path (or (plist-get item :path) "")))
    (vector label modified kind path)))

(defun arxana-browser--lab-items ()
  (let ((entries (or (arxana-lab-entries) '())))
    (if (and entries (listp entries))
        entries
      (list (list :type 'info
                  :label "No lab entries detected"
                  :description "Run dev/lab-export.clj to populate lab/raw.")))))

(defun arxana-browser-lab-browse-files (kind)
  "Open the lab files browser for KIND (raw, stubs, drafts)."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let* ((kind (if (symbolp kind) kind (intern kind)))
         (label (pcase kind
                  ('raw "Raw")
                  ('stubs "Stubs")
                  ('drafts "Drafts")
                  (_ (capitalize (format "%s" kind))))))
    (with-current-buffer (get-buffer-create arxana-browser--buffer)
      (setq arxana-browser--stack (list (list :view 'lab-files
                                                       :kind kind
                                                       :label label)))
      (setq arxana-browser--context nil))
    (arxana-browser--render)))

(defun arxana-browser-lab-browse-files-other-frame (kind)
  "Open the lab files browser for KIND in the Arxana frame."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let ((frame (arxana-browser-patterns--ensure-frame)))
    (with-selected-frame frame
      (let ((display-buffer-overriding-action
             '((display-buffer-same-window))))
        (arxana-browser-lab-browse-files kind))
      (select-frame-set-input-focus frame))))

(defalias 'arxana-browser-lab-browse-files-other-window
  #'arxana-browser-lab-browse-files-other-frame)

(defun arxana-browser--lab-entry-at-point ()
  (let ((item (tabulated-list-get-id)))
    (when (and item (eq (plist-get item :type) 'lab-entry))
      item)))

(defun arxana-browser--lab-open-trace ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-trace-object entry)))

(defun arxana-browser--lab-open-raw ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-raw-object entry)))

(defun arxana-browser--lab-open-draft ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-draft-object entry)))

(provide 'arxana-browser-lab)
;;; arxana-browser-lab.el ends here
