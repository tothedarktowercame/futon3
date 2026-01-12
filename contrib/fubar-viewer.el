;;; fubar-viewer.el --- MUSN viewer (pauses, hints, trail) -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only viewer for MUSN runs. Shows pauses/hints from the MUSN stream and
;; minimal trail/AIF snippets. Assumes a MUSN HTTP service is running and env
;; FUTON3_MUSN_URL points to it (default http://localhost:6065).

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

(defvar fubar-musn-url (or (getenv "FUTON3_MUSN_URL") "http://localhost:6065")
  "Base URL for MUSN service.")

(defvar fubar-musn-view-buffer "*FuBar MUSN Viewer*")
(defvar fubar-musn-session-id nil)
(defvar fubar-musn-turn nil)

(defun fubar-musn--json-request (method path payload)
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (and payload (encode-coding-string (json-encode payload) 'utf-8)))
         (buffer (url-retrieve-synchronously (concat (string-remove-suffix "/" fubar-musn-url) path) t t 5)))
    (unless buffer (error "No response from MUSN"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "\n\n" nil 'move)
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (when (not (string-empty-p (string-trim body)))
              (json-parse-string body :object-type 'plist :array-type 'list :null-object nil :false-object nil))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun fubar-musn--render-line (line)
  (when (and line (not (string-empty-p line)))
    (with-current-buffer (get-buffer-create fubar-musn-view-buffer)
      (goto-char (point-max))
      (insert line "\n"))))

(defun fubar-musn-display-pause (pause)
  (let* ((reason (plist-get pause :reason))
         (note (and reason (plist-get reason :note))))
    (fubar-musn--render-line (format "[MUSN-PAUSE] %s" (json-encode pause)))
    (fubar-musn--render-line (format "  reason: %s" note))
    (fubar-musn--render-line "  resume: M-x fubar-musn-resume")
    (display-buffer fubar-musn-view-buffer)))

(defun fubar-musn-view-toggle ()
  "Toggle the MUSN viewer buffer."
  (interactive)
  (if (get-buffer-window fubar-musn-view-buffer)
      (delete-window (get-buffer-window fubar-musn-view-buffer))
    (display-buffer (get-buffer-create fubar-musn-view-buffer))))

(defun fubar-musn-resume (note)
  "Resume current MUSN session with NOTE."
  (interactive "sResume note: ")
  (unless (and fubar-musn-session-id fubar-musn-turn)
    (user-error "No MUSN session/turn to resume"))
  (let ((resp (fubar-musn--json-request "POST" "/musn/turn/resume"
                                        `(("session/id" . ,fubar-musn-session-id)
                                          ("turn" . ,fubar-musn-turn)
                                          ("note" . ,note)))))
    (fubar-musn--render-line (format "[MUSN-RESUME] %s" (json-encode resp)))
    (display-buffer fubar-musn-view-buffer)))

(provide 'fubar-viewer)
;;; fubar-viewer.el ends here
