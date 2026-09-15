;;; dump-state.el -- emit real buffer state as JSON for the adapter.
;;; READ-ONLY: no buffer is killed, modified, or displayed by this.
;;; Reuses futon-buffer-cleaner's own predicates when loaded; else 'unknown.
(require 'json)

(defun bc--visible-p (buf)
  (cl-some (lambda (f) (get-buffer-window buf f)) (frame-list)))

(defun bc--kind (buf)
  (let ((h (make-hash-table)))
    (puthash buf (bc--visible-p buf) h)
    (if (fboundp 'futon-buffer-cleaner--candidate-kind)
        (symbol-name (or (ignore-errors
                           (futon-buffer-cleaner--candidate-kind buf h))
                         'unknown))
      "unknown")))

(defun bc--state ()
  "Return an alist of per-buffer state rows. Read-only."
  (let (out)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf))
            (age (if (and (boundp 'buffer-display-time) buffer-display-time)
                     (ignore-errors
                       (float-time (time-subtract (current-time)
                                                  buffer-display-time)))
                   -1)))
        (push (list (cons 'name name)
                    (cons 'kind (bc--kind buf))
                    (cons 'file (if (buffer-file-name buf) t :json-false))
                    (cons 'modified (if (buffer-modified-p buf) t :json-false))
                    (cons 'process-gone (if (get-buffer-process buf) :json-false t))
                    (cons 'visible (if (bc--visible-p buf) t :json-false))
                    (cons 'display-age-seconds age))
              out)))
    (reverse out)))

(defun bc--state-json ()
  "Entry point for emacsclient: print the packet as JSON."
  (princ (json-encode (list (cons 'buffers (bc--state))
                            (cons 'emitted-at (format-time-string "%FT%T%z"))))))

(defun bc--state-json-to-file (path)
  "Write the packet JSON directly to PATH (avoids emacsclient escaping)."
  (with-temp-file path
    (insert (json-encode (list (cons 'buffers (bc--state))
                               (cons 'emitted-at (format-time-string "%FT%T%z"))))))
  (princ "written"))
