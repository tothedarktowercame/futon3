;;; dump-state.el -- emit real buffer state as JSON for the adapter.
;;; READ-ONLY: no buffer is killed, modified, or displayed by this.
;;; Structural kinds only (no staleness threshold here): the wiring's
;;; args decide file-staleness from the emitted age. Preservation
;;; signals are emitted RAW; classification is the Clojure side's job.
;;; Robust to buffers dying mid-iteration (live Emacs mutates).
(require 'json)

(defun bc--visible-p (buf)
  (cl-some (lambda (f) (get-buffer-window buf f)) (frame-list)))

(defun bc--age (buf)
  "Per-buffer display age in seconds (buffer-local value), -1 if never displayed."
  (let ((dt (buffer-local-value 'buffer-display-time buf)))
    (if dt (ignore-errors (float-time (time-subtract (current-time) dt))) -1)))

(defun bc--kind (buf)
  "STRUCTURAL kind only; no threshold logic. Returns a string."
  (if (not (buffer-live-p buf))
      "dead"
    (condition-case nil
        (with-current-buffer buf
          (let ((name (buffer-name buf)))
            (cond
             ((buffer-file-name buf) "file")
             ((and (boundp 'futon-buffer-cleaner-stream-buffer-regexp)
                   (string-match-p futon-buffer-cleaner-stream-buffer-regexp name))
              "stream")
             ((and (boundp 'futon-buffer-cleaner-http-buffer-regexp)
                   (string-match-p futon-buffer-cleaner-http-buffer-regexp name))
              "http")
             ((string-prefix-p "*invoke: " name) "invoke")
             ((eq major-mode 'dired-mode) "dired")
             ((and (boundp 'futon-buffer-cleaner-temp-modes)
                   (memq major-mode futon-buffer-cleaner-temp-modes))
              "temp")
             ((and (boundp 'futon-buffer-cleaner-render-buffer-regexps)
                   (cl-some (lambda (re) (string-match-p re name))
                            futon-buffer-cleaner-render-buffer-regexps))
              "render")
             (t "other"))))
      (error "error"))))

(defun bc--row (buf)
  (list (cons 'name (buffer-name buf))
        (cons 'kind (bc--kind buf))
        (cons 'file (if (buffer-file-name buf) t :json-false))
        (cons 'modified (with-current-buffer buf (buffer-modified-p)))
        (cons 'has-process (if (get-buffer-process buf) t :json-false))
        (cons 'visible (if (bc--visible-p buf) t :json-false))
        (cons 'display-age-seconds (bc--age buf))))

(defun bc--state ()
  "List of per-buffer RAW state rows. Read-only, no decisions."
  (let (out)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (condition-case nil
            (push (bc--row buf) out)
          (error nil))))
    (reverse out)))

(defun bc--state-json-to-file (path)
  "Write the packet JSON directly to PATH (avoids emacsclient escaping)."
  (with-temp-file path
    (insert (json-encode (list (cons 'buffers (bc--state))
                               (cons 'emitted-at (format-time-string "%FT%T%z"))))))
  (princ "written"))
