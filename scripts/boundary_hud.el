;;; boundary_hud.el --- Futon boundary readiness HUD -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'tabulated-list)

(defgroup futon-boundary-hud nil
  "Visualise FUTON readiness / negative-space metrics."
  :group 'applications)

(defcustom futon-boundary-hud-file
  (let* ((base (or load-file-name buffer-file-name default-directory))
         (root (file-name-directory
                (directory-file-name
                 (file-name-directory (directory-file-name base))))))
    (expand-file-name "resources/boundary.edn" root))
  "Path to the boundary readiness EDN snapshot."
  :type 'file
  :group 'futon-boundary-hud)

(defvar futon-boundary-hud--entries nil)
(defvar futon-boundary-hud--buffer-name "*Futon Boundary*")

(defun futon-boundary-hud--read-file (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun futon-boundary-hud--edn->elisp (text)
  "Minimal EDN -> elisp reader for the boundary snapshot."
  (when text
    (let ((converted (replace-regexp-in-string
                      "}" ")"
                      (replace-regexp-in-string "{" "(" text nil 'literal)
                      nil 'literal)))
      (car (read-from-string converted)))))

(defun futon-boundary-hud--load ()
  (let* ((text (futon-boundary-hud--read-file futon-boundary-hud-file))
         (data (and text (futon-boundary-hud--edn->elisp text)))
         (futons (plist-get data :futons)))
    (setq futon-boundary-hud--entries (mapcar #'futon-boundary-hud--format futons))))

(defun futon-boundary-hud--orb (entry)
  (let* ((missing (or (plist-get entry :missing_evidence) 0))
         (protos (max 1 (or (plist-get entry :prototypes) 1)))
         (ratio (/ (float missing) protos)))
    (cond
     ((<= ratio 0.1) "🟢")
     ((<= ratio 0.5) "🔴")
     (t "🟣"))))

(defun futon-boundary-hud--format (entry)
  (let* ((id (plist-get entry :id))
         (orb (futon-boundary-hud--orb entry))
         (modified (or (plist-get entry :last_modified) "(unknown)"))
         (missing (plist-get entry :missing_evidence))
         (todos (plist-get entry :todo_count))
         (exists (plist-get entry :exists)))
    (list id
          (vector id
                  (if exists orb "(missing)")
                  modified
                  (number-to-string (or missing 0))
                  (number-to-string (or todos 0))))))

(defun futon-boundary-hud-refresh ()
  (interactive)
  (futon-boundary-hud--load)
  (setq tabulated-list-entries futon-boundary-hud--entries)
  (tabulated-list-print t))

(define-derived-mode futon-boundary-hud-mode tabulated-list-mode "FutonBoundary"
  "Major mode for inspecting FUTON boundary readiness."
  (setq tabulated-list-format [("Futon" 6 t)
                               ("Orb" 6 nil)
                               ("Last update" 25 t)
                               ("Missing evidence" 18 t)
                               ("TODOs" 8 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Futon" nil))
  (futon-boundary-hud-refresh))

(defun futon-boundary-hud ()
  "Open the Futon boundary readiness buffer."
  (interactive)
  (let ((buf (get-buffer-create futon-boundary-hud--buffer-name)))
    (with-current-buffer buf
      (futon-boundary-hud-mode))
    (pop-to-buffer buf)))

(provide 'boundary_hud)
