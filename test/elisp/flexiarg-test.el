;;; flexiarg-test.el --- ERT coverage for flexiarg highlighting -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the futon3 repo for resolving relative paths in tests.")

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(load (expand-file-name "contrib/flexiarg.el" futon3--test-root))

(defun futon3-test--face-has-p (face target)
  "Return non-nil when FACE contains TARGET."
  (cond
   ((null face) nil)
   ((listp face) (memq target face))
   (t (eq face target))))

(ert-deftest flexiarg-highlights-facet-name-and-content ()
  "Facet highlighting should apply to both name and content."
  (with-temp-buffer
    (insert "  + context: You are framing how proof-of-concept funds should be spent.\n")
    (flexiarg-mode)
    (setq flexiarg-highlight-facet-targets '(name content))
    (flexiarg-set-highlight-facets "context")
    (let ((re (flexiarg--facet-regexp)))
      (should re)
      (goto-char (point-min))
      (should (re-search-forward re nil t))
      (should (match-beginning 2)))
    (flexiarg--apply-facet-highlights)
    (let* ((name-pos (progn (goto-char (point-min))
                            (re-search-forward "\\bcontext\\b")
                            (match-beginning 0)))
           (content-pos (progn (goto-char (point-min))
                               (re-search-forward "You are framing")
                               (match-beginning 0)))
           (name-face (get-text-property name-pos 'face))
           (content-face (get-text-property content-pos 'face)))
      (should (futon3-test--face-has-p name-face 'flexiarg-highlight-facet-name-face))
      (should (futon3-test--face-has-p content-face 'flexiarg-highlight-facet-content-face)))))

;;; flexiarg-test.el ends here
