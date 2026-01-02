;;; futon3-sessions-test.el --- ERT tests for futon3-sessions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(defvar my-futon3--repo-root
  (expand-file-name "." futon3--test-root))

(require 'futon3-hud)
(require 'futon3-sessions)

(ert-deftest futon3-sessions-format-counts-plist ()
  (should (equal (my-futon3--format-counts '(:foo 2 :bar 1))
                 "foo=2, bar=1")))

(ert-deftest futon3-sessions-format-counts-alist ()
  (should (equal (my-futon3--format-counts '(("foo" . 2) ("bar" . 1)))
                 "foo=2, bar=1")))

(ert-deftest futon3-sessions-suggest-intent-text ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "f3/p4" "FUTON3 Prototype 4" table)
    (cl-letf (((symbol-function 'my-futon3--prototype-display-map)
               (lambda () table)))
      (should (equal (my-futon3--suggest-intent-text '("f3/p4"))
                     "FUTON3 Prototype 4")))))

