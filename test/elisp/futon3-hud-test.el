;;; futon3-hud-test.el --- ERT tests for futon3-hud -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(defvar my-futon3--repo-root
  (expand-file-name "." futon3--test-root))

(require 'futon3-hud)

(ert-deftest futon3-hud-prototype-string ()
  (should (equal (my-futon3--prototype-string :f3/p4) "f3/p4"))
  (should (equal (my-futon3--prototype-string "f3/p4") "f3/p4")))

(ert-deftest futon3-hud-parse-prototype-string ()
  (let ((my-futon3-tatami-default-prototypes '("f0/p0")))
    (should (equal (my-futon3--parse-prototype-string ":f3/p4, f2/p1")
                   '("f3/p4" "f2/p1")))))

(ert-deftest futon3-hud-display-prototypes ()
  (let ((my-futon3-tatami-default-prototypes '("f0/p0")))
    (should (equal (my-futon3--display-prototypes '("f3/p4")) '("f3/p4")))
    (should (equal (my-futon3--display-prototypes nil) '("f0/p0")))))

(ert-deftest futon3-hud-prototype-detail-lines ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "f3/p4" "FUTON3 Prototype 4" table)
    (cl-letf (((symbol-function 'my-futon3--prototype-display-map)
               (lambda () table)))
      (let ((lines (my-futon3--prototype-detail-lines '("f3/p4"))))
        (should (equal lines '("FUTON3 Prototype 4 (f3/p4)")))))))

