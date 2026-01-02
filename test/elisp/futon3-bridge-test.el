;;; futon3-bridge-test.el --- ERT tests for futon3-bridge -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(defvar my-futon3--repo-root
  (expand-file-name "." futon3--test-root))

(require 'futon3-bridge)

(ert-deftest futon3-bridge-tatami-url ()
  (let ((my-futon3-ui-base-url "http://localhost:6060/"))
    (should (equal (my-futon3--tatami-url "/musn/tatami/status")
                   "http://localhost:6060/musn/tatami/status"))))

