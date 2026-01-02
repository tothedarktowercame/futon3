;;; futon3-arxana-compat-test.el --- ERT tests for futon3-arxana-compat -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(defvar my-futon3--repo-root
  (expand-file-name "." futon3--test-root))

(require 'futon3-hud)
(require 'futon3-arxana-compat)

(ert-deftest futon3-arxana-logs-pattern-edit ()
  (let (calls)
    (cl-letf (((symbol-function 'my-futon3--clock-context)
               (lambda () (list :prototypes '("f3/p4") :intent "intent")))
              ((symbol-function 'my-chatgpt-shell--now-iso)
               (lambda () "2024-01-01T00:00:00Z"))
              ((symbol-function 'my-chatgpt-shell--futon1-post)
               (lambda (path payload) (push (list path payload) calls)))
              ((symbol-function 'my-chatgpt-shell--debug)
               (lambda (&rest _args) nil)))
      (my-futon3--log-pattern-edit "f3/p4")
      (should (= (length calls) 1))
      (let ((payload (cadar calls)))
        (should (equal (plist-get payload :type) ":clock/pattern-edit"))
        (should (equal (plist-get (plist-get payload :props) :intent) "intent"))))))

