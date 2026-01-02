;;; futon3-embedding-test.el --- ERT tests for futon3-embedding -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst futon3--test-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "contrib" futon3--test-root))

(defvar my-futon3--repo-root
  (expand-file-name "." futon3--test-root))

(require 'futon3-embedding)

(ert-deftest futon3-embedding-parse-hotwords ()
  (should (equal (my-futon3--parse-hotwords "alpha, beta; gamma")
                 '("alpha" "beta" "gamma"))))

(ert-deftest futon3-embedding-truth->hanzi ()
  (should (equal (my-futon3--truth->hanzi "hanzi (note)") "hanzi")))

(ert-deftest futon3-embedding-section->emoji ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "alpha" "A" table)
    (let ((my-futon3--tokipona-emoji-map table))
      (should (equal (my-futon3--section->emoji "alpha (note)") "A")))))

(ert-deftest futon3-embedding-scan-hotwords ()
  (cl-letf (((symbol-function 'my-futon3--pattern-index)
             (lambda ()
               (list (list :tokipona "section-a" :truth "han-a" :hotwords '("alpha"))
                     (list :tokipona "section-b" :truth "han-b" :hotwords '("beta")))))
            ((symbol-function 'my-futon3--type-vocab)
             (lambda ()
               (let ((table (make-hash-table :test 'equal)))
                 (puthash "alpha" t table)
                 (puthash "beta" t table)
                 table))))
    (let* ((result (my-futon3--embed-scan "alpha beta alpha"))
           (sections (car result))
           (hanzi (cadr result)))
      (should (equal (car sections) '("section-a" . 2)))
      (should (equal (car hanzi) '("han-a" . 2))))))

(ert-deftest futon3-embedding-embed-sigils-from-text ()
  (cl-letf (((symbol-function 'my-futon3--embed-scan)
             (lambda (_)
               (list '(("alpha" . 1))
                     '(("han" . 1)))))
            ((symbol-function 'my-futon3--section->emoji)
             (lambda (_label) "A"))
            ((symbol-function 'my-futon3--truth->hanzi)
             (lambda (_label) "H"))
            ((symbol-function 'my-chatgpt-shell--dedupe-sigils)
             (lambda (sigils) sigils)))
    (let ((sigils (my-futon3--embed-sigils-from-text "alpha")))
      (should (equal sigils '((:emoji "A" :hanzi "H")))))))

