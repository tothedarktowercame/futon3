;;; futon3-embedding.el --- Futon3 embedding helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to isolate Futon3 embedding
;; helpers (sigils, hotword scans) from chat UI logic.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function my-chatgpt-shell--dedupe-sigils "aob-chatgpt" (sigils))

(defvar my-futon3-pattern-index-file
  (expand-file-name "resources/sigils/patterns-index.tsv" my-futon3--repo-root)
  "Tab-separated index mapping patterns to Tokipona/TruthTable tags plus rationales.")

(defvar my-futon3-type-vocab-file
    (expand-file-name "resources/type_vocab.txt" my-futon3--repo-root)
    "Plain-text vocabulary of English trigger words used for classical embedding.")

(defvar my-futon3--tokipona-emoji-map nil)
(defvar my-futon3--tokipona-emoji-mtime nil)
(defconst my-futon3--tokipona-ignore-words '("la" "pi" "li" "e" "anu" "en")
  "Particles that should not seed emoji lookups when embedding intent text.")

(defvar my-futon3--cached-pattern-index nil)
(defvar my-futon3--cached-pattern-index-mtime nil)
(defvar my-futon3--cached-type-vocab nil)
(defvar my-futon3--cached-type-vocab-mtime nil)

(defun my-futon3--read-file-lines (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t "[ \t]+"))))

(defun my-futon3--type-vocab ()
  (let* ((file my-futon3-type-vocab-file)
         (mtime (and (file-exists-p file)
                     (float-time (file-attribute-modification-time (file-attributes file))))))
    (when (and mtime
               (or (null my-futon3--cached-type-vocab-mtime)
                   (> mtime my-futon3--cached-type-vocab-mtime)))
      (setq my-futon3--cached-type-vocab
            (let ((table (make-hash-table :test 'equal)))
              (dolist (line (my-futon3--read-file-lines file))
                (let ((token (string-trim (downcase line))))
                  (unless (or (string-empty-p token)
                              (string-prefix-p "#" token))
                    (puthash token t table))))
              table))
      (setq my-futon3--cached-type-vocab-mtime mtime))
    my-futon3--cached-type-vocab))

(defun my-futon3--parse-hotwords (field)
  (when (and field (not (string-empty-p field)))
    (mapcar #'string-trim
            (split-string (downcase field) "[,;]" t "[ \t]+"))))

(defun my-futon3--pattern-index ()
  (let* ((file my-futon3-pattern-index-file)
         (mtime (and (file-exists-p file)
                     (float-time (file-attribute-modification-time (file-attributes file))))))
    (when (and mtime
               (or (null my-futon3--cached-pattern-index-mtime)
                   (> mtime my-futon3--cached-pattern-index-mtime)))
      (setq my-futon3--cached-pattern-index
            (let (entries)
              (when (file-readable-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (not (eobp))
                    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                                (line-end-position))))
                      (unless (or (string-empty-p line)
                                  (string-prefix-p "#" line))
                        (let* ((cols (split-string line "\t"))
                               (pattern (nth 0 cols))
                               (tokipona (nth 1 cols))
                               (truth (nth 2 cols))
                               (rationale (nth 3 cols))
                               (hotwords (nth 4 cols)))
                          (push (list :pattern pattern
                                      :tokipona tokipona
                                      :truth truth
                                      :rationale rationale
                                      :hotwords (my-futon3--parse-hotwords hotwords))
                                entries))))
                    (forward-line 1))))
              (nreverse entries)))
      (setq my-futon3--cached-pattern-index-mtime mtime))
    my-futon3--cached-pattern-index))

(defun my-futon3--section->emoji (section)
  (let ((map (my-futon3--tokipona-emoji-map)))
    (when (and map section)
      (let* ((lower (downcase (string-trim section)))
             (base (car (split-string lower "(" t)))
             (tokens (seq-filter (lambda (token)
                                   (and (> (length token) 0)
                                        (not (member token my-futon3--tokipona-ignore-words))))
                                 (split-string (or base "") "[^[:alpha:]]+" t))))
        (seq-some (lambda (token)
                    (gethash token map))
                  tokens)))))

(defun my-futon3--truth->hanzi (text)
  (when (and text (not (string-empty-p text)))
    (let ((trim (string-trim text)))
      (when (string-match "\\`\\([^[:space:](]+\\)" trim)
        (match-string 1 trim)))))

(defun my-futon3--embed-sigils-from-text (text &optional limit)
  (let ((clean (and text (string-trim text)))
        (limit (or limit 4)))
    (when (and clean (> (length clean) 0))
      (cl-destructuring-bind (sections hanzi) (my-futon3--embed-scan clean)
        (let* ((emoji-values (cl-loop with acc = nil
                                      with taken = 0
                                      for (label . _) in sections
                                      for emoji = (my-futon3--section->emoji label)
                                      when emoji do (push emoji acc)
                                                   (setq taken (1+ taken))
                                      when (>= taken limit)
                                      return (nreverse acc)
                                      finally return (nreverse acc)))
               (hanzi-values (cl-loop with acc = nil
                                      with taken = 0
                                      for (label . _) in hanzi
                                      for ch = (my-futon3--truth->hanzi label)
                                      when ch do (push ch acc)
                                                 (setq taken (1+ taken))
                                      when (>= taken limit)
                                      return (nreverse acc)
                                      finally return (nreverse acc)))
               (pairs (cl-loop for emoji in emoji-values
                               for han in hanzi-values
                               while (and emoji han)
                               collect (list :emoji emoji :hanzi han))))
          (when pairs
            (my-chatgpt-shell--dedupe-sigils pairs)))))))

(defun my-futon3--tokipona-emoji-map ()
  (let* ((file (expand-file-name "holes/tokipona.org" my-futon3--repo-root))
         (attrs (and (file-exists-p file) (file-attributes file)))
         (mtime (and attrs (float-time (file-attribute-modification-time attrs)))))
    (when (and mtime
               (or (null my-futon3--tokipona-emoji-mtime)
                   (> mtime my-futon3--tokipona-emoji-mtime)))
      (let ((table (make-hash-table :test 'equal)))
        (when (file-readable-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                          (line-end-position))))
                (when (and (string-prefix-p "|" line)
                           (not (string-match-p "^|[-+]" line)))
                  (let* ((parts (mapcar #'string-trim (split-string line "|" t)))
                         (emoji (car parts))
                         (word (and (cadr parts)
                                    (downcase (cadr parts)))))
                    (when (and emoji word
                               (not (string-match-p "toki pona" word)))
                      (puthash word emoji table)))))
              (forward-line 1))))
        (setq my-futon3--tokipona-emoji-map table
              my-futon3--tokipona-emoji-mtime mtime)))
    my-futon3--tokipona-emoji-map))

(defun my-futon3--embed-scan (text)
  "Return alists of section/hanzi scores given TEXT."
  (let* ((lower (downcase text))
         (index (my-futon3--pattern-index))
         (type-vocab (my-futon3--type-vocab))
         (section-score (make-hash-table :test 'equal))
         (hanzi-score (make-hash-table :test 'equal)))
    (dolist (entry index)
      (let ((section (plist-get entry :tokipona))
            (hanzi (plist-get entry :truth))
            (hotwords (plist-get entry :hotwords)))
        (dolist (hw hotwords)
          (when (and (not (string-empty-p hw))
                     (or (null type-vocab) (gethash hw type-vocab))
                     (string-match-p (regexp-quote hw) lower))
            (cl-incf (gethash section section-score 0))
            (cl-incf (gethash hanzi hanzi-score 0))))))
    (list (sort (mapcar (lambda (key)
                          (cons key (gethash key section-score)))
                        (hash-table-keys section-score))
                (lambda (a b) (> (cdr a) (cdr b))))
          (sort (mapcar (lambda (key)
                          (cons key (gethash key hanzi-score)))
                        (hash-table-keys hanzi-score))
                (lambda (a b) (> (cdr a) (cdr b)))))))

(defun my-futon3-embed-english (text)
  "Embed TEXT into the Tokipona/Truth-table space using classical hotwords."
  (interactive "sText to embed: ")
  (cl-destructuring-bind (sections hanzi) (my-futon3--embed-scan text)
    (if (and (null sections) (null hanzi))
        (message "No hotword matches")
      (with-current-buffer (get-buffer-create "*F3-Embedding*")
        (erase-buffer)
        (insert (format "Input: %s\n\n" text))
        (insert "Sections (Tokipona):\n")
        (dolist (entry sections)
          (insert (format "  %s -> %s\n" (car entry) (cdr entry))))
        (insert "\nTruth-table tags:\n")
        (dolist (entry hanzi)
          (insert (format "  %s -> %s\n" (car entry) (cdr entry))))
        (display-buffer (current-buffer))))))

(defun my-futon3-embed-region (start end)
  "Embed the region between START and END."
  (interactive "r")
  (my-futon3-embed-english (buffer-substring-no-properties start end)))

(provide 'futon3-embedding)

;;; futon3-embedding.el ends here
