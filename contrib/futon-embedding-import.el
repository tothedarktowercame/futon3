;;; futon-embedding-import.el --- Import embedding caches -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for importing embedding caches into Futon1.

;;; Code:

(require 'json)
(require 'subr-x)

(declare-function arxana-links-make-embedding-cache "arxana-links"
                  (&key space model dimensions neighbors k threshold))
(declare-function arxana-links-persist-embedding-cache "arxana-links" (cache))

;;;###autoload
(defun futon-embedding-import-cache-from-json (space json-path &optional model dimensions)
  "Import an embedding cache from JSON-PATH and persist it to Futon1.
SPACE is the embedding space name (e.g. \"patterns-glove\").
The JSON is expected to be a list of entries with :id and :neighbors."
  (interactive
   (list (read-string "Embedding space: " "patterns-glove")
         (read-file-name "Neighbors JSON: " nil nil t)
         (read-string "Model (optional): " nil nil "glove")
         nil))
  (unless (and (stringp json-path) (file-readable-p json-path))
    (user-error "Neighbors JSON not found: %s" json-path))
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (entries (json-read-file json-path))
         (neighbors (mapcar
                     (lambda (entry)
                       (let* ((entry-id (plist-get entry :id))
                              (items (or (plist-get entry :neighbors) '()))
                              (items (mapcar
                                      (lambda (neighbor)
                                        (list :id (plist-get neighbor :id)
                                              :score (plist-get neighbor :score)))
                                      items)))
                         (cons entry-id items)))
                     entries))
         (k (let* ((first (car entries))
                   (items (and first (plist-get first :neighbors))))
              (and (listp items) (length items))))
         (cache (arxana-links-make-embedding-cache
                 :space space
                 :model (and model (string-trim model))
                 :dimensions dimensions
                 :neighbors neighbors
                 :k k)))
    (if (arxana-links-persist-embedding-cache cache)
        (message "Embedding cache persisted: %s (%d entries)"
                 (plist-get cache :xt/id)
                 (length neighbors))
      (message "Failed to persist embedding cache (check Futon sync)"))))

(provide 'futon-embedding-import)
;;; futon-embedding-import.el ends here
