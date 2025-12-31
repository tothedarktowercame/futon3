;;; flexiarg-mode.el --- Flexiformal argument DSL for EDN-backed summaries -*- lexical-binding: t; -*-

;; Author: ChatGPT (with Joseph in the loop)
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (cl-lib "0.6") (subr-x "0.1"))
;; Keywords: outlines, arguments, writing, tools
;; URL: https://example.invalid/flexiarg  ;; placeholder

;;; Commentary:

;; flexiarg-mode provides a lightweight, Fountain/Ink-style syntax for
;; specifying structured arguments that compile to an EDN-style tree.
;;
;; Example syntax:
;;
;;   @arg t4r/exec-summary
;;   @title talk4real – Infrastructure for Developing Collective Intelligence
;;   @audience research funders, doctoral-training consortia, university R&I leads
;;   @tone analytic-visionary
;;   @length 200-250
;;   @ban cutting-edge, state-of-the-art, world-leading
;;   @allow-new-claims false
;;   @max-iterations 3
;;   @up t4r/main-case
;;   @next t4r/support
;;
;;   ! conclusion: talk4real reimagines doctoral training as a shared
;;     experimental learning infrastructure that refines itself through
;;     reflexive, collective practice.
;;
;;   + because: Most doctoral programmes operate within disciplinary silos.
;;     + therefore: These silos contribute little to system-level learning.
;;
;;   + because: Collective-intelligence frameworks can capture feedback
;;     between researchers, institutions, and communities.
;;     + therefore: Embedding these feedback loops allows systems to learn.
;;     + instantiated-by: talk4real applies a REPL-style cycle.
;;       + clarifies: This cycle links ethnography, computation, design, reflection.
;;       ? example(optional): Add 1–2 recognisable precedents if space permits.
;;
;;   + underpinned-by: The model is structured by three design principles.
;;     + specifies: Ethical reflexivity ...
;;     + specifies: Lightweight infrastructure ...
;;     + specifies: Polyphonic governance ...
;;
;; Commands:
;;   M-x flexiarg-show-edn
;;     → parses current buffer and shows the EDN-style structure.
;;
;; This is intentionally minimal but structured so you can:
;; - feed the EDN spec into a "compile-summary" LLM prompt builder,
;; - run classical validation / grading loops,
;; - keep arguments flexiformal but machine-checkable.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq nil t)
(unless (fboundp 'seq)
  (defun seq (sequence)
    "Fallback for older Emacs; return SEQUENCE as a list or nil if empty."
    (cond
     ((null sequence) nil)
     ((listp sequence) sequence)
     ((and (vectorp sequence) (= 0 (length sequence))) nil)
     (t (append sequence nil)))))

(defgroup flexiarg nil
  "Flexiformal argument DSL."
  :group 'tools
  :prefix "flexiarg-")

(defface flexiarg-highlight-facet-name-face
  '((t :inherit font-lock-keyword-face :underline t))
  "Face used to highlight facet names."
  :group 'flexiarg)
(defvar flexiarg-highlight-facet-name-face 'flexiarg-highlight-facet-name-face
  "Face symbol used to highlight facet names.")

(defface flexiarg-highlight-facet-content-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face used to highlight facet contents."
  :group 'flexiarg)
(defvar flexiarg-highlight-facet-content-face 'flexiarg-highlight-facet-content-face
  "Face symbol used to highlight facet contents.")

(defvar flexiarg-mode-map (make-sparse-keymap)
  "Keymap for `flexiarg-mode'.")

(defun flexiarg--ensure-keymap ()
  "Ensure `flexiarg-mode-map' is a valid keymap."
  (unless (keymapp flexiarg-mode-map)
    (setq flexiarg-mode-map (make-sparse-keymap))))

(flexiarg--ensure-keymap)

;; ------------------------------
;; Syntax highlighting
;; ------------------------------

(defcustom flexiarg-highlight-facets nil
  "List of facet names to highlight (strings).

Example: '(\"context\" \"next-steps\")"
  :type '(repeat string)
  :group 'flexiarg)

(defcustom flexiarg-highlight-facet-targets '(name content)
  "Which parts of a facet to highlight."
  :type '(set (const name) (const content))
  :group 'flexiarg)

(defvar-local flexiarg--facet-font-lock-keywords nil
  "Buffer-local font-lock keywords for facet highlighting.")
(defvar flexiarg--base-font-lock-keywords
  (let* (
         ;; Highlight questions as warnings.
         (question-re "^\\s-*@question:.*$")
         ;; Meta directives: @arg, @title, @audience, etc.
         (meta-re "^\\s-*\\(@[a-zA-Z0-9_-]+\\)")
         ;; Markers: ! + ?
         (marker-re "^\\s-*\\([!+?]\\)\\s-*\\([^:]+\\):")
         )
    `(
      (,question-re . font-lock-warning-face)
      (,meta-re 1 font-lock-keyword-face)
      (,marker-re
       (1 font-lock-builtin-face)         ;; ! + ?
       (2 font-lock-keyword-face))        ;; relation / role
      )))

(defun flexiarg--facet-regexp ()
  (when (and flexiarg-highlight-facets
             (not (equal flexiarg-highlight-facets '())))
    (format "^\\s-*\\+\\s-*\\(%s\\)\\s-*:\\s-*\\(.+\\)$"
            (regexp-opt flexiarg-highlight-facets))))

(defun flexiarg--build-facet-keywords ()
  (when-let ((re (flexiarg--facet-regexp)))
    (let ((name? (memq 'name flexiarg-highlight-facet-targets))
          (content? (memq 'content flexiarg-highlight-facet-targets)))
      (cond
       ((and name? content?)
        (list (list re (list 1 flexiarg-highlight-facet-name-face 'prepend))
              (list #'flexiarg--facet-content-match
                    0
                    flexiarg-highlight-facet-content-face
                    'prepend)))
       (name?
        (list (list re (list 1 flexiarg-highlight-facet-name-face 'prepend))))
       (content?
        (list (list #'flexiarg--facet-content-match
                    0
                    flexiarg-highlight-facet-content-face
                    'prepend)))
       (t nil)))))

(defun flexiarg--facet-content-match (limit)
  "Search for facet content up to LIMIT and set match data to its contents."
  (let ((re (flexiarg--facet-regexp)))
    (when re
      (catch 'found
        (while (re-search-forward re limit t)
          (let ((start (match-beginning 2))
                (end (match-end 2)))
            (when (and start end (< start end))
              (set-match-data (list start end))
              (throw 'found t))))))))

(defun flexiarg--apply-facet-highlights ()
  "Apply facet highlighting directly over the current buffer."
  (let ((re (flexiarg--facet-regexp))
        (name? (memq 'name flexiarg-highlight-facet-targets))
        (content? (memq 'content flexiarg-highlight-facet-targets)))
    (when re
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (when name?
            (add-face-text-property (match-beginning 1)
                                    (match-end 1)
                                    flexiarg-highlight-facet-name-face))
          (when content?
            (add-face-text-property (match-beginning 2)
                                    (match-end 2)
                                    flexiarg-highlight-facet-content-face)))))))

(defun flexiarg--parse-facets (raw)
  (let* ((parts (split-string (or raw "") "[,]" t "[ \t\n]+"))
         (trimmed (mapcar #'string-trim parts)))
    (cl-remove-if #'string-empty-p trimmed)))

(defun flexiarg-set-highlight-facets (facets)
  "Set `flexiarg-highlight-facets' and refresh highlighting.

FACETS can be a list of strings or a comma-separated string."
  (interactive (list (read-string "Highlight facets (comma-separated, empty to clear): ")))
  (setq flexiarg-highlight-facets
        (cond
         ((listp facets) facets)
         ((stringp facets) (flexiarg--parse-facets facets))
         (t nil)))
  (when (derived-mode-p 'flexiarg-mode)
    (setq flexiarg--facet-font-lock-keywords (flexiarg--build-facet-keywords))
    (setq-local font-lock-defaults
                (list (append flexiarg--base-font-lock-keywords
                              flexiarg--facet-font-lock-keywords)))
    (font-lock-refresh-defaults)
    (font-lock-flush)
    (font-lock-ensure)
    (flexiarg--apply-facet-highlights)))

;; ------------------------------
;; Utility helpers
;; ------------------------------

(defun flexiarg--keyword (s)
  "Convert string S to a Clojure-style keyword (as symbol)."
  (intern (concat ":" (replace-regexp-in-string "[ \t]+" "-" (downcase s)))))

(defun flexiarg--split-list (s)
  "Split comma- or space-separated list S into non-empty strings."
  (when s
    (let* ((parts (split-string s "[,]" t "[ \t\n]+"))
           (trimmed (mapcar #'string-trim parts)))
      (cl-remove-if (lambda (x) (string-empty-p x)) trimmed))))

(defun flexiarg--parse-bool (s)
  "Parse S as boolean if it looks like true/false, else return S."
  (let ((ls (downcase (string-trim s))))
    (cond
     ((member ls '("true" "t" "yes" "y")) t)
     ((member ls '("false" "nil" "no" "n")) nil)
     (t s))))

(defun flexiarg--parse-number (s)
  "Parse S as number if possible, else nil."
  (when (string-match-p "^[0-9]+$" (string-trim s))
    (string-to-number (string-trim s))))

(defun flexiarg--parse-range (s)
  "Parse S like \"200-250\" into a list [min max], else nil."
  (when (string-match "^\\s-*\\([0-9]+\\)\\s-*[-–]\\s-*\\([0-9]+\\)\\s-*$" s)
    (list (string-to-number (match-string 1 s))
          (string-to-number (match-string 2 s)))))

;; ------------------------------
;; Parsing meta lines (@...)
;; ------------------------------

(defun flexiarg--parse-meta-line (line)
  "Parse a meta LINE starting with @.
Return (key . value) or nil."
  (when (string-match "^\\s-*@\\([a-zA-Z0-9_-]+\\)\\s-*\\(.*\\)$" line)
    (let* ((key (downcase (match-string 1 line)))
           (rest (string-trim (match-string 2 line))))
      (cons
       (pcase key
         ("arg"          :id)
         ("title"        :title)
         ("audience"     :audience)
         ("tone"         :tone)
         ("length"       :length)
         ("ban"          :banned-phrases)
         ("allow-new-claims" :allow-new-claims?)
         ("max-iterations" :max-iterations)
         (_ (intern (concat ":" key))))
       (pcase key
         ("audience" (flexiarg--split-list rest))
         ("ban"      (flexiarg--split-list rest))
         ("length"   (or (flexiarg--parse-range rest)
                         (flexiarg--parse-number rest)
                         rest))
         ("allow-new-claims" (flexiarg--parse-bool rest))
         ("max-iterations"   (or (flexiarg--parse-number rest) rest))
         (_ rest))))))

;; ------------------------------
;; Node construction
;; ------------------------------

(cl-defstruct flexiarg-node
  marker            ;; ? ! +
  relation-raw      ;; raw relation string (e.g. "because", "conclusion", "example(optional)")
  relation          ;; keyword for relation-to-parent
  role              ;; :conclusion, :premise, :hole, :example, etc.
  kind              ;; for holes/examples: :example, :evidence, etc.
  required?         ;; for holes
  text              ;; main text
  children)         ;; list of child nodes

(defun flexiarg--compute-role-and-rel (marker relation-raw)
  "Compute (role relation kind required?) from MARKER and RELATION-RAW."
  (let* ((rel-str (string-trim (or relation-raw "")))
         (rel-str-low (downcase rel-str)))
    (cond
     ;; Conclusion
     ((eq marker ?!)
      (let ((role :conclusion))
        (list role nil nil t)))
     ;; Hole / example marker
     ((eq marker ??)
      ;; e.g. example(optional), evidence(required)
      (if (string-match "^\\([a-zA-Z0-9-]+\\)(optional)\\s-*$" rel-str-low)
          (let* ((base (match-string 1 rel-str-low))
                 (kind (flexiarg--keyword base)))
            (list :hole :illustrates kind nil))
        (let* ((base (car (split-string rel-str-low "[() \t]" t)))
               (kind (flexiarg--keyword (or base "hole"))))
          (list :hole :illustrates kind t))))
     ;; Regular premise / relation
     ((eq marker ?+)
      (let* ((rel-kw (flexiarg--keyword rel-str-low)))
        (list :premise rel-kw nil t)))
     (t
      (list :premise nil nil t)))))

(defun flexiarg--parse-node-line (line)
  "Parse LINE starting with !, + or ? into a flexiarg-node, indent, and marker.
Return (indent node) or nil."
  (when (string-match "^\\(\\s-*\\)\\([!+?]\\)\\s-*\\([^:]+\\):\\s-*\\(.*\\)$" line)
    (let* ((indent (length (match-string 1 line)))
           (marker (string-to-char (match-string 2 line)))
           (relation-raw (string-trim (match-string 3 line)))
           (text (string-trim (match-string 4 line)))
           (rr (flexiarg--compute-role-and-rel marker relation-raw))
           (role (nth 0 rr))
           (relation (nth 1 rr))
           (kind (nth 2 rr))
           (required? (nth 3 rr)))
      (list indent
            (make-flexiarg-node
             :marker marker
             :relation-raw relation-raw
             :relation relation
             :role role
             :kind kind
             :required? required?
             :text text
             :children nil)))))

;; ------------------------------
;; Build tree from buffer
;; ------------------------------

(defun flexiarg--parse-buffer ()
  "Parse current buffer into (META ROOT-NODES).
META is an alist. ROOT-NODES is a list of flexiarg-node."
  (save-excursion
    (goto-char (point-min))
    (let ((meta '())
          (roots '())
          (stack '())) ;; stack of (indent . node)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ;; Skip empty or comment lines
           ((string-match-p "^\\s-*\\(;.*\\)?$" line)
            nil)
           ;; Meta line
           ((string-match "^\\s-*@" line)
            (let ((m (flexiarg--parse-meta-line line)))
              (when m
                (setq meta (assq-delete-all (car m) meta))
                (push m meta))))
           ;; Node line
           ((string-match "^\\s-*\\([!+?]\\)\\s-*[^:]+:" line)
            (pcase-let* ((`(,indent ,node) (flexiarg--parse-node-line line)))
              ;; unwind stack to find parent
              (while (and stack
                          (>= indent (caar stack))
                          ;; if same indent, pop last so siblings attach to same parent level
                          )
                (setq stack (cdr stack)))
              (cond
               ;; new root
               ((null stack)
                (push (cons indent node) stack)
                (push node roots))
               (t
                (let* ((parent-pair (car stack))
                       (parent (cdr parent-pair)))
                  (setf (flexiarg-node-children parent)
                        (append (flexiarg-node-children parent)
                                (list node)))
                  (push (cons indent node) stack))))))
           (t
            ;; Non-empty, non-meta, non-node line: treat as continuation of previous node text.
            (when stack
              (let* ((cur (cdr (car stack)))
                     (old (flexiarg-node-text cur))
                     (extra (string-trim line)))
                (when (not (string-empty-p extra))
                  (setf (flexiarg-node-text cur)
                        (concat old " " extra))))))))
        (forward-line 1))
      (list (nreverse meta) (nreverse roots)))))

;; ------------------------------
;; Convert tree to EDN-style alists
;; ------------------------------

(defvar flexiarg--id-counter 0)

(defun flexiarg--gen-id ()
  (setq flexiarg--id-counter (1+ flexiarg--id-counter))
  (intern (format ":n%d" flexiarg--id-counter)))

(defun flexiarg--node-to-edn (node)
  "Convert NODE (flexiarg-node) recursively to an EDN-style alist."
  (let* ((role (flexiarg-node-role node))
         (rel  (flexiarg-node-relation node))
         (kind (flexiarg-node-kind node))
         (required? (flexiarg-node-required? node))
         (text (flexiarg-node-text node))
         ;; classify children
         (children (flexiarg-node-children node))
         (support-children
          (cl-remove-if (lambda (ch)
                          (memq (flexiarg-node-role ch) '(:hole :example)))
                        children))
         (hole-children
          (cl-remove-if-not (lambda (ch)
                              (eq (flexiarg-node-role ch) :hole))
                            children))
         (example-children
          (cl-remove-if-not (lambda (ch)
                              (and (not (eq (flexiarg-node-role ch) :hole))
                                   (eq (flexiarg-node-kind ch) :example)))
                            children))
         (m `((:id ,(flexiarg--gen-id))
              (:role ,role)
              (:text ,text))))
    (when (and rel (not (eq role :conclusion)))
      (setq m (append m `((:relation-to-parent ,rel)))))
    (when (and kind (memq role '(:hole :example)))
      (setq m (append m `((:kind ,kind)))))
    (when (eq role :hole)
      (setq m (append m
                      `((:prompt ,text)
                        (:required? ,required?)
                        (:text nil)))))
    (when support-children
      (setq m (append m
                      `((:supports ,(mapcar #'flexiarg--node-to-edn
                                            support-children)))))
    (when (or hole-children example-children)
      (setq m (append m
                      `((:illustrates
                         ,(mapcar #'flexiarg--node-to-edn
                                  (append example-children hole-children)))))))
    m)))

(defun flexiarg--assemble-edn (meta roots)
  "Assemble final EDN-style map from META and ROOTS."
  (setq flexiarg--id-counter 0)
  (let* ((meta-map (mapcar (lambda (kv)
                             (cons (car kv) (cdr kv)))
                           meta))
         (root
          (cond
           ((= (length roots) 1) (car roots))
           (t
            ;; if multiple roots, wrap them under synthetic conclusion
            (make-flexiarg-node
             :marker ?!
             :relation-raw "conclusion"
             :relation nil
             :role :conclusion
             :kind nil
             :required? t
             :text "Top-level argument"
             :children roots))))
         (arg-edn (flexiarg--node-to-edn root))
         (constraints
          (let ((len (alist-get :length meta-map))
                (style (alist-get :style meta-map))
                (banned (alist-get :banned-phrases meta-map))
                (allow-new (alist-get :allow-new-claims? meta-map))
                (max-it (alist-get :max-iterations meta-map)))
            (cl-remove-if
             (lambda (pair) (null (cdr pair)))
             `((:style ,(or style "Clear, concrete, funding-application English"))
               (:word-limit ,len)
               (:banned-phrases ,banned)
               (:allow-new-claims? ,allow-new)
               (:max-iterations ,max-it)
               (:coverage (:all-supports-of ,(plist-get arg-edn :id))))))))
    `((:id ,(or (alist-get :id meta-map) "unnamed/argument"))
      (:title ,(alist-get :title meta-map))
      (:audience ,(alist-get :audience meta-map))
      (:tone ,(alist-get :tone meta-map))
      (:length ,(alist-get :length meta-map))
      (:keywords ,(alist-get :keywords meta-map))
      (:up ,(alist-get :up meta-map))
      (:next ,(alist-get :next meta-map))
      (:argument ,arg-edn)
      (:constraints ,constraints)
      (:actions
       ((:type :generate :target :summary)
        (:type :validate :mode :tree-walk))))))

(defun flexiarg-buffer-to-edn ()
  "Parse current buffer and return EDN-style structure as Lisp data."
  (pcase-let ((`(,meta ,roots) (flexiarg--parse-buffer)))
    (flexiarg--assemble-edn meta roots)))

;; ------------------------------
;; Interactive command
;; ------------------------------

;;;###autoload
(defun flexiarg-show-edn ()
  "Parse current buffer as flexiarg script and show EDN-style output in another buffer."
  (interactive)
  (let* ((edn (flexiarg-buffer-to-edn))
         (buf (get-buffer-create "*flexiarg-edn*")))
    (with-current-buffer buf
      (erase-buffer)
      (emacs-lisp-mode)
      (insert ";; EDN-style argument map generated from flexiarg buffer\n\n")
      (pp edn buf)
      (goto-char (point-min)))
    (display-buffer buf)))

;; ------------------------------
;; Major mode definition
;; ------------------------------

;;;###autoload
(define-derived-mode flexiarg-mode text-mode "FlexiArg"
  "Major mode for flexiformal argument scripts that compile to EDN.

Syntax:

  @key value          Metadata (arg id, title, audience, tone, etc.)
  ! role: text        Conclusion node (root of argument)
  + relation: text    Supporting premise, relation to parent (because, therefore, etc.)
  ? kind(optional):   Hole / example / prompt attached to parent

Indentation defines the tree: child lines are indented more than their parent.

Use \\[flexiarg-show-edn] to see compiled EDN-style structure."
  (flexiarg--ensure-keymap)
  (setq-local comment-start ";; ")
  (setq-local comment-start-skip ";;+\\s-*")
  (setq flexiarg--facet-font-lock-keywords (flexiarg--build-facet-keywords))
  (setq-local font-lock-defaults
              (list (append flexiarg--base-font-lock-keywords
                            flexiarg--facet-font-lock-keywords)))
  (font-lock-refresh-defaults)
  (flexiarg--apply-facet-highlights))

(provide 'flexiarg)
(provide 'flexiarg-mode)

;;; flexiarg-mode.el ends here
