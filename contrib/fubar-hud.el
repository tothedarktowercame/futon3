;;; fubar-hud.el --- HUD display for fulab sessions -*- lexical-binding: t -*-

;; Provides a Heads-Up Display showing pattern context for fulab sessions.
;; Humans see the same patterns/AIF signals that AI agents see.

;;; Commentary:

;; This module displays the fulab HUD - the shared context surface for
;; pattern-aware sessions. It shows:
;; - Current intent and sigils
;; - Pattern candidates with scores
;; - AIF suggestion and confidence (tau)
;; - Agent report (after turn completion)
;;
;; Usage:
;;   (fubar-hud-show)           ; Show HUD in side window
;;   (fubar-hud-refresh)        ; Refresh with current intent
;;   (fubar-hud-set-intent ...) ; Set intent and rebuild HUD

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'url)
(require 'url-http)

(defgroup fubar-hud nil
  "HUD display for fulab pattern-aware sessions."
  :group 'fubar
  :prefix "fubar-hud-")

(defcustom fubar-hud-buffer-name "*FuLab HUD*"
  "Name of the HUD buffer."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-futon3-root
  (expand-file-name "~/code/futon3")
  "Root directory of futon3 repository."
  :type 'directory
  :group 'fubar-hud)

(defcustom fubar-hud-server-url
  "http://localhost:5050"
  "Base URL for the futon3 server HUD endpoint."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-certify-commit t
  "When non-nil, request git commit certificates for HUD output."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-clojure-deps
  "{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"} futon2/futon2 {:local/root \"../futon2\"}}}"
  "Clojure deps for running HUD commands (fallback when server unavailable)."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-window-width 50
  "Width of HUD side window."
  :type 'integer
  :group 'fubar-hud)

(defcustom fubar-hud-pattern-click-action 'fubar-hud-browse-pattern
  "Function to call when a pattern ID is clicked."
  :type 'function
  :group 'fubar-hud)

(defcustom fubar-hud-stream-buffer-name "*FuLab Raw Stream*"
  "Name of the buffer showing the live fucodex stream."
  :type 'string
  :group 'fubar-hud)

(defcustom fubar-hud-staging-file "/tmp/fulab-next.txt"
  "Path for storing the staged next-move text."
  :type 'file
  :group 'fubar-hud)

(defcustom fubar-hud-pause-flag-file "/tmp/fulab-stream-paused"
  "Path for the pause flag file used by the stream formatter."
  :type 'file
  :group 'fubar-hud)

(defcustom fubar-hud-auto-refresh-seconds 2
  "Seconds between HUD auto-refreshes when the HUD buffer is visible."
  :type 'number
  :group 'fubar-hud)

(defcustom fubar-hud-show-score-sources t
  "When non-nil, render the score source legend."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-show-glove-neighbors nil
  "When non-nil, render the GloVe neighbors section."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-show-live-aif t
  "When non-nil, render live AIF updates from the session log."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-async-refresh t
  "When non-nil, refresh HUD via asynchronous HTTP calls."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-fucodex-aif-select t
  "When non-nil, run fucodex with --aif-select."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-approval-policy "untrusted"
  "Approval policy to pass to fucodex runs."
  :type '(choice (const "never")
                 (const "untrusted")
                 (const "on-request")
                 (const "on-failure"))
  :group 'fubar-hud)

(defcustom fubar-hud-intent-from-prompt nil
  "When non-nil, use the prompt as the intent for new fucodex runs."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-auto-layout t
  "When non-nil, show a two-up layout for live runs."
  :type 'boolean
  :group 'fubar-hud)

(defcustom fubar-hud-pause-when-selected t
  "When non-nil, pause auto-refresh while the HUD window is selected."
  :type 'boolean
  :group 'fubar-hud)

;;; State

(defvar-local fubar-hud--current-hud nil
  "Current HUD state as plist.")

(defvar-local fubar-hud--prompt-block nil
  "Cached HUD prompt block.")

(defvar-local fubar-hud--intent nil
  "Current intent string.")

(defvar-local fubar-hud--approval-policy nil
  "Approval policy override for this HUD buffer.")

(defvar-local fubar-hud--refresh-in-flight nil
  "Non-nil when an async HUD refresh is in progress.")


(defvar-local fubar-hud--operator-status nil
  "Operator status line for HUD display.")

(defvar-local fubar-hud--session-id nil
  "Current session ID.")

(defvar fubar-hud--stream-paused nil
  "Non-nil when the stream process is paused.")

(defvar fubar-hud--staged-next nil
  "Staged next-move text, if any.")

(defvar fubar-hud--auto-refresh-timer nil
  "Timer used to auto-refresh the HUD.")

(defvar fubar-hud--auto-refresh-paused nil
  "Non-nil when auto-refresh is paused because the HUD window is selected.")

;;; Faces

(defface fubar-hud-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for HUD section headers."
  :group 'fubar-hud)

(defface fubar-hud-intent-face
  '((t :inherit font-lock-string-face))
  "Face for intent text."
  :group 'fubar-hud)

(defface fubar-hud-sigil-face
  '((t :inherit font-lock-constant-face :height 1.2))
  "Face for sigils."
  :group 'fubar-hud)

(defface fubar-hud-pattern-id-face
  '((t :inherit link))
  "Face for pattern IDs."
  :group 'fubar-hud)

(defface fubar-hud-score-face
  '((t :inherit font-lock-number-face))
  "Face for scores."
  :group 'fubar-hud)

(defface fubar-hud-aif-suggestion-face
  '((t :inherit success :weight bold))
  "Face for AIF suggested pattern."
  :group 'fubar-hud)

(defface fubar-hud-tau-face
  '((t :inherit font-lock-type-face))
  "Face for tau (confidence) value."
  :group 'fubar-hud)

;;; HUD Building (server-first)

(defun fubar-hud--parse-json (text)
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (json-read-from-string text)))

(defun fubar-hud--decode-json-body (body)
  "Decode BODY as UTF-8 if needed."
  (when body
    (decode-coding-string (string-make-unibyte body) 'utf-8)))

(defun fubar-hud--extract-intent-from-prompt (prompt)
  "Extract an Intent block from PROMPT when present."
  (when (and prompt (stringp prompt))
    (let* ((lines (split-string prompt "\n"))
           (start (cl-position-if (lambda (line)
                                    (string-match-p "^\\s-*Intent\\s-*:" line))
                                  lines))
           (intent-lines nil))
      (when start
        (let ((line (nth start lines)))
          (setq intent-lines
                (list (string-trim (replace-regexp-in-string "^\\s-*Intent\\s-*:\\s*" "" line))))))
      (when start
        (let ((idx (1+ start)))
          (while (and (< idx (length lines))
                      (let ((line (nth idx lines)))
                        (not (string-empty-p (string-trim line)))))
            (push (string-trim (nth idx lines)) intent-lines)
            (setq idx (1+ idx))))))
      (when intent-lines
        (string-trim (string-join (nreverse intent-lines) " "))))))

(defvar-local fubar-hud--stream-fragment ""
  "Partial stream fragment awaiting a newline.")

(defun fubar-hud--maybe-update-intent-from-line (line)
  "Update HUD intent from a LINE that declares intent."
  (let ((case-fold-search t))
    (when (string-match "^\\s-*\\(?:\\[intent\\]\\|intent:\\)\\s-*\\(.+\\)$" line)
      (let ((intent (string-trim (match-string 1 line))))
        (when (and intent (not (string-empty-p intent)))
          (let ((hud-buf (get-buffer fubar-hud-buffer-name)))
            (when hud-buf
              (with-current-buffer hud-buf
                (setq fubar-hud--intent intent)
                (fubar-hud-refresh)))))))))

(defun fubar-hud--stream-filter (proc chunk)
  "Insert CHUNK into PROC buffer and parse intent lines."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert chunk)
            (set-marker (process-mark proc) (point)))
          (when moving
            (goto-char (process-mark proc)))))
      (with-current-buffer buf
        (let* ((text (concat fubar-hud--stream-fragment chunk))
               (lines (split-string text "\n"))
               (complete (butlast lines))
               (rest (car (last lines))))
          (setq fubar-hud--stream-fragment rest)
          (dolist (line complete)
            (fubar-hud--maybe-update-intent-from-line line)))))))

(defun fubar-hud--encode-json-request (payload)
  "Encode PAYLOAD as UTF-8 unibyte JSON for url-request-data."
  (string-make-unibyte (encode-coding-string (json-encode payload) 'utf-8)))

(defun fubar-hud--http-post-json (url payload)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("content-type" . "application/json; charset=utf-8")))
         (url-request-data (fubar-hud--encode-json-request payload))
         (coding-system-for-write 'binary)
         (buf (url-retrieve-synchronously url t t 5.0)))
    (when buf
      (with-current-buffer buf
        (unwind-protect
            (when (boundp 'url-http-end-of-headers)
              (goto-char url-http-end-of-headers)
              (let ((body (buffer-substring-no-properties (point) (point-max))))
                (fubar-hud--parse-json (fubar-hud--decode-json-body body))))
          (kill-buffer buf))))))

(defun fubar-hud--http-post-json-async (url payload callback)
  "POST PAYLOAD to URL and call CALLBACK with parsed JSON or nil."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("content-type" . "application/json; charset=utf-8")))
        (url-request-data (fubar-hud--encode-json-request payload))
        (coding-system-for-write 'binary))
    (url-retrieve
     url
     (lambda (status)
       (let (result)
         (unwind-protect
             (unless (plist-get status :error)
               (when (boundp 'url-http-end-of-headers)
                 (goto-char url-http-end-of-headers)
                 (let ((body (buffer-substring-no-properties (point) (point-max))))
                   (when (and body (not (string-empty-p (string-trim body))))
                     (setq result (fubar-hud--parse-json (fubar-hud--decode-json-body body)))))))
           (kill-buffer (current-buffer)))
         (funcall callback result status))))))

(defun fubar-hud--build-hud-server (intent &optional patterns)
  "Build HUD via futon3 server."
  (let* ((prototypes (when (and patterns (not (string-empty-p patterns)))
                       (split-string patterns "," t "[[:space:]]+")))
         (payload `(("intent" . ,(or intent "unspecified"))
                    ("prototypes" . ,prototypes)
                    ("pattern-limit" . 4)
                    ("certify-commit" . ,(and fubar-hud-certify-commit t))
                    ("repo-root" . ,fubar-hud-futon3-root)))
         (payload (if fubar-hud--session-id
                      (append payload `(("session-id" . ,fubar-hud--session-id)))
                    payload))
         (url (concat (replace-regexp-in-string "/$" "" fubar-hud-server-url)
                      "/fulab/hud/format"))
         (result (ignore-errors (fubar-hud--http-post-json url payload))))
    (when (and result (plist-get result :ok))
      (let ((hud (plist-get result :hud))
            (prompt (plist-get result :prompt)))
        (when prompt
          (setq fubar-hud--prompt-block prompt))
        hud))))

(defun fubar-hud--build-hud-server-async (intent patterns callback)
  "Build HUD via futon3 server and invoke CALLBACK with the HUD map or nil."
  (let* ((prototypes (when (and patterns (not (string-empty-p patterns)))
                       (split-string patterns "," t "[[:space:]]+")))
         (payload `(("intent" . ,(or intent "unspecified"))
                    ("prototypes" . ,prototypes)
                    ("pattern-limit" . 4)
                    ("certify-commit" . ,(and fubar-hud-certify-commit t))
                    ("repo-root" . ,fubar-hud-futon3-root)))
         (payload (if fubar-hud--session-id
                      (append payload `(("session-id" . ,fubar-hud--session-id)))
                    payload))
         (url (concat (replace-regexp-in-string "/$" "" fubar-hud-server-url)
                      "/fulab/hud/format")))
    (fubar-hud--http-post-json-async
     url
     payload
     (lambda (result _status)
       (if (and result (plist-get result :ok))
           (let ((hud (plist-get result :hud))
                 (prompt (plist-get result :prompt)))
             (when prompt
               (setq fubar-hud--prompt-block prompt))
             (funcall callback hud))
         (funcall callback nil))))))

(defun fubar-hud--run-clj (command &rest args)
  "Run lab-hud.clj COMMAND with ARGS, return output."
  (let* ((default-directory fubar-hud-futon3-root)
         (cmd (format "clojure -Sdeps '%s' -M dev/lab-hud.clj %s %s"
                      fubar-hud-clojure-deps
                      command
                      (mapconcat #'shell-quote-argument args " "))))
    (shell-command-to-string cmd)))

(defun fubar-hud--build-hud (intent &optional patterns)
  "Build HUD for INTENT, optionally seeding PATTERNS."
  (or (fubar-hud--build-hud-server intent patterns)
      (let* ((args (list "--intent" (or intent "unspecified")))
             (args (if patterns
                       (append args (list "--prototypes" patterns))
                     args))
             (output (apply #'fubar-hud--run-clj "build" args)))
        (condition-case nil
            (car (read-from-string output))
          (error nil)))))

(defun fubar-hud--format-hud (hud)
  "Get formatted prompt block for HUD."
  (or fubar-hud--prompt-block
      (let ((temp-file (make-temp-file "fubar-hud-" nil ".edn")))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (prin1 hud (current-buffer)))
              (fubar-hud--run-clj "format" "--hud-file" temp-file))
          (delete-file temp-file)))))

;;; Rendering

(defun fubar-hud--normalize-sigil-text (value)
  "Normalize sigil VALUE to a displayable string."
  (cond
   ((stringp value) (decode-coding-string value 'utf-8 t))
   (value (format "%s" value))
   (t "")))

(defun fubar-hud--map-get (data key)
  "Fetch KEY from DATA that may be plist or alist."
  (cond
   ((null data) nil)
   ((plist-member data key) (plist-get data key))
   ((listp data) (cdr (assoc key data)))
   (t nil)))

(defun fubar-hud--score-source-label (source)
  "Normalize score SOURCE label for display."
  (let ((label (format "%s" source)))
    (cond
     ((string= label ":glove-embedding") "glove-embedding")
     ((string= label ":sigil-distance") "sigil-distance")
     ((string= label ":combined") "combined")
     ((string= label ":unknown") "unknown")
     (t label))))

(defun fubar-hud--format-metric (value)
  "Format VALUE as a short metric string."
  (if (numberp value)
      (format "%.3f" value)
    "n/a"))

(defun fubar-hud--render-sigils (sigils)
  "Render SIGILS as formatted string."
  (if (and sigils (> (length sigils) 0))
      (mapconcat (lambda (s)
                   (let ((emoji (fubar-hud--normalize-sigil-text (plist-get s :emoji)))
                         (hanzi (fubar-hud--normalize-sigil-text (plist-get s :hanzi))))
                     (propertize (format "%s/%s" emoji hanzi)
                                 'face 'fubar-hud-sigil-face)))
                 sigils " ")
    (propertize "none" 'face 'font-lock-comment-face)))

(defun fubar-hud--make-pattern-button (pattern-id)
  "Create clickable button for PATTERN-ID."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive)
                                 (funcall fubar-hud-pattern-click-action pattern-id)))
    (define-key map (kbd "RET") (lambda () (interactive)
                                   (funcall fubar-hud-pattern-click-action pattern-id)))
    (propertize pattern-id
                'face 'fubar-hud-pattern-id-face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "Click to view %s" pattern-id))))

(defun fubar-hud--render-candidate (candidate index &optional g-score)
  "Render CANDIDATE at INDEX, optionally with G-SCORE."
  (let* ((id (plist-get candidate :id))
         (score (plist-get candidate :score))
         (source (plist-get candidate :score-source))
         (similarity (plist-get candidate :score-similarity))
         (sigil-dist (plist-get candidate :score-sigil))
         (glove-dist (plist-get candidate :score-glove))
         (summary (plist-get candidate :summary))
         (aif-line (when (numberp g-score)
                     (format "   AIF G: %.3f\n" g-score)))
         (source-line (when source
                        (format "   Source: %s\n"
                                (fubar-hud--score-source-label source))))
         (similarity-line (when (numberp similarity)
                            (format "   Similarity: %.3f\n" similarity)))
         (sigil-line (when (numberp sigil-dist)
                       (format "   Sigil dist: %.3f\n" sigil-dist)))
         (glove-line (when (numberp glove-dist)
                       (format "   GloVe dist: %.3f\n" glove-dist))))
    (concat
     (format "%d. " (1+ index))
     (fubar-hud--make-pattern-button id)
     " "
     (propertize (if (numberp score)
                     (format "(dist %.3f)" score)
                   "(score unavailable)")
                 'face 'fubar-hud-score-face)
     "\n"
     (or aif-line "")
     (or source-line "")
     (or sigil-line "")
     (or glove-line "")
     (or similarity-line "")
     (when (and summary (not (string-empty-p summary)))
       (concat "   " (propertize (truncate-string-to-width summary 40 nil nil "...")
                                 'face 'font-lock-doc-face)
               "\n")))))

(defun fubar-hud--render-aif (aif)
  "Render AIF section."
  (if aif
      (let ((suggested (plist-get aif :suggested))
            (tau (plist-get aif :tau))
            (g-scores (plist-get aif :G-scores))
            (g-components (plist-get aif :G-components))
            (candidate-count (plist-get aif :candidate-count))
            (rationale (plist-get aif :rationale)))
        (concat
         (propertize "AIF Suggestion\n" 'face 'fubar-hud-header-face)
         (if suggested
             (concat
              "  Pattern: "
              (propertize suggested 'face 'fubar-hud-aif-suggestion-face)
              "\n"
              (when candidate-count
                (format "  Pool: %d candidates\n" candidate-count))
              (when tau
                (format "  Confidence (τ): %s\n"
                        (propertize (format "%.2f" tau) 'face 'fubar-hud-tau-face)))
              (when-let ((g (and g-scores (cdr (assoc suggested g-scores)))))
                (format "  Expected FE (G): %.3f\n" g))
              (let ((components (and g-components (fubar-hud--map-get g-components suggested))))
                (when components
                  (let ((base (fubar-hud--map-get components :base))
                        (sigil-bonus (fubar-hud--map-get components :sigil-bonus))
                        (sigil-dist (fubar-hud--map-get components :sigil-distance))
                        (intent-dist (fubar-hud--map-get components :intent-distance))
                        (glove-dist (fubar-hud--map-get components :glove-distance))
                        (source (fubar-hud--map-get components :source)))
                    (format (concat "  G components: base %s, sigil %s, intent %s, glove %s, bonus %s\n"
                                    "  Source: %s\n")
                            (fubar-hud--format-metric base)
                            (fubar-hud--format-metric sigil-dist)
                            (fubar-hud--format-metric intent-dist)
                            (fubar-hud--format-metric glove-dist)
                            (fubar-hud--format-metric sigil-bonus)
                            (fubar-hud--score-source-label source)))))
              (when rationale
                (format "  Rationale: %s\n" rationale)))
           "  No suggestion\n")
         "  (tau: precision of the selector; higher is more confident)\n"
         "  (see docs/aif-pattern-engine.md for computation details)\n"))
    ""))

(defun fubar-hud--render-live-aif (aif-live)
  "Render live AIF updates from session log."
  (when aif-live
    (let* ((summary (fubar-hud--map-get aif-live :summary))
           (last-action (fubar-hud--map-get aif-live :last-action))
           (last-selection (fubar-hud--map-get aif-live :last-selection))
           (last-use (fubar-hud--map-get aif-live :last-use))
           (kind (and summary (fubar-hud--map-get summary :kind)))
           (chosen (and summary (fubar-hud--map-get summary :chosen)))
           (tau (and summary (fubar-hud--map-get summary :tau)))
           (g-chosen (and summary (fubar-hud--map-get summary :g-chosen)))
           (evidence-score (and summary (fubar-hud--map-get summary :evidence-score)))
           (evidence-delta (and summary (fubar-hud--map-get summary :evidence-delta)))
           (evidence-counts (and summary (fubar-hud--map-get summary :evidence-counts)))
           (prediction-error (and summary (fubar-hud--map-get summary :prediction-error)))
           (action (and last-action (fubar-hud--map-get last-action :action)))
           (pattern-id (and last-action (fubar-hud--map-get last-action :pattern-id)))
           (note (and last-action (fubar-hud--map-get last-action :note)))
           (sel-chosen (and last-selection (fubar-hud--map-get last-selection :chosen)))
           (sel-candidates (and last-selection (fubar-hud--map-get last-selection :candidates)))
           (use-pattern (and last-use (fubar-hud--map-get last-use :pattern-id))))
      (concat
       (propertize "Live AIF\n" 'face 'fubar-hud-header-face)
       (when summary
         (format "  Summary: %s%s%s%s\n"
                 (or kind "unknown")
                 (if chosen (format " → %s" chosen) "")
                 (if (numberp tau) (format " (τ %.2f)" tau) "")
                 (if (numberp g-chosen) (format " G=%.3f" g-chosen) "")))
       (when (numberp prediction-error)
         (format "  Prediction error: %.3f\n" prediction-error))
       (when (or (numberp evidence-score) evidence-counts)
         (format "  Evidence: %s%s%s\n"
                 (if (numberp evidence-score)
                     (format "%.3f" evidence-score)
                   "n/a")
                 (if (numberp evidence-delta)
                     (format " (Δ %.3f)" evidence-delta)
                   "")
                 (if evidence-counts
                     (format " %s" evidence-counts)
                   "")))
       (when last-selection
         (format "  Last selection: %s%s\n"
                 (or sel-chosen "unknown")
                 (if (and sel-candidates (sequencep sel-candidates))
                     (format " (candidates %d)" (length sel-candidates))
                   "")))
       (when last-use
         (format "  Last use: %s\n" (or use-pattern "unknown")))
       (when last-action
         (format "  Last action: %s %s%s\n"
                 (or action "action")
                 (or pattern-id "unknown")
                 (if (and note (not (string-empty-p note)))
                     (format " - %s" note)
                   "")))
       "\n"))))

(defun fubar-hud--render-score-sources (hud)
  "Render score attribution notes."
  (let ((aif (plist-get hud :aif)))
    (concat
     (propertize "Score Sources\n" 'face 'fubar-hud-header-face)
     "  Candidate scores: combined distance (0..1); lower is closer\n"
     "  Sigil dist: sigil-embedding distance (0..1)\n"
     "  GloVe dist: glove-embedding distance (0..1)\n"
     "  Intent dist: intent-to-pattern embedding distance (0..1)\n"
     "  Similarity: glove similarity (0..1)\n"
     "  Score missing: no sigil/embedding score; AIF defaults base to 0.5\n"
     "  Score 0.000 can mean exact match or rounding; check source/similarity\n"
     (if aif
         "  AIF scores: G = weighted distance (sigil + intent + glove) + sigil bonus\n"
       "  AIF scores: unavailable (no adapter output)\n"))))

(defun fubar-hud--render-glove-candidates (candidates)
  "Render GloVe neighbor candidates."
  (when (and candidates (> (length candidates) 0))
    (concat
     (propertize "GloVe Neighbors\n" 'face 'fubar-hud-header-face)
     (mapconcat
      (lambda (c)
        (let ((id (plist-get c :id))
              (dist (plist-get c :score-glove))
              (sim (plist-get c :score-similarity)))
          (format "  %s (dist %.3f, sim %.3f)\n"
                  id
                  (or dist 0.0)
                  (or sim 0.0))))
      candidates
      "")
     "\n")))

(defun fubar-hud--render-agent-report (report)
  "Render agent REPORT section."
  (if report
      (concat
       (propertize "Agent Report\n" 'face 'fubar-hud-header-face)
       (if-let ((applied (plist-get report :applied)))
           (concat
            "  Applied: "
            (fubar-hud--make-pattern-button applied)
            "\n"
            (when-let ((notes (plist-get report :notes)))
              (format "  Notes: %s\n" notes)))
         "  No pattern reported\n"))
    ""))

(defun fubar-hud--current-approval-policy ()
  "Return the active approval policy for this HUD buffer."
  (let ((policy (or fubar-hud--approval-policy fubar-hud-approval-policy)))
    (if (and (stringp policy) (not (string-empty-p policy)))
        policy
      "untrusted")))

(defun fubar-hud-toggle-approval-policy ()
  "Toggle approval policy between \"never\" and \"untrusted\"."
  (interactive)
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      (fubar-hud--ensure-mode)
      (let* ((current (fubar-hud--current-approval-policy))
             (next (if (string= current "never") "untrusted" "never")))
        (setq fubar-hud--approval-policy next)
        (fubar-hud-refresh)
        (message "Approval policy: %s" next)))))

(defun fubar-hud--render (hud)
  "Render HUD to current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "━━━ FuLab HUD ━━━\n\n" 'face 'fubar-hud-header-face))

    ;; Operator status
    (when fubar-hud--operator-status
      (insert (propertize "Operator Status\n" 'face 'fubar-hud-header-face))
      (insert "  " fubar-hud--operator-status "\n\n"))

    ;; Session
    (when fubar-hud--session-id
      (insert (propertize "Session\n" 'face 'fubar-hud-header-face))
      (insert "  " (propertize fubar-hud--session-id 'face 'font-lock-constant-face) "\n\n"))

    ;; Intent
    (insert (propertize "Intent\n" 'face 'fubar-hud-header-face))
    (insert "  " (propertize (or (plist-get hud :intent) "unspecified")
                              'face 'fubar-hud-intent-face)
            "\n\n")

    ;; Approvals
    (insert (propertize "Approvals\n" 'face 'fubar-hud-header-face))
    (let* ((policy (fubar-hud--current-approval-policy))
           (enabled (not (string= policy "never")))
           (label (if enabled "Disable" "Enable")))
      (insert (format "  Policy: %s " policy))
      (insert-text-button label
                          'help-echo "Toggle tool approval policy for fucodex runs"
                          'action (lambda (_event)
                                    (fubar-hud-toggle-approval-policy))
                          'follow-link t)
      (insert "\n\n"))

    ;; Sigils
    (insert (propertize "Sigils\n" 'face 'fubar-hud-header-face))
    (insert "  " (fubar-hud--render-sigils (plist-get hud :sigils)) "\n\n")

    ;; Candidates
    (insert (propertize "Pattern Candidates\n" 'face 'fubar-hud-header-face))
    (let* ((candidates (plist-get hud :candidates))
           (g-scores (plist-get (plist-get hud :aif) :G-scores)))
      (if (and candidates (> (length candidates) 0))
          (cl-loop for c in candidates
                   for i from 0
                   do (insert (fubar-hud--render-candidate
                               c
                               i
                               (when g-scores
                                 (cdr (assoc (plist-get c :id) g-scores))))))
        (insert "  " (propertize "No candidates found" 'face 'font-lock-comment-face) "\n")))
    (insert "\n")

    ;; GloVe neighbors
    (when (and fubar-hud-show-glove-neighbors
               (plist-get hud :glove-candidates))
      (insert (fubar-hud--render-glove-candidates
               (plist-get hud :glove-candidates))))

    ;; AIF
    (insert (fubar-hud--render-aif (plist-get hud :aif)))
    (when (and fubar-hud-show-live-aif (plist-get hud :aif-live))
      (insert (fubar-hud--render-live-aif (plist-get hud :aif-live))))
    (insert "\n")

    (when fubar-hud-show-score-sources
      (insert (fubar-hud--render-score-sources hud))
      (insert "\n"))

    ;; Staged Next Move
    (when fubar-hud--staged-next
      (let ((label (if (and fubar-hud--last-agent-report
                            (string= (plist-get fubar-hud--last-agent-report :applied) "none"))
                       "Staged (Non-op)\n"
                     "Staged Next\n")))
        (insert (propertize label 'face 'fubar-hud-header-face)))
      (insert "  " (propertize fubar-hud--staged-next 'face 'font-lock-string-face) "\n\n"))

    ;; Agent Report (from HUD or last session)
    (when-let ((report (or (plist-get hud :agent-report)
                           fubar-hud--last-agent-report)))
      (insert (fubar-hud--render-agent-report report)))

    ;; Footer
    (insert "\n" (propertize "━━━━━━━━━━━━━━━━━━\n" 'face 'font-lock-comment-face))
    (insert (propertize (format-time-string "Updated: %H:%M:%S")
                        'face 'font-lock-comment-face))
    (when fubar-hud--auto-refresh-paused
      (insert (propertize "  Auto-refresh: paused"
                          'face 'font-lock-comment-face)))

    (goto-char (point-min))))

;;; Pattern Actions

(defun fubar-hud-browse-pattern (pattern-id)
  "Browse to PATTERN-ID in the library."
  (let* ((parts (split-string pattern-id "/"))
         (namespace (car parts))
         (name (cadr parts))
         (paths (list
                 (expand-file-name (format "library/%s/%s.flexiarg" namespace name)
                                   fubar-hud-futon3-root)
                 (expand-file-name (format "library/%s.flexiarg" pattern-id)
                                   fubar-hud-futon3-root)
                 (expand-file-name (format "holes/LDTS/%s/%s.flexiarg" namespace name)
                                   fubar-hud-futon3-root))))
    (if-let ((found (cl-find-if #'file-exists-p paths)))
        (find-file-other-window found)
      (message "Pattern file not found: %s" pattern-id))))

(defun fubar-hud-copy-pattern-id ()
  "Copy pattern ID at point to kill ring."
  (interactive)
  (when-let ((id (get-text-property (point) 'fubar-hud-pattern-id)))
    (kill-new id)
    (message "Copied: %s" id)))

;;; Interactive Commands

(defun fubar-hud--ensure-mode ()
  "Ensure HUD buffer is in fubar-hud-mode without killing locals."
  (unless (derived-mode-p 'fubar-hud-mode)
    (fubar-hud-mode)))

(defun fubar-hud--show-run-layout (stream-buf)
  "Show STREAM-BUF on the left and the HUD on the right."
  (let ((hud-buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer hud-buf
      (fubar-hud--ensure-mode)
      (fubar-hud-refresh))
    (let ((root (selected-window)))
      (when (window-live-p root)
        (delete-other-windows root)
        (let ((hud-win (split-window root (- (max 30 fubar-hud-window-width)) 'right)))
          (set-window-buffer root stream-buf)
          (set-window-buffer hud-win hud-buf)
          (set-window-dedicated-p hud-win t)
          (select-window root))))
    (fubar-hud--maybe-start-auto-refresh)))

(defun fubar-hud-set-intent (intent)
  "Set INTENT and rebuild HUD."
  (interactive "sIntent: ")
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      (fubar-hud--ensure-mode)
      (setq fubar-hud--intent intent)
      (fubar-hud-refresh))))

(defun fubar-hud-set-session-id (session-id)
  "Set SESSION-ID and rebuild HUD."
  (interactive "sSession ID: ")
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      (fubar-hud--ensure-mode)
      (setq fubar-hud--session-id session-id)
      (fubar-hud-refresh))))

(defun fubar-hud-refresh ()
  "Refresh HUD with current intent."
  (interactive)
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      (setq fubar-hud--prompt-block nil)
      (setq fubar-hud--staged-next (fubar-hud--read-staging-file))
      (if (and fubar-hud-async-refresh fubar-hud-server-url)
          (when (not fubar-hud--refresh-in-flight)
            (setq fubar-hud--refresh-in-flight t)
            (fubar-hud--build-hud-server-async
             fubar-hud--intent
             nil
             (lambda (hud)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq fubar-hud--refresh-in-flight nil)
                   (when hud
                     (setq fubar-hud--current-hud hud)
                     (fubar-hud--render hud)))))))
        (let ((hud (fubar-hud--build-hud fubar-hud--intent)))
          (when hud
            (setq fubar-hud--current-hud hud)
            (fubar-hud--render hud)))))))

(defun fubar-hud-show ()
  "Show HUD in side window."
  (interactive)
  (let ((buf (get-buffer-create fubar-hud-buffer-name)))
    (with-current-buffer buf
      ;; Only set mode if not already set (mode change kills buffer-locals)
      (unless (derived-mode-p 'fubar-hud-mode)
        (fubar-hud-mode))
      (unless fubar-hud--current-hud
        (fubar-hud-refresh)))
    (display-buffer-in-side-window
     buf
     `((side . right)
       (window-width . ,fubar-hud-window-width)
       (slot . 0))))
  (fubar-hud--maybe-start-auto-refresh))

(defun fubar-hud-hide ()
  "Hide HUD window."
  (interactive)
  (when-let ((win (get-buffer-window fubar-hud-buffer-name)))
    (delete-window win))
  (unless (get-buffer-window fubar-hud-buffer-name)
    (fubar-hud--stop-auto-refresh)))

(defun fubar-hud--maybe-start-auto-refresh ()
  "Start auto-refresh timer when HUD buffer is visible."
  (when (and (get-buffer-window fubar-hud-buffer-name)
             (null fubar-hud--auto-refresh-timer)
             (numberp fubar-hud-auto-refresh-seconds))
    (setq fubar-hud--auto-refresh-timer
          (run-at-time 0 fubar-hud-auto-refresh-seconds
                       (lambda ()
                         (let ((hud-win (get-buffer-window fubar-hud-buffer-name)))
                           (when hud-win
                             (let* ((hud-buf (get-buffer fubar-hud-buffer-name))
                                    (pause? (and fubar-hud-pause-when-selected
                                                 (eq (window-buffer (selected-window)) hud-buf)
                                                 (not (and hud-buf
                                                           (buffer-local-value 'fubar-hud--session-id hud-buf))))))
                               (setq fubar-hud--auto-refresh-paused pause?)
                               (unless pause?
                                 (with-current-buffer fubar-hud-buffer-name
                                   (fubar-hud-refresh)))))))))))

(defun fubar-hud--stop-auto-refresh ()
  "Stop HUD auto-refresh timer."
  (when fubar-hud--auto-refresh-timer
    (cancel-timer fubar-hud--auto-refresh-timer)
    (setq fubar-hud--auto-refresh-timer nil)))

(defun fubar-hud-toggle ()
  "Toggle HUD visibility."
  (interactive)
  (if (get-buffer-window fubar-hud-buffer-name)
      (fubar-hud-hide)
      (fubar-hud-show)))

(defun fubar-hud-get-prompt-block ()
  "Get current HUD as prompt block for injection."
  (interactive)
  (if fubar-hud--current-hud
      (let ((block (fubar-hud--format-hud fubar-hud--current-hud)))
        (when (called-interactively-p 'any)
          (kill-new block)
          (message "HUD block copied to kill ring"))
        block)
    (message "No HUD loaded. Use fubar-hud-set-intent first.")
    nil))

(defun fubar-hud--stream-process ()
  "Return the process for the stream buffer, or nil."
  (when-let ((buf (get-buffer fubar-hud-stream-buffer-name)))
    (get-buffer-process buf)))

(defun fubar-hud-toggle-stream-pause ()
  "Pause or resume the stream process."
  (interactive)
  (let ((proc (fubar-hud--stream-process)))
    (if (not (process-live-p proc))
        (message "No live stream process for %s" fubar-hud-stream-buffer-name)
      (if fubar-hud--stream-paused
          (progn
            (process-send-signal 'SIGCONT proc)
            (setq fubar-hud--stream-paused nil)
            (when (file-exists-p fubar-hud-pause-flag-file)
              (delete-file fubar-hud-pause-flag-file))
            (message "Stream resumed"))
        (process-send-signal 'SIGSTOP proc)
        (setq fubar-hud--stream-paused t)
        (with-temp-file fubar-hud-pause-flag-file
          (insert "paused\n"))
        (message "Stream paused")))))

(defun fubar-hud--read-staging-file ()
  "Return the staged next-move text if present."
  (when (file-exists-p fubar-hud-staging-file)
    (let ((text (string-trim (with-temp-buffer
                               (insert-file-contents fubar-hud-staging-file)
                               (buffer-string)))))
      (unless (string-empty-p text)
        text))))

(defun fubar-hud-accept-staged ()
  "Run fucodex with the staged next-move text."
  (interactive)
  (if-let ((text (or fubar-hud--staged-next (fubar-hud--read-staging-file))))
      (let* ((default-directory fubar-hud-futon3-root)
             (resume-args (if fubar-hud--session-id
                            (list "--live" "resume" fubar-hud--session-id text)
                            (list "--live" "resume" "--last" text))))
        (apply #'start-process
               "fucodex-resume"
               "*FuLab Resume*"
               "/home/joe/code/futon3/fucodex"
               resume-args)
        (message "Sent staged next-move to fucodex."))
    (message "No staged next-move found.")))

(defvar-local fubar-hud--output-accumulator ""
  "Accumulator for fuclaude output to parse reports.")

(defun fubar-hud--parse-session-output (output)
  "Parse OUTPUT for session ID and FULAB-REPORT.
Returns plist with :session-id and :agent-report."
  (let (session-id agent-report)
    ;; Parse session ID from export message
    (when (string-match "\\[fuclaude\\] Exporting session: \\([a-f0-9-]+\\)" output)
      (setq session-id (match-string 1 output)))
    ;; Parse FULAB-REPORT block
    (when (string-match "\\[FULAB-REPORT\\]\\([^[]*\\)\\[/FULAB-REPORT\\]" output)
      (let ((report-text (match-string 1 output)))
        (setq agent-report
              (list :applied (when (string-match ":applied \"\\([^\"]+\\)\"" report-text)
                               (match-string 1 report-text))
                    :notes (when (string-match ":notes \"\\([^\"]+\\)\"" report-text)
                             (match-string 1 report-text))))))
    (list :session-id session-id :agent-report agent-report)))

(defun fubar-hud--process-sentinel (proc event)
  "Handle process completion EVENT."
  (when (string-match-p "finished\\|exited" event)
    (let* ((buf (process-buffer proc))
           (output (when buf
                     (with-current-buffer buf
                       (buffer-substring-no-properties (point-min) (point-max)))))
           (parsed (fubar-hud--parse-session-output (or output "")))
           (session-id (plist-get parsed :session-id))
           (agent-report (plist-get parsed :agent-report))
           (hud-buf (get-buffer fubar-hud-buffer-name))
           (label (or (process-get proc 'fubar-hud-label) "run")))
      ;; Update state in HUD buffer context
      (when hud-buf
        (with-current-buffer hud-buf
          (when session-id
            (setq fubar-hud--session-id session-id))
          (when agent-report
            (setq fubar-hud--last-agent-report agent-report))))
      ;; Also set globally for non-buffer-local access
      (when agent-report
        (setq fubar-hud--last-agent-report agent-report))
      (fubar-hud-refresh)
      (message "%s %s%s"
               label
               (string-trim event)
               (if session-id (format " [%s]" (substring session-id 0 8)) "")))))

(defvar fubar-hud--last-agent-report nil
  "Last agent report from fuclaude session.")

(defun fubar-hud--generate-session-id ()
  "Generate a session id for live runs."
  (format "codex-%s"
          (substring (md5 (format "%s-%s" (float-time) (random))) 0 8)))

(defun fubar-hud-run-fuclaude (prompt)
  "Run fuclaude with PROMPT and current HUD intent.
Output goes to *FuLab Raw Stream* buffer. On completion, parses
session ID and FULAB-REPORT to update the HUD."
  (interactive "sPrompt: ")
  (let* ((default-directory fubar-hud-futon3-root)
         (hud-buf (get-buffer fubar-hud-buffer-name))
         (intent (or (when hud-buf
                       (buffer-local-value 'fubar-hud--intent hud-buf))
                     "coding task"))
         (buf (get-buffer-create fubar-hud-stream-buffer-name))
         ;; Escape prompt for shell
         (escaped-prompt (shell-quote-argument prompt))
         (escaped-intent (shell-quote-argument intent))
         (cmd (format "HUD_SERVER=http://localhost:5050 %s/fuclaude --hud --intent %s -p %s 2>&1"
                      fubar-hud-futon3-root escaped-intent escaped-prompt))
         proc)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n━━━ fuclaude: %s ━━━\n"
                      (substring prompt 0 (min 60 (length prompt))))))
    (display-buffer buf)
    (setq proc (start-process-shell-command "fuclaude" buf cmd))
    (process-put proc 'fubar-hud-label "fuclaude")
    (set-process-sentinel proc #'fubar-hud--process-sentinel)
    (message "Started fuclaude with intent: %s" intent)))

(defun fubar-hud-run-fucodex (prompt &optional intent)
  "Run fucodex with PROMPT and current HUD intent.
Starts a live run, binds a new session id, and streams output to
*FuLab Raw Stream*."
  (interactive
   (let ((prompt (read-string "Prompt: ")))
     (if current-prefix-arg
         (list prompt (read-string "Intent (blank = none): "))
       (list prompt nil))))
  (let* ((default-directory fubar-hud-futon3-root)
         (hud-buf (get-buffer-create fubar-hud-buffer-name))
         (approval-policy (when hud-buf
                            (buffer-local-value 'fubar-hud--approval-policy hud-buf)))
         (approval-policy (if (and approval-policy (not (string-empty-p approval-policy)))
                              approval-policy
                            fubar-hud-approval-policy))
         (intent (let ((trimmed (and intent (string-trim intent))))
                   (cond
                    ((and trimmed (not (string-empty-p trimmed))) trimmed)
                    ((and fubar-hud-intent-from-prompt prompt)
                     (let* ((extracted (fubar-hud--extract-intent-from-prompt prompt))
                            (candidate (or extracted prompt)))
                       (when (and candidate
                                  (not (string-empty-p (string-trim candidate))))
                         (string-trim candidate))))
                    :else nil)))
         (session-id (fubar-hud--generate-session-id))
         (buf (get-buffer-create fubar-hud-stream-buffer-name))
         (args (append (list "--live" "--hud" "--session-id" session-id)
                       (when intent (list "--intent" intent))
                       (when (and approval-policy (not (string-empty-p approval-policy)))
                         (list "--approval-policy" approval-policy))
                       (when fubar-hud-fucodex-aif-select (list "--aif-select"))
                       (list prompt)))
         (process-environment (cons (format "HUD_SERVER=%s" fubar-hud-server-url)
                                    process-environment))
         proc)
    (when hud-buf
      (with-current-buffer hud-buf
        (fubar-hud--ensure-mode)
        (setq fubar-hud--intent intent)
        (setq fubar-hud--session-id session-id)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n━━━ fucodex: %s ━━━\n"
                      (substring prompt 0 (min 60 (length prompt))))))
    (if fubar-hud-auto-layout
        (fubar-hud--show-run-layout buf)
      (progn
        (display-buffer buf)
        (fubar-hud-show)))
    (setq proc (apply #'start-process
                      "fucodex"
                      buf
                      "/home/joe/code/futon3/fucodex"
                      args))
    (set-process-filter proc #'fubar-hud--stream-filter)
    (process-put proc 'fubar-hud-label "fucodex")
    (set-process-sentinel proc #'fubar-hud--process-sentinel)
    (fubar-hud-refresh)
    (message "Started fucodex [%s]." session-id)))

(defun fubar-hud-resume-session (prompt &optional session-id)
  "Resume SESSION-ID (or current session) with PROMPT.
Output goes to *FuLab Raw Stream* buffer."
  (interactive
   (let* ((default-id (when (get-buffer fubar-hud-buffer-name)
                        (buffer-local-value 'fubar-hud--session-id
                                            (get-buffer fubar-hud-buffer-name))))
          (id (read-string (format "Session ID%s: "
                                   (if default-id
                                       (format " [%s]" (substring default-id 0 8))
                                     ""))
                           nil nil default-id))
          (prompt (read-string "Prompt: ")))
     (list prompt id)))
  (let* ((default-directory fubar-hud-futon3-root)
         (sid (or session-id
                  (when (get-buffer fubar-hud-buffer-name)
                    (buffer-local-value 'fubar-hud--session-id
                                        (get-buffer fubar-hud-buffer-name)))))
         (sid (and sid (not (string-empty-p sid)) sid))
         (buf (get-buffer-create fubar-hud-stream-buffer-name))
         (escaped-prompt (shell-quote-argument prompt))
         (escaped-sid (and sid (shell-quote-argument sid)))
         (sid-label (if (and sid (>= (length sid) 8))
                        (substring sid 0 8)
                      (or sid "last")))
         (cmd (if sid
                  (format "HUD_SERVER=http://localhost:5050 %s/fuclaude --resume %s -p %s 2>&1"
                          fubar-hud-futon3-root escaped-sid escaped-prompt)
                (format "HUD_SERVER=http://localhost:5050 %s/fuclaude --continue -p %s 2>&1"
                        fubar-hud-futon3-root escaped-prompt)))
         proc)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n━━━ resume %s: %s ━━━\n"
                      sid-label
                      (substring prompt 0 (min 50 (length prompt))))))
    (display-buffer buf)
    (setq proc (start-process-shell-command "fuclaude-resume" buf cmd))
    (process-put proc 'fubar-hud-label "fuclaude-resume")
    (set-process-sentinel proc #'fubar-hud--process-sentinel)
    (message "Resuming session: %s" (or sid "last"))))

;;; Mode Definition

(defvar fubar-hud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'fubar-hud-refresh)
    (define-key map "i" #'fubar-hud-set-intent)
    (define-key map "q" #'fubar-hud-hide)
    (define-key map "w" #'fubar-hud-copy-pattern-id)
    (define-key map "p" #'fubar-hud-toggle-stream-pause)
    (define-key map "a" #'fubar-hud-accept-staged)
    (define-key map "y" #'fubar-hud-get-prompt-block)
    (define-key map "r" #'fubar-hud-run-fuclaude)
    (define-key map "c" #'fubar-hud-run-fucodex)
    (define-key map "R" #'fubar-hud-resume-session)
    map)
  "Keymap for `fubar-hud-mode'.")

(define-derived-mode fubar-hud-mode special-mode "FuLab-HUD"
  "Major mode for FuLab HUD display.

\\{fubar-hud-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; Integration Helpers

(defun fubar-hud-for-buffer (&optional buffer)
  "Build HUD for BUFFER (defaults to current) based on file context."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (file (buffer-file-name buf))
         (intent (cond
                  ((and file (string-match-p "test" file))
                   "writing tests")
                  ((and file (string-match-p "schema" file))
                   "defining schema")
                  ((and file (string-match-p "\\.clj$" file))
                   "Clojure development")
                  (t "coding task"))))
    (fubar-hud-set-intent intent)
    (fubar-hud-show)))

(defun fubar-hud-inject-into-prompt (prompt)
  "Return PROMPT with HUD block prepended."
  (if-let ((block (fubar-hud-get-prompt-block)))
      (concat block "\n\nUser task: " prompt)
    prompt))

(provide 'fubar-hud)
;;; fubar-hud.el ends here
