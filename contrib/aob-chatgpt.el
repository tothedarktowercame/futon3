;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'url)
(require 'button)
(require 'org)
(require 'org-id nil t)
(require 'org-element)
(require 'tabulated-list)
(add-to-list 'load-path "~/code/futon3/contrib/")
(require 'futon3-bridge)
(require 'futon3-embedding)
(require 'futon3-hud)
(require 'futon3-arxana-compat)
(require 'futon3-sessions)
(let ((futon-hot "/home/joe/code/futon0/contrib/futon-hot.el")
      (futon-config "/home/joe/code/futon0/contrib/futon-config.el"))
  (when (file-readable-p futon-hot)
    (load-file futon-hot))
  (when (file-readable-p futon-config)
    (load-file futon-config)))

(defvar my-futon-prompt-directory
  (let* ((base (or load-file-name (buffer-file-name) default-directory))
         (root (expand-file-name ".." (file-name-directory base))))
    (expand-file-name "contrib/" root))
  "Directory where complex system prompts are stored as text files.")

(defun my-futon-read-prompt (name)
  "Read a prompt file NAME from `my-futon-prompt-directory', or nil if missing.
NAME should be a bare name like \"par-shell\"; \".prompt\" is added automatically."
  (let* ((file (expand-file-name (concat name ".prompt")
                                 my-futon-prompt-directory)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

;;; ChatGPT profiles

(defvar-local my-chatgpt-shell-profile "General"
  "Name of the current futon prompt profile for this chatgpt-shell buffer.
Used to look up a system prompt in `my-futon-prompt-directory'.")

(defun my-chatgpt-shell-set-profile (profile)
  "Set `my-chatgpt-shell-profile' in the current buffer."
  (setq my-chatgpt-shell-profile profile
        my-chatgpt-shell--tatami-disabled nil)
  (force-mode-line-update))

(defun futon-set-general-profile ()
  "Restore the chat buffer to the General futon profile."
  (interactive)
  (my-chatgpt-shell-set-profile "General")
  (message "Futon profile set to General"))

(defun my-chatgpt-shell--insert-prompt (name &optional profile temporary)
  "Insert prompt NAME from `my-futon-read-prompt'.
When PROFILE is non-nil, switch the current buffer to that profile first.
If TEMPORARY is non-nil, skip Tatami ingestion for the next turn."
  (unless (derived-mode-p 'chatgpt-shell-mode)
    (user-error "Prompt insertion only works inside chatgpt-shell buffers"))
  (let ((body (my-futon-read-prompt name)))
    (unless body
      (user-error "Prompt %s.prompt was not found" name))
    (when profile
      (my-chatgpt-shell-set-profile profile))
    (when temporary
      (setq my-chatgpt-shell--tatami-disabled t))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert body)
    (unless (string-suffix-p "\n" body)
      (insert "\n"))
    (message "Inserted %s prompt" name)))



;;; Classical embedding support ---------------------------------------------



(defgroup futon0-clock-out nil
  "Clock-out automation and focus review settings."
  :group 'applications
  :prefix "my-chatgpt-shell-")

(defcustom my-chatgpt-shell-focus-files nil
  "Optional list of Org files scanned for `FOCUS' TODO headings during clock-out.
When nil, fall back to `org-agenda-files`."
  :type '(repeat file)
  :group 'futon0-clock-out)

(defcustom my-chatgpt-shell-focus-limit 5
  "Maximum number of focus headings to include in each automated clock-out prompt."
  :type 'integer
  :group 'futon0-clock-out)

(defcustom my-chatgpt-shell-focus-body-max 180
  "Maximum number of body characters copied from each focus heading into prompts."
  :type 'integer
  :group 'futon0-clock-out)

(defcustom my-chatgpt-shell-focus-auto-open-review t
  "When non-nil, automatically show the focus mini-game once decisions arrive."
  :type 'boolean
  :group 'futon0-clock-out)

(defvar my-chatgpt-shell-intent-sigil-limit 1
  "Maximum number of intent sigil pairs to retain/display in the HUD.")

;;; Futon3 (MUSN) orchestration ------------------------------------------------


(defvar my-chatgpt-shell--last-hints-error nil
  "Most recent `/musn/hints` failure message, or nil when the call succeeded.")
(defvar my-chatgpt-shell--last-cues-error nil
  "Most recent `/musn/cues` failure message, or nil when the call succeeded.")
(defcustom my-chatgpt-shell-hints-async t
  "When non-nil, fetch Tatami hints asynchronously to avoid blocking the UI."
  :type 'boolean
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-hints-async-skip-intent nil
  "When non-nil, omit intent from async hints (faster, less precise)."
  :type 'boolean
  :group 'tatami-integration)
(defvar my-chatgpt-shell--hints-request-id 0)
(defvar my-chatgpt-shell--hints-pending nil)
(defvar my-chatgpt-shell--last-intent-sigil-origin nil
  "How the most recent intent sigils were produced (:embedded, :pattern, prototype, etc.).")
(defvar my-chatgpt-shell--last-sigil-salient nil
  "Metadata describing where the current intent sigils came from (inbound, FROM-CHATGPT, or defaults).")
(defvar my-chatgpt-shell--last-cue-salient nil
  "Metadata describing which structure supplied the Tatami cues block (inbound hints or fallback).")
(defvar my-chatgpt-shell--last-replay-chatgpt-edn nil
  "Copy of the most recent FROM-CHATGPT-EDN payload for replay.")
(defvar my-chatgpt-shell--last-replay-tatami-edn nil
  "Copy of the most recent FROM-TATAMI-EDN payload for replay.")


;;; Set up chatgpt-shell

(require 'chatgpt-shell)
(require 'subr-x)
(require 'pp)
(require 'json)
(require 'futon-hud-windows)
(require 'stack-hud)
(require 'stack-doc)
(require 'stack-render)

(setq chatgpt-shell-model-version "gpt-5.2-chat-latest")
(setq chatgpt-shell-streaming t)

(defun my-chatgpt-shell--read-openai-key ()
  (let ((path (expand-file-name "~/.openai-key")))
    (when (file-readable-p path)
      (string-trim
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))))

(unless (getenv "GEMINI_API_KEY")
  (setenv "GEMINI_API_KEY"
          (string-trim
           (with-temp-buffer
             (insert-file-contents "~/.gemini-key")
             (buffer-string)))))

(setq chatgpt-shell-openai-key #'my-chatgpt-shell--read-openai-key)

(defvar-local my-chatgpt-shell--seeded-context nil
  "Non-nil after this chatgpt-shell buffer has sent the initial summary.")
(defvar-local my-chatgpt-shell-last-summary nil
  "Latest Tatami :me summary captured for this buffer.")
(defvar-local my-chatgpt-shell-last-focus nil
  "Latest Tatami focus header snippet captured for this buffer.")
(defvar-local my-chatgpt-shell--tatami-disabled nil
  "When non-nil, skip Tatami ingestion for the next message only.")
(defconst my-chatgpt-shell-context-buffer-name "*Tatami Context*")
(defvar my-chatgpt-shell-context-font-scale 0.5
  "Relative font scale for the Tatami Context HUD (e.g., 0.5 = 50% size).")
(defvar my-chatgpt-shell--hi-from-codex t)
(defvar my-chatgpt-shell-pattern-link-face 'default
  "Face used for pattern links in the Tatami Context HUD.")
(defvar my-chatgpt-shell-pattern-link-underline t
  "When non-nil, underline pattern links in the Tatami Context HUD.")
(defvar my-chatgpt-shell-pattern-anchor-face 'link
  "Face used for pattern id anchors in the Tatami Context HUD.")
(defvar my-chatgpt-shell-pattern-anchor-underline t
  "When non-nil, underline pattern id anchors in the Tatami Context HUD.")
(defconst my-chatgpt-shell-tatami-in-marker "FROM-TATAMI-EDN")
(defconst my-chatgpt-shell-tatami-out-marker "FROM-CHATGPT-EDN")
(defvar-local my-chatgpt-shell-last-inbound-edn nil
  "Most recent FROM-TATAMI-EDN payload prepared for ChatGPT.")
(defvar-local my-chatgpt-shell-last-edn nil
  "Most recent FROM-CHATGPT-EDN payload captured from ChatGPT.")
(defvar-local my-chatgpt-shell--context-face-cookie nil
  "Face remap cookie for the Tatami HUD buffer.")
(defvar-local my-chatgpt-shell-last-pattern-outcome nil
  "Plist describing whether the latest FROM-CHATGPT-EDN applied a pattern.")
(defvar-local my-chatgpt-shell-last-validation-warning nil
  "Latest warning emitted when FROM-CHATGPT-EDN misses required pattern events.")
(defvar my-chatgpt-shell--context-source nil
  "Most recent chatgpt-shell buffer that populated the Tatami HUD.")
(defvar my-chatgpt-shell--hud-state-registry (make-hash-table :test 'eq)
  "Registry of chat buffers → last HUD snapshot plists.")
(defvar my-chatgpt-shell-log-hud-manifest t
  "When non-nil, append each HUD manifest to `my-futon3-server-buffer'.")
(defvar my-chatgpt-shell-sync-futon1-manifest nil
  "When non-nil, POST HUD manifests to the Futon1 API.")
(defvar my-chatgpt-shell-log-pattern-clicks nil
  "When non-nil, log pattern clicks into Futon1 as scholia.")
(defvar my-chatgpt-shell-futon1-endpoint "http://localhost:8080/api/alpha"
  "Base URL for Futon1 API endpoints consumed by the HUD sync.")
(defvar my-chatgpt-shell-request-timeout 5
  "Seconds to wait for Futon1 HTTP requests before giving up.")
(defvar my-chatgpt-shell-edn-log-file
  (expand-file-name "resources/tatami-context.edn" my-futon3--repo-root)
  "When non-nil, append FROM-CHATGPT-EDN payloads to this EDN log.
Set to nil to disable persistence entirely.")
(defvar my-chatgpt-shell-hud-fixture-file
  (expand-file-name "resources/hud-fixtures.edn" my-futon3--repo-root)
  "Optional log of captured Tatami HUD fixtures for regression tests.")
(defvar my-chatgpt-shell-hints-log-file
  (expand-file-name "resources/hints-log.edn" my-futon3--repo-root)
  "Append-only log of /musn/hints requests and responses for debugging.")
(defvar my-chatgpt-shell-debug nil
  "When non-nil, emit debug messages for Tatami prompts/after-hook.")
(defvar-local my-chatgpt-shell--context-face-cookie nil
  "Face remap cookie for the Tatami HUD buffer.")

(defun my-chatgpt-shell--init-context ()
  (setq my-chatgpt-shell--seeded-context nil)
  (my-chatgpt-shell-set-profile "General")
  (add-hook 'kill-buffer-hook #'my-chatgpt-shell--release-hud-state nil t))

(defun my-chatgpt-shell--release-hud-state ()
  "Drop the HUD registry entry for the current buffer when it dies."
  (remhash (current-buffer) my-chatgpt-shell--hud-state-registry)
  (when (eq my-chatgpt-shell--context-source (current-buffer))
    (setq my-chatgpt-shell--context-source nil)))

(defun my-chatgpt-shell--state-buffer ()
  "Return the chat buffer that owns the current Tatami HUD state."
  (let* ((candidate (cond
                     ((derived-mode-p 'chatgpt-shell-mode) (current-buffer))
                     ((buffer-live-p my-chatgpt-shell--context-source)
                      my-chatgpt-shell--context-source)
                     (t nil)))
         (resolved (and candidate (buffer-live-p candidate) candidate)))
    (or resolved
        (let (fallback)
          (maphash (lambda (buffer _)
                     (when (and (null fallback) (buffer-live-p buffer))
                       (setq fallback buffer)))
                   my-chatgpt-shell--hud-state-registry)
          fallback))))

(defun my-chatgpt-shell--ensure-state-buffer ()
  "Return the chat buffer supplying HUD state or signal when missing."
  (or (my-chatgpt-shell--state-buffer)
      (user-error "Tatami HUD has no backing chatgpt-shell buffer.")))

(defmacro my-chatgpt-shell--with-state-buffer (&rest body)
  "Evaluate BODY inside the buffer that owns the Tatami HUD state."
  (declare (indent 0) (debug t))
  `(let ((buf (my-chatgpt-shell--ensure-state-buffer)))
     (with-current-buffer buf
       ,@body)))

(defun my-chatgpt-shell--hud-copy (value)
  "Return VALUE or a shallow copy when VALUE is a plist/vector."
  (cond
   ((consp value) (cl-copy-list value))
   ((vectorp value) (cl-copy-seq value))
   (t value)))

(defun my-chatgpt-shell--record-hud-state (&optional buffer)
  "Remember BUFFER's latest inbound/chatgpt state for HUD mirroring."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (puthash buf
               (list :inbound (my-chatgpt-shell--hud-copy my-chatgpt-shell-last-inbound-edn)
                     :chatgpt (my-chatgpt-shell--hud-copy my-chatgpt-shell-last-edn)
                     :summary (my-chatgpt-shell--hud-copy my-chatgpt-shell-last-summary)
                     :focus (my-chatgpt-shell--hud-copy my-chatgpt-shell-last-focus)
                     :pattern (my-chatgpt-shell--hud-copy my-chatgpt-shell-last-pattern-outcome)
                     :timestamp (float-time))
               my-chatgpt-shell--hud-state-registry))))

(defun my-chatgpt-shell--hud-snapshot (&optional buffer)
  "Return the cached HUD snapshot for BUFFER when available."
  (let ((buf (or buffer (my-chatgpt-shell--state-buffer))))
    (when buf
      (gethash buf my-chatgpt-shell--hud-state-registry))))

(defun my-chatgpt-shell--manifest-entry-list (items &rest fields)
  (let ((seq (my-chatgpt-shell--coerce-seq items)))
    (delq nil
          (mapcar (lambda (entry)
                    (when (and (listp entry)
                               (not (my-chatgpt-shell--json-null-p entry)))
                      (let (result)
                        (dolist (field fields)
                          (setq result (plist-put result field (plist-get entry field))))
                        result)))
                  seq))))

(defun my-chatgpt-shell--manifest-patterns (patterns)
  (let ((seq (my-chatgpt-shell--coerce-seq patterns)))
    (delq nil
          (mapcar (lambda (pattern)
                    (when (and (listp pattern)
                               (not (my-chatgpt-shell--json-null-p pattern)))
                      (list :id (plist-get pattern :id)
                            :score (plist-get pattern :score))))
                  seq))))

(defun my-chatgpt-shell--manifest-sigils (sigils)
  (let ((seq (my-chatgpt-shell--coerce-seq sigils)))
    (delq nil
          (mapcar (lambda (sigil)
                    (when (and (listp sigil)
                               (not (my-chatgpt-shell--json-null-p sigil)))
                      (list :emoji (plist-get sigil :emoji)
                            :hanzi (plist-get sigil :hanzi))))
                  seq))))

(defun my-chatgpt-shell--event-qualifier (event fruits paramitas)
  (let ((kind (plist-get event :kind)))
    (cond
     ((and (eq kind :fruit) fruits)
      (let ((match (or (and (plist-get event :fruit/id)
                             (seq-find (lambda (fruit)
                                         (string= (plist-get fruit :fruit/id)
                                                  (plist-get event :fruit/id)))
                                       fruits))
                             (car fruits))))
        (and match (append match nil))))
     ((and (eq kind :paramita) paramitas)
      (let ((match (or (and (plist-get event :paramita/id)
                             (seq-find (lambda (orb)
                                         (string= (plist-get orb :paramita/id)
                                                  (plist-get event :paramita/id)))
                                       paramitas))
                             (car paramitas))))
        (and match (append match nil)))))))

(defun my-chatgpt-shell--decorate-support-events (edn inbound)
  (let ((raw-events (plist-get edn :events)))
    (if (not raw-events)
        edn
      (let* ((events (my-chatgpt-shell--coerce-seq raw-events))
             (fruit-queue (my-chatgpt-shell--coerce-seq (and inbound (plist-get inbound :fruits))))
             (param-queue (my-chatgpt-shell--coerce-seq (and inbound (plist-get inbound :paramitas))))
             (decorated
              (mapcar
               (lambda (event)
                 (if (and (listp event)
                          (not (my-chatgpt-shell--json-null-p event)))
                     (let ((copy (cl-copy-list event)))
                       (pcase (plist-get copy :kind)
                         (:fruit
                          (unless (plist-get copy :fruit/id)
                            (let ((fruit (pop fruit-queue)))
                              (when fruit
                                (setq copy (plist-put copy :fruit/id (plist-get fruit :fruit/id)))
                                (when-let* ((emoji (plist-get fruit :emoji)))
                                  (setq copy (plist-put copy :emoji emoji)))))))
                         (:paramita
                          (unless (plist-get copy :paramita/id)
                            (let ((orb (pop param-queue)))
                              (when orb
                                (setq copy (plist-put copy :paramita/id (plist-get orb :paramita/id)))
                                (when-let* ((emoji (plist-get orb :emoji)))
                                  (setq copy (plist-put copy :emoji emoji))))))))
                       copy)
                   event))
               events))
             (result (if (vectorp raw-events)
                         (apply #'vector decorated)
                       decorated))
             (copy (cl-copy-list edn)))
        (plist-put copy :events result)))))

(defun my-chatgpt-shell--manifest-events (context fruits paramitas)
  (let ((seq (and context (my-chatgpt-shell--events-seq (plist-get context :events)))))
    (delq nil
          (mapcar (lambda (event)
                    (when (and (listp event)
                               (not (my-chatgpt-shell--json-null-p event)))
                      (let ((entry (list :pattern (plist-get event :pattern)
                                         :kind (plist-get event :kind))))
                        (when-let* ((notes (my-chatgpt-shell--string-or-nil (plist-get event :notes))))
                          (setq entry (plist-put entry :notes notes)))
                        (cond
                         ((plist-get event :fruit/id)
                          (setq entry (plist-put entry :fruit/id (plist-get event :fruit/id)))
                          (when-let* ((emoji (plist-get event :emoji)))
                            (setq entry (plist-put entry :emoji emoji))))
                         ((plist-get event :paramita/id)
                          (setq entry (plist-put entry :paramita/id (plist-get event :paramita/id)))
                          (when-let* ((emoji (plist-get event :emoji)))
                            (setq entry (plist-put entry :emoji emoji))))
                         (t
                          (when-let* ((qual (my-chatgpt-shell--event-qualifier event fruits paramitas)))
                            (when-let* ((fid (plist-get qual :fruit/id)))
                              (setq entry (plist-put entry :fruit/id fid)))
                            (when-let* ((pid (plist-get qual :paramita/id)))
                              (setq entry (plist-put entry :paramita/id pid)))
                            (when-let* ((emoji (plist-get qual :emoji)))
                              (setq entry (plist-put entry :emoji emoji))))))
                        entry)))
                  seq))))

(defun my-chatgpt-shell--build-hud-manifest (inbound context &optional pattern-outcome)
  (when inbound
    (list :clock (plist-get inbound :clock)
          :intent (plist-get inbound :intent)
          :prototypes (my-chatgpt-shell--coerce-seq (plist-get inbound :prototypes))
          :sigils (my-chatgpt-shell--manifest-sigils (plist-get inbound :intent-sigils))
          :patterns (my-chatgpt-shell--manifest-patterns (plist-get inbound :patterns))
          :fruits (my-chatgpt-shell--manifest-entry-list (plist-get inbound :fruits)
                                                        :fruit/id :emoji :score)
          :paramitas (my-chatgpt-shell--manifest-entry-list (plist-get inbound :paramitas)
                                                            :paramita/id :emoji :score)
          :events (my-chatgpt-shell--manifest-events context
                                                     (plist-get inbound :fruits)
                                                     (plist-get inbound :paramitas))
          :nearest-pattern pattern-outcome)))

(defun my-chatgpt-shell--futon1-post (path payload)
  "POST PAYLOAD (plist) as JSON to Futon1 PATH."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-debug nil)
         (url-privacy-level 'paranoid)
         (url-show-status nil)
         (json (json-encode payload))
         (url-request-data json)
         (url-user-agent "Futon3-HUD/1.0")
         (coding-system-for-write 'utf-8)
         (coding-system-for-read 'utf-8))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously
                              (concat my-chatgpt-shell-futon1-endpoint path)
                              t t my-chatgpt-shell-request-timeout)
          (kill-buffer (current-buffer))
          t)
      (error
       (my-chatgpt-shell--debug "[HUD] Futon1 sync failed (%s): %s"
                                path (error-message-string err))
       nil))))

(defun my-chatgpt-shell--now-ms ()
  (floor (* 1000 (float-time (current-time)))))

(defun my-chatgpt-shell--current-turn-id ()
  (or (my-chatgpt-shell--string-or-nil (plist-get my-chatgpt-shell-last-inbound-edn :clock))
      (my-chatgpt-shell--string-or-nil (plist-get my-chatgpt-shell-last-edn :clock))
      (my-chatgpt-shell--string-or-nil (plist-get my-chatgpt-shell-last-inbound-edn :turn-id))
      (my-chatgpt-shell--string-or-nil (plist-get my-chatgpt-shell-last-edn :turn-id))
      (my-chatgpt-shell--now-iso)))

(defun my-chatgpt-shell--fubar-session-id ()
  (format "fubar/%s" (my-chatgpt-shell--current-turn-id)))

(with-eval-after-load 'fubar
  (setq my-fubar-session-id-fn #'my-chatgpt-shell--fubar-session-id))

(defun my-chatgpt-shell--hud-turn-entity-id (turn-id)
  (format "hud/turn/%s" turn-id))

(defun my-chatgpt-shell--scholium-entity-id (turn-id pattern-id kind)
  (let ((seed (format "%s|%s|%s|%s"
                      (or turn-id "")
                      (or pattern-id "")
                      (or kind "")
                      (my-chatgpt-shell--now-iso))))
    (format "hud/scholium/%s" (secure-hash 'sha1 seed))))

(defun my-chatgpt-shell--ensure-hud-entity (payload)
  (when payload
    (my-chatgpt-shell--futon1-post "/entity" payload)))

(defun my-chatgpt-shell--log-turn-scholium (kind pattern-id &optional notes)
  "Record a turn-level scholium in Futon1 for PATTERN-ID."
  (when (and my-chatgpt-shell-sync-futon1-manifest
             (my-chatgpt-shell--string-or-nil pattern-id))
    (let* ((turn-id (my-chatgpt-shell--current-turn-id))
           (turn-entity-id (my-chatgpt-shell--hud-turn-entity-id turn-id))
           (scholium-id (my-chatgpt-shell--scholium-entity-id turn-id pattern-id kind))
           (stamp (my-chatgpt-shell--now-ms))
           (note-text (string-trim (format "%s%s"
                                           kind
                                           (if notes (format " - %s" notes) "")))))
      (my-chatgpt-shell--ensure-hud-entity
       (list :id turn-entity-id
             :name turn-entity-id
             :type :hud/turn
             :external-id turn-id))
      (my-chatgpt-shell--ensure-hud-entity
       (list :name pattern-id
             :type :pattern/library
             :external-id pattern-id))
      (my-chatgpt-shell--ensure-hud-entity
       (list :id scholium-id
             :name scholium-id
             :type :hud/scholium
             :source note-text
             :external-id (format "%s/%s" kind stamp)))
      (my-chatgpt-shell--futon1-post "/relation"
                                     (list :type ":hud/turn-scholium"
                                           :src turn-entity-id
                                           :dst scholium-id
                                           :last-seen stamp))
      (my-chatgpt-shell--futon1-post "/relation"
                                     (list :type ":hud/scholium-pattern"
                                           :src scholium-id
                                           :dst pattern-id
                                           :last-seen stamp))
      (my-chatgpt-shell--futon1-post "/relation"
                                     (list :type ":hud/turn-pattern"
                                           :src turn-entity-id
                                           :dst pattern-id
                                           :last-seen stamp)))))

(defun my-chatgpt-shell--log-pattern-save (pattern-id &optional notes)
  (my-chatgpt-shell--log-turn-scholium "pattern-save" pattern-id notes))

(defun my-chatgpt-shell--sync-futon1-manifest (manifest)
  "Mirror MANIFEST into Futon1 via existing API endpoints."
  (when (and my-chatgpt-shell-sync-futon1-manifest manifest)
    (let* ((intent (or (plist-get manifest :intent) "unspecified"))
           (turn-id (or (plist-get manifest :clock)
                        (my-chatgpt-shell--now-iso)))
           (entity (list :name intent
                         :type :hud/turn
                         :external-id turn-id))
           (patterns (my-chatgpt-shell--coerce-seq (plist-get manifest :patterns)))
           (fruits (my-chatgpt-shell--coerce-seq (plist-get manifest :fruits)))
           (paramitas (my-chatgpt-shell--coerce-seq (plist-get manifest :paramitas)))
           (events (my-chatgpt-shell--coerce-seq (plist-get manifest :events))))
      (my-chatgpt-shell--debug "[HUD] syncing manifest to Futon1 (%s)" intent)
      (my-chatgpt-shell--futon1-post "/entity" entity)
      (dolist (pattern patterns)
        (when-let* ((pid (my-chatgpt-shell--string-or-nil (plist-get pattern :id))))
          (my-chatgpt-shell--futon1-post "/entity"
                                         (list :name pid
                                               :type :pattern/library
                                               :external-id pid))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":hud/matched-pattern"
                                               :src (plist-get entity :name)
                                               :dst pid))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":link/hud-turn"
                                               :src (plist-get entity :name)
                                               :dst pid))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":link/hud-turn"
                                               :src pid
                                               :dst (plist-get entity :name))))))
      (dolist (fruit fruits)
        (when-let* ((fid (plist-get fruit :fruit/id)))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":hud/fruit"
                                               :src (plist-get entity :name)
                                               :dst fid))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":hud/fruit-salience"
                                               :src fid
                                               :dst (plist-get fruit :score)))))
      (dolist (orb paramitas)
        (when-let* ((pid (plist-get orb :paramita/id)))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":hud/paramita"
                                               :src (plist-get entity :name)
                                               :dst pid))
          (my-chatgpt-shell--futon1-post "/relation"
                                         (list :type ":hud/paramita-salience"
                                               :src pid
                                               :dst (plist-get orb :score)))))
      (dolist (event events)
        (let* ((kind (plist-get event :kind))
               (rel-type (pcase kind
                           (:fruit ":hud/fruit-reasoning")
                           (:paramita ":hud/paramita-reasoning")
                           (_ ":hud/reasoning")))
               (dst (or (plist-get event :pattern)
                        (plist-get event :fruit/id)
                        (plist-get event :paramita/id)))
               (payload (list :type rel-type
                              :src (plist-get entity :name))))
          (when dst
            (setq payload (plist-put payload :dst dst)))
          (when-let* ((notes (my-chatgpt-shell--string-or-nil (plist-get event :notes))))
            (setq payload (plist-put payload :notes notes)))
          (my-chatgpt-shell--futon1-post "/relation" payload)))))

(defun my-chatgpt-shell--log-hud-manifest (manifest)
  "Append MANIFEST to the Futon3 buffer and optionally mirror it to Futon1."
  (when manifest
    (my-chatgpt-shell--sync-futon1-manifest manifest)
    (when (and my-chatgpt-shell-log-hud-manifest my-chatgpt-shell-debug)
      (with-current-buffer (get-buffer-create my-futon3-server-buffer)
        (let ((inhibit-read-only t)
              (print-level nil)
              (print-length nil))
          (goto-char (point-max))
          (insert (format "\n[HUD manifest %s]\n" (or (plist-get manifest :clock)
                                                        (my-chatgpt-shell--now-iso))))
          (pp manifest (current-buffer)))))))

(add-hook 'chatgpt-shell-mode-hook #'my-chatgpt-shell--init-context)

(defvar-local my-chatgpt-shell--clock-out-ready nil
  "Non-nil when this buffer has already run the automated clock-out flow.")

(defvar-local my-chatgpt-shell--clock-out-pending nil
  "Non-nil when an automated clock-out request is in flight.")

(defvar-local my-chatgpt-shell--focus-candidates nil
  "Focus headings offered to the most recent automated clock-out prompt.")

(declare-function shell-maker-kill-buffer-query "shell-maker")

(defun my-chatgpt-shell--clock-out-applicable-p ()
  "Return non-nil when the current buffer should run a clock-out sequence."
  (and (derived-mode-p 'chatgpt-shell-mode)
       (or (string= my-chatgpt-shell-profile "General")
           my-chatgpt-shell--clock-out-pending)))

(defun my-chatgpt-shell--auto-close-after-clock-out (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my-chatgpt-shell--clock-out-ready t)
      (let ((kill-buffer-query-functions kill-buffer-query-functions))
        (kill-buffer buffer)))))

(defun my-chatgpt-shell--maybe-complete-clock-out (&optional edn)
  (when (and my-chatgpt-shell--clock-out-pending
             (derived-mode-p 'chatgpt-shell-mode)
             (string= my-chatgpt-shell-profile "clock-out"))
    (setq my-chatgpt-shell--clock-out-pending nil)
    (message "[clock-out] Summary captured; closing buffer…")
    (my-chatgpt-shell-set-profile "General")
    (when my-chatgpt-shell--pending-clock-out-event
      (my-chatgpt-shell--finalize-clock-out-event t))
    (unless my-chatgpt-shell--pending-clock-out-event
      (setq my-chatgpt-shell--pending-clock-out-event (list :edn edn)))
    (let* ((focus-block (and edn (plist-get edn :focus)))
           (prepared (and focus-block (my-chatgpt-shell--handle-focus-decisions focus-block))))
      (unless prepared
        (my-chatgpt-shell--finalize-clock-out-event nil)))
    (run-at-time 0 nil #'my-chatgpt-shell--auto-close-after-clock-out (current-buffer))))

(defun my-chatgpt-shell--ensure-clock-out ()
  (setq my-chatgpt-shell--clock-out-ready nil)
  (setq my-chatgpt-shell--clock-out-pending t)
  (setq my-chatgpt-shell--focus-candidates nil)
  (let* ((buffer (current-buffer))
         (base-prompt (my-futon-read-prompt "futon0-clock-out"))
         (focus-items (setq my-chatgpt-shell--focus-candidates
                            (my-chatgpt-shell--collect-focus-items)))
         (prompt (if (and base-prompt focus-items)
                     (concat base-prompt (my-chatgpt-shell--focus-prompt-section focus-items))
                   base-prompt)))
    (unless prompt
      (setq my-chatgpt-shell--clock-out-pending nil)
      (setq my-chatgpt-shell--clock-out-ready t)
      (setq my-chatgpt-shell--focus-candidates nil)
      (message "[clock-out] Missing futon0-clock-out.prompt; closing buffer without summary.")
      (run-at-time 0 nil #'my-chatgpt-shell--auto-close-after-clock-out buffer)
      (cl-return-from my-chatgpt-shell--ensure-clock-out))
    (my-chatgpt-shell-set-profile "clock-out")
    (message "[clock-out] Summarising session before closing…")
    (let ((chatgpt-shell-prompt-query-response-style 'shell))
      (condition-case err
          (chatgpt-shell-send-to-buffer
           prompt nil nil
          (lambda (_input _output success)
            (when (and (not success)
                       (buffer-live-p buffer))
              (with-current-buffer buffer
                (setq my-chatgpt-shell--clock-out-pending nil)
                (setq my-chatgpt-shell--clock-out-ready t)
                (my-chatgpt-shell-set-profile "General")
                (message "[clock-out] Request failed; close buffer manually or retry."))))
           'shell)
        (error
         (setq my-chatgpt-shell--clock-out-pending nil)
         (setq my-chatgpt-shell--clock-out-ready t)
         (setq my-chatgpt-shell--focus-candidates nil)
         (my-chatgpt-shell-set-profile "General")
         (message "[clock-out] Could not send summary (%s)."
                  (error-message-string err)))))))

(defun my-chatgpt-shell--clock-out-query ()
  "Kill-buffer guard that runs the Futon0 clock-out routine."
  (if (not (my-chatgpt-shell--clock-out-applicable-p))
      t
    (cond
     ((and my-chatgpt-shell--clock-out-ready
           (not my-chatgpt-shell--clock-out-pending))
      t)
     (my-chatgpt-shell--clock-out-pending
      (if (yes-or-no-p "Clock-out still running – abort and close?")
          (progn
            (setq my-chatgpt-shell--clock-out-pending nil)
            (setq my-chatgpt-shell--clock-out-ready nil)
            (my-chatgpt-shell-set-profile "General")
            (when my-chatgpt-shell--pending-clock-out-event
              (setq my-chatgpt-shell--pending-clock-out-event nil))
            t)
        (message "[clock-out] Summary still running – please wait…")
        nil))
     (t
      (if (yes-or-no-p "Run Futon0 clock-out routine?")
          (progn (my-chatgpt-shell--ensure-clock-out) nil)
        t)))))

(defun my-chatgpt-shell--install-kill-hooks ()
  "Remove the shell-maker transcript prompt and add our clock-out guard."
  (remove-hook 'kill-buffer-query-functions #'shell-maker-kill-buffer-query t)
  (add-hook 'kill-buffer-query-functions #'my-chatgpt-shell--clock-out-query nil t))

(add-hook 'chatgpt-shell-mode-hook #'my-chatgpt-shell--install-kill-hooks)

(defun my-chatgpt-shell--rename-buffer (&rest _)
  (when (derived-mode-p 'chatgpt-shell-mode)
    (remove-hook 'kill-buffer-query-functions #'shell-maker-kill-buffer-query t)
    (rename-buffer (generate-new-buffer-name "*Futon ChatGPT*") t)
    (my-chatgpt-shell-set-profile "General")))

(advice-add 'chatgpt-shell :after #'my-chatgpt-shell--rename-buffer)

(defun my-chatgpt-force-kill-buffer ()
  "Force-close the current chatgpt-shell buffer, aborting any clock-out."
  (interactive)
  (when (derived-mode-p 'chatgpt-shell-mode)
    (setq my-chatgpt-shell--clock-out-pending nil
          my-chatgpt-shell--clock-out-ready nil
          my-chatgpt-shell--focus-candidates nil)
    (setq my-chatgpt-shell--pending-clock-out-event nil)
    (my-chatgpt-shell-set-profile "General")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer))))

(define-key chatgpt-shell-mode-map (kbd "C-c C-k") #'my-chatgpt-force-kill-buffer)

;;; Focus harvesting and review -------------------------------------------------

(defconst my-chatgpt-shell-focus-review-buffer-name "*Futon Focus Review*")
(defvar my-chatgpt-shell--last-focus-review nil
  "Most recent set of focus-review suggestions captured from a clock-out turn.")

(defvar my-chatgpt-shell--pending-clock-out-event nil
  "Pending clock-out EDN awaiting persistence into Futon1.")

(defun my-chatgpt-shell--focus-source-files ()
  (seq-filter #'file-readable-p
              (or my-chatgpt-shell-focus-files
                  (and (fboundp 'org-agenda-files)
                       (ignore-errors (org-agenda-files)))
                  '())))

(defun my-chatgpt-shell--focus-format-time (time)
  (when time
    (format-time-string "%Y-%m-%d" time)))

(defun my-chatgpt-shell--strip-org-drawers (text)
  "Remove Org property/log drawers from TEXT."
  (let* ((pattern (rx ":" (or "PROPERTIES" "LOGBOOK") ":"
                      (+? anything)
                      ":END:"
                      (* "\n")))
         (clean text))
    (while (and clean (string-match pattern clean))
      (setq clean (replace-match "" nil nil clean)))
    clean))

(defun my-chatgpt-shell--focus-trim-body (text)
  (when text
    (let* ((clean-text (my-chatgpt-shell--strip-org-drawers text))
           (clean (string-trim (replace-regexp-in-string "\s-+" " " clean-text)))
           (max-len (max 0 my-chatgpt-shell-focus-body-max)))
      (cond
       ((zerop max-len) nil)
       ((<= (length clean) max-len) clean)
       (t (concat (substring clean 0 max-len) "…"))))))

(defun my-chatgpt-shell--focus-entry-from-point (file)
  (let* ((title (or (org-get-heading t t t t) "(no title)"))
         (raw-id (when (fboundp 'org-id-get) (ignore-errors (org-id-get))))
         (fallback (format "%s#%d"
                           (file-name-nondirectory file)
                           (line-number-at-pos)))
         (ref (or raw-id fallback))
         (scheduled (my-chatgpt-shell--focus-format-time (org-get-scheduled-time (point))))
         (deadline (my-chatgpt-shell--focus-format-time (org-get-deadline-time (point))))
         (body (my-chatgpt-shell--focus-trim-body (org-get-entry)))
         (marker (point-marker))
         (tags (string-join (or (org-get-tags nil t) '()) ", ")))
    (list :id raw-id
          :ref ref
          :fallback fallback
          :title title
          :scheduled scheduled
          :deadline deadline
          :body body
          :tags tags
          :file file
          :marker marker)))

(defun my-chatgpt-shell--collect-focus-items ()
  (let (items)
    (dolist (file (my-chatgpt-shell--focus-source-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((todo (org-get-todo-state)))
              (when (and todo (string= todo "FOCUS"))
                (push (my-chatgpt-shell--focus-entry-from-point file) items))))
          "+TODO=\"FOCUS\"" 'file))))
    (setq items (nreverse items))
    (if (and my-chatgpt-shell-focus-limit (> my-chatgpt-shell-focus-limit 0))
        (seq-take items my-chatgpt-shell-focus-limit)
      items)))

(defun my-chatgpt-shell--focus-line (entry)
  (let ((id (plist-get entry :ref))
        (title (plist-get entry :title))
        (scheduled (plist-get entry :scheduled))
        (deadline (plist-get entry :deadline))
        (tags (plist-get entry :tags))
        (body (plist-get entry :body)))
    (concat "- " id " | " title
            (when scheduled (format " (scheduled %s)" scheduled))
            (when deadline (format " (deadline %s)" deadline))
            (when (and tags (not (string-empty-p tags)))
              (format " [tags: %s]" tags))
            (when body (format " — %s" body)))))

(defun my-chatgpt-shell--focus-prompt-section (entries)
  (when entries
    (concat
     "\n\nFocus review queue (score each item and update the EDN :focus list).\n"
     "Report suggestions inside the EDN block as :focus [{:org/id \"ID\" :suggestion :done|:keep :notes \"...\" :confidence 0.0-1.0} …].\n"
     (string-join (mapcar #'my-chatgpt-shell--focus-line entries) "\n"))))

(defun my-chatgpt-shell--focus-find (id candidates)
  (seq-find
   (lambda (entry)
     (let ((ref (plist-get entry :ref))
           (raw (plist-get entry :id))
           (title (plist-get entry :title)))
       (or (and id ref (string= id ref))
           (and id raw (string= id raw))
           (and id title (string= (string-trim id) title)))))
   candidates))

(defun my-chatgpt-shell--focus-normalize-suggestion (value)
  (cond
   ((symbolp value) value)
   ((stringp value) (intern (downcase value)))
   (t :keep)))

(defun my-chatgpt-shell--prepare-focus-review (decisions)
  (let* ((candidates my-chatgpt-shell--focus-candidates)
         (entries
          (cl-loop for decision in (my-chatgpt-shell--coerce-seq decisions)
                   for id = (or (plist-get decision :org/id)
                                (plist-get decision :id)
                                (plist-get decision :ref))
                   for candidate = (and id (my-chatgpt-shell--focus-find id candidates))
                   when candidate
                   collect (append candidate
                                    (list :suggestion (my-chatgpt-shell--focus-normalize-suggestion
                                                       (plist-get decision :suggestion))
                                          :notes (plist-get decision :notes)
                                          :confidence (plist-get decision :confidence))))))
    entries))

(defvar my-chatgpt-shell-focus-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'my-chatgpt-shell-focus-review-mark-done)
    (define-key map (kbd "s") #'my-chatgpt-shell-focus-review-skip)
    (define-key map (kbd "RET") #'my-chatgpt-shell-focus-review-visit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap used in `my-chatgpt-shell-focus-review-mode'.")

(define-derived-mode my-chatgpt-shell-focus-review-mode tabulated-list-mode "FutonFocus"
  "Mode used for confirming AI-suggested focus completions."
  (setq tabulated-list-format [
                                ("ID" 16 t)
                                ("Suggestion" 11 t)
                                ("Title" 40 t)
                                ("Confidence" 11 t)
                                ("Notes" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Suggestion" nil))
  (tabulated-list-init-header))

(defvar-local my-chatgpt-shell-focus-review--entries nil)
(defvar-local my-chatgpt-shell-focus-review--finalized nil)

(defun my-chatgpt-shell-focus-review--refresh ()
  (setq tabulated-list-entries
        (mapcar
         (lambda (entry)
           (let ((id (plist-get entry :ref))
                 (title (plist-get entry :title))
                 (suggestion (or (plist-get entry :suggestion) :keep))
                 (confidence (plist-get entry :confidence))
                 (notes (or (plist-get entry :notes) "")))
             (list id
                   (vector id
                           (symbol-name suggestion)
                           title
                           (if confidence
                               (format "%.2f" confidence)
                             "?")
                           notes))))
         my-chatgpt-shell-focus-review--entries))
  (tabulated-list-print t))

(defun my-chatgpt-shell-focus-review--entry-at-point ()
  (let ((id (tabulated-list-get-id)))
    (seq-find (lambda (entry) (equal (plist-get entry :ref) id))
              my-chatgpt-shell-focus-review--entries)))

(defmacro my-chatgpt-shell-focus-review--with-entry (entry &rest body)
  (declare (indent 1))
  `(let* ((marker (plist-get ,entry :marker))
          (buf (and marker (marker-buffer marker)))
          (pos (and marker (marker-position marker))))
     (unless (and buf (buffer-live-p buf) pos)
       (user-error "Original Org heading is not available anymore"))
     (with-current-buffer buf
       (save-excursion
         (goto-char pos)
         (org-show-entry)
         ,@body))))

(defun my-chatgpt-shell-focus-review-mark-done ()
  "Mark the highlighted focus entry as DONE and drop it from the list."
  (interactive)
  (let ((entry (my-chatgpt-shell-focus-review--entry-at-point)))
    (unless entry
      (user-error "No focus entry at point"))
    (my-chatgpt-shell-focus-review--with-entry entry
      (org-todo 'done))
    (my-chatgpt-shell--focus-review-update-global entry :done)
    (setq my-chatgpt-shell-focus-review--entries
          (delq entry my-chatgpt-shell-focus-review--entries))
    (my-chatgpt-shell-focus-review--refresh)
    (message "Marked %s as DONE" (plist-get entry :title))
    (when (null my-chatgpt-shell-focus-review--entries)
      (my-chatgpt-shell-focus-review--finish nil)
      (quit-window))))

(defun my-chatgpt-shell-focus-review-skip ()
  "Skip the highlighted entry, leaving it untouched."
  (interactive)
  (let ((entry (my-chatgpt-shell-focus-review--entry-at-point)))
    (unless entry
      (user-error "No focus entry at point"))
    (my-chatgpt-shell--focus-review-update-global entry :skip)
    (setq my-chatgpt-shell-focus-review--entries
          (delq entry my-chatgpt-shell-focus-review--entries))
    (my-chatgpt-shell-focus-review--refresh)
    (message "Skipped %s" (plist-get entry :title))
    (when (null my-chatgpt-shell-focus-review--entries)
      (my-chatgpt-shell-focus-review--finish nil)
      (quit-window))))

(defun my-chatgpt-shell-focus-review-visit ()
  "Jump to the Org heading behind the highlighted entry."
  (interactive)
  (let ((entry (my-chatgpt-shell-focus-review--entry-at-point)))
    (unless entry
      (user-error "No focus entry at point"))
    (let* ((marker (plist-get entry :marker))
           (buf (marker-buffer marker)))
      (unless (and buf (buffer-live-p buf))
        (user-error "Original Org heading is not available anymore"))
      (pop-to-buffer buf)
      (goto-char (marker-position marker))
      (org-show-entry))))

(defun my-chatgpt-shell-focus-review--finish (aborted)
  (unless my-chatgpt-shell-focus-review--finalized
    (setq my-chatgpt-shell-focus-review--finalized t)
    (when my-chatgpt-shell--pending-clock-out-event
      (my-chatgpt-shell--finalize-clock-out-event aborted))))

(defun my-chatgpt-shell-focus-review--kill-hook ()
  (my-chatgpt-shell-focus-review--finish t))

(defun my-chatgpt-shell--show-focus-review-buffer (entries)
  (let ((buf (get-buffer-create my-chatgpt-shell-focus-review-buffer-name)))
    (with-current-buffer buf
      (my-chatgpt-shell-focus-review-mode)
      (setq my-chatgpt-shell-focus-review--entries (copy-tree entries t))
      (setq-local my-chatgpt-shell-focus-review--finalized nil)
      (add-hook 'kill-buffer-hook #'my-chatgpt-shell-focus-review--kill-hook nil t)
      (my-chatgpt-shell-focus-review--refresh))
    (display-buffer buf)))

(defun my-chatgpt-shell-open-last-focus-review ()
  "Reopen the most recent focus mini-game buffer."
  (interactive)
  (if (not my-chatgpt-shell--last-focus-review)
      (message "No recorded focus suggestions yet")
    (my-chatgpt-shell--show-focus-review-buffer (copy-tree my-chatgpt-shell--last-focus-review t))))

(defun my-chatgpt-shell--focus-review-update-global (entry decision)
  (let* ((id (plist-get entry :ref))
         (global (seq-find (lambda (item)
                             (let ((ref (plist-get item :ref))
                                   (fallback (plist-get item :fallback)))
                               (or (and id ref (equal ref id))
                                   (and (not id) fallback
                                        (equal fallback (plist-get entry :fallback))))))
                           my-chatgpt-shell--last-focus-review)))
    (when global
      (plist-put global :decision decision)
      (plist-put global :decision-at (my-chatgpt-shell--now-iso))
      (plist-put global :accepted
                 (and (eq decision :done)
                      (eq (plist-get global :suggestion) :done))))
    global))

(defun my-chatgpt-shell--focus-report (entries)
  (mapcar
   (lambda (entry)
     (list :org/id (or (plist-get entry :id) (plist-get entry :ref))
           :ref (plist-get entry :ref)
           :title (plist-get entry :title)
           :suggestion (plist-get entry :suggestion)
           :ai-notes (plist-get entry :notes)
           :decision (or (plist-get entry :decision) :pending)
           :decision-at (plist-get entry :decision-at)
           :accepted (plist-get entry :accepted)
           :confidence (plist-get entry :confidence)))
   entries))

(defun my-chatgpt-shell--focus-metrics (entries aborted)
  (let ((proposed 0)
        (accepted 0)
        (reviewed 0))
    (dolist (entry entries)
      (let ((suggestion (plist-get entry :suggestion))
            (decision (plist-get entry :decision)))
        (when (eq suggestion :done)
          (setq proposed (1+ proposed)))
        (when decision
          (setq reviewed (1+ reviewed)))
        (when (and (eq suggestion :done)
                   (eq decision :done))
          (setq accepted (1+ accepted)))))
    (let ((score (and (> proposed 0)
                      (/ (float accepted) proposed))))
      (list :proposed proposed
            :accepted accepted
            :score score
            :reviewed reviewed
            :aborted aborted))))

(defun my-chatgpt-shell--edn->string (edn)
  (when edn
    (let ((print-level nil)
          (print-length nil))
      (with-temp-buffer
        (prin1 edn (current-buffer))
        (buffer-string)))))

(defun my-chatgpt-shell--post-clock-out-event (edn entries metrics)
  (let* ((clock (or (plist-get edn :clock) (my-chatgpt-shell--now-iso)))
         (intent (or (plist-get edn :intent) "unspecified"))
         (name (format "clock-out/%s" clock))
         (payload (list :name name
                        :type :clock-out/summary
                        :external-id name
                        :clock clock
                        :intent intent
                        :edn (my-chatgpt-shell--edn->string edn))))
    (when entries
      (setq payload (append payload (list :focus (my-chatgpt-shell--focus-report entries)))))
    (when metrics
      (setq payload (append payload (list :focus-metrics metrics))))
    (my-chatgpt-shell--futon1-post "/entity" payload)))

(defun my-chatgpt-shell--finalize-clock-out-event (&optional aborted)
  (when my-chatgpt-shell--pending-clock-out-event
    (let* ((pending my-chatgpt-shell--pending-clock-out-event)
           (edn (plist-get pending :edn))
           (entries my-chatgpt-shell--last-focus-review)
           (metrics (and entries (my-chatgpt-shell--focus-metrics entries aborted))))
      (my-chatgpt-shell--post-clock-out-event edn entries metrics)
      (setq my-chatgpt-shell--pending-clock-out-event nil)
      (setq my-chatgpt-shell--last-focus-review nil)
      (setq my-chatgpt-shell--focus-candidates nil))))

(defun my-chatgpt-shell--handle-focus-decisions (entries)
  (let ((prepared (my-chatgpt-shell--prepare-focus-review entries)))
    (setq my-chatgpt-shell--last-focus-review (copy-tree prepared t))
    (when (and prepared my-chatgpt-shell-focus-auto-open-review)
      (my-chatgpt-shell--show-focus-review-buffer (copy-tree prepared t)))
    prepared))

(defun my-chatgpt-shell--close-futon3-on-kill ()
  (when (or (and (window-live-p my-chatgpt-shell--context-window)
                 (eq (window-parameter my-chatgpt-shell--context-window 'tatami-context-owner)
                     (current-buffer)))
            (and (window-live-p my-chatgpt-shell--stack-window)
                 (eq (window-parameter my-chatgpt-shell--stack-window 'tatami-context-owner)
                     (current-buffer))))
    (my-chatgpt-shell--delete-context-windows))
  (when (and my-chatgpt-shell-stack-frame-close-on-exit
             (frame-live-p my-chatgpt-shell--stack-frame))
    (delete-frame my-chatgpt-shell--stack-frame)
    (setq my-chatgpt-shell--stack-frame nil))
  (my-futon3-close-tatami-session
   (format "chatgpt-shell %s closed" (buffer-name))))

(add-hook 'chatgpt-shell-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook #'my-chatgpt-shell--close-futon3-on-kill nil t)))

;;; Further configuration of Tatami for interactive use

(require 'cl-lib)
(require 'seq)

(defun my-chatgpt-shell--format-focus (fh)
  (let* ((ts (alist-get 'generated_at fh))
         (current (alist-get 'current fh))
         (history (alist-get 'history fh))
         (intent  (alist-get 'intent fh)))
    (when (and (listp current) current)
      (let* ((usable (seq-remove (lambda (entry)
                                   (let ((label (alist-get 'label entry)))
                                     (or (null label)
                                         (string= label "I"))))
                                 current))
             (primary (or (car usable) (car history)))
             (current-labels (let* ((raw-labels (delq nil (mapcar (lambda (entry)
                                                                   (alist-get 'label entry))
                                                                 (or usable current)))))
                               (seq-uniq raw-labels #'string=)))
             (intent-type (when (listp intent)
                            (alist-get 'type intent)))
             (primary-score (and (listp primary)
                                 (alist-get 'score primary)))
             (parts (delq nil
                          (list (when ts
                                  (format "Generated at %s"
                                          (format-time-string "%Y-%m-%d %H:%M:%S (%Z)"
                                                              (seconds-to-time (/ ts 1000.0)))))
                                (when current-labels
                                  (format "Current focus: %s"
                                          (string-join current-labels ", ")))
                                (when intent-type
                                  (format "Intent: %s" intent-type))
                                (when primary
                                  (format "Top anchor: %s%s"
                                          (alist-get 'label primary)
                                          (if (numberp primary-score)
                                              (format " (score %.2f)" (float primary-score))
                                            "")))))))
        (when parts
          (setq my-chatgpt-shell-last-focus (string-join parts " · "))
          (my-chatgpt-shell--maybe-render-context)
          my-chatgpt-shell-last-focus)))))

(defun my-chatgpt-shell--focus-snippet ()
  (let* ((resp    (tatami-focus-header))
         (fh      (alist-get 'focus_header resp)))
    (my-chatgpt-shell--format-focus fh)))

;;;; --- Tatami availability + safe call --------------------------------------

(defgroup tatami-integration nil
  "Resilient helpers for the Tatami headless API."
  :group 'external)

(defcustom tatami-status-url nil
  "Optional health/ready URL for the headless API (e.g., http://127.0.0.1:5100/healthz).
 set, we’ll do a quick HTTP 200 check. If nil, we fall back to checking
r a live process in the *headless-api-server* buffer."
  :type '(choice (const :tag "No HTTP probe (buffer/process fallback)" nil)
		 (string :tag "Health URL")))

(defun tatami--http-200-p (url &optional timeout silent)
  "Return non-nil iff URL responds with HTTP 200 within TIMEOUT seconds."
  (when (and url (stringp url) (not (string-empty-p url)))
    (let ((url-request-method "GET")
          (url-show-status nil)
          (url-automatic-caching nil))
      (condition-case err
          (with-timeout ((or timeout 0.6) nil)
	    (when-let* ((buf (url-retrieve-synchronously url t (or timeout 0.6))))
              (unwind-protect
                  (with-current-buffer buf
		      (goto-char (point-min))
		      (when (re-search-forward "^HTTP/[^ ]+ \\([0-9][0-9][0-9]\\)" nil t)
                        (string= (match-string 1) "200")))
                (kill-buffer buf))))
        (error
         (unless silent
           (setq tatami--last-error (error-message-string err)))
         nil)))))

(defun tatami-available-p ()
  "Return non-nil if the headless API looks reachable."
  (or (tatami--server-running-p)
      (let ((buf (get-buffer "*headless-api-server*")))
	(when buf
          (when-let* ((proc (get-buffer-process buf)))
            (process-live-p proc))))))

(defun my-tatami--server-running-p (&rest _ignored)
  "Stricter probe that only treats HTTP 200 responses as healthy."
  (let* ((base (and tatami-base-url
                    (replace-regexp-in-string "/\\'" "" tatami-base-url)))
         (health-url
          (cond ((and (stringp tatami-status-url)
                      (not (string-empty-p tatami-status-url)))
                 tatami-status-url)
                ((and (stringp tatami-health-path)
                      (not (string-empty-p tatami-health-path))
                      base)
                 (concat base tatami-health-path))))
         (fallback (when base (concat base "/")))
         (candidates (delq nil (list health-url fallback))))
    (or (seq-some (lambda (url)
                    (when (tatami--http-200-p url 0.8 t)
                      (setq tatami--last-error nil)
                      t))
                  candidates)
        (progn
          (setq tatami--last-error
                (format "No HTTP 200 response from %s"
                        (if candidates
                            (string-join candidates ", ")
                          "the configured Tatami endpoints")))
          nil))))

(advice-add 'tatami--server-running-p :override #'my-tatami--server-running-p)

(defun tatami-me-summary-safe ()
  "Call `tatami-me-summary' only if available; never signal."
  (when (tatami-available-p)
    (condition-case _e
	(with-timeout (0.8 nil)       ;; belt-and-braces: don’t hang if it wedges
          (tatami-me-summary))
      (error nil))))

;;; --- Tatami EDN plumbing ----------------------------------------------------

(defun my-chatgpt-shell--debug (fmt &rest args)
  (when my-chatgpt-shell-debug
    (apply #'message (concat "[tatami] " fmt) args)))

(defun my-chatgpt-shell--context-log-entries ()
  "Return the current Tatami context log vector (or empty vector)."
  (let ((file my-chatgpt-shell-edn-log-file))
    (if (and file (file-readable-p file))
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (read (current-buffer)))
          (error
           (my-chatgpt-shell--debug "Tatami context log unreadable: %s"
                                    (error-message-string err))
           []))
      [])))

(defun my-chatgpt-shell--write-context-log (entries)
  "Persist ENTRIES (a vector) to `my-chatgpt-shell-edn-log-file'."
  (let ((file my-chatgpt-shell-edn-log-file))
    (when file
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix)
            (print-length nil)
            (print-level nil))
        (with-temp-file file
          (prin1 entries (current-buffer))
          (insert "\n"))))))

(defun my-chatgpt-shell--encoded-prototypes ()
  (or (my-futon3--encode-prototypes) '()))

(defun my-chatgpt-shell--prototype-keywords ()
  (mapcar (lambda (proto)
            (intern (concat ":" proto)))
          (my-chatgpt-shell--encoded-prototypes)))

(defun my-chatgpt-shell--current-futons ()
  (let (acc)
    (dolist (proto (my-chatgpt-shell--encoded-prototypes))
      (when (string-match "^\\([^/]+\\)/" proto)
        (let ((futon (intern (concat ":" (match-string 1 proto)))))
          (cl-pushnew futon acc :test #'eq))))
    (nreverse acc)))

(defun my-chatgpt-shell--now-iso ()
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun my-chatgpt-shell--vector (items)
  (apply #'vector items))

(defun my-chatgpt-shell--encode-sigils (pairs)
  (my-chatgpt-shell--vector
   (mapcar (lambda (pair)
             (list (cons "emoji" (plist-get pair :emoji))
                   (cons "hanzi" (plist-get pair :hanzi))))
           pairs)))

(defun my-chatgpt-shell--format-sigils (pairs)
  (when (and pairs (not (seq-empty-p pairs)))
    (let ((tokens (seq-filter (lambda (token)
                                (and token (not (string-empty-p token))))
                              (seq-map (lambda (pair)
                                         (let ((emoji (or (plist-get pair :emoji) "?"))
                                               (hanzi (or (plist-get pair :hanzi) "?")))
                                           (format "[%s/%s]" emoji hanzi)))
                                       pairs))))
      (when tokens
        (string-join tokens " ")))))

(defun my-chatgpt-shell--plist-p (value)
  (and (listp value)
       (cl-evenp (length value))))

(defun my-chatgpt-shell--edn-encode (value)
  (cond
   ((null value) "nil")
   ((eq value t) "true")
   ((eq value :false) "false")
   ((stringp value) (prin1-to-string value))
   ((keywordp value)
    (let* ((name (symbol-name value))
           (trim (if (and (> (length name) 0)
                           (char-equal (aref name 0) ?:))
                      (substring name 1)
                    name)))
      (concat ":" trim)))
   ((symbolp value) (symbol-name value))
   ((integerp value) (number-to-string value))
   ((floatp value) (format "%S" value))
   ((vectorp value)
    (concat "[" (mapconcat #'my-chatgpt-shell--edn-encode (append value nil) " ") "]"))
   ((my-chatgpt-shell--plist-p value)
    (let (parts)
      (while value
        (let ((k (pop value))
              (v (pop value)))
          (push (concat (my-chatgpt-shell--edn-encode k)
                        " "
                        (my-chatgpt-shell--edn-encode v))
                parts)))
      (concat "{" (mapconcat #'identity (nreverse parts) " ") "}")))
   ((listp value)
    (concat "(" (mapconcat #'my-chatgpt-shell--edn-encode value " ") ")"))
   (t (prin1-to-string value))))

(defun my-chatgpt-shell--format-edn-block (marker payload)
  (format "---%s---\n%s\n---END-%s---"
          marker (my-chatgpt-shell--edn-encode payload) marker))

(defun my-chatgpt-shell--extract-edn-block (text marker)
  (when (and text marker)
    (let* ((start-marker (format "---%s---" marker))
           (end-marker (format "---END-%s---" marker))
           (start (string-match (regexp-quote start-marker) text)))
      (when start
        (let ((end (string-match (regexp-quote end-marker) text start)))
          (when end
            (substring text (+ start (length start-marker)) end)))))))

(defun my-chatgpt-shell--parse-edn-string (raw)
  (when raw
    (let* ((converted (replace-regexp-in-string
                       "}" ")"
                       (replace-regexp-in-string "{" "(" raw nil 'literal)
                       nil 'literal))
           (value (condition-case err
                      (car (read-from-string converted))
                    (error
                     (message "Failed to parse EDN: %s"
                              (error-message-string err))
                     nil))))
      (when (and value (listp value))
        value))))

(defun my-chatgpt-shell--extract-chatgpt-edn (text)
  (when-let* ((raw (my-chatgpt-shell--extract-edn-block
                    text my-chatgpt-shell-tatami-out-marker)))
    (my-chatgpt-shell--parse-edn-string raw)))

(defun my-chatgpt-shell--coerce-seq (value)
  (cond
   ((or (null value)
        (my-chatgpt-shell--json-null-p value)) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun my-chatgpt-shell--events-seq (events)
  (cl-loop for event in (my-chatgpt-shell--coerce-seq events)
           when (and event (listp event)
                     (not (my-chatgpt-shell--json-null-p event)))
           collect event))

(defun my-chatgpt-shell--event-embedding-text (event)
  (when (listp event)
    (let ((pattern (my-chatgpt-shell--pattern->string (plist-get event :pattern)))
          (kind (my-chatgpt-shell--pattern->string (plist-get event :kind)))
          (notes (my-chatgpt-shell--string-or-nil (plist-get event :notes))))
      (string-join (delq nil (list pattern kind notes)) " "))))

(defun my-chatgpt-shell--intent-embedding-text (edn intent)
  (let* ((intent-text (and intent (string-trim intent)))
         (events (and edn (my-chatgpt-shell--events-seq (plist-get edn :events))))
         (event-lines (delq nil (mapcar #'my-chatgpt-shell--event-embedding-text events)))
         (parts (delq nil (append (list intent-text) event-lines))))
    (when parts
      (string-join parts "\n"))))

(defun my-chatgpt-shell--valid-pattern-value-p (value)
  (and value
       (or (and (stringp value) (> (length value) 0))
           (keywordp value)
           (symbolp value))))

(defun my-chatgpt-shell--pattern-event-p (event)
  (when (listp event)
    (let ((pattern (plist-get event :pattern))
          (notes (plist-get event :notes)))
      (and (my-chatgpt-shell--valid-pattern-value-p pattern)
           (stringp notes)
           (> (length notes) 0)))))

(defun my-chatgpt-shell--pattern-none-p (pattern)
  (let* ((raw (cond
               ((null pattern) nil)
               ((stringp pattern) pattern)
               ((keywordp pattern) (symbol-name pattern))
               ((symbolp pattern) (symbol-name pattern))
               (t (format "%s" pattern))))
         (trimmed (and raw (string-trim raw ":" ":"))))
    (and trimmed
         (string= (downcase trimmed) "none"))))

(defun my-chatgpt-shell--pattern-event-outcome (events)
  (let ((none-notes nil))
    (cl-loop for event in events
             when (my-chatgpt-shell--pattern-event-p event)
             do (let ((pattern (plist-get event :pattern))
                      (notes (plist-get event :notes)))
                  (if (my-chatgpt-shell--pattern-none-p pattern)
                      (unless none-notes
                        (setq none-notes notes))
                    (cl-return (list :status :match
                                     :pattern (my-chatgpt-shell--pattern->string pattern)
                                     :notes notes))))
             finally (when none-notes
                       (cl-return (list :status :none :notes none-notes))))))

(defun my-chatgpt-shell--json-null-p (value)
  (or (null value)
      (and (boundp 'json-null) (eq value json-null))
      (eq value :null)
      (eq value 'json-null)))


(defun my-chatgpt-shell--normalize-intent-string (value)
  (if (stringp value)
      (string-trim value)
    ""))

(defun my-chatgpt-shell--string-or-nil (value)
  (cond
   ((my-chatgpt-shell--json-null-p value) nil)
   ((stringp value)
    (let ((trim (string-trim value)))
      (unless (string-empty-p trim) trim)))
   ((keywordp value) (symbol-name value))
   ((symbolp value) (symbol-name value))
   ((numberp value) (number-to-string value))
   (t value)))

(defun my-chatgpt-shell--pattern-id (pattern)
  (or (my-chatgpt-shell--string-or-nil (plist-get pattern :id))
      (my-chatgpt-shell--string-or-nil (plist-get pattern :pattern/id))
      (my-chatgpt-shell--string-or-nil (plist-get pattern 'id))
      (my-chatgpt-shell--string-or-nil (plist-get pattern :title))))

(defun my-chatgpt-shell--sanitize-entry-list (entries)
  (cl-loop for entry in (my-chatgpt-shell--coerce-seq entries)
           when (and entry (listp entry))
           collect entry))

(defun my-chatgpt-shell--format-pattern-outcome (outcome &optional notes-limit)
  (when outcome
    (let* ((status (plist-get outcome :status))
           (pattern (plist-get outcome :pattern))
           (notes (plist-get outcome :notes)))
      (when (and notes notes-limit)
        (setq notes (my-chatgpt-shell--truncate notes notes-limit)))
      (pcase status
        (:match (concat "Patterns apply?: 🟢 "
                        (if (and pattern (> (length pattern) 0))
                            pattern
                          "Clause matched.")
                        (if (and notes (> (length notes) 0))
                            (format " - %s" notes)
                          "")))
        (:none  (concat "Patterns apply?: 🔴"
                        (if (and notes (> (length notes) 0))
                            (format " - %s" notes)
                          "")))
        (_ nil)))))

(defun my-chatgpt-shell--insert-pattern-outcome (outcome &optional notes-limit)
  (when outcome
    (let* ((status (plist-get outcome :status))
           (pattern (plist-get outcome :pattern))
           (notes (plist-get outcome :notes)))
      (when (and notes notes-limit)
        (setq notes (my-chatgpt-shell--truncate notes notes-limit)))
      (insert "Patterns apply?: ")
      (pcase status
        (:match
         (insert "🟢 ")
         (if (and pattern (> (length pattern) 0))
             (my-chatgpt-shell--insert-pattern-button pattern pattern)
           (insert "Clause matched.")))
        (:none
         (insert "🔴"))
        (_ (insert "(no verdict captured)")))
      (when (and notes (> (length notes) 0))
        (insert (format " - %s" notes)))
      (insert "\n\n"))))

(defun my-chatgpt-shell--pattern-outcome-line (&optional notes-limit)
  (my-chatgpt-shell--format-pattern-outcome
   my-chatgpt-shell-last-pattern-outcome notes-limit))

(defun my-chatgpt-shell--warn-missing-pattern-event ()
  (let ((msg "FROM-CHATGPT-EDN missing required :pattern event; remind the model to log {:pattern ... :notes ...} or {:pattern :none ...}."))
    (unless (equal my-chatgpt-shell-last-validation-warning msg)
      (message "%s" msg))
    (setq my-chatgpt-shell-last-validation-warning msg)))

(defun my-chatgpt-shell--validate-pattern-events (edn)
  (let* ((events (my-chatgpt-shell--events-seq (plist-get edn :events)))
         (outcome (my-chatgpt-shell--pattern-event-outcome events))
         (valid (not (null outcome))))
    ;; Pattern reasoning is persisted via :events so HUD + context log can replay it.
    (setq my-chatgpt-shell-last-pattern-outcome outcome)
    (if valid
        (setq my-chatgpt-shell-last-validation-warning nil)
      (my-chatgpt-shell--warn-missing-pattern-event))
    valid))

(defun my-chatgpt-shell--maybe-update-intent (edn)
  (let* ((incoming (plist-get edn :intent))
         (trimmed (and incoming (string-trim incoming))))
    (when (and trimmed
               (> (length trimmed) 0)
               (not (string= trimmed my-futon3-tatami-default-intent)))
      ;; Capture the newest FROM-CHATGPT intent so selection + HUD stay aligned.
      (setq my-futon3-tatami-default-intent trimmed)
      (when my-chatgpt-shell-last-inbound-edn
          (setq my-chatgpt-shell-last-inbound-edn
                (plist-put (cl-copy-list my-chatgpt-shell-last-inbound-edn)
                           :intent trimmed))
          (my-chatgpt-shell--record-hud-state))
      (when my-chatgpt-shell-last-edn
        (setq my-chatgpt-shell-last-edn
              (plist-put (cl-copy-list my-chatgpt-shell-last-edn)
                         :intent trimmed)))
      (my-futon3-sync-selection)

      (my-chatgpt-shell--render-context t))))

(defun my-chatgpt-shell--pattern->string (pattern)
  (cond
   ((my-chatgpt-shell--json-null-p pattern) "")
   ((stringp pattern) pattern)
   ((keywordp pattern) (symbol-name pattern))
   ((symbolp pattern) (symbol-name pattern))
   ((numberp pattern) (number-to-string pattern))
   (t (format "%s" pattern))))

(defun my-chatgpt-shell--truncate (text limit)
  (if (and text (> (length text) limit))
      (concat (substring text 0 limit) "…")
    text))

(defun my-chatgpt-shell--take-last (items limit)
  (let ((len (length items)))
    (cond
     ((or (<= len limit) (<= limit 0)) items)
     (t (nthcdr (- len limit) items)))))

(defun my-chatgpt-shell--format-intent-distance (pattern &optional style)
  (let* ((embed (plist-get pattern :score/intent-embed-distance))
         (sigil (plist-get pattern :score/intent-distance))
         (payload (cond
                   ((and (numberp embed) (numberp sigil))
                    (format "i=%.2f s=%.2f" embed sigil))
                   ((numberp embed) (format "i=%.2f" embed))
                   ((numberp sigil) (format "i=%.2f" sigil))
                   (t nil))))
    (if payload
        (pcase style
          (:paren (format " (%s)" payload))
          (:none (format " %s" payload))
          (_ (format " [%s]" payload)))
      "")))

(defun my-chatgpt-shell--format-pattern-line (pattern)
  (let* ((id (or (my-chatgpt-shell--pattern-id pattern)
                 "(unknown pattern)"))
         (title (my-chatgpt-shell--string-or-nil (plist-get pattern :title)))
         (summary (my-chatgpt-shell--string-or-nil (plist-get pattern :summary)))
         (score (plist-get pattern :score))
         (headline (string-trim
                    (or (and title
                             (not (string= title id))
                             (format "%s (%s)" title id))
                        title
                        id))))
    (concat "• " headline
            (if summary
                (format " – %s" (my-chatgpt-shell--truncate summary 140))
              "")
            (concat (when (numberp score)
                      (format " [d=%.2f]" score))
                    (my-chatgpt-shell--format-intent-distance pattern)))))

(declare-function arxana-browser-patterns--ensure-frame "arxana-browser-patterns")
(declare-function my-fubar-log-pattern-used "fubar" (session-id pattern-id &optional reason))
(declare-function my-fubar-session-id "fubar" ())

(defun my-chatgpt-shell--open-pattern (pattern-id)
  (when (and (stringp pattern-id)
             (fboundp 'my-futon3-set-active-pattern))
    (my-futon3-set-active-pattern pattern-id))
  (when (fboundp 'my-futon3--capture-pattern-provenance)
    (my-futon3--capture-pattern-provenance pattern-id "tatami-context"))
  (when my-chatgpt-shell-log-pattern-clicks
    (my-chatgpt-shell--log-turn-scholium "context-click" pattern-id "clickthrough"))
  (when (fboundp 'my-fubar-session-id)
    (my-fubar-log-pattern-used (my-fubar-session-id) pattern-id "tatami-context"))
  (cond
   ((fboundp 'arxana-patterns-open)
    (let ((frame (when (fboundp 'arxana-browser-patterns--ensure-frame)
                   (arxana-browser-patterns--ensure-frame))))
      (when (frame-live-p frame)
        (select-frame-set-input-focus frame))
      (arxana-patterns-open pattern-id)
      (when (frame-live-p frame)
        (with-selected-frame frame
          (delete-other-windows)))))
   (t (message "Pattern browser not available; load Futon4/arxana."))))

(defun my-chatgpt-shell--insert-pattern-button (label pattern-id)
  (if (and (stringp pattern-id) (> (length pattern-id) 0))
      (insert-text-button label
                          'face (if my-chatgpt-shell-pattern-anchor-underline
                                    (list my-chatgpt-shell-pattern-anchor-face 'underline)
                                  my-chatgpt-shell-pattern-anchor-face)
                          'mouse-face 'highlight
                          'follow-link t
                          'help-echo "Open pattern in Arxana"
                          'action (lambda (_btn)
                                    (my-chatgpt-shell--open-pattern pattern-id)))
    (insert label)))

(defun my-chatgpt-shell--insert-pattern-anchor (title id)
  (let ((title (my-chatgpt-shell--string-or-nil title))
        (id (my-chatgpt-shell--string-or-nil id)))
    (cond
     ((and title id (not (string= title id)))
      (insert title " (")
      (my-chatgpt-shell--insert-pattern-button id id)
      (insert ")"))
     (id
      (my-chatgpt-shell--insert-pattern-button id id))
     (title
      (insert title))
     (t
      (insert "(unknown pattern)")))))

(defun my-chatgpt-shell--insert-pattern-line (pattern)
  (let* ((id (my-chatgpt-shell--pattern-id pattern))
         (title (my-chatgpt-shell--string-or-nil (plist-get pattern :title)))
         (summary (my-chatgpt-shell--string-or-nil (plist-get pattern :summary)))
         (score (plist-get pattern :score)))
    (insert "• ")
    (my-chatgpt-shell--insert-pattern-anchor title id)
    (when summary
      (insert (format " – %s" (my-chatgpt-shell--truncate summary 140))))
    (when (numberp score)
      (insert (format " [d=%.2f]" score)))
    (insert (my-chatgpt-shell--format-intent-distance pattern))))

(defun my-chatgpt-shell--support-score (entry)
  (let ((score (plist-get entry :score)))
    (if (numberp score)
        (float score)
      0.0)))

(defun my-chatgpt-shell--support-summary (summary)
  (cond
   ((stringp summary) (string-trim summary))
   ((and (listp summary)
         (or (plist-get summary :professional)
             (plist-get summary :karmic)
             (plist-get summary :somatic)))
    (string-trim (or (plist-get summary :professional)
                     (plist-get summary :karmic)
                     (plist-get summary :somatic)
                     "")))
   (t nil)))

(defun my-chatgpt-shell--format-fruit-detail (entry)
  (let* ((emoji (my-chatgpt-shell--string-or-nil (plist-get entry :emoji)))
         (name (or (my-chatgpt-shell--string-or-nil (plist-get entry :fruit/id))
                   (my-chatgpt-shell--string-or-nil (plist-get entry :title))
                   "fruit"))
         (score (my-chatgpt-shell--support-score entry))
         (summary (or (my-chatgpt-shell--support-summary (plist-get entry :summary))
                      (my-chatgpt-shell--string-or-nil (plist-get entry :tp))
                      ""))
         (score-text (when (> score 0)
                       (format " (%.0f%%)" (* 100 score))))
         (label (string-trim (concat (or emoji "")
                                     (when (and emoji name) " ")
                                     name))))
    (when (> (length summary) 0)
      (format "%s%s – Tatami hint: %s" label (or score-text "") summary))))

(defun my-chatgpt-shell--format-paramita-detail (entry)
  (let* ((emoji (or (my-chatgpt-shell--string-or-nil (plist-get entry :emoji))
                    (my-chatgpt-shell--string-or-nil (plist-get entry :orb))))
         (name (or (my-chatgpt-shell--string-or-nil (plist-get entry :paramita/id))
                   (my-chatgpt-shell--string-or-nil (plist-get entry :title))
                   "paramita"))
         (sense (or (my-chatgpt-shell--string-or-nil (plist-get entry :sense))
                    (my-chatgpt-shell--string-or-nil (plist-get entry :summary))
                    ""))
         (score (my-chatgpt-shell--support-score entry))
         (score-text (when (> score 0)
                       (format " (%.0f%%)" (* 100 score))))
         (label (string-trim (concat (or emoji "")
                                     (when (and emoji name) " ")
                                     name))))
    (when (> (length sense) 0)
      (format "%s%s – Tatami hint: %s" label (or score-text "") sense))))

(defun my-chatgpt-shell--insert-support-list (title entries formatter)
  (let* ((clean (my-chatgpt-shell--sanitize-entry-list entries))
         (sorted (sort (cl-copy-list clean)
                       (lambda (a b)
                         (> (my-chatgpt-shell--support-score a)
                            (my-chatgpt-shell--support-score b))))))
    (when (seq-some formatter sorted)
      (insert (propertize title 'face 'bold) "\n")
      (dolist (entry sorted)
        (when-let* ((line (funcall formatter entry)))
          (insert "  • " line "\n")))
      (insert "\n"))))

(defun my-chatgpt-shell--format-event-line (event)
  (let* ((pattern (my-chatgpt-shell--pattern->string (plist-get event :pattern)))
         (kind (my-chatgpt-shell--pattern->string (plist-get event :kind)))
         (notes (string-trim (format "%s" (or (plist-get event :notes) ""))))
         (pattern-text (if (and pattern (> (length pattern) 0)
                                (not (string= pattern "none")))
                           (format "Pattern %s" pattern)
                         "No pattern"))
         (emoji (plist-get event :emoji))
         (fruit-id (plist-get event :fruit/id))
         (paramita-id (plist-get event :paramita/id))
         (kind-text (cond
                     (fruit-id
                      (format " (%s%s)"
                              (if emoji (concat emoji " ") "")
                              fruit-id))
                     (paramita-id
                      (format " (%s%s)"
                              (if emoji (concat emoji " ") "")
                              paramita-id))
                     ((> (length kind) 0)
                      (format " (%s)" kind))
                     (t "")))
         (notes-text (if (> (length notes) 0)
                         (my-chatgpt-shell--truncate notes 160)
                       "No notes supplied.")))
    (format "%s%s – %s" pattern-text kind-text notes-text)))

(defun my-chatgpt-shell--insert-event-line (event)
  (let* ((pattern (string-trim (format "%s" (or (plist-get event :pattern) ""))))
         (pattern-valid (and (> (length pattern) 0)
                             (not (string= pattern "none"))))
         (kind (my-chatgpt-shell--pattern->string (plist-get event :kind)))
         (notes (string-trim (format "%s" (or (plist-get event :notes) ""))))
         (emoji (plist-get event :emoji))
         (fruit-id (plist-get event :fruit/id))
         (paramita-id (plist-get event :paramita/id))
         (kind-text (cond
                     (fruit-id
                      (format " (%s%s)"
                              (if emoji (concat emoji " ") "")
                              fruit-id))
                     (paramita-id
                      (format " (%s%s)"
                              (if emoji (concat emoji " ") "")
                              paramita-id))
                     ((> (length kind) 0)
                      (format " (%s)" kind))
                     (t "")))
         (notes-text (if (> (length notes) 0)
                         (my-chatgpt-shell--truncate notes 160)
                       "No notes supplied.")))
    (if pattern-valid
        (progn
          (insert "Pattern ")
          (my-chatgpt-shell--insert-pattern-button pattern pattern))
      (insert "No pattern"))
    (insert kind-text " – " notes-text)))

;; TODO: check this fn
(defun my-chatgpt-shell--prompt-pattern-summary (edn)
  (let* ((intent (plist-get edn :intent))
         (patterns (my-chatgpt-shell--coerce-seq (plist-get edn :patterns)))
         (fruits (my-chatgpt-shell--coerce-seq (plist-get edn :fruits)))
         (paramitas (my-chatgpt-shell--coerce-seq (plist-get edn :paramitas)))
         (parts nil))
    (when (and intent
               (not (string-empty-p (string-trim intent))))
      (push (format "Current intent: %s" intent) parts))
    (when patterns
      (let ((lines (cl-loop for pattern in patterns
                            for idx from 0 below 4
                            for pid = (or (plist-get pattern :id)
                                          (plist-get pattern :title)
                                          "(unknown)")
                            for summary = (string-trim (or (plist-get pattern :summary)
                                                           "No summary provided."))
                            for score = (plist-get pattern :score)
                            collect (concat pid " → " summary
                                            (concat (when (numberp score)
                                                      (format " [d=%.2f]" score))
                                                    (my-chatgpt-shell--format-intent-distance pattern))))))
        (push (concat "Patterns: " (string-join lines " | ")) parts)))
    (when fruits
      (let ((tokens (cl-loop for fruit in fruits
                             for emoji = (plist-get fruit :emoji)
                             for name = (or (plist-get fruit :fruit/id)
                                            (plist-get fruit :title)
                                            "fruit")
                             for score = (plist-get fruit :score)
                             collect (string-trim (concat (or emoji "")
                                                          (when (and emoji name) " ")
                                                          name
                                                          (if (numberp score)
                                                              (format " [d=%.2f]" score)
                                                            ""))))))
        (when tokens
          (push (concat "Fruits: " (string-join tokens ", ")) parts))))
    (when paramitas
      (let ((tokens (cl-loop for paramita in paramitas
                             for emoji = (plist-get paramita :emoji)
                             for name = (or (plist-get paramita :paramita/id)
                                            (plist-get paramita :title)
                                            "paramita")
                             for score = (plist-get paramita :score)
                             collect (string-trim (concat (or emoji "")
                                                          (when (and emoji name) " ")
                                                          name
                                                          (if (numberp score)
                                                              (format " [d=%.2f]" score)
                                                            ""))))))
        (when tokens
          (push (concat "Pāramitās: " (string-join tokens ", ")) parts))))
    (when parts
      (string-join (nreverse parts) "\n"))))

(defun my-chatgpt-shell--pattern-headline (pattern)
  (string-trim-left (my-chatgpt-shell--format-pattern-line pattern) "• "))

(defun my-chatgpt-shell--nearest-pattern (patterns)
  (let* ((seq (my-chatgpt-shell--coerce-seq patterns))
         (best (car seq))
         (best-score (and best (plist-get best :score))))
    (dolist (pattern (cdr seq))
      (let ((score (plist-get pattern :score)))
        (when (and (numberp score)
                   (or (null best-score)
                       (> score best-score)))
          (setq best pattern
                best-score score))))
    best))

(defun my-chatgpt-shell--insert-nearest-pattern-line (pattern)
  (when pattern
    (let* ((id (my-chatgpt-shell--pattern-id pattern))
           (title (my-chatgpt-shell--string-or-nil (plist-get pattern :title)))
           (score (plist-get pattern :score)))
      (insert "Nearest pattern: ")
      (my-chatgpt-shell--insert-pattern-anchor title id)
      (when (numberp score)
        (insert (format " (d=%.2f)" score)))
      (insert (or (my-chatgpt-shell--format-intent-distance pattern :paren) ""))
      (insert "\n"))))

(defun my-chatgpt-shell--insert-fruit-summary (entries)
  (my-chatgpt-shell--insert-support-list "Tatami cues" entries
                                         #'my-chatgpt-shell--format-fruit-detail))

(defun my-chatgpt-shell--insert-paramita-summary (entries)
  (my-chatgpt-shell--insert-support-list "Pāramitās (virtues)" entries
                                         #'my-chatgpt-shell--format-paramita-detail))

(defun my-chatgpt-shell--support-present-p (edn)
  (when edn
    (let ((fruits (my-chatgpt-shell--sanitize-entry-list (plist-get edn :fruits)))
          (paramitas (my-chatgpt-shell--sanitize-entry-list (plist-get edn :paramitas))))
      (or fruits paramitas))))

(defun my-chatgpt-shell--cue-gap-reason (primary fallback)
  (let* ((health-check-enabled (or (and (boundp 'tatami-status-url) tatami-status-url)
                                   (and (boundp 'tatami-health-path) tatami-health-path)
                                   (and (boundp 'tatami-base-url) tatami-base-url)))
         (health-ok (and health-check-enabled
                         (fboundp 'my-tatami--server-running-p)
                         (ignore-errors (my-tatami--server-running-p))))
         (health-error (and (boundp 'tatami--last-error) tatami--last-error)))
    (cond
     ((not (my-futon3-running-p))
      "Futon3 isn't running, so Tatami hints/cues can't load; start it with `make dev` or `M-x my-futon3-start`.")
     ((and health-check-enabled (not health-ok))
      (if (and health-error (not (string-empty-p health-error)))
          (format "Tatami health check failed: %s" health-error)
        "Tatami health check failed; no HTTP 200 response from the configured endpoint."))
     ((null primary)
      "Tatami hasn't pushed /musn/hints into this buffer yet.")
     ((and primary (not (my-chatgpt-shell--support-present-p primary)))
      (if (my-chatgpt-shell--events-seq (plist-get primary :events))
          nil
        "Tatami hints returned no fruits/pāramitās for this turn; capture a fresh turn and rerun `M-x my-chatgpt-shell-refresh-context-hints`."))
     ((and fallback (not (my-chatgpt-shell--support-present-p fallback)))
      "Even the cached ChatGPT payload lacks Tatami cues; remind the LLM to log {:kind :note ...} events.")
     (t nil))))

(defun my-chatgpt-shell--futon3-ui-port ()
  (when (stringp my-futon3-ui-base-url)
    (when (string-match ":\\([0-9]+\\)\\(?:/\\|\\'\\)" my-futon3-ui-base-url)
      (match-string 1 my-futon3-ui-base-url))))

(defun my-chatgpt-shell--futon3-status-line (&optional terse)
  (let* ((ui-port (or (my-chatgpt-shell--futon3-ui-port) "6060"))
         (transport (or (plist-get my-futon3-last-status :transport-port)
                        my-futon3-transport-port
                        "5050")))
    (if (my-futon3-running-p)
        (format "Futon3 running — transport %s, UI %s (see %s; C-c there to stop)."
                transport ui-port my-futon3-server-buffer)
      (if terse
          (format "Futon3 idle — transport %s, UI %s."
                  transport ui-port)
        (format "Futon3 idle — transport %s, UI %s. Run `make dev` or `M-x my-futon3-start` to launch."
                transport ui-port)))))

(defun my-chatgpt-shell--cue-source (primary fallback)
  (let* ((fallback-edn fallback)
         (gap (my-chatgpt-shell--cue-gap-reason primary fallback-edn))
         (api-error (or my-chatgpt-shell--last-cues-error
                        my-chatgpt-shell--last-hints-error))
         (fallback-present (and fallback-edn (my-chatgpt-shell--support-present-p fallback-edn)))
         (result (cond
                  ((my-chatgpt-shell--support-present-p primary)
                   (list :edn primary :status :inbound))
                  (fallback-present
                   (list :edn fallback-edn :status :chatgpt
                         :reason (or api-error
                                      gap
                                      "hints unavailable")))
                  (t (list :edn nil :status :empty
                           :reason (or api-error
                                       gap
                                       "no cues captured"))))))
    (setq my-chatgpt-shell--last-cue-salient result)
    result))

(defun my-chatgpt-shell--cue-guidance (status &optional reason)
  (let* ((reason (or reason my-chatgpt-shell--last-hints-error))
         (details (when reason (format " (%s)" reason)))
         (start-needed (and (not (my-futon3-running-p))
                            (fboundp 'my-futon3-start)))
         (status-line (my-chatgpt-shell--futon3-status-line start-needed)))
    (unless (or reason start-needed)
      (setq status nil))
    (pcase status
      (:chatgpt (list :text (format "Tatami cues fallback%s — HUD is showing cached ChatGPT cues only. %s"
                                    (or details "") status-line)
                      :start-needed start-needed))
      (:empty (list :text (format "Tatami cues missing%s. %s"
                                  (or details "") status-line)
                    :start-needed start-needed
                    :suffix "Send a fresh turn once Tatami is ready so fruits/pāramitās log again."))
      (_ nil))))

(defun my-chatgpt-shell--insert-cue-guidance (guidance)
  (when (and guidance (plist-get guidance :text))
    (let ((start-needed (plist-get guidance :start-needed))
          (suffix (plist-get guidance :suffix)))
      (insert (plist-get guidance :text) " ")
      (if start-needed
          (progn
            (insert-text-button "Start Futon3"
                                'help-echo "Launch Futon3 (MUSN sandbox)"
                                'action (lambda (_event)
                                          (my-futon3-start t))
                                'follow-link t)
            (insert ", then run `M-x my-chatgpt-shell-refresh-context-hints` after Futon3 is ready so cues repopulate."))
        (insert "Run `M-x my-chatgpt-shell-refresh-context-hints` after Futon3 is ready so cues repopulate."))
      (when suffix
        (insert " " suffix))
      (insert "\n\n"))))

(defun my-chatgpt-shell--salient-tags ()
  (let (tags)
    (pcase (plist-get my-chatgpt-shell--last-sigil-salient :status)
      (:chatgpt (push "sigils=FROM-CHATGPT" tags))
      (:default (push "sigils=prototype" tags))
      (:empty (push "sigils=missing" tags)))
    (pcase (plist-get my-chatgpt-shell--last-cue-salient :status)
      (:chatgpt
       (push (if my-chatgpt-shell--last-hints-error
                 (format "cues=fallback (%s)" my-chatgpt-shell--last-hints-error)
               "cues=fallback")
             tags))
      (:empty
       (push (if my-chatgpt-shell--last-hints-error
                 (format "cues=missing (%s)" my-chatgpt-shell--last-hints-error)
               "cues=missing")
             tags)))
    (nreverse tags)))

(defun my-chatgpt-shell--insert-inbound-summary (edn)
  (let* ((fruits (my-chatgpt-shell--sanitize-entry-list (plist-get edn :fruits)))
         (paramitas (my-chatgpt-shell--sanitize-entry-list (plist-get edn :paramitas))))
    (if (or fruits paramitas)
        (progn
          (my-chatgpt-shell--insert-fruit-summary fruits)
          (my-chatgpt-shell--insert-paramita-summary paramitas))
      (insert "No Tatami cues available.\n\n"))))

(defun my-chatgpt-shell--insert-pattern-verdict (outcome events inbound-patterns intent)
  (let* ((recent (my-chatgpt-shell--events-seq events))
         (reason-lines-raw (and recent
                                (mapcar (lambda (event)
                                          (cons event (my-chatgpt-shell--format-event-line event)))
                                        recent)))
         (patterns (my-chatgpt-shell--sanitize-entry-list inbound-patterns))
         (pattern-limit 3)
         (nearest (my-chatgpt-shell--nearest-pattern patterns)))
    (let ((reason-lines (or reason-lines-raw '())))
      (insert (propertize "Pattern reasoning & Tatami hints" 'face 'bold) "\n")
      (if outcome
          (my-chatgpt-shell--insert-pattern-outcome outcome 160)
        (insert "Patterns apply?: (no verdict captured)\n\n"))
      (unless patterns
        (if reason-lines
            (dolist (pair reason-lines)
              (insert "• ")
              (my-chatgpt-shell--insert-event-line (car pair))
              (insert "\n"))
          (insert "No reasoning events recorded yet.\n")))
      (when patterns
        (insert "\n")
        (my-chatgpt-shell--insert-nearest-pattern-line nearest)
        (insert (if (> (length patterns) pattern-limit)
                    (format "Top %d candidate clauses:\n" pattern-limit)
                  "Candidate clauses:\n"))
        (let (matched-events)
          (cl-loop for pattern in patterns
                   for idx from 0
                   while (< idx pattern-limit)
                   for event = (my-chatgpt-shell--find-event-for-pattern pattern events)
                   do (my-chatgpt-shell--insert-pattern-candidate pattern event)
                   when event do (push event matched-events))
          (let* ((unique (delq nil matched-events))
                 (extras (seq-remove (lambda (pair)
                                       (and (car pair)
                                            (seq-some (lambda (evt)
                                                        (equal evt (car pair)))
                                                      unique)))
                                     reason-lines)))
            (when extras
              (insert "\nAdditional reasoning:\n")
              (dolist (pair extras)
                (insert "  • ")
                (my-chatgpt-shell--insert-event-line (car pair))
                (insert "\n"))))))
        (insert "\n"))
      (insert "\n")))

(defun my-chatgpt-shell--insert-edn-summary (edn &optional inbound)
  (let* ((session (or (plist-get edn :session-id) "?"))
         (mode (my-chatgpt-shell--pattern->string (plist-get edn :mode)))
         (clock (or (plist-get edn :clock) "?"))
         (patterns (my-chatgpt-shell--sanitize-entry-list
                   (or (plist-get edn :patterns)
                       (and inbound (plist-get inbound :patterns)))))
         (fruits (my-chatgpt-shell--sanitize-entry-list
                 (or (plist-get edn :fruits)
                     (and inbound (plist-get inbound :fruits)))))
         (paramitas (my-chatgpt-shell--sanitize-entry-list
                    (or (plist-get edn :paramitas)
                        (and inbound (plist-get inbound :paramitas)))))
         (events (my-chatgpt-shell--events-seq (plist-get edn :events)))
         (pattern-limit 3)
         (event-limit 3))
    (insert (propertize "Latest FROM-CHATGPT state" 'face 'bold) "\n")
    (let* ((session-text (let ((val (and session (not (equal session "?")) session)))
                           (when val (format "Session %s" val))))
           (mode-trim (and mode (not (string-empty-p (string-trim mode))) (string-trim mode)))
           (mode-text (when mode-trim (format "Mode %s" mode-trim)))
           (clock-text (let ((val (and clock (not (equal clock "?")) clock)))
                         (when val (format "Clock %s" val))))
           (segments (delq nil (list session-text mode-text clock-text))))
      (if segments
          (insert (string-join segments " | ") "\n")
        (insert "Session details unavailable\n")))
    (insert (format "Intent: %s\n\n"
                    (or (and (plist-get edn :intent)
                             (stringp (plist-get edn :intent))
                             (plist-get edn :intent))
                        my-futon3-tatami-default-intent
                        "unspecified")))
    (when my-chatgpt-shell-last-pattern-outcome
      (my-chatgpt-shell--insert-pattern-outcome my-chatgpt-shell-last-pattern-outcome 200))
    (if patterns
        (progn
          (insert (propertize "Candidate clauses" 'face 'bold)
                  (if (> (length patterns) pattern-limit)
                      (format " (top %d of %d)" pattern-limit (length patterns))
                    "")
                  "\n")
          (cl-loop for pattern in patterns
                   for idx from 0
                   while (< idx pattern-limit)
                   do (progn
                        (my-chatgpt-shell--insert-pattern-line pattern)
                        (insert "\n")))
          (insert "\n"))
      (insert "No candidate clauses available.\n\n"))
    (my-chatgpt-shell--insert-fruit-summary fruits)
    (my-chatgpt-shell--insert-paramita-summary paramitas)
    (insert (propertize "Reasoning trace" 'face 'bold)
            (if (> (length events) event-limit)
                (format " (latest %d of %d)" event-limit (length events))
              "")
            "\n")
    (let ((recent (my-chatgpt-shell--take-last events event-limit)))
      (if recent
          (dolist (event recent)
            (insert "• ")
            (my-chatgpt-shell--insert-event-line event)
            (insert "\n"))
        (insert "No reasoning events recorded yet.\n")))
    (insert "\n")))


(defun my-chatgpt-shell--strip-edn-block (text)
  (let* ((str (or text ""))
         (start-marker (format "---%s---" my-chatgpt-shell-tatami-out-marker))
         (end-marker (format "---END-%s---" my-chatgpt-shell-tatami-out-marker))
         (start (string-match (regexp-quote start-marker) str)))
    (when start
      (let ((end (string-match (regexp-quote end-marker) str start)))
        (when end
          (setq str (concat (substring str 0 start)
                            (substring str (+ end (length end-marker))))))))
    (string-trim str)))

(defun my-chatgpt-shell--remove-edn-from-buffer ()
  (let ((start-marker (format "---%s---" my-chatgpt-shell-tatami-out-marker))
        (end-marker (format "---END-%s---" my-chatgpt-shell-tatami-out-marker)))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (when (search-backward end-marker nil t)
          (let ((end (match-end 0)))
            (when (search-backward start-marker nil t)
              (delete-region (match-beginning 0) end))))))))

(defun my-chatgpt-shell--buffer-edn-string ()
  (let ((start-marker (format "---%s---" my-chatgpt-shell-tatami-out-marker))
        (end-marker (format "---END-%s---" my-chatgpt-shell-tatami-out-marker)))
    (save-excursion
      (goto-char (point-max))
      (when (search-backward end-marker nil t)
        (let ((end (match-beginning 0)))
          (when (search-backward start-marker nil t)
            (let ((start (match-end 0)))
              (buffer-substring-no-properties start end))))))))

(defun my-chatgpt-shell--process-buffer-edn (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (boundp 'my-chatgpt-shell-profile)
                 (member my-chatgpt-shell-profile '("General" "clock-out")))
        (when-let* ((raw (my-chatgpt-shell--buffer-edn-string)))
          (my-chatgpt-shell--remove-edn-from-buffer)
          (when-let* ((edn (my-chatgpt-shell--parse-edn-string raw)))
            (my-chatgpt-shell--debug "Captured FROM-CHATGPT-EDN: %s" (prin1-to-string edn))
            (my-chatgpt-shell--apply-chatgpt-edn edn)))))))

(defun my-chatgpt-shell--refresh-context-buffer ()
  (when (derived-mode-p 'chatgpt-shell-mode)
    (condition-case err
        (progn
          (my-chatgpt-shell--build-inbound-edn))
      (error
       (message "Tatami hint refresh failed: %s" (error-message-string err))))))

(defun my-chatgpt-shell--refresh-context-all ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'chatgpt-shell-mode)
        (my-chatgpt-shell--refresh-context-buffer)))))

(defun my-chatgpt-shell--reset-context-state ()
  "Clear cached Tatami context for every chatgpt-shell buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'chatgpt-shell-mode)
        (setq my-chatgpt-shell-last-edn nil
              my-chatgpt-shell-last-inbound-edn nil
              my-chatgpt-shell-last-pattern-outcome nil
              my-chatgpt-shell-last-validation-warning nil)))))

(defun my-chatgpt-shell-persist-edn (payload)
  "Persist the latest FROM-CHATGPT-EDN PAYLOAD to the HUD log.
Returns the record written, or nil when persistence is disabled."
  (when (and payload my-chatgpt-shell-edn-log-file)
    (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
           (session-id (or (plist-get payload :session-id)
                           (and (listp my-chatgpt-shell-last-inbound-edn)
                                (plist-get my-chatgpt-shell-last-inbound-edn :session-id))))
           (record (list :timestamp timestamp
                         :profile my-chatgpt-shell-profile
                         :buffer (buffer-name)
                         :session-id session-id
                         :tatami-session my-futon3-tatami-session-id
                         :intent (plist-get payload :intent)
                         :mode (plist-get payload :mode)
                         :pattern-outcome my-chatgpt-shell-last-pattern-outcome
                         :validation-warning my-chatgpt-shell-last-validation-warning
                         :tatami my-chatgpt-shell-last-inbound-edn
                         :payload payload)))
      (condition-case err
          (let* ((existing (my-chatgpt-shell--context-log-entries))
                 (entries (vconcat existing (vector record))))
            (my-chatgpt-shell--write-context-log entries))
        (error
         (my-chatgpt-shell--debug "Tatami context log write failed: %s"
                                  (error-message-string err))))
      record)))

(defun my-chatgpt-shell-append-hud-fixture (label)
  "Append the latest Tatami HUD state to `my-chatgpt-shell-hud-fixture-file'.
LABEL is a human-friendly tag describing the captured scenario."
  (interactive "sHUD fixture label: ")
  (unless my-chatgpt-shell-hud-fixture-file
    (user-error "HUD fixture logging is disabled"))
  (unless (and (listp my-chatgpt-shell-last-inbound-edn)
               (listp my-chatgpt-shell-last-edn))
    (user-error "Capture a Tatami turn before recording a HUD fixture"))
  (let* ((expect (cond
                  ((my-chatgpt-shell--support-present-p my-chatgpt-shell-last-inbound-edn)
                   :inbound)
                  ((my-chatgpt-shell--support-present-p my-chatgpt-shell-last-edn)
                   :fallback)
                  (t :missing)))
         (entry (list :label label
                      :timestamp (my-chatgpt-shell--now-iso)
                      :expect expect
                      :inbound my-chatgpt-shell-last-inbound-edn
                      :chatgpt my-chatgpt-shell-last-edn
                      :hints-error my-chatgpt-shell--last-hints-error
                      :cues-error my-chatgpt-shell--last-cues-error)))
    (with-temp-buffer
      (insert (my-chatgpt-shell--encode-entry-edn entry) "\n")
      (write-region (point-min) (point-max)
                    my-chatgpt-shell-hud-fixture-file t 'silent))
    (message "Tatami HUD fixture ‘%s’ captured in %s"
             label my-chatgpt-shell-hud-fixture-file)))

(defun my-chatgpt-shell--log-hints-transaction (request response)
  (when my-chatgpt-shell-hints-log-file
    (let ((entry (list :timestamp (my-chatgpt-shell--now-iso)
                       :request request
                       :response response)))
      (with-temp-buffer
        (insert (my-chatgpt-shell--encode-entry-edn entry) "\n")
        (write-region (point-min) (point-max)
                      my-chatgpt-shell-hints-log-file t 'silent)))))

(defun my-chatgpt-shell--tokenize (text)
  (when (and text (not (string-empty-p text)))
    (let* ((lower (downcase text))
           (tokens (split-string lower "[^[:alnum:]]+" t)))
      (cl-remove-if (lambda (tok) (< (length tok) 3)) tokens))))

(defun my-chatgpt-shell--tatami-patterns (edn)
  (let* ((tatami (plist-get edn :tatami))
         (tatami-patterns (plist-get tatami :patterns))
         (chatgpt-patterns (plist-get edn :patterns))
         (combined (append (my-chatgpt-shell--coerce-seq chatgpt-patterns)
                           (my-chatgpt-shell--coerce-seq tatami-patterns))))
    (my-chatgpt-shell--sanitize-entry-list combined)))

(defun my-chatgpt-shell--sigil-equal-p (a b)
  (and (string= (or (plist-get a :emoji) "")
                (or (plist-get b :emoji) ""))
       (string= (or (plist-get a :hanzi) "")
                (or (plist-get b :hanzi) ""))))

(defun my-chatgpt-shell--dedupe-sigils (sigils)
  (let (acc)
    (dolist (sigil sigils (nreverse acc))
      (when (and sigil
                 (not (seq-some (lambda (existing)
                                  (my-chatgpt-shell--sigil-equal-p sigil existing))
                                acc)))
        (push sigil acc)))))

(defun my-chatgpt-shell--trim-sigils (sigils &optional limit)
  (let ((limit (or limit my-chatgpt-shell-intent-sigil-limit)))
    (cond
     ((or (null sigils) (null limit) (<= limit 0)) sigils)
     ((<= (length sigils) limit) sigils)
     (t (seq-take sigils limit)))))

(defun my-chatgpt-shell--context-sigils (context)
  (when (and context (plist-get context :intent-sigils))
    (let ((entry (list :sigils (plist-get context :intent-sigils)
                       :origin (or (plist-get context :intent-sigils-source)
                                   :unknown)
                       :intent (or (plist-get context :intent-sigils-intent)
                                   (plist-get context :intent)))))
      entry)))

(defun my-chatgpt-shell--sigils-refresh-needed-p (entry intent)
  (let ((origin (plist-get entry :origin))
        (snapshot (plist-get entry :intent)))
    (or (null entry)
        (null (plist-get entry :sigils))
        (null origin)
        (eq origin :prototype)
        (and intent snapshot
             (not (string= intent snapshot))))))

(defun my-chatgpt-shell--assign-intent-sigils (edn sigils origin intent)
  (when edn
    (let ((copy (cl-copy-list edn)))
      (setq copy (plist-put copy :intent-sigils sigils))
      (setq copy (plist-put copy :intent-sigils-source origin))
      (setq copy (plist-put copy :intent-sigils-intent intent))
      copy)))

(defun my-chatgpt-shell--persist-intent-sigils (sigils origin intent)
  (setq sigils (my-chatgpt-shell--trim-sigils
                (my-chatgpt-shell--dedupe-sigils sigils)))
  (when my-chatgpt-shell-last-inbound-edn
    (setq my-chatgpt-shell-last-inbound-edn
          (my-chatgpt-shell--assign-intent-sigils my-chatgpt-shell-last-inbound-edn
                                                  sigils origin intent)))
  (when my-chatgpt-shell-last-edn
    (setq my-chatgpt-shell-last-edn
          (my-chatgpt-shell--assign-intent-sigils my-chatgpt-shell-last-edn
                                                  sigils origin intent)))
  (my-chatgpt-shell--record-hud-state))

(defun my-chatgpt-shell--sigil-source (intent proto-source &optional context)
  (let* ((inbound (and my-chatgpt-shell-last-inbound-edn
                       (my-chatgpt-shell--context-sigils my-chatgpt-shell-last-inbound-edn)))
         (chatgpt (and my-chatgpt-shell-last-edn
                       (my-chatgpt-shell--context-sigils my-chatgpt-shell-last-edn)))
         (defaults (my-futon3--prototype-sigils proto-source))
         (raw (cond
               (inbound (plist-put (cl-copy-list inbound) :status :inbound))
               (chatgpt (plist-put (cl-copy-list chatgpt) :status :chatgpt))
               (defaults (list :sigils defaults :status :default :origin :prototype :intent intent))
               (t (list :sigils nil :status :empty :origin nil :intent intent))))
         (refined raw)
         (sigils (plist-get refined :sigils))
         (origin (plist-get refined :origin)))
    (when (and context intent
               (my-chatgpt-shell--sigils-refresh-needed-p refined intent))
      (let ((fresh (my-chatgpt-shell--sigils-from-intent context intent)))
        (when fresh
          (setq sigils fresh)
          (setq origin (or my-chatgpt-shell--last-intent-sigil-origin :embedded))
          (setq refined (list :sigils sigils
                              :status :inbound
                              :intent intent
                              :origin origin)))))
    (unless origin
      (setq origin (pcase (plist-get refined :status)
                     (:default :prototype)
                     (:chatgpt :chatgpt)
                     (:inbound :embedded)
                     (_ origin)))
      (setq refined (plist-put refined :origin origin)))
    (unless (plist-get refined :intent)
      (setq refined (plist-put refined :intent intent)))
    refined))

(defun my-chatgpt-shell--resolve-intent-sigils ()
  (message "RESOLVING INTENT SIGILS")
  (let* ((intent (or (and my-chatgpt-shell-last-inbound-edn
                          (plist-get my-chatgpt-shell-last-inbound-edn :intent))
                     (and my-chatgpt-shell-last-edn
                          (plist-get my-chatgpt-shell-last-edn :intent))
                     my-futon3-tatami-default-intent))
         (proto-source (or (and my-chatgpt-shell-last-inbound-edn
                                (plist-get my-chatgpt-shell-last-inbound-edn :prototypes))
                           (and my-chatgpt-shell-last-edn
                                (plist-get my-chatgpt-shell-last-edn :prototypes))
                           my-futon3-tatami-default-prototypes))
         (context (or my-chatgpt-shell-last-inbound-edn my-chatgpt-shell-last-edn))
         (source (my-chatgpt-shell--sigil-source intent proto-source context))
         (sigils (my-chatgpt-shell--trim-sigils
                  (my-chatgpt-shell--dedupe-sigils (plist-get source :sigils))))
         (origin (plist-get source :origin))
         (status (plist-get source :status))
         (snapshot-intent (or (plist-get source :intent) intent)))
    (unless sigils
      (setq sigils (my-futon3--prototype-sigils proto-source)
            origin :prototype
            status :default
            snapshot-intent intent)
      (setq sigils (my-chatgpt-shell--trim-sigils
                    (my-chatgpt-shell--dedupe-sigils sigils))))
    (when sigils
      (my-chatgpt-shell--persist-intent-sigils sigils origin snapshot-intent))
    (setq my-chatgpt-shell--last-sigil-salient (list :sigils sigils
                                                     :status status
                                                     :origin origin))
    sigils))

(defun my-chatgpt-shell--normalize-intent-string (value)
  (if (stringp value)
      (string-trim value)
    ""))

(defun my-chatgpt-shell--pattern-derived-sigils (edn intent)
  (let* ((tokens (my-chatgpt-shell--tokenize intent))
         (patterns (my-chatgpt-shell--tatami-patterns edn)))
    (when (and tokens patterns)
      (let (matches)
        (dolist (pattern patterns)
          (let* ((title (downcase (or (my-chatgpt-shell--string-or-nil (plist-get pattern :title)) "")))
                 (summary (downcase (or (my-chatgpt-shell--string-or-nil (plist-get pattern :summary)) "")))
                 (body (concat title " " summary))
                 (overlap (seq-filter (lambda (tok)
                                        (and (> (length tok) 0)
                                             (string-match-p (regexp-quote tok) body)))
                                      tokens)))
            (when overlap
              (setq matches (nconc matches (my-chatgpt-shell--sanitize-entry-list (plist-get pattern :sigils)))))))
        (let ((deduped (my-chatgpt-shell--dedupe-sigils matches)))
          (or deduped
              (let* ((all-sigils (apply #'nconc (mapcar (lambda (pattern)
                                                         (my-chatgpt-shell--sanitize-entry-list (plist-get pattern :sigils)))
                                                       patterns)))
                     (limited (seq-take all-sigils 4)))
                (my-chatgpt-shell--dedupe-sigils limited))))))))

(defun my-chatgpt-shell--sigils-from-intent (edn intent)
  (message "[DEBUG] Intent: %s" intent)
  (let* ((text (my-chatgpt-shell--intent-embedding-text edn intent))
         (embedded (and text (my-futon3--embed-sigils-from-text text))))
    (message "[DEBUG] Text to embed: %s" text)
    (when (and text (null embedded))
      (message "[DEBUG] Intent embedding produced no sigils: %s"
               (my-chatgpt-shell--truncate text 1200)))
    (message "[DEBUG] Embedded: %s" embedded)
    (setq my-chatgpt-shell--last-intent-sigil-origin nil)
    (let ((result
           (cond
            (embedded
             (setq my-chatgpt-shell--last-intent-sigil-origin :embedded)
             embedded)
            (t
             (let ((derived (my-chatgpt-shell--pattern-derived-sigils edn intent)))
               (when derived
                 (setq my-chatgpt-shell--last-intent-sigil-origin :pattern))
               derived)))))
      (my-chatgpt-shell--trim-sigils
       (my-chatgpt-shell--dedupe-sigils result)))))

(defun my-chatgpt-shell--encode-entry-edn (entry)
  (let ((print-escape-newlines t)
        (print-level nil)
        (print-length nil))
    (prin1-to-string entry)))

(defun my-chatgpt-shell--enrich-with-cues (edn)
  (let ((resp (my-futon3-fetch-intent-cues edn))
        (copy (and edn (cl-copy-list edn))))
    (if (and copy resp)
        (progn
          (let ((fruits (plist-get resp :fruits)))
            (when (sequencep fruits)
              (setq copy (plist-put copy :fruits fruits))
              (unless (seq-empty-p fruits)
                (my-chatgpt-shell--debug "[HUD] /musn/cues fruits=%S" fruits))))
          (let ((paramitas (plist-get resp :paramitas)))
            (when (sequencep paramitas)
              (setq copy (plist-put copy :paramitas paramitas))
              (unless (seq-empty-p paramitas)
                (my-chatgpt-shell--debug "[HUD] /musn/cues paramitas=%S" paramitas))))
          (when-let* ((intent (plist-get resp :intent)))
            (setq copy (plist-put copy :cue/intent intent)))
          copy)
      edn)))

(defun my-chatgpt-shell--apply-hints-response (hints payload target-intent context)
  (when hints
    (my-chatgpt-shell--log-hints-transaction payload hints))
  (let* ((base (or (and my-chatgpt-shell-last-inbound-edn
                        (cl-copy-list my-chatgpt-shell-last-inbound-edn))
                   (and context (cl-copy-list context))
                   nil))
         (updated (or base (list :intent target-intent))))
    (setq updated (plist-put updated :intent target-intent))
    (setq updated (plist-put updated :prototypes (or (plist-get context :prototypes)
                                                    (plist-get updated :prototypes)
                                                    my-futon3-tatami-default-prototypes)))
    (when hints
      (setq updated (plist-put updated :patterns (or (plist-get hints :patterns) [])))
      (setq updated (plist-put updated :fruits (or (plist-get hints :fruits) [])))
      (setq updated (plist-put updated :paramitas (or (plist-get hints :paramitas) [])))
      (let ((paramitas (plist-get updated :paramitas))
            (fruits (plist-get updated :fruits)))
        (when (and paramitas (not (seq-empty-p paramitas)))
          (my-chatgpt-shell--debug "[HUD] /musn/hints paramitas=%S" paramitas))
        (when (and fruits (not (seq-empty-p fruits)))
          (my-chatgpt-shell--debug "[HUD] /musn/hints fruits=%S" fruits))))
    (setq updated (my-chatgpt-shell--enrich-with-cues updated))
    (setq my-chatgpt-shell-last-inbound-edn updated)
    (setq my-chatgpt-shell--last-replay-tatami-edn (and updated (cl-copy-list updated)))
    (setq my-chatgpt-shell-last-edn (or context my-chatgpt-shell-last-edn))
    (my-chatgpt-shell--record-hud-state)
    (my-chatgpt-shell--log-hud-manifest
     (my-chatgpt-shell--build-hud-manifest updated context my-chatgpt-shell-last-pattern-outcome))
    (my-chatgpt-shell--render-context t)
      updated))

(defun my-chatgpt-shell--refresh-context-hints (&optional intent)
  "Refresh Tatami HUD hints in-place, optionally forcing INTENT."
  (my-chatgpt-shell--with-state-buffer
    (setq my-chatgpt-shell--context-source (current-buffer))
    (let* ((context (or my-chatgpt-shell-last-edn '()))
           (target-intent (or intent
                              (plist-get context :intent)
                              my-futon3-tatami-default-intent
                              "unspecified"))
           (proto-source (or (plist-get context :prototypes)
                             my-futon3-tatami-default-prototypes))
           (sigil-source (my-chatgpt-shell--sigil-source target-intent proto-source context))
           (sigils (plist-get sigil-source :sigils))
           (sigil-origin (plist-get sigil-source :origin))
           (request (list :intent target-intent
                          :prototypes (or (plist-get context :prototypes)
                                          my-futon3-tatami-default-prototypes)))
           (request (if sigils (plist-put request :sigils sigils) request))
           (base (or (and my-chatgpt-shell-last-inbound-edn
                          (cl-copy-list my-chatgpt-shell-last-inbound-edn))
                     (and context (cl-copy-list context))
                     nil))
           (updated (or base (list :intent target-intent))))
      (setq updated (plist-put updated :intent target-intent))
      (when (and my-chatgpt-shell-last-inbound-edn
                 (not (string= (my-chatgpt-shell--normalize-intent-string
                                (plist-get my-chatgpt-shell-last-inbound-edn :intent))
                               (my-chatgpt-shell--normalize-intent-string target-intent))))
        (setq sigils nil
              sigil-origin nil)
        (setq updated (plist-put updated :intent-sigils nil))
        (setq updated (plist-put updated :intent-sigils-source nil))
        (setq updated (plist-put updated :intent-sigils-intent nil)))
      (setq updated (plist-put updated :prototypes (or (plist-get context :prototypes)
                                                      (plist-get updated :prototypes)
                                                      my-futon3-tatami-default-prototypes)))
      (when (and sigils (not (seq-empty-p sigils)))
        (setq updated (plist-put updated :intent-sigils sigils))
        (setq updated (plist-put updated :intent-sigils-source sigil-origin))
        (setq updated (plist-put updated :intent-sigils-intent target-intent)))
      (when (or (null (plist-get updated :intent-sigils))
                (eq sigil-origin :prototype))
        (let ((after-hints (my-chatgpt-shell--sigils-from-intent updated target-intent)))
          (when after-hints
            (setq sigils after-hints
                  sigil-origin (or my-chatgpt-shell--last-intent-sigil-origin :embedded))
            (setq updated (plist-put updated :intent-sigils sigils))
            (setq updated (plist-put updated :intent-sigils-source sigil-origin))
            (setq updated (plist-put updated :intent-sigils-intent target-intent)))))
      (setq my-chatgpt-shell-last-inbound-edn updated)
      (setq my-chatgpt-shell--last-replay-tatami-edn (and updated (cl-copy-list updated)))
      (setq my-chatgpt-shell-last-edn (or context my-chatgpt-shell-last-edn))
      (my-chatgpt-shell--record-hud-state)
      (my-chatgpt-shell--log-hud-manifest
       (my-chatgpt-shell--build-hud-manifest updated context my-chatgpt-shell-last-pattern-outcome))
      (setq my-chatgpt-shell--hints-pending t)
      (my-chatgpt-shell--render-context t)
      (cl-incf my-chatgpt-shell--hints-request-id)
      (let* ((request-id my-chatgpt-shell--hints-request-id)
             (payload (if my-chatgpt-shell-hints-async-skip-intent
                          (let ((copy (cl-copy-list request)))
                            (plist-put copy :intent nil))
                        request)))
        (if my-chatgpt-shell-hints-async
            (my-futon3-fetch-hints-async
             payload
             (lambda (hints err)
               (when (and (= request-id my-chatgpt-shell--hints-request-id)
                          (buffer-live-p (my-chatgpt-shell--state-buffer)))
                 (my-chatgpt-shell--with-state-buffer
                   (setq my-chatgpt-shell--hints-pending nil)
                   (when hints
                     (my-chatgpt-shell--apply-hints-response hints payload target-intent context))))))
          (let ((hints (my-futon3-fetch-hints request)))
            (setq my-chatgpt-shell--hints-pending nil)
            (when hints
              (my-chatgpt-shell--apply-hints-response hints request target-intent context)))))
      updated)))

(defun my-chatgpt-shell-refresh-context-hints (&optional intent)
  "Interactively refresh Tatami HUD hints, optionally forcing INTENT."
  (interactive
   (list (when current-prefix-arg
           (read-string "Intent (blank for current): "))))
  (when (and (stringp intent) (string-empty-p intent))
    (setq intent nil))
  (my-chatgpt-shell--refresh-context-hints intent))

(defun my-chatgpt-shell--hints-debug-report (&optional intent)
  (let* ((context (or my-chatgpt-shell-last-inbound-edn
                      my-chatgpt-shell-last-edn)))
    (unless context
      (user-error "No Tatami/ChatGPT context available yet"))
    (let* ((target-intent (or intent
                              (plist-get context :intent)
                              my-futon3-tatami-default-intent
                              "unspecified"))
           (proto-source (or (plist-get context :prototypes)
                             my-futon3-tatami-default-prototypes))
           (sigil-source (my-chatgpt-shell--sigil-source target-intent proto-source context))
           (sigils (plist-get sigil-source :sigils))
           (request (list :intent target-intent
                          :prototypes (or (plist-get context :prototypes)
                                          my-futon3-tatami-default-prototypes)))
           (request (if sigils (plist-put request :sigils sigils) request))
           (hints (my-futon3-fetch-hints request))
           (cues (my-futon3-fetch-intent-cues context)))
      (when hints
        (my-chatgpt-shell--log-hints-transaction request hints))
      (with-temp-buffer
        (insert "Tatami hints/cues debug\n\n")
        (insert (format "Intent: %s\n" target-intent))
        (let* ((raw-protos (my-chatgpt-shell--coerce-seq (plist-get request :prototypes)))
               (protos (mapcar (lambda (proto)
                                 (cond
                                  ((keywordp proto) (substring (symbol-name proto) 1))
                                  ((symbolp proto) (symbol-name proto))
                                  ((stringp proto) proto)
                                  (t (format "%s" proto))))
                               raw-protos)))
          (insert (format "Prototypes: %s\n"
                          (if protos (string-join protos ", ") "(none)"))))
        (insert (format "Sigils: %s\n\n" (or sigils "(none)")))
        (insert (format "Hints error: %s\n" (or my-chatgpt-shell--last-hints-error "none")))
        (insert (format "Cues error: %s\n\n" (or my-chatgpt-shell--last-cues-error "none")))
        (let ((patterns (and hints (plist-get hints :patterns)))
              (fruits (and hints (plist-get hints :fruits)))
              (paramitas (and hints (plist-get hints :paramitas))))
          (insert (format "Hints patterns: %s\n" (if patterns (length patterns) 0)))
          (insert (format "Hints fruits: %s\n" (if fruits (length fruits) 0)))
          (insert (format "Hints paramitas: %s\n\n" (if paramitas (length paramitas) 0))))
        (let ((fruits (and cues (plist-get cues :fruits)))
              (paramitas (and cues (plist-get cues :paramitas))))
          (insert (format "Cues fruits: %s\n" (if fruits (length fruits) 0)))
          (insert (format "Cues paramitas: %s\n" (if paramitas (length paramitas) 0)))
          (insert "\n"))
        (when hints
          (insert "Hints payload:\n")
          (pp hints (current-buffer))
          (insert "\n"))
        (when cues
          (insert "Cues payload:\n")
          (pp cues (current-buffer)))
        (buffer-string)))))

(defun my-chatgpt-shell-test-hints-and-cues (&optional intent)
  "Fetch /musn/hints and /musn/cues for the latest turn and report counts."
  (interactive
   (list (when current-prefix-arg
           (read-string "Intent (blank for current): "))))
  (when (and (stringp intent) (string-empty-p intent))
    (setq intent nil))
  (let ((report (my-chatgpt-shell--hints-debug-report intent)))
    (with-current-buffer (get-buffer-create "*Tatami Hints Debug*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report)))
    (display-buffer "*Tatami Hints Debug*")))

(defcustom my-chatgpt-shell-hints-debug-file
  (expand-file-name "resources/hints-debug.txt" my-futon3--repo-root)
  "Path where hints/cues debug snapshots are written."
  :type 'file
  :group 'tatami-integration)

(defun my-chatgpt-shell-replay-last-turn-debug-hints (&optional intent)
  "Replay the last turn, refresh hints, and write a debug report to disk."
  (interactive
   (list (when current-prefix-arg
           (read-string "Intent (blank for current): "))))
  (my-chatgpt-shell-replay-last-turn)
  (my-chatgpt-shell-refresh-context-hints intent)
  (let ((report (my-chatgpt-shell--hints-debug-report intent)))
    (make-directory (file-name-directory my-chatgpt-shell-hints-debug-file) t)
    (with-temp-file my-chatgpt-shell-hints-debug-file
      (insert report))
    (message "Tatami hints debug written to %s" my-chatgpt-shell-hints-debug-file)))

(defun my-chatgpt-shell--build-inbound-edn ()
  (my-futon3-ensure-tatami-session)
  (let* ((existing (and my-chatgpt-shell-last-edn
                        (cl-copy-list my-chatgpt-shell-last-edn)))
         (session (or (plist-get existing :session-id)
                      my-futon3-tatami-session-id))
         (intent-value (or (and existing (plist-get existing :intent))
                            my-futon3-tatami-default-intent
                           "unspecified"))
         (proto-source (or (plist-get existing :prototypes)
                           my-futon3-tatami-default-prototypes))
         (sigil-source (my-chatgpt-shell--sigil-source intent-value proto-source existing))
         (sigils (plist-get sigil-source :sigils))
         (sigil-origin (plist-get sigil-source :origin))
         (request (list :intent intent-value
                        :prototypes (my-chatgpt-shell--prototype-keywords)))
         (request (if sigils (plist-put request :sigils sigils) request))
         (hints (my-futon3-fetch-hints request))
         (edn (or existing '())))
    (setq edn (plist-put edn :session-id session))
    (setq edn (plist-put edn :mode (or (plist-get edn :mode) :ambient)))
    (setq edn (plist-put edn :clock (my-chatgpt-shell--now-iso)))
    (setq edn (plist-put edn :futons (my-chatgpt-shell--current-futons)))
    (setq edn (plist-put edn :prototypes (my-chatgpt-shell--prototype-keywords)))
    (setq edn (plist-put edn :intent intent-value))
    (when sigils
      (setq edn (plist-put edn :intent-sigils sigils))
      (setq edn (plist-put edn :intent-sigils-source sigil-origin))
      (setq edn (plist-put edn :intent-sigils-intent intent-value)))
    (setq edn (plist-put edn :patterns (or (plist-get hints :patterns) [])))
    (setq edn (plist-put edn :fruits (or (plist-get hints :fruits) [])))
    (setq edn (plist-put edn :paramitas (or (plist-get hints :paramitas) [])))
    (when (or (null sigils)
              (eq sigil-origin :prototype))
      (let ((after-hints (my-chatgpt-shell--sigils-from-intent edn intent-value)))
        (when after-hints
          (setq sigils after-hints
                sigil-origin (or my-chatgpt-shell--last-intent-sigil-origin :embedded))
          (setq edn (plist-put edn :intent-sigils sigils))
          (setq edn (plist-put edn :intent-sigils-source sigil-origin))
          (setq edn (plist-put edn :intent-sigils-intent intent-value)))))
    (setq edn (my-chatgpt-shell--enrich-with-cues edn))
    (setq edn (plist-put edn :events (or (plist-get edn :events) [])))
    (setq my-chatgpt-shell-last-inbound-edn edn)
    (my-chatgpt-shell--record-hud-state)
    (my-chatgpt-shell--log-hud-manifest
     (my-chatgpt-shell--build-hud-manifest edn existing my-chatgpt-shell-last-pattern-outcome))
    (setq my-chatgpt-shell--last-replay-tatami-edn (and edn (cl-copy-list edn)))
    (my-chatgpt-shell--render-context)
    edn))

;; --- Prompt builder for futon1 ----------------------------------------

(defun my-chatgpt-shell--profile-uses-edn-p (profile)
  "Return non-nil when PROFILE should include Tatami/EDN context."
  (member profile '("General" "clock-out")))

(defun my-chatgpt-shell--build-general-system-prompt (profile-body)
  "Compose the Futon General system prompt, optionally appending PROFILE-BODY."
  (let* ((now   (current-time))
         (date  (format-time-string "%A, %B %d, %Y" now))
         (time  (format-time-string "%H:%M" now))
         (context
          (if my-chatgpt-shell--seeded-context
              (or (my-chatgpt-shell--focus-snippet)
                  "Current focus unavailable.")
            (setq my-chatgpt-shell--seeded-context t)
            (or (when-let* ((sum (tatami-me-summary-safe)))
                  (setq my-chatgpt-shell-last-summary (my-chatgpt-shell--format-summary sum))
                  (my-chatgpt-shell--maybe-render-context)
                  sum)
                "Current profile summary unavailable.")))
         (tatami-edn (my-chatgpt-shell--build-inbound-edn))
         (tatami-block (my-chatgpt-shell--format-edn-block
                        my-chatgpt-shell-tatami-in-marker tatami-edn))
         (adjacency-brief (or (my-chatgpt-shell--prompt-pattern-summary tatami-edn)
                              "No adjacency cues supplied.")))
    (concat
     "You use markdown liberally to structure responses. "
     "Always show code snippets in markdown blocks with language labels. "
     "The user’s most recent query has been submitted at the following date: "
     date " and the local time (in the London timezone) is " time
     ". Please preface all your replies with the metadata ["
     date "/" time "].\n"
     "When giving answers related to time, do not use any other cached time values.\n\n"
     "Current working intent: " (or my-futon3-tatami-default-intent "unspecified") "\n"
     "Infer or refine this intent (<= 10 words) each turn and write it to the FROM-CHATGPT-EDN :intent key so Futon3 can retarget you.\n"
     "If you are unsure, describe the **next best question or artifact** that would advance the proof.\n\n"
     "Primary directive: perform pattern-theoretic inference (deduction, abduction, induction) over the supplied Tatami hints. Keep any direct response to the user's explicit question to roughly 100 words, then devote the rest of the turn to analysing patterns and adjacent structures.\n"
     "Treat all candidate clauses as proof obligations: compare them, combine them creatively, and identify missing premises so Futon3 can learn from gaps.\n\n"
     "Pattern reasoning checklist (use this to populate the FROM-CHATGPT-EDN :events block; keep your visible reply focused on the user's request):\n"
     "1. Inspect every entry in :patterns (and supporting fruits/pāramitās) from the inbound EDN.\n"
     "2. Decide whether one or more clauses apply to the user's latest move.\n"
     "3. When a clause applies, capture its ID and justification inside :events rather than in the visible reply.\n"
     "4. If none apply, record {:pattern :none :notes \"Reason\"} explaining the blocker.\n"
     "5. Every turn must append at least one {:kind :note ...} event so the HUD can render the verdict later.\n\n"
     "Keep the user-facing reply on the concrete ask, and rely on the HUD/EDN log for detailed pattern reporting.\n\n"
     "Adjacency briefing (top hints):\n" adjacency-brief "\n\n"
     "After answering the user in clear markdown, append a block delimited by ---"
     my-chatgpt-shell-tatami-out-marker
     "--- and ---END-" my-chatgpt-shell-tatami-out-marker
     "--- containing valid EDN that mirrors and updates the keys shown below. "
     "Only include keys whose values changed this turn (e.g., :intent, updated :events); do NOT echo the inbound :patterns/fruits etc. unless you are altering them. "
     "Do not include commentary or markdown inside that EDN block.\n\n"
     "Here is the current FROM-TATAMI-EDN block for this turn. These patterns are the candidate clauses you must reason about. Follow the checklist/template above, then append the required :events entry (e.g., {:kind :note :pattern \"t4r/rationale\" :notes \"Matched because...\"}).\nDo not mirror the block blindly—use these hints to guide your reasoning and report the result.\nHere is the block:\n"
     tatami-block "\n\n"
     context "\n\n"
     (or profile-body
         "No profile-specific system prompt was found; respond helpfully and clearly."))))

(defun my-chatgpt-shell--build-profile-system-prompt (profile profile-body)
  "Compose a non-EDN prompt for PROFILE using PROFILE-BODY when available."
  (or profile-body
      (format "No %s.prompt was found; respond helpfully and clearly." profile)))

(defun set-user-system-prompt (_command)
  "Build a context-aware system prompt for chatgpt-shell.
Includes date/time, Tatami summary/focus when enabled, and an optional
profile-specific prompt loaded from `my-futon-prompt-directory`."
  (let* ((profile (or my-chatgpt-shell-profile "General"))
         (profile-body (my-futon-read-prompt profile))
         (prompt (if (my-chatgpt-shell--profile-uses-edn-p profile)
                     (my-chatgpt-shell--build-general-system-prompt profile-body)
                   (my-chatgpt-shell--build-profile-system-prompt profile profile-body))))
    (setq chatgpt-shell-system-prompts
          `(("tl;dr" . "Be as succinct but informative as possible and respond in tl;dr form to my queries")
            (,profile . ,prompt)))
    (setq-local chatgpt-shell-system-prompt prompt)))


;;; Launchers

(defun my-chatgpt-shell--spawn (profile buffer-name)
  "Open a new chatgpt-shell BUFFER-NAME configured for PROFILE."
  (let ((buf (chatgpt-shell)))
    (with-current-buffer buf
      (rename-buffer (generate-new-buffer-name buffer-name) t)
      (my-chatgpt-shell-set-profile profile))
    buf))

(defun par-case-shell ()
  "Open a chatgpt-shell configured for PAR → Case File work."
  (interactive)
  (my-chatgpt-shell--spawn "par-case-shell" "*PAR Case ChatGPT*"))

(defalias 'par-shell #'par-case-shell)

(defun paramita-shell ()
  "Open a chatgpt-shell configured for Pāramitā signature work."
  (interactive)
  (my-chatgpt-shell--spawn "paramita-shell" "*Paramita ChatGPT*"))

(global-set-key (kbd "C-c P") #'par-shell)
(global-set-key (kbd "C-c M") #'paramita-shell)

(defun futon0-insert-clock-out-prompt ()
  "Insert the futon0 clock-out prompt into the current chatgpt-shell buffer."
  (interactive)
  (my-chatgpt-shell--insert-prompt "futon0-clock-out" "clock-out" t))

(defun par-case-insert-prompt ()
  "Insert the PAR → Case File helper prompt into the current chat buffer."
  (interactive)
  (my-chatgpt-shell--insert-prompt "par-case-shell" "par-case-shell" t))

(defun paramita-insert-prompt ()
  "Insert the Pāramitā signature prompt for a single turn."
  (interactive)
  (my-chatgpt-shell--insert-prompt "paramita-shell" "paramita-shell" t))


;;; Advice

(defun my-chatgpt-shell-before-command (command)
  "Wrapper for `chatgpt-shell-before-command-functions`.

Always sets the system prompt. Tatami ingestion now occurs via
`my-chatgpt-shell-after-command` so the LLM response is not blocked."
  (set-user-system-prompt command))

(defun my-chatgpt-shell--context-buffer (&optional ensure)
  (let ((buf (if ensure
                 (get-buffer-create my-chatgpt-shell-context-buffer-name)
               (get-buffer my-chatgpt-shell-context-buffer-name))))
    (when (and ensure buf)
      (with-current-buffer buf
        (set-buffer-multibyte t)
        (setq-local buffer-file-coding-system 'utf-8-unix)
        (unless my-chatgpt-shell--context-face-cookie
          (setq my-chatgpt-shell--context-face-cookie
                (face-remap-add-relative 'default
                                         `(:height ,my-chatgpt-shell-context-font-scale))))
        (visual-line-mode 1)
        (setq-local truncate-lines nil)))
    buf))

(defun my-chatgpt-shell--render-context (&optional ensure)
  (let* ((state-buffer (or (and (derived-mode-p 'chatgpt-shell-mode)
                                (setq my-chatgpt-shell--context-source (current-buffer)))
                           (and (buffer-live-p my-chatgpt-shell--context-source)
                                my-chatgpt-shell--context-source)
                           (current-buffer)))
         (summary (buffer-local-value 'my-chatgpt-shell-last-summary state-buffer))
         (focus (buffer-local-value 'my-chatgpt-shell-last-focus state-buffer))
         (inbound (buffer-local-value 'my-chatgpt-shell-last-inbound-edn state-buffer))
         (last-edn (buffer-local-value 'my-chatgpt-shell-last-edn state-buffer))
         (state-pattern (buffer-local-value 'my-chatgpt-shell-last-pattern-outcome state-buffer))
         (pattern-outcome state-pattern)
         (last-events (and last-edn
                           (my-chatgpt-shell--events-seq (plist-get last-edn :events)))))
    ;; Layout order: selection/intent summary, focus header, pattern verdict,
    ;; Tatami hint digest, :me snapshot, and latest FROM-CHATGPT turn.
    (when-let* ((buf (my-chatgpt-shell--context-buffer ensure)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (derived-mode-p 'special-mode)
            (special-mode))
          (setq-local truncate-lines nil)
          (let* ((status (and (my-futon3-running-p)
                              (or (and (not my-chatgpt-shell--hints-pending)
                                       (my-futon3-refresh-status))
                                  my-futon3-last-status)))
                 (stack (and status (plist-get status :stack)))
                 (selection (and status (plist-get status :selection)))
                 (active-protos (or (and selection (plist-get selection :prototypes))
                                    my-futon3-tatami-default-prototypes))
                 (raw-proto-ids (delq nil (mapcar #'my-futon3--prototype-string
                                                  (my-futon3--prototype-list active-protos))))
                 (qualified? (seq-some (lambda (id) (string-match-p "/" id)) raw-proto-ids))
                 (detail-source (if qualified?
                                    active-protos
                                  (or my-futon3-tatami-default-prototypes active-protos)))
                 (sel-protos (my-futon3--display-prototypes active-protos))
                 (sel-details (my-futon3--prototype-detail-lines detail-source))
                 (sel-intent (or (and selection (plist-get selection :intent))
                                 my-futon3-tatami-default-intent
                                 "unspecified"))
                 (pattern-slug (or my-futon3-active-pattern
                                    (and selection (plist-get selection :pattern))))
                 (cached-sigils (my-chatgpt-shell--context-sigils (or inbound last-edn)))
                 (intent-sigils (or (and cached-sigils
                                         (plist-get cached-sigils :sigils))
                                    (let ((source-buffer (and (buffer-live-p state-buffer)
                                                              state-buffer)))
                                      (if source-buffer
                                          (with-current-buffer source-buffer
                                            (my-chatgpt-shell--resolve-intent-sigils))
                                        (my-chatgpt-shell--resolve-intent-sigils)))))
                 (intent-sigil-text (my-chatgpt-shell--format-sigils intent-sigils)))
            (when (and cached-sigils (plist-get cached-sigils :sigils))
              (setq my-chatgpt-shell--last-sigil-salient
                    (list :sigils (plist-get cached-sigils :sigils)
                          :status (or (plist-get cached-sigils :origin) :cached)
                          :origin (plist-get cached-sigils :origin))))
            (insert (propertize "Clocked session" 'face 'bold) "\n")
            (insert (format "Prototype: %s\n"
                            (if sel-protos
                                (string-join sel-protos ", ")
                              "(none selected)")))
            (insert "Pattern: ")
            (if pattern-slug
                (progn
                  (my-chatgpt-shell--insert-pattern-button pattern-slug pattern-slug)
                  (insert " (C-c C-e to edit)"))
              (insert "(unset; C-c C-a to choose)"))
            (insert "\n")
            (insert (format "Intent: %s%s\n"
                            sel-intent
                            (if intent-sigil-text
                                (format " %s" intent-sigil-text)
                              "")))
            (when-let* ((salients (my-chatgpt-shell--salient-tags)))
              (insert (format "Salients: %s\n" (string-join salients ", "))))
            (cond
             (sel-details
              (dolist (detail sel-details)
                (insert (format "    • %s\n" detail))))
             (active-protos
              (insert "    • Prototype metadata unavailable; check devmaps for details.\n")))
            (when (or sel-protos sel-details active-protos)
              (insert "\n"))
            (stack-hud--render-context stack)
            (when (and status (plist-get status :events))
              (insert (format "Events: %s\nProof summaries today: %s\n\n"
                              (plist-get status :events)
                              (if (plist-get status :proofs?) "yes" "no"))))
            (insert (propertize "Latest focus header" 'face 'bold) "\n")
            (if focus
                (insert focus "\n\n")
              (insert "No Tatami focus header captured yet." "\n\n"))
            (when (or pattern-outcome last-events inbound)
              (let ((intent-text (or (and last-edn (plist-get last-edn :intent))
                                     sel-intent)))
                 (my-chatgpt-shell--insert-pattern-verdict pattern-outcome last-events
                                                          (and inbound (plist-get inbound :patterns))
                                                          intent-text)))
            (let* ((cue-source (my-chatgpt-shell--cue-source inbound last-edn))
                   (cue-edn (plist-get cue-source :edn))
                   (cue-status (plist-get cue-source :status)))
              (cond
               ((and cue-edn (eq cue-status :inbound))
                (my-chatgpt-shell--insert-inbound-summary cue-edn))
               ((and cue-edn (eq cue-status :chatgpt))
                (when-let* ((msg (my-chatgpt-shell--cue-guidance cue-status
                                                               (plist-get cue-source :reason))))
                  (my-chatgpt-shell--insert-cue-guidance msg))
                (my-chatgpt-shell--insert-inbound-summary cue-edn))
               ((eq cue-status :empty)
                (when-let* ((msg (my-chatgpt-shell--cue-guidance cue-status
                                                               (plist-get cue-source :reason))))
                  (my-chatgpt-shell--insert-cue-guidance msg))))))
            (when summary
              (insert (propertize "Latest :me summary" 'face 'bold) "\n"
                      summary "\n"))
            (when last-edn
              (my-chatgpt-shell--insert-edn-summary last-edn inbound))
            (goto-char (point-min)))))))

(defun my-chatgpt-shell--read-sample-text ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string "Paste sample transcript (with EDN blocks): ")))

(defun my-chatgpt-shell-fake-hud-from-text (&optional text)
  "Simulate a HUD update using TEXT containing Tatami/ChatGPT EDN blocks.
Parses the inbound/outbound EDN markers (---FROM-TATAMI-EDN--- /
---FROM-CHATGPT-EDN---) from TEXT, applies them as if the LLM had just
responded, and refreshes the Tatami Context HUD. When called interactively,
use the active region if present, otherwise prompt for the sample string."
  (interactive)
  (let* ((sample (or text (my-chatgpt-shell--read-sample-text)))
         (chatgpt-raw (or (my-chatgpt-shell--extract-edn-block sample my-chatgpt-shell-tatami-out-marker)
                          (user-error "No FROM-CHATGPT-EDN block found.")))
         (tatami-raw (my-chatgpt-shell--extract-edn-block sample my-chatgpt-shell-tatami-in-marker))
         (chatgpt-edn (my-chatgpt-shell--parse-edn-string chatgpt-raw))
         (tatami-edn (and tatami-raw (my-chatgpt-shell--parse-edn-string tatami-raw))))
    (unless chatgpt-edn
      (user-error "Could not parse FROM-CHATGPT-EDN payload."))
    (when tatami-edn
      (setq my-chatgpt-shell-last-inbound-edn tatami-edn)
      (my-chatgpt-shell--record-hud-state))
    (my-chatgpt-shell--apply-chatgpt-edn chatgpt-edn)
    (my-chatgpt-shell--render-context t)
    (message "Tatami HUD updated from sample payload.")))

(defun my-chatgpt-shell--warn-on-hud-drift (buffer snapshot)
  "Log when BUFFER's live state and SNAPSHOT disagree." 
  (let* ((live-edn (and (buffer-live-p buffer)
                        (buffer-local-value 'my-chatgpt-shell-last-inbound-edn buffer)))
         (snapshot-edn (plist-get snapshot :inbound))
         (live-paramitas (and live-edn (plist-get live-edn :paramitas)))
         (snapshot-paramitas (and snapshot-edn (plist-get snapshot-edn :paramitas))))
    (when (and live-paramitas snapshot-paramitas
               (not (equal live-paramitas snapshot-paramitas)))
      (my-chatgpt-shell--debug
       "[HUD] snapshot drift detected for %s (live=%S snapshot=%S)"
       buffer live-paramitas snapshot-paramitas))))

(defun my-chatgpt-shell--maybe-render-context ()
  (let* ((source (my-chatgpt-shell--state-buffer))
         (snapshot (and source (my-chatgpt-shell--hud-snapshot source)))
         (buf (my-chatgpt-shell--context-buffer t)))
    (cond
     ((buffer-live-p source)
      (setq my-chatgpt-shell--context-source source)
      (when snapshot
        (my-chatgpt-shell--warn-on-hud-drift source snapshot))
      (setq my-chatgpt-shell-last-inbound-edn
            (my-chatgpt-shell--hud-copy (buffer-local-value 'my-chatgpt-shell-last-inbound-edn source)))
      (setq my-chatgpt-shell-last-edn
            (my-chatgpt-shell--hud-copy (buffer-local-value 'my-chatgpt-shell-last-edn source)))
      (setq my-chatgpt-shell-last-summary
            (buffer-local-value 'my-chatgpt-shell-last-summary source))
      (setq my-chatgpt-shell-last-focus
            (buffer-local-value 'my-chatgpt-shell-last-focus source))
      (setq my-chatgpt-shell-last-pattern-outcome
            (buffer-local-value 'my-chatgpt-shell-last-pattern-outcome source))
      (setq my-chatgpt-shell-last-validation-warning
            (buffer-local-value 'my-chatgpt-shell-last-validation-warning source)))
     (snapshot
      (setq my-chatgpt-shell--context-source nil)
      (setq my-chatgpt-shell-last-inbound-edn (plist-get snapshot :inbound))
      (setq my-chatgpt-shell-last-edn (plist-get snapshot :chatgpt))
      (setq my-chatgpt-shell-last-summary (plist-get snapshot :summary))
      (setq my-chatgpt-shell-last-focus (plist-get snapshot :focus))
      (setq my-chatgpt-shell-last-pattern-outcome (plist-get snapshot :pattern))
      (setq my-chatgpt-shell-last-validation-warning nil))
     (t
      (message "Tatami context unavailable; capture a turn to seed HUD state.")))
    (my-chatgpt-shell--render-context t)
    (my-chatgpt-shell--ensure-context-window buf)))

(defun my-chatgpt-shell-toggle-context ()
  "Toggle display of the Tatami context HUD at the bottom of the frame."
  (interactive)
  (let ((buf (my-chatgpt-shell--context-buffer t)))
    (if-let* ((win (get-buffer-window buf)))
        (progn
          (my-chatgpt-shell--delete-context-windows)
          (setq my-chatgpt-shell--context-window nil))
      (unless (or my-chatgpt-shell-last-summary
                  my-chatgpt-shell-last-focus)
        (when-let* ((sum (tatami-me-summary-safe)))
          (setq my-chatgpt-shell-last-summary
                (my-chatgpt-shell--format-summary sum))
          (my-chatgpt-shell--render-context t)))
      (if (and (null my-chatgpt-shell-last-summary)
               (null my-chatgpt-shell-last-focus))
          (message "Tatami context unavailable; capture a turn or ensure the headless server is running.")
        (my-chatgpt-shell--maybe-render-context)))))

(defun my-chatgpt-shell--ingest (text)
  "Safely send TEXT to Tatami without surfacing hard errors."
  (unless (and my-chatgpt-shell--tatami-disabled
               (progn (setq my-chatgpt-shell--tatami-disabled nil) nil))
    (let ((waited 0)
        (max-wait 3)
        (sleep 0.3))
    (while (and (< waited max-wait)
                (not (tatami-available-p)))
      (sleep-for sleep)
      (setq waited (+ waited sleep)))
    (if (tatami--server-running-p)
        (condition-case err
            (let ((tatami-startup-wait 20))
              (when-let* ((fh (tatami-send-sentences text)))
                (my-chatgpt-shell--format-focus fh)))
          (error
           (message "Tatami ingestion skipped: %s (%s)"
                    (error-message-string err)
                    (or (tatami-last-error) "no tatami-last-error"))))
      (message "Tatami ingestion skipped: headless API unavailable (waited %.1fs)." waited)))))

(defun my-chatgpt-shell-after-command (command output _success)
  "Capture FROM-CHATGPT-EDN payloads every turn; only General chats ingest Tatami."
  (let ((profile (and (boundp 'my-chatgpt-shell-profile)
                      my-chatgpt-shell-profile)))
    (message "DEBUG: [tatami] after-command profile=%s output=%s"
             profile
             (and output (substring output 0 (min 40 (length output)))))
    ;; BUG NOTE: chatgpt-shell-after-command currently hands us only the
    ;; *timestamp header* (see https://github.com/xenodium/chatgpt-shell/issues/412),
    ;; so we cannot rely on OUTPUT to contain the FROM-CHATGPT-EDN block. Instead
    ;; we grab the block directly from the buffer, process it asynchronously, and
    ;; then strip it from the visible chat. Once the upstream bug is fixed we can
    ;; simplify this collector.
    (when profile
      (let ((buffer (current-buffer)))
        (run-at-time 0 nil #'my-chatgpt-shell--process-buffer-edn buffer)))
    (when (string= profile "General")
      (my-chatgpt-shell--ingest command)
      (my-futon3-log-chatgpt-turn command))))

(advice-add 'chatgpt-shell :before
            (lambda (&rest _ignore)
              (my-futon3-ensure-running)
              ;; Force interactive start so we surface failures in the minibuffer.
              (setq tatami--last-command tatami-start-command)
              (when (tatami--launch-server t)
                (run-at-time 0.2 nil #'my-tatami--notify-ready))))

(defvar my-tatami--notify-timer nil)
(defvar my-tatami--notify-start nil)

(defun my-tatami--notify-ready ()
  "Emit a message when the headless server becomes reachable."
  (setq my-tatami--notify-start (float-time))
  (when (timerp my-tatami--notify-timer)
    (cancel-timer my-tatami--notify-timer))
  (setq my-tatami--notify-timer
        (run-with-timer 0.5 0.5 #'my-tatami--notify-ready-tick)))

(defun my-tatami--notify-ready-tick ()
  (let* ((start (if (numberp my-tatami--notify-start)
                    my-tatami--notify-start
                  (float-time)))
         (timer my-tatami--notify-timer))
    (unless (numberp my-tatami--notify-start)
      (setq my-tatami--notify-start start))
    (if (tatami--server-running-p)
        (progn
          (when (timerp timer)
            (cancel-timer timer))
          (setq my-tatami--notify-timer nil)
          (message "Headless server available after %.1fs"
                   (- (float-time) start)))
      (when (> (- (float-time) start) 10)
        (when (timerp timer)
          (cancel-timer timer))
        (setq my-tatami--notify-timer nil)
        (message "Headless server still unavailable after %.1fs"
                 (- (float-time) start))))))

(add-hook 'chatgpt-shell-before-command-functions #'my-chatgpt-shell-before-command)
(add-hook 'chatgpt-shell-after-command-functions #'my-chatgpt-shell-after-command)

;;; Minor mode with lighters

;; --- Mode-line lighter for futon/chatgpt shells --------------------------

(defvar-local my-chatgpt-shell-profile "General"
  "Name of the current futon prompt profile for this chatgpt-shell buffer.
Used both for selecting the system prompt and for the mode-line lighter.")

(defun futon-profile-lighter ()
  "Return a short mode-line lighter based on `my-chatgpt-shell-profile`."
  (pcase my-chatgpt-shell-profile
    ("par-shell"      " PAR")
    ("par-case-shell" " PC")
    ("paramita-shell" " Π")
    ("clock-out"      " CO")
    ("General"        " F0")
    (_                (concat " " my-chatgpt-shell-profile))))

(define-minor-mode futon-profile-mode
  "Minor mode to show the futon/chatgpt profile in the mode line."
  :init-value t
  :lighter '(:eval (futon-profile-lighter)))

(add-hook 'chatgpt-shell-mode-hook #'futon-profile-mode)

(define-key chatgpt-shell-mode-map (kbd "C-c C-t") #'my-chatgpt-shell-toggle-context)
(define-key chatgpt-shell-mode-map (kbd "C-c C-a") #'my-futon3-set-active-pattern)
(define-key chatgpt-shell-mode-map (kbd "C-c C-e") #'my-futon3-open-active-pattern)
(define-key chatgpt-shell-mode-map (kbd "C-c C-o") #'futon0-insert-clock-out-prompt)
(define-key chatgpt-shell-mode-map (kbd "C-c C-p") #'par-case-insert-prompt)
(define-key chatgpt-shell-mode-map (kbd "C-c C-m") #'paramita-insert-prompt)
(define-key chatgpt-shell-mode-map (kbd "C-c C-g") #'futon-set-general-profile)

(defun my-chatgpt-shell--format-summary (text)
  (when text
    (if (string-match "Generated at: \([0-9]+\)" text)
        (let* ((ms (string-to-number (match-string 1 text)))
               (ts (format-time-string "%Y-%m-%d %H:%M:%S (%Z)"
                                       (seconds-to-time (/ ms 1000.0)))))
          (replace-match (format "Generated at: %s" ts) t t text))
      text)))
(defun my-chatgpt-shell--apply-chatgpt-edn (edn)
  (let ((enriched (my-chatgpt-shell--enrich-with-cues edn)))
    (when (derived-mode-p 'chatgpt-shell-mode)
      (setq my-chatgpt-shell--context-source (current-buffer)))
    (let ((sigils (and enriched (plist-get enriched :intent-sigils))))
      (when sigils
        (setq sigils (my-chatgpt-shell--trim-sigils
                      (my-chatgpt-shell--dedupe-sigils sigils)))
        (unless (equal sigils (plist-get enriched :intent-sigils))
          (setq enriched (cl-copy-list enriched))
          (setq enriched (plist-put enriched :intent-sigils sigils)))
        (let ((needs-copy (or (null (plist-get enriched :intent-sigils-source))
                              (null (plist-get enriched :intent-sigils-intent)))))
          (when needs-copy
            (setq enriched (cl-copy-list enriched)))
          (unless (plist-get enriched :intent-sigils-source)
            (setq enriched (plist-put enriched :intent-sigils-source :chatgpt)))
          (unless (plist-get enriched :intent-sigils-intent)
            (setq enriched (plist-put enriched :intent-sigils-intent
                                       (plist-get enriched :intent)))))))
    (setq enriched (my-chatgpt-shell--decorate-support-events enriched my-chatgpt-shell-last-inbound-edn))
    (setq my-chatgpt-shell--last-replay-chatgpt-edn (and enriched (cl-copy-list enriched)))
    (setq my-chatgpt-shell-last-edn enriched)
    (when-let* ((sigils (plist-get enriched :intent-sigils)))
      (let ((origin (or (plist-get enriched :intent-sigils-source) :chatgpt))
            (intent (or (plist-get enriched :intent-sigils-intent)
                        (plist-get enriched :intent))))
        (my-chatgpt-shell--persist-intent-sigils sigils origin intent)))
    (my-chatgpt-shell-persist-edn enriched)
    (my-chatgpt-shell--validate-pattern-events enriched)
    (my-chatgpt-shell--maybe-update-intent enriched)
    (my-chatgpt-shell--refresh-context-hints)
    (my-chatgpt-shell--maybe-render-context)
    (my-chatgpt-shell--maybe-complete-clock-out enriched)))

(defun my-chatgpt-shell-replay-last-turn ()
  "Reapply the most recent Tatami/ChatGPT EDN snapshots without contacting the LLM."
  (interactive)
  (if (and my-chatgpt-shell--last-replay-chatgpt-edn
           my-chatgpt-shell--last-replay-tatami-edn)
      (let ((chatgpt (cl-copy-list my-chatgpt-shell--last-replay-chatgpt-edn))
            (tatami (cl-copy-list my-chatgpt-shell--last-replay-tatami-edn)))
        (setq my-chatgpt-shell-last-inbound-edn tatami)
        (setq my-chatgpt-shell-last-edn chatgpt)
        (my-chatgpt-shell--record-hud-state)
        (my-chatgpt-shell--validate-pattern-events chatgpt)
        (my-chatgpt-shell--resolve-intent-sigils)
        (my-chatgpt-shell--maybe-render-context)
        (message "Tatami HUD replayed from last turn."))
    (message "No Tatami snapshot available for replay.")))

(defun my-chatgpt-shell-inject-chatgpt-edn (text)
  "Manually feed a FROM-CHATGPT-EDN payload, bypassing the LLM."
  (interactive "sFROM-CHATGPT-EDN payload: ")
  (let ((edn (my-chatgpt-shell--parse-edn-string text)))
    (unless edn
      (user-error "Could not parse EDN payload"))
    (my-chatgpt-shell--apply-chatgpt-edn edn)
    (message "Injected FROM-CHATGPT-EDN payload.")))

(defun my-chatgpt-shell--normalize-pattern-token (token)
  (when token
    (let ((trim (string-trim (format "%s" token))))
      (when (> (length trim) 0)
        (downcase trim)))))

(defun my-chatgpt-shell--pattern-key-from-event (pattern)
  (when-let* ((raw (my-chatgpt-shell--string-or-nil pattern)))
    (let* ((parts (split-string raw "/"))
           (tail (car (last parts))))
      (my-chatgpt-shell--normalize-pattern-token tail))))

(defun my-chatgpt-shell--pattern-key-from-entry (entry)
  (or (my-chatgpt-shell--normalize-pattern-token (plist-get entry :summary))
      (my-chatgpt-shell--normalize-pattern-token (plist-get entry :title))
      (my-chatgpt-shell--normalize-pattern-token (plist-get entry :id))))

(defun my-chatgpt-shell--find-event-for-pattern (entry events)
  (let ((key (my-chatgpt-shell--pattern-key-from-entry entry)))
    (when key
      (cl-find-if (lambda (event)
                    (let ((event-key (my-chatgpt-shell--pattern-key-from-event
                                      (plist-get event :pattern))))
                      (and event-key (string= event-key key))))
                  events))))

(defun my-chatgpt-shell--format-pattern-status (event)
  (let ((kind (plist-get event :kind)))
    (pcase kind
      (:match "🟢 match")
      (:note "🟡 note")
      (:dismiss "🔴 dismiss")
      (_ (and kind (format "%s" kind))))))

(defun my-chatgpt-shell--format-pattern-candidate (pattern event)
  (let* ((headline (my-chatgpt-shell--pattern-headline pattern))
         (score (plist-get pattern :score))
         (status (and event (my-chatgpt-shell--format-pattern-status event)))
         (notes (and event (plist-get event :notes)))
         (score-text (concat (when (numberp score)
                               (format " [d=%.2f]" score))
                             (my-chatgpt-shell--format-intent-distance pattern)))
         (note-text (when (and notes (> (length notes) 0))
                      (my-chatgpt-shell--truncate notes 140))))
    (cons (string-trim
           (concat "  • " headline score-text
                   (cond
                    (status
                     (if note-text
                         (format " — %s – %s" status note-text)
                       (format " — %s" status)))
                    (note-text (format " — %s" note-text))
                    (t ""))))
          status)))

(defun my-chatgpt-shell--insert-pattern-candidate (pattern event)
  (let* ((id (my-chatgpt-shell--pattern-id pattern))
         (title (my-chatgpt-shell--string-or-nil (plist-get pattern :title)))
         (score (plist-get pattern :score))
         (status (and event (my-chatgpt-shell--format-pattern-status event)))
         (notes (and event (plist-get event :notes)))
         (score-text (concat (when (numberp score)
                               (format " [d=%.2f]" score))
                             (my-chatgpt-shell--format-intent-distance pattern)))
         (note-text (when (and notes (> (length notes) 0))
                      (my-chatgpt-shell--truncate notes 140))))
    (insert "  • ")
    (my-chatgpt-shell--insert-pattern-anchor title id)
    (insert score-text)
    (cond
     (status
      (if note-text
          (insert (format " — %s – %s" status note-text))
        (insert (format " — %s" status))))
     (note-text (insert (format " — %s" note-text))))
    (insert "\n")))

(provide 'aob-chatgpt)

;;; aob-chatgpt.el ends here
