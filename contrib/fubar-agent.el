;;; fubar-agent.el --- MUSN human driver (start/plan/select/action/use/resume) -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal MUSN client for humans to drive the same surface as fucodex.
;; Uses the MUSN HTTP service (FUTON3_MUSN_URL env or default http://localhost:6065).

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'fubar-viewer)

(defvar fubar-musn-agent-session nil)
(defvar fubar-musn-agent-turn 0)

(defun fubar-musn--post (path payload)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("X-Musn-Client" . "fubar-agent")
                                      ("User-Agent" . "fubar-agent")))
         (url-request-data (and payload (encode-coding-string (json-encode payload) 'utf-8)))
         (url (concat (string-remove-suffix "/" fubar-musn-url) path))
         (buffer (url-retrieve-synchronously url t t 5)))
    (unless buffer (error "No response from MUSN"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "\n\n" nil 'move)
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (when (not (string-empty-p (string-trim body)))
              (json-parse-string body :object-type 'plist :array-type 'list :null-object nil :false-object nil))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun fubar-musn-agent-start (&optional session-id)
  "Create or load a MUSN session."
  (interactive)
  (let* ((resp (fubar-musn--post "/musn/session/create"
                                 (if session-id `(("session/id" . ,session-id)) {})))
         (sid (plist-get resp :session/id)))
    (setq fubar-musn-agent-session sid
          fubar-musn-agent-turn 0)
    (message "MUSN session %s" sid)
    sid))

(defun fubar-musn-agent-turn-start ()
  "Start a turn."
  (interactive)
  (unless fubar-musn-agent-session
    (fubar-musn-agent-start))
  (setq fubar-musn-agent-turn (1+ fubar-musn-agent-turn))
  (fubar-musn--post "/musn/turn/start"
                    `(("session/id" . ,fubar-musn-agent-session)
                      ("turn" . ,fubar-musn-agent-turn)))
  (message "Turn %d started" fubar-musn-agent-turn))

(defun fubar-musn-agent-plan (plan)
  "Send PLAN for current turn."
  (interactive "sPlan: ")
  (fubar-musn--post "/musn/turn/plan"
                    `(("session/id" . ,fubar-musn-agent-session)
                      ("turn" . ,fubar-musn-agent-turn)
                      ("plan" . ,plan))))

(defun fubar-musn-agent-select (pattern-id)
  "Select PATTERN-ID."
  (interactive "sPattern: ")
  (fubar-musn--post "/musn/turn/select"
                    `(("session/id" . ,fubar-musn-agent-session)
                      ("turn" . ,fubar-musn-agent-turn)
                      ("candidates" . [,pattern-id])
                      ("chosen" . ,pattern-id)
                      ("reason" . (("mode" . "use")
                                   ("note" . "human"))))))

(defun fubar-musn-agent-action (pattern-id action note files)
  "Record ACTION on PATTERN-ID with NOTE and FILES (comma-separated)."
  (interactive "sPattern: \nsAction (read/implement/update): \nsNote: \nsFiles (comma-separated): ")
  (let ((files (when (and files (not (string-empty-p files)))
                 (vconcat (mapcar #'string-trim (split-string files ","))))))
    (fubar-musn--post "/musn/turn/action"
                      (append `(("session/id" . ,fubar-musn-agent-session)
                                ("turn" . ,fubar-musn-agent-turn)
                                ("pattern/id" . ,pattern-id)
                                ("action" . ,action)
                                ("note" . ,note))
                              (when files `(("files" . ,files)))))
    (when (member action '("implement" "update"))
      (fubar-musn--post "/musn/turn/use"
                        `(("session/id" . ,fubar-musn-agent-session)
                          ("turn" . ,fubar-musn-agent-turn)
                          ("pattern/id" . ,pattern-id)))
      (when files
        (fubar-musn--post "/musn/evidence/add"
                          `(("session/id" . ,fubar-musn-agent-session)
                            ("turn" . ,fubar-musn-agent-turn)
                            ("pattern/id" . ,pattern-id)
                            ("files" . ,files)
                            ("note" . ,note)))))))

(defun fubar-musn-agent-end-turn ()
  "End current turn."
  (interactive)
  (let ((resp (fubar-musn--post "/musn/turn/end"
                                `(("session/id" . ,fubar-musn-agent-session)
                                  ("turn" . ,fubar-musn-agent-turn)))))
    (when (plist-get resp :halt?)
      (fubar-musn-display-pause (plist-get resp :pause)))
    (message "Turn %d summary: %s" fubar-musn-agent-turn (plist-get resp :summary))))

(defun fubar-musn-launch-and-view (prompt)
  "One-shot MUSN launch: start session, turn, plan with PROMPT, and show viewer."
  (interactive "sPlan/prompt: ")
  (let ((sid (fubar-musn-agent-start)))
    (fubar-musn-agent-turn-start)
    (fubar-musn-agent-plan prompt)
    (fubar-musn-view-toggle)
    (message "MUSN session %s turn %d started; viewer displayed." sid fubar-musn-agent-turn)))

(provide 'fubar-agent)
;;; fubar-agent.el ends here
