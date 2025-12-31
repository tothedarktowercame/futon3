;;; futon3-sessions.el --- Futon3 session helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to keep Futon3 session/intent
;; helpers and dashboard wiring grouped together.

;;; Code:

(require 'subr-x)

(declare-function my-futon3--prototype-display-map "futon3-hud" ())
(declare-function my-futon3--prototype-list "futon3-hud" (value))
(declare-function my-futon3--prototype-string "futon3-hud" (proto))
(declare-function my-futon3--tatami-request "futon3-bridge" (method path payload))
(declare-function my-futon3--truncate "futon3-hud" (text max-len))
(declare-function my-futon3-ensure-tatami-session "futon3-hud" ())
(declare-function my-futon3-reset-tatami-session "futon3-hud" ())
(declare-function my-futon3-running-p "futon3-bridge" ())

(defun my-futon3-log-chatgpt-turn (text)
  (when (and text (my-futon3-running-p))
    (let ((attempt 0)
          (max-attempts 3)
          (done nil)
          (last-error nil))
      (while (and (< attempt max-attempts) (not done))
        (setq attempt (1+ attempt))
        (condition-case err
            (progn
              (my-futon3-ensure-tatami-session)
              (my-futon3--tatami-request
               "POST" "/musn/tatami/log"
               `(("session-id" . ,my-futon3-tatami-session-id)
                 ("activity" . "agent-work")
                 ("performed?" . t)
                 ("felt-state" . "ok")
                 ("notes" . ,(my-futon3--truncate text 800))))
              (setq done t))
          (error
           (let ((msg (error-message-string err)))
             (setq last-error msg)
            (if (string-match-p "unknown-session" msg)
                (progn
                  (my-futon3-reset-tatami-session)
                  (message "Futon3 tatami session expired; establishing a new session (attempt %d/%d)."
                            attempt max-attempts))
               (setq attempt max-attempts))))))
      (unless done
        (message "Futon3 tatami log failed after %d attempt(s): %s"
                 attempt (or last-error "unknown error"))))))

(defun my-futon3-close-tatami-session (&optional summary)
  (when my-futon3-tatami-session-id
    (ignore-errors
      (my-futon3--tatami-request
       "POST" "/musn/tatami/close"
       `(("session-id" . ,my-futon3-tatami-session-id)
         ("summary" . ,(or summary "chatgpt-shell session")))))
    (setq my-futon3-tatami-session-id nil)))

(defun my-futon3--format-counts (data)
  (let ((fruit-map
         '(("indicator" . "🍌")
             ("obligation" . "🍐")
             ("joy" . "🍒")
             ("insight" . "🍓")
             ("sleep" . "💤")
             ("stretch" . "🍊")
             ("baseline" . "🍈")
             ("rocket" . "🚀")
             ("bell" . "🔔")
             ("ghost" . "👻"))))
    (cond
     ((and (listp data) (keywordp (car data)))
      (let (parts p)
        (setq p data)
        (while (and p (cdr p))
          (let ((key (pop p))
                (val (pop p)))
            (let* ((name (if (keywordp key) (substring (symbol-name key) 1) key))
                   (icon (or (cdr (assoc name fruit-map)) name)))
              (push (format "%s=%s" icon val) parts))))
        (string-join (nreverse parts) ", ")))
     ((and (listp data) (consp (car data)))
      (let (parts)
        (dolist (pair data)
          (let ((key (car pair))
                (val (cdr pair)))
                                     (let* ((name (if (keywordp key) (substring (symbol-name key) 1) key))
                                            (icon (or (cdr (assoc name fruit-map)) name)))
              (push (format "%s=%s" icon val) parts))))
        (string-join (nreverse parts) ", "))))))

(defun my-futon3--suggest-intent-text (prototypes)
  (let* ((ids (delq nil (mapcar #'my-futon3--prototype-string
                                (my-futon3--prototype-list prototypes))))
         (map (my-futon3--prototype-display-map))
         (labels (delq nil (mapcar (lambda (id)
                                     (or (and map id (gethash id map))
                                         id))
                                   ids))))
    (cond
     ((null labels) nil)
     ((= (length labels) 1) (car labels))
     (t (string-join labels " + ")))))

(defun my-futon3-open-dashboard ()
  "Open the Futon3 sessions dashboard in a browser."
  (interactive)
  (browse-url (concat (string-remove-suffix "/" my-futon3-ui-base-url) "/musn/sessions")))

(provide 'futon3-sessions)

;;; futon3-sessions.el ends here
