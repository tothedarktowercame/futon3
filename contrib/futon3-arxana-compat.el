;;; futon3-arxana-compat.el --- Futon3/Arxana glue -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to keep Futon3/Arxana glue
;; (Futon4 crossover) separate from core Futon3 logic.

;;; Code:

(require 'subr-x)

(declare-function arxana-patterns-open "arxana" (slug))
(declare-function my-chatgpt-shell--debug "aob-chatgpt" (fmt &rest args))
(declare-function my-chatgpt-shell--futon1-post "aob-chatgpt" (path payload))
(declare-function my-chatgpt-shell--now-iso "aob-chatgpt" ())
(declare-function my-futon3--clock-context "futon3-hud" ())

(defun my-futon3-open-active-pattern ()
  "Open the current active pattern using the Futon4 editor."
  (interactive)
  (cond
   ((not my-futon3-active-pattern)
    (message "No active pattern set. Use C-c C-a to choose one."))
   ((not (fboundp 'arxana-patterns-open))
    (message "arxana-patterns-open not available; load Futon4/arxana."))
   (t
    (arxana-patterns-open my-futon3-active-pattern))))

(defun my-futon3--log-pattern-edit (pattern)
  "Create Futon1 relations showing PATTERN was edited for the clocked prototype."
  (when-let* ((ctx (my-futon3--clock-context))
              (protos (plist-get ctx :prototypes))
              (intent (plist-get ctx :intent))
              (timestamp (my-chatgpt-shell--now-iso)))
    (dolist (proto protos)
      (my-chatgpt-shell--futon1-post "/relation"
                                     (list :type ":clock/pattern-edit"
                                           :src {:name proto
                                                 :type :devmap/prototype}
                                           :dst pattern
                                           :props {:intent intent
                                                   :timestamp timestamp}))))
  (my-chatgpt-shell--debug "[HUD] logged pattern edit for %s" pattern))

(defun my-futon3--after-arxana-save (&rest _)
  (when my-futon3-active-pattern
    (my-futon3--log-pattern-edit my-futon3-active-pattern)))

(with-eval-after-load 'arxana
  (when (fboundp 'arxana-patterns-save)
    (advice-add 'arxana-patterns-save :after #'my-futon3--after-arxana-save)))

(provide 'futon3-arxana-compat)

;;; futon3-arxana-compat.el ends here
