;;; futon3-arxana-compat.el --- Futon3/Arxana glue -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to keep Futon3/Arxana glue
;; (Futon4 crossover) separate from core Futon3 logic.

;;; Code:

(require 'subr-x)
(require 'fubar nil t)

(declare-function arxana-patterns-open "arxana" (slug))
(declare-function my-chatgpt-shell--debug "aob-chatgpt" (fmt &rest args))
(declare-function my-chatgpt-shell--futon1-post "aob-chatgpt" (path payload))
(declare-function my-chatgpt-shell--log-pattern-save "aob-chatgpt" (pattern-id &optional notes))
(declare-function my-chatgpt-shell--current-turn-id "aob-chatgpt" ())
(declare-function my-chatgpt-shell--now-iso "aob-chatgpt" ())
(declare-function my-fubar-log-doc-written "fubar" (session-id doc))
(declare-function my-fubar-log-pattern-used "fubar" (session-id pattern-id &optional reason))
(declare-function my-fubar-record-artifact "fubar" (session-id artifact-path &optional action))
(declare-function my-fubar-session-id "fubar" ())
(declare-function my-futon3--clock-context "futon3-hud" ())

(defvar-local my-futon3-pattern-provenance nil
  "Buffer-local provenance for the current Arxana pattern buffer.")

(defun my-futon3--capture-pattern-provenance (pattern-id &optional source)
  "Capture provenance for PATTERN-ID in the current buffer."
  (setq-local my-futon3-pattern-provenance
              (list :pattern-id pattern-id
                    :turn-id (when (fboundp 'my-chatgpt-shell--current-turn-id)
                               (my-chatgpt-shell--current-turn-id))
                    :timestamp (when (fboundp 'my-chatgpt-shell--now-iso)
                                 (my-chatgpt-shell--now-iso))
                    :source source)))

(defun my-futon3--pattern-provenance-note (pattern-id)
  (let* ((prov my-futon3-pattern-provenance)
         (prov-id (plist-get prov :pattern-id)))
    (when (and prov prov-id (string= (format "%s" prov-id) (format "%s" pattern-id)))
      (let ((source (or (plist-get prov :source) "unknown"))
            (timestamp (plist-get prov :timestamp))
            (turn-id (plist-get prov :turn-id)))
        (string-trim
         (format "provenance=%s%s%s"
                 source
                 (if timestamp (format "@%s" timestamp) "")
                 (if turn-id (format " turn=%s" turn-id) "")))))))

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
              (timestamp (my-chatgpt-shell--now-iso))
              (base-note (format "intent=%s prototypes=%s"
                                 intent
                                 (mapconcat (lambda (p) (format "%s" p)) protos ", ")))
              (prov-note (my-futon3--pattern-provenance-note pattern))
              (note (string-trim
                     (string-join (delq nil (list base-note prov-note)) " | "))))
    (dolist (proto protos)
      (my-chatgpt-shell--futon1-post "/relation"
                                     (list :type ":clock/pattern-edit"
                                           :src {:name proto
                                                 :type :devmap/prototype}
                                           :dst pattern
                                           :props {:intent intent
                                                   :timestamp timestamp})))
    (when (fboundp 'my-chatgpt-shell--log-pattern-save)
      (my-chatgpt-shell--log-pattern-save pattern note)))
  (my-chatgpt-shell--debug "[HUD] logged pattern edit for %s" pattern))

(defun my-futon3--after-arxana-save (&rest _)
  (when my-futon3-active-pattern
    (my-futon3--log-pattern-edit my-futon3-active-pattern)
    (when (fboundp 'my-fubar-session-id)
      (let* ((session-id (my-fubar-session-id))
             (path (or (buffer-file-name)
                       (format "pattern:%s" my-futon3-active-pattern)))
             (doc (list :path path
                        :type :pattern
                        :topic my-futon3-active-pattern
                        :pattern-refs (list my-futon3-active-pattern))))
        (my-fubar-log-doc-written session-id doc)
        (my-fubar-record-artifact session-id path :modified)))
    (setq-local my-futon3-pattern-provenance nil)))

(defun my-futon3--after-arxana-open (&rest _)
  (when my-futon3-active-pattern
    (my-futon3--capture-pattern-provenance my-futon3-active-pattern "arxana-open")
    (when (fboundp 'my-fubar-session-id)
      (my-fubar-log-pattern-used (my-fubar-session-id)
                                 my-futon3-active-pattern
                                 "arxana-open"))))

(with-eval-after-load 'arxana
  (when (fboundp 'arxana-patterns-save)
    (advice-add 'arxana-patterns-save :after #'my-futon3--after-arxana-save)))

(with-eval-after-load 'arxana
  (when (fboundp 'arxana-patterns-open)
    (advice-add 'arxana-patterns-open :after #'my-futon3--after-arxana-open)))

(provide 'futon3-arxana-compat)

;;; futon3-arxana-compat.el ends here
