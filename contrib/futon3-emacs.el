;;; futon3-emacs.el --- Emacs integration notes for Futon3 -*- lexical-binding: t; -*-

;; Commentary:
;; This file documents the Emacs-side integration surface for Futon3.
;; - fubar.el mirrors the MUSN turn lifecycle via HTTP.
;; - aob-chatgpt.el submits workday evidence and HUD context.
;;
;; Typical setup:
;;   (require 'fubar)
;;   (require 'aob-chatgpt)
;;   ;; Start a MUSN session and log actions via fubar.el.
;;   ;; Submit workday claims from aob-chatgpt.el as needed.
;;
;; Ensure both packages point at your local services:
;;   - fubar-musn-url -> MUSN server (default http://localhost:6065)
;;   - aob-chatgpt endpoints -> Futon3 transport (see aob-chatgpt.el)

(defun futon3-emacs-setup ()
  "Load fubar + aob-chatgpt if available."
  (interactive)
  (require 'fubar nil t)
  (require 'aob-chatgpt nil t)
  (message "[futon3] Emacs helpers loaded (fubar, aob-chatgpt)"))

(provide 'futon3-emacs)

;;; futon3-emacs.el ends here
