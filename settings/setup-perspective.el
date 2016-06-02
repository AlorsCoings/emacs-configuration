;;; package --- setup-perspective

;;; Commentary:
;;; Emacs configuration file for perspective

;;; Code:

(require 'perspective)

;; Enable perspective mode
(persp-mode t)

; s -- persp-switch: Query a perspective to switch or create
; k -- persp-remove-buffer: Query a buffer to remove from current perspective
; c -- persp-kill : Query a perspective to kill
; r -- persp-rename: Rename current perspective
; a -- persp-add-buffer: Query an open buffer to add to current perspective
; A -- persp-set-buffer: Add buffer to current perspective and remove it from all others
; i -- persp-import: Import a given perspective from another frame.
; n, <right> -- persp-next : Switch to next perspective
; p, <left> -- persp-prev: Switch to previous perspective

(defun custom-persp-last ()
  "Jump to last perspective."
  (interactive)
  (persp-switch (persp-name persp-last)))

(define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)

(provide 'setup-perspective)
;;; setup-perspective.el ends here
