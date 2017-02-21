;;; package --- setup-magit

;;; Commentary:
;;; Emacs magit configuration

;;; Code:

(require 'magit)

;; Don't prompt me
(magit-auto-revert-mode t)
(set-default 'magit-completing-read-function 'magit-ido-completing-read)

(eval-after-load 'ediff
  '(progn
     (set-face-foreground 'ediff-odd-diff-B "#ffffff")
     (set-face-background 'ediff-odd-diff-B "#292521")
     (set-face-foreground 'ediff-even-diff-B "#ffffff")
     (set-face-background 'ediff-even-diff-B "#292527")

     (set-face-foreground 'ediff-odd-diff-A "#ffffff")
     (set-face-background 'ediff-odd-diff-A "#292521")
     (set-face-foreground 'ediff-even-diff-A "#ffffff")
     (set-face-background 'ediff-even-diff-A "#292527")))

;; Load git configurations
;; For instance, to run magit-svn-mode in a project, do:
;;
;;     git config --add magit.extension svn
;;
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(defun magit-exit-commit-mode ()
  "Kill current buffer and window."
  (interactive)
  (kill-buffer)
  (delete-window))

(eval-after-load "git-commit-mode"
  '(define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))

(defun magit-status-fullscreen (prefix)
  "Start magit in fullscreen unless PREFIX is not nil."
  (interactive "P")
  (magit-status-internal default-directory)
  (unless prefix
    (delete-other-windows)))

;; Full screen vc-annotate

(defun vc-annotate-quit ()
  "Restore the previous window configuration and kill the `vc-annotate' buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(require 'vc-annotate)
(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

(provide 'setup-magit)
;;; setup-magit.el ends here
