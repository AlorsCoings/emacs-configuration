;;; package --- setup-paredit

;;; Commentary:
;;; Emacs configuration file for paredit

;;; Code:

(require 'paredit)

(defun paredit-everywhere()
  "Load some paredit keybindings in `prog-mode'."
  (local-set-key (kbd "C-M-S-s") 'paredit-splice-sexp-killing-forward)
  (local-set-key (kbd "C-M-S-d") 'paredit-splice-sexp-killing-backward)
  (local-set-key (kbd "C-M-S-r") 'paredit-forward-slurp-sexp)
  (local-set-key (kbd "C-M-S-l") 'paredit-forward-barf-sexp)
  (local-set-key (kbd "C-M-j") 'paredit-join-sexps)
  (local-set-key (kbd "C-M-?") 'paredit-splice-sexp)
  (local-set-key (kbd "C-e") 'paredit-forward-delete)
  (local-set-key (kbd "C-'") 'paredit-backward-delete)
  (local-set-key (kbd "M-'") 'paredit-backward-kill-word)
  (local-set-key (kbd "M-e") 'paredit-forward-kill-word))

(add-hook 'prog-mode-hook 'paredit-everywhere)

(provide 'setup-paredit)
;;; setup-paredit.el ends here
