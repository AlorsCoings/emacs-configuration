;;; package --- setup-flycheck

;;; Commentary:
;;; Emacs configuration file for flycheck

;;; Code:

(require 'flycheck)

;; Make flycheck use the current load-path
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 3.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)

;; Remove newline checks, since they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically '(save
                                            mode-enabled))

(setq flycheck-flake8rc "~/setup.cfg")

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
