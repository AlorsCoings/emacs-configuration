;;; package --- setup-typescript

;;; Commentary:
;;; Emacs configuration file for typescript

;;; Code:

(require 'tide)

(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (when (or (locate-dominating-file default-directory "tsconfig.json")
            (locate-dominating-file default-directory "jsconfig.json"))
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)))

(require 'company)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'setup-typescript)
;;; setup-typescript.el ends here
