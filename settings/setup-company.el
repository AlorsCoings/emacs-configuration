;;; package --- setup-company

;;; Commentary:
;;; Emacs configuration file for company

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)
(require 'company)

(define-key company-search-map (kbd "C-d") 'company-select-previous)
(define-key company-search-map (kbd "C-s") 'company-select-next)

;; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)

(provide 'setup-company)
;;; setup-company.el ends here
