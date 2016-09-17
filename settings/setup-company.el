;;; package --- setup-company

;;; Commentary:
;;; Emacs configuration file for company

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)
(require 'company)

(define-key company-search-map (kbd "C-d") 'company-select-previous)
(define-key company-search-map (kbd "C-s") 'company-select-next)

;; bigger popup window
(setq company-tooltip-limit 20)

;; align annotations to the right tooltip border
(setq company-tooltip-align-annotations 't)

;; decrease delay before autocompletion popup shows
(setq company-idle-delay .3)

(provide 'setup-company)
;;; setup-company.el ends here
