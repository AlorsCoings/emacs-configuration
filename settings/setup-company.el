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

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(provide 'setup-company)
;;; setup-company.el ends here
