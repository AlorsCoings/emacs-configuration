;;; package --- setup-pdf-tools

;;; Commentary:
;;; Emacs configuration file for pdf-tools
;;; https://github.com/politza/pdf-tools

;;; Code:

(require 'pdf-view)
(require 'tex)

(when (eq system-type 'gnu/linux)

  (pdf-tools-install)

  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

  (define-key pdf-view-mode-map (kbd "h") 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "m") 'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "M") 'pdf-view-reset-slice)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "v") 'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "r") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "T") 'pdf-history-backward)
  (define-key pdf-view-mode-map (kbd "R") 'pdf-history-forward)
  (define-key pdf-view-mode-map (kbd "s") 'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "d") 'pdf-view-scroll-down-or-previous-page)
  (define-key pdf-view-mode-map (kbd "o") 'pdf-outline)
  (define-key pdf-view-mode-map (kbd "C-n") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-M-n") 'isearch-forward-regexp)
  (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "C-x h") 'pdf-view-mark-whole-page))

(provide 'setup-pdf-tools)
;;; setup-pdf-tools.el ends here
