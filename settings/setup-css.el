;;; package --- setup-css

;;; Commentary:
;;; Emacs configuration file for css

;;; Code:

;;; Colourise CSS colour literals
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode)
  ;; Disable hl-line-mode for css
  (make-variable-buffer-local 'global-hl-line-mode)
  (add-hook hook (lambda () (setq global-hl-line-mode nil))))

(require 'web-beautify)
(require 'css-mode)
(define-key css-mode-map (kbd "C-c C-n") 'web-beautify-css)

;;; SASS and SCSS
(require 'scss-mode)
(setq-default scss-compile-at-save nil)

;;; Use eldoc for syntax hints
(require 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

(provide 'setup-css)
;;; setup-css.el ends here
