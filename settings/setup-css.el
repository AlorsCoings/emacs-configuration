;;; package --- setup-css

;;; Commentary:
;;; Emacs configuration file for css

;;; Code:

;;; Colourise CSS colour literals
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))

;;; SASS and SCSS
(require 'sass-mode)
(require 'scss-mode)
(setq-default scss-compile-at-save nil)

;;; Use eldoc for syntax hints
(require 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

(provide 'setup-css)
;;; setup-css.el ends here
