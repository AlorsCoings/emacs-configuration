;;; package --- setup-web

;;; Commentary:
;;; This my emacs configuration file for web development

;;; Code:

;; to use impatient-mode
;; M-x httpd-start
;; then M-x impatient-mode on the buffer you want to listen
;; then go on http://localhost:8080/imp/
(require 'impatient-mode)

;; navigation C-c C-n between opening / closing HTML tags
;; code folding C-c C-f for HTML elements and control blocks
;; snippet insertion C-c C-s
;; clever selection and expansion C-c C-m
;; suspicious whitespaces detection C-c C-w
(require 'web-mode)
(require 'emmet-mode)

;; example
;; html[ng-app=nicolasGros lang=fr]>head>title+meta[charset=utf-8]+p*3>lorem3
;; m0+p10+c#f+fw:b+w100+h20+bg#f00

(dolist (hook (list
               'sgml-mode-hook
               'css-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq emmet-preview-default t)
				   (setq web-mode-markup-indent-offset 2)
				   (setq web-mode-css-indent-offset 2)
				   (emmet-mode))))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)

(provide 'setup-web)
;;; setup-web.el ends here
