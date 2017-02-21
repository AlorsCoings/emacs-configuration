;;; package --- setup-web

;;; Commentary:
;;; Emacs configuration file for web development

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
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-auto-quoting t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-inlays t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-sql-detection t)

(set-face-attribute 'web-mode-html-attr-custom-face nil :inherit font-lock-keyword-face)
;; (set-face-attribute 'web-mode-variable-name-face t :foreground "Pink3")
;; (set-face-attribute 'web-mode-comment-face t :foreground "#8fbc8f")
;; (set-face-attribute 'web-mode-current-element-highlight-face t :background "gray15")
;; (set-face-attribute 'web-mode-html-attr-custom-face t :inherit font-lock-keyword-face)
;; (set-face-attribute 'web-mode-html-attr-name-face t :inherit web-mode-variable-name-face)


;; example
;; html[ng-app=nicolasGros lang=fr]>head>title+meta[charset=utf-8]+p*3>lorem3
;; m0+p10+c#f+fw:b+w100+h20+bg#f00
(require 'emmet-mode)
(dolist (hook (list
               'sgml-mode-hook
               'css-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq emmet-preview-default t)
                   (emmet-mode))))

(define-key web-mode-map (kbd "C-c C-n") 'web-beautify-html)

(provide 'setup-web)
;;; setup-web.el ends here
