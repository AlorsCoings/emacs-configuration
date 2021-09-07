;;; package --- mode-mappings

;;; Commentary:
;;; Emacs configuration file for mode mapping

;;; Code:

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Groovy
(autoload 'groovy-mode "groovy-mode")
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

;; Scala
(autoload 'scala-mode "scala-mode2")
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(autoload 'ensime-scala-mode-hook "ensime")
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Lisp
(autoload 'lisp-mode "lisp-mode")
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

;; Clojure
(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; Xml
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))

;; Matlab - Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Crontab
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))

;; Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Turtle
(add-to-list 'auto-mode-alist '("\\.ttl$" . ttl-mode))

;; Dart
(add-to-list 'auto-mode-alist '("\\.dart$" . dart-mode))

(provide 'mode-mappings)
;;; mode-mappings.el ends here
