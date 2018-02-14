;;; package --- init.el

;;; Commentary:
;;; Primary Emacs configuration which call others files

;;; Code:

(package-initialize)

(defvar settings-dir
  (expand-file-name "settings" user-emacs-directory)
  "Path to settings directory.")

(setq message-log-max 10000)
(defun my-tracing-function (orig-fun &rest args)
  "Print a message before and after ORIG-FUN is call with ARGS."
  (message "require %S start" args)
  (let ((res (apply orig-fun args)))
    (message "require %S ended" res)
    res))

(advice-add 'require :around #'my-tracing-function)

;; Set up load path
(add-to-list 'load-path settings-dir)

(require 'setup-first)

(require 'setup-package)

;; Set up appearance early
(require 'appearance)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(require 'setup-ido)
(require 'setup-paredit)
(require 'setup-org)
(require 'setup-dired)
(require 'setup-rgrep)
(require 'setup-shell)
(require 'setup-yasnippet)
(require 'setup-web)
(require 'setup-css)
(require 'setup-latex)
(require 'setup-c)
(require 'setup-ispell)
(require 'setup-sql)
(require 'setup-gnus)
(require 'setup-bbdb)
(require 'setup-nxml)
(require 'setup-ibuffer)
(require 'setup-image)
(require 'setup-gnuplot)
(require 'setup-helm-dash)
(require 'setup-magit)
(require 'setup-php)
(require 'setup-lua)
(require 'setup-pdf-tools)
(require 'setup-python)
(require 'setup-js2-mode)
(require 'setup-lisp)
(require 'setup-flycheck)
(require 'setup-company)
(require 'setup-yaml)
(require 'setup-docker)
(require 'setup-dart)
(require 'setup-typescript)

;; Map files to modes
(require 'mode-mappings)

(require 'setup-defuns)

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'wgrep)
(require 'google-translate)
(require 'google-translate-default-ui)
(require 'legalese)
(require 'misc)

(require 'key-bindings)

(require 'atomic-chrome)
(require 'setup-server)

;; Remove advice defined at the beginning of this file
(advice-remove 'require #'my-tracing-function)

(provide 'init)
;;; init.el ends here
