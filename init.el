;;; package --- init.el

;;; Commentary:
;;; Primary Emacs configuration which call others files

;;; Code:

(package-initialize)

(defvar settings-dir
  (expand-file-name "settings" user-emacs-directory)
  "Path to settings directory.")

(setq message-log-max 10000)

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
(require 'setup-nxml)
(require 'setup-ibuffer)
(require 'setup-image)
(require 'setup-magit)
(require 'setup-php)
(require 'setup-lua)
(require 'setup-dart)
(require 'setup-python)
(require 'setup-js2-mode)
(require 'setup-lisp)
(require 'setup-flycheck)
(require 'setup-company)
(require 'setup-yaml)
(require 'setup-markdown)
(require 'setup-docker)
(require 'setup-typescript)
(require 'setup-go)
(require 'setup-prolog)

;; Map files to modes
(require 'mode-mappings)

(require 'setup-defuns)

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'wgrep)
(require 'legalese)
(require 'misc)

(require 'key-bindings)

(require 'atomic-chrome)
(require 'setup-server)

(provide 'init)
;;; init.el ends here
