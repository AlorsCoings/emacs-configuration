;;; package --- setup-package

;;; Commentary:
;;; Emacs configuration file which handle packages

;;; Code:

(require 'package)

(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

;; Initialize installed packages
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(when (not (package-installed-p 'dash))
  (package-install 'dash))
(require 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))

(defun packages-install (packages)
  "Install PACKAGES which wasn't installed."
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows)
  )

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(setq package-check-signature nil)

;; Install extensions if they're missing
(defun init--install-packages ()
  "Install extensions if they're missing."
  (packages-install
   '(
     add-node-modules-path
     angular-snippets
     anzu
     apache-mode
     arduino-mode
     atomic-chrome
     auctex
     auctex-lua
     auto-compile
     auto-complete
     bash-completion
     bbdb
     beginend
     browse-kill-ring
     c-eldoc
     company
     company-auctex
     company-c-headers
     company-emacs-eclim
     company-go
     company-irony
     company-jedi
     company-restclient
     crappy-jsp-mode
     css-eldoc
     cmake-mode
     dart-mode
     dash
     dash-functional
     deferred
     diminish
     ;dired-details
     ;dired-sort
     docker
     docker-compose-mode
     ;; docker-tramp replace by integrated tramp-container
     dockerfile-mode
     elisp-slime-nav
     elnode
     elpy
     emacsql
     emacsql-sqlite
     emmet-mode
     exec-path-from-shell
     expand-region
     fill-column-indicator
     flx-ido
     flycheck
     flycheck-pos-tip
     fullframe
     git-messenger
     ;; gitconfig-mode
     ;; gitignore-mode
     gnuplot
     gnuplot-mode
     go-guru
     go-mode
     helm
     helm-dash
     highlight-escape-sequences
     highlight-indentation
     highlight-symbol
     ibuffer-vc
     ;icomplete+
     ido-at-point
     ido-completing-read+
     ido-vertical-mode
     idomenu
     impatient-mode
     irony
     js2-mode
     json-mode
     karma
     latex-extra
     legalese
     lua-mode
     magit
     magit-gitflow
     markdown-mode
     math-symbol-lists
     markdown-mode
     move-text
     multiple-cursors
     nginx-mode
     oauth2
     org
     org-gcal
     ;; org-pdfview
     page-break-lines
     paredit
     php-mode
     prettier-js
     ;plsql
     rainbow-mode
     realgud
     sass-mode
     scala-mode
     scss-mode
     ;; shell-command
     skewer-mode
     slime
     slime-company
     smart-mode-line
     smex
     sparql-mode
     sql-indent
     ;sqlplus
     sqlup-mode
     tern
     ;tidy
     tide
     toggle-quotes
     ;; ttl-mode
     typescript-mode
     undo-tree
     visual-regexp
     vlf
     web-beautify
     web-mode
     wgrep
     whitespace-cleanup-mode
     yaml-mode
     yasnippet)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'setup-package)
;;; setup-package.el ends here
