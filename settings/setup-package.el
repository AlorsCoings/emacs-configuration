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
     ace-jump-mode
     alert
     anaconda-mode
     angular-snippets
     anzu
     apache-mode
     apel
     arduino-mode
     async
     atomic-chrome
     auctex
     auctex-lua
     auto-compile
     auto-complete
     bash-completion
     bbdb
     bison-mode
     browse-kill-ring
     buster-mode
     buster-snippets
     c-eldoc
     caml
     change-inner
     cider
     clj-refactor
     clojure-mode
     clojure-mode-extra-font-locking
     cmake-mode
     coffee-mode
     color-theme
     color-theme-sanityinc-solarized
     color-theme-solarized
     company
     company-arduino
     company-auctex
     company-c-headers
     company-inf-ruby
     company-irony
     company-jedi
     company-math
     company-restclient
     concurrent
     cpputils-cmake
     crappy-jsp-mode
     creole
     crontab-mode
     css-eldoc
     css-mode
     ctable
     dash
     dash-functional
     datomic-snippets
     db
     deferred
     diminish
     dired-details
     dired-sort
     dockerfile-mode
     edit-server
     edn
     elisp-slime-nav
     elnode
     elpy
     emacs-eclim
     emacsql
     emacsql-sqlite
     emmet-mode
     epc
     epl
     ess
     ess-R-data-view
     ess-R-object-popup
     ess-smart-equals
     ess-smart-underscore
     ess-view
     exec-path-from-shell
     expand-region
     f
     fakir
     feature-mode
     fill-column-indicator
     finalize
     find-file-in-project
     flim
     flx
     flx-ido
     flycheck
     flycheck-clojure
     flycheck-google-cpplint
     flycheck-hdevtools
     flycheck-ocaml
     flycheck-pos-tip
     fold-this
     frame-cmds
     frame-fns
     fullframe
     gh
     gist
     git-commit
     git-messenger
     gitconfig-mode
     gitignore-mode
     gmail-message-mode
     gntp
     gnuplot
     gnuplot-mode
     google-c-style
     google-translate
     groovy-mode
     guide-key
     ham-mode
     haml-mode
     hardcore-mode
     helm
     helm-core
     helm-dash
     highlight-escape-sequences
     highlight-indentation
     highlight-symbol
     hl-sexp
     ht
     html-to-markdown
     htmlize
     hydra
     ibuffer-vc
     icomplete+
     ido-at-point
     ido-completing-read+
     ido-ubiquitous
     ido-vertical-mode
     idomenu
     iedit
     image+
     impatient-mode
     inf-ruby
     inflections
     ipretty
     irony
     ivy
     jade-mode
     jedi-core
     js2-mode
     js2-refactor
     json-mode
     json-reformat
     json-snatcher
     julia-mode
     jump-char
     karma
     know-your-http-well
     kv
     latex-extra
     legalese
     let-alist
     log4e
     logito
     lua-mode
     macrostep
     magit
     magit-popup
     markdown-mode
     markdown-mode+
     marshal
     math-symbol-lists
     merlin
     misc-cmds
     misc-fns
     mmm-mode
     mmt
     move-text
     multi-eshell
     multifiles
     multiple-cursors
     nodejs-repl
     noflet
     oauth2
     org
     org-gcal
     org-mobile-sync
     org-pdfview
     packed
     page-break-lines
     paredit
     paredit-everywhere
     pcache
     pdf-tools
     peg
     perspective
     php-mode
     pkg-info
     plsql
     popup
     popwin
     pos-tip
     prodigy
     puppet-mode
     python-environment
     pythonic
     pyvenv
     queue
     rainbow-mode
     request
     request-deferred
     restclient
     rhtml-mode
     rich-minority
     robe
     ruby-compilation
     ruby-hash-syntax
     s
     sass-mode
     scss-mode
     semi
     seq
     shell-command
     simple-httpd
     simplezen
     skewer-mode
     slime
     slime-company
     smart-forward
     smart-mode-line
     smartparens
     smarty-mode
     smex
     smooth-scrolling
     smtpmail-multi
     spinner
     sql-indent
     sqlite
     sqlplus
     sqlup-mode
     swiper
     tablist
     tern
     tidy
     toggle-quotes
     top-mode
     tuareg
     typing-game
     typit
     undo-tree
     visual-regexp
     web
     web-beautify
     web-completion-data
     web-mode
     websocket
     wgrep
     whitespace-cleanup-mode
     with-editor
     yaml-mode
     yari
     yasnippet
     zeal-at-point
     zencoding-mode
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(provide 'setup-package)
;;; setup-package.el ends here
