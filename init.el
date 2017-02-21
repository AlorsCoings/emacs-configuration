;;; package --- init.el

;;; Commentary:
;;; Primary Emacs configuration which call others files

;;; Code:

(package-initialize)

;; Seed the random-number generator
(random t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)

;; ;; Start emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist '((fullscreen . maximized)))

(defvar settings-dir
  (expand-file-name "settings" user-emacs-directory))

(require 'gnus)
(require 'gnus-start)
(setq
 gnus-home-directory "~/.gnus.d/"
 gnus-init-file (expand-file-name "setup-gnus.el" settings-dir)
 message-directory (expand-file-name "Mail/" gnus-home-directory))

(require 'epa)
;; (epa-file-enable)
;; (setq epg-gpg-program "/usr/bin/gpg2")

;; Set up load path
(add-to-list 'load-path settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(load custom-file)

;; Write backup nfiles to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save cursor position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  "Install extensions if they're missing."
  (packages-install
   '(
     ace-jump-mode
     alert
     angular-snippets
     anzu
     apache-mode
     apel
     arduino-mode
     async
     auctex
     auctex-lua
     auto-compile
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
     edn
     elisp-slime-nav
     elnode
     emacs-eclim
     emmet-mode
     epc
     epl
     exec-path-from-shell
     expand-region
     f
     fakir
     feature-mode
     fill-column-indicator
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
     highlight-escape-sequences
     hl-sexp
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
     jade-mode
     jedi-core
     js2-mode
     js2-refactor
     json-mode
     json-reformat
     json-snatcher
     jump-char
     know-your-http-well
     kv
     latex-extra
     legalese
     let-alist
     log4e
     logito
     lua-mode
     magit
     magit-popup
     markdown-mode
     markdown-mode+
     math-symbol-lists
     merlin
     misc-cmds
     misc-fns
     mmm-mode
     move-text
     multifiles
     multiple-cursors
     nodejs-repl
     noflet
     oauth2
     org
     org-mobile-sync
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
     prodigy
     puppet-mode
     python-environment
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
     sqlplus
     sqlup-mode
     swiper
     tagedit
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
     wgrep
     whitespace-cleanup-mode
     with-editor
     yaml-mode
     yari
     yasnippet
     zencoding-mode
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Set up appearance early
(require 'appearance)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-c C-r C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Setup extensions
(require 'setup-ido)
(require 'setup-smartparens)
(require 'setup-org)
(require 'setup-dired)
(require 'setup-rgrep)
(require 'setup-shell)
(require 'setup-yasnippet)
(require 'setup-perspective)
(require 'setup-web)
(require 'setup-css)
(require 'setup-latex)
(require 'setup-ocaml)
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
(require 'prodigy)
(require 'setup-magit)
(require 'setup-php)
(require 'setup-lua)
(require 'setup-pdf-tools)
(require 'setup-R)
(require 'setup-python)

(eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
(require 'smartparens-config)

(--each '(css-mode-hook
          restclient-mode-hook
          js-mode-hook
          java-mode
          ruby-mode
          markdown-mode
          groovy-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
(eval-after-load 'lisp-mode '(require 'setup-lisp))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)
(require 'setup-flycheck)
(require 'setup-company)

;; Map files to modes
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)

;; Functions (load all files in defuns-dir)
(defvar defuns-dir (expand-file-name "defuns" user-emacs-directory))

(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)
(require 'google-translate)
(require 'google-translate-default-ui)
(require 'legalese)
(require 'misc)

;; Convenient printing
(require 'printing)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

(require 'atomic-chrome)
;; Emacs server
(require 'server)
(unless (server-running-p)
  (atomic-chrome-start-server)
  (add-hook 'atomic-chrome-edit-mode-hook (lambda () (flyspell-mode)))
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(icomplete-mode 99)

(provide 'init)
;;; init.el ends here
