;;; package --- setup-c

;;; Commentary:
;;; This my emacs configuration file for c/c++ coding

;;; Code:

(require 'cc-mode)
(setq c-default-style "k&r")
(setq c-basic-offset 4)

;; google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

;; (require 'cmake-mode)
;; (require 'cpputils-cmake)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (if (derived-mode-p 'c-mode 'c++-mode)
;;                 (cppcm-reload-all)
;;               )))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
;; (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;; (require 'auto-complete-clang)
;; (ac-common-setup)
;; (add-to-list 'ac-sources 'ac-clang)


;; (require 'auto-complete)
;; (require 'auto-complete-c-headers)
;; ;; auto-complete-c-headers
;; ;; associated auto-complete to a key ???
;; (defun my:ac-c-header-init()
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;;   ;; gcc -xc++ -E -v -
;;  (add-to-list 'achead:include-directories '"/usr/include/c++/4.8")
;;   (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu/c++/4.8")
;;   (add-to-list 'achead:include-directories '"/usr/include/c++/4.8/backward")
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/4.8/include")
;;   (add-to-list 'achead:include-directories '"/usr/local/include")
;;   (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed")
;;   (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu")
;;   (add-to-list 'achead:include-directories '"/usr/include")
;;   )
;; (add-hook 'c++-mode-hook 'my:ac-c-header-init)
;; (add-hook 'c-mode-hook 'my:ac-c-header-init)

;; CEDET
;; (semantic-mode 1)

;; (setq semantic-load-turn-everything-on t)

;; (defun my:add-semantic-to-autocomplete()
;;   (add-to-list 'ac-sources 'ac-source-semantic-raw)
;;   )
;; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; (global-set-key [(control tab)] 'semantic-analyze-possible-completions)

;; idle mode
;; (global-semantic-idle-scheduler-mode 1)

;; Semantic
;; might want to disable this line because interference with autocomplete
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-show-unmatched-syntax-mode t)

;; CC-mode
;; (defun my:ac-sources-init()
;;   (lambda ()
;;     (setq ac-sources (append '(ac-source-semantic) ac-sources))
;;     (local-set-key (kbd "RET") 'newline-and-indent)
;;     (linum-mode t)
;;     (semantic-mode t)))
;; (add-hook 'c++-mode-hook 'my:ac-sources-init)
;; (add-hook 'c-mode-hook 'my:ac-sources-init)

(define-key c++-mode-map (kbd "C-c C-c") 'compile)
(define-key c-mode-map (kbd "C-c C-c") 'compile)

;; Autocomplete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories
;; 			 (expand-file-name "~/.emacs.d/elpa/auto-complete-20150408.1132/dict"))
;; (setq ac-comphist-file (expand-file-name
;;                         "~/.emacs.d/ac-comphist.dat"))
;; (ac-config-default)
;; (auto-complete-mode 1)
;; (require 'auto-complete-c-headers)
;; (add-to-list 'ac-sources 'ac-source-c-headers)

(require 'gud)
(define-key gud-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)

;; gdb print vector
;; print *(myVector._M_impl._M_start)@myVector.size()

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
(defun c-eldoc-define-keybindings (map)
  "Eldoc defines key bindings in MAP."
  (define-key map (kbd "C-c C-d") 'c-eldoc-force-cache-update))
(add-hook 'c-mode-hook
          (lambda ()
            (c-eldoc-define-keybindings c-mode-map)))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-eldoc-define-keybindings c++-mode-map)))
(setq c-eldoc-cpp-command "/usr/bin/clang")
;; (defvar c-eldoc-cpp-command "/lib/cpp ") ;; compiler
;; (defvar c-eldoc-cpp-macro-arguments "-dD -w -P")
;; (defvar c-eldoc-cpp-normal-arguments "-w -P")
;; (defvar c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ") ;; include flags

(provide 'setup-c)
;;; setup-c.el ends here
