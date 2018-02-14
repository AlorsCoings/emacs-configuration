;;; package --- setup-c

;;; Commentary:
;;; This my Emacs configuration file for c/c++ coding

;;; Code:

(require 'cc-mode)
(setq c-default-style "k&r")
(setq c-basic-offset 4)

;; google-c-style
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; (add-hook 'c++-mode-common-hook 'google-set-c-style)
;; (add-hook 'c++-mode-common-hook 'google-make-newline-indent)

;; (semantic-mode 1)
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-show-unmatched-syntax-mode t)

;; (require 'cmake-mode)
;; (require 'cpputils-cmake)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (if (derived-mode-p 'c-mode 'c++-mode)
;;                 (cppcm-reload-all)
;;               )))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
;; (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

(require 'gud)
(define-key gud-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)

;; gdb print vector
;; print *(myVector._M_impl._M_start)@myVector.size()

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
;; (setq c-eldoc-cpp-command "/usr/bin/clang")
;; (setq c-eldoc-cpp-command "/usr/bin/cpp")
;; (defvar c-eldoc-cpp-command "/lib/cpp ") ;; compiler
;; (defvar c-eldoc-cpp-macro-arguments "-dD -w -P")
;; (defvar c-eldoc-cpp-normal-arguments "-w -P")
;; (defvar c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ") ;; include flags

(define-key c-mode-base-map (kbd "C-c C-c") 'compile)
(define-key c-mode-base-map (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key c-mode-base-map (kbd "C-<tab>") 'company-complete)
(define-key c-mode-base-map (kbd "C-e") 'c-electric-delete-forward)
(define-key c-mode-base-map (kbd "C-d") 'previous-line)
(define-key c-mode-base-map (kbd "M-e") 'kill-word)
(define-key c-mode-map (kbd "C-c C-d") 'c-eldoc-force-cache-update)

(require 'company)
(add-to-list 'company-backends 'company-c-headers)
;; (setq company-clang-arguments '("-I/usr/include" "-I/usr/local/include"))

(require 'compile)
(define-key compilation-mode-map (kbd "C-o") 'other-window)
(define-key compilation-mode-map (kbd "C-S-o") (lambda() (interactive) (other-window -1)))

(provide 'setup-c)
;;; setup-c.el ends here
