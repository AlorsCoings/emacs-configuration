;;; package --- setup-lisp

;;; Commentary:
;;; Emacs configuration file for lisp

;;; Code:

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") " - Emacs ♥ you!\n\n"))

(require 'ipretty)
(ipretty-mode 1)

;; Automatic byte compilation
(require 'auto-compile)
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)

;; Load .el if newer than corresponding .elc
(setq load-prefer-newer t)

(provide 'setup-lisp)
;;; setup-lisp.el ends here
