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

(require 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(eval-after-load 'slime
  '(define-key slime-mode-map (kbd "M-h") 'slime-documentation-lookup))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(setq slime-auto-start 'always)

(provide 'setup-lisp)
;;; setup-lisp.el ends here
