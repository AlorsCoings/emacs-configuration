;;; package --- setup-lisp

;;; Commentary:
;;; Emacs configuration file for Lisp

;;; Code:

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") " - Emacs ♥ you!\n\n"))

(require 'slime)

;; (setq inferior-lisp-program "/usr/bin/sbcl")
(setq inferior-lisp-program "/usr/bin/clisp")
(setq slime-contribs '(slime-fancy))
(eval-after-load 'slime
  '(define-key slime-mode-map (kbd "M-h") 'slime-documentation-lookup))

(require 'slime-repl)
(after-load 'slime-repl
  (define-key slime-repl-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer))

(setq slime-net-coding-system 'utf-8-unix)
(let ((extras (when (require 'slime-company nil t)
                '(slime-company))))
  (slime-setup (append '(slime-repl slime-fuzzy) extras)))
(add-hook 'slime-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))
(setq slime-auto-start 'always)

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Search lispdoc.com for SYMBOL, which is by default the symbol currently under the curser."
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

(provide 'setup-lisp)
;;; setup-lisp.el ends here
