;;; package --- setup-smartparens

;;; Commentary:
;;; Emacs configuration file for smartparens

;;; Code:

(require 'smartparens)
(require 'smartparens-latex)

(smartparens-global-mode t)
;; (--each '(css-mode-hook
;;           restclient-mode-hook
;;           js-mode-hook
;;           java-mode
;;           ruby-mode
;;           markdown-mode
;;           groovy-mode)
;;   (add-hook it 'turn-on-smartparens-mode))
(show-smartparens-global-mode t)

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "\"" nil :actions '(wrap))

(mapc (lambda (info)
        (let ((key (kbd (car info)))
              (function (car (cdr info))))
          (global-set-key key function)))
      '(("C-M-r" sp-forward-sexp)
        ("C-M-t" sp-backward-sexp)

        ("C-M-s" sp-down-sexp)
        ;; ("C-M-d" sp-backward-up-sexp)

        ("C-M-S-s" sp-splice-sexp-killing-forward)
        ("C-M-S-d" sp-splice-sexp-killing-backward)

        ("C-M-f" sp-transpose-sexp)

        ("C-M-e" sp-kill-sexp)
        ("C-M-'" sp-backward-kill-sexp)
        ("C-M-w" sp-copy-sexp)

        ;; ("C-k" sp-kill-hybrid-sexp)

        ("C-M-S-r" sp-forward-slurp-sexp)
        ("C-M-S-l" sp-forward-barf-sexp)
        ("C-M-S-t" sp-backward-slurp-sexp)
        ("C-M-S-v" sp-backward-barf-sexp)

        ("C-M-(" sp-rewrap-sexp)
        ("C-\"" sp-rewrap-sexp)
        ("C-M-)" sp-unwrap-sexp)

        ("C-M-S-c" sp-convolute-sexp)

        ("C-M-j" sp-join-sexp)
        ("C-M-S-j" sp-split-sexp)

        ("C-M-S-e" sp-splice-sexp-killing-forward)
        ("C-M-?" sp-splice-sexp-killing-backward)

        ("C-e" sp-delete-char)
        ("C-'" sp-backward-delete-char)
        ("M-e" sp-kill-word)))

(define-key smartparens-mode-map (kbd "M-'") (lambda () (interactive) (sp-kill-word -1)))

(provide 'setup-smartparens)
;;; setup-smartparens.el ends here
