;;; package --- setup-paredit

;;; Commentary:
;;; Emacs configuration file for paredit

;;; Code:

(require 'paredit)
(require 'dash)
(require 's)

(defun paredit-wrap-round-from-behind ()
  "With paredit wrap round from behind."
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun paredit-wrap-square-from-behind ()
  "With paredit wrap square from behind."
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-square))

(defun paredit-wrap-curly-from-behind ()
  "With paredit wrap curly from behind."
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-curly))

(defun paredit-kill-region-or-backward-word ()
  "With paredit kill region or backward word."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
(define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
(define-key paredit-mode-map (kbd "M-s-8") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-s-9") 'paredit-wrap-square-from-behind)
(define-key paredit-mode-map (kbd "M-s-(") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "M-s-)") 'paredit-wrap-curly-from-behind)

(define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region-or-backward-word)
(define-key paredit-mode-map (kbd "M-C-<backspace>") 'backward-kill-sexp)

;; Change nasty paredit keybindings
(defvar my-nasty-paredit-keybindings-remappings
  '(("M-s"         "C-M-S-d"         paredit-splice-sexp)
    ("M-<up>"      "C-M-S-p"      paredit-splice-sexp-killing-backward)
    ("M-<down>"    "C-M-S-n"    paredit-splice-sexp-killing-forward)
    ("C-<right>"   "C-M-S-f"   paredit-forward-slurp-sexp)
    ("C-<left>"    "C-M-S-b"    paredit-forward-barf-sexp)
    ("C-M-<left>"  "C-M-S-a"  paredit-backward-slurp-sexp)
    ("C-M-<right>" "C-M-S-e" paredit-backward-barf-sexp)))

(define-key paredit-mode-map (kbd "C-M-S-r") 'paredit-raise-sexp)

(--each my-nasty-paredit-keybindings-remappings
 (let ((original (car it))
        (replacement (cadr it))
        (command (car (last it))))
    (define-key paredit-mode-map (read-kbd-macro original) nil)
    (define-key paredit-mode-map (read-kbd-macro replacement) command)))

;; don't hijack \ please
(define-key paredit-mode-map (kbd "\\") nil)

(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; functions in smartparens that do not have an equivalent in paredit - take a look at them
(when nil
  '(sp-beginning-of-sexp
    sp-end-of-sexp
    sp-next-sexp
    sp-previous-sexp
    sp-kill-sexp
    sp-unwrap-sexp
    sp-backward-unwrap-sexp
    sp-select-next-thing-exchange
    sp-select-next-thing
    sp-forward-symbol
    sp-backward-symbol))

(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'css-mode-hook 'paredit-everywhere-mode)

(provide 'setup-paredit)
;;; setup-paredit.el ends here
