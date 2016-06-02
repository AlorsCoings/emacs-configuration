;;; package --- setup-ispell

;;; Commentary:
;;; Emacs Ispell configuration

;;; Code:

(declare-function after-load "sane-defaults" (feature &rest body))

(require 'ispell)

(setq ispell-highlight-face 'custom-themed)
(setq ispell-dictionary "french")
(defun fd-switch-dictionary()
  "Change dictionary for ispell."
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "french") "english" "french")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "If RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if RUN-TOGETHER
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))

(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(require 'flyspell)

;; Add spell-checking in comments for all programming language modes
;; (if (fboundp 'prog-mode)
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;   (dolist (hook '(lisp-mode-hook
;;                   emacs-lisp-mode-hook
;;                   scheme-mode-hook
;;                   clojure-mode-hook
;;                   ruby-mode-hook
;;                   yaml-mode
;;                   python-mode-hook
;;                   shell-mode-hook
;;                   php-mode-hook
;;                   css-mode-hook
;;                   haskell-mode-hook
;;                   caml-mode-hook
;;                   nxml-mode-hook
;;                   crontab-mode-hook
;;                   perl-mode-hook
;;                   c++-mode-hook
;;                   c-mode-hook
;;                   tcl-mode-hook
;;                   sh-mode-hook
;;                   javascript-mode-hook))
;;     (add-hook hook 'flyspell-prog-mode)))

(after-load 'flyspell
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

(global-set-key (kbd "<f8>") 'fd-switch-dictionary)

(provide 'setup-ispell)
;;; setup-ispell.el ends here
