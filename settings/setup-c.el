;;; package --- setup-c

;;; Commentary:
;;; This my Emacs configuration file for c/c++ coding

;;; Code:

(require 'cc-mode)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu")))

(setq c-basic-offset 4)

;; (semantic-mode 1)
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-show-unmatched-syntax-mode t)

(require 'gud)
(define-key gud-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)

;; gdb print vector
;; print *(myVector._M_impl._M_start)@myVector.size()

(define-key c-mode-base-map (kbd "C-c C-c") 'compile)
(define-key c-mode-base-map (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key c-mode-base-map (kbd "C-<tab>") 'company-complete)
(define-key c-mode-base-map (kbd "C-e") 'c-electric-delete-forward)
(define-key c-mode-base-map (kbd "C-d") 'previous-line)
(define-key c-mode-base-map (kbd "C-M-e") nil)
(define-key c-mode-base-map (kbd "M-<return>") 'c-context-line-break)

(defun astyle-this-buffer ()
  "Reformat the buffer with custom style."
  (interactive)
  (let ((temp-point (point)))
    (shell-command-on-region (point-min) (point-max)
                             "astyle --style=kr --indent=spaces=4 --align-pointer=type --align-reference=type --remove-brackets --max-code-length=140 --break-after-logical"
                             (current-buffer) t
                             (get-buffer-create "*Astyle Errors*") t)
    (goto-char temp-point)))

(define-key c-mode-base-map (kbd "C-c C-r") 'astyle-this-buffer)

(require 'company)

(require 'compile)
(define-key compilation-mode-map (kbd "C-o") 'other-window)
(define-key compilation-mode-map (kbd "C-S-o") (lambda() (interactive) (other-window -1)))

(require 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(require 'flycheck-google-cpplint)
(setq flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
(setq flycheck-gcc-include-path '("/usr/include/python3.6" "/usr/local/include/QualityCoversLib"))
(flycheck-add-next-checker 'c/c++-gcc
                           'c/c++-googlelint
                           'append)
(custom-set-variables
 '(flycheck-googlelint-verbose "3")
 '(flycheck-googlelint-filter "+build,+whitespace,+runtime,+readability,-whitespace/braces,-whitespace/indent")
 '(flycheck-googlelint-linelength "140"))

(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++11")
                           (setq fill-column 140)))

(require 'cmake-mode)

(provide 'setup-c)
;;; setup-c.el ends here
