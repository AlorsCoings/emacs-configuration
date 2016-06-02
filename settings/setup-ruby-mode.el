;;; package --- setup-ruby-mode

;;; Commentary:
;;; Emacs configuration file for ruby

;;; Code:

(declare-function after-load "sane-defaults" (feature &rest body))

(require 'ruby-mode)
(require 'ruby-hash-syntax)

;; avoid ridiculous ruby indentation
(setq ruby-deep-indent-paren nil)

(require 'ruby-compilation)

(after-load 'ruby-mode
  (let ((m ruby-mode-map))
    (define-key m (kbd "C-c C-c") 'ruby-compilation-this-buffer)
    (define-key m (kbd "C-c c") 'ruby-compilation-this-test)))

(require 'inf-ruby)
(define-key ruby-mode-map (kbd "C-S-z") 'inf-ruby)

(add-hook 'inf-ruby-mode-hook
          (lambda ()
            (define-key inf-ruby-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; M-. to jump to the definition
;; M-, to jump back
;; C-c C-d to see the documentation
;; C-c C-k to refresh Rails environment
;; C-M-i to complete the symbol at point
(require 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))

(require 'yari)
(define-key ruby-mode-map (kbd "M-.") 'yari)

(defun ruby--jump-to-test ()
  "Jump to test within ruby."
  (find-file
   (replace-regexp-in-string
    "/lib/" "/test/"
    (replace-regexp-in-string
     "/\\([^/]+\\).rb$" "/test_\\1.rb"
     (buffer-file-name)))))

(defun ruby--jump-to-lib ()
  "Jump to lib within ruby."
  (find-file
   (replace-regexp-in-string
    "/test/" "/lib/"
    (replace-regexp-in-string
     "/test_\\([^/]+\\).rb$" "/\\1.rb"
     (buffer-file-name)))))

(defun ruby-jump-to-other ()
  "Jump to other within ruby."
  (interactive)
  (if (string-match-p "/test/" (buffer-file-name))
      (ruby--jump-to-lib)
    (ruby--jump-to-test)))

(define-key ruby-mode-map (kbd "C-c t") 'ruby-jump-to-other)

(provide 'setup-ruby-mode)
;;; setup-ruby-mode.el ends here
