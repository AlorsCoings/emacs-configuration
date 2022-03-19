;;; package --- setup-kotlin

;;; Commentary:
;;; Emacs configuration file for kotlin

;;; Code:

(require 'kotlin-mode)
(require 'flycheck-kotlin)
(add-hook 'kotlin-mode-hook 'flycheck-mode)

(define-key kotlin-mode-map (kbd "C-c C-p") 'kotlin-repl)
(define-key kotlin-mode-map (kbd "C-c C-c") 'kotlin-send-region)
(define-key kotlin-mode-map (kbd "C-u C-c C-c") 'kotlin-send-buffer)

;(define-key inferior-kotlin-mode-map (kbd "M-d") 'comint-previous-input)
;(define-key inferior-kotlin-mode-map (kbd "C-d") 'previous-line)
;(define-key inferior-kotlin-mode-map (kbd "M-r") 'subword-forward)
;(define-key inferior-kotlin-mode-map (kbd "M-s") 'comint-next-input)
;(define-key inferior-kotlin-mode-map (kbd "C-s") 'next-line)
;(define-key inferior-kotlin-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)

;; C-c C-b         kotlin-send-buffer
;; C-c C-c         kotlin-send-block
;; C-c C-n         kotlin-send-line
;; C-c C-r         kotlin-send-region
;; C-c C-z         kotlin-repl

(provide 'setup-kotlin)
;;; setup-kotlin.el ends here
