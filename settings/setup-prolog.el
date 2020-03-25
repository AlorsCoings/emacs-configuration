;;; package --- setup-prolog

;;; Commentary:
;;; Emacs configuration file for prolog

;;; Code:

(require 'prolog)

(define-key prolog-mode-map (kbd "C-c C-c") 'prolog-consult-region)

(define-key prolog-inferior-mode-map (kbd "M-d") 'comint-previous-input)
(define-key prolog-inferior-mode-map (kbd "C-d") 'previous-line)
(define-key prolog-inferior-mode-map (kbd "M-r") 'subword-forward)
(define-key prolog-inferior-mode-map (kbd "M-s") 'comint-next-input)
(define-key prolog-inferior-mode-map (kbd "C-s") 'next-line)
(define-key prolog-inferior-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)

(add-hook 'prolog-inferior-mode-hook
          (lambda ()
            (setq
             indent-tabs-mode nil ;; i.e. indent with spaces
             tab-width 4          ;; i.e. tabs consts of 4 spaces
             )))

(defun my-comint-tab ()
  "Insert the char in the buffer or pass it directly to the process."
  (interactive)
  (insert ";")
  (comint-send-input))

(define-key prolog-inferior-mode-map (kbd "<tab>") #'my-comint-tab)

(provide 'setup-prolog)
;;; setup-prolog.el ends here
