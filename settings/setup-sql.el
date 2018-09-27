;;; package --- setup-sql

;;; Commentary:
;;; Emacs sql configuration

;;; Code:

(declare-function after-load "sane-defaults" (feature &rest body))

(require 'sql)
(require 'sql-indent)

(defun sanityinc/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if sql-buffer
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (sanityinc/pop-to-sqli-buffer))))

(after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer))

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
  "Font lock everything in sql interactive mode."
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)

(require 'page-break-lines)
(push 'sql-mode page-break-lines-modes)

(require 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

(defun my:sqlup-capitalize-keywords-in-region (&optional begin end)
  "Call `sqlup-capitalize-keywords-in-region' for buffer if no region selected.

Otherwise call `sqlup-capitalize-keywords-in-region' from BEGIN to END."
  (interactive "r")
  (unless mark-active
    (setq begin (point-min))
    (setq end (point-max)))
  ;; sqlup-capitalize-keywords-in-region seems to go to far
  (setq end (- end 2))
  (sqlup-capitalize-keywords-in-region begin end)
  )

(define-key sql-mode-map (kbd "C-c C-u") 'my:sqlup-capitalize-keywords-in-region)
(define-key sql-mode-map (kbd "C-c u") 'my:sqlup-capitalize-keywords-in-region)
(define-key sql-interactive-mode-map (kbd "C-c C-u") 'my:sqlup-capitalize-keywords-in-region)
(define-key sql-interactive-mode-map (kbd "C-c u") 'my:sqlup-capitalize-keywords-in-region)

(provide 'setup-sql)
;;; setup-sql.el ends here
