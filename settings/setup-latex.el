;;; package --- setup-latex

;;; Commentary:
;;; This my emacs configuration file for latex

;;; Code:

;; 'C-c C-p C-d' (for 'Preview/Document')
;; stop the background process by pressing 'C-c C-k'
;; 'C-c C-p C-p' (for 'Preview/at point')
;; 'C-c C-p C-c C-d' 'preview-clearout-document'
;; 'C-c C-p C-c C-p' 'preview-clearout-at-point'
;; 'C-c C-t C-p' This command toggles between DVI and PDF output
;; 'C-c C-w' ('TeX-toggle-debug-boxes') you can toggle whether AUCTeX should notify you of overfull and underfull boxes in addition to regular errors.


(setenv "PATH"
		(concat
		 "/usr/texbin" ":"
		 (getenv "PATH")))

(require 'tex)
(TeX-global-PDF-mode t)
(setq TeX-PDF-mode t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master t)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; to use to compile with xetex
;; (setq-default TeX-engine 'xetex)
;; (setq-default TeX-engine 'luatex)

(require 'tex-mode)

(define-key latex-mode-map (kbd "C-S-l") 'LaTeX-edit-Lua-code-start)
(define-key latex-mode-map (kbd "C-c C-a") 'latex/compile-commands-until-done)

;; C-c C-q `latex/clean-fill-indent-environment'
;; Completely cleans up the entire current environment
;; 1. Removing extraneous spaces and blank lines.
;; 2. Filling text (and only text, not equations).
;; 3. Indenting everything.

(provide 'setup-latex)
;;; setup-latex.el ends here
