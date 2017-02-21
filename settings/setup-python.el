;;; package --- setup-python

;;; Commentary:
;;; Emacs configuration file for python

;;; Code:

(require 'python)

(setq python-indent-offset 4)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
(setq python-shell-interpreter "python3")

(define-key inferior-python-mode-map (kbd "C-d") 'comint-previous-input)
(define-key inferior-python-mode-map (kbd "C-s") 'comint-next-input)
(define-key inferior-python-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)

(setq python-shell-exec-path '("~/anaconda3/bin"))

;; (require 'elpy)
;; (elpy-enable)

(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(define-key anaconda-mode-map (kbd "M-r") nil)
(define-key anaconda-mode-map (kbd "C-c C-d") 'anaconda-mode-show-doc)
(define-key anaconda-mode-map (kbd "M-.") 'anaconda-mode-find-references)
;; Add intern dependencies
(add-to-list 'python-shell-extra-pythonpaths "~/anaconda3")
;; Virtual environment
;; M-x pythonic-activate RET /path/to/virtualenv RET

(provide 'setup-python)
;;; setup-python.el ends here
