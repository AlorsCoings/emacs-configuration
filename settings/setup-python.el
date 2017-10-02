;;; package --- setup-python

;;; Commentary:
;;; Emacs configuration file for python

;;; Code:

(require 'python)

;; (setq python-indent-offset 4)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")
;; (setq python-shell-interpreter "python")
;; (setq python-shell-interpreter "ipython")
;; (setq python-shell-interpreter-args "--profile=dev")
;; (setq python-shell-interpreter-args "")

(define-key python-mode-map (kbd "C-M-S-h") 'python-mark-defun)
(define-key inferior-python-mode-map (kbd "M-d") 'comint-previous-input)
(define-key inferior-python-mode-map (kbd "C-d") 'previous-line)
(define-key inferior-python-mode-map (kbd "M-r") 'subword-forward)
(define-key inferior-python-mode-map (kbd "M-s") 'comint-next-input)
(define-key inferior-python-mode-map (kbd "C-s") 'next-line)
(define-key inferior-python-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)

;; pip install jedi flake8 importmagic autopep8
(require 'elpy)
(elpy-enable)

(define-key elpy-mode-map (kbd "C-c C-p") 'run-python)

;; (define-key elpy-mode-map (kbd "C-c C-r") 'python-shell-send-region)
;; (require 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; (define-key anaconda-mode-map (kbd "M-r") nil)
;; (define-key anaconda-mode-map (kbd "C-c C-d") 'anaconda-mode-show-doc)
;; (define-key anaconda-mode-map (kbd "M-.") 'anaconda-mode-find-references)
;; Remove warning when starting python shell
;; (add-to-list 'python-shell-completion-native-disabled-interpreters "python")

;; (add-hook 'python-mode-hook #'flycheck-mode)

;; (elpy-enable)
;; (elpy-use-ipython)
;; (require 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; (define-key anaconda-mode-map (kbd "M-r") nil)
;; (define-key anaconda-mode-map (kbd "C-c C-d") 'anaconda-mode-show-doc)
;; (define-key anaconda-mode-map (kbd "M-.") 'anaconda-mode-find-references)

;; (remove-hook 'anaconda-mode-response-read-fail-hook
;;              'anaconda-mode-show-unreadable-response)
;; Add intern dependencies
;; Virtual environment
;; M-x pythonic-activate RET /path/to/virtualenv RET

(provide 'setup-python)
;;; setup-python.el ends here
