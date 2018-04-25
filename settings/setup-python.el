;;; package --- setup-python

;;; Commentary:
;;; Emacs configuration file for python

;;; Code:

(require 'python)

;; (setq python-indent-offset 4)
(add-to-list 'python-shell-completion-native-disabled-interpreters "python")
(setq python-shell-interpreter "python3")
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

;; Remove highlight of indentation
(delete 'elpy-module-highlight-indentation elpy-modules)

(setq elpy-rpc-python-command "python3")

(require 'flycheck)
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-pylint-executable "python3")
(setq flycheck-disabled-checkers '("python-flake8"))

(add-hook 'python-mode-hook (lambda()
                             (flymake-mode nil)
                             (set-fill-column 90)))
;; (elpy-use-ipython)
;; (elpy-use-cpython)

;; Virtual environment
;; M-x pythonic-activate RET /path/to/virtualenv RET

(provide 'setup-python)
;;; setup-python.el ends here
