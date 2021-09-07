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
(setq python-shell-interpreter-args "-u")

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

;; Remove highlight of indentation
(delete 'elpy-module-highlight-indentation elpy-modules)

(defun elpy-shell-switch-to-shell-in-root ()
  "Switch to python shell starting in project root."
  (interactive)
  (let ((default-directory (elpy-project-root)))
    (elpy-shell-switch-to-shell)))

(define-key elpy-mode-map (kbd "C-c C-p") 'elpy-shell-switch-to-shell-in-root)
(define-key elpy-mode-map (kbd "C-c C-r f") 'elpy-format-code)
(define-key elpy-mode-map (kbd "C-c p") 'run-python)

;; PYLINT Messages references
;; Disable them by using '# pylint: disable=C0103'
;; C0103: Invalid %s name

(setq elpy-rpc-python-command "python3")

(require 'flycheck)
(setq flycheck-python-pycompile-executable "python3")
(setq flycheck-python-pylint-executable "python3")
(setq flycheck-python-flake8-executable "python3")
(setq flycheck-disabled-checkers '("python-flake8"))


(add-hook 'python-mode-hook (lambda()
                              (set-fill-column 120)))
;; (elpy-use-ipython)
;; (elpy-use-cpython)

;; Virtual environment
(define-key elpy-mode-map (kbd "C-c C-a") 'pyvenv-activate)

;; Setup for tensorflow
;; (setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":/home/gros/projects/tensorflow_models/research:/home/gros/projects/tensorflow_models/research/slim"))

(setq elpy-rpc-timeout 10)

(require 'company-jedi)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(provide 'setup-python)
;;; setup-python.el ends here
