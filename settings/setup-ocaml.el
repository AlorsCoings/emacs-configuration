;;; package --- setup-ocaml

;;; Commentary:
;;; Emacs appearance configuration

;;; Code:

;; (setq auto-mode-alist (cons '("\\.ml[iylp]?" . caml-mode) auto-mode-alist))
;; (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;; (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger." t)

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(require 'tuareg)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (setq tuareg-in-indent 0)
            (setq tuareg-let-always-indent t)
            (setq tuareg-let-indent tuareg-default-indent)
            (setq tuareg-with-indent 0)
            (setq tuareg-function-indent 0)
            (setq tuareg-fun-indent 0)
            (setq tuareg-match-indent 0)
            (setq tuareg-begin-indent tuareg-default-indent)
            (setq tuareg-use-smie nil)
            ))
(setq tuareg-library-path "/usr/lib/ocaml")
(add-hook 'tuareg-interactive-mode-hook
          (lambda ()
            (define-key tuareg-interactive-mode-map (kbd "C-e") 'comint-delchar-or-eof-or-kill-buffer)))

(provide 'setup-ocaml)
;;; setup-ocaml.el ends here
