;;; package --- setup-js2-mode

;;; Commentary:
;;; Emacs configuration file for js2

;;; Code:

;; Install jshint
;; npm install -g jshint

(require 'js2-mode)

(setq-default js2-allow-rhino-new-expr-initializer nil
              js2-idle-timer-delay 0.1
              js2-strict-inconsistent-return-warning nil
              js2-include-rhino-externs nil
              js2-concat-multiline-strings 'eol
              js2-rebind-eol-bol-keys nil
              js-indent-level 4
              js2-strict-missing-semi-warning t
              js2-strict-trailing-comma-warning t)

(setq-default js2-global-externs
              '("expect"
                "it"
                "inject"
                "beforeEach"
                "describe"
                "angular"
                "module"
                "require"
                "buster"
                "sinon"
                "assert"
                "refute"
                "setTimeout"
                "clearTimeout"
                "setInterval"
                "clearInterval"
                "location"
                "__dirname"
                "console"
                "JSON"))

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(require 'js2-imenu-extras)
(js2-imenu-extras-setup)

(define-key js2-mode-map (kbd ";")
  (lambda () (interactive)
    (if (looking-at ";")
        (forward-char)
      (funcall 'self-insert-command 1))))

(require 'json)
(require 'json-mode)

(define-key json-mode-map (kbd "C-c C-n") 'web-beautify-js)

(require 'karma)

(define-key js2-mode-map (kbd "C-c C-n") 'web-beautify-js)
(define-key js2-mode-map (kbd "M-j") (lambda() (interactive) (join-line -1)))

(provide 'setup-js2-mode)
;;; setup-js2-mode.el ends here
