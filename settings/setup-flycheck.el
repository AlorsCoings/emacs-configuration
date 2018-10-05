;;; package --- setup-flycheck

;;; Commentary:
;;; Emacs configuration file for flycheck

;;; Code:

(require 'flycheck)

;; Make flycheck use the current load-path
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 3.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)

;; Remove newline checks, since they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically '(save
                                            mode-enabled))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
;; (setq flycheck-eslintrc "~/.eslintrc")

(require 'flycheck-pos-tip)
(setq flycheck-display-errors-function (function flycheck-pos-tip-error-messages))

;; https://www.emacswiki.org/emacs/Flycheck
(require 'json)
(defun parse-jslinter-warning (warning)
  (flycheck-error-new
   :line (1+ (cdr (assoc 'line warning)))
   :column (1+ (cdr (assoc 'column warning)))
   :message (cdr (assoc 'message warning))
   :level 'error))
(defun jslinter-error-parser (output checker buffer)
  (mapcar 'parse-jslinter-warning
          (cdr (assoc 'warnings (aref (json-read-from-string output) 0)))))
(flycheck-define-checker javascript-jslinter
  "A JavaScript syntax and style checker based on JSLinter.

See URL `https://github.com/tensor5/JSLinter'."
  :command ("/home/toad/local/bin/jslint" "--raw" source)
  :error-parser jslinter-error-parser
  :modes (js-mode js2-mode js3-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
