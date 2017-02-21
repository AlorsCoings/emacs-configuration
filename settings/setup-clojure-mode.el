;;; package --- setup-clojure-mode

;;; Commentary:
;;; Emacs configuration file for clojure

;;; Code:

(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)

(defadvice clojure-test-run-tests (before save-first activate)
  "Save before `cloujure-test-run-tests'."
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  "Save before `nrepl-load-current-buffer'."
  (save-buffer))

(require 'clj-refactor)

(setq cljr-favor-prefix-notation nil)
(setq cljr-favor-private-functions nil)

;; (cljr-add-keybindings-with-modifier "C-")
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

(define-key clojure-mode-map [remap sp-forward-sexp] 'clojure-forward-logical-sexp)
(define-key clojure-mode-map [remap sp-backward-sexp] 'clojure-backward-logical-sexp)

(add-hook 'clojure-mode-hook #'clj-refactor-mode)

(add-to-list 'cljr-project-clean-functions 'cleanup-buffer)

(require 'cider)

(define-key cider-repl-mode-map (kbd "<home>") nil)
(define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-repl-mode-map (kbd "M-d") 'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "M-s") 'cider-repl-next-input)
(define-key cider-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-c C-q") 'nrepl-close)
(define-key cider-mode-map (kbd "C-c C-Q") 'cider-quit)


;; Don't warn me about the dangers of clj-refactor, fire the missiles!
;; (setq cljr-warn-on-eval nil)

;; ;; Don't prompt for symbols
;; (setq cider-prompt-for-symbol nil)

;; ;; Use figwheel for cljs repl
;; (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Indent and highlight more commands
(put-clojure-indent 'match 'defun)

;; Hide nrepl buffers when switching buffers (switch to by prefixing with space)
;; (setq nrepl-hide-special-buffers t)

;; auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
;; (setq cider-repl-pop-to-buffer-on-connect t)

;; ;; Pretty print results in repl
(setq cider-repl-use-pretty-printing t)

(add-to-list 'cljr-magic-require-namespaces '("time" . "clj-time.core"))
(add-to-list 'cljr-magic-require-namespaces '("log"  . "clojure.tools.logging"))
(add-to-list 'cljr-magic-require-namespaces '("json" . "cheshire.core"))

(require 'flycheck-clojure)
(add-hook 'cider-mode-hook #'flycheck-mode)

(defadvice cljr-find-usages (before setup-grep activate)
  "Make q quit out of find-usages to previous window config."
  (window-configuration-to-register ?$))

(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

(provide 'setup-clojure-mode)
;;; setup-clojure-mode.el ends here
