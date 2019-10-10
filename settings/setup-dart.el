;;; package --- setup-dart

;;; Commentary:
;;; Emacs configuration file for dart

;;; Code:

(require 'dart-mode)

(setq dart-enable-analysis-server t)

(setq dart-sdk-path 'nil)
(setq dart-sdk-path "/usr/lib/dart/")
(setq dart-formatter-command-override "/usr/lib/dart/bin/dartfmt")
(add-hook 'dart-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-checkers 'dart-analysis-server 'append)

;; Seeing more information at point
;; (C-u) C-c ?

;; Go to definition
;; C-c C-g

;; You can search for all references to the identifier under your cursor.
;; This will show you everywhere a method, getter, or setter is called
;; everywhere a class is used as a type, constructed, or has static methods called on it
;; everywhere a named argument is passed; and so on.
;; C-c C-f.

;; You can search for all member declarations with a given name.
;; This will list all declarations within classes that have the name, but
;; not any declarations at the top level.
;; C-c C-e

;; If you want to search for top-level declarations instead
;; C-c C-t

;; If you want to find all references to members with a given name. This
;; will show you everywhere a member with that name is called, even if it's in a
;; dynamic context and the analyzer can't figure out what it's referring to.
;; C-c C-r

;; If you press M-/, the analyzer will try to expand whatever text you've
;; already typed into a valid identifier. This uses the same logic that IDEs use
;; for autocomplete, but the UI works like Emacs' dabbrev-expand command. You
;; can press M-/ multiple times in a row to cycle through possible completions.

;; When you've selected an expansion that's a method call, you can press M-?
;; (M-/ plus shift) to insert the parameter list. The first parameter will be
;; selected, and anything you type will replace it. Once it's replaced, you can
;; press M-? again to select the second parameter, and so on.

;; If the analysis server isn't enabled for the current buffer, this will fall
;; back to whatever command is assigned to M-/ outside of Dart mode
;; (dabbrev-expand in vanilla Emacs). This will usually pick up any custom key
;; bindings, but if it doesn't you can manually choose a fallback by setting the
;; `

(define-key dart-mode-map (kbd "C-d") 'nil)
(define-key dart-mode-map (kbd "C-c C-r") 'dart-format)
(define-key dart-mode-map (kbd "C-j") 'emmet-expand-yas)
(define-key dart-mode-map (kbd "M-e") 'nil)
(define-key dart-mode-map (kbd "C-<tab>") 'dart-expand)
(define-key dart-mode-map (kbd "C-S-<iso-lefttab>") 'dart-expand-parameters)

(provide 'setup-dart)
;;; setup-dart.el ends here
