;;; package --- setup-first

;;; Commentary:
;;; Emacs configuration file to be loaded first

;;; Code:

;; Seed the random-number generator
(random t)

;; Load .el if newer than corresponding .elc
(setq load-prefer-newer t)

;; Automatic byte compilation
;(require 'auto-compile)
;(auto-compile-on-save-mode 1)
;(auto-compile-on-load-mode 1)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)

;; ;; Start emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist '((fullscreen . maximized)))

(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))

;; Write backup nfiles to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save cursor position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(load custom-file)

(provide 'setup-first)
;;; setup-first.el ends here
