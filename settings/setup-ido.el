;;; package --- setup-ido

;;; Commentary:
;;; Emacs ido configuration

;;; Code:

;; Interactively Do Things

(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(setq ido-cr+-max-items 50000)

;; Try out flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)
;; Disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'dash)

(defun my/ido-go-straight-home ()
  "Change directory to ~ and / if already to ~."
  (interactive)
  (cond
   ((looking-back "~/" nil nil) (insert "//"))
   ((looking-back "/" nil nil) (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(defun my/setup-ido ()
  "Define keys for `ido-common-completion-map'."
  (define-key ido-common-completion-map (kbd "~") 'my/ido-go-straight-home)
  (define-key ido-common-completion-map (kbd "C-~") 'my/ido-go-straight-home)
  (define-key ido-common-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-common-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)
  (define-key ido-common-completion-map (kbd "C-e") 'ido-magic-delete-char)
  (define-key ido-common-completion-map (kbd "C-'") 'ido-delete-backward-updir)
  (define-key ido-common-completion-map (kbd "C-t") 'ido-magic-backward-char)
  (define-key ido-common-completion-map (kbd "C-r") 'ido-magic-forward-char)
  (define-key ido-common-completion-map (kbd "C-d") 'ido-prev-match))

(add-hook 'ido-setup-hook 'my/setup-ido)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Ido at point (C-x TAB)
(require 'ido-at-point)
(ido-at-point-mode)

(provide 'setup-ido)
;;; setup-ido.el ends here
