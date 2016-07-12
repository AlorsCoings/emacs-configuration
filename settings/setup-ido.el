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

(require 'ido-ubiquitous)
(setq
 ido-cr+-max-items 50000
 ido-ubiquitous-command-overrides
 '((enable exact "mml-attach-file")
   (disable exact "execute-extended-command")
   (enable prefix "wl-")
   (enable-old prefix "Info-")
   (enable exact "webjump")
   (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'")
   (disable prefix "org-")
   (disable prefix "tmm-")
   (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit\\)-theme\\'")
   (enable-old prefix "bbdb-")
   (enable-old exact "where-is")
   (enable prefix "xref-")
   (disable exact "todo-add-category")
   (enable exact "find-tag")
   (enable prefix "etags-select-")))
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  "Advice CMD of PACKAGE to use ido-ubiquitous."
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))
(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

;; Try out flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)
;; Disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; C-n/p is more intuitive in vertical layout
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'dash)

(require 'etags)
;; Actually unused
(defun my-ido-find-tag ()
  "Find a tag using ido."
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  "Find file in tag files with ido."
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(defun my/ido-go-straight-home ()
  "Change directory to ~ and / if already to ~."
  (interactive)
  (cond
   ((looking-back "~/") (insert "//"))
   ((looking-back "/") (insert "~/"))
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

;; Ido at point (C-,)
(require 'ido-at-point)
(ido-at-point-mode)

(require 'gnus-util)
(setq gnus-completing-read-function 'gnus-ido-completing-read)

(provide 'setup-ido)
;;; setup-ido.el ends here
