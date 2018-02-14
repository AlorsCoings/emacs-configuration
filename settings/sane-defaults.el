;;; package --- sane-defaults

;;; Commentary:
;;; Emacs sane defaults configuration

;;; Code:

;; Enable accent
(load-library "iso-transl")

(setq max-specpdl-size 13000)
(setq max-lisp-eval-depth 5000)

(setq-default
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 indent-tabs-mode nil
 save-interprogram-paste-before-kill t
 show-trailing-whitespace t)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Redefine M-< and M-> for some modes
(beginend-global-mode)

(require 'anzu)
(global-anzu-mode t)
(diminish 'anzu-mode)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
(custom-set-variables
 '(anzu-mode-lighter "") ;; Same as diminish
 '(anzu-deactivate-region t)
 '(anzu-replace-to-string-separator " => "))

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook
                markdown-mode-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))

(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)

(global-prettify-symbols-mode 1)

;; Set default indentation level
(setq-default tab-width 4)

;; Don't scroll every buffer
(scroll-all-mode 0)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

(require 'autorevert)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
;; Not good when having double hard drives
;; (setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(set-language-environment "UTF-8") ; pretty
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)
;; (smooth-scrolling-mode 1)
;; (setq smooth-scroll-margin 10)
(setq scroll-margin 10
      scroll-step 1
      scroll-conservatively 10
      scroll-preserve-screen-position 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Represent undo-history as an actual tree (visualize with C-x u)
(require 'undo-tree)
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)

;; A saner ediff
(require 'ediff)
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor actually move."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  "Offer to create parent directories if they do not exist."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                                 parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Disable most mouse interaction
(when window-system
  (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
               [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
               [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
               [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
               [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
    (global-unset-key k)))

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defadvice kill-line (after indent-after-kill-line activate)
  "Make `kill-line' indent after killing."
  (indent-according-to-mode))

(require 'hexl)
(add-hook 'hexl-mode-hook 'font-lock-fontify-buffer)

(setq kill-ring-max 1000)

(require 'ansi-color)
(setq ansi-color-faces-vector
       [default bold shadow italic underline bold bold-italic bold])
(setq anzu-deactivate-region t)
(setq anzu-mode-lighter "")
(setq anzu-replace-to-string-separator " => ")
(setq anzu-search-threshold 1000)

(require 'eww)
(setq eww-download-directory "~/EWW_Downloads/")

(defadvice undo-tree-undo (around keep-region activate)
  "Keep region when undoing in region."
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Make C-x C-e run 'eval-region if the region is active
(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp.
With argument PREFIX, print output into current buffer."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))
(global-set-key [remap eval-expression] 'pp-eval-expression)
(define-key emacs-lisp-mode-map (kbd "C-x C-e")
  'sanityinc/eval-last-sexp-or-region)

;; Add Urban Dictionary to webjump (C-x g)
(require 'webjump)

;; Webjump let's you quickly search google, wikipedia
(defvar webjump-sites
  '(("Google" .
     [simple-query "www.google.com" "www.google.com/search?q=" ""])
    ("Google Groups" .
     [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
    ("DuckDuckGo" .
     [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
    ("Wikipedia" .
     [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
    ("Urban Dictionary" .
     [simple-query "www.urbandictionary.com"
                   "http://www.urbandictionary.com/define.php?term=" ""])))

;; Fix whitespace on save, but only if the file was clean
(global-whitespace-cleanup-mode)

;; Do not remove double space at end of line
(require 'markdown-mode)
(add-hook 'markdown-mode-hook (lambda () (whitespace-cleanup-mode -1)))
(define-key markdown-mode-map (kbd "M-p") 'subword-downcase)

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(defadvice fill-paragraph (after indent-after activate)
  "Indent after calling `fill-paragraph'."
  (indent-buffer))

(defun describe-major-mode ()
  "Show inline doc for current `major-mode'."
  ;; code by Kevin Rodgers. 2009-02-25
  (interactive)
  (describe-function major-mode))

(defun my-randomize-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
                (delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
           (mapcar 'cdr
                   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
                         (lambda (a b) (< (car a) (car b))))))))

(require 'google-translate-default-ui)
;; Make Google translate from auto-detect to french by default
(setq google-translate-default-source-language "auto")
(setq google-translate-default-target-language "fr")

;; No warning for goal-column
(put 'set-goal-column 'disabled nil)

;; Avoid issue with non-ascii characters with dired over ssh with tramp
(require 'tramp)
(setq tramp-remote-process-environment ())
(add-to-list 'tramp-remote-process-environment "LC_ALL=en_US.UTF-8" 'append)

(when (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
  (set-default 'tramp-default-method "plink"))

;; Do not ask which shell to get
(setenv "ESHELL" "bash")

;; Change key bindings during isearch
(define-key isearch-mode-map (kbd "C-d") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-'") 'isearch-delete-char)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(icomplete-mode 99)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Visual regexp
(require 'visual-regexp)

;; View Large File
(require 'vlf)
(require 'vlf-setup)

(provide 'sane-defaults)
;;; sane-defaults.el ends here
