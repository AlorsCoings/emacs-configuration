;;; package --- key-bindings

;;; Commentary:
;;; Key bindings for emacs

;;; Code:

;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; insert quote
(global-set-key (kbd "C-S-q") 'quoted-insert)

;; No more scrolling surprises
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))

;; Avoid typing full path when starting gdb
(global-set-key (kbd "C-c g")
                '(lambda ()
                   (interactive)
                   (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))

;; Text scale (zoom)
(global-set-key (kbd "C-c C-+") 'text-scale-increase)
(global-set-key (kbd "C-c C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-*") 'text-scale-set)

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; Switch global-visual-line-mode
(global-set-key (kbd "C-M-v") 'global-visual-line-mode)

;; iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; Multiple cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
;; Extra multiple cursors stuff
(global-set-key (kbd "C-c i") 'mc/insert-numbers)
(global-set-key (kbd "C-c C-i") 'mc--insert-number-and-increase)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Expand and contract region
(global-set-key (kbd "C-v") 'er/expand-region)
(global-set-key (kbd "C-S-v") 'er/contract-region)

;; Manage windows
(global-set-key (kbd "C-M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-<down>") 'shrink-window)
(global-set-key (kbd "C-M-S-<up>") 'enlarge-window)

;; Window switching
(global-set-key (kbd "C-x o") 'switch-window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-window-vertically-with-other-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-with-other-buffer)


;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Quickly jump in document with ace-jump-mode
(global-set-key (kbd "C-q") 'ace-jump-mode)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; M-i for back-to-indentation
(global-set-key (kbd "C-S-a") 'back-to-indentation)

(global-set-key (kbd "M-D") 'kill-region-or-backward-word)

;; Visual regexp
(global-set-key (kbd "M-&") 'vr/replace)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") (λ (save-region-or-current-line 1)))
(global-set-key (kbd "M-W") 'save-region-or-current-line)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Create new frame
(global-set-key (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'idomenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c C-y") 'bury-buffer)
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
;; (global-set-key (kbd "C-x b") 'quick-switch-buffer)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path or name to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)
(global-set-key (kbd "C-x M-W") 'copy-current-file-name)

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Help should search more than just commands
(global-set-key (kbd "C-h j") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(defvar webjump-sites
  '(("Google" .
     [simple-query "www.google.com" "www.google.com/search?q=" ""])
    ("Google Groups" .
     [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
    ("DuckDuckGo" .
     [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
    ("Wikipedia" .
     [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])))

(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-S-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-S-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))
(global-set-key (kbd "M-F") (λ (ignore-errors (subword-forward 5))))
(global-set-key (kbd "M-B") (λ (ignore-errors (subword-backward 5))))

;; Convenience on ThinkPad Keyboard: Use back/forward as pg up/down
(global-set-key (kbd "<XF86Back>") 'scroll-down)
(global-set-key (kbd "<XF86Forward>") 'scroll-up)
(global-set-key (kbd "<XF86WakeUp>") 'beginning-of-buffer)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'anzu-replace-at-cursor-thing)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

;; zap to char
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; Activate occur inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<C-S-right>") 'windmove-right)
(global-set-key (kbd "<C-S-left>") 'windmove-left)
(global-set-key (kbd "<C-S-up>") 'windmove-up)
(global-set-key (kbd "<C-S-down>") 'windmove-down)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Ispell check word
(global-set-key (kbd "M-#") 'ispell-word)

;; Google translate
(global-set-key (kbd "C-à") 'google-translate-smooth-translate)
(global-set-key (kbd "C-À") 'google-translate-at-point)

;; Clever newlines
;; (global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(global-set-key (kbd "M-N") 'move-text-down)
(global-set-key (kbd "M-P") 'move-text-up)

;; Fold the active region
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)
(global-set-key (kbd "C--") 'subtract-number-at-point)

(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "C- ") 'undo-tree-redo)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

(global-set-key (kbd "C-o") 'switch-window)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-set-key (kbd "C-c C-j") (lambda () (interactive) (require 'setup-java)))
(global-set-key (kbd "C-c j") 'eclim-manage-projects)

(global-set-key (kbd "C-S-f") 'forward-sentence)

(global-set-key (kbd "C-c t") 'typing-game)
(global-set-key (kbd "C-c C-t") 'typing-game)

(provide 'key-bindings)
;;; key-bindings.el ends here
