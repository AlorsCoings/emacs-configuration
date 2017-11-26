;;; package --- key-bindings

;;; Commentary:
;;; Key bindings for Emacs

;;; Code:

(declare-function replace-region-by "editing-defuns" (fn))
(declare-function save-region-or-current-line "editing-defuns" (arg))
(declare-function subword-forward "subword" (&optional arg))
(declare-function subword-backward "subword" (&optional arg))

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
                '(lambda()
                   (interactive)
                   (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))

;; Text scale (zoom)
(global-set-key (kbd "C-c C-+") 'text-scale-increase)
(global-set-key (kbd "C-c C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-*") 'text-scale-set)

(global-set-key (kbd "RET") 'newline-and-indent)

;; Multiple cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-»") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-«") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-«") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-pop)
(global-set-key (kbd "C-c C-»") 'mc--insert-number-and-increase)
(global-set-key (kbd "C-c C-«") 'mc--insert-letter-and-increase)

;; Manage windows
(global-set-key (kbd "C-M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-<down>") 'shrink-window)
(global-set-key (kbd "C-M-S-<up>") 'enlarge-window)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)

;; Don't zoom like this
(global-unset-key (kbd "C-x C-+"))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-window-vertically-with-other-buffer)
(global-set-key (kbd "C-x 3") 'split-window-horizontally-with-other-buffer)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") (lambda() (interactive) (save-region-or-current-line 1)))
(global-set-key (kbd "M-W") 'save-region-or-current-line)

;; Make shell more convenient
(global-set-key (kbd "C-z") 'shell)

;; Create new frame
(global-set-key (kbd "C-x C-n") 'make-frame-command)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c C-y") 'bury-buffer)
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>") (lambda() (interactive) (revert-buffer t t)))

;; Copy file path or name to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)
(global-set-key (kbd "C-x M-W") 'copy-current-file-name)

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
(global-set-key (kbd "M-j") (lambda() (interactive) (join-line -1)))

;; Help should search more than just commands
(global-set-key (kbd "C-h j") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "C-c C-o") 'browse-url-of-file)
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "M-<tab>") 'completion-at-point)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'anzu-replace-at-cursor-thing)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

;; Zap to char
(global-set-key (kbd "M-Z") 'zap-up-to-char)
(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; Activate occur inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)

;; Ispell check word
(global-set-key (kbd "M-#") 'ispell-word)

;; Google translate
(global-set-key (kbd "C-à") 'google-translate-smooth-translate)
(global-set-key (kbd "C-À") 'google-translate-at-point)

;; Clever newlines
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)
(global-set-key (kbd "C--") 'subtract-number-at-point)

(global-set-key (kbd "C-ê") 'undo-tree-undo)
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "C- ") 'undo-tree-redo)
(global-set-key (kbd "C-Ê") 'undo-tree-redo)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Browse kill ring
(require 'browse-kill-ring)
(define-key browse-kill-ring-mode-map (kbd "d") 'previous-line)
(define-key browse-kill-ring-mode-map (kbd "s") 'next-line)

;; Find directory
(global-set-key (kbd "M-s f") 'find-name-dired)

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-S-o") (lambda() (interactive) (other-window -1)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-set-key (kbd "C-c C-j") (lambda () (interactive) (require 'setup-java)))
(global-set-key (kbd "C-c j") 'eclim-manage-projects)

(global-set-key (kbd "C-h é") 'describe-major-mode)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (lambda() (interactive) (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (lambda() (interactive) (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (lambda() (interactive) (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (lambda() (interactive) (replace-region-by 's-upper-camel-case)))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-M-k") 'kill-and-retry-line)
(global-set-key (kbd "C-c C-'") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-c '") 'kill-to-beginning-of-line)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'idomenu)
(global-set-key (kbd "C-c C-i") 'idomenu)
(global-set-key (kbd "C-M-<tab>") 'idomenu)

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Switch beetween buffer
(global-set-key (kbd "C-é") 'switch-to-next-buffer)
(global-set-key (kbd "C-É") 'switch-to-prev-buffer)

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

(global-set-key (kbd "C-x k") (lambda() (interactive) (kill-buffer (current-buffer))))

;; Goal-column
;; (global-set-key (kbd "M-n") 'set-goal-column)
;; (global-set-key (kbd "M-N") (lambda () (interactive) (set-goal-column -1)))

;; Helm-dash
(global-set-key (kbd "C-M-z") 'helm-dash-at-point)

;; Ergonomic key-bindings for bépo keyboard
;; a → v
(global-set-key (kbd "C-v") 'move-beginning-of-line) ;; was er/expand-region
(global-set-key (kbd "C-S-v") 'back-to-indentation) ;; was er/contract-region
(global-set-key (kbd "M-v") 'backward-sentence)
(global-set-key (kbd "M-V") 'backward-sentence)
(global-set-key (kbd "C-M-v") 'beginning-of-defun) ;; was global-visual-line-mode
(global-set-key (kbd "C-M-S-v") 'beginning-of-buffer) ;; was global-visual-line-mode

;; e → l
(global-set-key (kbd "C-l") 'move-end-of-line) ;; was recenter-top-bottom
(global-set-key (kbd "C-S-l") 'move-end-of-line) ;; was recenter-top-bottom
(global-set-key (kbd "M-l") 'forward-sentence) ;; was subword-downcase
(global-set-key (kbd "M-L") 'forward-sentence) ;; was subword-downcase
(global-set-key (kbd "C-M-l") 'end-of-defun) ;; was reposition-window
(global-set-key (kbd "C-M-S-l") 'end-of-buffer) ;; was reposition-window

;; p → d
(global-set-key (kbd "C-d") 'previous-line) ;; was paredit-forward-delete
(global-set-key (kbd "C-S-d") (lambda() (interactive) (ignore-errors (previous-line 5)))) ;; was paredit-forward-delete
(global-set-key (kbd "M-d") 'move-text-up) ;; was paredit-forward-kill-word
(global-set-key (kbd "M-D") 'move-text-up) ;; was kill-region-or-backward-word
(global-set-key (kbd "C-M-d") 'backward-paragraph)

;; n → s
(global-set-key (kbd "C-s") 'next-line) ;; was isearch-forward
(global-set-key (kbd "C-S-s") (lambda() (interactive) (ignore-errors (next-line 5)))) ;; was isearch-forward-use-region
(global-set-key (kbd "M-S") 'move-text-down)
(global-set-key (kbd "C-M-s") 'forward-paragraph)

;; b → t
(global-set-key (kbd "C-t") 'backward-char) ;; was transpose-chars
(global-set-key (kbd "C-S-t") 'backward-sentence) ;; was transpose-chars
(global-set-key (kbd "M-t") 'subword-backward) ;; was transpose-(l/p/s/w)
(global-set-key (kbd "M-T") (lambda() (interactive) (ignore-errors (subword-backward 5)))) ;; was transpose-(l/p/s/w)
(global-set-key (kbd "C-M-t") 'backward-sexp) ;; is launch terminal

;; f → r
(global-set-key (kbd "C-r") 'forward-char) ;; was isearch-backward
(global-set-key (kbd "C-S-r") 'forward-sentence) ;; was isearch-backward-use-region
(global-set-key (kbd "M-r") 'subword-forward) ;; was paredit-raise-sexp
(global-set-key (kbd "M-R") (lambda() (interactive) (ignore-errors (subword-forward 5)))) ;; was paredit-raise-sexp
(global-set-key (kbd "C-M-r") 'forward-sexp) ;; was isearch-backward-regexp

;; Expand and contract region
(global-set-key (kbd "M-h") 'er/expand-region)
(global-set-key (kbd "M-H") 'er/contract-region)

(global-set-key (kbd "C-M-h") 'global-visual-line-mode)

(global-set-key (kbd "C-p") 'recenter-top-bottom)

(global-set-key (kbd "M-p") 'subword-downcase)
78.95
(global-set-key (kbd "C-e") 'delete-forward-char)
(global-set-key (kbd "M-e") 'kill-word)
(global-set-key (kbd "C-M-e") 'kill-sexp)
(global-set-key (kbd "C-'") 'delete-backward-char)
(global-set-key (kbd "M-'") 'backward-kill-word)
(global-set-key (kbd "C-M-'") 'backward-kill-sexp)

(global-set-key (kbd "C-n") 'isearch-forward)
(global-set-key (kbd "C-M-n") 'isearch-forward-regexp)
(global-set-key (kbd "M-n") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-n") 'isearch-backward)
(global-set-key (kbd "C-M-S-n") 'isearch-backward-regexp)

(global-set-key (kbd "C-f") 'transpose-chars)
(global-set-key (kbd "C-M-f") 'transpose-sexps)
;; Transpose stuff with M-f
(global-unset-key (kbd "M-f"))
(global-set-key (kbd "M-f s") 'transpose-sexps)
(global-set-key (kbd "M-f l") 'transpose-lines)
(global-set-key (kbd "M-f w") 'transpose-words)
(global-set-key (kbd "M-f p") 'transpose-params)

(global-set-key (kbd "M-,") 'comment-dwim)
(provide 'key-bindings)
;;; key-bindings.el ends here
