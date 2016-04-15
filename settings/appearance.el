;;; package --- appearance

;;; Commentary:
;;; Emacs appearance configuration

;;; Code:

(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Highlight sexp
(require 'hl-sexp)
;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(eval-after-load 'hl-sexp
            (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
              (when turn-on
                (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode 0))

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "flyspell" '(diminish 'flyspell-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(eval-after-load "subword" '(diminish 'subword-mode))


(require 'powerline)
(require 'smart-mode-line)
;; Setup smart-mode-line with powerline
(setq sml/theme 'powerline)
(sml/setup)
;; Customize smart-mode-line
(setq powerline-default-separator 'curve)
(setq powerline-default-separator-dir '(right . left)
      sml/name-width 30
      sml/mode-width 'full)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "gray10"
                    :box nil)

(provide 'appearance)
;;; appearance ends here
