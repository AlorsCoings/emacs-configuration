;;; package --- appearance

;;; Commentary:
;;; Emacs appearance configuration

;;; Code:

(setq font-lock-maximum-decoration t
      truncate-partial-width-windows nil)

(set-face-attribute 'default nil :inherit nil :stipple nil :background "black"
                    :foreground "gray90" :inverse-video nil :box nil
                    :strike-through nil :overline nil :underline nil
                    :slant 'normal :weight 'normal :height 128 :width 'normal
                    :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'region nil :background "gray25")
(set-face-attribute 'font-lock-warning-face nil :foreground "#ff6666")
(set-face-attribute 'highlight nil :background "#111111")

;; Highlight current line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :background "gray10")

;; Highlight sexp
(require 'hl-sexp)
;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(eval-after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil :background "#333399")
(set-face-attribute 'show-paren-mismatch nil :background "red3" :foreground "white")

(setq global-visual-line-mode nil)

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
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))

(setq custom-safe-themes
      '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
        default))

(require 'smart-mode-line)
(setq sml/theme 'dark)
(sml/setup)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "gray30")
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)
(add-hook 'lua-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'tuareg-mode-hook 'fci-mode)
(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'ruby-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'js-mode-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)
(add-hook 'lisp-mode-hook 'fci-mode)
(add-hook 'sh-mode-hook 'fci-mode)
(add-hook 'java-mode-hook 'fci-mode)

(require 'vc-annotate)
(setq vc-annotate-background nil)
(setq vc-annotate-color-map
  (quote
   ((20 . "#dc322f")
    (40 . "#cb4b16")
    (60 . "#b58900")
    (80 . "#859900")
    (100 . "#2aa198")
    (120 . "#268bd2")
    (140 . "#d33682")
    (160 . "#6c71c4")
    (180 . "#dc322f")
    (200 . "#cb4b16")
    (220 . "#b58900")
    (240 . "#859900")
    (260 . "#2aa198")
    (280 . "#268bd2")
    (300 . "#d33682")
    (320 . "#6c71c4")
    (340 . "#dc322f")
    (360 . "#cb4b16"))))
(setq vc-annotate-very-old-color nil)

(provide 'appearance)
;;; appearance ends here
