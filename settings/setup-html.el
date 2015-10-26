;;; package --- setup-web

;;; Commentary:
;;; This my emacs configuration file for web development

;;; Code:

;; to use impatient-mode
;; M-x httpd-start
;; then M-x impatient-mode on the buffer you want to listen
;; then go on http://localhost:8017/imp/
(require 'impatient-mode)

;; General
;; M-; comment / uncomment line(s)
;; C-c C-f toggle folding on a tag/block
;; C-c C-i indent entire buffer
;; C-c C-m mark and expand
;; C-c C-s insert snippet
;; C-c C-w toggle display of invalid whitespaces

;; DOM
;; C-c C-d a replace apostrophes
;; C-c C-d d show tag mismatch
;; C-c C-d e replace HTML entities
;; C-c C-d n normalize
;; C-c C-d q replace dumb quotes
;; C-c C-d t traverse dom tree
;; C-c C-d x xpath

;; Block
;; C-c C-b b block beginning
;; C-c C-b c block close
;; C-c C-b e block end
;; C-c C-b k block kill
;; C-c C-b n next block
;; C-c C-b p previous block
;; C-c C-b s block select

;; HTML element
;; C-c C-e / element close
;; C-c C-e a select element content
;; C-c C-e b element beginning
;; C-c C-e c element clone
;; C-c C-e d child element (down)
;; C-c C-e e element end
;; C-c C-e f toggle folding on children
;; C-c C-e i element insert
;; C-c C-e k element kill
;; C-c C-e m mute blanks between children
;; C-c C-e n next element
;; C-c C-e p previous element
;; C-c C-e r rename element
;; C-c C-e s select element
;; C-c C-e t transpose element
;; C-c C-e u parent element (up)
;; C-c C-e v element vanish
;; C-c C-e w wrap element

;; HTML tag
;; C-c C-t a sort attributes
;; C-c C-t b tag beginning
;; C-c C-t e tag end
;; C-c C-t m fetch matching tag
;; C-c C-t n next tag
;; C-c C-t p previous tag
;; C-c C-t s select tag

;; HTML attribute
;; C-c C-a b attribute beginning
;; C-c C-a e attribute end
;; C-c C-a i attribute insert
;; C-c C-a k attribute kill
;; C-c C-a n attribute next
;; C-c C-a p attribute previous
;; C-c C-a s attribute select
;; C-c C-a t attribute transpose

;; navigation C-c C-n between opening / closing HTML tags
;; code folding C-c C-f for HTML elements and control blocks
;; snippet insertion C-c C-s
;; clever selection and expansion C-c C-m
;; suspicious whitespaces detection C-c C-w
;; checkout http://web-mode.org/
(require 'web-mode)
(require 'emmet-mode)

(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(require 'yasnippet)
(defun --setup-simplezen ()
  (require 'simplezen)
  (set (make-local-variable 'yas-fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))

(add-hook 'web-mode-hook '--setup-simplezen)

(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-auto-indentation t)
(after-load 'sgml-mode
  '(progn
     ;; don't include equal sign in symbols
     (modify-syntax-entry ?= "." html-mode-syntax-table)

     (define-key html-mode-map [remap forward-paragraph] 'skip-to-next-blank-line)
     (define-key html-mode-map [remap backward-paragraph] 'skip-to-previous-blank-line)
     (define-key html-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)
     (define-key html-mode-map (kbd "/") nil) ;; no buggy matching of slashes

     (define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point)

     (require 'tagedit)

     ;; paredit lookalikes
     (define-key html-mode-map (kbd "C-M-S-f") 'tagedit-forward-slurp-tag)
     (define-key html-mode-map (kbd "C-M-S-b") 'tagedit-forward-barf-tag)
     (define-key html-mode-map (kbd "C-M-S-r") 'tagedit-raise-tag)
     (define-key html-mode-map (kbd "C-M-S-d") 'tagedit-splice-tag)
     (define-key html-mode-map (kbd "C-M-S-s") 'tagedit-split-tag)
     (define-key html-mode-map (kbd "C-M-S-j") 'tagedit-join-tags)
     (define-key html-mode-map (kbd "C-M-S-c") 'tagedit-convolute-tags)

     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))

     ;; no paredit equivalents
     (define-key html-mode-map (kbd "M-k") 'tagedit-kill-attribute)
     (define-key html-mode-map (kbd "s-<return>") 'tagedit-toggle-multiline-tag)))

;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

;; example
;; html[ng-app=nicolasGros lang=fr]>head>title+meta[charset=utf-8]+p*3>lorem3
;; m0+p10+c#f+fw:b+w100+h20+bg#f00
(dolist (hook (list
               'sgml-mode-hook
               'css-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq emmet-preview-default t)
				   (setq web-mode-markup-indent-offset 2)
				   (setq web-mode-css-indent-offset 2)
				   (emmet-mode))))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-current-element-highlight t)

(provide 'setup-web)
;;; setup-web.el ends here
