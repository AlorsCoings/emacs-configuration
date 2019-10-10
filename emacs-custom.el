;;; package --- Custom

;;; Commentary:
;;; This my Emacs configuration file creating by using the customization command

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("tabular" LaTeX-indent-tabular)
     ("tabular*" LaTeX-indent-tabular)
     ("align" LaTeX-indent-tabular)
     ("align*" LaTeX-indent-tabular)
     ("array" LaTeX-indent-tabular)
     ("eqnarray" LaTeX-indent-tabular)
     ("eqnarray*" LaTeX-indent-tabular)
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing")
     ("comment" current-indentation))))
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(flycheck-googlelint-filter
   "+build,+whitespace,+runtime,+readability,-whitespace/braces,-whitespace/indent")
 '(flycheck-googlelint-linelength "140")
 '(flycheck-googlelint-verbose "3")
 '(package-selected-packages
   (quote
    (realgud go-guru company-go company-tabnine nginx-mode sass-mode turtle-mode sparql-mode go-mode auto-yasnippet ttl-mode magit-gitflow css-eldoc js2-mode paredit docker docker-compose-mode docker-tramp yaml-mode beginend lua-mode yasnippet org helm flycheck emacsql deferred dash company auctex expand-region company-c-headers company-irony gnuplot ido-completing-read+ multiple-cursors irony ibuffer-vc math-symbol-lists highlight-indentation arduino-mode slime scala-mode csharp-mode company-emacs-eclim eclim elpy atomic-chrome org-gcal org-pdfview whitespace-cleanup-mode wgrep web-mode web-beautify visual-regexp undo-tree top-mode toggle-quotes tidy tern sqlup-mode sqlplus sql-indent smex smart-mode-line slime-company skewer-mode shell-command scss-mode rainbow-mode plsql php-mode page-break-lines oauth2 move-text magit legalese latex-extra karma json-mode impatient-mode image+ idomenu ido-vertical-mode ido-at-point icomplete+ hl-sexp highlight-symbol highlight-escape-sequences helm-dash google-translate gnuplot-mode gitignore-mode gitconfig-mode git-messenger fullframe flycheck-pos-tip flx-ido fill-column-indicator exec-path-from-shell emmet-mode emacsql-sqlite elnode elisp-slime-nav dockerfile-mode dired-sort dired-details diminish dash-functional crappy-jsp-mode company-restclient company-jedi company-auctex c-eldoc browse-kill-ring bbdb bash-completion auto-complete auto-compile auctex-lua apache-mode anzu angular-snippets)))
 '(python-indent-guess-indent-offset-verbose nil))
;;; .emacs-custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
