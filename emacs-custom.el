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
 '(package-selected-packages
   (quote
    (docker docker-compose-mode docker-tramp yaml-mode beginend lua-mode yasnippet org js2-mode helm flycheck emacsql deferred dash company auctex expand-region company-c-headers company-irony gnuplot ido-completing-read+ multiple-cursors irony ibuffer-vc math-symbol-lists highlight-indentation flim arduino-mode apel slime scala-mode csharp-mode company-emacs-eclim eclim elpy ess-R-object-popup atomic-chrome org-gcal org-pdfview whitespace-cleanup-mode wgrep web-mode web-beautify visual-regexp undo-tree top-mode toggle-quotes tidy tern sqlup-mode sqlplus sql-indent smex smartparens smart-mode-line slime-company skewer-mode shell-command semi scss-mode rainbow-mode plsql php-mode pdf-tools page-break-lines oauth2 move-text magit legalese latex-extra karma json-mode impatient-mode image+ idomenu ido-vertical-mode ido-at-point icomplete+ hl-sexp highlight-symbol highlight-escape-sequences helm-dash google-translate gnuplot-mode gitignore-mode gitconfig-mode git-messenger fullframe flycheck-pos-tip flycheck-google-cpplint flx-ido fill-column-indicator exec-path-from-shell emmet-mode emacsql-sqlite elnode elisp-slime-nav dockerfile-mode dired-sort dired-details diminish dash-functional css-mode css-eldoc crontab-mode crappy-jsp-mode company-restclient company-jedi company-auctex c-eldoc buster-snippets buster-mode browse-kill-ring bison-mode bbdb bash-completion auto-complete auto-compile auctex-lua apache-mode anzu angular-snippets ace-jump-mode)))
 '(python-indent-guess-indent-offset-verbose nil))
;;; .emacs-custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
