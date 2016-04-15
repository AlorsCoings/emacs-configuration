;;; package --- Custom

;;; Commentary:
;;; This my emacs configuration file creating by using the customization command

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "gray88" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "adobe" :family "Source Code Pro"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(hl-line ((nil (:background "#222222"))))
 '(iedit-occurrence ((t (:background "gray30"))))
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
 '(region ((nil (:background "#464740"))))
 '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red"))))
 '(web-mode-comment-face ((t (:foreground "#8fbc8f"))))
 '(web-mode-current-element-highlight-face ((t (:background "gray15"))))
 '(web-mode-html-attr-custom-face ((t (:inherit font-lock-keyword-face))))
 '(web-mode-html-attr-name-face ((t (:inherit web-mode-variable-name-face))))
 '(yas-field-highlight-face ((nil (:background "#333399")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Text viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-expand-list
   (quote
    (("%p" TeX-printer-query)
     ("%q"
      (lambda nil
        (TeX-printer-query t)))
     ("%V"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-view-command-raw)))
     ("%vv"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-output-style-check TeX-output-view-style)))
     ("%v"
      (lambda nil
        (TeX-source-correlate-start-server-maybe)
        (TeX-style-check TeX-view-style)))
     ("%r"
      (lambda nil
        (TeX-style-check TeX-print-style)))
     ("%l"
      (lambda nil
        (TeX-style-check LaTeX-command-style)))
     ("%(PDF)"
      (lambda nil
        (if
            (and
             (eq TeX-engine
                 (quote default))
             (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
            "pdf" "")))
     ("%(PDFout)"
      (lambda nil
        (cond
         ((and
           (eq TeX-engine
               (quote xetex))
           (not TeX-PDF-mode))
          " -no-pdf")
         ((and
           (eq TeX-engine
               (quote luatex))
           (not TeX-PDF-mode))
          " --output-format=dvi")
         ((and
           (eq TeX-engine
               (quote default))
           (not TeX-PDF-mode)
           TeX-DVI-via-PDFTeX)
          " \"\\pdfoutput=0 \"")
         (t ""))))
     ("%(mode)"
      (lambda nil
        (if TeX-interactive-mode "" " -interaction=nonstopmode")))
     ("%(file-line-error)"
      (lambda nil
        (if TeX-file-line-error " -file-line-error" "")))
     ("%(o?)"
      (lambda nil
        (if
            (eq TeX-engine
                (quote omega))
            "o" "")))
     ("%(tex)"
      (lambda nil
        (eval
         (nth 2
              (assq TeX-engine
                    (TeX-engine-alist))))))
     ("%(latex)"
      (lambda nil
        (eval
         (nth 3
              (assq TeX-engine
                    (TeX-engine-alist))))))
     ("%(execopts)" ConTeXt-expand-options)
     ("%(extraopts)"
      (lambda nil TeX-command-extra-options))
     ("%S" TeX-source-correlate-expand-options)
     ("%dS" TeX-source-specials-view-expand-options)
     ("%cS" TeX-source-specials-view-expand-client)
     ("%(outpage)"
      (lambda nil
        (and TeX-source-correlate-mode
             (null TeX-source-correlate-output-page-function)
             (eq
              (TeX-source-correlate-method-active)
              (quote synctex))
             (setq TeX-source-correlate-output-page-function
                   (quote TeX-synctex-output-page)))
        (or
         (if TeX-source-correlate-output-page-function
             (funcall TeX-source-correlate-output-page-function))
         "1")))
     ("%s" file nil t)
     ("%t" file t t)
     ("%`"
      (lambda nil
        (setq TeX-command-pos t TeX-command-text "")))
     (" \"\\"
      (lambda nil
        (if
            (eq TeX-command-pos t)
            (setq TeX-command-pos pos pos
                  (+ 3 pos))
          (setq pos
                (1+ pos)))))
     ("\""
      (lambda nil
        (if
            (numberp TeX-command-pos)
            (setq TeX-command-text
                  (concat TeX-command-text
                          (substring command TeX-command-pos
                                     (1+ pos)))
                  command
                  (concat
                   (substring command 0 TeX-command-pos)
                   (substring command
                              (1+ pos)))
                  pos TeX-command-pos TeX-command-pos t)
          (setq pos
                (1+ pos)))))
     ("%'"
      (lambda nil
        (prog1
            (if
                (stringp TeX-command-text)
                (progn
                  (setq pos
                        (+ pos
                           (length TeX-command-text)
                           9)
                        TeX-command-pos
                        (and
                         (string-match " "
                                       (funcall file t t))
                         "\""))
                  (concat TeX-command-text " \"\\input\""))
              (setq TeX-command-pos nil)
              "")
          (setq TeX-command-text nil))))
     ("%n" TeX-current-line)
     ("%d" file "dvi" t)
     ("%f" file "ps" t)
     ("%o"
      (lambda nil
        (funcall file
                 (TeX-output-extension)
                 t)))
     ("%b" TeX-current-file-name-master-relative)
     ("%a"
      (lambda nil
        (prin1-to-string
         (expand-file-name
          (buffer-file-name)))))
     ("%m" preview-create-subdirectory))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server nil)
 '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-html "xdg-open")
     (output-pdf "Evince"))))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(eww-download-directory "~/EWW_Downloads/")
 '(fci-rule-color "#073642")
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(global-visual-line-mode nil)
 '(gnus-signature-separator nil)
 '(ido-ubiquitous-command-overrides
   (quote
    ((enable exact "mml-attach-file")
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
     (enable prefix "etags-select-"))))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(ispell-highlight-face (quote custom-themed))
 '(json-reformat:indent-width 2)
 '(json-reformat:pretty-string\? t)
 '(max-lisp-eval-depth 6000)
 '(max-specpdl-size 13000)
 '(org-agenda-files
   (quote
    ("/home/toad/Dropbox/org/courses.org" "/home/toad/Dropbox/org/emacs.org" "/home/toad/Dropbox/org/home.org" "/home/toad/Dropbox/org/inbox.org" "/home/toad/Dropbox/org/lesDoudous.org" "/home/toad/Dropbox/org/myAgenda.org" "/home/toad/Dropbox/org/work.org")))
 '(org-log-done (quote note))
 '(semantic-c-dependency-system-include-path
   (quote
    ("/usr/include" "/usr/include/c++/4.8" "/usr/include/x86_64-linux-gnu/c++/4.8" "/usr/include/c++/4.8/backward" "/usr/lib/gcc/x86_64-linux-gnu/4.8/include" "/usr/local/include" "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed" "/usr/include/x86_64-linux-gnu")))
 '(send-mail-function (quote smtpmail-send-it))
 '(sml/mode-width
   (if
       (eq powerline-default-separator
           (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
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
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-quoting t)
 '(web-mode-enable-css-colorization t)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-enable-inlays t)
 '(web-mode-enable-part-face t)
 '(web-mode-enable-sql-detection t))
;;; .emacs-custom.el ends here
