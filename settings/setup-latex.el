;;; package --- setup-latex

;;; Commentary:
;;; Emacs configuration file for latex

;;; Code:

;; 'C-c C-p C-d' (for 'Preview/Document')
;; stop the background process by pressing 'C-c C-k'
;; 'C-c C-p C-p' (for 'Preview/at point')
;; 'C-c C-p C-c C-d' 'preview-clearout-document'
;; 'C-c C-p C-c C-p' 'preview-clearout-at-point'
;; 'C-c C-t C-p' This command toggles between DVI and PDF output
;; 'C-c C-w' ('TeX-toggle-debug-boxes') you can toggle whether AUCTeX should notify you of overfull and underfull boxes in addition to regular errors.
;; 'C-c C-g' jump to PDF location from source

(require 'tex)
(TeX-global-PDF-mode t)
(setq TeX-PDF-mode t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master t)

;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; to use to compile with xetex
;; (setq-default TeX-engine 'xetex)
;; (setq-default TeX-engine 'luatex)

(require 'tex-mode)

(define-key latex-mode-map (kbd "C-S-l") 'LaTeX-edit-Lua-code-start)
(define-key latex-mode-map (kbd "C-c C-a") 'latex/compile-commands-until-done)

;; C-c C-q `latex/clean-fill-indent-environment'
;; Completely cleans up the entire current environment
;; 1. Removing extraneous spaces and blank lines.
;; 2. Filling text (and only text, not equations).
;; 3. Indenting everything.

;; customize-font chapter section subsection
(setq LaTeX-command "latex -synctex=1")
(setq TeX-command-list
      '(("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
         (plain-tex-mode texinfo-mode ams-tex-mode)
         :help "Run plain TeX")
        ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
         (latex-mode doctex-mode)
         :help "Run LaTeX")
        ("Glossary" "makeglossaries %s" TeX-run-command nil
         (latex-mode) :help "Create glossaries")
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
        ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
(setq TeX-expand-list
      '(("%p" TeX-printer-query)
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
        ("%m" preview-create-subdirectory)))
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server nil)
(setq TeX-view-program-list '("Okular" "okular --unique %o#src:%n%b"))
(setq TeX-view-program-selection
      '(((output-dvi style-pstricks)
         "dvips and gv")
        (output-dvi "xdvi")
        (output-html "xdg-open")
        (output-pdf "Evince")))

(require 'latex)
(define-key LaTeX-mode-map (kbd "C-c C-n") 'cleanup-buffer)

(require 'outline)
(setq outline-minor-mode-prefix "C-é")

(require 'ox-latex)
(setq org-latex-listings t)
(setq org-latex-listings-options
      '(("literate" "{á}{{\\'a}}1 {é}{{\\'e}}1 {í}{{\\'i}}1 {ó}{{\\'o}}1 {ú}{{\\'u}}1
  {Á}{{\\'A}}1 {É}{{\\'E}}1 {Í}{{\\'I}}1 {Ó}{{\\'O}}1 {Ú}{{\\'U}}1
  {à}{{\\`a}}1 {è}{{\\`e}}1 {ì}{{\\`i}}1 {ò}{{\\`o}}1 {ù}{{\\`u}}1
  {À}{{\\`A}}1 {È}{{\\'E}}1 {Ì}{{\\`I}}1 {Ò}{{\\`O}}1 {Ù}{{\\`U}}1
  {ä}{{\\\"a}}1 {ë}{{\\\"e}}1 {ï}{{\\\"i}}1 {ö}{{\\\"o}}1 {ü}{{\\\"u}}1
  {Ä}{{\\\"A}}1 {Ë}{{\\\"E}}1 {Ï}{{\\\"I}}1 {Ö}{{\\\"O}}1 {Ü}{{\\\"U}}1
  {â}{{\\^a}}1 {ê}{{\\^e}}1 {î}{{\\^i}}1 {ô}{{\\^o}}1 {û}{{\\^u}}1
  {Â}{{\\^A}}1 {Ê}{{\\^E}}1 {Î}{{\\^I}}1 {Ô}{{\\^O}}1 {Û}{{\\^U}}1
  {œ}{{\\oe}}1 {Œ}{{\\OE}}1 {æ}{{\\ae}}1 {Æ}{{\\AE}}1 {ß}{{\\ss}}1
  {ç}{{\\c c}}1 {Ç}{{\\c C}}1 {ø}{{\\o}}1 {å}{{\\r a}}1 {Å}{{\\r A}}1
  {€}{{\\EUR}}1 {£}{{\\pounds}}1")
        ("inputencoding" "utf8")
        ("basicstyle" "\\slshape\\scriptsize\\ttfamily")
        ("extendedchars" "true")
        ("tabsize" "2")
        ("breaklines" "true")
        ("keepspaces" "true")
        ("showspaces" "false")
        ("showstringspaces" "false")
        ("showtabs" "false")
        ("keywordstyle" "\\color{blue}")
        ("stringstyle" "\\color{red}")
        ("commentstyle" "\\color{magenta}")
        ("morecomment" "[l][\\color{magenta}]{\\#}")))
(add-to-list 'org-latex-listings-langs '(nxml "XML"))
(add-to-list 'org-latex-listings-langs '(sh "bash"))
(add-to-list 'org-latex-listings-langs '(java "Java"))

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t
         ("pdflatex"))
        ("T1" "fontenc" t
         ("pdflatex"))
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("hidelinks" "hyperref" nil)))
(add-to-list 'org-latex-packages-alist '("T1" "fontenc" nil))
(add-to-list 'org-latex-packages-alist '("utf8" "luainputenc" nil))
(add-to-list 'org-latex-packages-alist '("" "lmodern" nil))
(add-to-list 'org-latex-packages-alist '("frenchb" "babel" nil))
(add-to-list 'org-latex-packages-alist '("math" "kurier" nil))
(add-to-list 'org-latex-packages-alist '("" "color" nil))
(add-to-list 'org-latex-packages-alist '("" "multicol" nil))
(add-to-list 'org-latex-packages-alist '("" "titlesec" nil))
(add-to-list 'org-latex-packages-alist '("" "placeins" nil))
(add-to-list 'org-latex-packages-alist '("" "sistyle" nil))
(add-to-list 'org-latex-packages-alist '("" "listingsutf8" nil))
(add-to-list 'org-latex-packages-alist '("" "colortbl" nil))
(add-to-list 'org-latex-packages-alist '("" "sourcecodepro" nil))
(add-to-list 'org-latex-packages-alist '("" "fancyhdr" nil))
(add-to-list 'org-latex-packages-alist '("" "pict2e" nil))
(add-to-list 'org-latex-packages-alist '("" "array" nil))
(add-to-list 'org-latex-packages-alist '("" "multirow" nil))
(add-to-list 'org-latex-packages-alist '("" "makecell" nil))
(add-to-list 'org-latex-packages-alist '("" "supertabular" nil))
(add-to-list 'org-latex-packages-alist '("" "tabularx" nil))
(add-to-list 'org-latex-packages-alist '("" "calc" nil))
(add-to-list 'org-latex-packages-alist '("" "caption" nil))
(add-to-list 'org-latex-packages-alist '("" "mathrsfs" nil))
(add-to-list 'org-latex-packages-alist '("" "sectsty" nil))
(add-to-list 'org-latex-packages-alist '("" "mathabx" nil))
(add-to-list 'org-latex-packages-alist '("" "pdflscape" nil))
(add-to-list 'org-latex-packages-alist '("" "url" nil))
(add-to-list 'org-latex-packages-alist '("" "diagbox" nil))
(add-to-list 'org-latex-packages-alist '("" "tikz" nil))
(add-to-list 'org-latex-packages-alist '("" "standalone" nil))
(add-to-list 'org-latex-packages-alist '("" "comment" nil))
(add-to-list 'org-latex-packages-alist '("" "algpseudocode" nil))
(add-to-list 'org-latex-packages-alist '("" "fourier-orns" nil))
(add-to-list 'org-latex-packages-alist '("" "enumitem" nil))
(add-to-list 'org-latex-packages-alist '("" "underscore" nil))
(add-to-list 'org-latex-packages-alist '("" "cite" nil))
(add-to-list 'org-latex-packages-alist '("" "afterpage" nil))
(add-to-list 'org-latex-packages-alist '("" "boxedminipage" nil))
(add-to-list 'org-latex-packages-alist '("" "stfloats" nil))
(add-to-list 'org-latex-packages-alist '("" "todonotes" nil))
(add-to-list 'org-latex-packages-alist '("" "textpos" nil))
(setq org-latex-packages-alist (reverse org-latex-packages-alist))

;; Original file ~/latex/preamble.tex
(add-to-list 'org-latex-classes
             '("report-ufr"
               "\\documentclass[a4paper, 12pt, titlepage]{report}
[DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]

\\usetikzlibrary{cd, shapes, calc, fadings, matrix, arrows, automata,
positioning, decorations.markings, decorations.pathmorphing}

\\makeatletter

\\titleformat{\\chapter}[display]
{\\normalfont\\huge\\bfseries}{}{20pt}{\\Huge}
\\titlespacing*{\\chapter}{0pt}{-50pt}{40pt}

\\makeatother

\\newcommand{\\HRule}{\\rule{\\linewidth}{0.5mm}}

% \\def\\title{title}
% \\def\\author{Nicolas \\textsc{Gros}}
% \\def\\date{Année 2016/2017}
\\def\\supervisor{prenom \\textsc{nom}}
\\def\\pretitle{pretitle}
"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(provide 'setup-latex)
;;; setup-latex.el ends here
