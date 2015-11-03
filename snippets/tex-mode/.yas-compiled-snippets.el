;;; Compiled snippets and support files for `tex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'tex-mode
                     '(("tikz" "\\documentclass{standalone}\n\n\\usepackage[utf8]{inputenc}\n\\usepackage[francais]{babel}\n\\usepackage[T1]{fontenc}\n\\usepackage[usenames, dvipsnames]{xcolor}\n\\usepackage{tikz}\n\\usetikzlibrary{shapes}\n\n\\begin{document}\n% \\usetikzlibrary{calc}\n\n$0\n\\newcommand{\\romain}[1]{\\uppercase\\expandafter{\\romannumeral #1}}\n\n\\begin{tikzpicture}[scale=1]\n\\tikzstyle{debut}=[ellipse,draw,text=black]\n\\tikzstyle{end}=[rectangle,draw,fill=yellow!50]\n\\tikzstyle{test}=[diamond, aspect=2.5,thick,\ndraw=blue,fill=yellow!50,text=blue]\n\\tikzstyle{next}=[->,>=stealth,thick,rounded corners=4pt]\n\n\\node[debut] (debut) at (0, 0) {debut};\n\\node[test] (test) at (0, 2) {test};\n\\node[end] (end) at (0,4) {end};\n\n\\draw[next] (debut) -- (test);\n\\draw[next] (test) -| (end) node[midway,fill=white]{non};\n\n\n\\end{tikzpicture}\n\\end{document}" "tikz" nil nil nil nil nil nil)
                       ("rapport" "\\documentclass[a4paper, 12pt, titlepage]{report}\n\n\\input{/home/toad/latex/preamble.tex}\n\n% \\setlength{\\parindent}{0pt}\n\\definecolor{veryLightGray}{gray}{0.80}\n\\definecolor{lightGray}{gray}{0.60}\n\n\\lstset{\n  language=C++\n}\n\n\\def\\pretitle{$1}\n\\def\\title{$2}\n\\def\\author{Nicolas \\textsc{Gros}}\n\\def\\supervisor{$3 \\textsc{$4}}\n\\def\\date{Année 2015/2016}\n\n\\begin{document}\n\\input{/home/toad/latex/titlepageUFRMaster1.tex}\n\n\\newpage\n\\pagestyle{empty}\n\\strut\n\\newpage\n\n\\renewcommand{\\contentsname}{Sommaire}\n\\chapter*{Résumé}\n\n\\tableofcontents\n\\thispagestyle{empty}\n\n\\chapter{$0}\n\\setcounter{page}{1}\n\n\\appendix\n\\chapter{Annexes}\n\n\\end{document}" "rapport" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Mon Aug 24 17:16:27 2015