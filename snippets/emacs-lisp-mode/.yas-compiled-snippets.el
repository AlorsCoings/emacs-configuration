;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("req" "(require '$0)" "req" nil nil nil "/home/toad/.emacs.d/snippets/emacs-lisp-mode/req" nil nil)
                       ("pro" "(provide '`(buffer-file-name-body)`)$0" "provide" nil nil nil "/home/toad/.emacs.d/snippets/emacs-lisp-mode/provide.yasnippet" nil nil)
                       ("ends" "(provide '`(buffer-file-name-body)`)\n;;; `(buffer-file-name-body)`.el ends here$0" "ends" nil nil nil "/home/toad/.emacs.d/snippets/emacs-lisp-mode/ends" nil nil)
                       ("begin" ";;; package --- `(buffer-file-name-body)`\n\n;;; Commentary:\n;;; Emacs configuration file for $1\n\n;;; Code:\n$0" "begin" nil nil nil "/home/toad/.emacs.d/snippets/emacs-lisp-mode/begin" nil nil)
                       ("auto" ";;;###autoload" "autoload" nil nil nil "/home/toad/.emacs.d/snippets/emacs-lisp-mode/autoload" nil nil)))


;;; Do not edit! File generated at Fri Nov 27 20:06:11 2015
