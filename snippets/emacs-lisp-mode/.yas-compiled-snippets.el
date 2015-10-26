;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("req" "(require '$0)" "req" nil nil nil nil nil nil)
                       ("pro" "(provide '`(buffer-file-name-body)`)$0" "provide" nil nil nil nil nil nil)
                       ("ends" "(provide '`(buffer-file-name-body)`)\n;;; `(buffer-file-name-body)`.el ends here$0" "ends" nil nil nil nil nil nil)
                       ("begin" ";;; package --- `(buffer-file-name-body)`\n\n;;; Commentary:\n;;; Emacs configuration file for $1\n\n;;; Code:\n$0" "begin" nil nil nil nil nil nil)
                       ("auto" ";;;###autoload" "autoload" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Mon Aug 24 17:16:26 2015
