;;; package --- my-macro

;;; Commentary:
;;; Emacs configuration file for defining my own macro

;;; Code:

;; Save last macro
(defun save-macro (name)
  "Save current macro with NAME.
Take a name as argument and save the last defined macro under
this name at the end of your .emacs"
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (save-excursion
    (save-window-excursion
      (find-file (concat settings-dir "/my-macro.el"))
      (goto-char (point-max))
      (forward-line -2)
      (insert-kbd-macro name)
      (newline))))

(provide 'my-macro)
;;; my-macro.el ends here
