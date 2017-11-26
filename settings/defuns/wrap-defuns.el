;;; package --- wrap-defuns

;;; Commentary:
;;; Emacs configuration file for own wrapping functions

;;; Code:

(defun my-wrap ()
  "Wrap region or sexp with \"."
  (interactive)
  (cond ((use-region-p)
         (let ((beg-mark (region-beginning))
               (end-mark (region-end)))
           (goto-char end-mark)
           (insert "\"")
           (goto-char beg-mark)
           (insert "\"")))
        (t
         (when (in-sexp)
           (backward-sexp))
         (insert "\"")
         (forward-sexp)
         (insert "\""))))

(provide 'wrap-defuns)
;;; wrap-defuns.el ends here
