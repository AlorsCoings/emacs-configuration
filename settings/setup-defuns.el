;;; package --- setup-defuns

;;; Commentary:
;;; Emacs configuration file for loading function from defuns dir

;;; Code:

(defvar defuns-dir (expand-file-name "defuns" settings-dir))

(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(provide 'setup-defuns)
;;; setup-defuns.el ends here
