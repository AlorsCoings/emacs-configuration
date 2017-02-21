;;; package --- setup-R

;;; Commentary:
;;; Emacs configuration file for R

;;; Code:

(require 'ess-mode)

(define-key ess-mode-map (kbd "C-c C-n") 'cleanup-buffer)

(provide 'setup-R)
;;; setup-R.el ends here
