;;; package --- setup-markdown

;;; Commentary:
;;; Emacs configuration file for markdown

;;; Code:

(require 'markdown-mode)

(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key markdown-mode-map (kbd "M-p") nil)))

(provide 'setup-markdown)
;;; setup-mardown.el ends here
