;;; package --- setup-server

;;; Commentary:
;;; Emacs configuration file for setup Emacs server

;;; Code:

(require 'server)
(unless (server-running-p)
  (atomic-chrome-start-server)
  (add-hook 'atomic-chrome-edit-mode-hook (lambda () (flyspell-mode)))
  (server-start))

(provide 'setup-server)
;;; setup-server.el ends here
