;;; package --- setup-server

;;; Commentary:
;;; Emacs configuration file for setup Emacs server

;;; Code:

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'setup-server)
;;; setup-server.el ends here
