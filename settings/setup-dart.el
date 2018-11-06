;;; package --- setup-dart

;;; Commentary:
;;; Emacs configuration file for dart

;;; Code:

(require 'dart-mode)

(setq dart-enable-analysis-server t)
(add-hook 'dart-mode-hook 'flycheck-mode)

(provide 'setup-dart)
;;; setup-dart.el ends here
