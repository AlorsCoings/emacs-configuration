;;; package --- setup-gnuplot

;;; Commentary:
;;; Emacs configuration file for gnuplot

;;; Code:

(require 'gnuplot-mode)

(setq gnuplot-program "/usr/bin/gnuplot")

(setq auto-mode-alist
      (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))

(provide 'setup-gnuplot)
;;; setup-gnuplot.el ends here
