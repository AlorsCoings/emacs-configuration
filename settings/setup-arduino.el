;;; package --- setup-arduino

;;; Commentary:
;;; Emacs arduino configuration

;;; Code:

(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

(provide 'setup-arduino)
;;; setup-arduino.el ends here
