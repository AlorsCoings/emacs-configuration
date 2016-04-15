;;; package --- setup-java

;;; Commentary:
;;; Emacs configuration file for java using eclim

;;; Code:

(require 'eclim)
(global-eclim-mode)

;; to control eclimd from emacs
(require 'eclimd)

(custom-set-variables
 '(eclim-eclipse-dirs '("/opt/eclipse"))
 '(eclim-executable '"/opt/eclipse/eclim")
 )

;; Displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

(define-key java-mode-map (kbd "C-<tab>") 'company-complete)
(define-key java-mode-map (kbd "M-<tab>") 'company-complete)
(define-key java-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))

(add-hook 'java-mode-hook 'semantic-mode)
(define-key java-mode-map (kbd "C-.") 'semantic-complete-jump-local)
(define-key java-mode-map (kbd "C-c C-i") 'eclim-java-import-organize)
(define-key java-mode-map (kbd "C-c p") 'eclim-problems)
(define-key java-mode-map (kbd "C-c C-p") 'eclim-problems-compilation-buffer)
(define-key java-mode-map (kbd "C-c C-c") 'eclim-run-class)

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4))
          t)

;;; eclim-java-hierarchy, C-c C-e h
;;; eclim-java-find-type, C-c C-e f t
;;; on a given method definition
;;; eclim-java-find-references, C-c C-e f r
(provide 'setup-java)
;;; setup-java.el ends here
