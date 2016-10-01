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

(require 'help-at-pt)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

(require 'cc-mode)

(setq eclimd-executable nil
      eclimd-default-workspace "~/workspace")

(define-key java-mode-map (kbd "C-<tab>") 'company-complete)
(define-key java-mode-map (kbd "M-<tab>") 'company-complete)
(define-key java-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key java-mode-map (kbd "C-.") 'semantic-complete-jump-local)
(define-key java-mode-map (kbd "C-c C-i") 'eclim-java-import-organize)
(define-key java-mode-map (kbd "C-c p") 'eclim-problems)
(define-key java-mode-map (kbd "C-c C-p") 'eclim-problems-correct)
(define-key java-mode-map (kbd "C-c C-l") 'eclim-problems-compilation-buffer)
(define-key java-mode-map (kbd "C-c C-c") 'eclim-run-class)
(define-key java-mode-map (kbd "C-c C-u") 'eclim-maven-lifecycle-phase-run)
(define-key java-mode-map (kbd "C-c C-c") 'eclim-run-class)
(define-key java-mode-map (kbd "C-c C-r") 'eclim-java-refactor-rename-symbol-at-point)
(define-key java-mode-map (kbd "C-c C-f") 'eclim-java-find-references)
(define-key java-mode-map (kbd "C-c C-d") 'eclim-java-show-documentation-for-current-element)
(define-key java-mode-map (kbd "C-c u") 'eclim-maven-run)

(define-key java-mode-map (kbd "C-d") 'previous-line)
(define-key java-mode-map (kbd "C-c C-n") 'cleanup-buffer)

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (semantic-mode 1))
          t)

;;; eclim-java-hierarchy, C-c C-e h
;;; eclim-java-find-type, C-c C-e f t
;;; on a given method definition
;;; eclim-java-find-references, C-c C-e f r

(provide 'setup-java)
;;; setup-java.el ends here
