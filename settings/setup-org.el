;;; package --- setup-org

;;; Commentary:
;;; This my emacs configuration file for org-mode

;;; Code:

(require 'org)
(require 'org-archive)

(defun myorg-update-parent-cookie ()
  "Update the todo statistics in parent heading."
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  "Update todo statistics on kill line."
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  "Update todo statistics on kill whole line."
  (myorg-update-parent-cookie))

;; Various preferences
(setq org-log-done 'time
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-catch-invisible-edits 'show
      ;; org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
	  org-tags-column 80)

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

;; (setq org-log-done 'note)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/myAgenda.org"
                             "~/org/test.org"
                             "~/org/lesDoudous.org"
                             "~/org/maison.org"
                             "~/org/home.org"))


(defun my-org-metaup ()
  "Increase date element if on a date otherwise use org-metaup."
  (interactive)
  (if
	  (or (org-at-date-range-p) (org-at-timestamp-p))
	  (org-timestamp-up)
	(org-metaup)))

(defun my-org-metadown ()
  "Decrease date element if on a date otherwise use org-metadown."
  (interactive)
  (if
	  (or (org-at-date-range-p) (org-at-timestamp-p))
	  (org-timestamp-down)
	(org-metadown)))

(defun my-org-metaleft ()
  "Decrease days if on a date otherwise use org-metaleft."
  (interactive)
  (if
	  (or (org-at-date-range-p) (org-at-timestamp-p))
	  (org-timestamp-down-day)
	(org-metaleft)))

(defun my-org-metaright ()
  "Increase days if on a date otherwise use org-metaright."
  (interactive)
  (if
	  (or (org-at-date-range-p) (org-at-timestamp-p) (org-at-clock-log-p))
	  (org-timestamp-up-day)
	(org-metaright)))

(add-hook 'org-mode-hook
		  (lambda ()
			(define-key org-mode-map (kbd "M-p") 'my-org-metaup)
			(define-key org-mode-map (kbd "M-n") 'my-org-metadown)
			(define-key org-mode-map (kbd "M-P") 'org-shiftmetaup)
			(define-key org-mode-map (kbd "M-N") 'org-shiftmetadown)
			(define-key org-mode-map (kbd "M-a") 'my-org-metaleft)
			(define-key org-mode-map (kbd "M-e") 'my-org-metaright)
			(define-key org-mode-map (kbd "M-A") 'org-shiftmetaleft)
			(define-key org-mode-map (kbd "M-E") 'org-shiftmetaright)
			(define-key org-mode-map (kbd "C-c C-x C-s") 'org-clock-in)))

(require 'org-table)
(add-to-list 'orgtbl-radio-table-templates
             '(web-mode "<!-- BEGIN RECEIVE ORGTBL %n -->
<!-- END RECEIVE ORGTBL %n -->
<!--
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
-->\n"))

(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'LaTeX-mode-hook 'turn-on-orgtbl)
(add-hook 'js2-mode-hook 'turn-on-orgtbl)
(add-hook 'web-mode-hook 'turn-on-orgtbl)
(require 'company-irony)
(push 'orgtbl-self-insert-command company-begin-commands)

(require 'org-clock)
(setq org-clock-idle-time 10)
(defvar org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/home.org" "Tasks")
				 "* TODO %?\nCreated at %T")
				("e" "Emacs Todo" entry (file+headline "~/org/home.org" "Emacs")
				 "* TODO %?\nCreated at %T")
				("a" "Agenda" entry (file "~/org/agenda.org")
				 "* %?Event\n\t:PROPERTIES:\n\t:ID:\n\t:END:\n\n\t%T\n")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
				 "* %?\nEntered on %U\n  %i\n  %a")))

;; To save the clock history across Emacs sessions
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
(defun sanityinc/show-org-clock-in-header-line ()
  "Show org clock in header line."
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  "Hide org clock in header line."
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(setq org-archive-location "%s_archive::* Archive")

;; (require 'org-compat)
;; (require 'org-gcal)
;; (setq org-gcal-client-id "298706557354-jea7ks1mctj7a0cb6c1dnrcj7ulo4jrf.apps.googleusercontent.com"
;;       org-gcal-client-secret "whupPIGL6RmIWL0rJXjxkYlg"
;;       org-gcal-file-alist '(("nico8ness@gmail.com" .  "~/org/agenda.org")
;; 							("s101uit489k7q1v5hr2g8jedro@group.calendar.google.com" .  "~/lesDoudous.org")                            ))
;; ("s101uit489k7q1v5hr2g8jedro@group.calendar.google.com" .  "~/lesDoudous.org")

;; mobileorg settings
(require 'org-mobile)
(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files '("~/Dropbox/org"))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;; 			(add-hook 'after-save-hook 'org-mobile-push nil 'make-it-local)))

;; And add babel inline code execution, for executing code in org-mode.
(org-babel-do-load-languages
 'org-babel-load-languages
 ; load all language marked with (lang . t).
 '((C . t)
   (R . t)
   (asymptote)
   (awk)
   (calc)
   (clojure)
   (comint)
   (css)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (fortran)
   (gnuplot . t)
   (haskell)
   (io)
   (java)
   (js)
   (latex)
   (ledger)
   (lilypond)
   (lisp)
   (matlab)
   (maxima)
   (mscgen)
   (ocaml)
   (octave)
   (org . t)
   (perl)
   (picolisp)
   (plantuml)
   (python . t)
   (ref)
   (ruby)
   (sass)
   (scala)
   (scheme)
   (screen)
   (sh . t)
   (shen)
   (sql)
   (sqlite)))

(provide 'setup-org)
;;; setup-org.el ends here
