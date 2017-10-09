;;; package --- setup-org

;;; Commentary:
;;; This my Emacs configuration file for org-mode

;;; Code:

(declare-function after-load "sane-defaults" (feature &rest body))

(require 'org)
(require 'org-archive)

(add-to-list 'org-export-backends 'beamer)

(eval-after-load 'org '(require 'org-pdfview))

(setq org-src-fontify-natively t)

(add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
(add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))

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

(define-key org-mode-map (kbd "<RET>") 'org-return-indent)

(defadvice org-return-indent (after fix-cookies activate)
  "Update todo statistics on kill line."
  (myorg-update-parent-cookie))

;; Various preferences
(setq org-log-done 'time
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-catch-invisible-edits 'show
      org-fast-tag-selection-single-key 'expert
      org-tags-column 80
      org-enforce-todo-dependencies t)

(require 'ox)
(setq org-export-coding-system 'utf-8)

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(setq org-agenda-files '("~/org"))

(require 'org-agenda)
(define-key org-agenda-mode-map (kbd "V") 'org-agenda-do-date-earlier)
(define-key org-agenda-mode-map (kbd "L") 'org-agenda-do-date-later)
(define-key org-agenda-mode-map (kbd "c") 'org-agenda-capture)

(setq org-agenda-custom-commands
      '(("w" "Work"
         ((agenda "")
          (tags-todo "work")))
        ;; tags "work"
        ;; ((org-agenda-overriding-header "Work")
        ;;  (org-tags-match-list-sublevels t)
        ;;  (agenda "")))
        ;; (("h" "Agenda and Home-related tasks"
        ;;   ((agenda "")
        ;;    (tags-todo "home")
        ;;    (tags "garden")))
        ;;  ("o" "Agenda and Office-related tasks"
        ;;   ((agenda "")
        ;;    (tags-todo "work")
        ;;    (tags "office"))))
        ;; ("h" "Habits" tags-todo "STYLE=\"habit\""
        ;;  ((org-agenda-overriding-header "Habits")
        ;;   (org-agenda-sorting-strategy
        ;;    '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda "" t)
          (tags-todo "Work"
                     ((org-agenda-overriding-header "Work")
                      (org-tags-match-list-sublevels nil)))))))

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
      (or (org-at-date-range-p)
          (org-at-timestamp-p)
          (org-at-clock-log-p))
      (org-timestamp-up-day)
    (org-metaright)))

;; Don't split the line when creating new headline
(add-to-list 'org-M-RET-may-split-line '(headline . nil))

(mapc (lambda (info)
        (let ((key (kbd (car info)))
              (function (car (cdr info))))
          (define-key org-mode-map key function)))
      '(("C-M-t" nil)
        ("C-c C-<" mc/mark-all-like-this)
        ("C-M-v" org-backward-heading-same-level)
        ("C-M-l" org-forward-heading-same-level)
        ("C-M-d" outline-up-heading)
        ("M-d" my-org-metaup)
        ("M-s" my-org-metadown)
        ("M-D" org-shiftmetaup)
        ("M-S" org-shiftmetadown)
        ("M-v" my-org-metaleft)
        ("M-l" my-org-metaright)
        ("M-V" org-shiftmetaleft)
        ("M-L" org-shiftmetaright)
        ("C-c v" outline-up-heading)
        ("C-c C-" org-table-edit-field)
        ("C-e" delete-forward-char)
        ("M-e" kill-word)
        ("C-M-e" kill-sexp)
        ("C-i" pcomplete)
        ("M-i" backward-kill-word)
        ("C-M-i" backward-kill-sexp)
        ("C-c C-x C-s" org-clock-in)
        ("C-c C-x C-p" org-set-property)
        ("C-c C-x C-d" org-clock-display)
        ("C-c C-x C-o" org-clock-out)
        ("C-c C-x e" org-set-effort)
        ("C-'" delete-backward-char)
        ("C-c C-n" cleanup-buffer)))

(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)

(define-key minibuffer-local-map (kbd "C-d") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-s") 'next-history-element)

(define-key org-agenda-mode-map (kbd "r") 'org-agenda-later)
(define-key org-agenda-mode-map (kbd "t") 'org-agenda-earlier)
(define-key org-agenda-mode-map (kbd "C-c t") 'org-agenda-todo)

;; (setq org-html-validation-link nil)

;; Configure org-agenda-time-grid ?
;; (setq org-agenda-use-time-grid nil)

(setq org-agenda-custom-commands
      '(("A" "Project A"
         ((agenda "")
          (tags-todo "pa|pb")))))

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

(defvar org-capture-templates
  '(("t" "Todo" entry (file+headline "~/org/home.org" "Tasks")
     "* TODO %?\nCreated at %U")
    ("e" "Emacs Todo" entry (file+headline "~/org/home.org" "Emacs")
     "* TODO %?\nSCHEDULED: %t\nCreated at %U")
    ("j" "Perles Soline" entry (file+datetree "~/org/perlesSoline.org")
     "* %?\nEntered on %U")
    ("n" "Next stuff" entry (file+headline "~/org/home.org" "Miscellaneous")
     "* TODO %?\nSCHEDULED: %t\nEntered on %U")
    ("c" "Clock" entry (clock)
     "* TODO %?\nEntered on %U\n%a")
    ("a" "Agenda" entry (file "~/org/agenda.org")
     "*  %?

%T

Description")
    ("l" "Agenda Location" entry (file "~/org/agenda.org")
     "*  %? %^{LOCATION}p

%T

Description")
    ("s" "Journal" entry (file+datetree "~/org/journal.org")
     "* %?\nEntered on %U\n  %i\n  %a")))

(require 'org-feed)
(setq org-feed-alist
      '(("Slashdot"
         "http://rss.slashdot.org/Slashdot/slashdot"
         "~/org/feeds.org" "Slashdot Entries")
        ("PersistentInfo"
         "http://feeds.feedburner.com/PersistentInfo"
         "~/org/feeds.org" "PersistentInfo")
        ("LifeHacker"
         "http://feeds.gawker.com/lifehacker/full"
         "~/org/feeds.org" "LifeHacker")))

(require 'org-clock)
(setq org-clock-idle-time 10)
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

(require 'setup-org-gcal)

(add-hook 'after-init-hook (lambda ()
                             (org-agenda-list)
                             (delete-other-windows)))

;; And add babel inline code execution, for executing code in org-mode.
;; load all language marked with (lang . t).
(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     ;; (R . t) ;; seems to bug under some installation
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
     (java . t)
     (js)
     (latex . t)
     (ledger)
     (lilypond)
     (lisp . t)
     (matlab)
     (maxima)
     (mscgen)
     (ocaml)
     (octave)
     (org . t)
     (perl . t)
     (picolisp)
     (plantuml)
     (python . t)
     (ref)
     (ruby . t)
     (sass)
     (scala)
     (scheme)
     (screen)
     (shell . t)
     (shen)
     (sql . t)
     (dot . t)
     (sqlite))))

(setq org-agenda-compact-blocks nil)
(setq org-agenda-dim-blocked-tasks nil)

(setq org-confirm-babel-evaluate nil)
;; (setq org-export-babel-evaluate t)

(add-hook 'org-export-before-processing-hook #'org-babel-tangle)

(add-hook 'after-init-hook
          (lambda ()
            (org-agenda-list)
            (delete-other-windows)))

(provide 'setup-org)
;;; setup-org.el ends here
