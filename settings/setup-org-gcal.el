;;; package --- setup-org-gcal

;;; Commentary:
;;; Emacs configuration file for org-gcal

;;; Code:

(require 'org-gcal)

(setq org-gcal-client-id "715462846304-h9tbe2jvs2pbpakt6gfl2f452kots5gg.apps.googleusercontent.com"
      org-gcal-client-secret "71P4oJtFKIVuK_Gk4QCR14PL"
      org-gcal-file-alist '(;; ("qufrio94dao8o2mn85pdcbhfjk@group.calendar.google.com" .  "~/org/agenda.org")
                            ("s101uit489k7q1v5hr2g8jedro@group.calendar.google.com" . "~/org/doudous.org")))
(define-key org-mode-map (kbd "C-c C-r") 'org-gcal-post-at-point)
(define-key org-mode-map (kbd "C-c C-f") 'org-gcal-sync)

(org-gcal-refresh-token)
(org-gcal-sync)

(provide 'setup-org-gcal)
;;; setup-org-gcal.el ends here
