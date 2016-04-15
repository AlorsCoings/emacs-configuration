;;; package --- Gnus mail configuration

;;; Commentary:
;;; This my Emacs configuration file for reading mail with gnus

;;; Code:
(require 'gnus)
(require 'gnus-start)

(setq user-full-name "Nicolas Gros")
(setq user-mail-address "nico8ness@gmail.com")
(require 'starttls)
(setq starttls-use-gnutls t)

;; You need this to be able to list all labels in gmail
(setq gnus-ignored-newsgroups "")

;; (setenv "GPG_AGENT_INFO"  nil)

(setq gnus-select-method '(nnml ""))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gros.nicolas0"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
					  ))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "nico9ness"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
					  ))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "nico8ness"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
					  ))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "nico7ness"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
					  ))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(require 'gnus-group)
(setq gnus-group-sort-function 'gnus-group-sort-by-real-name)
(setq gnus-permanently-visible-groups ".*")

(require 'sendmail)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")


;; This is needed to allow msmtp to do its magic:
(setq message-sendmail-f-is-evil 't)

;;need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

;; (setq message-send-mail-function 'message-send-mail-with-sendmail)
;; (setq sendmail-program "/usr/local/bin/msmtpq"
;;       mail-specify-envelope-from t
;;       message-sendmail-f-is-evil nil
;;       mail-envelope-from 'header
;;       message-sendmail-envelope-from 'header)

(setq gnus-parameters
      '(("nnimap gros.nicolas0:nnimap+gros.nicolas0*"
         (display . all)
         (posting-style
          (name "Nicolas Gros")
          (address "gros.nicolas0@gmail.com")
          (organization "")
          (signature-file "~/.emacs.d/signature.d/gros.nicolas0@gmail.com")
          )
         (expiry-target . delete)
         (expiry-wait . never))
        ("nnimap nico9ness:nnimap+nico9ness*"
         (display . all)
         (posting-style
          (name "Nicolas Gros")
          (address "nico9ness@gmail.com")
          (organization "")
          (signature-file "~/.emacs.d/signature.d/nico9ness@gmail.com")
          )
         (expiry-target . delete)
         (expiry-wait . never))
        ("nnimap nico8ness:nnimap+nico8ness*"
         (display . all)
         (posting-style
          (name "Nicolas Gros")
          (address "nico8ness@gmail.com")
          (signature-file "~/.emacs.d/signature.d/nico8ness@gmail.com")
          )
         (expiry-target . delete)
         (expiry-wait . never))
        ("nnimap nico7ness:nnimap+nico7ness*"
         (display . all)
         (posting-style
          (name "Nicolas Gros")
          (address "nico7ness@gmail.com")
          (signature-file "~/.emacs.d/signature.d/nico7ness@gmail.com")
          )
         (expiry-target . delete)
         (expiry-wait . never))))

(require 'gnus-msg)
(setq gnus-posting-styles
      '(((header "to" "nico9ness@gmail.com")
         (address "nico9ness@gmail.com"))
        ((header "cc" "nico9ness@gmail.com")
         (address "nico9ness@gmail.com"))
        ((header "to" "nico8ness@gmail.com")
         (address "nico8ness@gmail.com"))
        ((header "cc" "nico8ness@gmail.com")
         (address "nico8ness@gmail.com"))
        ((header "to" "nico7ness@gmail.com")
         (address "nico7ness@gmail.com"))
        ((header "cc" "nico7ness@gmail.com")
         (address "nico7ness@gmail.com"))
        ((header "to" "gros.nicolas0@gmail.com")
         (address "gros.nicolas0@gmail.com"))
        ((header "cc" "gros.nicolas0@gmail.com")
         (address "gros.nicolas0@gmail.com"))))

;; gnus-notify allows you to select which groups you want to include in the report
;; It does this via a „group parameter“ you have to add manually.
;; To do it: use G p in the group buffer, then add (modeline-notify t) to the list there
;; If it were the only property, it would look like this: ((modeline-notify t))
(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) ; this does a call to gnus-group-get-new-news

(provide 'setup-gnus)
;;; setup-gnus.el ends here
