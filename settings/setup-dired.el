;;; package --- setup-dired

;;; Commentary:
;;; Emacs dired configuration

;;; Code:

(require 'dired)
(require 'dash)

(add-hook 'dired-mode-hook
          (lambda()
            (dired-hide-details-mode)))

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Get human readeable value of file's size in dired mode
(setq dired-listing-switches "-alh")

;; Move files between split panes
(setq dired-dwim-target t)

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  "\\<dired-back-to-start-of-files> is nicer in dired if it move back to start of files."
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key dired-mode-map (kbd "k") 'dired-do-delete)
(define-key dired-mode-map (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-S-o") (lambda() (interactive) (other-window -1)))
(define-key dired-mode-map (kbd "C-S-n") 'dired-do-isearch)
(define-key dired-mode-map (kbd "C-M-S-n") 'dired-do-isearch-regexp)
(define-key dired-mode-map (kbd "C-M-n") 'isearch-forward-regexp)

(define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  "Move to the last file."
  (interactive)
  (goto-char (point-max))
  ;; For smooth-scroll to take effect
  (dired-next-line -2)
  (dired-next-line 1))

(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)

(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'dired-zip-files))

(defun dired-zip-files (zip-file)
  "Create an archive containing the marked ZIP-FILE."
  (interactive "sEnter name of zip file: ")
  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip "
             zip-file
             " "
             (concat-string-list (mapcar
                                  #'(lambda (filename)
                                      (file-name-nondirectory filename))
                                  (dired-get-marked-files))))))
  (revert-buffer))

(defun concat-string-list (list)
  "Return a string which is a concatenation of all elements of the LIST separated by spaces."
  (mapconcat #'(lambda (obj) (format "%s" obj)) list " "))

(define-key dired-mode-map (kbd "C-t") 'backward-char)

(provide 'setup-dired)
;;; setup-dired ends here
