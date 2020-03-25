;;; package --- setup-go

;;; Commentary:
;;; Emacs configuration file for go

;;; Code:

(require 'go-mode)
(require 'go-guru)

(defun my-run-file()
  "Execute './filename-sans-extension' in shell buffer."
  (interactive)
  (shell-command
   (concat "./" (file-name-sans-extension
                 (file-name-nondirectory
                  (buffer-file-name))))))


(defun my-go-compile(arg)
  "Compile when call and execute current file if `universal-argument' ARG is given."
  (interactive "P")
  (message "args are: %s" arg)
  (cond
   ((equal arg nil)
    (call-interactively #'compile))
   (t
    (call-interactively #'my-run-file))))

(defun my-go-doc(arg)
  "Get documentation and handle `universal-argument' ARG is given."
  (interactive "P")
  (message "args are: %s" arg)
  (cond
   ((equal arg nil)
    (call-interactively #'godoc-at-point))
   (t
    (call-interactively #'godef-describe))))

;; My go setup
(defun my-go-setup ()
  "My go setup."
  ;; Use goimports insteat of gofmt
  (setq gofmt-command "goimports")

  ;; Format on save
  (add-hook 'before-save-hook #'gofmt-before-save)

  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate -v && go build -v && go test -v && go vet"))

  (define-key go-mode-map (kbd "C-c C-c") #'my-go-compile)
  (define-key go-mode-map (kbd "C-c C-d") #'my-go-doc)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-c C-p") 'go-guru-describe)

  (go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  )

(add-hook 'go-mode-hook #'my-go-setup)


(provide 'setup-go)
;;; setup-go.el ends here
