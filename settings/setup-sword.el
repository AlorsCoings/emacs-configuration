;;; package --- setup-sword

;;; Commentary:
;;; Emacs configuration file for sword
;; To use
;; (autoload 'ttl-mode "ttl-mode")
;; (add-hook 'ttl-mode-hook 'turn-on-font-lock)
;; (add-to-list 'auto-mode-alist '("\\.\\(n3\\|ttl\\|trig\\)\\'" . ttl-mode))

;;; Code:

;;;###autoload
(define-derived-mode sword-mode prog-mode "Sword mode"
  "Major mode for Sword documents."

  ;; Comments syntax
  (set (make-local-variable 'comment-start) "# ")
  (modify-syntax-entry ?# "< b" sword-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" sword-mode-syntax-table)

  ;; fontification
  (setq font-lock-defaults
        `((,(regexp-opt '("@prefix" "@base" "a") 'symbols)  ;keywords
           ("\\^\\^[^,;.]+" 0 font-lock-preprocessor-face t) ;literal types
           ("@[[:word:]_]+" . font-lock-preprocessor-face) ;languages
           ("\\S-*?:" . font-lock-type-face)       ;prefix
           (":\\([[:word:]_-]+\\)\\>" 1 font-lock-constant-face nil) ;suffix
           ("<.*?>" 0 font-lock-function-name-face t) ;resources
           ("[,;.]" 0 font-lock-keyword-face) ;punctuation
           ("^\\s-*\\(#.*\\)" 1 font-lock-comment-face t) ;comment
           ) nil))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'sword-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil))

;; electric punctuation
;; (define-key sword-mode-map (kbd "\,") 'sword-electric-comma)
(define-key sword-mode-map (kbd "\;") 'sword-electric-semicolon)
(define-key sword-mode-map (kbd "\.") 'sword-electric-dot)
(define-key sword-mode-map [backspace] 'sword-hungry-delete-backwards)


(defgroup sword nil "Customization for sword-mode")

(defcustom sword-indent-level 4
  "Number of spaces for each indentation step in `sword-mode'."
  :type 'integer)

(defcustom sword-electric-punctuation t
  "*If non-nil, `\;' or `\.' will self insert, reindent the line, and do a newline. (To insert while t, do: \\[quoted-insert] \;)."
  :type 'boolean)


(defun sword-indent-line ()
  (interactive)
  (save-excursion
    (indent-line-to
     (or (ignore-errors (sword-calculate-indentation)) 0)))
  (move-to-column (max (current-indentation) (current-column))))

(defun sword-calculate-indentation ()
  (save-excursion
    (backward-to-indentation 0)
    (cond
     ;; in multiline string
     ((nth 3 (syntax-ppss)) 'noindent)	; (current-indentation)
     ;; empty line
     ((looking-at "$") (save-excursion (backward-to-indentation 1)))
     ;; beginning of stanza
     ((or (looking-at "@")         ; @prefix
          (looking-at "#")         ; @base
          (save-excursion          ; a subject
            (while (forward-comment -1))
            (or (looking-back "\\.")
                (back-to-indentation) (looking-at "@"))) ; after prolog
          ) 0)
     ;; inside blank nodes
     (t (* sword-indent-level
           (+ (if (save-excursion
                    (while (forward-comment -1))
                    (looking-back "\\,")) ; object list
                  2 1)
              (nth 0 (syntax-ppss))	; levels in parens
              ))))))

(defun sword-insulate ()
  "Return true if this location should not be electrified"
  (or (not sword-electric-punctuation)
      (let '(s (syntax-ppss))
        (or (nth 3 s)
            (nth 4 s)
            (sword-in-resource-p)))))

(defun sword-in-resource-p ()
  "Is point within a resource, marked by <...>?"
  (save-excursion
    (and (re-search-backward "[<> ]" nil t)
         (looking-at "<"))))

;; (defun sword-electric-comma ()
;;   (interactive)
;;   (if (sword-insulate) (insert ",")
;;     (if (not (looking-back " ")) (insert " "))
;;     (insert ",")
;;     (reindent-then-newline-and-indent)))

(defun sword-electric-semicolon ()
  (interactive)
  (if (sword-insulate) (insert ";")
    (if (not (looking-back " ")) (insert " "))
    (insert ";")
    (reindent-then-newline-and-indent)))

(defun sword-electric-dot ()
  (interactive)
  (if (sword-insulate) (insert ".")
    (if (not (looking-back " ")) (insert " "))
    (insert ".")
    (reindent-then-newline-and-indent)))

(defun sword-skip-ws-backwards ()  ;adapted from cc-mode
  "Move backwards across whitespace."
  (while (progn
           (skip-chars-backward " \t\n\r\f\v")
           (and (eolp)
                (eq (char-before) ?\\)))
    (backward-char)))

(defun sword-hungry-delete-backwards ()
  "Delete backwards, either all of the preceding whitespace,
or a single non-whitespace character if there is no whitespace before point."
  (interactive)
  (let ((here (point)))
    (sword-skip-ws-backwards)
    (if (/= (point) here)
        (delete-region (point) here)
      (backward-delete-char-untabify 1))))

(provide 'sword-mode)
;;; sword-mode.el ends here
