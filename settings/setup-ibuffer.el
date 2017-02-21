;;; package --- setup-ibuffer

;;; Commentary:
;;; Emacs configuration file for ibuffer

;;; Code:

;; M-s a C-s - Do incremental search in the marked buffers.
;; M-s a C-M-s - Isearch for regexp in the marked buffers.
;; U - Replace by regexp in each of the marked buffers.
;; Q - Query replace in each of the marked buffers.
;; I - As above, with a regular expression.
;; 0 - Run occur on the marked buffers.

(declare-function after-load "sane-defaults" (feature &rest body))

(require 'ibuffer)

;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(after-load 'ibuffer
  (require 'ibuffer-vc))

(defun ibuffer-set-up-preferred-filters ()
  "Filter ibuffer by filename/process."
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-set-up-preferred-filters)
                          (define-key ibuffer-mode-map (kbd "C-d") 'previous-line)))

(setq-default ibuffer-show-empty-filter-groups nil)


(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(define-key ibuffer-mode-map (kbd "C-t") 'backward-char)
(define-key ibuffer-mode-map (kbd "C-r") 'forward-char)
(define-key ibuffer-mode-map (kbd "C-d") 'previous-line)
(define-key ibuffer-mode-map (kbd "C-s") 'next-line)
(define-key ibuffer-mode-map (kbd "d") 'previous-line)
(define-key ibuffer-mode-map (kbd "s") 'next-line)
(define-key ibuffer-mode-map (kbd "D") 'ibuffer-mark-for-delete)
(define-key ibuffer-mode-map (kbd "C-o") 'other-window)
(define-key ibuffer-mode-map (kbd "U") 'ibuffer-unmark-all)

(provide 'setup-ibuffer)
;;; setup-ibuffer.el ends here
