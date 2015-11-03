;;; package --- setup-nxml

;;; Commentary:
;;; Emacs configuration file for nxml

;;; Code:

(require 'nxml-mode)

(setq nxml-slash-auto-complete-flag t)

;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun sanityinc/pp-xml-region (beg end)
  "Pretty format XML markup in region from BEG to END.
Insert linebreaks to separate tags that have nothing but
whitespace between them.  It then indents the markup by
using nxml's indentation rules."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  ;; Use markers because our changes will move END
  (setq beg (set-marker (make-marker) beg)
        end (set-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "\>[ \\t]*\<" end t)
      (backward-char) (insert "\n"))
    (nxml-mode)
    (indent-region beg end)))

;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------
(require 'tidy)
(add-hook 'nxml-mode-hook (lambda () (tidy-build-menu nxml-mode-map)))

(defun sanityinc/tidy-buffer-xml (beg end)
  "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))

(provide 'setup-nxml)
;;; setup-nxml.el ends here