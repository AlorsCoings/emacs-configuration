;;; package --- setup-image

;;; Commentary:
;;; Emacs configuration file for image+

;;; Code:

(require 'image-mode)

(define-key image-mode-map (kbd "+") 'imagex-sticky-zoom-in)
(define-key image-mode-map (kbd "-") 'imagex-sticky-zoom-out)
(define-key image-mode-map (kbd "M") 'imagex-sticky-maximize)
(define-key image-mode-map (kbd "O") 'imagex-sticky-restore-original)
(define-key image-mode-map (kbd "S") 'imagex-sticky-save-image)
(define-key image-mode-map (kbd "r") 'imagex-sticky-rotate-right)
(define-key image-mode-map (kbd "l") 'imagex-sticky-rotate-left)

(provide 'setup-image)
;;; setup-image.el ends here
