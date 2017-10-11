;;; package --- setup-docker

;;; Commentary:
;;; Emacs configuration file for Docker

;;; Code:

(require 'docker)

;; Provide context-aware completion for docker-compose files
(require 'docker-compose-mode)

;; C-x C-f /docker:[user]@container:/path/to/file
(require 'docker-tramp)

;; Add this line to dockerfile to give it a default name
;;
;; ## -*- docker-image-name: "your-image-name-here" -*-
;;
;; Then C-c C-b to build the image
(require 'dockerfile-mode)

(provide 'setup-docker)
;;; setup-docker.el ends here
