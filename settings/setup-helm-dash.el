;;; package --- setup-helm-dash

;;; Commentary:
;;; Emacs configuration file for using dash docset with helm-dash

;;; Code:

(require 'helm-dash)
(setq helm-dash-docsets-path (concat user-emacs-directory ".docsets"))
(setq helm-dash-browser-func 'browse-url)
;; (setq helm-dash-browser-func 'eww)
(setq helm-dash-enable-debugging nil)

(defvar helm-dash-docsets nil "Local docset's list for helm-dash.")

(dolist (docset '("Emacs Lisp"))
  (add-to-list 'helm-dash-common-docsets docset))

(defun lua-doc ()
  "Define local `helm-dash-docsets' for Lua."
  (interactive)
  (setq-local helm-dash-docsets '("Lua")))
(add-hook 'lua-mode-hook 'lua-doc)

(defun python-doc ()
  "Define local `helm-dash-docsets' for Python."
  (interactive)
  (setq-local helm-dash-docsets '("Python 3"
                                  "OpenCV Python")))
(add-hook 'python-mode-hook 'python-doc)

(defun ocaml-doc ()
  "Define local `helm-dash-docsets' for Ocaml."
  (interactive)
  (setq-local helm-dash-docsets '("OCaml")))
(add-hook 'tuareg-mode-hook 'ocaml-doc)

(defun clojure-doc ()
  "Define local `helm-dash-docsets' for Clojure."
  (interactive)
  (setq-local helm-dash-docsets '("Clojure")))
(add-hook 'clojure-mode-hook 'clojure-doc)

(defun apache-doc ()
  "Define local `helm-dash-docsets' for Apache."
  (interactive)
  (setq-local helm-dash-docsets '("Apache_HTTP_Server")))
(add-hook 'apache-mode-hook 'apache-doc)

(defun ruby-doc ()
  "Define local `helm-dash-docsets' for Ruby."
  (interactive)
  (setq-local helm-dash-docsets '("Ruby"
                                  "Ruby on Rails")))
(add-hook 'ruby-mode-hook 'ruby-doc)

(defun javascript-doc ()
  "Define local `helm-dash-docsets' for Javascript."
  (interactive)
  (setq-local helm-dash-docsets '("AWS_JavaScript"
                                  "Grunt"
                                  "AngularJS"
                                  "JavaScript"
                                  "jQuery"
                                  "NodeJS")))
(add-hook 'js2-mode-hook 'javascript-doc)
(add-hook 'js-mode-hook 'javascript-doc)

(defun css-doc ()
  "Define local `helm-dash-docsets' for CSS."
  (interactive)
  (setq-local helm-dash-docsets '("CSS")))
(add-hook 'css-mode-hook 'css-doc)

(defun emacs-lisp-doc ()
  "Define local `helm-dash-docsets' for Emacs Lisp."
  (interactive)
  (setq-local helm-dash-docsets '("Emacs Lisp")))
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-doc)

(defun lisp-doc ()
  "Define local `helm-dash-docsets' for Lisp."
  (interactive)
  (setq-local helm-dash-docsets '("Common Lisp")))
(add-hook 'lisp-mode-hook 'lisp-doc)

(defun web-doc ()
  "Define local `helm-dash-docsets' for Web."
  (interactive)
  (setq-local helm-dash-docsets '("Angular.dart"
                                  "AngularJS"
                                  "Bootstrap 4"
                                  "CSS"
                                  "Emmet"
                                  "HTML"
                                  "PHP"
                                  "jQuery")))
(add-hook 'web-mode-hook 'web-doc)

(defun php-doc ()
  "Define local `helm-dash-docsets' for PHP."
  (interactive)
  (setq-local helm-dash-docsets '("PHP")))
(add-hook 'php-mode-hook 'php-doc)

(defun arduino-doc ()
  "Define local `helm-dash-docsets' for Arduino."
  (interactive)
  (setq-local helm-dash-docsets '("Arduino")))
(add-hook 'arduino-mode-hook 'arduino-doc)

(defun LaTeX-doc ()
  "Define local `helm-dash-docsets' for LaTeX."
  (interactive)
  (setq-local helm-dash-docsets '("LaTeX")))
(add-hook 'LaTeX-mode-hook 'LaTeX-doc)

(defun bash-doc ()
  "Define local `helm-dash-docsets' for Bash."
  (interactive)
  (setq-local helm-dash-docsets '("Bash")))
(add-hook 'sh-mode-hook 'bash-doc)

(defun c-doc ()
  "Define local `helm-dash-docsets' for C."
  (interactive)
  (setq-local helm-dash-docsets '("C"
                                  "OpenCV C"
                                  "OpenGL4"
                                  "Qt")))
(add-hook 'c-mode-hook 'c-doc)

(defun c++-doc ()
  "Define local `helm-dash-docsets' for C++."
  (interactive)
  (setq-local helm-dash-docsets '("C++"
                                  "OpenCV C++"
                                  "OpenGL4"
                                  "Qt")))
(add-hook 'c++-mode-hook 'c++-doc)

(defun cmake-doc ()
  "Define local `helm-dash-docsets' for CMake."
  (interactive)
  (setq-local helm-dash-docsets '("CMake")))
(add-hook 'makefile-mode-hook 'cmake-doc)

(defun java-doc ()
  "Define local `helm-dash-docsets' for Java."
  (interactive)
  (setq-local helm-dash-docsets '("Java"
                                  "Java EE7"
                                  "OpenCV Java")))
(add-hook 'java-mode-hook 'java-doc)

;; (helm-dash-install-docset "ActionScript")
;; (helm-dash-install-docset "Android")
;; (helm-dash-install-docset "Angular.dart")
;; (helm-dash-install-docset "AngularJS")
;; (helm-dash-install-docset "Apache_HTTP_Server")
;; (helm-dash-install-docset "Arduino")
;; (helm-dash-install-docset "AWS_JavaScript")
;; (helm-dash-install-docset "Bash")
;; (helm-dash-install-docset "Bootstrap_4")
;; (helm-dash-install-docset "C")
;; (helm-dash-install-docset "C++")
;; (helm-dash-install-docset "Clojure")
;; (helm-dash-install-docset "CMake")
;; (helm-dash-install-docset "Common_Lisp")
;; (helm-dash-install-docset "CSS")
;; (helm-dash-install-docset "Emacs_Lisp")
;; (helm-dash-install-docset "Emmet")
;; (helm-dash-install-docset "Grunt")
;; (helm-dash-install-docset "HTML")
;; (helm-dash-install-docset "Java_EE7")
;; (helm-dash-install-docset "JavaScript")
;; (helm-dash-install-docset "Java_SE7")
;; (helm-dash-install-docset "Java_SE8")
;; (helm-dash-install-docset "jQuery")
;; (helm-dash-install-docset "LaTeX")
;; (helm-dash-install-docset "Lua_5.3")
;; (helm-dash-install-docset "NodeJS")
;; (helm-dash-install-docset "OCaml")
;; (helm-dash-install-docset "OpenCV_C")
;; (helm-dash-install-docset "OpenCV_C++")
;; (helm-dash-install-docset "OpenCV_Java")
;; (helm-dash-install-docset "OpenCV_Python")
;; (helm-dash-install-docset "OpenGL_4")
;; (helm-dash-install-docset "PHP")
;; (helm-dash-install-docset "Python_3")
;; (helm-dash-install-docset "Qt_5")
;; (helm-dash-install-docset "Ruby")
;; (helm-dash-install-docset "Ruby_on_Rails_5")


(provide 'setup-helm-dash)
;;; setup-helm-dash.el ends here
