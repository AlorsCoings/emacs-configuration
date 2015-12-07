;;; Compiled snippets and support files for `clojure-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojure-mode
                     '(("tc" "(ns `(snippet--clojure-namespace-from-buffer-file-name)`\n  (:use [`(snippet--clojure-namespace-under-test)`])\n  (:use [clojure.test]))\n\ntt$0" "testcase" nil nil nil "/home/toad/.emacs.d/snippets/clojure-mode/testcase" nil nil)
                       ("tt" "(deftest $1\n  (is (= $0)))" "test" nil nil nil "/home/toad/.emacs.d/snippets/clojure-mode/test" nil nil)
                       ("ns" "(ns `(snippet--clojure-namespace-from-buffer-file-name)`)$0" "namespace" nil nil nil "/home/toad/.emacs.d/snippets/clojure-mode/namespace" nil nil)))


;;; Do not edit! File generated at Fri Nov 27 20:06:11 2015
