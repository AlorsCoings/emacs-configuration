;;; Compiled snippets and support files for `feature-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'feature-mode
                     '(("sc" "Scenario: $1\n  Given $0\n  When\n  Then" "scenario" nil nil nil nil nil nil)
                       ("ft" "Feature: $1\n\n  sc$0\n" "feature" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Mon Aug 24 17:16:26 2015