;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("try" "try {\n    $0\n} catch (e) {\n}" "try" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/try" nil nil)
                       ("." "this.$0" "this" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/this.yasnippet" nil nil)
                       ("ppp" "${2:$1}: { value: params.${1:property} },\nppp$0" "object.create-property+" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/object.create-property+" nil nil)
                       ("pp" "${2:$1}: { value: params.${1:property} }$0" "object.create-property" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/object.create-property" nil nil)
                       ("create" "create: function (params) {\n  return Object.create(this, {\n    ppp$0\n  });\n}" "object.create" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/object.create" nil nil)
                       ("req" "var ${3:${1:$(lower-camel-case (file-name-nondirectory yas/text))}} = require(\"${1:fs}\")$2;$0" "node.require" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/node.require.yasnippet" nil nil)
                       ("mx" "module.exports = $0;" "node.module.exports" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/node.module.exports.yasnippet" nil nil)
                       ("ifnode" "if (typeof require === \"function\" && typeof module !== \"undefined\") {\n    $0\n}" "node.ifnode" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/node.ifnode.yasnippet" nil nil)
                       ("tc" "(function (${2:${1:$(buster--shortcuts-for-globals yas/text)}}) {\n  testCase(\"${3:`(chop-suffix \"Test\" (upper-camel-case (buffer-file-name-body)))`}$4Test\", sinon.testCase({\n    tt$0\n  }));\n}(${1:`(if buster-add-default-global-to-iife buster-default-global)`}));\n" "jstd.testCase"
                        (not buster-testcase-snippets-enabled)
                        nil nil "/home/toad/.emacs.d/snippets/js-mode/jstd.testCase.yasnippet" nil nil)
                       ("log" "jstestdriver.console.log($0);" "jstd.log" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/jstd.log.yasnippet" nil nil)
                       ("tcc" "testCase(\"${2:`(chop-suffix \"Test\" (upper-camel-case (buffer-file-name-body)))`}$3\", sinon.testCase({\n    tt$0\n}));\n" "jstd.additionalTestCase"
                        (not buster-testcase-snippets-enabled)
                        nil nil "/home/toad/.emacs.d/snippets/js-mode/jstd.additionalTestCase.yasnippet" nil nil)
                       ("doc" "/*:DOC ${1:element} = <${2:div}>$0</$2>*/" "jstd-doc" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/jstd-doc" nil nil)
                       ("ff" "(function (${2:${1:$(buster--shortcuts-for-globals yas/text)}}) {\n    `(buster--maybe-use-strict)\n `$0\n}(${1:`(if buster-add-default-global-to-iife buster-default-global)`}));\n" "immediately-invoked-function-expression" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/immediately-invoked-function-expression.yasnippet" nil nil)
                       ("f" "function ${1:`(snippet--function-name)`}($2) {$0}`(snippet--function-punctuation)`" "function" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/function.yasnippet" nil nil)
                       ("forin" "for (var key in ${1:obj}) {\n  if ($1.hasOwnProperty(key)) {\n    $0\n  }\n}" "for-in" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/for-in" nil nil)
                       ("for" "for (var i = 0, l = ${num}; i < l; i++) {$0}" "for" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/for" nil nil)
                       ("eb" "var $1 = FINN.elementBuilder(\"${1:div}\");$0" "finn-element-builder" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/finn-element-builder" nil nil)
                       ("ng" "angular.module(\"OIIKU_SHARED\")\n    .directive(\"`(s-lower-camel-case (buffer-file-name-body))`\", function (getContextPath) {\n        return {\n            restrict: \"E\",\n            templateUrl: getContextPath(\"angular-shared\") + \"/`(buffer-file-name-body)`.html\",\n            link: function () {\n\n            }\n        };\n    });" "directive-oiiku-shared" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/directive-oiiku-shared" nil nil)
                       ("dg" "var ${1:`buster-default-global`} = this.$1 || {};" "declare-global" nil nil nil "/home/toad/.emacs.d/snippets/js-mode/declare-global.yasnippet" nil nil)))


;;; Do not edit! File generated at Fri Nov 27 20:06:12 2015
