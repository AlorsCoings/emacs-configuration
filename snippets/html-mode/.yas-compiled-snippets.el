;;; Compiled snippets and support files for `html-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
                     '(("th" "<th$1>$2</th>" "<th>...</th>" nil
                        ("table")
                        nil nil nil nil)
                       ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil nil nil nil nil)
                       ("strong" "<strong>$0</strong>" "strong" nil nil nil nil nil nil)
                       ("script.javascript-src" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil nil nil nil nil)
                       ("script.javascript" "<script type=\"text/javascript\">\n  $0\n</script>" "<script type=\"text/javascript\">...</script> " nil nil nil nil nil nil)
                       ("script" "<script src=\"$0\"></script>" "script" nil nil nil nil nil nil)
                       ("mod" "<div class=\"mod\">\n  <div class=\"inner\">\n    <div class=\"bd\">\n      $0\n    </div>\n  </div>\n</div>" "oocss-module" nil nil nil nil nil nil)
                       ("meta.http-equiv" "<meta http-equiv=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />\n" "<meta http-equiv=\"...\" content=\"...\" />" nil
                        ("meta")
                        nil nil nil nil)
                       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil
                        ("meta")
                        nil nil nil nil)
                       ("media" "<div class=\"media\">\n  <img src=\"$1\" class=\"img\" alt=\"$2\" />\n  <div class=\"bd\">\n    $0\n  </div>\n</div>" "media" nil nil nil nil nil nil)
                       ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil nil nil nil nil)
                       ("link.stylesheet-ie" "<!--[if IE]>\n<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />\n<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil nil nil nil nil)
                       ("link.stylesheet" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil nil)
                       ("link" "<link rel=\"${1:stylesheet}\" href=\"$0\">" "link" nil nil nil nil nil nil)
                       ("kbd" "<kbd>$1</kbd>$0" "kbd" nil nil nil nil nil nil)
                       ("input" "<input type=\"${1:text}\" name=\"$2\" value=\"$0\">" "input" nil nil nil nil nil nil)
                       ("inc" "<%@ include file=\"_$0.jsp\" %>" "include" nil nil nil nil nil nil)
                       ("html.xmlns" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil nil nil nil)
                       ("html" "<html>\n  $0\n</html>\n" "<html>...</html>" nil nil nil nil nil nil)
                       ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">\n  $0\n</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil nil nil nil nil)
                       ("em" "<em>$0</em>" "em" nil nil nil nil nil nil)
                       ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil
                        ("list")
                        nil nil nil nil)
                       ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil
                        ("meta")
                        nil nil nil nil)
                       ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil
                        ("meta")
                        nil nil nil nil)
                       ("doctype.xhtml1_1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil
                        ("meta")
                        nil nil nil nil)
                       ("doctype.xhtml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil
                        ("meta")
                        nil nil nil nil)
                       ("doctype.html5" "<!DOCTYPE html>\n" "Doctype HTML 5" nil
                        ("meta")
                        nil nil nil nil)
                       ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil
                        ("meta")
                        nil nil nil nil)
                       ("dl" "<dl>\n    $0\n</dl>\n" "<dl> ... </dl>" nil
                        ("list")
                        nil nil nil nil)
                       ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil
                        ("list")
                        nil nil nil nil)
                       ("code" "<code>$0</code>" "code" nil nil nil nil nil nil)
                       ("cif" "<c:if test=\"$1\">$0</c:if>" "c:if" nil nil nil nil nil nil)
                       ("cfe" "<c:forEach items=\"\\${$1}\" var=\"$2\">\n           $0\n</c:forEach>" "c:forEach" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Mon Aug 24 17:16:26 2015
