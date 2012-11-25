;;;; com.elbeno.ll1-parser-gen.asd

(asdf:defsystem #:com.elbeno.ll1-parser-gen
  :serial t
  :description "An LL(1) parser generator."
  :author "Ben Deane <lisp@elbeno.com>"
  :license "GPLv3"
  :components ((:file "package")
               (:file "lexer")
               (:file "parser-gen")
               (:file "example")))
