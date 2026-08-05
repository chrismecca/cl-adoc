;;;; cl-adoc.asd

(defsystem "cl-adoc"
  :description "An AsciiDoc parser and HTML5 renderer."
  :author "Chris Mecca"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/chrismecca/cl-adoc"
  :depends-on ("esrap")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "conditions")
                             (:file "ast")
                             (:file "line-reader")
                             (:file "inline")
                             (:file "blocks")
                             (:file "html")
                             (:file "api"))))
  :in-order-to ((test-op (test-op "cl-adoc/tests"))))

(defsystem "cl-adoc/tests"
  :description "Test suite for cl-adoc."
  :author "Chris Mecca"
  :license "MIT"
  :depends-on ("cl-adoc" "parachute")
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "inline")
                             (:file "blocks")
                             (:file "lists")
                             (:file "macros")
                             (:file "golden"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :adoc/tests)))
