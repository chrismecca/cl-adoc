;;;; Errors raised while reading a document.
;;;;
;;;; Each condition records the source line that caused it.
;;;;
;;;; This is why the block parser reads lines instead of characters. A writer
;;;; needs an error such as "unterminated listing block opened on line 42".
;;;; A character offset into a large string is much less useful.

(in-package #:adoc)

(define-condition adoc-error (error)
  ()
  (:documentation "Base class for errors signaled by cl-adoc."))

(define-condition syntax-error (adoc-error)
  ((line :initarg :line
         :reader syntax-error-line
         :type (integer 0)
         :documentation "One-based line number where the error was detected.")
   (message :initarg :message
            :reader syntax-error-message
            :type string))
  (:report (lambda (condition stream)
             (format stream "~a (line ~d)"
                     (syntax-error-message condition)
                     (syntax-error-line condition))))
  (:documentation "An AsciiDoc document could not be read."))

(define-condition unterminated-block (syntax-error)
  ((delimiter :initarg :delimiter
              :reader unterminated-block-delimiter
              :type string))
  (:documentation
   "A delimited block reached the end of the document before it closed.

The LINE slot holds the line where the block opened. It does not hold the end
of the file. The opening delimiter is the line the author must find."))
