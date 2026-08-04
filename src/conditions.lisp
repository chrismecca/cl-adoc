;;;; Errors raised while reading a document.
;;;;
;;;; Every condition here carries the source line it was raised on. That is
;;;; most of the argument for making the block layer a line reader instead of
;;;; a character-level parser: someone writing prose is served by "unterminated
;;;; listing block opened on line 42" and not at all by a character offset into
;;;; a four-kilobyte string.

(in-package #:adoc)

(define-condition adoc-error (error)
  ()
  (:documentation "Base class for every error signalled by cl-adoc."))

(define-condition syntax-error (adoc-error)
  ((line :initarg :line
         :reader syntax-error-line
         :type (integer 0)
         :documentation "One-based line number the error was detected on.")
   (message :initarg :message
            :reader syntax-error-message
            :type string))
  (:report (lambda (condition stream)
             (format stream "~a (line ~d)"
                     (syntax-error-message condition)
                     (syntax-error-line condition))))
  (:documentation "A document could not be read as AsciiDoc."))

(define-condition unterminated-block (syntax-error)
  ((delimiter :initarg :delimiter
              :reader unterminated-block-delimiter
              :type string))
  (:documentation
   "A delimited block ran to the end of the document without closing.
The LINE slot holds the line the block was opened on, not the end of the
file, since the opening delimiter is what the author needs to find."))
