;;;; Tests for block parsing and document headers.

(in-package #:adoc/tests)

(define-test blocks)

(define-test (blocks header)
  (let ((document (adoc:parse-string "= The Title
:author: Chris Mecca
:date: 2026-08-04

Body text.
")))
    (is string= "The Title" (adoc:node-text (adoc:document-title document)))
    (is string= "Chris Mecca" (adoc:document-attribute document "author"))
    (is string= "2026-08-04" (adoc:document-attribute document "date"))
    (is = 1 (length (adoc:document-blocks document)))))

(define-test (blocks header-is-optional)
  (let ((document (adoc:parse-string "Just a paragraph.")))
    (is eq nil (adoc:document-title document))
    (is = 1 (length (adoc:document-blocks document)))))

(define-test (blocks heading-levels)
  (let ((document (adoc:parse-string "== Two

=== Three

==== Four
")))
    (is equal '(2 3 4) (mapcar #'adoc:heading-level (adoc:document-blocks document))))
  ;; Five equals signs do not make a heading.
  ;; The line is parsed as ordinary text.
  (let ((document (adoc:parse-string "===== Five")))
    (is eq nil (find-if #'adoc:heading-p (adoc:document-blocks document)))))

(define-test (blocks paragraphs)
  :description "A blank line separates paragraphs. A single newline does not."
  (let ((document (adoc:parse-string "One line
and its continuation.

A second paragraph.
")))
    (is = 2 (length (adoc:document-blocks document)))))

(define-test (blocks paragraph-stops-at-a-block)
  :description "A block opener ends the paragraph above it, even without a blank line."
  (let ((document (adoc:parse-string "Some prose.
----
literal
----
")))
    (is = 2 (length (adoc:document-blocks document)))
    (true (adoc:paragraph-p (first (adoc:document-blocks document))))
    (true (adoc:listing-p (second (adoc:document-blocks document))))))

(define-test (blocks listing-blocks)
  (let ((document (adoc:parse-string "[source,lisp]
----
(defun hello () \"hi\")
----
")))
    (let ((block (first (adoc:document-blocks document))))
      (is string= "lisp" (adoc:listing-language block))
      (is string= "(defun hello () \"hi\")" (adoc:listing-text block))))
  ;; A listing block without a language still renders.
  ;; It has no language class.
  (let ((block (first (adoc:document-blocks (adoc:parse-string "----
plain
----
")))))
    (is eq nil (adoc:listing-language block))))

(define-test (blocks listing-contents-are-verbatim)
  :description "Listing block content is not parsed as markup."
  (is string= "<pre><code>*not bold* &lt;tag&gt; _x_</code></pre>
"
      (render "----
*not bold* <tag> _x_
----")))

(define-test (blocks unterminated-listing)
  :description "An unclosed block reports the line where it opened."
  (let ((source "Prose.

----
(defun oops ()
"))
    (fail (adoc:parse-string source) 'adoc:unterminated-block)
    (let ((condition (handler-case (adoc:parse-string source)
                       (adoc:unterminated-block (c) c))))
      (is = 3 (adoc:syntax-error-line condition))
      (is string= "----" (adoc:unterminated-block-delimiter condition)))))

(define-test (blocks line-numbers)
  (let ((blocks (adoc:document-blocks (adoc:parse-string "= Title

First paragraph.

== A heading
"))))
    (is = 3 (adoc:node-line (first blocks)))
    (is = 5 (adoc:node-line (second blocks)))))
