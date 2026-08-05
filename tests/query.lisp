;;;; Tests for reading a parsed document.

(in-package #:adoc/tests)

(define-test query)

(defparameter *everything*
  "= The Title

An opening paragraph with *bold* and link:https://example.com[a link].

== A Section Heading

* an item with _emphasis_
* an item with image:icon.png[an icon]

NOTE: an admonition with `code`.

> a quotation with *emphasis of its own*

image::photo.jpg[a photo]

Mathematics stem:[x^2] in a sentence.
"
  "A document exercising every container the walker has to descend into.")

(define-test (query the-walk-reaches-nested-nodes)
  :description "Each of these sits at least two containers deep, which is
where a hand-rolled walk in a caller would quietly stop."
  (let ((document (adoc:parse-string *everything*)))
    ;; strong inside a paragraph inside the document
    (true (adoc:find-node #'adoc:strong-p document))
    ;; emphasis inside a list item inside a list
    (true (adoc:find-node #'adoc:emphasis-p document))
    ;; an image inside a list item
    (true (adoc:find-node #'adoc:inline-image-p document))
    ;; a code span inside an admonition
    (true (adoc:find-node #'adoc:monospace-p document))
    ;; emphasis inside a paragraph inside a blockquote
    (true (adoc:find-node #'adoc:blockquote-p document))
    ;; and the title, which is not one of the blocks
    (true (adoc:find-node #'adoc:heading-p document))))

(define-test (query node-text-removes-markup)
  (is string= "bold text" (adoc:node-text (adoc:parse-string "*bold* text")))
  (is string= "code here" (adoc:node-text (adoc:parse-string "`code` here")))
  ;; An image contributes its alt text and nothing else.
  (is string= "an icon" (adoc:node-text (adoc:parse-string "image:icon.png[an icon]")))
  ;; A link contributes its text, not its target.
  (is string= "the docs" (adoc:node-text (adoc:parse-string "link:https://x.com[the docs]"))))

(define-test (query node-text-accepts-a-list)
  :description "Several slots hold lists of nodes rather than one node."
  (let ((document (adoc:parse-string (format nil "= The Title~%~%Body."))))
    (is string= "The Title" (adoc:node-text (adoc:document-title document)))))

(define-test (query heading-ids-match-what-is-rendered)
  :description "A table of contents has to emit the anchors the renderer emits.
Both sides call HEADING-ID, so this asserts they cannot drift."
  (let* ((document (adoc:parse-string "== A Section, With Punctuation!"))
         (heading (first (adoc:document-headings document)))
         (id (adoc:heading-id heading)))
    (is string= "a-section-with-punctuation" id)
    (true (search (format nil "id=\"~a\"" id) (adoc:render-html document))
          "the rendered heading carries id ~s" id)))

(define-test (query document-headings-are-in-order)
  (let ((document (adoc:parse-string (format nil "== One~%~%=== Two~%~%== Three"))))
    (is equal '("One" "Two" "Three")
        (mapcar #'adoc:node-text (adoc:document-headings document)))
    (is equal '(2 3 2)
        (mapcar #'adoc:heading-level (adoc:document-headings document)))))

(define-test (query stem-detection-sees-inline-mathematics)
  :description "The whole reason this is a function. Inline mathematics lives
inside a paragraph, so scanning only the top-level blocks misses it."
  (let ((document (adoc:parse-string "Only inline stem:[x^2] here.")))
    (true (adoc:document-uses-stem-p document))
    ;; The mistake this exists to prevent:
    (is eq nil (find-if #'adoc:block-stem-p (adoc:document-blocks document))))
  ;; Display mathematics is found too.
  (true (adoc:document-uses-stem-p
         (adoc:parse-string (format nil "++++~%x^2~%++++"))))
  ;; And a document with none says so.
  (is eq nil (adoc:document-uses-stem-p (adoc:parse-string "Just prose."))))

(define-test (query references-cover-links-and-both-images)
  (let* ((document (adoc:parse-string
                    (format nil "See link:https://x.com[x] and image:a.png[a].~%~%image::b.jpg[b]")))
         (references (adoc:document-references document)))
    (is = 3 (length references))
    (is equal '("https://x.com" "a.png" "b.jpg")
        (mapcar #'adoc:reference-target references)))
  ;; A bare URL is a link, so it is checkable like any other reference.
  (is equal '("https://x.com/a")
      (mapcar #'adoc:reference-target
              (adoc:document-references (adoc:parse-string "See https://x.com/a.")))))

(define-test (query find-node-returns-the-first-match)
  (let ((document (adoc:parse-string (format nil "== First~%~%== Second"))))
    (is string= "First" (adoc:node-text (adoc:find-node #'adoc:heading-p document))))
  (is eq nil (adoc:find-node (constantly nil) (adoc:parse-string "prose"))))

(define-test (query leaf-nodes-have-no-children)
  :description "None of these carry nodes inside them, so the walk stops here."
  (dolist (source '("plain text" "`code`" "stem:[x]" "image:a.png[a]"))
    (let ((node (adoc:find-node (lambda (each) (typep each 'adoc:inline-node))
                                (adoc:parse-string source))))
      (true node "~s produces an inline node" source)
      (is eq nil (adoc:node-children node) "~s is a leaf" source))))
