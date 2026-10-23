;;;; Tests for parsed-document queries.

(in-package #:adoc/tests)

(define-test query)

(defparameter *everything*
  "= The Title

An opening paragraph with *bold* and link:https://example.com[a link].

== A Section Heading

* an item with _emphasis_
* an item with image:icon.png[an icon]

NOTE: an admonition with `code`.

> a quotation with *bold of its own*

image::photo.jpg[a photo]

Mathematics stem:[x^2] in a sentence.
"
  "A document that contains each container type the walker must enter.")

(define-test (query the-walk-reaches-nested-nodes)
  :description "The walker reaches nodes inside other containers."
  (let ((document (adoc:parse-string *everything*)))
    ;; Strong text inside a paragraph.
    (true (adoc:find-node #'adoc:strong-p document))
    ;; Emphasis inside a list item.
    (true (adoc:find-node #'adoc:emphasis-p document))
    ;; An image inside a list item.
    (true (adoc:find-node #'adoc:inline-image-p document))
    ;; A code span inside an admonition.
    (true (adoc:find-node #'adoc:monospace-p document))
    ;; Strong text inside a paragraph inside a blockquote.
    ;;
    ;; The opening paragraph also has strong text, so search only the
    ;; blockquote.
    (true (adoc:find-node #'adoc:strong-p
                          (adoc:find-node #'adoc:blockquote-p document)))
    ;; Title text.
    ;;
    ;; The title is not in DOCUMENT-BLOCKS, and it is not a HEADING node.
    (true (adoc:find-node (lambda (node)
                            (and (adoc:text-p node)
                                 (string= "The Title" (adoc:text-string node))))
                          document))))

(define-test (query node-text-removes-markup)
  (is string= "bold text" (adoc:node-text (adoc:parse-string "*bold* text")))
  (is string= "code here" (adoc:node-text (adoc:parse-string "`code` here")))
  ;; An image contributes only its alt text.
  (is string= "an icon" (adoc:node-text (adoc:parse-string "image:icon.png[an icon]")))
  ;; A link contributes its text, not its target.
  (is string= "the docs" (adoc:node-text (adoc:parse-string "link:https://x.com[the docs]"))))

(define-test (query node-text-accepts-a-list)
  :description "Some document slots contain node lists."
  (let ((document (adoc:parse-string (format nil "= The Title~%~%Body."))))
    (is string= "The Title" (adoc:node-text (adoc:document-title document)))))

(define-test (query heading-ids-match-what-is-rendered)
  :description "Rendered heading IDs match HEADING-ID."
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
  :description "Stem detection checks the full tree, not only top-level blocks."
  (let ((document (adoc:parse-string "Only inline stem:[x^2] here.")))
    (true (adoc:document-uses-stem-p document))
    ;; This is the top-level-only mistake the query prevents.
    (is eq nil (find-if #'adoc:block-stem-p (adoc:document-blocks document))))
  ;; Block mathematics is found too.
  (true (adoc:document-uses-stem-p
         (adoc:parse-string (format nil "++++~%x^2~%++++"))))
  ;; A document with no mathematics returns NIL.
  (is eq nil (adoc:document-uses-stem-p (adoc:parse-string "Just prose."))))

(define-test (query references-cover-links-and-both-images)
  (let* ((document (adoc:parse-string
                    (format nil "See link:https://x.com[x] and image:a.png[a].~%~%image::b.jpg[b]")))
         (references (adoc:document-references document)))
    (is = 3 (length references))
    (is equal '("https://x.com" "a.png" "b.jpg")
        (mapcar #'adoc:reference-target references)))
  ;; A bare URL is a link, so it is also a reference.
  (is equal '("https://x.com/a")
      (mapcar #'adoc:reference-target
              (adoc:document-references (adoc:parse-string "See https://x.com/a.")))))

(define-test (query find-node-returns-the-first-match)
  (let ((document (adoc:parse-string (format nil "== First~%~%== Second"))))
    (is string= "First" (adoc:node-text (adoc:find-node #'adoc:heading-p document))))
  (is eq nil (adoc:find-node (constantly nil) (adoc:parse-string "prose"))))

(define-test (query leaf-nodes-have-no-children)
  :description "Leaf nodes stop traversal."
  (dolist (source '("plain text" "`code`" "stem:[x]" "image:a.png[a]"))
    (let ((node (adoc:find-node (lambda (each) (typep each 'adoc:inline-node))
                                (adoc:parse-string source))))
      (true node "~s produces an inline node" source)
      (is eq nil (adoc:node-children node) "~s is a leaf" source))))
