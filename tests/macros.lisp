;;;; Tests for link macros, image macros, and bare URLs.

(in-package #:adoc/tests)

(define-test macros)

(define-test (macros link-macro)
  (is string= "<p>See <a href=\"https://example.com\">the docs</a> now.</p>
" (render "See link:https://example.com[the docs] now."))
  ;; An empty attribute list displays the target.
  (is string= "<p><a href=\"https://example.com\">https://example.com</a></p>
" (render "link:https://example.com[]"))
  ;; Link text is parsed as inline content.
  (is string= "<p><a href=\"https://x.com\"><strong>bold</strong> text</a></p>
" (render "link:https://x.com[*bold* text]")))

(define-test (macros targets-are-escaped-as-attributes)
  (is string= "<p><a href=\"https://x.com/?a=1&amp;b=2\">q</a></p>
" (render "link:https://x.com/?a=1&b=2[q]")))

(define-test (macros autolinks)
  (is string= "<p>See <a href=\"https://example.com/a\">https://example.com/a</a> here.</p>
" (render "See https://example.com/a here."))
  (is string= "<p><a href=\"http://example.com\">http://example.com</a></p>
" (render "http://example.com")))

(define-test (macros autolinks-stop-before-sentence-punctuation)
  :description "A URL at the end of a sentence must not include the sentence punctuation."
  (is string= "<p><a href=\"https://x.com/a\">https://x.com/a</a>.</p>
" (render "https://x.com/a."))
  (is string= "<p><a href=\"https://x.com/a\">https://x.com/a</a>, next</p>
" (render "https://x.com/a, next"))
  (is string= "<p>(<a href=\"https://x.com/a\">https://x.com/a</a>)</p>
" (render "(https://x.com/a)"))
  ;; Punctuation inside a URL is kept when more URL text follows it.
  (is string= "<p><a href=\"https://sub.x.com/a.html\">https://sub.x.com/a.html</a></p>
" (render "https://sub.x.com/a.html")))

(define-test (macros autolink-contents-are-not-formatted)
  :description "Underscores in a URL are part of the URL."
  (is string= "<p><a href=\"https://x.com/a_b_c\">https://x.com/a_b_c</a></p>
" (render "https://x.com/a_b_c")))

(define-test (macros inline-images)
  (is string= "<p>An <img src=\"icon.png\" alt=\"icon\"> inline.</p>
" (render "An image:icon.png[icon] inline."))
  ;; An image with no alt text still gets an alt attribute.
  (is string= "<p><img src=\"icon.png\" alt=\"\"></p>
" (render "image:icon.png[]")))

(define-test (macros block-images)
  (is string= "<figure><img src=\"photo.jpg\" alt=\"A photo\"></figure>
" (render "image::photo.jpg[A photo]"))
  (is string= "<figure><img src=\"photo.jpg\" alt=\"\"></figure>
" (render "image::photo.jpg[]"))
  (let ((block (first (adoc:document-blocks (adoc:parse-string "image::photo.jpg[A photo]")))))
    (true (adoc:block-image-p block))
    (is string= "photo.jpg" (adoc:block-image-target block))
    (is string= "A photo" (adoc:block-image-alt block))))

(define-test (macros the-block-form-needs-its-own-line)
  :description "`image::` opens a block only when the bracket closes the line."
  (is string= "<p>Prose mentioning image::photo.jpg[x] mid-line.</p>
" (render "Prose mentioning image::photo.jpg[x] mid-line."))
  ;; A block image directly after a paragraph ends that paragraph.
  (let ((blocks (adoc:document-blocks
                 (adoc:parse-string (format nil "Some prose.~%image::photo.jpg[]")))))
    (is = 2 (length blocks))
    (true (adoc:paragraph-p (first blocks)))
    (true (adoc:block-image-p (second blocks)))
    (is = 2 (adoc:node-line (second blocks)))))

(define-test (macros attribute-lists)
  :description "Attribute values split on commas. v1 uses only the first image attribute as alt text."
  (is equal '("alt" "200") (adoc::parse-attribute-list "alt, 200"))
  (is equal '() (adoc::parse-attribute-list ""))
  (is equal '("alt") (adoc::parse-attribute-list "  alt  "))
  (is string= "<figure><img src=\"p.jpg\" alt=\"alt\"></figure>
" (render "image::p.jpg[alt,200]")))

(define-test (macros a-macro-without-brackets-is-not-a-macro)
  :description "A macro without brackets is text. A URL inside it can still autolink."
  (is string= "<p>link:<a href=\"https://x.com\">https://x.com</a></p>
" (render "link:https://x.com")))

(define-test (macros headings-derive-ids-from-link-text)
  (is string= "<h2 id=\"a-heading\">A <a href=\"https://x.com\">heading</a></h2>
" (render "== A link:https://x.com[heading]")))
