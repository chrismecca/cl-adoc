;;;; Tests for mathematics and passthrough blocks.

(in-package #:adoc/tests)

(define-test stem)

(define-test (stem inline)
  (is string= "<p>Energy is <span class=\"stem\">\\(E = mc^2\\)</span> exactly.</p>
" (render "Energy is stem:[E = mc^2] exactly.")))

(define-test (stem notation-is-literal)
  :description "Notation is built from the same characters the inline grammar
uses, so none of it is read as markup."
  (is string= "<p><span class=\"stem\">\\(a_1 * b_2\\)</span></p>
" (render "stem:[a_1 * b_2]"))
  ;; It is still escaped for HTML on the way out.
  (is string= "<p><span class=\"stem\">\\(a &lt; b &amp; c\\)</span></p>
" (render "stem:[a < b & c]")))

(define-test (stem block-form)
  (is string= "<div class=\"stem\">\\[\\sum_{i=1}^n i\\]</div>
" (render (format nil "++++~%\\sum_{i=1}^n i~%++++")))
  ;; An explicit [stem] says the same thing as no attribute line at all.
  (is string= "<div class=\"stem\">\\[x^2\\]</div>
" (render (format nil "[stem]~%++++~%x^2~%++++"))))

(define-test (stem passthrough)
  :description "A passthrough block reaches the output untouched, which is the
one place cl-adoc emits markup it did not write."
  (is string= "<custom-el a=\"1\"></custom-el>
" (render (format nil "[pass]~%++++~%<custom-el a=\"1\"></custom-el>~%++++")))
  (let ((block (first (adoc:document-blocks
                       (adoc:parse-string (format nil "[pass]~%++++~%<b>x</b>~%++++"))))))
    (true (adoc:passthrough-p block))
    (is string= "<b>x</b>" (adoc:passthrough-text block))))

(define-test (stem a-bare-delimiter-is-mathematics)
  :description "Asciidoctor defaults a bare ++++ to passthrough; cl-adoc
defaults it to stem. SPEC.adoc records the deviation."
  (let ((block (first (adoc:document-blocks
                       (adoc:parse-string (format nil "++++~%x~%++++"))))))
    (true (adoc:block-stem-p block))))

(define-test (stem attribute-lines)
  (is-values (adoc::block-attribute-line "[source,lisp]")
    (eq t)
    (equal '("source" "lisp")))
  (is-values (adoc::block-attribute-line "[stem]")
    (eq t)
    (equal '("stem")))
  (is eq nil (adoc::block-attribute-line "not an attribute line")))

(define-test (stem source-blocks-still-work)
  :description "Regression: [source,lang] now goes through the shared
attribute-line reader rather than its own."
  (let ((block (first (adoc:document-blocks
                       (adoc:parse-string (format nil "[source,lisp]~%----~%(f x)~%----"))))))
    (is string= "lisp" (adoc:listing-language block))
    (is string= "(f x)" (adoc:listing-text block)))
  ;; [source] with no language leaves the block untagged.
  (let ((block (first (adoc:document-blocks
                       (adoc:parse-string (format nil "[source]~%----~%(f x)~%----"))))))
    (is eq nil (adoc:listing-language block))))

(define-test (stem unterminated-blocks)
  (let ((source (format nil "Prose.~%~%++++~%x^2~%")))
    (fail (adoc:parse-string source) 'adoc:unterminated-block)
    (let ((condition (handler-case (adoc:parse-string source)
                       (adoc:unterminated-block (c) c))))
      (is = 3 (adoc:syntax-error-line condition))
      (is string= "++++" (adoc:unterminated-block-delimiter condition)))))

(define-test (stem closes-an-open-paragraph)
  (let ((blocks (adoc:document-blocks
                 (adoc:parse-string (format nil "Prose.~%++++~%x~%++++")))))
    (is = 2 (length blocks))
    (true (adoc:paragraph-p (first blocks)))
    (true (adoc:block-stem-p (second blocks)))
    (is = 2 (adoc:node-line (second blocks)))))
