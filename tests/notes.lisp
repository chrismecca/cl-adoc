;;;; Tests for admonitions and blockquotes.

(in-package #:adoc/tests)

(define-test notes)

(define-test (notes admonitions)
  (is string= "<div class=\"admonition note\">
<p class=\"admonition-label\">Note</p>
<p>Admonitions look like this.</p>
</div>
" (render "NOTE: Admonitions look like this."))
  (is string= "<div class=\"admonition warning\">
<p class=\"admonition-label\">Warning</p>
<p>Careful with <code>code</code>.</p>
</div>
" (render "WARNING: Careful with `code`.")))

(define-test (notes every-label-is-recognised)
  (let ((kinds '(("NOTE" . :note)
                 ("TIP" . :tip)
                 ("WARNING" . :warning)
                 ("IMPORTANT" . :important)
                 ("CAUTION" . :caution))))
    (dolist (pair kinds)
      (let ((block (first (adoc:document-blocks
                           (adoc:parse-string (format nil "~a: text" (car pair)))))))
        (true (adoc:admonition-p block) "~a opens an admonition" (car pair))
        (is eq (cdr pair) (adoc:admonition-kind block) "~a" (car pair))))))

(define-test (notes labels-are-matched-exactly)
  :description "A near miss is prose, not an admonition."
  (is string= "<p>NOTES: this is prose.</p>
" (render "NOTES: this is prose."))
  (is string= "<p>Note: also prose.</p>
" (render "Note: also prose."))
  ;; The space after the colon is part of the marker.
  (is string= "<p>NOTE:no space</p>
" (render "NOTE:no space")))

(define-test (notes admonitions-wrap)
  (is string= "<div class=\"admonition tip\">
<p class=\"admonition-label\">Tip</p>
<p>a tip that runs
onto a second line</p>
</div>
" (render (format nil "TIP: a tip that runs~%onto a second line"))))

(define-test (notes blockquotes)
  (is string= "<blockquote>
<p>a quoted line
and another</p>
</blockquote>
" (render (format nil "> a quoted line~%> and another")))
  (is string= "<blockquote>
<p>a quote with <strong>bold</strong></p>
</blockquote>
" (render "> a quote with *bold*")))

(define-test (notes a-bare-marker-separates-paragraphs)
  :description "A lone > inside a quotation ends one paragraph and starts the
next, rather than folding both into a single run of text."
  (is string= "<blockquote>
<p>first</p>
<p>second</p>
</blockquote>
" (render (format nil "> first~%>~%> second")))
  (let ((quote (first (adoc:document-blocks
                       (adoc:parse-string (format nil "> one~%>~%> two"))))))
    (is = 2 (length (adoc:blockquote-content quote)))
    (is equal '(1 3) (mapcar #'adoc:node-line (adoc:blockquote-content quote)))))

(define-test (notes the-quote-marker-needs-its-space)
  (is string= "<p>&gt;nospace is prose</p>
" (render ">nospace is prose")))

(define-test (notes both-close-an-open-paragraph)
  (dolist (source (list (format nil "Prose.~%NOTE: a note.")
                        (format nil "Prose.~%> quoted")))
    (let ((blocks (adoc:document-blocks (adoc:parse-string source))))
      (is = 2 (length blocks) "~s produces two blocks" source)
      (true (adoc:paragraph-p (first blocks))))))

(define-test (notes line-numbers)
  (let ((blocks (adoc:document-blocks
                 (adoc:parse-string (format nil "Prose.~%~%NOTE: a note.~%~%> quoted")))))
    (is = 3 (adoc:node-line (second blocks)))
    (is = 5 (adoc:node-line (third blocks)))))
