;;;; Tests for the inline layer.
;;;;
;;;; Most of these are regression tests for one failure mode. A renderer built
;;;; out of successive string substitutions passes the easy cases here and
;;;; fails the interesting ones, because by its second pass it is reading
;;;; markup it wrote during the first.

(in-package #:adoc/tests)

(defun render (source)
  "Parse SOURCE and return the rendered HTML fragment."
  (adoc:render-html (adoc:parse-string source)))

(define-test inline)

(define-test (inline formatting)
  (is string= "<p><strong>bold</strong></p>
" (render "*bold*"))
  (is string= "<p><em>italic</em></p>
" (render "_italic_"))
  (is string= "<p><code>mono</code></p>
" (render "`mono`"))
  (is string= "<p>This is <strong>bold with <em>italic</em> inside</strong>.</p>
" (render "This is *bold with _italic_ inside*.")))

(define-test (inline code-spans-are-literal)
  :description "A code span's contents are never re-read as markup."
  (is string= "<p><code>foo_bar_baz</code></p>
" (render "`foo_bar_baz`"))
  (is string= "<p><code>*not bold*</code></p>
" (render "`*not bold*`"))
  (is string= "<p><code>&lt;div&gt;</code></p>
" (render "`<div>`")))

(define-test (inline constrained-delimiters)
  :description "A delimiter only opens a span at a word boundary."
  (is string= "<p>snake_case_name</p>
" (render "snake_case_name"))
  (is string= "<p>2 * 3 * 4</p>
" (render "2 * 3 * 4"))
  ;; A boundary is any non-word character, not only a space.
  (is string= "<p>(<em>parenthesised</em>)</p>
" (render "(_parenthesised_)")))

(define-test (inline escaping)
  (is string= "<p>*asterisk*</p>
" (render "\\*asterisk\\*"))
  (is string= "<p>a &lt; b &amp;&amp; c &gt; d</p>
" (render "a < b && c > d"))
  ;; An unmatched delimiter is just the character the author typed.
  (is string= "<p>a * b</p>
" (render "a * b")))

(define-test (inline plain-text-extraction)
  :description "Heading identifiers are derived from text with markup removed."
  (let ((document (adoc:parse-string "== A *bold* `heading`")))
    (is string= "<h2 id=\"a-bold-heading\">A <strong>bold</strong> <code>heading</code></h2>
"
        (adoc:render-html document))))
