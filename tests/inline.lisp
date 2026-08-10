;;;; Tests for the inline parser.
;;;;
;;;; Many tests here guard against one failure mode: repeated string
;;;; replacement.
;;;;
;;;; A replacement-based renderer can pass simple cases. It fails when a later
;;;; pass reads markup that an earlier pass wrote.
;;;;
;;;; cl-adoc avoids that by parsing inline text into nodes before rendering.

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
  :description "Code span content is not parsed as markup."
  (is string= "<p><code>foo_bar_baz</code></p>
" (render "`foo_bar_baz`"))
  (is string= "<p><code>*not bold*</code></p>
" (render "`*not bold*`"))
  (is string= "<p><code>&lt;div&gt;</code></p>
" (render "`<div>`")))

(define-test (inline constrained-delimiters)
  :description "A delimiter starts a span only at a word boundary."
  (is string= "<p>snake_case_name</p>
" (render "snake_case_name"))
  (is string= "<p>2 * 3 * 4</p>
" (render "2 * 3 * 4"))
  ;; A boundary can be any non-word character.
  ;; It does not have to be a space.
  (is string= "<p>(<em>parenthesised</em>)</p>
" (render "(_parenthesised_)")))

(define-test (inline escaping)
  (is string= "<p>*asterisk*</p>
" (render "\\*asterisk\\*"))
  (is string= "<p>a &lt; b &amp;&amp; c &gt; d</p>
" (render "a < b && c > d"))
  ;; An unmatched delimiter stays literal.
  (is string= "<p>a * b</p>
" (render "a * b")))

(define-test (inline plain-text-extraction)
  :description "Heading IDs are generated from text with markup removed."
  (let ((document (adoc:parse-string "== A *bold* `heading`")))
    (is string= "<h2 id=\"a-bold-heading\">A <strong>bold</strong> <code>heading</code></h2>
"
        (adoc:render-html document))))
