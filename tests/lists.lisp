;;;; Tests for lists.

(in-package #:adoc/tests)

(define-test lists)

(define-test (lists unordered)
  (is string= "<ul>
<li>one</li>
<li>two</li>
</ul>
" (render (format nil "* one~%* two"))))

(define-test (lists ordered)
  (is string= "<ol>
<li>first</li>
<li>second</li>
</ol>
" (render (format nil ". first~%. second"))))

(define-test (lists a-marker-needs-a-following-space)
  :description "Bold at the start of a line is a paragraph, not a bullet.
The marker rule is what disambiguates them, and it is the reason the SPEC
excludes hyphen markers."
  (is string= "<p><strong>bold</strong> leads this line</p>
" (render "*bold* leads this line"))
  ;; A lone asterisk is not a marker either.
  (is string= "<p>*</p>
" (render "*")))

(define-test (lists items-hold-inline-content)
  (is string= "<ul>
<li>an item with <code>code</code> and <strong>bold</strong></li>
</ul>
" (render "* an item with `code` and *bold*")))

(define-test (lists soft-wrapped-items)
  :description "An item continues onto following lines, and the indentation
used to line them up is not content."
  (let ((document (adoc:parse-string (format nil "* an item that~%  wraps~%* another"))))
    (is = 1 (length (adoc:document-blocks document)))
    (is = 2 (length (adoc:unordered-list-items (first (adoc:document-blocks document))))))
  (is string= "<ul>
<li>an item that
wraps</li>
</ul>
" (render (format nil "* an item that~%  wraps"))))

(define-test (lists blank-lines-between-items)
  :description "A blank line between items does not start a second list."
  (let ((blocks (adoc:document-blocks (adoc:parse-string (format nil "* one~%~%* two")))))
    (is = 1 (length blocks))
    (is = 2 (length (adoc:unordered-list-items (first blocks))))))

(define-test (lists a-list-ends-at-another-block)
  (let ((blocks (adoc:document-blocks
                 (adoc:parse-string (format nil "* one~%~%== A Heading")))))
    (is = 2 (length blocks))
    (true (adoc:unordered-list-p (first blocks)))
    (true (adoc:heading-p (second blocks))))
  ;; Switching marker starts a new list rather than extending the old one.
  (let ((blocks (adoc:document-blocks (adoc:parse-string (format nil "* one~%. two")))))
    (is = 2 (length blocks))
    (true (adoc:unordered-list-p (first blocks)))
    (true (adoc:ordered-list-p (second blocks)))))

(define-test (lists a-list-ends-a-paragraph)
  :description "A list opening on the next line closes the paragraph above it,
with no blank line required."
  (let ((blocks (adoc:document-blocks (adoc:parse-string (format nil "Some prose.~%* item")))))
    (is = 2 (length blocks))
    (true (adoc:paragraph-p (first blocks)))
    (true (adoc:unordered-list-p (second blocks)))))

(define-test (lists checklists)
  (is string= "<ul class=\"checklist\">
<li><input type=\"checkbox\" disabled checked> done</li>
<li><input type=\"checkbox\" disabled> todo</li>
</ul>
" (render (format nil "* [x] done~%* [ ] todo")))
  ;; An uppercase X marks an item too.
  (let ((items (adoc:unordered-list-items
                (first (adoc:document-blocks (adoc:parse-string "* [X] done"))))))
    (is eq :checked (adoc:list-item-checked (first items))))
  ;; A list with no checkbox anywhere is not marked as a checklist.
  (is string= "<ul>
<li>plain</li>
</ul>
" (render "* plain")))

(define-test (lists escaped-checkbox)
  :description "A backslash keeps the bracket literal at the one position
where checklist syntax is recognised."
  (is string= "<ul>
<li>[ ] literal brackets</li>
</ul>
" (render "* \\[ ] literal brackets"))
  ;; A bracket group that is not a word of its own was never a checkbox.
  (is string= "<ul>
<li>[x]y is not a checkbox</li>
</ul>
" (render "* [x]y is not a checkbox")))

(define-test (lists line-numbers)
  (let* ((blocks (adoc:document-blocks
                  (adoc:parse-string (format nil "Prose.~%~%* one~%* two"))))
         (items (adoc:unordered-list-items (second blocks))))
    (is = 3 (adoc:node-line (second blocks)))
    (is = 3 (adoc:node-line (first items)))
    (is = 4 (adoc:node-line (second items)))))

(define-test (lists description)
  (is string= "<dl>
<dt>term</dt>
<dd>what it means</dd>
<dt>other</dt>
<dd>something else</dd>
</dl>
" (render (format nil "term:: what it means~%other:: something else"))))

(define-test (lists description-delimiter-needs-a-following-space)
  :description "The rule that separates a description item from a block image.
A block macro puts its target hard against the colons and an item never does,
so one rule tells them apart with no ordering trick behind it."
  (is string= "<figure><img src=\"diagram.svg\" alt=\"A diagram\"></figure>
" (render "image::diagram.svg[A diagram]"))
  ;; The same word, with a space, is an ordinary term.
  (is string= "<dl>
<dt>image</dt>
<dd>a picture</dd>
</dl>
" (render "image:: a picture")))

(define-test (lists description-terms-may-contain-colons)
  :description "The delimiter is the first :: with a space after it, not the
first colon, so a term that is itself a colon-wrapped attribute survives."
  (is string= "<dl>
<dt><code>:date:</code></dt>
<dd>ISO 8601</dd>
</dl>
" (render "`:date:`:: ISO 8601")))

(define-test (lists description-soft-wraps)
  :description "A definition continues onto following lines the way a list
item does, so a term may carry nothing and be defined underneath."
  (is string= "<dl>
<dt>term</dt>
<dd>one line
and another</dd>
</dl>
" (render (format nil "term:: one line~%  and another")))
  (is string= "<dl>
<dt>term</dt>
<dd>defined below</dd>
</dl>
" (render (format nil "term::~%  defined below"))))

(define-test (lists description-ends-a-paragraph)
  :description "A description item is a block boundary, which is what stops it
being swallowed by the prose above it."
  (is string= "<p>Prose above.</p>
<dl>
<dt>term</dt>
<dd>meaning</dd>
</dl>
" (render (format nil "Prose above.~%term:: meaning"))))

(define-test (lists description-needs-a-term)
  :description "A line opening with the delimiter has nothing to define."
  (is string= "<p>:: not a term</p>
" (render ":: not a term")))
